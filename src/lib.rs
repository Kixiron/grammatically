use core::{fmt, iter::Peekable, str::CharIndices};
use randomize::{AnyPCG, PCG32};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone)]
pub struct Generator<'src> {
    rules: Vec<Rule<'src>>,
    rng: Option<AnyPCG<PCG32>>,
}

impl<'src> Generator<'src> {
    pub fn new(grammar: &'src str, seed: u64) -> Result<Self> {
        let rules = Parser::new(grammar).parse()?;
        let rng = Some(AnyPCG::new(PCG32::seed(seed, 342488881)));

        let mut gen = Self { rules, rng };
        gen.optimize()?;

        Ok(gen)
    }

    pub fn from_rules(rules: Vec<Rule<'src>>, seed: u64) -> Self {
        let rng = Some(AnyPCG::new(PCG32::seed(seed, 342488881)));

        Self { rules, rng }
    }

    pub fn rules<'a>(&'a self) -> impl Iterator<Item = &'a str> + 'a {
        self.rules.iter().map(|r| r.name)
    }

    fn optimize(&mut self) -> Result<()> {
        let mut need_change = Vec::with_capacity(self.rules.len() * 2);

        for (rule_idx, rule) in self.rules.iter().enumerate() {
            for (seg_idx, seg) in rule.body.iter().enumerate() {
                if let Segment::Rule(GrammarRule::Rule(name)) = seg {
                    let idx = self.rules.iter().position(|r| &r.name == name);

                    if let Some(idx) = idx {
                        need_change.push((
                            rule_idx,
                            seg_idx,
                            Segment::Rule(GrammarRule::Index(idx)),
                        ));
                    } else {
                        return Err(Error::new(
                            ErrorKind::NoRule,
                            format!(
                                "Attempted to find the rule '{}', but it doesn't exist",
                                name
                            ),
                            None,
                        ));
                    }
                }
            }
        }

        for (rule_idx, seg_idx, new_seg) in need_change {
            self.rules[rule_idx].body[seg_idx] = new_seg;
        }

        Ok(())
    }

    pub fn generate(&mut self, rule: &str) -> Result<String> {
        let mut buf = String::new();
        let mut rng = self.rng.take().ok_or_else(|| {
            Error::new(
                ErrorKind::Internal,
                "Tried to take internal RNG, but it doesn't exist",
                None,
            )
        })?;

        let production = self.rules.iter().find(|r| r.name == rule).ok_or_else(|| {
            Error::new(
                ErrorKind::NoRule,
                format!(
                    "Attempted to find the rule '{}', but it doesn't exist",
                    rule,
                ),
                None,
            )
        })?;

        production.append(&self.rules, &mut buf, &mut rng)?;
        self.rng = Some(rng);

        Ok(buf)
    }
}

trait EBNF {
    fn is_whitespace(self) -> bool;
    fn is_char(self) -> bool;
    fn is_special(self) -> bool;
}

impl EBNF for char {
    fn is_whitespace(self) -> bool {
        matches!(self, ' ' | '\n' | '\r' | '\t')
    }

    fn is_char(self) -> bool {
        match self {
            '\x09' | '\x0A' | '\x0D' => true,
            c => {
                ('\x20'..'\u{D7FF}').contains(&c)
                    || ('\u{E000}'..'\u{FFFD}').contains(&c)
                    || ('\u{10000}'..'\u{10FFFF}').contains(&c)
            }
        }
    }

    fn is_special(self) -> bool {
        matches!(
            self,
            '?' | '*' | '+' | '|' | ':' | ';' | ')' | '(' | ']' | '[' | '{' | '}'
        )
    }
}

#[derive(Debug, Clone)]
pub struct Parser<'src> {
    source: &'src str,
    chars: Peekable<CharIndices<'src>>,
}

/// Interface functions
impl<'src> Parser<'src> {
    pub fn new(source: &'src str) -> Self {
        Self {
            source,
            chars: source.char_indices().peekable(),
        }
    }

    fn parse(mut self) -> Result<Vec<Rule<'src>>> {
        self.parse_inner()
    }
}

/// Parsing functions
impl<'src> Parser<'src> {
    fn parse_inner(&mut self) -> Result<Vec<Rule<'src>>> {
        let mut ast = Vec::with_capacity(10);

        while self.peek().is_ok() {
            self.eat_whitespace();
            let name = self.ident()?;
            self.assign()?;
            let body = self.rule_body()?;
            self.eat_whitespace();

            ast.push(Rule { name, body });
        }

        Ok(ast)
    }

    fn eat_whitespace(&mut self) {
        while matches!(self.peek(), Ok((_, c)) if c.is_whitespace()) {
            self.next().expect("Should never fail");
        }
    }

    fn ident(&mut self) -> Result<&'src str> {
        let start = self.next()?.0;
        let mut end = start;

        self.eat_whitespace();
        let (_, mut current) = self.peek()?;
        while !current.is_whitespace() && current.is_char() && !current.is_special() {
            end = self.next()?.0 + 1;
            current = self.peek()?.1;
        }
        self.eat_whitespace();

        Ok(&self.source[start..end])
    }

    fn assign(&mut self) -> Result<&'src str> {
        if let [Ok((start, ':')), Ok((_, ':')), Ok((end, '='))] =
            [self.next(), self.next(), self.next()]
        {
            Ok(&self.source[start..end])
        } else {
            todo!()
        }
    }

    fn rule_body(&mut self) -> Result<Vec<Segment<'src>>> {
        self.eat_whitespace();

        let mut body = Vec::with_capacity(5);
        while !matches!(self.peek(), Ok((_, ';'))) {
            if let Some(seg) = self.segment()? {
                body.push(seg);
            } else {
                break;
            }
            self.eat_whitespace();
        }

        self.eat(';')?;

        Ok(body)
    }

    fn literal(&mut self) -> Result<Segment<'src>> {
        self.eat_whitespace();

        let start = self.eat('\'')?.0 + 1;
        while !matches!(self.peek(), Ok((_, '\''))) {
            self.next()?;
        }

        let end = self.eat('\'')?.0;

        Ok(Segment::Literal(&self.source[start..end]))
    }

    fn segment(&mut self) -> Result<Option<Segment<'src>>> {
        self.eat_whitespace();
        let seg = match self.peek()? {
            (_, '[') => self.set()?,
            (_, '(') => self.paren()?,
            (_, '\'') => self.literal()?,
            (_, c) if c.is_char() && !c.is_special() => {
                Segment::Rule(GrammarRule::Rule(self.ident()?))
            }
            (_, ';') => return Ok(None),

            (idx, c) => {
                return Err(Error::new(
                    ErrorKind::Parse,
                    format!("Expected a rule segment, got '{}'", c),
                    Some((idx, self.source)),
                ));
            }
        };

        self.eat_whitespace();
        match self.peek() {
            Ok((_, '?')) => {
                self.eat('?')?;
                Ok(Some(Segment::ZeroOrOne(Box::new(seg))))
            }
            Ok((_, '*')) => {
                self.eat('*')?;
                Ok(Some(Segment::ZeroOrMore(Box::new(seg))))
            }
            Ok((_, '+')) => {
                self.eat('+')?;
                Ok(Some(Segment::OneOrMore(Box::new(seg))))
            }
            Ok((_, '|')) => {
                self.eat('|')?;
                let right = if let Some(right) = self.segment()? {
                    right
                } else {
                    return Ok(None);
                };

                Ok(Some(Segment::Either(Box::new(seg), Box::new(right))))
            }
            Ok((_, '{')) => Ok(Some(Segment::Repeat(
                Box::new(seg),
                self.eat_repetitions()?,
            ))),

            _ => Ok(Some(seg)),
        }
    }

    fn eat_repetitions(&mut self) -> Result<Vec<u32>> {
        self.eat('{')?;
        self.eat_whitespace();

        let mut repeats = Vec::new();
        while !matches!(self.peek(), Ok((_, '}'))) {
            let start = if matches!(self.peek(), Ok((_, c)) if c.is_numeric()) {
                self.next()?.0
            } else {
                break;
            };
            let mut end = start;

            while matches!(self.peek(), Ok((_, c)) if c.is_numeric()) {
                end = self.next()?.0;
            }

            if matches!(self.peek(), Ok((_, ','))) {
                self.eat(',')?;
                self.eat_whitespace();
            } else {
                break;
            }

            repeats.push((&self.source[start..end]).parse().unwrap());
        }
        self.eat('}')?;

        Ok(repeats)
    }

    fn set(&mut self) -> Result<Segment<'src>> {
        self.eat_whitespace();
        self.eat('[')?;

        self.eat_whitespace();
        let mut body = Vec::with_capacity(3);
        let _negated = if matches!(self.peek(), Ok((_, '^'))) {
            self.eat('^')?;
            true
        } else {
            false
        };

        self.eat_whitespace();
        while !matches!(self.peek(), Ok((_, ']'))) {
            self.eat_whitespace();
            match self.next()? {
                (_, c) if c.is_whitespace() => self.eat_whitespace(),
                (_, c) if c.is_char() => {
                    self.eat_whitespace();
                    if matches!(self.peek(), Ok((_, '-'))) {
                        self.eat('-')?;

                        self.eat_whitespace();
                        let (_, c2) = self.next()?;

                        if c2.is_char() {
                            body.extend((c as u32..=c2 as u32).filter_map(core::char::from_u32));
                        } else {
                            todo!("{:?}", c2)
                        }
                    } else {
                        body.push(c);
                    }
                }

                (idx, c) => {
                    return Err(Error::new(
                        ErrorKind::Parse,
                        format!("error: Expected a char, got {}", c),
                        Some((idx, self.source)),
                    ));
                }
            }
        }
        self.eat(']')?;

        Ok(Segment::Set(body))
    }

    fn paren(&mut self) -> Result<Segment<'src>> {
        self.eat_whitespace();
        self.eat('(')?;

        self.eat_whitespace();
        let mut body = Vec::with_capacity(3);
        while !matches!(self.peek(), Ok((_, ')'))) {
            if let Some(seg) = self.segment()? {
                body.push(seg);
            } else {
                break;
            }
            self.eat_whitespace();
        }
        self.eat(')')?;

        Ok(Segment::Grouped(body))
    }
}

/// Utility functions
impl<'src> Parser<'src> {
    #[inline(always)]
    fn next(&mut self) -> Result<(usize, char)> {
        self.chars
            .next()
            .ok_or_else(|| Error::new(ErrorKind::Parse, "Unexpected EOF", None))
    }

    #[inline(always)]
    fn peek(&mut self) -> Result<(usize, char)> {
        self.chars
            .peek()
            .copied()
            .ok_or_else(|| Error::new(ErrorKind::Parse, "Unexpected EOF", None))
    }

    /// Eats one of the `expected` token
    #[inline(always)]
    fn eat(&mut self, expected: char) -> Result<(usize, char)> {
        let (idx, c) = self.next()?;

        if c == expected {
            Ok((idx, c))
        } else {
            Err(Error::new(
                ErrorKind::Parse,
                format!("error: Expected '{}', got {}", expected, c),
                Some((idx, self.source)),
            ))
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Rule<'src> {
    name: &'src str,
    body: Vec<Segment<'src>>,
}

impl<'src> Rule<'src> {
    pub fn append(&self, rules: &[Rule], buf: &mut String, rng: &mut AnyPCG<PCG32>) -> Result<()> {
        for seg in self.body.iter() {
            seg.append(rules, buf, rng)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Segment<'src> {
    Literal(&'src str),
    Rule(GrammarRule<'src>),
    Grouped(Vec<Segment<'src>>),
    Set(Vec<char>),
    Char(char),
    ZeroOrMore(Box<Segment<'src>>),
    OneOrMore(Box<Segment<'src>>),
    ZeroOrOne(Box<Segment<'src>>),
    Either(Box<Segment<'src>>, Box<Segment<'src>>),
    Repeat(Box<Segment<'src>>, Vec<u32>),
}

impl<'src> Segment<'src> {
    pub fn append(&self, rules: &[Rule], buf: &mut String, rng: &mut AnyPCG<PCG32>) -> Result<()> {
        match self {
            Self::Char(c) => buf.push(*c),
            Self::Literal(lit) => buf.push_str(lit),
            Self::Set(set) => buf.push(set[rng.next_u32() as usize % set.len()]),

            Self::Grouped(group) => {
                for seg in group {
                    seg.append(rules, buf, rng)?;
                }
            }

            Self::Rule(rule) => {
                let rule = match rule {
                    GrammarRule::Index(idx) => &rules[*idx],
                    GrammarRule::Rule(name) => {
                        rules.iter().find(|r| r.name == *name).ok_or_else(|| {
                            Error::new(
                                ErrorKind::NoRule,
                                format!(
                                    "Attempted to find the rule '{}', but it doesn't exist",
                                    name
                                ),
                                None,
                            )
                        })?
                    }
                };

                rule.append(rules, buf, rng)?;
            }

            Self::ZeroOrMore(seg) => {
                for _ in 0..rng.next_u32() as usize % 2 {
                    seg.append(rules, buf, rng)?;
                }
            }

            Self::OneOrMore(seg) => {
                seg.append(rules, buf, rng)?;

                for _ in 0..rng.next_u32() as usize % 2 {
                    seg.append(rules, buf, rng)?;
                }
            }

            Self::ZeroOrOne(seg) => {
                if rng.next_bool() {
                    seg.append(rules, buf, rng)?;
                }
            }

            Self::Either(left, right) => {
                if rng.next_bool() {
                    left.append(rules, buf, rng)?;
                } else {
                    right.append(rules, buf, rng)?;
                }
            }

            Self::Repeat(seg, repeats) => {
                for _ in 0..repeats[rng.next_u32() as usize % repeats.len()] {
                    seg.append(rules, buf, rng)?;
                }
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum GrammarRule<'src> {
    Rule(&'src str),
    Index(usize),
}

#[test]
fn new() {
    let mut generator = match Generator::new(
        "Grammar    ::= Production*;
         Production ::= Ident ' ::= ' Body ';' '\n';
         Ident      ::= Char+;
         Body       ::= Ident | String | Set;
         String     ::= '\"'Char*'\"';
         Set        ::= '[' (CharRange | Char)+ ']';
         CharRange  ::= Char '-' Char;
         Char       ::= [a-zA-Z0-9_];",
        4324222,
    ) {
        Ok(gen) => gen,
        Err(err) => {
            println!("{}", err);
            return;
        }
    };

    match generator.generate("Grammar") {
        Ok(gen) => println!("{}", gen),
        Err(err) => println!("{}", err),
    }
    println!("\n");
}

#[test]
fn c() {
    let mut generator = Generator::new(include_str!("../c.ebnf"), 33333).unwrap();
    println!("{}", generator.generate("jump-statement").unwrap());
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Error {
    kind: ErrorKind,
    message: String,
    loc: Option<(usize, usize)>,
    source: Option<String>,
}

impl Error {
    fn new(kind: ErrorKind, message: impl Into<String>, loc: Option<(usize, &str)>) -> Self {
        let (loc, source) = if let Some((offset, src)) = loc {
            let mut line_num = 0;
            let mut curr = 0;
            let mut full_line = None;

            for line in src.lines() {
                curr += line.len();
                line_num += 1;

                if offset >= curr - line.len() && offset <= curr {
                    curr -= offset;
                    full_line = Some(line.to_string());
                    break;
                }
            }

            (Some((line_num, curr)), full_line)
        } else {
            (None, None)
        };

        Self {
            kind,
            message: message.into(),
            loc,
            source,
        }
    }

    pub fn kind(&self) -> ErrorKind {
        self.kind
    }

    pub fn message(&self) -> &str {
        &self.message
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let (Some((line, column)), Some(src)) = (self.loc, self.source.as_ref()) {
            write!(
                f,
                "Error ({:?}): {}\n {}:{} => {}\n{:>width$}",
                self.kind,
                self.message,
                line,
                column,
                src,
                "^",
                width = " {}:{} => {} ".len() + column + 1,
            )
        } else {
            write!(f, "Error: {:?}\n{}\n", self.kind, self.message)
        }
    }
}

#[non_exhaustive]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum ErrorKind {
    Parse,
    Internal,
    NoRule,
}
