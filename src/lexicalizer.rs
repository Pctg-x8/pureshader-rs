#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Position {
    pub left: usize,
    pub line: usize,
}
impl Default for Position {
    fn default() -> Self {
        Position { left: 1, line: 1 }
    }
}
impl Position {
    pub fn line_feed(&mut self, amount: usize) -> &mut Self {
        self.line += amount;
        self.left = Self::default().left;
        self
    }
    pub fn advance_left(&mut self, amount: usize) -> &mut Self {
        self.left += amount;
        self
    }
}

#[derive(Clone)]
pub struct Span<'s> {
    pub source: &'s str,
    pub pos: usize,
    pub len: usize,
}
impl<'s> Span<'s> {
    pub fn empty(pos: usize) -> Self {
        Self {
            source: "",
            pos,
            len: 0,
        }
    }

    pub fn as_str(&self) -> &'s str {
        &self.source[self.pos..self.pos + self.len]
    }

    pub fn calc_position(&self) -> Position {
        let line = 1 + self.source[..self.pos]
            .chars()
            .filter(|&c| c == '\n')
            .count();
        let left = 1 + self.source[..self.pos]
            .chars()
            .rev()
            .take_while(|&c| c != '\n')
            .fold(0, |a, c| a + (if c != '\r' { 1 } else { 0 }));

        Position { line, left }
    }
}
impl std::ops::Deref for Span<'_> {
    type Target = str;

    fn deref(&self) -> &str {
        self.as_str()
    }
}
impl std::fmt::Debug for Span<'_> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        if self.len == 0 {
            write!(fmt, "Span::empty")
        } else {
            write!(fmt, "Span({})", self.as_str())
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Module,
    Import,
    Export,
    Hiding,
    Foreign,
    Type,
    Data,
    Class,
    Instance,
    Struct,
    Const,
    If,
    Else,
    Then,
    True,
    False,
    Let,
    Where,
    Do,
    In,
    Qualified,
    As,
    Case,
    Of,
    Int,
    UInt,
    Float,
    Vector2,
    Vector3,
    Vector4,
    Matrix2,
    Matrix3,
    Matrix4,
    Matrix2x3,
    Matrix2x4,
    Matrix3x2,
    Matrix3x4,
    Matrix4x2,
    Matrix4x3,
}
impl Keyword {
    fn introduce_block(&self) -> bool {
        *self == Self::Where || *self == Self::Do || *self == Self::Let || *self == Self::Of
    }
}

#[derive(Debug, Clone)]
pub struct NumberLiteral<'s> {
    ipart: Span<'s>,
    fpart: Option<Span<'s>>,
    epart: Option<Span<'s>>,
    float_ty: bool,
    unsigned_ty: bool,
    long_ty: bool,
}

#[derive(Debug, Clone)]
pub enum Token<'s> {
    Punct(Span<'s>),
    /// {n}
    LayoutMarkerBlockOpener(usize, Span<'s>),
    /// <n>
    LayoutMarkerIndentation(usize, Span<'s>),
    LargeStartIdentifier(Span<'s>),
    SmallStartIdentifier(Span<'s>),
    StringLiteral(Span<'s>),
    NumberLiteral(NumberLiteral<'s>),
    LeftParenthese(Span<'s>),
    RightParenthese(Span<'s>),
    LeftBracket(Span<'s>),
    RightBracket(Span<'s>),
    Equal(Span<'s>),
    Typing(Span<'s>),
    Op(Span<'s>),
    Backquote(Span<'s>),
    BlockStart(Span<'s>),
    BlockEnd(Span<'s>),
    Semicolon(Span<'s>),
    Comma(Span<'s>),
    Arrow(Span<'s>),
    FatArrow(Span<'s>),
    Backslash(Span<'s>),
    At(Span<'s>),
    Keyword(Keyword, Span<'s>),
}
impl Token<'_> {
    pub fn head_span(&self) -> &Span {
        match self {
            Self::NumberLiteral(NumberLiteral { ipart, .. }) => ipart,
            Self::Punct(s)
            | Self::LayoutMarkerBlockOpener(_, s)
            | Self::LayoutMarkerIndentation(_, s)
            | Self::LargeStartIdentifier(s)
            | Self::SmallStartIdentifier(s)
            | Self::StringLiteral(s)
            | Self::LeftParenthese(s)
            | Self::RightParenthese(s)
            | Self::LeftBracket(s)
            | Self::RightBracket(s)
            | Self::Equal(s)
            | Self::Typing(s)
            | Self::Op(s)
            | Self::Backquote(s)
            | Self::BlockStart(s)
            | Self::BlockEnd(s)
            | Self::Semicolon(s)
            | Self::Comma(s)
            | Self::Arrow(s)
            | Self::FatArrow(s)
            | Self::Backslash(s)
            | Self::At(s)
            | Self::Keyword(_, s) => s,
        }
    }
}

#[derive(Debug)]
pub enum TokenizeError {
    UnexpectedCharacter(char, usize),
    UnclosedBlockComment(usize),
    UnclosedBackquotedIdentifier(usize),
    UnclosedString(usize),
}
impl TokenizeError {
    pub fn calc_position(&self, source: &str) -> Position {
        match self {
            &Self::UnexpectedCharacter(_, p)
            | &Self::UnclosedBlockComment(p)
            | &Self::UnclosedBackquotedIdentifier(p)
            | &Self::UnclosedString(p) => Span {
                source,
                pos: p,
                len: 0,
            }
            .calc_position(),
        }
    }
}

pub enum TokenGenerator<'s> {
    Empty,
    Single(Option<Token<'s>>),
    Double(Option<Token<'s>>, Option<Token<'s>>),
}
impl<'s> TokenGenerator<'s> {
    pub fn new(tok: Token<'s>) -> Self {
        Self::Single(Some(tok))
    }

    pub fn with_preceded_layout_marker(layout_marker: Token<'s>, tok: Token<'s>) -> Self {
        Self::Double(Some(layout_marker), Some(tok))
    }
}
impl<'s> Iterator for TokenGenerator<'s> {
    type Item = Token<'s>;

    fn next(&mut self) -> Option<Token<'s>> {
        match self {
            Self::Empty => None,
            Self::Single(ref mut s) => s.take(),
            Self::Double(ref mut s1, ref mut s2) => s1.take().or_else(|| s2.take()),
        }
    }
}

pub struct Lexicalizer<'s> {
    src: &'s str,
    pos: usize,
    first_lexeme: bool,
    requires_block_opener_layout_marker: bool,
    preceding_whitespaces: Option<usize>,
}

impl<'s> Lexicalizer<'s> {
    pub fn new(s: &'s str) -> Self {
        Lexicalizer {
            src: s,
            pos: 0,
            first_lexeme: true,
            requires_block_opener_layout_marker: false,
            preceding_whitespaces: None,
        }
    }

    fn byte_at_rel(&self, offset: usize) -> Option<u8> {
        self.src.as_bytes().get(self.pos + offset).copied()
    }
    fn char_at_rel(&self, offset: usize) -> Option<char> {
        self.src[self.pos + offset..].chars().next()
    }
    fn head_char(&self) -> Option<char> {
        self.char_at_rel(0)
    }
    fn starts_with(&self, s: &str) -> bool {
        self.src[self.pos..].starts_with(s)
    }

    fn spanize(&self, len: usize) -> Span<'s> {
        Span {
            source: self.src,
            pos: self.pos,
            len,
        }
    }

    pub fn parse_next(&mut self) -> Result<TokenGenerator<'s>, TokenizeError> {
        const OPCHARACTERS: &[char] = &[
            '<', '>', '=', '?', '|', '!', '#', '$', '%', '&', '~', '^', '-', '+', '*', '/', ':',
            '.',
        ];

        if self.starts_with("--") {
            let b = self.src[2..]
                .chars()
                .take_while(|&c| c != '\n')
                .fold(0, |a, c| a + c.len_utf8());
            let t = Token::Punct(self.spanize(2 + b));
            self.pos += 2 + b;

            return Ok(TokenGenerator::new(t));
        }

        if self.starts_with("{-") {
            let b = self.try_read_block_comment(self.pos + 2)?;
            let t = Token::Punct(self.spanize(2 + b));
            self.pos += 2 + b;

            return Ok(TokenGenerator::new(t));
        }

        let head_span = Span {
            source: self.src,
            pos: self.pos,
            len: 0,
        };
        let mut insert_block_opener_marker = self.requires_block_opener_layout_marker;

        let t = match self.head_char() {
            // EOF
            None => return Ok(TokenGenerator::Empty),
            Some(c) if c == ' ' || c == '\n' || c == '\t' || c.is_whitespace() => {
                let b = self.src[self.pos..]
                    .chars()
                    .take_while(|&c| c == ' ' || c == '\n' || c == '\t' || c.is_whitespace())
                    .fold(0, |a, c| a + c.len_utf8());

                let sp = self.spanize(b);
                let indent_level = sp.as_str().chars().rev().take_while(|&c| c == ' ').count();
                let has_linefeed = sp.as_str().contains('\n');
                let t = Token::Punct(sp);
                self.pos += b;
                self.preceding_whitespaces = Some(indent_level).filter(|&v| has_linefeed);

                // early return (ignoring layout)
                return Ok(TokenGenerator::new(t));
            }
            Some('(') => {
                let t = Token::LeftParenthese(self.spanize(1));
                self.pos += 1;
                self.requires_block_opener_layout_marker = false;

                t
            }
            Some(')') => {
                let t = Token::RightParenthese(self.spanize(1));
                self.pos += 1;
                self.requires_block_opener_layout_marker = false;

                t
            }
            Some('[') => {
                let t = Token::LeftBracket(self.spanize(1));
                self.pos += 1;

                t
            }
            Some(']') => {
                let t = Token::RightBracket(self.spanize(1));
                self.pos += 1;
                self.requires_block_opener_layout_marker = false;

                t
            }
            Some('{') => {
                let t = Token::BlockStart(self.spanize(1));
                self.pos += 1;
                self.requires_block_opener_layout_marker = false;
                insert_block_opener_marker = false;

                t
            }
            Some('}') => {
                let t = Token::BlockEnd(self.spanize(1));
                self.pos += 1;
                self.requires_block_opener_layout_marker = false;

                t
            }
            Some(',') => {
                let t = Token::Comma(self.spanize(1));
                self.pos += 1;
                self.requires_block_opener_layout_marker = false;

                t
            }
            Some('\\') => {
                let t = Token::Backslash(self.spanize(1));
                self.pos += 1;
                self.requires_block_opener_layout_marker = false;

                t
            }
            Some('`') => {
                let t = Token::Backquote(self.spanize(1));
                self.pos += 1;
                self.requires_block_opener_layout_marker = false;

                t
            }
            Some('"') => {
                let b = self.try_parse_string_main(self.src[self.pos + 1..].chars(), 0)?;
                self.pos += 1;
                let t = Token::StringLiteral(self.spanize(b));
                self.pos += b + 1;
                self.requires_block_opener_layout_marker = false;

                t
            }
            Some('@') => {
                let t = Token::At(self.spanize(1));
                self.pos += 1;
                self.requires_block_opener_layout_marker = false;

                t
            }
            Some(c) if ('0'..='9').contains(&c) => {
                let ipart_b = self.src[self.pos..]
                    .chars()
                    .take_while(|c| ('0'..='9').contains(c))
                    .fold(0, |a, c| a + c.len_utf8());

                let fpart_b = if self.byte_at_rel(ipart_b) == Some(b'.') {
                    self.src[self.pos + ipart_b + 1..]
                        .chars()
                        .take_while(|c| ('0'..='9').contains(c))
                        .fold(0, |a, c| a + c.len_utf8())
                } else {
                    0
                };

                let epart_start = if fpart_b > 0 {
                    ipart_b + 1 + fpart_b
                } else {
                    ipart_b
                };
                let epart_b = if self.byte_at_rel(epart_start) == Some(b'e')
                    || self.byte_at_rel(epart_start) == Some(b'E')
                {
                    self.src[self.pos + epart_start + 1..]
                        .chars()
                        .take_while(|c| ('0'..='9').contains(c))
                        .fold(0, |a, c| a + c.len_utf8())
                } else {
                    0
                };

                let suffixpart_start = if epart_b > 0 {
                    epart_start + 1 + epart_b
                } else {
                    epart_start
                };
                let mut suffixpart_b = 0;
                let mut ci = self.src[self.pos + suffixpart_start..].chars();
                let (mut float_ty, mut long_ty, mut unsigned_ty) = (false, false, false);
                while let Some(c) = ci.next() {
                    match c {
                        'l' | 'L' => {
                            long_ty = true;
                            suffixpart_b += c.len_utf8();
                        }
                        'f' | 'F' => {
                            float_ty = true;
                            suffixpart_b += c.len_utf8();
                        }
                        'u' | 'U' => {
                            unsigned_ty = true;
                            suffixpart_b += c.len_utf8();
                        }
                        _ => break,
                    }
                }

                let t = Token::NumberLiteral(NumberLiteral {
                    ipart: Span {
                        source: self.src,
                        pos: self.pos,
                        len: ipart_b,
                    },
                    fpart: if fpart_b > 0 {
                        Some(Span {
                            source: self.src,
                            pos: self.pos + ipart_b + 1,
                            len: fpart_b,
                        })
                    } else {
                        None
                    },
                    epart: if epart_b > 0 {
                        Some(Span {
                            source: self.src,
                            pos: self.pos + epart_start + 1,
                            len: epart_b,
                        })
                    } else {
                        None
                    },
                    float_ty,
                    long_ty,
                    unsigned_ty,
                });
                self.pos += suffixpart_start + suffixpart_b;
                self.requires_block_opener_layout_marker = false;

                t
            }
            Some(c) if OPCHARACTERS.iter().any(|&a| a == c) => {
                let b = self.src[self.pos..]
                    .chars()
                    .take_while(|&c| OPCHARACTERS.iter().any(|&a| a == c))
                    .fold(0, |a, c| a + c.len_utf8());
                let t = match &self.src[self.pos..self.pos + b] {
                    "=" => Token::Equal(self.spanize(1)),
                    "::" => Token::Typing(self.spanize(2)),
                    "->" => Token::Arrow(self.spanize(2)),
                    "=>" => Token::FatArrow(self.spanize(2)),
                    _ => Token::Op(self.spanize(b)),
                };
                self.pos += b;
                self.requires_block_opener_layout_marker = false;

                t
            }
            Some(c) if ('a'..='z').contains(&c) || ('A'..='Z').contains(&c) || c == '_' => {
                let b = self.src[self.pos..]
                    .chars()
                    .take_while(|&c| {
                        ('a'..='z').contains(&c)
                            || ('A'..='Z').contains(&c)
                            || ('0'..='9').contains(&c)
                            || c == '_'
                            || c == '\''
                    })
                    .fold(0, |a, c| a + c.len_utf8());

                let kw = match &self.src[self.pos..self.pos + b] {
                    "module" => Some(Keyword::Module),
                    "import" => Some(Keyword::Import),
                    "export" => Some(Keyword::Export),
                    "hiding" => Some(Keyword::Hiding),
                    "qualified" => Some(Keyword::Qualified),
                    "as" => Some(Keyword::As),
                    "foreign" => Some(Keyword::Foreign),
                    "type" => Some(Keyword::Type),
                    "data" => Some(Keyword::Data),
                    "class" => Some(Keyword::Class),
                    "instance" => Some(Keyword::Instance),
                    "if" => Some(Keyword::If),
                    "else" => Some(Keyword::Else),
                    "then" => Some(Keyword::Then),
                    "true" => Some(Keyword::True),
                    "false" => Some(Keyword::False),
                    "let" => Some(Keyword::Let),
                    "where" => Some(Keyword::Where),
                    "do" => Some(Keyword::Do),
                    "in" => Some(Keyword::In),
                    "const" => Some(Keyword::Const),
                    "struct" => Some(Keyword::Struct),
                    "case" => Some(Keyword::Case),
                    "of" => Some(Keyword::Of),
                    "Matrix2" | "Matrix2x2" => Some(Keyword::Matrix2),
                    "Matrix2x3" => Some(Keyword::Matrix2x3),
                    "Matrix2x4" => Some(Keyword::Matrix2x4),
                    "Matrix3" | "Matirx3x3" => Some(Keyword::Matrix3),
                    "Matrix3x2" => Some(Keyword::Matrix3x2),
                    "Matrix3x4" => Some(Keyword::Matrix3x4),
                    "Matrix4" | "Matrix4x4" => Some(Keyword::Matrix4),
                    "Matrix4x2" => Some(Keyword::Matrix4x2),
                    "Matrix4x3" => Some(Keyword::Matrix4x3),
                    "Float" => Some(Keyword::Float),
                    "Int" => Some(Keyword::Int),
                    "UInt" => Some(Keyword::UInt),
                    "Vector2" => Some(Keyword::Vector2),
                    "Vector3" => Some(Keyword::Vector3),
                    "Vector4" => Some(Keyword::Vector4),
                    _ => None,
                };
                self.requires_block_opener_layout_marker =
                    kw.map_or(false, |kw| kw.introduce_block());

                let t = match kw {
                    Some(kw) => Token::Keyword(kw, self.spanize(b)),
                    None if self.src[self.pos..].starts_with(|c: char| c.is_uppercase()) => {
                        Token::LargeStartIdentifier(self.spanize(b))
                    }
                    None => Token::SmallStartIdentifier(self.spanize(b)),
                };
                self.pos += b;

                t
            }
            Some(c) => return Err(TokenizeError::UnexpectedCharacter(c, self.pos)),
        };

        let is_first_lexeme = self.first_lexeme;
        self.first_lexeme = false;

        if is_first_lexeme
            && !matches!(t, Token::BlockStart(_) | Token::Keyword(Keyword::Module, _))
        {
            Ok(TokenGenerator::with_preceded_layout_marker(
                Token::LayoutMarkerBlockOpener(head_span.calc_position().left, head_span),
                t,
            ))
        } else if insert_block_opener_marker {
            // disable next indentation
            self.preceding_whitespaces = None;
            Ok(TokenGenerator::with_preceded_layout_marker(
                Token::LayoutMarkerBlockOpener(head_span.calc_position().left, head_span),
                t,
            ))
        } else if let Some(ind) = self.preceding_whitespaces.take() {
            Ok(TokenGenerator::with_preceded_layout_marker(
                Token::LayoutMarkerIndentation(ind + 1, head_span),
                t,
            ))
        } else {
            Ok(TokenGenerator::new(t))
        }
    }

    fn try_read_block_comment(&self, start_from: usize) -> Result<usize, TokenizeError> {
        let mut c = 0;

        while start_from + c <= self.src.len() {
            if self.src[start_from + c..].starts_with("{-") {
                // nested
                let l = self.try_read_block_comment(start_from + c + 2)?;
                c += 2 + l;
            }
            if self.src[start_from + c..].starts_with("-}") {
                return Ok(c + 2);
            }
            c += self.src[start_from + c..]
                .chars()
                .next()
                .expect("stack exhausted?")
                .len_utf8();
        }

        Err(TokenizeError::UnclosedBlockComment(start_from))
    }

    fn try_parse_string_main(
        &self,
        mut iter: std::str::Chars,
        b: usize,
    ) -> Result<usize, TokenizeError> {
        match iter.next() {
            Some('"') => Ok(b),
            Some('\\') => self.try_parse_string_escaped(iter, b + 1),
            Some('\n') => Err(TokenizeError::UnexpectedCharacter('\n', self.pos + b)),
            Some(ch) => self.try_parse_string_main(iter, b + ch.len_utf8()),
            None => Err(TokenizeError::UnclosedString(self.pos)),
        }
    }
    fn try_parse_string_escaped(
        &self,
        mut iter: std::str::Chars,
        b: usize,
    ) -> Result<usize, TokenizeError> {
        match iter.next() {
            Some('u') => unimplemented!("unicode codepoint"),
            Some('\n') => Err(TokenizeError::UnexpectedCharacter('\n', self.pos + b)),
            Some(ch) => self.try_parse_string_main(iter, b + ch.len_utf8()),
            None => Err(TokenizeError::UnclosedString(self.pos)),
        }
    }
}
impl<'s> IntoIterator for Lexicalizer<'s> {
    type IntoIter = LexIter<'s>;
    type Item = <LexIter<'s> as Iterator>::Item;

    fn into_iter(self) -> LexIter<'s> {
        LexIter {
            lexicalizer: self,
            pending: None,
        }
    }
}

pub struct LexIter<'s> {
    lexicalizer: Lexicalizer<'s>,
    pending: Option<TokenGenerator<'s>>,
}
impl<'s> Iterator for LexIter<'s> {
    type Item = Result<Token<'s>, TokenizeError>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(mut p) = self.pending.take() {
            if let Some(rval) = p.next() {
                self.pending = Some(p);
                return Some(Ok(rval));
            }
        }

        match self.lexicalizer.parse_next() {
            Ok(TokenGenerator::Empty) => None,
            Ok(gen) => {
                self.pending = Some(gen);
                self.next()
            }
            Err(e) => Some(Err(e)),
        }
    }
}
