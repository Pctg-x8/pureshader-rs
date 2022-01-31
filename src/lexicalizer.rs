#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Position {
    left: usize,
    line: usize,
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
    pub fn as_str(&self) -> &'s str {
        &self.source[self.pos..self.pos + self.len]
    }

    pub fn calc_position(&self) -> Position {
        let line = 1 + self.source[..self.pos]
            .chars()
            .filter(|&c| c == '\n')
            .count();
        let left = self.source[..self.pos]
            .chars()
            .rev()
            .take_while(|&c| c != '\n')
            .count();

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
        write!(fmt, "Span({})", self.as_str())
    }
}

#[derive(Debug, Clone)]
pub enum Token<'s> {
    Punct(Span<'s>),
    LargeStartIdentifier(Span<'s>),
    SmallStartIdentifier(Span<'s>),
    StringLiteral(Span<'s>),
    NumberLiteral {
        ipart: Span<'s>,
        fpart: Option<Span<'s>>,
        epart: Option<Span<'s>>,
        float_ty: bool,
        unsigned_ty: bool,
        long_ty: bool,
    },
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
    Comma(Span<'s>),
    Arrow(Span<'s>),
    FatArrow(Span<'s>),
    Backslash(Span<'s>),
    Module(Span<'s>),
    Import(Span<'s>),
    Hiding(Span<'s>),
    Foreign(Span<'s>),
    Type(Span<'s>),
    Data(Span<'s>),
    Class(Span<'s>),
    Instance(Span<'s>),
    Struct(Span<'s>),
    Const(Span<'s>),
    If(Span<'s>),
    Else(Span<'s>),
    Then(Span<'s>),
    True(Span<'s>),
    False(Span<'s>),
    Let(Span<'s>),
    Where(Span<'s>),
    Do(Span<'s>),
    In(Span<'s>),
    Qualified(Span<'s>),
    As(Span<'s>),
    Case(Span<'s>),
    Of(Span<'s>),
    Matrix2(Span<'s>),
    Matrix3(Span<'s>),
    Matrix4(Span<'s>),
    Float(Span<'s>),
    Int(Span<'s>),
    UInt(Span<'s>),
    Vector2(Span<'s>),
    Vector3(Span<'s>),
    Vector4(Span<'s>),
    Matrix2x3(Span<'s>),
    Matrix2x4(Span<'s>),
    Matrix3x2(Span<'s>),
    Matrix3x4(Span<'s>),
    Matrix4x2(Span<'s>),
    Matrix4x3(Span<'s>),
}

#[derive(Debug)]
pub enum TokenizeError {
    UnexpectedCharacter(char, usize),
    UnclosedBlockComment(usize),
    UnclosedBackquotedIdentifier(usize),
    UnclosedString(usize),
}

pub struct Lexicalizer<'s> {
    src: &'s str,
    pos: usize,
}

impl<'s> Lexicalizer<'s> {
    pub fn new(s: &'s str) -> Self {
        Lexicalizer { src: s, pos: 0 }
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

    pub fn parse_next(&mut self) -> Result<Option<Token<'s>>, TokenizeError> {
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

            return Ok(Some(t));
        }

        if self.starts_with("{-") {
            let b = self.try_read_block_comment(self.pos + 2)?;
            let t = Token::Punct(self.spanize(2 + b));
            self.pos += 2 + b;

            return Ok(Some(t));
        }

        match self.head_char() {
            // EOF
            None => Ok(None),
            Some(c) if c == ' ' || c == '\n' || c == '\t' || c.is_whitespace() => {
                let b = self.src[self.pos..]
                    .chars()
                    .take_while(|&c| c == ' ' || c == '\n' || c == '\t' || c.is_whitespace())
                    .fold(0, |a, c| a + c.len_utf8());

                let t = Token::Punct(self.spanize(b));
                self.pos += b;

                Ok(Some(t))
            }
            Some('(') => {
                let t = Token::LeftParenthese(self.spanize(1));
                self.pos += 1;

                Ok(Some(t))
            }
            Some(')') => {
                let t = Token::RightParenthese(self.spanize(1));
                self.pos += 1;

                Ok(Some(t))
            }
            Some('[') => {
                let t = Token::LeftBracket(self.spanize(1));
                self.pos += 1;

                Ok(Some(t))
            }
            Some(']') => {
                let t = Token::RightBracket(self.spanize(1));
                self.pos += 1;

                Ok(Some(t))
            }
            Some('{') => {
                let t = Token::BlockStart(self.spanize(1));
                self.pos += 1;

                Ok(Some(t))
            }
            Some('}') => {
                let t = Token::BlockEnd(self.spanize(1));
                self.pos += 1;

                Ok(Some(t))
            }
            Some(',') => {
                let t = Token::Comma(self.spanize(1));
                self.pos += 1;

                Ok(Some(t))
            }
            Some('\\') => {
                let t = Token::Backslash(self.spanize(1));
                self.pos += 1;

                Ok(Some(t))
            }
            Some('`') => {
                let t = Token::Backquote(self.spanize(1));
                self.pos += 1;

                Ok(Some(t))
            }
            Some('"') => {
                let b = self.try_parse_string_main(self.src[self.pos + 1..].chars(), 0)?;
                self.pos += 1;
                let t = Token::StringLiteral(self.spanize(b));
                self.pos += b + 1;

                Ok(Some(t))
            }
            Some(c) if ('0'..='9').contains(&c) => {
                let ipart_b = self
                    .src
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

                let t = Token::NumberLiteral {
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
                };
                self.pos += suffixpart_start + suffixpart_b;

                Ok(Some(t))
            }
            Some(c) if OPCHARACTERS.iter().any(|&a| a == c) => {
                let b = self
                    .src
                    .chars()
                    .take_while(|&c| OPCHARACTERS.iter().any(|&a| a == c))
                    .fold(0, |a, c| a + c.len_utf8());
                let t = match &self.src[..b] {
                    "=" => Token::Equal(self.spanize(1)),
                    "::" => Token::Typing(self.spanize(2)),
                    "->" => Token::Arrow(self.spanize(2)),
                    "=>" => Token::FatArrow(self.spanize(2)),
                    _ => Token::Op(self.spanize(b)),
                };
                self.pos += b;

                Ok(Some(t))
            }
            Some(c) if ('a'..='z').contains(&c) || ('A'..='Z').contains(&c) || c == '_' => {
                let b = self
                    .src
                    .chars()
                    .take_while(|&c| {
                        ('a'..='z').contains(&c)
                            || ('A'..='Z').contains(&c)
                            || ('0'..='9').contains(&c)
                            || c == '_'
                            || c == '\''
                    })
                    .fold(0, |a, c| a + c.len_utf8());

                let t = match &self.src[..b] {
                    "module" => Token::Module(self.spanize(b)),
                    "import" => Token::Import(self.spanize(b)),
                    "hiding" => Token::Hiding(self.spanize(b)),
                    "qualified" => Token::Qualified(self.spanize(b)),
                    "as" => Token::As(self.spanize(b)),
                    "foreign" => Token::Foreign(self.spanize(b)),
                    "type" => Token::Type(self.spanize(b)),
                    "data" => Token::Data(self.spanize(b)),
                    "class" => Token::Class(self.spanize(b)),
                    "instance" => Token::Instance(self.spanize(b)),
                    "if" => Token::If(self.spanize(b)),
                    "else" => Token::Else(self.spanize(b)),
                    "then" => Token::Then(self.spanize(b)),
                    "true" => Token::True(self.spanize(b)),
                    "false" => Token::False(self.spanize(b)),
                    "let" => Token::Let(self.spanize(b)),
                    "where" => Token::Where(self.spanize(b)),
                    "do" => Token::Do(self.spanize(b)),
                    "in" => Token::In(self.spanize(b)),
                    "const" => Token::Const(self.spanize(b)),
                    "struct" => Token::Struct(self.spanize(b)),
                    "case" => Token::Case(self.spanize(b)),
                    "of" => Token::Of(self.spanize(b)),
                    "Matrix2" | "Matrix2x2" => Token::Matrix2(self.spanize(b)),
                    "Matrix2x3" => Token::Matrix2x3(self.spanize(b)),
                    "Matrix2x4" => Token::Matrix2x4(self.spanize(b)),
                    "Matrix3" | "Matirx3x3" => Token::Matrix3(self.spanize(b)),
                    "Matrix3x2" => Token::Matrix3x2(self.spanize(b)),
                    "Matrix3x4" => Token::Matrix3x4(self.spanize(b)),
                    "Matrix4" | "Matrix4x4" => Token::Matrix4(self.spanize(b)),
                    "Matrix4x2" => Token::Matrix4x2(self.spanize(b)),
                    "Matrix4x3" => Token::Matrix4x3(self.spanize(b)),
                    "Float" => Token::Float(self.spanize(b)),
                    "Int" => Token::Int(self.spanize(b)),
                    "UInt" => Token::UInt(self.spanize(b)),
                    "Vector2" => Token::Vector2(self.spanize(b)),
                    "Vector3" => Token::Vector3(self.spanize(b)),
                    "Vector4" => Token::Vector4(self.spanize(b)),
                    s if s.starts_with(|c: char| c.is_uppercase()) => {
                        Token::LargeStartIdentifier(self.spanize(b))
                    }
                    s => Token::SmallStartIdentifier(self.spanize(b)),
                };
                self.pos += b;

                Ok(Some(t))
            }
            Some(c) => Err(TokenizeError::UnexpectedCharacter(c, self.pos)),
        }
    }

    fn try_read_block_comment(&self, start_from: usize) -> Result<usize, TokenizeError> {
        let mut c = 0;

        while self.src.len() < start_from + c {
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
impl<'s> Iterator for Lexicalizer<'s> {
    type Item = Result<Token<'s>, TokenizeError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.parse_next().transpose()
    }
}
