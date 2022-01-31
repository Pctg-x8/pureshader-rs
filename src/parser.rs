use crate::lexicalizer::{Span, Token, TokenizeError};

pub enum ParseError {
    Tokenize(TokenizeError),
    UnexpectedEndToken,
    UnexpectedToken(usize),
}
pub type ParseResult<T> = Result<T, ParseError>;
impl From<TokenizeError> for ParseError {
    fn from(v: TokenizeError) -> Self {
        Self::Tokenize(v)
    }
}

pub struct ParserState<'t, 's> {
    tokens: &'t [Token<'s>],
    pos: usize,
    max_pos: usize,
}
impl<'t, 's> ParserState<'t, 's> {
    pub fn new(tokens: &'t [Token<'s>]) -> Self {
        Self {
            tokens,
            pos: 0,
            max_pos: 0,
        }
    }

    pub fn update_max_pos(&mut self) {
        self.max_pos = self.max_pos.max(self.pos);
    }

    pub fn branch(&self) -> Self {
        Self {
            tokens: self.tokens,
            pos: self.pos,
            max_pos: self.max_pos,
        }
    }
    pub fn unwind(&mut self, saved: Self) {
        self.tokens = saved.tokens;
        self.pos = saved.pos;
        // max_posは戻さない（エラー表示用のやつなので）
    }

    pub fn head(&self) -> Option<&'t Token<'s>> {
        self.tokens.get(self.pos)
    }
    pub fn consume(&mut self) {
        self.pos += 1;
    }
}

fn parse_modid<'s>(state: &mut ParserState<'_, 's>) -> ParseResult<Vec<Span<'s>>> {
    let mut tok = match state.head().ok_or(ParseError::UnexpectedEndToken)? {
        Token::LargeStartIdentifier(s) => {
            state.consume();
            vec![s.clone()]
        }
        _ => {
            state.update_max_pos();
            return Err(ParseError::UnexpectedToken(state.pos));
        }
    };

    loop {
        match state.head() {
            Some(Token::Op(s)) if s.as_str() == "." => state.consume(),
            _ => break Ok(tok),
        };

        match state.head().ok_or(ParseError::UnexpectedEndToken)? {
            Token::LargeStartIdentifier(s) => {
                state.consume();
                tok.push(s.clone());
            }
            _ => {
                state.update_max_pos();
                return Err(ParseError::UnexpectedToken(state.pos));
            }
        }
    }
}

pub enum ImportSpec<'s> {
    Var(Span<'s>),
    Type(Span<'s>, Vec<Span<'s>>),
    Class(Span<'s>, Vec<Span<'s>>),
    TypeOrClassOnly(Span<'s>),
    TypeOrClassWithAllSubSymbols(Span<'s>),
}
impl<'s> ImportSpec<'s> {
    pub fn parse(state: &mut ParserState<'_, 's>) -> ParseResult<Self> {
        match state.head().ok_or(ParseError::UnexpectedEndToken)? {
            Token::SmallStartIdentifier(s) => {
                state.consume();
                Ok(Self::Var(s.clone()))
            }
            Token::LargeStartIdentifier(s) => {
                state.consume();
                while matches!(state.head(), Some(Token::Punct(_))) {
                    state.consume();
                }
                match state.head() {
                    Some(Token::LeftParenthese(_)) => state.consume(),
                    _ => return Ok(Self::TypeOrClassOnly(s.clone())),
                }

                while matches!(state.head(), Some(Token::Punct(_))) {
                    state.consume();
                }
                let node = match state.head().ok_or(ParseError::UnexpectedEndToken)? {
                    Token::Op(s) if s.as_str() == ".." => {
                        state.consume();

                        Self::TypeOrClassWithAllSubSymbols(s.clone())
                    }
                    Token::LargeStartIdentifier(s1) => {
                        // data constructors
                        let mut ctors = vec![s1.clone()];

                        loop {
                            while matches!(state.head(), Some(Token::Punct(_))) {
                                state.consume();
                            }
                            match state.head() {
                                Some(Token::Comma(_)) => {
                                    state.consume();
                                }
                                _ => break,
                            };

                            while matches!(state.head(), Some(Token::Punct(_))) {
                                state.consume();
                            }
                            match state.head() {
                                Some(Token::LargeStartIdentifier(s)) => {
                                    state.consume();
                                    ctors.push(s.clone());
                                }
                                _ => break,
                            }
                        }

                        Self::Type(s.clone(), ctors)
                    }
                    Token::SmallStartIdentifier(s1) => {
                        // class members
                        let mut members = vec![s1.clone()];

                        loop {
                            while matches!(state.head(), Some(Token::Punct(_))) {
                                state.consume();
                            }
                            match state.head() {
                                Some(Token::Comma(_)) => {
                                    state.consume();
                                }
                                _ => break,
                            };

                            while matches!(state.head(), Some(Token::Punct(_))) {
                                state.consume();
                            }
                            match state.head() {
                                Some(Token::SmallStartIdentifier(s)) => {
                                    state.consume();
                                    members.push(s.clone());
                                }
                                _ => break,
                            }
                        }

                        Self::Class(s.clone(), members)
                    }
                    _ => {
                        state.update_max_pos();
                        return Err(ParseError::UnexpectedToken(state.pos));
                    }
                };

                while matches!(state.head(), Some(Token::Punct(_))) {
                    state.consume();
                }
                if !matches!(
                    state.head().ok_or(ParseError::UnexpectedEndToken)?,
                    Token::RightParenthese(_)
                ) {
                    state.update_max_pos();
                    return Err(ParseError::UnexpectedToken(state.pos));
                }
                state.consume();

                Ok(node)
            }
            _ => {
                state.update_max_pos();
                Err(ParseError::UnexpectedToken(state.pos))
            }
        }
    }
}

pub struct ImportDecl<'s> {
    pub qualified: bool,
    pub id: Vec<Span<'s>>,
    pub as_name: Option<Vec<Span<'s>>>,
    pub hiding: bool,
    pub specs: Vec<ImportSpec<'s>>,
}
impl<'s> ImportDecl<'s> {
    pub fn parse(state: &mut ParserState<'_, 's>) -> ParseResult<Self> {
        if !matches!(
            state.head().ok_or(ParseError::UnexpectedEndToken)?,
            Token::Import(_)
        ) {
            state.update_max_pos();
            return Err(ParseError::UnexpectedToken(state.pos));
        }
        state.consume();

        while matches!(state.head(), Some(Token::Punct(_))) {
            state.consume();
        }

        let qualified = match state.head() {
            Some(Token::Qualified(_)) => {
                state.consume();
                true
            }
            _ => false,
        };

        while matches!(state.head(), Some(Token::Punct(_))) {
            state.consume();
        }

        let id = parse_modid(state)?;

        while matches!(state.head(), Some(Token::Punct(_))) {
            state.consume();
        }

        let as_name = match state.head() {
            Some(Token::As(_)) => {
                state.consume();

                while matches!(state.head(), Some(Token::Punct(_))) {
                    state.consume();
                }

                Some(parse_modid(state)?)
            }
            _ => None,
        };

        while matches!(state.head(), Some(Token::Punct(_))) {
            state.consume();
        }

        let hiding = match state.head() {
            Some(Token::Hiding(_)) => {
                state.consume();
                true
            }
            _ => false,
        };

        while matches!(state.head(), Some(Token::Punct(_))) {
            state.consume();
        }

        let specs = match state.head() {
            Some(Token::LeftParenthese(_)) => {
                state.consume();

                while matches!(state.head(), Some(Token::Punct(_))) {
                    state.consume();
                }

                let mut specs = vec![ImportSpec::parse(state)?];

                loop {
                    while matches!(state.head(), Some(Token::Punct(_))) {
                        state.consume();
                    }

                    match state.head() {
                        Some(Token::Comma(_)) => {
                            state.consume();
                        }
                        _ => break,
                    }

                    while matches!(state.head(), Some(Token::Punct(_))) {
                        state.consume();
                    }

                    let s = state.branch();
                    match ImportSpec::parse(state) {
                        Ok(s) => {
                            specs.push(s);
                        }
                        _ => {
                            state.unwind(s);
                            break;
                        }
                    }
                }

                while matches!(state.head(), Some(Token::Punct(_))) {
                    state.consume();
                }

                if !matches!(
                    state.head().ok_or(ParseError::UnexpectedEndToken)?,
                    Token::RightParenthese(_)
                ) {
                    state.update_max_pos();
                    return Err(ParseError::UnexpectedToken(state.pos));
                }

                specs
            }
            _ => Vec::new(),
        };

        Ok(Self {
            qualified,
            id,
            as_name,
            hiding,
            specs,
        })
    }
}
