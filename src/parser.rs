use crate::{
    layout::{Layout, LayoutContext},
    lexicalizer::{Keyword, Span, Token, TokenizeError},
};

#[derive(Debug)]
pub enum ParseError {
    Tokenize(TokenizeError),
    UnexpectedEndToken,
    UnexpectedToken(usize),
    LayoutError,
}
pub type ParseResult<T> = Result<T, ParseError>;
impl From<TokenizeError> for ParseError {
    fn from(v: TokenizeError) -> Self {
        Self::Tokenize(v)
    }
}

pub struct ParserState<'t, 's> {
    pub tokens: TokenPointer<'t, 's>,
    pub layout: LayoutContext,
}
impl<'t, 's> ParserState<'t, 's> {
    pub fn new(tokens: &'t [Token<'s>]) -> Self {
        Self {
            tokens: TokenPointer::new(tokens),
            layout: LayoutContext::new(),
        }
    }
}
impl<'s> ParserState<'_, 's> {
    pub fn skip_puncts(&mut self) {
        while matches!(self.tokens.head(), Some(Token::Punct(_))) {
            self.tokens.consume();
        }
    }

    pub fn try_open_block(&mut self) -> ParseResult<()> {
        match self.tokens.head() {
            Some(Token::BlockStart(_)) => {
                self.tokens.consume();
                self.layout.shift(Layout::Explicit);
                Ok(())
            }
            Some(&Token::LayoutMarkerBlockOpener(n, _)) => match self.layout.current() {
                Some(Layout::Implicit(m)) if n > m => {
                    self.tokens.consume();
                    self.layout.shift(Layout::Implicit(n));
                    Ok(())
                }
                Some(Layout::Explicit) => {
                    self.tokens.consume();
                    self.layout.shift(Layout::Implicit(n));
                    Ok(())
                }
                Some(_) => {
                    unimplemented!("replacing current layout marker");
                }
                None => {
                    self.tokens.consume();
                    self.layout.shift(Layout::Implicit(n));
                    Ok(())
                }
            },
            Some(_) => Err(ParseError::UnexpectedToken(self.tokens.pos)),
            None => Err(ParseError::UnexpectedEndToken),
        }
    }

    pub fn try_close_block(&mut self) -> ParseResult<()> {
        match self.tokens.head() {
            Some(Token::BlockEnd(_)) => match self.layout.current() {
                Some(Layout::Explicit) => {
                    self.tokens.consume();
                    self.layout.unshift();
                    Ok(())
                }
                _ => Err(ParseError::UnexpectedToken(self.tokens.pos)),
            },
            Some(&Token::LayoutMarkerIndentation(n, _)) => match self.layout.current() {
                Some(Layout::Implicit(m)) if n < m => {
                    self.layout.unshift();
                    Ok(())
                }
                _ => Err(ParseError::UnexpectedToken(self.tokens.pos)),
            },
            Some(_) => Err(ParseError::UnexpectedToken(self.tokens.pos)),
            None => match self.layout.current() {
                Some(_) => {
                    self.layout.unshift();
                    Ok(())
                }
                _ => Err(ParseError::UnexpectedEndToken),
            },
        }
    }

    pub fn try_close_block_by_parse_error(&mut self) -> ParseResult<()> {
        match self.layout.current() {
            Some(Layout::Implicit(_)) => {
                self.layout.unshift();
                Ok(())
            }
            _ => Err(ParseError::LayoutError),
        }
    }

    pub fn try_separate(&mut self) -> ParseResult<()> {
        match self.tokens.head() {
            Some(Token::Semicolon(_)) => {
                self.tokens.consume();
                Ok(())
            }
            Some(&Token::LayoutMarkerIndentation(n, _)) => match self.layout.current() {
                Some(Layout::Implicit(m)) if n == m => {
                    self.tokens.consume();
                    Ok(())
                }
                _ => Err(ParseError::LayoutError),
            },
            Some(_) => Err(ParseError::UnexpectedToken(self.tokens.pos)),
            None => Err(ParseError::UnexpectedEndToken),
        }
    }

    pub fn branch(&self) -> Self {
        Self {
            tokens: self.tokens.branch(),
            layout: self.layout.clone(),
        }
    }
    pub fn unwind(&mut self, saved: Self) {
        self.tokens.unwind(saved.tokens);
        self.layout = saved.layout;
    }
}

pub struct TokenPointer<'t, 's> {
    tokens: &'t [Token<'s>],
    pos: usize,
    max_pos: usize,
}
impl<'t, 's> TokenPointer<'t, 's> {
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
    let mut tok = match state.tokens.head().ok_or(ParseError::UnexpectedEndToken)? {
        Token::LargeStartIdentifier(s) => {
            state.tokens.consume();
            vec![s.clone()]
        }
        _ => {
            state.tokens.update_max_pos();
            return Err(ParseError::UnexpectedToken(state.tokens.pos));
        }
    };

    loop {
        match state.tokens.head() {
            Some(Token::Op(s)) if s.as_str() == "." => state.tokens.consume(),
            _ => break Ok(tok),
        };

        match state.tokens.head().ok_or(ParseError::UnexpectedEndToken)? {
            Token::LargeStartIdentifier(s) => {
                state.tokens.consume();
                tok.push(s.clone());
            }
            _ => {
                state.tokens.update_max_pos();
                return Err(ParseError::UnexpectedToken(state.tokens.pos));
            }
        }
    }
}

#[derive(Debug)]
pub enum ImportSpec<'s> {
    Var(Span<'s>),
    Type(Span<'s>, Vec<Span<'s>>),
    Class(Span<'s>, Vec<Span<'s>>),
    TypeOrClassOnly(Span<'s>),
    TypeOrClassWithAllSubSymbols(Span<'s>),
}
impl<'s> ImportSpec<'s> {
    pub fn parse(state: &mut ParserState<'_, 's>) -> ParseResult<Self> {
        match state.tokens.head().ok_or(ParseError::UnexpectedEndToken)? {
            Token::SmallStartIdentifier(s) => {
                state.tokens.consume();
                Ok(Self::Var(s.clone()))
            }
            Token::LargeStartIdentifier(s) => {
                state.tokens.consume();
                while matches!(state.tokens.head(), Some(Token::Punct(_))) {
                    state.tokens.consume();
                }
                match state.tokens.head() {
                    Some(Token::LeftParenthese(_)) => state.tokens.consume(),
                    _ => return Ok(Self::TypeOrClassOnly(s.clone())),
                }

                while matches!(state.tokens.head(), Some(Token::Punct(_))) {
                    state.tokens.consume();
                }
                let node = match state.tokens.head().ok_or(ParseError::UnexpectedEndToken)? {
                    Token::Op(s) if s.as_str() == ".." => {
                        state.tokens.consume();

                        Self::TypeOrClassWithAllSubSymbols(s.clone())
                    }
                    Token::LargeStartIdentifier(s1) => {
                        // data constructors
                        let mut ctors = vec![s1.clone()];

                        loop {
                            while matches!(state.tokens.head(), Some(Token::Punct(_))) {
                                state.tokens.consume();
                            }
                            match state.tokens.head() {
                                Some(Token::Comma(_)) => {
                                    state.tokens.consume();
                                }
                                _ => break,
                            };

                            while matches!(state.tokens.head(), Some(Token::Punct(_))) {
                                state.tokens.consume();
                            }
                            match state.tokens.head() {
                                Some(Token::LargeStartIdentifier(s)) => {
                                    state.tokens.consume();
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
                            while matches!(state.tokens.head(), Some(Token::Punct(_))) {
                                state.tokens.consume();
                            }
                            match state.tokens.head() {
                                Some(Token::Comma(_)) => {
                                    state.tokens.consume();
                                }
                                _ => break,
                            };

                            while matches!(state.tokens.head(), Some(Token::Punct(_))) {
                                state.tokens.consume();
                            }
                            match state.tokens.head() {
                                Some(Token::SmallStartIdentifier(s)) => {
                                    state.tokens.consume();
                                    members.push(s.clone());
                                }
                                _ => break,
                            }
                        }

                        Self::Class(s.clone(), members)
                    }
                    _ => {
                        state.tokens.update_max_pos();
                        return Err(ParseError::UnexpectedToken(state.tokens.pos));
                    }
                };

                while matches!(state.tokens.head(), Some(Token::Punct(_))) {
                    state.tokens.consume();
                }
                if !matches!(
                    state.tokens.head().ok_or(ParseError::UnexpectedEndToken)?,
                    Token::RightParenthese(_)
                ) {
                    state.tokens.update_max_pos();
                    return Err(ParseError::UnexpectedToken(state.tokens.pos));
                }
                state.tokens.consume();

                Ok(node)
            }
            _ => {
                state.tokens.update_max_pos();
                Err(ParseError::UnexpectedToken(state.tokens.pos))
            }
        }
    }
}

#[derive(Debug)]
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
            state.tokens.head().ok_or(ParseError::UnexpectedEndToken)?,
            Token::Keyword(Keyword::Import, _)
        ) {
            state.tokens.update_max_pos();
            return Err(ParseError::UnexpectedToken(state.tokens.pos));
        }
        state.tokens.consume();

        while matches!(state.tokens.head(), Some(Token::Punct(_))) {
            state.tokens.consume();
        }

        let qualified = match state.tokens.head() {
            Some(Token::Keyword(Keyword::Qualified, _)) => {
                state.tokens.consume();
                true
            }
            _ => false,
        };

        while matches!(state.tokens.head(), Some(Token::Punct(_))) {
            state.tokens.consume();
        }

        let id = parse_modid(state)?;

        while matches!(state.tokens.head(), Some(Token::Punct(_))) {
            state.tokens.consume();
        }

        let as_name = match state.tokens.head() {
            Some(Token::Keyword(Keyword::As, _)) => {
                state.tokens.consume();

                while matches!(state.tokens.head(), Some(Token::Punct(_))) {
                    state.tokens.consume();
                }

                Some(parse_modid(state)?)
            }
            _ => None,
        };

        while matches!(state.tokens.head(), Some(Token::Punct(_))) {
            state.tokens.consume();
        }

        let hiding = match state.tokens.head() {
            Some(Token::Keyword(Keyword::Hiding, _)) => {
                state.tokens.consume();
                true
            }
            _ => false,
        };

        while matches!(state.tokens.head(), Some(Token::Punct(_))) {
            state.tokens.consume();
        }

        let specs = match state.tokens.head() {
            Some(Token::LeftParenthese(_)) => {
                state.tokens.consume();

                while matches!(state.tokens.head(), Some(Token::Punct(_))) {
                    state.tokens.consume();
                }

                let mut specs = vec![ImportSpec::parse(state)?];

                loop {
                    while matches!(state.tokens.head(), Some(Token::Punct(_))) {
                        state.tokens.consume();
                    }

                    match state.tokens.head() {
                        Some(Token::Comma(_)) => {
                            state.tokens.consume();
                        }
                        _ => break,
                    }

                    while matches!(state.tokens.head(), Some(Token::Punct(_))) {
                        state.tokens.consume();
                    }

                    let s = state.tokens.branch();
                    match ImportSpec::parse(state) {
                        Ok(s) => {
                            specs.push(s);
                        }
                        _ => {
                            state.tokens.unwind(s);
                            break;
                        }
                    }
                }

                while matches!(state.tokens.head(), Some(Token::Punct(_))) {
                    state.tokens.consume();
                }

                if !matches!(
                    state.tokens.head().ok_or(ParseError::UnexpectedEndToken)?,
                    Token::RightParenthese(_)
                ) {
                    state.tokens.update_max_pos();
                    return Err(ParseError::UnexpectedToken(state.tokens.pos));
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

#[derive(Debug)]
pub struct Body<'s> {
    imports: Vec<ImportDecl<'s>>,
}
impl<'s> Body<'s> {
    pub fn parse(state: &mut ParserState<'_, 's>) -> ParseResult<Self> {
        state.try_open_block()?;

        let mut imports = Vec::new();
        loop {
            let saved = state.branch();

            while matches!(state.tokens.head(), Some(Token::Punct(_))) {
                state.tokens.consume();
            }

            let import = match ImportDecl::parse(state) {
                Ok(x) => x,
                Err(_) => {
                    state.unwind(saved);
                    break;
                }
            };

            while matches!(state.tokens.head(), Some(Token::Punct(_))) {
                state.tokens.consume();
            }

            match state.try_separate() {
                Ok(_) => (),
                Err(_) => {
                    state.unwind(saved);
                    break;
                }
            }

            imports.push(import);
        }

        state.try_close_block()?;
        Ok(Self { imports })
    }
}

#[derive(Debug)]
pub enum ModuleExports<'s> {
    Var(Span<'s>),
    TypeOrClassOnly(Span<'s>),
    TypeOrClassAll(Span<'s>),
    Type(Span<'s>, Vec<Span<'s>>),
    Class(Span<'s>, Vec<Span<'s>>),
    Module(Vec<Span<'s>>),
}
impl<'s> ModuleExports<'s> {
    pub fn parse(state: &mut ParserState<'_, 's>) -> ParseResult<Self> {
        match state.tokens.head().ok_or(ParseError::UnexpectedEndToken)? {
            Token::Keyword(Keyword::Module, _) => {
                state.tokens.consume();

                while matches!(state.tokens.head(), Some(Token::Punct(_))) {
                    state.tokens.consume();
                }

                let id = parse_modid(state)?;

                Ok(Self::Module(id))
            }
            Token::SmallStartIdentifier(s) => {
                state.tokens.consume();
                Ok(Self::Var(s.clone()))
            }
            Token::LargeStartIdentifier(s) => {
                state.tokens.consume();
                while matches!(state.tokens.head(), Some(Token::Punct(_))) {
                    state.tokens.consume();
                }
                match state.tokens.head() {
                    Some(Token::LeftParenthese(_)) => state.tokens.consume(),
                    _ => return Ok(Self::TypeOrClassOnly(s.clone())),
                }

                while matches!(state.tokens.head(), Some(Token::Punct(_))) {
                    state.tokens.consume();
                }
                let node = match state.tokens.head().ok_or(ParseError::UnexpectedEndToken)? {
                    Token::Op(s) if s.as_str() == ".." => {
                        state.tokens.consume();

                        Self::TypeOrClassAll(s.clone())
                    }
                    Token::LargeStartIdentifier(s1) => {
                        // data constructors
                        let mut ctors = vec![s1.clone()];

                        loop {
                            while matches!(state.tokens.head(), Some(Token::Punct(_))) {
                                state.tokens.consume();
                            }
                            match state.tokens.head() {
                                Some(Token::Comma(_)) => {
                                    state.tokens.consume();
                                }
                                _ => break,
                            };

                            while matches!(state.tokens.head(), Some(Token::Punct(_))) {
                                state.tokens.consume();
                            }
                            match state.tokens.head() {
                                Some(Token::LargeStartIdentifier(s)) => {
                                    state.tokens.consume();
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
                            while matches!(state.tokens.head(), Some(Token::Punct(_))) {
                                state.tokens.consume();
                            }
                            match state.tokens.head() {
                                Some(Token::Comma(_)) => {
                                    state.tokens.consume();
                                }
                                _ => break,
                            };

                            while matches!(state.tokens.head(), Some(Token::Punct(_))) {
                                state.tokens.consume();
                            }
                            match state.tokens.head() {
                                Some(Token::SmallStartIdentifier(s)) => {
                                    state.tokens.consume();
                                    members.push(s.clone());
                                }
                                _ => break,
                            }
                        }

                        Self::Class(s.clone(), members)
                    }
                    _ => {
                        state.tokens.update_max_pos();
                        return Err(ParseError::UnexpectedToken(state.tokens.pos));
                    }
                };

                while matches!(state.tokens.head(), Some(Token::Punct(_))) {
                    state.tokens.consume();
                }
                if !matches!(
                    state.tokens.head().ok_or(ParseError::UnexpectedEndToken)?,
                    Token::RightParenthese(_)
                ) {
                    state.tokens.update_max_pos();
                    return Err(ParseError::UnexpectedToken(state.tokens.pos));
                }
                state.tokens.consume();

                Ok(node)
            }
            _ => {
                state.tokens.update_max_pos();
                Err(ParseError::UnexpectedToken(state.tokens.pos))
            }
        }
    }
}

#[derive(Debug)]
pub struct ModuleHeader<'s> {
    id: Vec<Span<'s>>,
    exports: Option<Vec<ModuleExports<'s>>>,
}
impl<'s> ModuleHeader<'s> {
    pub fn parse(state: &mut ParserState<'_, 's>) -> ParseResult<Self> {
        if !matches!(
            state.tokens.head().ok_or(ParseError::UnexpectedEndToken)?,
            Token::Keyword(Keyword::Module, _)
        ) {
            state.tokens.update_max_pos();
            return Err(ParseError::UnexpectedToken(state.tokens.pos));
        }
        state.tokens.consume();

        while matches!(state.tokens.head(), Some(Token::Punct(_))) {
            state.tokens.consume();
        }

        let id = parse_modid(state)?;

        while matches!(state.tokens.head(), Some(Token::Punct(_))) {
            state.tokens.consume();
        }

        let exports = match state.tokens.head() {
            Some(Token::LeftParenthese(_)) => {
                state.tokens.consume();

                while matches!(state.tokens.head(), Some(Token::Punct(_))) {
                    state.tokens.consume();
                }

                let mut specs = vec![ModuleExports::parse(state)?];

                loop {
                    while matches!(state.tokens.head(), Some(Token::Punct(_))) {
                        state.tokens.consume();
                    }

                    match state.tokens.head() {
                        Some(Token::Comma(_)) => {
                            state.tokens.consume();
                        }
                        _ => break,
                    }

                    while matches!(state.tokens.head(), Some(Token::Punct(_))) {
                        state.tokens.consume();
                    }

                    let s = state.tokens.branch();
                    match ModuleExports::parse(state) {
                        Ok(s) => {
                            specs.push(s);
                        }
                        _ => {
                            state.tokens.unwind(s);
                            break;
                        }
                    }
                }

                while matches!(state.tokens.head(), Some(Token::Punct(_))) {
                    state.tokens.consume();
                }

                if !matches!(
                    state.tokens.head().ok_or(ParseError::UnexpectedEndToken)?,
                    Token::RightParenthese(_)
                ) {
                    state.tokens.update_max_pos();
                    return Err(ParseError::UnexpectedToken(state.tokens.pos));
                }

                Some(specs)
            }
            _ => None,
        };

        Ok(Self { id, exports })
    }
}

#[derive(Debug)]
pub struct Module<'s> {
    head: Option<ModuleHeader<'s>>,
    body: Body<'s>,
}
impl<'s> Module<'s> {
    pub fn parse(state: &mut ParserState<'_, 's>) -> ParseResult<Self> {
        let head = match state.tokens.head() {
            Some(Token::Keyword(Keyword::Module, _)) => {
                let h = ModuleHeader::parse(state)?;

                while matches!(state.tokens.head(), Some(Token::Punct(_))) {
                    state.tokens.consume();
                }

                if !matches!(
                    state.tokens.head().ok_or(ParseError::UnexpectedEndToken)?,
                    Token::Keyword(Keyword::Where, _)
                ) {
                    state.tokens.update_max_pos();
                    return Err(ParseError::UnexpectedToken(state.tokens.pos));
                }
                state.tokens.consume();

                Some(h)
            }
            _ => None,
        };

        while matches!(state.tokens.head(), Some(Token::Punct(_))) {
            state.tokens.consume();
        }

        let body = Body::parse(state)?;

        Ok(Self { head, body })
    }
}
