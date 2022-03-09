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
pub enum Type<'s> {
    Arrow(Box<Type<'s>>, Box<Type<'s>>),
    Application(Box<Type<'s>>, Box<Type<'s>>),
    Tuple(Vec<Type<'s>>),
    List(Box<Type<'s>>),
    Unit(Span<'s>),
    ListConstructor(Span<'s>),
    ArrowConstructor(Span<'s>),
    /// number of commas
    TupleConstructor(usize, Span<'s>),
    Int(Span<'s>),
    Float(Span<'s>),
    UInt(Span<'s>),
    VecConstructor(usize, Span<'s>),
    /// row dimensions, col dimensions
    MatConstructor(usize, usize, Span<'s>),
    GenericTyCon(Span<'s>),
    TyVar(Span<'s>),
}
impl<'s> Type<'s> {
    pub fn parse(state: &mut ParserState<'_, 's>) -> ParseResult<Self> {
        let tybase = Self::parse_base(state)?;

        state.skip_puncts();

        match state.tokens.head() {
            Some(Token::Arrow(_)) => {
                state.tokens.consume();

                state.skip_puncts();

                let tyret = Self::parse(state)?;

                Ok(Self::Arrow(Box::new(tybase), Box::new(tyret)))
            }
            _ => Ok(tybase),
        }
    }

    pub fn parse_base(state: &mut ParserState<'_, 's>) -> ParseResult<Self> {
        let mut ty = Self::parse_factor(state)?;

        loop {
            state.skip_puncts();

            let saved = state.tokens.branch();
            match Self::parse_factor(state) {
                Ok(arg) => {
                    ty = Self::Application(Box::new(ty), Box::new(arg));
                }
                _ => {
                    state.tokens.unwind(saved);
                    break;
                }
            }
        }

        Ok(ty)
    }

    pub fn parse_factor(state: &mut ParserState<'_, 's>) -> ParseResult<Self> {
        println!(
            "tyhead: {:?} {:?}",
            state.tokens.head(),
            state.tokens.head().map(|s| s.head_span().calc_position())
        );

        match state.tokens.head().ok_or(ParseError::UnexpectedEndToken)? {
            Token::LeftParenthese(shead) => {
                // unit, tuple, arrow ctor, tuple ctor or prioritized type construction

                state.tokens.consume();

                match state.tokens.head().ok_or(ParseError::UnexpectedEndToken)? {
                    Token::RightParenthese(_) => {
                        state.tokens.consume();

                        Ok(Self::Unit(shead.clone()))
                    }
                    Token::Arrow(_) => {
                        state.tokens.consume();

                        if !matches!(
                            state.tokens.head().ok_or(ParseError::UnexpectedEndToken)?,
                            Token::RightParenthese(_)
                        ) {
                            state.tokens.update_max_pos();

                            return Err(ParseError::UnexpectedToken(state.tokens.pos));
                        }
                        state.tokens.consume();

                        Ok(Self::ArrowConstructor(shead.clone()))
                    }
                    Token::Comma(_) => {
                        state.tokens.consume();

                        let mut cons_count = 1;
                        while matches!(
                            state.tokens.head().ok_or(ParseError::UnexpectedEndToken)?,
                            Token::Comma(_)
                        ) {
                            state.tokens.consume();
                            cons_count += 1;
                        }

                        if !matches!(
                            state.tokens.head().ok_or(ParseError::UnexpectedEndToken)?,
                            Token::RightParenthese(_)
                        ) {
                            state.tokens.update_max_pos();

                            return Err(ParseError::UnexpectedToken(state.tokens.pos));
                        }
                        state.tokens.consume();

                        Ok(Self::TupleConstructor(cons_count, shead.clone()))
                    }
                    _ => {
                        state.skip_puncts();

                        let ty1 = Self::parse(state)?;

                        state.skip_puncts();

                        match state.tokens.head().ok_or(ParseError::UnexpectedEndToken)? {
                            Token::RightParenthese(_) => {
                                state.tokens.consume();

                                Ok(ty1)
                            }
                            Token::Comma(_) => {
                                state.tokens.consume();

                                let mut types = vec![ty1];

                                match state.tokens.head().ok_or(ParseError::UnexpectedEndToken)? {
                                    Token::RightParenthese(_) => {
                                        state.tokens.consume();

                                        Ok(Self::Tuple(types))
                                    }
                                    _ => loop {
                                        state.skip_puncts();

                                        types.push(Self::parse(state)?);

                                        match state
                                            .tokens
                                            .head()
                                            .ok_or(ParseError::UnexpectedEndToken)?
                                        {
                                            Token::RightParenthese(_) => {
                                                state.tokens.consume();

                                                break Ok(Self::Tuple(types));
                                            }
                                            Token::Comma(_) => {
                                                state.tokens.consume();
                                            }
                                            _ => {
                                                state.tokens.update_max_pos();
                                                break Err(ParseError::UnexpectedToken(
                                                    state.tokens.pos,
                                                ));
                                            }
                                        }
                                    },
                                }
                            }
                            _ => {
                                state.tokens.update_max_pos();

                                Err(ParseError::UnexpectedToken(state.tokens.pos))
                            }
                        }
                    }
                }
            }
            Token::LeftBracket(shead) => {
                // list or list constructor
                state.tokens.consume();

                match state.tokens.head().ok_or(ParseError::UnexpectedEndToken)? {
                    Token::RightBracket(_) => {
                        state.tokens.consume();

                        Ok(Self::ListConstructor(shead.clone()))
                    }
                    _ => {
                        state.skip_puncts();
                        let ty = Self::parse(state)?;
                        state.skip_puncts();

                        if !matches!(
                            state.tokens.head().ok_or(ParseError::UnexpectedEndToken)?,
                            Token::RightBracket(_)
                        ) {
                            state.tokens.update_max_pos();

                            return Err(ParseError::UnexpectedToken(state.tokens.pos));
                        }
                        state.tokens.consume();

                        Ok(Self::List(Box::new(ty)))
                    }
                }
            }
            Token::LargeStartIdentifier(s) => {
                state.tokens.consume();

                Ok(Self::GenericTyCon(s.clone()))
            }
            Token::SmallStartIdentifier(s) => {
                state.tokens.consume();

                Ok(Self::TyVar(s.clone()))
            }
            Token::Keyword(Keyword::Int, s) => {
                state.tokens.consume();

                Ok(Self::Int(s.clone()))
            }
            Token::Keyword(Keyword::Float, s) => {
                state.tokens.consume();

                Ok(Self::Float(s.clone()))
            }
            Token::Keyword(Keyword::UInt, s) => {
                state.tokens.consume();

                Ok(Self::UInt(s.clone()))
            }
            Token::Keyword(Keyword::Vector2, s) => {
                state.tokens.consume();

                Ok(Self::VecConstructor(2, s.clone()))
            }
            Token::Keyword(Keyword::Vector3, s) => {
                state.tokens.consume();

                Ok(Self::VecConstructor(3, s.clone()))
            }
            Token::Keyword(Keyword::Vector4, s) => {
                state.tokens.consume();

                Ok(Self::VecConstructor(4, s.clone()))
            }
            Token::Keyword(Keyword::Matrix2, s) => {
                state.tokens.consume();

                Ok(Self::MatConstructor(2, 2, s.clone()))
            }
            Token::Keyword(Keyword::Matrix3, s) => {
                state.tokens.consume();

                Ok(Self::MatConstructor(3, 3, s.clone()))
            }
            Token::Keyword(Keyword::Matrix4, s) => {
                state.tokens.consume();

                Ok(Self::MatConstructor(4, 4, s.clone()))
            }
            Token::Keyword(Keyword::Matrix2x3, s) => {
                state.tokens.consume();

                Ok(Self::MatConstructor(2, 3, s.clone()))
            }
            Token::Keyword(Keyword::Matrix2x4, s) => {
                state.tokens.consume();

                Ok(Self::MatConstructor(2, 4, s.clone()))
            }
            Token::Keyword(Keyword::Matrix3x2, s) => {
                state.tokens.consume();

                Ok(Self::MatConstructor(3, 2, s.clone()))
            }
            Token::Keyword(Keyword::Matrix3x4, s) => {
                state.tokens.consume();

                Ok(Self::MatConstructor(3, 4, s.clone()))
            }
            Token::Keyword(Keyword::Matrix4x2, s) => {
                state.tokens.consume();

                Ok(Self::MatConstructor(4, 2, s.clone()))
            }
            Token::Keyword(Keyword::Matrix4x3, s) => {
                state.tokens.consume();

                Ok(Self::MatConstructor(4, 3, s.clone()))
            }
            _ => {
                state.tokens.update_max_pos();

                Err(ParseError::UnexpectedToken(state.tokens.pos))
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
pub struct SimpleType<'s> {
    ctor_name: Span<'s>,
    vars: Vec<Span<'s>>,
}
impl<'s> SimpleType<'s> {
    pub fn parse(state: &mut ParserState<'_, 's>) -> ParseResult<Self> {
        let ctor_name = match state.tokens.head() {
            Some(Token::LargeStartIdentifier(s)) => {
                state.tokens.consume();

                s.clone()
            }
            Some(_) => {
                state.tokens.update_max_pos();

                return Err(ParseError::UnexpectedToken(state.tokens.pos));
            }
            None => {
                state.tokens.update_max_pos();

                return Err(ParseError::UnexpectedEndToken);
            }
        };

        let mut vars = Vec::new();
        loop {
            state.skip_puncts();

            if state.try_separate().is_ok() {
                break;
            }

            if let Some(Token::SmallStartIdentifier(s)) = state.tokens.head() {
                state.tokens.consume();
                vars.push(s.clone());
            } else {
                break;
            }
        }

        Ok(Self { ctor_name, vars })
    }
}

#[derive(Debug)]
pub struct ForeignImportBody<'s> {
    convention: Span<'s>,
    entry: Span<'s>,
    varname: Span<'s>,
    ty: Type<'s>,
}
impl<'s> ForeignImportBody<'s> {
    pub fn parse(state: &mut ParserState<'_, 's>) -> ParseResult<Self> {
        let convention = match state.tokens.head() {
            Some(Token::SmallStartIdentifier(s)) => {
                state.tokens.consume();
                s
            }
            Some(_) => {
                state.tokens.update_max_pos();
                return Err(ParseError::UnexpectedToken(state.tokens.pos));
            }
            None => {
                state.tokens.update_max_pos();
                return Err(ParseError::UnexpectedEndToken);
            }
        };

        state.skip_puncts();

        let entry = match state.tokens.head() {
            Some(Token::StringLiteral(s)) => {
                state.tokens.consume();
                s
            }
            Some(_) => {
                state.tokens.update_max_pos();
                return Err(ParseError::UnexpectedToken(state.tokens.pos));
            }
            None => {
                state.tokens.update_max_pos();
                return Err(ParseError::UnexpectedEndToken);
            }
        };

        state.skip_puncts();

        let varname = match state.tokens.head() {
            Some(Token::SmallStartIdentifier(s)) => {
                state.tokens.consume();
                s
            }
            Some(_) => {
                state.tokens.update_max_pos();
                return Err(ParseError::UnexpectedToken(state.tokens.pos));
            }
            None => {
                state.tokens.update_max_pos();
                return Err(ParseError::UnexpectedEndToken);
            }
        };

        state.skip_puncts();

        if !matches!(
            state.tokens.head().ok_or(ParseError::UnexpectedEndToken)?,
            Token::Typing(_)
        ) {
            state.tokens.update_max_pos();
            return Err(ParseError::UnexpectedToken(state.tokens.pos));
        }
        state.tokens.consume();

        state.skip_puncts();

        let ty = Type::parse(state)?;

        Ok(Self {
            convention: convention.clone(),
            entry: entry.clone(),
            varname: varname.clone(),
            ty,
        })
    }
}

#[derive(Debug)]
pub struct ForeignExportBody<'s> {
    convention: Span<'s>,
    entry: Span<'s>,
    varname: Span<'s>,
    ty: Type<'s>,
}
impl<'s> ForeignExportBody<'s> {
    pub fn parse(state: &mut ParserState<'_, 's>) -> ParseResult<Self> {
        let convention = match state.tokens.head() {
            Some(Token::SmallStartIdentifier(s)) => {
                state.tokens.consume();
                s
            }
            Some(_) => {
                state.tokens.update_max_pos();
                return Err(ParseError::UnexpectedToken(state.tokens.pos));
            }
            None => {
                state.tokens.update_max_pos();
                return Err(ParseError::UnexpectedEndToken);
            }
        };

        state.skip_puncts();

        let entry = match state.tokens.head() {
            Some(Token::StringLiteral(s)) => {
                state.tokens.consume();
                s
            }
            Some(_) => {
                state.tokens.update_max_pos();
                return Err(ParseError::UnexpectedToken(state.tokens.pos));
            }
            None => {
                state.tokens.update_max_pos();
                return Err(ParseError::UnexpectedEndToken);
            }
        };

        state.skip_puncts();

        let varname = match state.tokens.head() {
            Some(Token::SmallStartIdentifier(s)) => {
                state.tokens.consume();
                s
            }
            Some(_) => {
                state.tokens.update_max_pos();
                return Err(ParseError::UnexpectedToken(state.tokens.pos));
            }
            None => {
                state.tokens.update_max_pos();
                return Err(ParseError::UnexpectedEndToken);
            }
        };

        state.skip_puncts();

        if !matches!(
            state.tokens.head().ok_or(ParseError::UnexpectedEndToken)?,
            Token::Typing(_)
        ) {
            state.tokens.update_max_pos();
            return Err(ParseError::UnexpectedToken(state.tokens.pos));
        }

        state.tokens.consume();

        state.skip_puncts();

        let ty = Type::parse(state)?;

        Ok(Self {
            convention: convention.clone(),
            entry: entry.clone(),
            varname: varname.clone(),
            ty,
        })
    }
}

#[derive(Debug)]
pub enum TopDecl<'s> {
    TypeAlias(SimpleType<'s>, Type<'s>),
    ForeignImport(Span<'s>, ForeignImportBody<'s>),
    ForeignExport(Span<'s>, ForeignExportBody<'s>),
}
impl<'s> TopDecl<'s> {
    pub fn parse(state: &mut ParserState<'_, 's>) -> ParseResult<Self> {
        match state.tokens.head().ok_or(ParseError::UnexpectedEndToken)? {
            Token::Keyword(Keyword::Type, _) => {
                state.tokens.consume();

                state.skip_puncts();

                let lhs = SimpleType::parse(state)?;

                state.skip_puncts();

                if !matches!(
                    state.tokens.head().ok_or(ParseError::UnexpectedEndToken)?,
                    Token::Equal(_)
                ) {
                    state.tokens.update_max_pos();

                    return Err(ParseError::UnexpectedToken(state.tokens.pos));
                }
                state.tokens.consume();

                state.skip_puncts();

                let rhs = Type::parse(state)?;

                Ok(Self::TypeAlias(lhs, rhs))
            }
            Token::Keyword(Keyword::Foreign, sp0) => {
                state.tokens.consume();
                state.skip_puncts();

                match state.tokens.head().ok_or(ParseError::UnexpectedEndToken)? {
                    Token::Keyword(Keyword::Import, _) => {
                        state.tokens.consume();
                        state.skip_puncts();

                        ForeignImportBody::parse(state).map(|b| Self::ForeignImport(sp0.clone(), b))
                    }
                    Token::Keyword(Keyword::Export, _) => {
                        state.tokens.consume();
                        state.skip_puncts();

                        ForeignExportBody::parse(state).map(|b| Self::ForeignExport(sp0.clone(), b))
                    }
                    _ => {
                        state.tokens.update_max_pos();

                        Err(ParseError::UnexpectedToken(state.tokens.pos))
                    }
                }
            }
            _ => {
                state.tokens.update_max_pos();

                Err(ParseError::UnexpectedToken(state.tokens.pos))
            }
        }
    }
}

#[derive(Debug)]
pub struct Body<'s> {
    imports: Vec<ImportDecl<'s>>,
    top_decls: Vec<TopDecl<'s>>,
}
impl<'s> Body<'s> {
    pub fn parse(state: &mut ParserState<'_, 's>) -> ParseResult<Self> {
        state.try_open_block()?;

        let mut imports = Vec::new();
        loop {
            let saved = state.branch();

            state.skip_puncts();

            let import = match ImportDecl::parse(state) {
                Ok(x) => x,
                Err(_) => {
                    state.unwind(saved);
                    break;
                }
            };

            state.skip_puncts();

            match state.try_separate() {
                Ok(_) => (),
                Err(_) => {
                    state.unwind(saved);
                    break;
                }
            }

            imports.push(import);
        }

        state.skip_puncts();

        let mut top_decls = Vec::new();
        loop {
            state.skip_puncts();

            let saved = state.branch();

            let td = match TopDecl::parse(state) {
                Ok(x) => x,
                Err(_) => {
                    state.unwind(saved);
                    break;
                }
            };

            state.skip_puncts();

            if state.try_separate().is_err() {
                state.unwind(saved);
                break;
            }

            top_decls.push(td);
        }

        state.try_close_block()?;
        Ok(Self { imports, top_decls })
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

                state.skip_puncts();

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

                state.skip_puncts();

                let node = match state.tokens.head().ok_or(ParseError::UnexpectedEndToken)? {
                    Token::Op(s) if s.as_str() == ".." => {
                        state.tokens.consume();

                        Self::TypeOrClassAll(s.clone())
                    }
                    Token::LargeStartIdentifier(s1) => {
                        // data constructors
                        let mut ctors = vec![s1.clone()];

                        loop {
                            state.skip_puncts();

                            match state.tokens.head() {
                                Some(Token::Comma(_)) => {
                                    state.tokens.consume();
                                }
                                _ => break,
                            };

                            state.skip_puncts();

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
                            state.skip_puncts();

                            if !matches!(state.tokens.head(), Some(Token::Comma(_))) {
                                break;
                            }
                            state.tokens.consume();

                            state.skip_puncts();

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

                state.skip_puncts();

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

        state.skip_puncts();

        let id = parse_modid(state)?;

        state.skip_puncts();

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

                state.skip_puncts();

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

        state.skip_puncts();

        let body = Body::parse(state)?;

        Ok(Self { head, body })
    }
}
