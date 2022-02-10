fn main() {
    let mut c = String::new();
    std::io::Read::read_to_string(&mut std::io::stdin(), &mut c)
        .expect("Failed to read from stdin");
    let tokens = match pureshader_rs::lexicalizer::Lexicalizer::new(&c)
        .into_iter()
        .collect::<Result<Vec<_>, _>>()
    {
        Ok(t) => t,
        Err(e) => panic!("TokenizerError: {e:?} ({:?})", e.calc_position(&c)),
    };

    let mut ps = pureshader_rs::parser::ParserState::new(&tokens);
    ps.skip_puncts();
    let ast = match pureshader_rs::parser::Module::parse(&mut ps) {
        Ok(t) => t,
        Err(pureshader_rs::parser::ParseError::UnexpectedToken(tpos)) => {
            panic!(
                "ParseError: Unexpected Token {:?} ({:?})",
                tokens[tpos],
                tokens[tpos].head_span().calc_position()
            );
        }
        Err(e) => panic!("ParseError: {:?}", e),
    };
    println!("{ast:?}");
}
