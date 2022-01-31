fn main() {
    let mut c = String::new();
    std::io::Read::read_to_string(&mut std::io::stdin(), &mut c).expect("Failed to read from stdin");
    let lex = pureshader_rs::lexicalizer::Lexicalizer::new(&c);
    let indents = pureshader_rs::lexicalizer::IndentTokenStreamAdapter::from(lex);
    for t in indents {
        println!("{:?}", t.expect("ParseError"));
    }
}
