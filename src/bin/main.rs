fn main() {
    let mut c = String::new();
    std::io::Read::read_to_string(&mut std::io::stdin(), &mut c)
        .expect("Failed to read from stdin");
    for t in pureshader_rs::lexicalizer::Lexicalizer::new(&c) {
        match t {
            Ok(t) => println!("{t:?}"),
            Err(e) => panic!("ParseError: {e:?} ({:?})", e.calc_position(&c)),
        }
    }
}
