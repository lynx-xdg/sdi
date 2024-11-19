fn main() {
    let text = "-1 + 4 * 2 + 43d51 * 4";
    println!("{:?}", text);
    let mut lexer = sdi::Lexer::new(text);
    loop {
        if let Some(token) = lexer.get_next_token() {
            print!("{:?} ", token);
        } else {
            break;
        }
    }
    println!();
    let lexer = sdi::Lexer::new(text);
    let mut parser = sdi::Parser::new(lexer);
    let node = parser.parse();
    println!("{:?}", node);
    println!("{:?}", node.eval());
}
