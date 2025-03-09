fn main() {
    let text = "2d4 + 7d5";
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
    let node = sdi::roll(text);
    println!("{:?}", node);
}
