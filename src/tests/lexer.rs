use lexer::{Lexer, NextToken, Token};

#[test]
fn comments() {
    let mut lexer = Lexer::new(
        r#"
            -- this is a comment
            -- this is also -- a comment
            --[[ long comment ]]
            --[==[ longer comment ]==]

            -- Real token
            -

            --[====[ longest comment
                these shouldn't trigger the end of comments
                ]=] ]==] ]===]
            ]====]

            -- Real token
            =
        "#.as_bytes(),
    );

    match lexer.next().unwrap() {
        NextToken::Token {
            token: Token::Minus,
            line_number: 7,
        } => {}
        e => panic!("wrong next token {:?}", e),
    }

    match lexer.next().unwrap() {
        NextToken::Token {
            token: Token::Assign,
            line_number: 15,
        } => {}
        e => panic!("wrong next token {:?}", e),
    }

    match lexer.next().unwrap() {
        NextToken::End => {}
        e => panic!("wrong next token {:?}", e),
    }
}

#[test]
fn long_string() {
    let mut lexer = Lexer::new(
        r#"
            [====[ [==[ this is a [[]] long string ]==] ]====]
            [[ [=] another long string [=] ]]
        "#.as_bytes(),
    );

    match lexer.next().unwrap() {
        NextToken::Token {
            token: Token::String(s),
            ..
        } => assert_eq!(
            s.as_ref(),
            " [==[ this is a [[]] long string ]==] ".as_bytes()
        ),
        e => panic!("wrong next token {:?}", e),
    }

    match lexer.next().unwrap() {
        NextToken::Token {
            token: Token::String(s),
            ..
        } => assert_eq!(s.as_ref(), " [=] another long string [=] ".as_bytes()),
        e => panic!("wrong next token {:?}", e),
    }

    match lexer.next().unwrap() {
        NextToken::End => {}
        e => panic!("wrong next token {:?}", e),
    }
}
