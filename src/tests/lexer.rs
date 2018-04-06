use lexer::{Lexer, NextToken, Token};

fn test_tokens(source: &str, tokens: &[Token]) {
    let mut lexer = Lexer::new(source.as_bytes());
    let mut i = 0;
    while let NextToken::Next { token, .. } = lexer.next().unwrap() {
        assert!(i < tokens.len(), "too many tokens");
        assert_eq!(token, tokens[i], "tokens not equal");
        i += 1;
    }
    assert!(i == tokens.len(), "not enough tokens");
}

fn test_tokens_lines(source: &str, tokens: &[(Token, u64)]) {
    let mut lexer = Lexer::new(source.as_bytes());
    let mut i = 0;
    while let NextToken::Next { token, line_number } = lexer.next().unwrap() {
        assert!(i < tokens.len(), "too many tokens");
        assert_eq!(token, tokens[i].0, "tokens not equal");
        assert_eq!(line_number, tokens[i].1, "line numbers do not match");
        i += 1;
    }
    assert!(i == tokens.len(), "not enough tokens");
}

fn str_token(s: &str) -> Token {
    Token::String(s.as_bytes())
}

#[test]
fn comments() {
    test_tokens_lines(
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
        "#,
        &[(Token::Minus, 7), (Token::Assign, 15)],
    );
}

#[test]
fn long_string() {
    test_tokens(
        r#"
            [====[ [==[ this is a [[]] long string ]== ]==] ]====]
            [[ [=] [==] another long string [==] [=] ]]
        "#,
        &[
            str_token(" [==[ this is a [[]] long string ]== ]==] "),
            str_token(" [=] [==] another long string [==] [=] "),
        ],
    );
}

#[test]
fn short_string() {
    test_tokens_lines(
        r#"
            "\\ \" '"
            '\n \t "'
            "begin \z
            end"
            'state\u{2e}'
            "question\x3f"
            "exclaim\33"
        "#,
        &[
            (str_token("\\ \" '"), 1),
            (str_token("\n \t \""), 2),
            (str_token("begin end"), 3),
            (str_token("state."), 5),
            (str_token("question?"), 6),
            (str_token("exclaim!"), 7),
        ],
    );
}
