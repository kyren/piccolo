use lexer::{Lexer, NextToken, Number, Token};

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

fn str_token<'a>(s: &'a str) -> Token<'a> {
    Token::String(s.as_bytes())
}

fn name_token<'a>(s: &'a str) -> Token<'a> {
    Token::Name(s.as_bytes())
}

fn int_token<'a>(is_hex: bool, digits: &'a [u8]) -> Token<'a> {
    Token::Number(Number::Integer { is_hex, digits })
}

fn float_token<'a>(
    is_hex: bool,
    whole_part: &'a [u8],
    frac_part: &'a [u8],
    exp_part: &'a [u8],
    exp_neg: bool,
) -> Token<'a> {
    Token::Number(Number::Float {
        is_hex,
        whole_part,
        frac_part,
        exp_part,
        exp_neg,
    })
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

#[test]
fn numerals() {
    test_tokens(
        r#"
            0xdeadbeef
            12345
            12345.
            3.1415e-2
            0xdead.beefp+1
            0Xaa.bbP2
            .123E-10
        "#,
        &[
            int_token(true, &[13, 14, 10, 13, 11, 14, 14, 15]),
            int_token(false, &[1, 2, 3, 4, 5]),
            float_token(false, &[1, 2, 3, 4, 5], &[], &[], false),
            float_token(false, &[3], &[1, 4, 1, 5], &[2], true),
            float_token(true, &[13, 14, 10, 13], &[11, 14, 14, 15], &[1], false),
            float_token(true, &[10, 10], &[11, 11], &[2], false),
            float_token(false, &[], &[1, 2, 3], &[1, 0], true),
        ],
    );
}

#[test]
fn words() {
    test_tokens(
        r#"
            break do else elseif end function goto if in local nil for while repeat until return
            then true false not and or
            custom names
        "#,
        &[
            Token::Break,
            Token::Do,
            Token::Else,
            Token::ElseIf,
            Token::End,
            Token::Function,
            Token::Goto,
            Token::If,
            Token::In,
            Token::Local,
            Token::Nil,
            Token::For,
            Token::While,
            Token::Repeat,
            Token::Until,
            Token::Return,
            Token::Then,
            Token::True,
            Token::False,
            Token::Not,
            Token::And,
            Token::Or,
            name_token("custom"),
            name_token("names"),
        ],
    );
}

#[test]
fn ops() {
    test_tokens(
        r#"+ - * / // ^ % & ~ | >> << . .. ... = < <= > >= == ~= : :: # [ ] { }"#,
        &[
            Token::Plus,
            Token::Minus,
            Token::Times,
            Token::Div,
            Token::IDiv,
            Token::Pow,
            Token::Mod,
            Token::BitAnd,
            Token::BitNot,
            Token::BitOr,
            Token::ShiftRight,
            Token::ShiftLeft,
            Token::Dot,
            Token::Concat,
            Token::Dots,
            Token::Assign,
            Token::Less,
            Token::LessEqual,
            Token::Greater,
            Token::GreaterEqual,
            Token::Equal,
            Token::NotEqual,
            Token::Colon,
            Token::DoubleColon,
            Token::Len,
            Token::LeftBracket,
            Token::RightBracket,
            Token::LeftBrace,
            Token::RightBrace,
        ],
    );
}
