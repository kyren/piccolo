use std::f64;

use luster::lexer::{Lexer, Token};

fn test_tokens(source: &str, tokens: &[Token]) {
    let mut lexer = Lexer::new(source.as_bytes());
    let mut i = 0;
    while let Some(token) = lexer.read_token().unwrap() {
        assert!(i < tokens.len(), "too many tokens");
        assert_eq!(token, tokens[i], "tokens not equal");
        i += 1;
    }
    assert!(i == tokens.len(), "not enough tokens");
}

fn test_tokens_lines(source: &str, tokens: &[(Token, u64)]) {
    let mut lexer = Lexer::new(source.as_bytes());
    let mut i = 0;
    loop {
        lexer.skip_whitespace().unwrap();
        let line_number = lexer.line_number();
        if let Some(token) = lexer.read_token().unwrap() {
            assert!(i < tokens.len(), "too many tokens");
            assert_eq!(token, tokens[i].0, "tokens not equal");
            assert_eq!(line_number, tokens[i].1, "line numbers do not match");
            i += 1;
        } else {
            break;
        }
    }
    assert!(i == tokens.len(), "not enough tokens");
}

fn str_token(s: &str) -> Token {
    Token::String(s.as_bytes().to_vec().into_boxed_slice())
}

fn name_token(s: &str) -> Token {
    Token::Name(s.as_bytes().to_vec().into_boxed_slice())
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
            0x22.4p+1
            0Xaa.8P-2
            0x8.4P0
            .123E-10
            0x99999999999999999999999999999999p999999999999999999999999999999
            9223372036854775807
            9223372036854775808
        "#,
        &[
            Token::Integer(0xdeadbeef),
            Token::Integer(12345),
            Token::Float(12345.0),
            Token::Float(3.1415e-2),
            Token::Float(68.5),
            Token::Float(42.625),
            Token::Float(8.25),
            Token::Float(0.123e-10),
            Token::Float(f64::INFINITY),
            Token::Integer(9223372036854775807),
            Token::Float(9223372036854775808.0),
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
        r#"- + * / // ^ % & ~ | , ; >> << . .. ... = < <= > >= == ~= : :: # ( ) [ ] { }"#,
        &[
            Token::Minus,
            Token::Add,
            Token::Mul,
            Token::Div,
            Token::IDiv,
            Token::Pow,
            Token::Mod,
            Token::BitAnd,
            Token::BitNotXor,
            Token::BitOr,
            Token::Comma,
            Token::SemiColon,
            Token::ShiftRight,
            Token::ShiftLeft,
            Token::Dot,
            Token::Concat,
            Token::Dots,
            Token::Assign,
            Token::LessThan,
            Token::LessEqual,
            Token::GreaterThan,
            Token::GreaterEqual,
            Token::Equal,
            Token::NotEqual,
            Token::Colon,
            Token::DoubleColon,
            Token::Len,
            Token::LeftParen,
            Token::RightParen,
            Token::LeftBracket,
            Token::RightBracket,
            Token::LeftBrace,
            Token::RightBrace,
        ],
    );
}
