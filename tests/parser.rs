use luster::parser::{
    parse_chunk, Block, CallSuffix, Chunk, ConstructorField, Expression, FunctionCallStatement,
    HeadExpression, PrimaryExpression, SimpleExpression, Statement, SuffixedExpression,
    TableConstructor,
};

#[test]
fn test_function_call() {
    assert_eq!(
        parse_chunk("print(10, 20);print'foo';print{30.0}".as_bytes(), |s| s
            .to_vec()
            .into_boxed_slice())
        .unwrap(),
        Chunk {
            block: Block {
                statements: vec![
                    Statement::FunctionCall(FunctionCallStatement {
                        head: SuffixedExpression {
                            primary: PrimaryExpression::Name(
                                "print".as_bytes().to_vec().into_boxed_slice(),
                            ),
                            suffixes: vec![],
                        },
                        call: CallSuffix::Function(vec![
                            Expression {
                                head: Box::new(HeadExpression::Simple(SimpleExpression::Integer(
                                    10,
                                ))),
                                tail: vec![],
                            },
                            Expression {
                                head: Box::new(HeadExpression::Simple(SimpleExpression::Integer(
                                    20,
                                ))),
                                tail: vec![],
                            },
                        ]),
                    }),
                    Statement::FunctionCall(FunctionCallStatement {
                        head: SuffixedExpression {
                            primary: PrimaryExpression::Name(
                                "print".as_bytes().to_vec().into_boxed_slice(),
                            ),
                            suffixes: vec![],
                        },
                        call: CallSuffix::Function(vec![Expression {
                            head: Box::new(HeadExpression::Simple(SimpleExpression::String(
                                "foo".as_bytes().to_vec().into_boxed_slice(),
                            ))),
                            tail: vec![],
                        },]),
                    }),
                    Statement::FunctionCall(FunctionCallStatement {
                        head: SuffixedExpression {
                            primary: PrimaryExpression::Name(
                                "print".as_bytes().to_vec().into_boxed_slice(),
                            ),
                            suffixes: vec![],
                        },
                        call: CallSuffix::Function(vec![Expression {
                            head: Box::new(HeadExpression::Simple(
                                SimpleExpression::TableConstructor(TableConstructor {
                                    fields: vec![ConstructorField::Array(Expression {
                                        head: Box::new(HeadExpression::Simple(
                                            SimpleExpression::Float(30.0),
                                        )),
                                        tail: vec![],
                                    }),],
                                }),
                            )),
                            tail: vec![],
                        },]),
                    }),
                ],
                return_statement: None,
            },
        }
    );
}
