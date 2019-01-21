use std::error::Error as StdError;
use std::fmt;
use std::io::Read;
use std::rc::Rc;

use gc_arena::Collect;

use crate::{Lexer, LexerError, Token};

#[derive(Debug, PartialEq, Clone)]
pub struct Chunk<S> {
    pub block: Block<S>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block<S> {
    pub statements: Vec<Statement<S>>,
    pub return_statement: Option<ReturnStatement<S>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement<S> {
    If(IfStatement<S>),
    While(WhileStatement<S>),
    Do(Block<S>),
    For(ForStatement<S>),
    Repeat(RepeatStatement<S>),
    Function(FunctionStatement<S>),
    LocalFunction(LocalFunctionStatement<S>),
    LocalStatement(LocalStatement<S>),
    Label(LabelStatement<S>),
    Break,
    Goto(GotoStatement<S>),
    FunctionCall(FunctionCallStatement<S>),
    Assignment(AssignmentStatement<S>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnStatement<S> {
    pub returns: Vec<Expression<S>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfStatement<S> {
    pub if_part: (Expression<S>, Block<S>),
    pub else_if_parts: Vec<(Expression<S>, Block<S>)>,
    pub else_part: Option<Block<S>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct WhileStatement<S> {
    pub condition: Expression<S>,
    pub block: Block<S>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ForStatement<S> {
    Numeric {
        name: S,
        initial: Expression<S>,
        limit: Expression<S>,
        step: Option<Expression<S>>,
        body: Block<S>,
    },
    Generic {
        names: Vec<S>,
        arguments: Vec<Expression<S>>,
        body: Block<S>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub struct RepeatStatement<S> {
    pub body: Block<S>,
    pub until: Expression<S>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct LabelStatement<S> {
    pub name: S,
}

#[derive(Debug, PartialEq, Clone)]
pub struct GotoStatement<S> {
    pub name: S,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionStatement<S> {
    pub name: S,
    pub fields: Vec<S>,
    pub method: Option<S>,
    pub definition: FunctionDefinition<S>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct LocalFunctionStatement<S> {
    pub name: S,
    pub definition: FunctionDefinition<S>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct LocalStatement<S> {
    pub names: Vec<S>,
    pub values: Vec<Expression<S>>,
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Mod,
    Pow,
    Div,
    IDiv,
    BitAnd,
    BitOr,
    BitXor,
    ShiftLeft,
    ShiftRight,
    Concat,
    NotEqual,
    Equal,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    And,
    Or,
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
pub enum UnaryOperator {
    Not,
    Minus,
    BitNot,
    Len,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Expression<S> {
    pub head: Box<HeadExpression<S>>,
    pub tail: Vec<(BinaryOperator, Expression<S>)>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum HeadExpression<S> {
    Simple(SimpleExpression<S>),
    UnaryOperator(UnaryOperator, Expression<S>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum SimpleExpression<S> {
    Float(f64),
    Integer(i64),
    String(S),
    Nil,
    True,
    False,
    VarArgs,
    TableConstructor(TableConstructor<S>),
    Function(FunctionDefinition<S>),
    Suffixed(SuffixedExpression<S>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum PrimaryExpression<S> {
    Name(S),
    GroupedExpression(Expression<S>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum FieldSuffix<S> {
    Named(S),
    Indexed(Expression<S>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum CallSuffix<S> {
    Method(S, Vec<Expression<S>>),
    Function(Vec<Expression<S>>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum SuffixPart<S> {
    Field(FieldSuffix<S>),
    Call(CallSuffix<S>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct SuffixedExpression<S> {
    pub primary: PrimaryExpression<S>,
    pub suffixes: Vec<SuffixPart<S>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDefinition<S> {
    pub parameters: Vec<S>,
    pub has_varargs: bool,
    pub body: Block<S>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionCallStatement<S> {
    pub head: SuffixedExpression<S>,
    pub call: CallSuffix<S>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct AssignmentStatement<S> {
    pub targets: Vec<AssignmentTarget<S>>,
    pub values: Vec<Expression<S>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum AssignmentTarget<S> {
    Name(S),
    Field(SuffixedExpression<S>, FieldSuffix<S>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct TableConstructor<S> {
    pub fields: Vec<ConstructorField<S>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ConstructorField<S> {
    Array(Expression<S>),
    Record(RecordKey<S>, Expression<S>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum RecordKey<S> {
    Named(S),
    Indexed(Expression<S>),
}

#[derive(Debug, Collect)]
#[collect(require_static)]
pub enum ParserError {
    Unexpected {
        unexpected: String,
        expected: Option<String>,
    },
    EndOfStream {
        expected: Option<String>,
    },
    AssignToExpression,
    ExpressionNotStatement,
    RecursionLimit,
    LexerError(LexerError),
}

impl StdError for ParserError {}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let write_expected = |f: &mut fmt::Formatter, expected: &Option<String>| {
            match expected {
                Some(expected) => {
                    write!(f, ", expected {}", expected)?;
                }
                None => {}
            }
            Ok(())
        };

        match self {
            ParserError::Unexpected {
                unexpected,
                expected,
            } => {
                write!(f, "found {:?}", unexpected)?;
                write_expected(f, expected)
            }
            ParserError::EndOfStream { expected } => {
                write!(f, "unexpected end of token stream")?;
                write_expected(f, expected)
            }
            ParserError::AssignToExpression => write!(f, "cannot assign to expression"),
            ParserError::ExpressionNotStatement => write!(f, "expression is not a statement"),
            ParserError::RecursionLimit => write!(f, "recursion limit reached"),
            ParserError::LexerError(lexer_error) => write!(f, "{}", lexer_error),
        }
    }
}

pub fn parse_chunk<R, S, CS>(source: R, create_string: CS) -> Result<Chunk<S>, ParserError>
where
    R: Read,
    S: fmt::Debug + PartialEq,
    CS: FnMut(&[u8]) -> S,
{
    Parser {
        lexer: Lexer::new(source, create_string),
        read_buffer: Vec::new(),
        recursion_guard: Rc::new(()),
    }
    .parse_chunk()
}

struct Parser<R, S, CS> {
    lexer: Lexer<R, CS>,
    read_buffer: Vec<Token<S>>,
    recursion_guard: Rc<()>,
}

impl<R, S, CS> Parser<R, S, CS>
where
    R: Read,
    S: fmt::Debug + PartialEq,
    CS: FnMut(&[u8]) -> S,
{
    fn parse_chunk(&mut self) -> Result<Chunk<S>, ParserError> {
        let block = self.parse_block()?;
        if self.look_ahead(0)? != None {
            Err(ParserError::EndOfStream { expected: None })
        } else {
            Ok(Chunk { block })
        }
    }

    fn parse_block(&mut self) -> Result<Block<S>, ParserError> {
        let mut statements = Vec::new();
        let mut return_statement = None;

        loop {
            match self.look_ahead(0)? {
                Some(&Token::Else) | Some(&Token::ElseIf) | Some(&Token::End)
                | Some(&Token::Until) => break,
                Some(&Token::SemiColon) => {
                    self.take_next()?;
                }
                Some(&Token::Return) => {
                    return_statement = Some(self.parse_return_statement()?);
                    break;
                }
                None => break,
                _ => {
                    statements.push(self.parse_statement()?);
                }
            }
        }

        Ok(Block {
            statements,
            return_statement,
        })
    }

    fn parse_statement(&mut self) -> Result<Statement<S>, ParserError> {
        let _recursion_guard = self.recursion_guard()?;

        Ok(match *self.get_next()? {
            Token::If => Statement::If(self.parse_if_statement()?),
            Token::While => Statement::While(self.parse_while_statement()?),
            Token::Do => {
                self.take_next()?;
                let statement = Statement::Do(self.parse_block()?);
                self.expect_next(Token::End)?;
                statement
            }
            Token::For => Statement::For(self.parse_for_statement()?),
            Token::Repeat => Statement::Repeat(self.parse_repeat_statement()?),
            Token::Function => Statement::Function(self.parse_function_statement()?),
            Token::Local => {
                if self.check_ahead(1, Token::Function)? {
                    self.take_next()?;
                    Statement::LocalFunction(self.parse_local_function_statement()?)
                } else {
                    Statement::LocalStatement(self.parse_local_statement()?)
                }
            }
            Token::DoubleColon => Statement::Label(self.parse_label_statement()?),
            Token::Break => {
                self.take_next()?;
                Statement::Break
            }
            Token::Goto => Statement::Goto(self.parse_goto_statement()?),
            _ => self.parse_expression_statement()?,
        })
    }

    fn parse_return_statement(&mut self) -> Result<ReturnStatement<S>, ParserError> {
        self.expect_next(Token::Return)?;
        let returns = match self.look_ahead(0)? {
            None
            | Some(Token::End)
            | Some(Token::SemiColon)
            | Some(Token::Else)
            | Some(Token::ElseIf)
            | Some(Token::Until) => Vec::new(),
            _ => self.parse_expression_list()?,
        };
        if self.check_ahead(0, Token::SemiColon)? {
            self.take_next()?;
        }
        Ok(ReturnStatement { returns })
    }

    fn parse_if_statement(&mut self) -> Result<IfStatement<S>, ParserError> {
        self.expect_next(Token::If)?;
        let if_cond = self.parse_expression()?;
        self.expect_next(Token::Then)?;
        let if_block = self.parse_block()?;

        let mut else_if_parts = Vec::new();
        while self.check_ahead(0, Token::ElseIf)? {
            self.take_next()?;
            let cond = self.parse_expression()?;
            self.expect_next(Token::Then)?;
            let block = self.parse_block()?;
            else_if_parts.push((cond, block));
        }

        let else_part = if self.check_ahead(0, Token::Else)? {
            self.take_next()?;
            Some(self.parse_block()?)
        } else {
            None
        };

        self.expect_next(Token::End)?;

        Ok(IfStatement {
            if_part: (if_cond, if_block),
            else_if_parts,
            else_part,
        })
    }

    fn parse_while_statement(&mut self) -> Result<WhileStatement<S>, ParserError> {
        self.expect_next(Token::While)?;
        let condition = self.parse_expression()?;
        self.expect_next(Token::Do)?;
        let block = self.parse_block()?;
        self.expect_next(Token::End)?;

        Ok(WhileStatement { condition, block })
    }

    fn parse_for_statement(&mut self) -> Result<ForStatement<S>, ParserError> {
        self.expect_next(Token::For)?;
        let name = self.expect_name()?;

        match self.get_next()? {
            Token::Assign => {
                self.take_next()?;
                let initial = self.parse_expression()?;
                self.expect_next(Token::Comma)?;
                let limit = self.parse_expression()?;
                let step = if self.check_ahead(0, Token::Comma)? {
                    self.take_next()?;
                    Some(self.parse_expression()?)
                } else {
                    None
                };

                self.expect_next(Token::Do)?;
                let body = self.parse_block()?;
                self.expect_next(Token::End)?;

                Ok(ForStatement::Numeric {
                    name,
                    initial,
                    limit,
                    step,
                    body,
                })
            }

            Token::Comma | Token::In => {
                let mut names = Vec::new();
                names.push(name);
                while self.check_ahead(0, Token::Comma)? {
                    self.take_next()?;
                    names.push(self.expect_name()?);
                }
                self.expect_next(Token::In)?;
                let arguments = self.parse_expression_list()?;

                self.expect_next(Token::Do)?;
                let body = self.parse_block()?;
                self.expect_next(Token::End)?;

                Ok(ForStatement::Generic {
                    names,
                    arguments,
                    body,
                })
            }

            token => Err(ParserError::Unexpected {
                unexpected: format!("{:?}", token),
                expected: Some("'=' or 'in'".to_owned()),
            }),
        }
    }

    fn parse_repeat_statement(&mut self) -> Result<RepeatStatement<S>, ParserError> {
        self.expect_next(Token::Repeat)?;
        let body = self.parse_block()?;
        self.expect_next(Token::Until)?;
        let until = self.parse_expression()?;
        Ok(RepeatStatement { body, until })
    }

    fn parse_function_statement(&mut self) -> Result<FunctionStatement<S>, ParserError> {
        self.expect_next(Token::Function)?;

        let name = self.expect_name()?;
        let mut fields = Vec::new();
        let mut method = None;
        loop {
            match self.look_ahead(0)? {
                Some(&Token::Dot) => {
                    self.take_next()?;
                    fields.push(self.expect_name()?);
                }
                Some(&Token::Colon) => {
                    self.take_next()?;
                    method = Some(self.expect_name()?);
                    break;
                }
                _ => break,
            }
        }

        let definition = self.parse_function_definition()?;

        Ok(FunctionStatement {
            name,
            fields,
            method,
            definition,
        })
    }

    fn parse_local_function_statement(&mut self) -> Result<LocalFunctionStatement<S>, ParserError> {
        self.expect_next(Token::Function)?;

        let name = self.expect_name()?;
        let definition = self.parse_function_definition()?;

        Ok(LocalFunctionStatement { name, definition })
    }

    fn parse_local_statement(&mut self) -> Result<LocalStatement<S>, ParserError> {
        self.expect_next(Token::Local)?;
        let mut names = Vec::new();
        names.push(self.expect_name()?);
        while self.check_ahead(0, Token::Comma)? {
            self.take_next()?;
            names.push(self.expect_name()?);
        }

        let values = if self.check_ahead(0, Token::Assign)? {
            self.take_next()?;
            self.parse_expression_list()?
        } else {
            Vec::new()
        };

        Ok(LocalStatement { names, values })
    }

    fn parse_label_statement(&mut self) -> Result<LabelStatement<S>, ParserError> {
        self.expect_next(Token::DoubleColon)?;
        let name = self.expect_name()?;
        self.expect_next(Token::DoubleColon)?;
        Ok(LabelStatement { name })
    }

    fn parse_goto_statement(&mut self) -> Result<GotoStatement<S>, ParserError> {
        self.expect_next(Token::Goto)?;
        let name = self.expect_name()?;
        Ok(GotoStatement { name })
    }

    fn parse_expression_statement(&mut self) -> Result<Statement<S>, ParserError> {
        let mut suffixed_expression = self.parse_suffixed_expression()?;
        if self.check_ahead(0, Token::Assign)? || self.check_ahead(0, Token::Comma)? {
            let mut targets = Vec::new();
            loop {
                let assignment_target = if let Some(suffix) = suffixed_expression.suffixes.pop() {
                    match suffix {
                        SuffixPart::Field(field_suffix) => {
                            AssignmentTarget::Field(suffixed_expression, field_suffix)
                        }
                        SuffixPart::Call(_) => {
                            return Err(ParserError::AssignToExpression);
                        }
                    }
                } else {
                    match suffixed_expression.primary {
                        PrimaryExpression::Name(name) => AssignmentTarget::Name(name),
                        _ => return Err(ParserError::AssignToExpression),
                    }
                };
                targets.push(assignment_target);

                if !self.check_ahead(0, Token::Comma)? {
                    break;
                } else {
                    self.take_next()?;
                    suffixed_expression = self.parse_suffixed_expression()?;
                }
            }

            self.expect_next(Token::Assign)?;
            let values = self.parse_expression_list()?;

            Ok(Statement::Assignment(AssignmentStatement {
                targets,
                values,
            }))
        } else if let Some(suffix) = suffixed_expression.suffixes.pop() {
            match suffix {
                SuffixPart::Call(call_suffix) => {
                    Ok(Statement::FunctionCall(FunctionCallStatement {
                        head: suffixed_expression,
                        call: call_suffix,
                    }))
                }
                SuffixPart::Field(_) => Err(ParserError::ExpressionNotStatement),
            }
        } else {
            Err(ParserError::ExpressionNotStatement)
        }
    }

    fn parse_expression(&mut self) -> Result<Expression<S>, ParserError> {
        self.parse_sub_expression(MIN_PRIORITY)
    }

    fn parse_expression_list(&mut self) -> Result<Vec<Expression<S>>, ParserError> {
        let mut expressions = Vec::new();
        expressions.push(self.parse_expression()?);
        while self.check_ahead(0, Token::Comma)? {
            self.take_next()?;
            expressions.push(self.parse_expression()?);
        }
        Ok(expressions)
    }

    fn parse_sub_expression(&mut self, priority_limit: u8) -> Result<Expression<S>, ParserError> {
        let _recursion_guard = self.recursion_guard()?;

        let head = if let Some(unary_op) = get_unary_operator(self.get_next()?) {
            self.take_next()?;
            HeadExpression::UnaryOperator(unary_op, self.parse_sub_expression(UNARY_PRIORITY)?)
        } else {
            HeadExpression::Simple(self.parse_simple_expression()?)
        };

        let mut tail = Vec::new();
        while let Some(binary_op) = self.look_ahead(0)?.and_then(get_binary_operator) {
            let (left_priority, right_priority) = binary_priority(binary_op);
            if left_priority <= priority_limit {
                break;
            }

            self.take_next()?;
            let right_expression = self.parse_sub_expression(right_priority)?;
            tail.push((binary_op, right_expression));
        }

        Ok(Expression {
            head: Box::new(head),
            tail,
        })
    }

    fn parse_simple_expression(&mut self) -> Result<SimpleExpression<S>, ParserError> {
        Ok(match *self.get_next()? {
            Token::Float(f) => {
                self.take_next()?;
                SimpleExpression::Float(f)
            }
            Token::Integer(i) => {
                self.take_next()?;
                SimpleExpression::Integer(i)
            }
            Token::String(_) => SimpleExpression::String(self.expect_string()?),
            Token::Nil => {
                self.take_next()?;
                SimpleExpression::Nil
            }
            Token::True => {
                self.take_next()?;
                SimpleExpression::True
            }
            Token::False => {
                self.take_next()?;
                SimpleExpression::False
            }
            Token::Dots => {
                self.take_next()?;
                SimpleExpression::VarArgs
            }
            Token::LeftBrace => SimpleExpression::TableConstructor(self.parse_table_constructor()?),
            Token::Function => {
                self.take_next()?;
                SimpleExpression::Function(self.parse_function_definition()?)
            }
            _ => SimpleExpression::Suffixed(self.parse_suffixed_expression()?),
        })
    }

    fn parse_primary_expression(&mut self) -> Result<PrimaryExpression<S>, ParserError> {
        match self.take_next()? {
            Token::LeftParen => {
                let expr = self.parse_expression()?;
                self.expect_next(Token::RightParen)?;
                Ok(PrimaryExpression::GroupedExpression(expr))
            }
            Token::Name(n) => Ok(PrimaryExpression::Name(n)),
            token => Err(ParserError::Unexpected {
                unexpected: format!("{:?}", token),
                expected: Some("grouped expression or name".to_owned()),
            }),
        }
    }

    fn parse_field_suffix(&mut self) -> Result<FieldSuffix<S>, ParserError> {
        match self.get_next()? {
            Token::Dot => {
                self.take_next()?;
                Ok(FieldSuffix::Named(self.expect_name()?))
            }
            Token::LeftBracket => {
                self.take_next()?;
                let expr = self.parse_expression()?;
                self.expect_next(Token::RightBracket)?;
                Ok(FieldSuffix::Indexed(expr))
            }
            token => Err(ParserError::Unexpected {
                unexpected: format!("{:?}", token),
                expected: Some("field or suffix".to_owned()),
            }),
        }
    }

    fn parse_call_suffix(&mut self) -> Result<CallSuffix<S>, ParserError> {
        let method_name = match *self.get_next()? {
            Token::Colon => {
                self.take_next()?;
                Some(self.expect_name()?)
            }
            _ => None,
        };

        let args = match self.get_next()? {
            Token::LeftParen => {
                self.take_next()?;
                let args = if *self.get_next()? != Token::RightParen {
                    self.parse_expression_list()?
                } else {
                    Vec::new()
                };
                self.expect_next(Token::RightParen)?;
                args
            }
            Token::LeftBrace => vec![Expression {
                head: Box::new(HeadExpression::Simple(SimpleExpression::TableConstructor(
                    self.parse_table_constructor()?,
                ))),
                tail: vec![],
            }],
            Token::String(_) => vec![Expression {
                head: Box::new(HeadExpression::Simple(SimpleExpression::String(
                    self.expect_string()?,
                ))),
                tail: vec![],
            }],
            token => {
                return Err(ParserError::Unexpected {
                    unexpected: format!("{:?}", token),
                    expected: Some("function arguments".to_owned()),
                });
            }
        };

        Ok(if let Some(method_name) = method_name {
            CallSuffix::Method(method_name, args)
        } else {
            CallSuffix::Function(args)
        })
    }

    fn parse_suffix_part(&mut self) -> Result<SuffixPart<S>, ParserError> {
        match self.get_next()? {
            Token::Dot | Token::LeftBracket => Ok(SuffixPart::Field(self.parse_field_suffix()?)),
            Token::Colon | Token::LeftParen | Token::LeftBrace | Token::String(_) => {
                Ok(SuffixPart::Call(self.parse_call_suffix()?))
            }
            token => Err(ParserError::Unexpected {
                unexpected: format!("{:?}", token),
                expected: Some("expression suffix".to_owned()),
            }),
        }
    }

    fn parse_suffixed_expression(&mut self) -> Result<SuffixedExpression<S>, ParserError> {
        let primary = self.parse_primary_expression()?;
        let mut suffixes = Vec::new();
        loop {
            match self.look_ahead(0)? {
                Some(&Token::Dot)
                | Some(&Token::LeftBracket)
                | Some(&Token::Colon)
                | Some(&Token::LeftParen)
                | Some(&Token::LeftBrace)
                | Some(&Token::String(_)) => {
                    suffixes.push(self.parse_suffix_part()?);
                }
                _ => break,
            }
        }

        Ok(SuffixedExpression { primary, suffixes })
    }

    fn parse_function_definition(&mut self) -> Result<FunctionDefinition<S>, ParserError> {
        self.expect_next(Token::LeftParen)?;

        let mut parameters = Vec::new();
        let mut has_varargs = false;
        if !self.check_ahead(0, Token::RightParen)? {
            loop {
                match self.take_next()? {
                    Token::Name(name) => parameters.push(name),
                    Token::Dots => {
                        has_varargs = true;
                        break;
                    }
                    token => {
                        return Err(ParserError::Unexpected {
                            unexpected: format!("{:?}", token),
                            expected: Some("parameter name or '...'".to_owned()),
                        });
                    }
                }
                if self.check_ahead(0, Token::Comma)? {
                    self.take_next()?;
                } else {
                    break;
                }
            }
        }
        self.expect_next(Token::RightParen)?;

        let body = self.parse_block()?;
        self.expect_next(Token::End)?;

        Ok(FunctionDefinition {
            parameters,
            has_varargs,
            body,
        })
    }

    fn parse_table_constructor(&mut self) -> Result<TableConstructor<S>, ParserError> {
        self.expect_next(Token::LeftBrace)?;
        let mut fields = Vec::new();
        loop {
            if self.check_ahead(0, Token::RightBrace)? {
                break;
            }
            fields.push(self.parse_constructor_field()?);
            match *self.get_next()? {
                Token::Comma | Token::SemiColon => {
                    self.take_next()?;
                }
                _ => break,
            }
        }
        self.expect_next(Token::RightBrace)?;
        Ok(TableConstructor { fields })
    }

    fn parse_constructor_field(&mut self) -> Result<ConstructorField<S>, ParserError> {
        Ok(match *self.get_next()? {
            Token::Name(_) => {
                if self.check_ahead(1, Token::Assign)? {
                    let key = self.expect_name()?;
                    self.expect_next(Token::Assign)?;
                    let value = self.parse_expression()?;
                    ConstructorField::Record(RecordKey::Named(key), value)
                } else {
                    ConstructorField::Array(self.parse_expression()?)
                }
            }
            Token::LeftBracket => {
                self.take_next()?;
                let key = self.parse_expression()?;
                self.expect_next(Token::RightBracket)?;
                self.expect_next(Token::Assign)?;
                let value = self.parse_expression()?;
                return Ok(ConstructorField::Record(RecordKey::Indexed(key), value));
            }
            _ => ConstructorField::Array(self.parse_expression()?),
        })
    }

    // Error if we have more than MAX_RECURSION guards live, otherwise return a new recursion guard
    // (a recursion guard is just an Rc used solely for its live count).
    fn recursion_guard(&self) -> Result<Rc<()>, ParserError> {
        if Rc::strong_count(&self.recursion_guard) < MAX_RECURSION {
            Ok(self.recursion_guard.clone())
        } else {
            Err(ParserError::RecursionLimit)
        }
    }

    // Return a reference to the next token in the stream, erroring if we are at the end.
    fn get_next(&mut self) -> Result<&Token<S>, ParserError> {
        self.read_ahead(1)?;
        if let Some(token) = self.read_buffer.get(0) {
            Ok(token)
        } else {
            Err(ParserError::EndOfStream { expected: None })
        }
    }

    // Consumes the next token, returning an error if it does not match the given token.
    fn expect_next(&mut self, token: Token<S>) -> Result<(), ParserError> {
        self.read_ahead(1)?;
        if self.read_buffer.is_empty() {
            Err(ParserError::EndOfStream {
                expected: Some(format!("{:?}", token)),
            })
        } else {
            let next_token = self.read_buffer.remove(0);
            if next_token == token {
                Ok(())
            } else {
                Err(ParserError::Unexpected {
                    unexpected: format!("{:?}", next_token),
                    expected: Some(format!("{:?}", token)),
                })
            }
        }
    }

    // Consume the next token which should be a name, and return it, otherwise error.
    fn expect_name(&mut self) -> Result<S, ParserError> {
        self.read_ahead(1)?;
        if self.read_buffer.is_empty() {
            Err(ParserError::EndOfStream {
                expected: Some("name".to_owned()),
            })
        } else {
            match self.read_buffer.remove(0) {
                Token::Name(name) => Ok(name),
                token => Err(ParserError::Unexpected {
                    unexpected: format!("{:?}", token),
                    expected: Some("name".to_owned()),
                }),
            }
        }
    }

    // Consume the next token which should be a string, and return it, otherwise error.
    fn expect_string(&mut self) -> Result<S, ParserError> {
        self.read_ahead(1)?;
        if self.read_buffer.is_empty() {
            Err(ParserError::EndOfStream {
                expected: Some("string".to_owned()),
            })
        } else {
            match self.read_buffer.remove(0) {
                Token::String(string) => Ok(string),
                token => Err(ParserError::Unexpected {
                    unexpected: format!("{:?}", token),
                    expected: Some("string".to_owned()),
                }),
            }
        }
    }

    // Take the next token in the stream by value, erroring if we are at the end.
    fn take_next(&mut self) -> Result<Token<S>, ParserError> {
        self.read_ahead(1)?;
        if self.read_buffer.is_empty() {
            Err(ParserError::EndOfStream { expected: None })
        } else {
            Ok(self.read_buffer.remove(0))
        }
    }

    // Return the nth token ahead in the stream, if it is not past the end.
    fn look_ahead(&mut self, n: usize) -> Result<Option<&Token<S>>, ParserError> {
        self.read_ahead(n + 1)?;
        Ok(self.read_buffer.get(n))
    }

    // Return true if the nth token ahead in the stream matches the given token.  If this would read
    // past the end of the stream, this will simply return false.
    fn check_ahead(&mut self, n: usize, token: Token<S>) -> Result<bool, ParserError> {
        self.read_ahead(n)?;
        Ok(if let Some(t) = self.read_buffer.get(n) {
            *t == token
        } else {
            false
        })
    }

    // Read at least `n` tokens ahead in the stream, filling the read buffer up to size `n` (if
    // possible).
    fn read_ahead(&mut self, n: usize) -> Result<(), ParserError> {
        while self.read_buffer.len() <= n {
            if let Some(token) = self.lexer.read_token().map_err(ParserError::LexerError)? {
                self.read_buffer.push(token);
            } else {
                break;
            }
        }
        Ok(())
    }
}

const MAX_RECURSION: usize = 200;

// Priority lower than any unary or binary operator.
const MIN_PRIORITY: u8 = 0;

// Priority of all unary operators.
const UNARY_PRIORITY: u8 = 12;

// Returns the left and right priority of the given binary operator.
fn binary_priority(operator: BinaryOperator) -> (u8, u8) {
    match operator {
        BinaryOperator::Add => (10, 10),
        BinaryOperator::Sub => (10, 10),
        BinaryOperator::Mul => (11, 11),
        BinaryOperator::Mod => (11, 11),
        BinaryOperator::Pow => (14, 13),
        BinaryOperator::Div => (11, 11),
        BinaryOperator::IDiv => (11, 11),
        BinaryOperator::BitAnd => (6, 6),
        BinaryOperator::BitOr => (4, 4),
        BinaryOperator::BitXor => (5, 5),
        BinaryOperator::ShiftLeft => (7, 7),
        BinaryOperator::ShiftRight => (7, 7),
        BinaryOperator::Concat => (9, 8),
        BinaryOperator::NotEqual => (10, 10),
        BinaryOperator::Equal => (3, 3),
        BinaryOperator::LessThan => (3, 3),
        BinaryOperator::LessEqual => (3, 3),
        BinaryOperator::GreaterThan => (3, 3),
        BinaryOperator::GreaterEqual => (3, 3),
        BinaryOperator::And => (2, 2),
        BinaryOperator::Or => (1, 1),
    }
}

// Get the unary operator associated with the given token, if it exists.
fn get_unary_operator<S>(token: &Token<S>) -> Option<UnaryOperator> {
    match *token {
        Token::Not => Some(UnaryOperator::Not),
        Token::Minus => Some(UnaryOperator::Minus),
        Token::BitNotXor => Some(UnaryOperator::BitNot),
        Token::Len => Some(UnaryOperator::Len),
        _ => None,
    }
}

// Get the binary operator associated with the given token, if it exists.
fn get_binary_operator<S>(token: &Token<S>) -> Option<BinaryOperator> {
    match *token {
        Token::Minus => Some(BinaryOperator::Sub),
        Token::Add => Some(BinaryOperator::Add),
        Token::Mul => Some(BinaryOperator::Mul),
        Token::Mod => Some(BinaryOperator::Mod),
        Token::Pow => Some(BinaryOperator::Pow),
        Token::Div => Some(BinaryOperator::Div),
        Token::IDiv => Some(BinaryOperator::IDiv),
        Token::BitAnd => Some(BinaryOperator::BitAnd),
        Token::BitOr => Some(BinaryOperator::BitOr),
        Token::BitNotXor => Some(BinaryOperator::BitXor),
        Token::ShiftLeft => Some(BinaryOperator::ShiftLeft),
        Token::ShiftRight => Some(BinaryOperator::ShiftRight),
        Token::Concat => Some(BinaryOperator::Concat),
        Token::NotEqual => Some(BinaryOperator::NotEqual),
        Token::Equal => Some(BinaryOperator::Equal),
        Token::LessThan => Some(BinaryOperator::LessThan),
        Token::LessEqual => Some(BinaryOperator::LessEqual),
        Token::GreaterThan => Some(BinaryOperator::GreaterThan),
        Token::GreaterEqual => Some(BinaryOperator::GreaterEqual),
        Token::And => Some(BinaryOperator::And),
        Token::Or => Some(BinaryOperator::Or),
        _ => None,
    }
}
