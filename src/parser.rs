use std::io::Read;
use std::rc::Rc;

use failure::{err_msg, format_err, Error};

use crate::lexer::{Lexer, Token};

#[derive(Debug, PartialEq, Clone)]
pub struct Chunk {
    pub block: Block,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub return_statement: Option<ReturnStatement>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    If(IfStatement),
    While(WhileStatement),
    Do(Block),
    For(ForStatement),
    Repeat(RepeatStatement),
    Function(FunctionStatement),
    LocalFunction(FunctionStatement),
    LocalStatement(LocalStatement),
    Label(LabelStatement),
    Break,
    Goto(GotoStatement),
    FunctionCall(FunctionCallStatement),
    Assignment(AssignmentStatement),
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnStatement {
    pub returns: Vec<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfStatement {
    pub if_part: (Expression, Block),
    pub else_if_parts: Vec<(Expression, Block)>,
    pub else_part: Option<Block>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct WhileStatement {
    pub condition: Expression,
    pub block: Block,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ForStatement {
    Num {
        name: Box<[u8]>,
        initial: Expression,
        limit: Expression,
        step: Option<Expression>,
        body: Block,
    },
    List {
        names: Vec<Box<[u8]>>,
        lists: Vec<Expression>,
        body: Block,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub struct RepeatStatement {
    pub body: Block,
    pub until: Expression,
}

#[derive(Debug, PartialEq, Clone)]
pub struct LabelStatement {
    pub name: Box<[u8]>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct GotoStatement {
    pub name: Box<[u8]>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionStatement {
    pub name: FunctionName,
    pub definition: FunctionDefinition,
}

#[derive(Debug, PartialEq, Clone)]
pub struct LocalStatement {
    pub names: Vec<Box<[u8]>>,
    pub values: Vec<Expression>,
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
pub struct Expression {
    pub head: Box<HeadExpression>,
    pub tail: Vec<(BinaryOperator, Expression)>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum HeadExpression {
    Simple(SimpleExpression),
    UnaryOperator(UnaryOperator, Expression),
}

#[derive(Debug, PartialEq, Clone)]
pub enum SimpleExpression {
    Float(f64),
    Integer(i64),
    String(Box<[u8]>),
    Nil,
    True,
    False,
    VarArgs,
    TableConstructor(TableConstructor),
    Function(FunctionDefinition),
    Suffixed(SuffixedExpression),
}

#[derive(Debug, PartialEq, Clone)]
pub enum PrimaryExpression {
    Name(Box<[u8]>),
    GroupedExpression(Expression),
}

#[derive(Debug, PartialEq, Clone)]
pub enum FieldSuffix {
    Named(Box<[u8]>),
    Indexed(Expression),
}

#[derive(Debug, PartialEq, Clone)]
pub enum CallSuffix {
    Method(Box<[u8]>, Vec<Expression>),
    Function(Vec<Expression>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum SuffixPart {
    Field(FieldSuffix),
    Call(CallSuffix),
}

#[derive(Debug, PartialEq, Clone)]
pub struct SuffixedExpression {
    pub primary: PrimaryExpression,
    pub suffixes: Vec<SuffixPart>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDefinition {
    pub parameters: Vec<Box<[u8]>>,
    pub has_varargs: bool,
    pub body: Block,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionCallStatement {
    pub head: SuffixedExpression,
    pub call: CallSuffix,
}

#[derive(Debug, PartialEq, Clone)]
pub struct AssignmentStatement {
    pub targets: Vec<AssignmentTarget>,
    pub values: Vec<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum AssignmentTarget {
    Name(Box<[u8]>),
    Field(SuffixedExpression, FieldSuffix),
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionName {
    pub name: Box<[u8]>,
    pub fields: Vec<Box<[u8]>>,
    pub method: Option<Box<[u8]>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TableConstructor {
    pub fields: Vec<ConstructorField>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ConstructorField {
    Array(Expression),
    Record(RecordKey, Expression),
}

#[derive(Debug, PartialEq, Clone)]
pub enum RecordKey {
    Named(Box<[u8]>),
    Indexed(Expression),
}

pub fn parse_chunk<R: Read>(source: R) -> Result<Chunk, Error> {
    Parser {
        lexer: Lexer::new(source),
        read_buffer: Vec::new(),
        recursion_guard: Rc::new(()),
    }
    .parse_chunk()
}

struct Parser<R: Read> {
    lexer: Lexer<R>,
    read_buffer: Vec<Token>,
    recursion_guard: Rc<()>,
}

impl<R: Read> Parser<R> {
    fn parse_chunk(&mut self) -> Result<Chunk, Error> {
        let block = self.parse_block()?;
        if self.look_ahead(0)? != None {
            Err(err_msg("expected end of token stream"))
        } else {
            Ok(Chunk { block })
        }
    }

    fn parse_block(&mut self) -> Result<Block, Error> {
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

    fn parse_statement(&mut self) -> Result<Statement, Error> {
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
                    Statement::LocalFunction(self.parse_function_statement()?)
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

    fn parse_return_statement(&mut self) -> Result<ReturnStatement, Error> {
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

    fn parse_if_statement(&mut self) -> Result<IfStatement, Error> {
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

    fn parse_while_statement(&mut self) -> Result<WhileStatement, Error> {
        self.expect_next(Token::While)?;
        let condition = self.parse_expression()?;
        self.expect_next(Token::Do)?;
        let block = self.parse_block()?;
        self.expect_next(Token::End)?;

        Ok(WhileStatement { condition, block })
    }

    fn parse_for_statement(&mut self) -> Result<ForStatement, Error> {
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

                Ok(ForStatement::Num {
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
                let lists = self.parse_expression_list()?;

                self.expect_next(Token::Do)?;
                let body = self.parse_block()?;
                self.expect_next(Token::End)?;

                Ok(ForStatement::List { names, lists, body })
            }

            _ => Err(format_err!(
                "unexpected token {:?} expected '=' or 'in'",
                self.take_next()?,
            )),
        }
    }

    fn parse_repeat_statement(&mut self) -> Result<RepeatStatement, Error> {
        self.expect_next(Token::Repeat)?;
        let body = self.parse_block()?;
        self.expect_next(Token::Until)?;
        let until = self.parse_expression()?;
        Ok(RepeatStatement { body, until })
    }

    fn parse_function_statement(&mut self) -> Result<FunctionStatement, Error> {
        self.expect_next(Token::Function)?;
        let name = self.parse_function_name()?;
        let definition = self.parse_function_definition()?;

        Ok(FunctionStatement { name, definition })
    }

    fn parse_local_statement(&mut self) -> Result<LocalStatement, Error> {
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

    fn parse_label_statement(&mut self) -> Result<LabelStatement, Error> {
        self.expect_next(Token::DoubleColon)?;
        let name = self.expect_name()?;
        self.expect_next(Token::DoubleColon)?;
        Ok(LabelStatement { name })
    }

    fn parse_goto_statement(&mut self) -> Result<GotoStatement, Error> {
        self.expect_next(Token::Goto)?;
        let name = self.expect_name()?;
        Ok(GotoStatement { name })
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, Error> {
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
                            return Err(err_msg("cannot assign to expression"));
                        }
                    }
                } else {
                    match suffixed_expression.primary {
                        PrimaryExpression::Name(name) => AssignmentTarget::Name(name),
                        _ => return Err(err_msg("cannot assign to expression")),
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
        } else {
            if let Some(suffix) = suffixed_expression.suffixes.pop() {
                match suffix {
                    SuffixPart::Call(call_suffix) => {
                        Ok(Statement::FunctionCall(FunctionCallStatement {
                            head: suffixed_expression,
                            call: call_suffix,
                        }))
                    }
                    SuffixPart::Field(_) => Err(err_msg("expression is not a statement")),
                }
            } else {
                Err(err_msg("expression is not a statement"))
            }
        }
    }

    fn parse_expression(&mut self) -> Result<Expression, Error> {
        self.parse_sub_expression(MIN_PRIORITY)
    }

    fn parse_expression_list(&mut self) -> Result<Vec<Expression>, Error> {
        let mut expressions = Vec::new();
        expressions.push(self.parse_expression()?);
        while self.check_ahead(0, Token::Comma)? {
            self.take_next()?;
            expressions.push(self.parse_expression()?);
        }
        Ok(expressions)
    }

    fn parse_sub_expression(&mut self, priority_limit: u8) -> Result<Expression, Error> {
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

    fn parse_simple_expression(&mut self) -> Result<SimpleExpression, Error> {
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

    fn parse_primary_expression(&mut self) -> Result<PrimaryExpression, Error> {
        match self.take_next()? {
            Token::LeftParen => {
                let expr = self.parse_expression()?;
                self.expect_next(Token::RightParen)?;
                Ok(PrimaryExpression::GroupedExpression(expr))
            }
            Token::Name(n) => Ok(PrimaryExpression::Name(n)),
            t => Err(format_err!(
                "unexpected token {:?} expected grouped expression or name",
                t
            )),
        }
    }

    fn parse_field_suffix(&mut self) -> Result<FieldSuffix, Error> {
        match *self.get_next()? {
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
            _ => Err(format_err!(
                "unexpected token {:?} expected field suffix",
                self.take_next()?
            )),
        }
    }

    fn parse_call_suffix(&mut self) -> Result<CallSuffix, Error> {
        let method_name = match *self.get_next()? {
            Token::Colon => {
                self.take_next()?;
                Some(self.expect_name()?)
            }
            _ => None,
        };

        let args = match *self.get_next()? {
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
            _ => {
                return Err(format_err!(
                    "unexpected token {:?} expected function arguments",
                    self.take_next()?
                ));
            }
        };

        Ok(if let Some(method_name) = method_name {
            CallSuffix::Method(method_name, args)
        } else {
            CallSuffix::Function(args)
        })
    }

    fn parse_suffix_part(&mut self) -> Result<SuffixPart, Error> {
        match *self.get_next()? {
            Token::Dot | Token::LeftBracket => Ok(SuffixPart::Field(self.parse_field_suffix()?)),
            Token::Colon | Token::LeftParen | Token::LeftBrace | Token::String(_) => {
                Ok(SuffixPart::Call(self.parse_call_suffix()?))
            }
            _ => Err(format_err!(
                "unexpected token {:?} expected expression suffix",
                self.take_next()?
            )),
        }
    }

    fn parse_suffixed_expression(&mut self) -> Result<SuffixedExpression, Error> {
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

    fn parse_function_name(&mut self) -> Result<FunctionName, Error> {
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

        Ok(FunctionName {
            name,
            fields,
            method,
        })
    }

    fn parse_function_definition(&mut self) -> Result<FunctionDefinition, Error> {
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
                        return Err(format_err!(
                            "unexpected token {:?} expected parameter name or '...'",
                            token,
                        ))
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

    fn parse_table_constructor(&mut self) -> Result<TableConstructor, Error> {
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

    fn parse_constructor_field(&mut self) -> Result<ConstructorField, Error> {
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
    fn recursion_guard(&self) -> Result<Rc<()>, Error> {
        if Rc::strong_count(&self.recursion_guard) < MAX_RECURSION {
            Ok(self.recursion_guard.clone())
        } else {
            Err(err_msg("recursion limit reached"))
        }
    }

    // Return a reference to the next token in the stream, erroring if we are at the end.
    fn get_next(&mut self) -> Result<&Token, Error> {
        self.read_ahead(1)?;
        if let Some(token) = self.read_buffer.get(0) {
            Ok(token)
        } else {
            Err(format_err!("unexpected end of token stream"))
        }
    }

    // Consumes the next token, returning an error if it does not match the given token.
    fn expect_next(&mut self, token: Token) -> Result<(), Error> {
        self.read_ahead(1)?;
        if self.read_buffer.is_empty() {
            Err(format_err!(
                "unexpected end of token stream, expected {:?}",
                token
            ))
        } else {
            let next_token = self.read_buffer.remove(0);
            if next_token == token {
                Ok(())
            } else {
                Err(format_err!(
                    "expected token {:?} found {:?}",
                    token,
                    next_token,
                ))
            }
        }
    }

    // Consume the next token which should be a name, and return it, otherwise error.
    fn expect_name(&mut self) -> Result<Box<[u8]>, Error> {
        self.read_ahead(1)?;
        if self.read_buffer.is_empty() {
            Err(err_msg("unexpected end of token stream, expected name"))
        } else {
            match self.read_buffer.remove(0) {
                Token::Name(name) => Ok(name),
                token => Err(format_err!("expected name found {:?}", token)),
            }
        }
    }

    // Consume the next token which should be a string, and return it, otherwise error.
    fn expect_string(&mut self) -> Result<Box<[u8]>, Error> {
        self.read_ahead(1)?;
        if self.read_buffer.is_empty() {
            Err(err_msg("unexpected end of token stream, expected string"))
        } else {
            match self.read_buffer.remove(0) {
                Token::String(string) => Ok(string),
                token => Err(format_err!("expected string found {:?}", token)),
            }
        }
    }

    // Take the next token in the stream by value, erroring if we are at the end.
    fn take_next(&mut self) -> Result<Token, Error> {
        self.read_ahead(1)?;
        if self.read_buffer.is_empty() {
            Err(err_msg("unexpected end of token stream"))
        } else {
            Ok(self.read_buffer.remove(0))
        }
    }

    // Return the nth token ahead in the stream, if it is not past the end.
    fn look_ahead(&mut self, n: usize) -> Result<Option<&Token>, Error> {
        self.read_ahead(n + 1)?;
        Ok(self.read_buffer.get(n))
    }

    // Return true if the nth token ahead in the stream matches the given token.  If this would read
    // past the end of the stream, this will simply return false.
    fn check_ahead(&mut self, n: usize, token: Token) -> Result<bool, Error> {
        self.read_ahead(n)?;
        Ok(if let Some(t) = self.read_buffer.get(n) {
            *t == token
        } else {
            false
        })
    }

    // Read at least `n` tokens ahead in the stream, filling the read buffer up to size `n` (if
    // possible).
    fn read_ahead(&mut self, n: usize) -> Result<(), Error> {
        while self.read_buffer.len() <= n {
            if let Some(token) = self.lexer.read_token()? {
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
fn get_unary_operator(token: &Token) -> Option<UnaryOperator> {
    match *token {
        Token::Not => Some(UnaryOperator::Not),
        Token::Minus => Some(UnaryOperator::Minus),
        Token::BitNotXor => Some(UnaryOperator::BitNot),
        Token::Len => Some(UnaryOperator::Len),
        _ => None,
    }
}

// Get the binary operator associated with the given token, if it exists.
fn get_binary_operator(token: &Token) -> Option<BinaryOperator> {
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
