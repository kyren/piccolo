#![allow(unused)]

use std::io::Read;

use failure::{err_msg, Error};

use lexer::{Lexer, Token};

pub struct Chunk {
    pub block: Block,
}

pub struct Block {
    pub statements: Vec<Statement>,
    pub return_statement: Option<ReturnStatement>,
}

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

pub struct ReturnStatement {
    pub returns: Vec<Expression>,
}

pub struct IfStatement {
    pub if_part: (Expression, Block),
    pub else_if_parts: Vec<(Expression, Block)>,
    pub else_part: Option<Block>,
}

pub struct WhileStatement {
    pub condition: Expression,
    pub block: Block,
}

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

pub struct RepeatStatement {
    pub body: Block,
    pub until: Expression,
}

pub struct LabelStatement {
    pub name: Box<[u8]>,
}

pub struct GotoStatement {
    pub name: Box<[u8]>,
}

pub struct FunctionStatement {
    pub name: FunctionName,
    pub parameters: Vec<FunctionParameter>,
    pub body: Block,
}

pub struct LocalStatement {
    pub names: Vec<Box<[u8]>>,
    pub values: Vec<Expression>,
}

pub struct Expression {
    pub prefix: Box<PrefixExpression>,
    pub tail: Vec<(BinaryOperator, Expression)>,
}

pub enum PrefixExpression {
    Simple(SimpleExpression),
    UnaryOperator(UnaryOperator, Expression),
}

pub enum SimpleExpression {
    Float(f64),
    Int(i64),
    String(Box<[u8]>),
    Nil,
    True,
    False,
    VarArgs,
    TableConstructor(TableConstructor),
    Function(FunctionExpression),
    Suffixed(SuffixedExpression),
}

pub enum PrimaryExpression {
    Name(Box<[u8]>),
    GroupedExpression(Expression),
}

pub enum VarSuffix {
    Field(Box<[u8]>),
    Table(Expression),
}

pub enum CallSuffix {
    Method(Box<[u8]>, Vec<Expression>),
    Function(Vec<Expression>),
}

pub enum SuffixPart {
    Var(VarSuffix),
    Call(CallSuffix),
}

pub struct SuffixedExpression {
    pub primary: PrimaryExpression,
    pub suffixes: Vec<SuffixPart>,
}

pub struct FunctionExpression {
    pub parameter_list: Vec<FunctionParameter>,
    pub body: Block,
}

pub struct FunctionCallStatement {
    pub head: SuffixedExpression,
    pub call: CallSuffix,
}

pub struct AssignmentStatement {
    pub targets: Vec<AssignmentTarget>,
    pub values: Vec<Expression>,
}

pub enum AssignmentTarget {
    Name(Box<[u8]>),
    Suffixed(SuffixedExpression, VarSuffix),
}

pub struct FunctionName {
    pub name: Box<[u8]>,
    pub fields: Vec<FieldSelector>,
    pub self_field: Option<Box<[u8]>>,
}

pub struct FieldSelector {
    pub is_method: bool,
    pub field_name: Box<[u8]>,
}

pub enum FunctionParameter {
    Name(Box<[u8]>),
    VarArgs,
}

pub struct TableConstructor {
    pub fields: Vec<ConstructorField>,
}

pub enum ConstructorField {
    List(Expression),
    Record(RecordField, Expression),
}

pub enum RecordField {
    Name(Box<[u8]>),
    Expression(Expression),
}

#[derive(Eq, PartialEq, Copy, Clone)]
pub enum UnaryOperator {
    Not,
    Minus,
    BitNot,
    Len,
}

#[derive(Eq, PartialEq, Copy, Clone)]
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

pub fn parse_chunk<R: Read>(source: R) -> Result<Chunk, Error> {
    Parser::new(source).parse_chunk()
}

struct Parser<R: Read> {
    lexer: Lexer<R>,
    read_buffer: Vec<Token>,
}

impl<R: Read> Parser<R> {
    fn new(source: R) -> Parser<R> {
        Parser {
            lexer: Lexer::new(source),
            read_buffer: Vec::new(),
        }
    }

    fn parse_chunk(&mut self) -> Result<Chunk, Error> {
        Ok(Chunk {
            block: self.parse_block()?,
        })
    }

    fn parse_block(&mut self) -> Result<Block, Error> {
        let mut statements = Vec::new();
        let mut return_statement = None;

        loop {
            if self.at_end()? {
                break;
            } else {
                match *self.get_next()? {
                    Token::Else | Token::ElseIf | Token::End | Token::Until => break,
                    Token::SemiColon => {}
                    Token::Return => {
                        return_statement = Some(self.parse_return_statement()?);
                        break;
                    }
                    _ => {
                        statements.push(self.parse_statement()?);
                    }
                }
            }
        }

        Ok(Block {
            statements,
            return_statement,
        })
    }

    fn parse_statement(&mut self) -> Result<Statement, Error> {
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
        Ok(ReturnStatement {
            returns: self.parse_expression_list()?,
        })
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

        match self.take_next()? {
            Token::Assign => {
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

            token => Err(format_err!(
                "unexpected token {:?} expectede '=' or 'in'",
                token,
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
        self.expect_next(Token::LeftParen)?;

        let mut parameters = Vec::new();
        while !self.check_ahead(0, Token::RightParen)? {
            parameters.push(self.parse_function_parameter()?);
        }
        self.take_next()?;

        let body = self.parse_block()?;
        self.expect_next(Token::End)?;

        Ok(FunctionStatement {
            name,
            parameters,
            body,
        })
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
                        SuffixPart::Var(var_suffix) => {
                            AssignmentTarget::Suffixed(suffixed_expression, var_suffix)
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
                    SuffixPart::Var(_) => Err(err_msg("expression is not a statement")),
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
            expressions.push(self.parse_expression()?);
        }
        Ok(expressions)
    }

    fn parse_sub_expression(&mut self, priority_limit: u8) -> Result<Expression, Error> {
        let prefix = if let Some(unary_op) = get_unary_operator(self.get_next()?) {
            self.take_next()?;
            PrefixExpression::UnaryOperator(unary_op, self.parse_sub_expression(UNARY_PRIORITY)?)
        } else {
            PrefixExpression::Simple(self.parse_simple_expression()?)
        };

        let mut tail = Vec::new();
        while let Some(binary_op) = get_binary_operator(self.get_next()?) {
            let (left_priority, right_priority) = binary_priority(binary_op);
            if left_priority <= priority_limit {
                break;
            }

            self.take_next()?;
            let right_expression = self.parse_sub_expression(right_priority)?;
            tail.push((binary_op, right_expression));
        }

        Ok(Expression {
            prefix: Box::new(prefix),
            tail,
        })
    }

    fn parse_simple_expression(&mut self) -> Result<SimpleExpression, Error> {
        unimplemented!()
    }

    fn parse_primary_expression(&mut self) -> Result<PrimaryExpression, Error> {
        unimplemented!()
    }

    fn parse_var_suffix(&mut self) -> Result<VarSuffix, Error> {
        unimplemented!()
    }

    fn parse_call_suffix(&mut self) -> Result<CallSuffix, Error> {
        unimplemented!()
    }

    fn parse_suffix_part(&mut self) -> Result<SuffixPart, Error> {
        unimplemented!()
    }

    fn parse_suffixed_expression(&mut self) -> Result<SuffixedExpression, Error> {
        unimplemented!()
    }

    fn parse_function_expression(&mut self) -> Result<FunctionExpression, Error> {
        unimplemented!()
    }

    fn parse_function_name(&mut self) -> Result<FunctionName, Error> {
        unimplemented!()
    }

    fn parse_field_selector(&mut self) -> Result<FieldSelector, Error> {
        unimplemented!()
    }

    fn parse_function_parameter(&mut self) -> Result<FunctionParameter, Error> {
        unimplemented!()
    }

    fn parse_table_constructor(&mut self) -> Result<TableConstructor, Error> {
        unimplemented!()
    }

    fn parse_constructor_field(&mut self) -> Result<ConstructorField, Error> {
        unimplemented!()
    }

    fn parse_record_field(&mut self) -> Result<RecordField, Error> {
        unimplemented!()
    }

    // Return true if there are no more tokens left in the token stream.
    fn at_end(&mut self) -> Result<bool, Error> {
        self.read_ahead(1)?;
        Ok(self.read_buffer.is_empty())
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
                token => Err(format_err!("expected name found {:?}", token,)),
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

// Priority lower than any unary or binary operator.
const MIN_PRIORITY: u8 = 0;

// Priority of all unary operators.
const UNARY_PRIORITY: u8 = 12;

// Returns the left and right priority of the given binary operator.
fn binary_priority(binop: BinaryOperator) -> (u8, u8) {
    match binop {
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
