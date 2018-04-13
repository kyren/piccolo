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
    pub parameter_list: Vec<FunctionParameter>,
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

pub enum SimpleExpression {
    Float(f64),
    Int(i64),
    String(Box<[u8]>),
    Nil,
    True,
    False,
    TableConstructor(TableConstructor),
    Function(FunctionExpression),
    Suffixed(SuffixedExpression),
}

pub enum PrefixExpression {
    Simple(SimpleExpression),
    UnaryOperator(UnaryOperator, Expression),
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
    pub assignments: Vec<(SuffixedExpression, VarSuffix, Expression)>,
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

pub enum UnaryOperator {
    Not,
    Minus,
    BitNot,
    Len,
}

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
    read_buffer: Vec<(Token, u64)>,
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
            (Token::Assign, _) => {
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

            (Token::Comma, _) | (Token::In, _) => {
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

            (token, line_num) => Err(format_err!(
                "unexpected token {:?} at line: {}, expectede '=' or 'in'",
                token,
                line_num
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
        unimplemented!()
    }

    fn parse_local_statement(&mut self) -> Result<LocalStatement, Error> {
        unimplemented!()
    }

    fn parse_label_statement(&mut self) -> Result<LabelStatement, Error> {
        unimplemented!()
    }

    fn parse_goto_statement(&mut self) -> Result<GotoStatement, Error> {
        unimplemented!()
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, Error> {
        unimplemented!()
    }

    fn parse_expression(&mut self) -> Result<Expression, Error> {
        unimplemented!()
    }

    fn parse_expression_list(&mut self) -> Result<Vec<Expression>, Error> {
        let mut expressions = Vec::new();
        expressions.push(self.parse_expression()?);
        while self.check_ahead(0, Token::Comma)? {
            expressions.push(self.parse_expression()?);
        }
        Ok(expressions)
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

    fn parse_simple_expression(&mut self) -> Result<SimpleExpression, Error> {
        unimplemented!()
    }

    fn parse_prefix_expression(&mut self) -> Result<PrefixExpression, Error> {
        unimplemented!()
    }

    fn parse_function_expression(&mut self) -> Result<FunctionExpression, Error> {
        unimplemented!()
    }

    fn parse_function_call_statement(&mut self) -> Result<FunctionCallStatement, Error> {
        unimplemented!()
    }

    fn parse_assignment_statement(&mut self) -> Result<AssignmentStatement, Error> {
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
        if let Some(&(ref token, _)) = self.read_buffer.get(0) {
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
            let (next_token, line_num) = self.read_buffer.remove(0);
            if next_token == token {
                Ok(())
            } else {
                Err(format_err!(
                    "expected token {:?} found {:?} on line: {}",
                    token,
                    next_token,
                    line_num + 1
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
                (Token::Name(name), _) => Ok(name),
                (token, line_num) => Err(format_err!(
                    "expected name found {:?} on line: {}",
                    token,
                    line_num + 1
                )),
            }
        }
    }

    // Take the next token in the stream by value, erroring if we are at the end.
    fn take_next(&mut self) -> Result<(Token, u64), Error> {
        self.read_ahead(1)?;
        if self.read_buffer.is_empty() {
            Err(format_err!(
                "unexpected end of token stream at line: {}",
                self.lexer.line_number()
            ))
        } else {
            Ok(self.read_buffer.remove(0))
        }
    }

    // Return true if the nth token ahead in the stream matches the given token.  If this would read
    // past the end of the stream, this will simply return false.
    fn check_ahead(&mut self, n: usize, token: Token) -> Result<bool, Error> {
        self.read_ahead(n)?;
        Ok(if let Some(&(ref t, _)) = self.read_buffer.get(n) {
            *t == token
        } else {
            false
        })
    }

    // Read at least `n` tokens ahead in the stream, filling the read buffer up to size `n` (if
    // possible).
    fn read_ahead(&mut self, n: usize) -> Result<(), Error> {
        while self.read_buffer.len() <= n {
            self.lexer.skip_whitespace()?;
            let line_number = self.lexer.line_number();
            if let Some(token) = self.lexer.read_token()? {
                self.read_buffer.push((token, line_number));
            } else {
                break;
            }
        }
        Ok(())
    }
}
