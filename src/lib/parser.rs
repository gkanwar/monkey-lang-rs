use crate::lexer;
use crate::lexer::{ StringIdx, IdentIdx, Token };

#[derive(Debug,Eq,PartialEq)]
pub enum CmpType {
  Less, LessEquals, Greater, GreaterEquals,
  Equals, NotEquals
}

#[derive(Debug,Eq,PartialEq)]
pub enum Expr {
  // recursive
  Add(Box<Expr>, Box<Expr>),
  Sub(Box<Expr>, Box<Expr>),
  Mul(Box<Expr>, Box<Expr>),
  Div(Box<Expr>, Box<Expr>),
  Not(Box<Expr>),
  Cmp(Box<Expr>, Box<Expr>, CmpType),
  // terminal
  Read(IdentIdx),
  LiteralString(StringIdx),
  LiteralInt(i64),
  LiteralBool(bool)
}

#[derive(Debug,Eq,PartialEq)]
pub enum Statement {
  Assign {
    left: IdentIdx,
    right: Expr
  },
  IfElse {
    pred: Expr,
    yes: Vec<Statement>,
    no: Option<Vec<Statement>>
  },
  FnDefn {
    args: Vec<IdentIdx>,
    body: Vec<Statement>
  },
  Return(Expr),
  Block(Vec<Statement>),
  // just an expr like a bare fn call
  Bare(Expr),
}

#[derive(Debug)]
pub enum ParseError {
  General(String),
  EarlyEof(String),
  UntermStmt(String),
  UntermExpr(String),
  UnexpectedToken {
    got: Token,
    expected: String
  }
}

pub struct Program {
  statements: Vec<Statement>,
  idents: Vec<String>,
  strings: Vec<String>,
}

macro_rules! expect_tok {
  ( $tokens:expr, $i_mut_ref:expr, $expected:pat, $exp_str:expr ) => {
    if $tokens.len() <= *$i_mut_ref {
      Err(ParseError::EarlyEof(std::format!(
        "expected token {}", $exp_str).into()))
    }
    else {
      match $tokens[*$i_mut_ref] {
        $expected => {
          *$i_mut_ref += 1;
          Ok(())
        },
        _ => Err(ParseError::UnexpectedToken {
          got: $tokens[*$i_mut_ref], expected: $exp_str.into() })
      }
    }
  }
}

fn consume_expr(tokens: &Vec<Token>, i: &mut usize)
  -> Result<Expr, ParseError>
{
  todo!();
}

fn consume_if_else(tokens: &Vec<Token>, i: &mut usize)
  -> Result<Statement, ParseError>
{
  expect_tok!(tokens, i, Token::If, "if")?;
  expect_tok!(tokens, i, Token::ParenOpen, "(")?;
  let pred_expr = consume_expr(tokens, i)?;
  expect_tok!(tokens, i, Token::ParenClose, ")")?;
  expect_tok!(tokens, i, Token::CurlyOpen, "{")?;
  let body = consume_block(tokens, i)?;
  expect_tok!(tokens, i, Token::CurlyClose, "}")?;
  let peek_else = tokens.get(*i);
  match peek_else {
    Some(Token::Else) => {},
    _ => {
      return Ok(Statement::IfElse {
        pred: pred_expr, yes: body, no: None
      });
    }
  };
  expect_tok!(tokens, i, Token::Else, "else")?;
  let else_body = consume_block(tokens, i)?;
  expect_tok!(tokens, i, Token::CurlyClose, "}")?;
  Ok(Statement::IfElse {
    pred: pred_expr, yes: body, no: Some(else_body)
  })
}

fn consume_block(tokens: &Vec<Token>, i: &mut usize)
  -> Result<Vec<Statement>, ParseError>
{
  let mut stmts: Vec<Statement> = vec![];
  while *i < tokens.len() {
    match tokens[*i] {
      Token::CurlyClose => {
        return Ok(stmts)
      },
      _ => {}
    };
    let maybe_stmt = consume_statement(tokens, i)?;
    match maybe_stmt {
      Some(stmt) => stmts.push(stmt),
      None => {}
    };
  }
  Err(ParseError::EarlyEof("eof before end of block".into()))
}

fn consume_let(tokens: &Vec<Token>, i: &mut usize)
  -> Result<Statement, ParseError>
{
  expect_tok!(tokens, i, Token::Let, "let")?;
  expect_tok!(tokens, i, Token::Identifier(_), "identifier")?;
  let var_tok = tokens[*i-1];
  let var = match var_tok {
    Token::Identifier(idx) => idx,
    _ => unimplemented!()
  };
  expect_tok!(tokens, i, Token::Equals, "=")?;
  let expr = consume_expr(tokens, i)?;
  Ok(Statement::Assign {
    left: var,
    right: expr
  })
}

fn consume_statement(tokens: &Vec<Token>, i: &mut usize)
  -> Result<Option<Statement>, ParseError>
{
  let peek = match tokens.get(*i) {
    Some(x) => x,
    None => {
      return Ok(None);
    }
  };
  use Token::*;
  let stmt: Statement = match peek {
    Let => consume_let(tokens, i)?,
    Fn | ParenOpen | ParenClose | SquareOpen | SquareClose | Not | Minus
      | Identifier(_) | Plus | Minus | Times | Divide | Not | CmpEquals
      | CmpNotEquals | CmpLess | CmpLessEquals | CmpGreater | CmpGreaterEquals
      | Comma | Colon | LiteralString(_) | LiteralInt(_) | LiteralBool(_) => {
      Statement::Bare(consume_expr(tokens, i)?)
    },
    If => {
      consume_if_else(tokens, i)?
    },
    Else => {
      return Err(ParseError::General("else without if".into()));
    },
    Return => {
      *i += 1;
      Statement::Return(consume_expr(tokens, i)?)
    },
    Semicolon => {
      return Ok(None);
    },
    Equals => {
      return Err(ParseError::General("equals sign without assignment".into()));
    },
    CurlyOpen => {
      *i += 1;
      let stmts = consume_block(tokens, i)?;
      expect_tok!(tokens, i, Token::CurlyClose, "}")?;
      Statement::Block(stmts)
    },
    CurlyClose => {
      return Err(ParseError::General("got } without opening block".into()));
    },
  };
  Ok(Some(stmt))
}

pub fn parse(prog: lexer::Program) -> Result<Program, ParseError> {
  let mut i: usize = 0;
  let statements = consume_block(&prog.tokens, &mut i)?;
  Ok(Program {
    statements,
    idents: prog.idents,
    strings: prog.strings
  })
}

mod tests {
  use super::*;
  use crate::lexer::lex;
  #[test]
  fn let_stmt() {
    let res = lex(r#"1+2/3*4;"#.chars().collect());
    let res = parse(res.unwrap());
    assert!(res.is_ok());
    let prog = res.unwrap();
    assert_eq!(
      prog.statements, vec![
      Statement::Bare(
        Expr::Add(
          Box::new(Expr::LiteralInt(1)),
          Box::new(Expr::Mul(
            Box::new(Expr::Div(
              Box::new(Expr::LiteralInt(2)),
              Box::new(Expr::LiteralInt(3)))),
            Box::new(Expr::LiteralInt(4))))))
      ]);
  }
}
