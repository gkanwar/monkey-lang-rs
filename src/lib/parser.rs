use crate::lexer;
use crate::lexer::{IdentIdx, StringIdx, Token};

#[derive(Debug, Eq, PartialEq)]
pub enum CmpType {
  Less,
  LessEquals,
  Greater,
  GreaterEquals,
  Equals,
  NotEquals,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Expr {
  BinaryOp(BinaryOp, Box<Expr>, Box<Expr>),
  UnaryOp(UnaryOp, Box<Expr>),
  Atom(Atomic),
}
#[derive(Debug, Eq, PartialEq)]
pub enum BinaryOp {
  CmpEquals,
  CmpNotEquals,
  CmpLess,
  CmpLessEquals,
  CmpGreater,
  CmpGreaterEquals,
  Plus,
  Minus,
  Times,
  Divide,
}
#[derive(Debug, Eq, PartialEq)]
pub enum UnaryOp {
  Not,
  UPlus,
  UMinus,
}
#[derive(Debug, Eq, PartialEq)]
pub enum Atomic {
  Var(IdentIdx),
  LiteralString(StringIdx),
  LiteralInt(i64),
  LiteralBool(bool),
}

#[derive(Debug, Eq, PartialEq)]
pub enum Statement {
  Assign {
    left: IdentIdx,
    right: Expr,
  },
  IfElse {
    pred: Expr,
    yes: Vec<Statement>,
    no: Option<Vec<Statement>>,
  },
  FnDefn {
    args: Vec<IdentIdx>,
    body: Vec<Statement>,
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
  UnexpectedToken { got: Token, expected: String },
}

pub struct Program {
  statements: Vec<Statement>,
  idents: Vec<String>,
  strings: Vec<String>,
}

macro_rules! expect_tok {
  ( $tokens:expr, $i_mut_ref:expr, $expected:pat, $exp_str:expr ) => {
    if $tokens.len() <= *$i_mut_ref {
      Err(ParseError::EarlyEof(
        std::format!("expected token {}", $exp_str).into(),
      ))
    } else {
      match $tokens[*$i_mut_ref] {
        $expected => {
          *$i_mut_ref += 1;
          Ok(())
        }
        _ => Err(ParseError::UnexpectedToken {
          got: $tokens[*$i_mut_ref],
          expected: $exp_str.into(),
        }),
      }
    }
  };
}

fn infix_binding_power(bin_op: &BinaryOp) -> (f64, f64) {
  use BinaryOp::*;
  match bin_op {
    // function call
    // ParenOpen => (6.0, 0.0),
    Times | Divide => (4.0, 4.1),
    Plus | Minus => (3.0, 3.1),
    CmpLess | CmpLessEquals | CmpGreater | CmpGreaterEquals => (2.0, 2.1),
    CmpEquals | CmpNotEquals => (1.0, 1.1),
  }
}

fn prefix_binding_power(op: &UnaryOp) -> f64 {
  use UnaryOp::*;
  match op {
    UPlus | UMinus | Not => 5.0,
  }
}

// https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
fn consume_expr_bp(tokens: &Vec<Token>, i: &mut usize, bp: f64) -> Result<Expr, ParseError> {
  println!("consume_expr_bp {} {:?}", *i, tokens[*i]);
  if tokens.len() <= *i {
    return Err(ParseError::UntermExpr("expected an expression".into()));
  }
  let peek = tokens[*i];
  let mut lhs = match peek {
    // atom
    Token::LiteralInt(x) => {
      *i += 1;
      Expr::Atom(Atomic::LiteralInt(x))
    }
    Token::LiteralString(x) => {
      *i += 1;
      Expr::Atom(Atomic::LiteralString(x))
    }
    Token::LiteralBool(x) => {
      *i += 1;
      Expr::Atom(Atomic::LiteralBool(x))
    }
    Token::Identifier(x) => {
      *i += 1;
      Expr::Atom(Atomic::Var(x))
    }
    // wrapping
    Token::ParenOpen => {
      expect_tok!(tokens, i, Token::ParenOpen, "(")?;
      let expr = consume_expr(tokens, i)?;
      expect_tok!(tokens, i, Token::ParenClose, ")")?;
      expr
    }
    // prefix
    Token::Not => {
      *i += 1;
      let op = UnaryOp::Not;
      let r_bp = prefix_binding_power(&op);
      Expr::UnaryOp(op, Box::new(consume_expr_bp(tokens, i, r_bp)?))
    }
    _ => todo!(),
  };
  while *i < tokens.len() {
    println!("checking binop");
    let op = tokens[*i];
    let maybe_bin_op = match op {
      Token::CmpEquals => Some(BinaryOp::CmpEquals),
      Token::CmpNotEquals => Some(BinaryOp::CmpNotEquals),
      Token::CmpLess => Some(BinaryOp::CmpLess),
      Token::CmpLessEquals => Some(BinaryOp::CmpLessEquals),
      Token::CmpGreater => Some(BinaryOp::CmpGreater),
      Token::CmpGreaterEquals => Some(BinaryOp::CmpGreaterEquals),
      Token::Plus => Some(BinaryOp::Plus),
      Token::Minus => Some(BinaryOp::Minus),
      Token::Times => Some(BinaryOp::Times),
      Token::Divide => Some(BinaryOp::Divide),
      _ => {
        break;
      } // TODO: wrapping ops like parens
    };
    let bin_op = maybe_bin_op.unwrap();
    let (l_bp, r_bp) = infix_binding_power(&bin_op);
    if l_bp < bp {
      break;
    }
    *i += 1;
    let rhs = consume_expr_bp(tokens, i, r_bp)?;
    lhs = Expr::BinaryOp(bin_op, Box::new(lhs), Box::new(rhs));
  }
  println!("Final expr: {:?}", lhs);
  Ok(lhs)
}

fn consume_expr(tokens: &Vec<Token>, i: &mut usize) -> Result<Expr, ParseError> {
  consume_expr_bp(tokens, i, 0.0)
}

fn consume_if_else(tokens: &Vec<Token>, i: &mut usize) -> Result<Statement, ParseError> {
  expect_tok!(tokens, i, Token::If, "if")?;
  expect_tok!(tokens, i, Token::ParenOpen, "(")?;
  let pred_expr = consume_expr(tokens, i)?;
  expect_tok!(tokens, i, Token::ParenClose, ")")?;
  expect_tok!(tokens, i, Token::CurlyOpen, "{")?;
  let body = consume_multi_statement(tokens, i)?;
  expect_tok!(tokens, i, Token::CurlyClose, "}")?;
  let peek_else = tokens.get(*i);
  match peek_else {
    Some(Token::Else) => {}
    _ => {
      return Ok(Statement::IfElse {
        pred: pred_expr,
        yes: body,
        no: None,
      });
    }
  };
  expect_tok!(tokens, i, Token::Else, "else")?;
  let else_body = consume_multi_statement(tokens, i)?;
  expect_tok!(tokens, i, Token::CurlyClose, "}")?;
  Ok(Statement::IfElse {
    pred: pred_expr,
    yes: body,
    no: Some(else_body),
  })
}

fn consume_multi_statement(
  tokens: &Vec<Token>,
  i: &mut usize,
) -> Result<Vec<Statement>, ParseError> {
  let mut stmts: Vec<Statement> = vec![];
  while *i < tokens.len() {
    match tokens[*i] {
      Token::CurlyClose => {
        break;
      }
      _ => {}
    };
    println!("Consume block statement {} of {}", *i, tokens.len());
    let maybe_stmt = consume_statement(tokens, i)?;
    match maybe_stmt {
      Some(stmt) => stmts.push(stmt),
      None => {}
    };
  }
  Ok(stmts)
}

fn consume_let(tokens: &Vec<Token>, i: &mut usize) -> Result<Statement, ParseError> {
  expect_tok!(tokens, i, Token::Let, "let")?;
  expect_tok!(tokens, i, Token::Identifier(_), "identifier")?;
  let var_tok = tokens[*i - 1];
  let var = match var_tok {
    Token::Identifier(idx) => idx,
    _ => unimplemented!(),
  };
  expect_tok!(tokens, i, Token::Equals, "=")?;
  let expr = consume_expr(tokens, i)?;
  Ok(Statement::Assign {
    left: var,
    right: expr,
  })
}

fn consume_statement(tokens: &Vec<Token>, i: &mut usize) -> Result<Option<Statement>, ParseError> {
  let peek = match tokens.get(*i) {
    Some(x) => x,
    None => {
      return Ok(None);
    }
  };
  use Token::*;
  let stmt: Statement = match peek {
    Let => consume_let(tokens, i)?,
    Fn | ParenOpen | ParenClose | SquareOpen | SquareClose | Not | Minus | Identifier(_) | Plus
    | Minus | Times | Divide | Not | CmpEquals | CmpNotEquals | CmpLess | CmpLessEquals
    | CmpGreater | CmpGreaterEquals | Comma | Colon | LiteralString(_) | LiteralInt(_)
    | LiteralBool(_) => {
      let stmt = Statement::Bare(consume_expr(tokens, i)?);
      expect_tok!(tokens, i, Token::Semicolon, ";")?;
      stmt
    }
    If => consume_if_else(tokens, i)?,
    Else => {
      return Err(ParseError::General("else without if".into()));
    }
    Return => {
      *i += 1;
      Statement::Return(consume_expr(tokens, i)?)
    }
    Semicolon => {
      return Ok(None);
    }
    Equals => {
      return Err(ParseError::General("equals sign without assignment".into()));
    }
    CurlyOpen => {
      *i += 1;
      let stmts = consume_multi_statement(tokens, i)?;
      expect_tok!(tokens, i, Token::CurlyClose, "}")?;
      Statement::Block(stmts)
    }
    CurlyClose => {
      return Err(ParseError::General("got } without opening block".into()));
    }
  };
  Ok(Some(stmt))
}

pub fn parse(prog: lexer::Program) -> Result<Program, ParseError> {
  let mut i: usize = 0;
  let statements = consume_multi_statement(&prog.tokens, &mut i)?;
  Ok(Program {
    statements,
    idents: prog.idents,
    strings: prog.strings,
  })
}

mod tests {
  use super::Atomic::*;
  use super::BinaryOp::*;
  use super::Expr::*;
  use super::UnaryOp::*;
  use super::*;
  use crate::lexer::lex;
  #[test]
  fn let_stmt() {
    let res = lex(r#"1+2/3*4;"#.chars().collect());
    let res = parse(res.unwrap());
    if let Err(e) = &res {
      println!("e = {:?}", e);
    }
    assert!(res.is_ok());
    let prog = res.unwrap();
    assert_eq!(
      prog.statements,
      vec![Statement::Bare(BinaryOp(
        Plus,
        Box::new(Atom(LiteralInt(1))),
        Box::new(BinaryOp(
          Times,
          Box::new(BinaryOp(
            Divide,
            Box::new(Atom(LiteralInt(2))),
            Box::new(Atom(LiteralInt(3)))
          )),
          Box::new(Atom(LiteralInt(4)))
        ))
      ))]
    );
  }
}
