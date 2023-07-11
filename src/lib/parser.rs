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
  WrapOp(WrapOp, Box<Expr>, Vec<Expr>),
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
  LogicalAnd,
  LogicalOr,
  Plus,
  Minus,
  Times,
  Divide,
}
#[derive(Debug, Eq, PartialEq)]
pub enum UnaryOp {
  LogicalNot,
  UPlus,
  UMinus,
}
#[derive(Debug, Eq, PartialEq)]
pub enum WrapOp {
  FnCall,
}
#[derive(Debug, Eq, PartialEq)]
pub enum Atomic {
  Var(IdentIdx),
  LiteralString(StringIdx),
  LiteralInt(i64),
  LiteralBool(bool),
  Func {
    args: Vec<Expr>,
    body: Vec<Statement>,
  },
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
    LogicalAnd => (0.5, 0.6),
    LogicalOr => (0.3, 0.4),
  }
}

fn prefix_binding_power(op: &UnaryOp) -> f64 {
  use UnaryOp::*;
  match op {
    UPlus | UMinus | LogicalNot => 5.0,
  }
}

fn consume_args(tokens: &Vec<Token>, i: &mut usize) -> Result<Vec<Expr>, ParseError> {
  let mut args: Vec<Expr> = vec![];
  while *i < tokens.len() {
    args.push(consume_expr(tokens, i)?);
    if tokens.len() <= *i {
      return Err(ParseError::EarlyEof("expected fn args".into()));
    }
    match tokens[*i] {
      Token::Comma => {
        *i += 1;
      }
      Token::ParenClose => {
        break;
      }
      _ => {
        return Err(ParseError::UnexpectedToken {
          got: tokens[*i],
          expected: "continuation of args or close paren".into(),
        });
      }
    };
  }
  Ok(args)
}

// https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
fn consume_expr_bp(tokens: &Vec<Token>, i: &mut usize, bp: f64) -> Result<Expr, ParseError> {
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
    Token::LogicalNot => {
      *i += 1;
      let op = UnaryOp::LogicalNot;
      let r_bp = prefix_binding_power(&op);
      Expr::UnaryOp(op, Box::new(consume_expr_bp(tokens, i, r_bp)?))
    }
    Token::Minus => {
      *i += 1;
      let op = UnaryOp::UMinus;
      let r_bp = prefix_binding_power(&op);
      Expr::UnaryOp(op, Box::new(consume_expr_bp(tokens, i, r_bp)?))
    }
    Token::Plus => {
      *i += 1;
      let op = UnaryOp::UPlus;
      let r_bp = prefix_binding_power(&op);
      Expr::UnaryOp(op, Box::new(consume_expr_bp(tokens, i, r_bp)?))
    }
    // misc
    Token::Fn => {
      expect_tok!(tokens, i, Token::Fn, "fn")?;
      expect_tok!(tokens, i, Token::ParenOpen, "(")?;
      let args = consume_args(tokens, i)?;
      expect_tok!(tokens, i, Token::ParenClose, ")")?;
      expect_tok!(tokens, i, Token::CurlyOpen, "{")?;
      let body = consume_multi_statement(tokens, i)?;
      expect_tok!(tokens, i, Token::CurlyClose, "}")?;
      Expr::Atom(Atomic::Func { args, body })
    }
    _ => {
      return Err(ParseError::General(std::format!(
            "unexpected token in expression: {:?}", peek)));
    }
  };
  while *i < tokens.len() {
    let op = tokens[*i];
    let maybe_bin_op = match op {
      Token::CmpEquals => Some(BinaryOp::CmpEquals),
      Token::CmpNotEquals => Some(BinaryOp::CmpNotEquals),
      Token::CmpLess => Some(BinaryOp::CmpLess),
      Token::CmpLessEquals => Some(BinaryOp::CmpLessEquals),
      Token::CmpGreater => Some(BinaryOp::CmpGreater),
      Token::CmpGreaterEquals => Some(BinaryOp::CmpGreaterEquals),
      Token::LogicalOr => Some(BinaryOp::LogicalOr),
      Token::LogicalAnd => Some(BinaryOp::LogicalAnd),
      Token::Plus => Some(BinaryOp::Plus),
      Token::Minus => Some(BinaryOp::Minus),
      Token::Times => Some(BinaryOp::Times),
      Token::Divide => Some(BinaryOp::Divide),
      _ => None,
    };
    if let Some(bin_op) = maybe_bin_op {
      let (l_bp, r_bp) = infix_binding_power(&bin_op);
      if l_bp < bp {
        break;
      }
      *i += 1;
      let rhs = consume_expr_bp(tokens, i, r_bp)?;
      lhs = Expr::BinaryOp(bin_op, Box::new(lhs), Box::new(rhs));
      continue;
    }
    let maybe_wrap_op = match op {
      Token::ParenOpen => Some((WrapOp::FnCall, Token::ParenClose)),
      _ => None,
    };
    if let Some((wrap_op, close_tok)) = maybe_wrap_op {
      *i += 1;
      let args = consume_args(tokens, i)?;
      if tokens.len() <= *i || tokens[*i] != close_tok {
        return Err(ParseError::UnexpectedToken {
          got: tokens[*i],
          expected: "closing token".into(),
        });
      }
      *i += 1;
      lhs = Expr::WrapOp(wrap_op, Box::new(lhs), args);
      continue;
    }
    break;
  }
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
  expect_tok!(tokens, i, Token::CurlyOpen, "{")?;
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
    Let => {
      let stmt = consume_let(tokens, i)?;
      expect_tok!(tokens, i, Token::Semicolon, ";")?;
      stmt
    }
    Fn | ParenOpen | ParenClose | SquareOpen | SquareClose | LogicalNot | LogicalAnd
    | LogicalOr | Minus | Identifier(_) | Plus
    | Times | Divide | CmpEquals | CmpNotEquals | CmpLess | CmpLessEquals | CmpGreater
    | CmpGreaterEquals | Comma | Colon | LiteralString(_) | LiteralInt(_) | LiteralBool(_) => {
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
      *i += 1;
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
  use super::WrapOp::*;
  use super::*;
  use crate::lexer::lex;

  trait StandardFmt {
    fn standard_format(&self, idents: &Vec<String>, strings: &Vec<String>) -> String;
  }

  fn standard_format_binop(bin_op: &super::BinaryOp) -> String {
    match bin_op {
      CmpEquals => "==",
      CmpNotEquals => "!=",
      CmpLess => "<",
      CmpLessEquals => "<=",
      CmpGreater => ">",
      CmpGreaterEquals => ">=",
      LogicalAnd => "&&",
      LogicalOr => "||",
      Plus => "+",
      Minus => "-",
      Times => "*",
      Divide => "/",
    }
    .into()
  }

  fn standard_format_unop(un_op: &super::UnaryOp) -> String {
    match un_op {
      LogicalNot => "!",
      UPlus => "+",
      UMinus => "-",
    }
    .into()
  }

  fn standard_format_wrapop(wrap_op: &super::WrapOp) -> (String, String) {
    match wrap_op {
      FnCall => ("(".into(), ")".into()),
    }
  }
  fn standard_format_stmts(
    stmts: &Vec<Statement>,
    idents: &Vec<String>,
    strings: &Vec<String>,
  ) -> String {
    stmts
      .iter()
      .map(|stmt| stmt.standard_format(idents, strings))
      .collect()
  }

  fn standard_format_args(args: &Vec<Expr>, idents: &Vec<String>, strings: &Vec<String>) -> String {
    let mut out = String::new();
    for i in 0..args.len() {
      if i != 0 {
        out.push_str(", ");
      }
      out.push_str(&args[i].standard_format(idents, strings));
    }
    out
  }

  impl StandardFmt for Expr {
    fn standard_format(&self, idents: &Vec<String>, strings: &Vec<String>) -> String {
      match self {
        BinaryOp(bin_op, expr1, expr2) => {
          std::format!(
            "({} {} {})",
            standard_format_binop(bin_op),
            expr1.standard_format(idents, strings),
            expr2.standard_format(idents, strings)
          )
        }
        UnaryOp(un_op, expr) => {
          std::format!(
            "({} {})",
            standard_format_unop(un_op),
            expr.standard_format(idents, strings)
          )
        }
        WrapOp(wrap_op, outer, args) => {
          std::format!(
            "{}{}{}{}",
            outer.standard_format(idents, strings),
            standard_format_wrapop(wrap_op).0,
            standard_format_args(args, idents, strings),
            standard_format_wrapop(wrap_op).1
          )
        }
        Atom(atomic) => match atomic {
          Var(i) => idents[*i].clone(),
          LiteralString(i) => std::format!("\"{}\"", strings[*i]),
          LiteralInt(n) => std::format!("{}", n),
          LiteralBool(b) => std::format!("{}", b),
          Func { args, body } => std::format!(
            "fn({}) {{\n{}}}",
            standard_format_args(args, idents, strings),
            standard_format_stmts(body, idents, strings)
          ),
        },
      }
    }
  }
  impl StandardFmt for Statement {
    fn standard_format(&self, idents: &Vec<String>, strings: &Vec<String>) -> String {
      use Statement::*;
      match self {
        Assign { left, right } => {
          std::format!(
            "let {} = {};\n",
            idents[*left],
            &right.standard_format(idents, strings)
          )
        }
        IfElse { pred, yes, no } => {
          let mut out = std::format!(
            "if ({}) {{\n{}}}\n",
            &pred.standard_format(idents, strings),
            standard_format_stmts(&yes, idents, strings)
          );
          if let Some(no) = no {
            out.push_str(&std::format!(
              "else {{\n{}}}\n",
              standard_format_stmts(&no, idents, strings)
            ));
          }
          out
        }
        Return(expr) => {
          std::format!("return {};\n", &expr.standard_format(idents, strings))
        }
        Block(stmts) => {
          let mut out = String::from("{\n");
          out.push_str(&standard_format_stmts(&stmts, idents, strings));
          out.push_str("}\n");
          out
        }
        Bare(expr) => {
          std::format!("{};\n", expr.standard_format(idents, strings))
        }
      }
    }
  }

  #[test]
  fn arith_stmt() {
    let res = lex(r#"1+2/3*4;"#.chars().collect());
    let res = parse(res.unwrap());
    let prog = res.unwrap();
    let prog_fmt = standard_format_stmts(&prog.statements, &prog.idents, &prog.strings);
    assert_eq!(prog_fmt, "(+ 1 (* (/ 2 3) 4));\n");
  }

  #[test]
  fn let_stmt() {
    let res = lex("let a = 1; let b = 2; let c = a+b;".chars().collect());
    let res = parse(res.unwrap());
    let prog = res.unwrap();
    let prog_fmt = standard_format_stmts(&prog.statements, &prog.idents, &prog.strings);
    assert_eq!(prog_fmt, "let a = 1;\nlet b = 2;\nlet c = (+ a b);\n");
  }

  #[test]
  fn fn_defn_call() {
    let res = lex(
      r#"
      let add = fn(a,b) {
        return a + b;
      };
      add(1,2);
      "#
      .chars()
      .collect(),
    );
    let res = parse(res.unwrap());
    let prog = res.unwrap();
    let prog_fmt = standard_format_stmts(&prog.statements, &prog.idents, &prog.strings);
    assert_eq!(
      prog_fmt,
      "let add = fn(a, b) {\nreturn (+ a b);\n};\nadd(1, 2);\n"
    );
  }

  #[test]
  fn if_else() {
    let res = lex(
      r#"
      if (a < b && c > d) {
        let x = -y;
      }
      if (a > b && c >= d) {
        let x = -x;
      }
      else {
        return false;
      }
      "#
      .chars()
      .collect(),
    );
    let res = parse(res.unwrap());
    let prog = res.unwrap();
    let prog_fmt = standard_format_stmts(&prog.statements, &prog.idents, &prog.strings);
    assert_eq!(
      prog_fmt,
      "if ((&& (< a b) (> c d))) {\nlet x = (- y);\n}\nif ((&& (> a b) (>= c d))) {\nlet x = (- x);\n}\nelse {\nreturn false;\n}\n");
  }
}
