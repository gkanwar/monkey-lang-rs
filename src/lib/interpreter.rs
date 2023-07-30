use crate::parser;
use std::{rc::Rc, collections::HashMap, collections::hash_map::Entry};


#[derive(Clone,Debug,PartialEq,Eq)]
pub enum Value {
  Int(i64),
  Bool(bool),
  Str(String),
  List(Vec<Value>),
  Func {
    args: Vec<parser::Expr>,
    body: Vec<parser::Statement>,
  },
}

// struct Binding {
//   ident: usize,
//   value: Value,
// }

struct Scope {
  // TODO: need to do var name transform some time...
  bindings: HashMap<usize, Rc<Value>>,
}
impl Scope {
  fn bind(&mut self, ident: usize, value: Value) {
    println!("Bind {} {:?}", ident, value);
    match self.bindings.entry(ident) {
      Entry::Occupied(mut entry) => {
        entry.insert(Rc::new(value));
      }
      Entry::Vacant(entry) => {
        entry.insert(Rc::new(value));
      }
    }
    println!("bindings {:?}", self.bindings);
  }
  // gives a clone of the contained value if it exists
  fn lookup(&self, ident: usize) -> Option<Value> {
    println!("Lookup bindings {:?} ident {}", self.bindings, ident);
    self.bindings.get(&ident).map(|v| (**v).clone())
  }
}

struct State {
  // from outermost to innermost
  scopes: Vec<Scope>,
}
impl State {
  fn new() -> Self {
    Self { scopes: vec![] }
  }
  fn push_scope(&mut self) {
    self.scopes.push(Scope { bindings: HashMap::new() });
  }
  fn pop_scope(&mut self) {
    self.scopes.pop();
  }
  fn lookup(&self, ident: usize) -> Option<Value> {
    for scope in self.scopes.iter().rev() {
      if let Some(v) = scope.lookup(ident) {
        return Some(v);
      }
    }
    None
  }
}

#[derive(Debug)]
pub enum RuntimeError {
  TypeError(String),
  NameError(String),
}

fn expect_bool_value(value: &Value) -> Result<bool, RuntimeError> {
  if let Value::Bool(b) = value {
    Ok(*b)
  }
  else {
    Err(RuntimeError::TypeError(std::format!(
          "expected bool, received {:?}", value)))
  }
}

// TODO: Other number types like float?
fn expect_number_value(value: &Value) -> Result<i64, RuntimeError> {
  if let Value::Int(i) = value {
    Ok(*i)
  }
  else {
    Err(RuntimeError::TypeError(std::format!(
          "expected int, received {:?}", value)))
  }
}

fn expect_string_value(value: &Value) -> Result<String, RuntimeError> {
  if let Value::Str(s) = value {
    Ok(s.clone()) // TODO
  }
  else {
    Err(RuntimeError::TypeError(std::format!(
          "expected string, received {:?}", value)))
  }
}

fn eval_expression(
  expr: &parser::Expr,
  state: &mut State,
  idents: &Vec<String>,
  strings: &Vec<String>,
) -> Result<Value, RuntimeError> {
  use parser::Expr;
  use parser::Atomic::*;
  use parser::BinaryOp::*;
  let value = match expr {
    Expr::BinaryOp(bin_op, left, right) => {
      let value_l = eval_expression(left, state, idents, strings)?;
      let value_r = eval_expression(right, state, idents, strings)?;
      match bin_op {
        LogicalAnd => {
          let bool_l = expect_bool_value(&value_l)?;
          let bool_r = expect_bool_value(&value_r)?;
          Value::Bool(bool_l && bool_r)
        }
        LogicalOr => {
          let bool_l = expect_bool_value(&value_l)?;
          let bool_r = expect_bool_value(&value_r)?;
          Value::Bool(bool_l || bool_r)
        }
        CmpEquals => {
          match value_l {
            Value::Int(i) => Value::Bool(i == expect_number_value(&value_r)?),
            Value::Bool(b) => Value::Bool(b == expect_bool_value(&value_r)?),
            Value::Str(s) => Value::Bool(s == expect_string_value(&value_r)?),
            _ => {
              return Err(RuntimeError::TypeError(
                std::format!("value {:?} does not support equality check", value_l)));
            }
          }
        }
        CmpNotEquals => {
          match value_l {
            Value::Int(i) => Value::Bool(i != expect_number_value(&value_r)?),
            Value::Bool(b) => Value::Bool(b != expect_bool_value(&value_r)?),
            Value::Str(s) => Value::Bool(s != expect_string_value(&value_r)?),
            _ => {
              return Err(RuntimeError::TypeError(
                std::format!("value {:?} does not support equality check", value_l)));
            }
          }
        }
        CmpLess => {
          let num_l = expect_number_value(&value_l)?;
          let num_r = expect_number_value(&value_r)?;
          Value::Bool(num_l < num_r)
        }
        CmpLessEquals => {
          let num_l = expect_number_value(&value_l)?;
          let num_r = expect_number_value(&value_r)?;
          Value::Bool(num_l <= num_r)
        }
        CmpGreater => {
          let num_l = expect_number_value(&value_l)?;
          let num_r = expect_number_value(&value_r)?;
          Value::Bool(num_l > num_r)
        }
        CmpGreaterEquals => {
          let num_l = expect_number_value(&value_l)?;
          let num_r = expect_number_value(&value_r)?;
          Value::Bool(num_l >= num_r)
        }
        Plus => {
          let num_l = expect_number_value(&value_l)?;
          let num_r = expect_number_value(&value_r)?;
          Value::Int(num_l + num_r)
        }
        Minus => {
          let num_l = expect_number_value(&value_l)?;
          let num_r = expect_number_value(&value_r)?;
          Value::Int(num_l - num_r)
        }
        Times => {
          let num_l = expect_number_value(&value_l)?;
          let num_r = expect_number_value(&value_r)?;
          Value::Int(num_l * num_r)
        }
        Divide => {
          let num_l = expect_number_value(&value_l)?;
          let num_r = expect_number_value(&value_r)?;
          Value::Int(num_l / num_r)
        }
      }
    }
    Expr::UnaryOp(un_op, value) => {
      todo!();
    }
    Expr::WrapOp(wrap_op, outer, inner) => {
      todo!();
    }
    Expr::Atom(atomic) => match atomic {
      LiteralInt(i) => Value::Int(*i),
      Var(ident) => match state.lookup(*ident) {
        Some(v) => v,
        None => {
          return Err(RuntimeError::NameError(idents[*ident].clone()));
        }
      }
      _ => {
        todo!();
      }
    },
  };
  Ok(value)
}

fn eval_statement(
  stmt: &parser::Statement,
  state: &mut State,
  idents: &Vec<String>,
  strings: &Vec<String>,
) -> Result<Option<Value>, RuntimeError> {
  use parser::Statement::*;
  let value: Option<Value> = match stmt {
    Assign { left, right } => {
      let value = eval_expression(right, state, idents, strings)?;
      let scope = state.scopes.last_mut().unwrap();
      scope.bind(*left, value.clone());
      Some(value)
    }
    IfElse { pred, yes, no } => {
      let pred_value = eval_expression(pred, state, idents, strings)?;
      match pred_value {
        Value::Bool(b) => {
          state.push_scope();
          let value = if b {
            eval_statements(yes, state, idents, strings)?
          } else {
            if let Some(no_stmts) = no {
              eval_statements(no_stmts, state, idents, strings)?
            }
            else {
              None
            }
          };
          state.pop_scope();
          value
        }
        _ => return Err(RuntimeError::TypeError("if predicate expected bool".into())),
      }
    }
    Return(expr) => {
      todo!();
    }
    Block(stmts) => {
      state.push_scope();
      let value = eval_statements(stmts, state, idents, strings)?;
      state.pop_scope();
      value
    }
    Bare(expr) => {
      Some(eval_expression(expr, state, idents, strings)?)
    }
  };
  Ok(value)
}

fn eval_statements(
  stmts: &Vec<parser::Statement>,
  state: &mut State,
  idents: &Vec<String>,
  strings: &Vec<String>,
) -> Result<Option<Value>, RuntimeError> {
  let mut value = None;
  for stmt in stmts.iter() {
    value = eval_statement(stmt, state, idents, strings)?;
  }
  Ok(value)
}

pub fn interpret(prog: parser::Program) -> Result<Option<Value>, RuntimeError> {
  let mut state = State::new();
  state.push_scope(); // global scope
  let out = eval_statements(
    &prog.statements, &mut state, &prog.idents, &prog.strings);
  state.pop_scope();
  out
}

mod tests {
  use super::*;
  use crate::lexer::lex;
  use crate::parser::parse;

  #[test]
  pub fn simple_add() {
    let prog = lex("1+2;".chars().collect()).unwrap();
    let prog = parse(prog).unwrap();
    let out: Option<Value> = interpret(prog).unwrap();
    assert!(out.is_some());
    let val = out.unwrap();
    assert_eq!(val, Value::Int(3));
  }
  #[test]
  pub fn simple_let() {
    let prog = lex("let a = 42; a;".chars().collect()).unwrap();
    let prog = parse(prog).unwrap();
    let out: Option<Value> = interpret(prog).unwrap();
    assert!(out.is_some());
    let val = out.unwrap();
    assert_eq!(val, Value::Int(42));
  }
}
