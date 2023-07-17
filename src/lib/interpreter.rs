use std::rc::Rc;
use crate::parser;

#[derive(Debug)]
enum Value {
  Int(i64),
  Bool(bool),
  Str(String),
  List(Vec<Value>),
  Func {
    args: Vec<parser::Expr>,
    body: Vec<parser::Statement>,
  }
}

struct Binding {
  ident: usize,
  value: Value,
}

struct Scope {
  // TODO: need to do var name transform some time...
  bindings: Vec<Rc<Binding>>,
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
    self.scopes.push(Scope { bindings: vec![] });
  }
  fn pop_scope(&mut self) {
    self.scopes.pop();
  }
}

pub enum RuntimeError {
  TypeError(String)
}

fn eval_expression(expr: &parser::Expr, state: &mut State,
                   idents: &Vec<String>, strings: &Vec<String>)
  -> Result<Value, RuntimeError> {
  use parser::Expr::*;
  match expr {
    BinaryOp(bin_op, left, right) => {
      let expr_l = eval_expression(left, state, idents, strings)?;
      let expr_r = eval_expression(right, state, idents, strings)?;
      match bin_op {
        _ => { todo!(); }
      }
    }
    UnaryOp(un_op, value) => {
      todo!();
    }
    WrapOp(wrap_op, outer, inner) => {
      todo!();
    }
    Atom(atomic) => match atomic {
      _ => { todo!(); }
    }
  }
}

fn eval_statement(
  stmt: &parser::Statement, state: &mut State,
  idents: &Vec<String>, strings: &Vec<String>)
  -> Result<(), RuntimeError> {
  use parser::Statement::*;
  match stmt {
    Assign { left, right } => {
      let value = eval_expression(right, state, idents, strings)?;
      todo!();
    }
    IfElse { pred, yes, no } => {
      let pred_value = eval_expression(pred, state, idents, strings)?;
      match pred_value {
        Value::Bool(b) => {
          state.push_scope();
          if b {
            eval_statements(yes, state, idents, strings)?;
          }
          else {
            if let Some(no_stmts) = no {
              eval_statements(no_stmts, state, idents, strings)?;
            }
          }
          state.pop_scope();
        }
        _ => {
          return Err(RuntimeError::TypeError("if predicate expected bool".into()))
        }
      }
    }
    Return(expr) => {
      todo!();
    }
    Block(stmts) => {
      state.push_scope();
      eval_statements(stmts, state, idents, strings)?;
      state.pop_scope();
    }
    Bare(expr) => {
      todo!();
    }
  };
  Ok(())
}

fn eval_statements(
  stmts: &Vec<parser::Statement>, state: &mut State,
  idents: &Vec<String>, strings: &Vec<String>)
  -> Result<(), RuntimeError> {
  for stmt in stmts.iter() {
    eval_statement(stmt, state, idents, strings)?;
  }
  Ok(())
}

pub fn interpret(prog: parser::Program) -> Result<(), RuntimeError> {
  let mut state = State::new();
  eval_statements(
    &prog.statements, &mut state, &prog.idents, &prog.strings)?;
  Ok(())
}
