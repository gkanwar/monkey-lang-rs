// Index into identifier table
pub type IdentIdx = usize;
// Index into strings table
pub type StringIdx = usize;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Token {
  Let,
  Fn,
  Return,
  Identifier(IdentIdx),
  Equals,
  CmpEquals,
  CmpNotEquals,
  CmpLess,
  CmpLessEquals,
  CmpGreater,
  CmpGreaterEquals,
  // TODO: And, Or
  If,
  Else,
  Comma,
  Colon,
  Plus,
  Minus,
  Times,
  Divide,
  Not,
  Semicolon,
  LiteralString(StringIdx),
  LiteralInt(i64),
  LiteralBool(bool),
  ParenOpen,
  ParenClose,
  SquareOpen,
  SquareClose,
  CurlyOpen,
  CurlyClose,
}

pub struct Program {
  pub tokens: Vec<Token>,
  pub idents: Vec<String>,
  pub strings: Vec<String>,
}

#[derive(Debug)]
pub enum LexerError {
  Generic(String),
}

fn consume_whitespace(chars: &Vec<char>, i: &mut usize) -> Result<(), LexerError> {
  while *i < chars.len() && chars[*i].is_ascii_whitespace() {
    *i += 1;
  }
  Ok(())
}

fn consume_int_literal(chars: &Vec<char>, i: &mut usize) -> Result<Token, LexerError> {
  let mut s = String::new();
  while *i < chars.len() && chars[*i].is_digit(10) {
    s.push(chars[*i]);
    *i += 1;
  }
  let res = s.parse::<i64>();
  match res {
    Ok(x) => Ok(Token::LiteralInt(x)),
    Err(_) => Err(LexerError::Generic(std::format!(
      "invalid int literal {}",
      s
    ))),
  }
}

fn consume_line_comment(chars: &Vec<char>, i: &mut usize) {
  *i += 2;
  let _start = *i; // in case we want to save the comments
  while *i < chars.len() && chars[*i] != '\n' {
    *i += 1;
  }
}

fn consume_identifier_or_keyword(
  chars: &Vec<char>,
  i: &mut usize,
  idents: &mut Vec<String>,
) -> Result<Token, LexerError> {
  let mut s = String::new();
  s.push(chars[*i]);
  *i += 1;
  while *i < chars.len() && chars[*i].is_ascii_alphanumeric() {
    s.push(chars[*i]);
    *i += 1;
  }
  match s.as_str() {
    "let" => Ok(Token::Let),
    "fn" => Ok(Token::Fn),
    "return" => Ok(Token::Return),
    "if" => Ok(Token::If),
    "else" => Ok(Token::Else),
    "true" => Ok(Token::LiteralBool(true)),
    "false" => Ok(Token::LiteralBool(false)),
    _ => match idents.iter().position(|x| *x == s) {
      Some(idx) => Ok(Token::Identifier(idx)),
      None => {
        idents.push(s);
        Ok(Token::Identifier(idents.len() - 1))
      }
    },
  }
}

fn consume_string_literal(
  chars: &Vec<char>,
  i: &mut usize,
  strings: &mut Vec<String>,
) -> Result<Token, LexerError> {
  let quote = chars[*i];
  *i += 1;
  let mut s = String::new();
  while *i < chars.len() && chars[*i] != quote {
    s.push(chars[*i]);
    *i += 1;
  }
  if *i == chars.len() {
    Err(LexerError::Generic("unterminated string".into()))
  } else {
    *i += 1;
    match strings.iter().position(|x| *x == s) {
      Some(idx) => Ok(Token::LiteralString(idx)),
      None => {
        strings.push(s);
        Ok(Token::LiteralString(strings.len() - 1))
      }
    }
  }
}

fn consume_next_token(
  chars: &Vec<char>,
  i: &mut usize,
  idents: &mut Vec<String>,
  strings: &mut Vec<String>,
) -> Result<Option<Token>, LexerError> {
  let peek = chars[*i];
  Ok(Some(match peek {
    '0'..='9' => consume_int_literal(chars, i)?,
    c @ (';' | ':' | '+' | '-' | '*' | ',' | '(' | ')' | '[' | ']' | '{' | '}') => {
      *i += 1;
      match c {
        ';' => Token::Semicolon,
        ':' => Token::Colon,
        '+' => Token::Plus,
        '-' => Token::Minus,
        '*' => Token::Times,
        ',' => Token::Comma,
        '(' => Token::ParenOpen,
        ')' => Token::ParenClose,
        '[' => Token::SquareOpen,
        ']' => Token::SquareClose,
        '{' => Token::CurlyOpen,
        '}' => Token::CurlyClose,
        _ => unimplemented!(),
      }
    }
    '/' => {
      if *i + 1 < chars.len() && chars[*i + 1] == '/' {
        consume_line_comment(chars, i);
        return Ok(None);
      } else {
        *i += 1;
        Token::Divide
      }
    }
    '=' => {
      if *i + 1 < chars.len() && chars[*i + 1] == '=' {
        *i += 2;
        Token::CmpEquals
      } else {
        *i += 1;
        Token::Equals
      }
    }
    '!' => {
      if *i + 1 < chars.len() && chars[*i + 1] == '=' {
        *i += 2;
        Token::CmpNotEquals
      } else {
        *i += 1;
        Token::Not
      }
    }
    '<' => {
      if *i + 1 < chars.len() && chars[*i + 1] == '=' {
        *i += 2;
        Token::CmpLessEquals
      } else {
        *i += 1;
        Token::CmpLess
      }
    }
    '>' => {
      if *i + 1 < chars.len() && chars[*i + 1] == '=' {
        *i += 2;
        Token::CmpGreaterEquals
      } else {
        *i += 1;
        Token::CmpGreater
      }
    }
    '"' | '\'' => consume_string_literal(chars, i, strings)?,
    c if c.is_ascii_alphabetic() => consume_identifier_or_keyword(chars, i, idents)?,
    c if !c.is_ascii() => {
      return Err(LexerError::Generic(std::format!(
        "non-ascii char {} can only be used inside string literals",
        peek
      )));
    }
    _ => {
      return Err(LexerError::Generic(std::format!(
        "invalid character {}",
        peek
      )));
    }
  }))
}

pub fn lex(chars: Vec<char>) -> Result<Program, LexerError> {
  let mut tokens: Vec<Token> = vec![];
  let mut i: usize = 0;
  let mut idents: Vec<String> = vec![];
  let mut strings: Vec<String> = vec![];
  while i < chars.len() {
    consume_whitespace(&chars, &mut i)?;
    if i == chars.len() {
      break;
    }
    let res = consume_next_token(&chars, &mut i, &mut idents, &mut strings)?;
    match res {
      Some(tok) => {
        tokens.push(tok);
      }
      None => {}
    }
  }
  Ok(Program {
    tokens,
    idents,
    strings,
  })
}

#[cfg(test)]
mod tests {
  use super::*;
  use Token::*;

  #[test]
  fn basic_arith() {
    let res = lex(r#"1+2/3*4"#.chars().collect());
    assert!(res.is_ok());
    let prog = res.unwrap();
    assert_eq!(
      prog.tokens,
      vec![
        LiteralInt(1),
        Plus,
        LiteralInt(2),
        Divide,
        LiteralInt(3),
        Times,
        LiteralInt(4)
      ]
    );
    assert_eq!(prog.idents.len(), 0);
    assert_eq!(prog.strings.len(), 0);
  }

  #[test]
  fn let_assign() {
    let res = lex(r#"let a = 1 + 2;"#.chars().collect());
    assert!(res.is_ok());
    let prog = res.unwrap();
    assert_eq!(
      prog.tokens,
      vec![
        Let,
        Identifier(0),
        Equals,
        LiteralInt(1),
        Plus,
        LiteralInt(2),
        Semicolon
      ]
    );
    assert_eq!(prog.idents, vec!["a"]);
    assert_eq!(prog.strings.len(), 0);
  }

  #[test]
  fn big_prog() {
    let res = lex(
      r#"
      let a = 2*3/4+-6;a+1;
      fn asdf(10) {}[]() return;
      "hello world fn return let";
      // some comment if else
      if else != == !a !1 1a a1;
      , < <= < = > >= > =;
      : true false;
    "#
      .chars()
      .collect(),
    );
    assert!(res.is_ok());
    let prog = res.unwrap();
    assert_eq!(
      prog.tokens,
      vec![
        Let,
        Identifier(0),
        Equals,
        LiteralInt(2),
        Times,
        LiteralInt(3),
        Divide,
        LiteralInt(4),
        Plus,
        Minus,
        LiteralInt(6),
        Semicolon,
        Identifier(0),
        Plus,
        LiteralInt(1),
        Semicolon,
        Fn,
        Identifier(1),
        ParenOpen,
        LiteralInt(10),
        ParenClose,
        CurlyOpen,
        CurlyClose,
        SquareOpen,
        SquareClose,
        ParenOpen,
        ParenClose,
        Return,
        Semicolon,
        LiteralString(0),
        Semicolon,
        If,
        Else,
        CmpNotEquals,
        CmpEquals,
        Not,
        Identifier(0),
        Not,
        LiteralInt(1),
        LiteralInt(1),
        Identifier(0),
        Identifier(2),
        Semicolon,
        Comma,
        CmpLess,
        CmpLessEquals,
        CmpLess,
        Equals,
        CmpGreater,
        CmpGreaterEquals,
        CmpGreater,
        Equals,
        Semicolon,
        Colon,
        LiteralBool(true),
        LiteralBool(false),
        Semicolon
      ]
    );
    assert_eq!(prog.idents, vec!["a", "asdf", "a1"]);
    assert_eq!(prog.strings, vec!["hello world fn return let"]);
  }
}
