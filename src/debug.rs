use crate::interpreter::lexer::{Token, DebugToken};

pub fn format_token_vec(tokens: &Vec<DebugToken>) -> String {
    let mut rv = "[".to_string();
    for t in tokens {
        rv = format!("{}\n    {:?},", rv, t);
    }
    format!("{}\n]", rv)
}

pub fn format_token_vec_simplified(tokens: &Vec<DebugToken>) -> String {
    let mut rv = "[".to_string();
    for t in tokens {
        rv = format!(
            "{}\n    {},",
            rv,
            match &t.token {
                Token::ExplicitString(s) => format!("{:?}", s.string),
                Token::Word(w) => format!("{:?}", w.word),
                Token::NewLine(s) => format!("<newline indent {}>", s.indent),
                Token::ParenOpen(c)
                | Token::ParenClose(c)
                | Token::Colon(c)
                | Token::Equals(c)
                | Token::Comma(c)
                | Token::Plus(c)
                | Token::Minus(c)
                | Token::Star(c)
                | Token::Slash(c)
                | Token::Pipe(c)
                | Token::Hashtag(c) => format!("{:?}", c.character),
            }
        );
    }
    format!("{}\n]", rv)
}

pub fn char_repr(c: char) -> String {
    c.escape_debug().to_string().replace("\\\"", "\"")
}