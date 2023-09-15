use crate::interpreter::lexer::DebugToken;
use lazy_static::lazy_static;
use std::collections::HashSet;


pub fn format_token_vec_simplified(tokens: &Vec<DebugToken>) -> String {
    let mut rv = "[".to_string();
    for t in tokens {
        rv = format!("{}\n    {},", rv, t.token.simple_string());
    }
    format!("{}\n]", rv)
}

pub fn char_repr(c: char) -> String {
    c.escape_debug().to_string().replace("\\\"", "\"")
}

lazy_static! {
    static ref OPEN_PAREN: HashSet<char> = {
        let mut set = HashSet::new();
        set.insert('(');
        set.insert('{');
        set.insert('[');
        set
    };
}

lazy_static! {
    static ref CLOSE_PAREN: HashSet<char> = {
        let mut set = HashSet::new();
        set.insert(')');
        set.insert('}');
        set.insert(']');
        set
    };
}

#[allow(dead_code)]
pub fn format_parens(s: &str, max_substr_len: usize) -> String {
    let string_vec = format_parens_recurse(s, 0, max_substr_len).0;
    let mut rv = String::new();
    rv.push_str(&string_vec.join(", "));
    rv
}

fn format_parens_recurse(s: &str, indent: usize, max_substr_len: usize) -> (Vec<String>, usize) {
    let mut string_chars = s.chars().enumerate().peekable();

    let mut current_string = String::new();
    let mut final_vec = vec![];

    let mut pointer = 0;

    loop {
        let (i, this_char) = match string_chars.next() {
            Some(v) => v,
            None => break,
        };
        pointer = i;
        if OPEN_PAREN.contains(&this_char) {
            current_string.push(this_char);
            let (substring, chars_consumed) =
                format_parens_recurse(&s[i + 1..s.len()], indent + 1, max_substr_len);
            let closing_paren = string_chars.nth(chars_consumed).unwrap().1;
            let mut full_substring_len = (substring.len() - 1) * 2;
            for s in &substring {
                full_substring_len += s.len();
            }
            if full_substring_len > max_substr_len {
                current_string.push('\n');
                let mut indent_string = String::new();

                for _ in 0..((indent + 1) * 4) {
                    indent_string.push(' ');
                }
                current_string.push_str(&indent_string);
                let joined_str = &substring.join(&format!(",\n{}", indent_string));
                let stripped_str = match joined_str.strip_prefix(" ") {
                    Some(s) => s,
                    None => &joined_str,
                };
                current_string.push_str(stripped_str);
                current_string.push('\n');
                for _ in 0..((indent) * 4) {
                    current_string.push(' ');
                }
            } else {
                let mut joined_str = substring.join(", ");
                if let Some(stripped_str) = joined_str.strip_prefix(" ") {
                    joined_str = stripped_str.to_string();
                }
                current_string.push_str(&joined_str);
            }
            current_string.push(closing_paren)
        } else if CLOSE_PAREN.contains(&this_char) {
            final_vec.push(current_string);
            return (final_vec, i);
        } else if this_char == ',' {
            final_vec.push(current_string);
            while let Some((_, ' ')) = string_chars.peek() {
                string_chars.next();
            }
            current_string = String::new();
        } else {
            current_string.push(this_char);
        }
    }

    final_vec.push(current_string);
    (final_vec, pointer)
}
