use std::iter::Peekable;

use unicode_segmentation::{GraphemeIndices, UnicodeSegmentation};

use super::{
    lexer::Lexer,
    token::{Greek as GreekToken, Token, TokenDelim},
};

impl<'lexer> Lexer for Greek<'lexer> {
    fn origin(&self) -> String {
        self.origin_name.to_string()
    }

    fn len(&self) -> usize {
        self.input.len()
    }

    fn input(&self) -> String {
        self.input.to_string()
    }
}

#[derive(Debug)]
pub struct Greek<'lexer> {
    pub input: &'lexer str,
    origin_name: &'lexer str,
    inner: Peekable<GraphemeIndices<'lexer>>,
    offset: usize,
}

impl<'lexer> Greek<'lexer> {
    pub fn new(input: &'lexer str, origin_name: &'lexer str) -> Self {
        Self {
            input,
            origin_name,
            inner: input.grapheme_indices(true).peekable(),
            offset: 0,
        }
    }

    fn lex(&mut self) -> Option<<Self as Iterator>::Item> {
        match self.inner.next() {
            Some((i, c)) if c.is_delim() => {
                self.offset = i;
                self.lex_delim(c)
            }
            Some((i, _)) => {
                self.offset = i;
                self.lex_token()
            }
            None => Some(Token::new(
                self.offset..self.input.len() - 1,
                &self.input[self.offset..],
            )),
        }
    }

    fn lex_delim(&mut self, c: &str) -> Option<<Self as Iterator>::Item> {
        if c == "." {
            if let Some((i, ".")) = self.inner.peek() {
                let t = Some(Token::new(
                    self.offset..i + ".".len(),
                    &self.input[self.offset..i + ".".len()],
                ));
                self.inner.next();
                t
            } else {
                Some(Token::new(
                    self.offset..self.offset,
                    &self.input[self.offset..self.offset + c.len()],
                ))
            }
        } else {
            Some(Token::new(
                self.offset..self.offset,
                &self.input[self.offset..self.offset + c.len()],
            ))
        }
    }

    fn lex_token(&mut self) -> Option<<Self as Iterator>::Item> {
        while let Some((i, c)) = self.inner.peek() {
            if c.is_delim() {
                return Some(Token::new(
                    self.offset..(*i - 1),
                    &self.input[self.offset..*i],
                ));
            } else {
                self.inner.next();
            }
        }
        Some(Token::new(
            self.offset..self.input.len() - 1,
            &self.input[self.offset..],
        ))
    }
}

impl<'lexer> Iterator for Greek<'lexer> {
    type Item = Token<'lexer, GreekToken>;
    fn next(&mut self) -> Option<Self::Item> {
        match self.inner.peek() {
            Some((i, " ")) => {
                self.offset = *i + " ".len();
                self.inner.next();
                self.next()
            }
            Some((_, _)) => self.lex(),
            None => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Greek;

    #[test]
    fn lex_test() {
        let mut lexer = Greek::new("Hello + friend = maybe", "test");
        println!("{:#?}", lexer.next());
        println!("{:#?}", lexer.next());
        println!("{:#?}", lexer.next());
        println!("{:#?}", lexer.next());
        println!("{:#?}", lexer.next());
    }

    #[test]
    fn lex_doubledot() {
        let mut lexer = Greek::new("Hello + friend. - ma.. = maybe", "test");
        println!("{:#?}", lexer.next());
        println!("{:#?}", lexer.next());
        println!("{:#?}", lexer.next());
        println!("{:#?}", lexer.next());
        println!("{:#?}", lexer.next());
        println!("{:#?}", lexer.next());
        println!("{:#?}", lexer.next());
        println!("{:#?}", lexer.next());
    }

    #[test]
    fn lex_nowhitespace() {
        let mut lexer = Greek::new("Hello+friend=maybe", "test");
        println!("{:#?}", lexer.next());
        println!("{:#?}", lexer.next());
        println!("{:#?}", lexer.next());
        println!("{:#?}", lexer.next());
        println!("{:#?}", lexer.next());
    }

    #[test]
    fn lex_singletokens() {
        let mut lexer = Greek::new("+-*/&!=.", "test");
        println!("{:#?}", lexer.next());
        println!("{:#?}", lexer.next());
        println!("{:#?}", lexer.next());
        println!("{:#?}", lexer.next());
        println!("{:#?}", lexer.next());
    }

    #[test]
    fn lex_string() {
        let mut lexer = Greek::new("32 \"hello world\"", "test");
        println!("{:#?}", lexer.next());
        println!("{:#?}", lexer.next());
        println!("{:#?}", lexer.next());
        println!("{:#?}", lexer.next());
        println!("{:#?}", lexer.next());
    }
}
