use std::iter::Peekable;

use unicode_segmentation::{GraphemeIndices, UnicodeSegmentation};

use super::token::{Token, TokenDelim};

#[derive(Clone)]
pub struct Lexer<'lexer> {
    pub name: &'lexer str,
    pub src: &'lexer str,
    pub offset: usize,
    inner: Peekable<GraphemeIndices<'lexer>>,
}

impl<'lexer> Lexer<'lexer> {
    pub fn new(name: &'lexer str, src: &'lexer str) -> Self {
        Self {
            name,
            src,
            offset: 0,
            inner: src.grapheme_indices(true).peekable(),
        }
    }

    pub fn peek(&self) -> Option<<Self as Iterator>::Item> {
        let mut cloned = self.clone();
        cloned.next()
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
                self.offset..self.src.len() - 1,
                &self.src[self.offset..],
            )),
        }
    }

    fn lex_delim(&mut self, c: &str) -> Option<<Self as Iterator>::Item> {
        if c == "." {
            if let Some((i, ".")) = self.inner.peek() {
                let t = Some(Token::new(
                    self.offset..i + ".".len(),
                    &self.src[self.offset..i + ".".len()],
                ));
                self.inner.next();
                t
            } else {
                Some(Token::new(
                    self.offset..self.offset,
                    &self.src[self.offset..self.offset + c.len()],
                ))
            }
        } else {
            Some(Token::new(
                self.offset..self.offset,
                &self.src[self.offset..self.offset + c.len()],
            ))
        }
    }

    fn lex_token(&mut self) -> Option<<Self as Iterator>::Item> {
        while let Some((i, c)) = self.inner.peek() {
            if c.is_delim() {
                return Some(Token::new(
                    self.offset..(*i - 1),
                    &self.src[self.offset..*i],
                ));
            } else {
                self.inner.next();
            }
        }
        Some(Token::new(
            self.offset..self.src.len() - 1,
            &self.src[self.offset..],
        ))
    }
}

impl<'lexer> Iterator for Lexer<'lexer> {
    type Item = Token<'lexer>;
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
mod test {
    use super::Lexer;

    #[test]
    fn lex() {
        let lexer = Lexer::new("main", "ΣΥΝΑΡΤΗΣΗ ΡΙΖΑ(): 2 + 2.");
        for token in lexer {
            println!("{:#?}", token)
        }
    }

    #[test]
    fn peek_test() {
        let mut lexer = Lexer::new("main", "ΣΥΝΑΡΤΗΣΗ ΡΙΖΑ(): 2 + 2.");
        println!("{:#?}", lexer.peek());
        println!("{:#?}", lexer.next());
        println!("{:#?}", lexer.peek());
        println!("{:#?}", lexer.next());
    }
}
