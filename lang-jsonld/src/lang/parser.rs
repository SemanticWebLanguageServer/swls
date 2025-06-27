use std::{
    io::{self, Write},
    ops::Range,
};

use chumsky::{prelude::*, Error, Parser, Stream};
use lsp_core::prelude::{spanned, MyTerm, Spanned, Token};
use Token::*;

struct ObjectMemberManager<'a> {
    out: Vec<Spanned<ObjectMember>>,
    full_start: usize,
    start: usize,

    seen_comma: bool,
    seen_colon: bool,

    current_key: Option<Spanned<Token>>,
    current_value: Option<Spanned<Json>>,
    emit: &'a mut dyn FnMut(Simple<Token>),
}

impl<'a> ObjectMemberManager<'a> {
    fn new(span: &Range<usize>, emit: &'a mut dyn FnMut(Simple<Token>)) -> Self {
        Self {
            out: vec![],
            full_start: span.start,
            start: span.start,
            seen_comma: false,
            seen_colon: false,
            current_key: None,
            current_value: None,
            emit,
        }
    }

    #[allow(unused)]
    fn print(&self) {
        println!(
            "key {:?} value {:?} (out {} len) (start {} full start {})",
            self.current_key.as_ref().map(|x| x.value()),
            self.current_value.as_ref().map(|x| x.value()),
            self.out.len(),
            self.start,
            self.full_start
        )
    }

    fn invalid(&mut self, span: Range<usize>) -> Spanned<Token> {
        (self.emit)(Simple::custom(span.clone(), "Expected valid token"));
        Spanned(Token::Invalid("".to_string()), span)
    }

    fn invalid_json(&mut self, span: Range<usize>) -> Spanned<Json> {
        (self.emit)(Simple::custom(span.clone(), "Expected valid json"));
        Spanned(Json::Invalid, span)
    }

    fn eat_json(&mut self, part: Spanned<Json>) {
        if self.current_key.is_none() {
            let span = part.span().clone();
            match part {
                Spanned(Json::Token(t), span) => {
                    self.current_key = Some(Spanned(t, span));
                }
                x => {
                    self.current_key = Some(self.invalid(self.start..span.start));
                    self.current_value = Some(x);
                }
            }

            self.full_start = span.start;
            self.start = span.end + 1;
            return;
        }

        if self.current_value.is_none() {
            if !self.seen_colon {
                (self.emit)(Simple::custom(
                    self.start - 1..self.start,
                    "expected colon, didn't find one",
                ));
            }

            self.start = part.span().end + 1;
            self.current_value = Some(part);
            return;
        }

        // We didn't expect to flush a thing, but we did
        self.flush(self.full_start..part.span().end, false);
        self.eat_json(part);
    }

    fn eat_token(&mut self, token: Spanned<Token>) {
        match token {
            Spanned(Token::Colon, span) => {
                (self.current_key, self.current_value) =
                    match (self.current_key.take(), self.current_value.take()) {
                        (Some(k), Some(Spanned(Json::Token(k2), r))) => {
                            self.current_key = Some(k);
                            // self.current_value = Some(self.invalid_json(r.clone()));
                            self.flush(span.clone(), false);
                            (Some(Spanned(k2, r)), None)
                        }
                        (k, v) => (k, v),
                    };
                if self.seen_colon {
                    (self.emit)(Simple::custom(
                        span.clone(),
                        "Unexepected colon, already seen one",
                    ));
                }
                self.seen_colon = true;
                // we expect to set the second part
                if self.current_key.is_none() {
                    self.current_key = Some(self.invalid(self.start..span.start));
                }
                self.start = span.end;
            }
            Spanned(Token::Comma, span) => {
                if self.seen_comma {
                    (self.emit)(Simple::custom(
                        span.clone(),
                        "Unexepected comma, already seen one",
                    ));
                }
                self.seen_comma = true;
                self.flush(span, false);
            }
            Spanned(x, s) => {
                (self.emit)(Simple::expected_input_found(
                    s,
                    [Some(Token::Colon), Some(Token::Comma)],
                    Some(x),
                ));
            }
        }
    }
    fn flush(&mut self, span: Range<usize>, end: bool) {
        if !end && !self.seen_comma {
            (self.emit)(Simple::custom(
                span.end - 1..span.end,
                "Expected comma, but didn't find one",
            ))
        }
        let k = match self.current_key.take() {
            Some(k) => k,
            None => self.invalid(span.clone()),
        };
        let v = match self.current_value.take() {
            Some(v) => v,
            None => self.invalid_json(span.clone()),
        };
        self.out
            .push(Spanned(ObjectMember::Full(k, v), self.full_start..span.end));
        self.start = span.end + 1;
        self.full_start = span.end + 1;
        self.seen_colon = false;
        self.seen_comma = false;
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum ObjectMember {
    Full(Spanned<Token>, Spanned<Json>),
    Partial(Spanned<Token>, Option<Spanned<()>>, Option<Spanned<Json>>),
}
impl ObjectMember {
    pub fn field(&self) -> &Spanned<Token> {
        match self {
            ObjectMember::Full(spanned, _) => spanned,
            ObjectMember::Partial(spanned, _, _) => spanned,
        }
    }

    pub fn json_value(&self) -> Option<&Spanned<Json>> {
        match self {
            ObjectMember::Full(_, spanned) => Some(spanned),
            ObjectMember::Partial(_, _, spanned) => spanned.as_ref(),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Json {
    Invalid,
    Token(Token),
    Array(Vec<Spanned<Json>>),
    Object(Vec<Spanned<ObjectMember>>),
}

impl Json {
    pub fn extract_triples(&self) -> Vec<MyTerm<'static>> {
        Vec::new()
    }
    pub fn token(&self) -> Option<&Token> {
        match self {
            Json::Token(t) => Some(t),
            _ => None,
        }
    }
}

pub struct JsonFormatter {
    pub indent: String,
    pub inc: usize,
}
impl JsonFormatter {
    pub fn inc(&mut self) {
        self.inc += 1;
    }

    pub fn decr(&mut self) {
        self.inc -= 1;
    }

    pub fn line(&mut self, writer: &mut impl Write) -> io::Result<()> {
        write!(writer, "\n")?;
        for _ in 0..self.inc {
            write!(writer, "{}", &self.indent)?;
        }
        Ok(())
    }

    pub fn format(&mut self, json: &Json, writer: &mut impl Write) -> io::Result<()> {
        use std::io::{Error, ErrorKind};
        match json {
            Json::Invalid => {
                return Result::Err(Error::new(ErrorKind::Other, "cannot format invalid json"))
            }
            Json::Token(t) => write!(writer, "{}", t)?,
            Json::Array(xs) => {
                write!(writer, "[")?;
                self.inc();
                self.line(writer)?;
                let mut first = true;
                for t in xs {
                    if !first {
                        write!(writer, ",")?;
                        self.line(writer)?;
                    }
                    self.format(&t.0, writer)?;
                    first = false;
                }
                self.decr();
                self.line(writer)?;
                write!(writer, "]")?;
            }
            Json::Object(xs) => {
                write!(writer, "{{")?;
                self.inc();
                self.line(writer)?;
                let mut first = true;
                for t in xs {
                    if !first {
                        write!(writer, ",")?;
                        self.line(writer)?;
                    }
                    match &t.0 {
                        ObjectMember::Full(x, y) => {
                            write!(writer, "{}: ", x.0)?;
                            self.format(y, writer)?;
                        }
                        ObjectMember::Partial(_, _, _) => {
                            return Result::Err(Error::new(
                                ErrorKind::Other,
                                "cannot format invalid json",
                            ))
                        }
                    }
                    first = false;
                }
                self.decr();
                self.line(writer)?;
                write!(writer, "}}")?;
            }
        }
        Ok(())
    }
}

impl Default for Json {
    fn default() -> Self {
        Self::Invalid
    }
}

pub fn parse(source: &str, tokens: Vec<Spanned<Token>>) -> (Spanned<Json>, Vec<Simple<Token>>) {
    let stream = Stream::from_iter(
        0..source.len() + 1,
        tokens.into_iter().map(|Spanned(x, s)| (x, s)),
    );

    let parser = parser().then_ignore(end().recover_with(skip_then_retry_until([])));
    let (json, json_errors) = parser.parse_recovery(stream);

    (
        json.unwrap_or(Spanned(Json::Invalid, 0..source.len())),
        json_errors,
    )
}

type S = std::ops::Range<usize>;
fn expect_token(
    token: Token,
    not_allowed: Token,
) -> impl Parser<Token, Token, Error = Simple<Token, S>> + Clone {
    just(token.clone()).or(none_of([token.clone(), not_allowed]).rewind().validate(
        move |x, span: S, emit| {
            emit(Simple::expected_input_found(
                span,
                [Some(token.clone())],
                Some(x),
            ));
            token.clone()
        },
    ))
}

fn parser() -> impl Parser<Token, Spanned<Json>, Error = Simple<Token>> {
    recursive(|value| {
        let array = value
            .clone()
            .separated_by(expect_token(Token::Comma, Token::SqClose))
            .delimited_by(just(SqOpen), just(SqClose))
            .map(Json::Array)
            .labelled("array");

        // let array = just(SqOpen).ignore_then(value.clone().separated_by(just(Comma))).then_ignore(just(SqClose)).map(Json::Array);

        let member_part = value
            .map(Result::Ok)
            .or(one_of([Token::Comma, Token::Colon])
                .map_with_span(spanned)
                .map(Result::Err));
        // let member_value = just(Token::Colon).ignore_then(value.clone());
        // let member = filter(Token::is_str)
        //     .map_with_span(spanned)
        //     .then(member_value.or())
        //     .validate(|(s, o), span, emit| match o {
        //         Some(o) => ObjectMember::Full(s, o),
        //         None => {
        //             emit(Simple::custom(span, "Erroneous object member"));
        //             ObjectMember::Partial(s, None, None)
        //         }
        //     })
        //     .labelled("object member");

        let obj = just(CurlOpen)
            .ignore_then(member_part.repeated().validate(|parts, span, emit| {
                let mut manager = ObjectMemberManager::new(&span, emit);

                for part in parts {
                    // manager.print();
                    match part {
                        Ok(e) => manager.eat_json(e),
                        Err(e) => manager.eat_token(e),
                    }
                }
                // manager.print();
                manager.flush(span, true);
                manager.out
            }))
            .then_ignore(just(CurlClose))
            .map(Json::Object)
            .labelled("object");

        // let obj = member
        //     .map_with_span(spanned)
        //     .separated_by(just(Comma).recover_with(skip_then_retry_until([])))
        //     .delimited_by(just(CuOpen), just(CuClose))
        //     .map(Json::Object);

        let leaves = chumsky::prelude::select! {
            Null => Json::Token(Null),
            True => Json::Token(True),
            False => Json::Token(False),
            Token::Str(x, st) => Json::Token(Token::Str(x, st)),
            Token::Number(n) => Json::Token(Token::Number(n)),
        }
        .labelled("leaf");

        choice((array, obj, leaves))
            // .map(std::result::Result::Ok)
            // .or(any().map(std::result::Result::Err))
            // .validate(|t, span, emit| match t {
            //     Ok(x) => x,
            //     Err(v) => {
            //         emit(Simple::custom(span, format!("Expected JSON found {:?}", v)));
            //         Json::Invalid
            //     }
            // })
            .map_with_span(spanned)
    })
}

#[cfg(test)]
mod tests {
    use lsp_core::prelude::StringStyle;

    use super::*;
    use crate::lang::tokenizer::tokenize;

    #[test]
    fn parse_json_simple() {
        let source = "\"test\"";
        let (tokens, token_errors) = tokenize(source);
        let (json, json_errors) = parse(source, tokens);

        assert!(token_errors.is_empty());
        assert!(json_errors.is_empty());

        assert_eq!(
            json.into_value(),
            Json::Token(Token::Str("test".into(), StringStyle::Double))
        );
    }

    #[test]
    fn parse_json_array() {
        let source = "[\"test\", 42]";
        let (tokens, token_errors) = tokenize(source);
        let (json, json_errors) = parse(source, tokens);

        assert!(token_errors.is_empty());
        assert!(json_errors.is_empty());

        let arr: Vec<_> = match json.into_value() {
            Json::Array(x) => x.into_iter().map(|x| x.into_value()).collect(),
            _ => panic!("Expected json array"),
        };

        assert_eq!(
            arr,
            vec![
                Json::Token(Token::Str("test".into(), StringStyle::Double)),
                Json::Token(Token::Number("42".into()))
            ]
        );
    }

    #[test]
    fn parse_json_object_no_comma() {
        let source = r#"{
  "@type": "foaf:Document"
  "foaf:topic": "foaf:Document"
}"#;

        let (tokens, token_errors) = tokenize(source);
        assert_eq!(token_errors, vec![]);

        let (json, json_errors) = parse(source, tokens);

        println!("json errors {:?}", json_errors);
        assert_eq!(json_errors.len(), 1, "One json error");

        let obj = match json.into_value() {
            Json::Object(xs) => xs,
            x => panic!("Expected json object, found {:?}", x),
        };
        assert_eq!(obj.len(), 2);
    }

    #[test]
    fn parse_json_object_no_value() {
        let source = r#"{
  "something":
  "foaf:topic": "foaf:Document"
}"#;

        let (tokens, token_errors) = tokenize(source);
        assert_eq!(token_errors, vec![]);

        let (json, json_errors) = parse(source, tokens);

        for e in &json_errors {
            println!("json errors {:?}", e);
        }

        let obj = match json.into_value() {
            Json::Object(xs) => xs,
            x => panic!("Expected json object, found {:?}", x),
        };
        assert_eq!(obj.len(), 2);

        assert_eq!(
            json_errors.len(),
            2,
            "Erroneous object member and expected comma"
        );
    }

    #[test]
    fn parse_json_object_no_colon_value() {
        let source = r#"{
  "something"
  "foaf:topic": "foaf:Document"
}"#;

        let (tokens, token_errors) = tokenize(source);
        assert_eq!(token_errors, vec![]);

        let (json, json_errors) = parse(source, tokens);

        for e in &json_errors {
            println!("json errors {:?}", e);
        }

        let obj = match json.into_value() {
            Json::Object(xs) => xs,
            x => panic!("Expected json object, found {:?}", x),
        };
        assert_eq!(obj.len(), 2);

        for e in &json_errors {
            println!("e {:?}", e);
        }

        assert_eq!(
            json_errors.len(),
            3,
            "Erroneous object member and expected comma"
        );
    }

    #[ignore]
    #[test]
    fn parse_json_array_invalid() {
        let source = "[\"test\" :  , 42 ]";
        let (tokens, token_errors) = tokenize(source);
        let (json, json_errors) = parse(source, tokens);

        assert!(token_errors.is_empty());
        // assert_eq!(json_errors.len(), 1);

        println!("Error: {:?}", json_errors);
        let arr: Vec<_> = match json.into_value() {
            Json::Array(x) => x.into_iter().map(|x| x.into_value()).collect(),
            x => panic!("Expected json array, got {:?}", x),
        };

        assert_eq!(
            arr,
            vec![
                Json::Token(Token::Str("test".into(), StringStyle::Double)),
                Json::Token(Token::Number("42".to_string())),
            ]
        );
    }

    #[test]
    fn parse_failed() {
        let source = r#"
{
  "@context": [
    "https://data.vlaanderen.be/doc/applicatieprofiel/sensoren-en-bemonstering/kandidaatstandaard/2022-04-28/context/ap-sensoren-en-bemonstering.jsonld",
    {
      "foaf": "foaf_exp"
    } 
  ], "test": "test_exp"
}
"#;

        let (tokens, token_errors) = tokenize(source);
        let (_, json_errors) = parse(source, tokens);

        assert!(token_errors.is_empty());
        assert_eq!(json_errors.len(), 0);
    }
}
