use chumsky::prelude::*;
use logos::Logos;
use swls_core::prelude::{spanned, Spanned, StringStyle, Token};
use swls_token_helpers::*;

#[allow(non_camel_case_types)]
#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f\r]+")] // Ignore this regex pattern between tokens
enum TurtleToken {
    #[token("@prefix")]
    Prefix,

    #[token("prefix", ignore(case))]
    SqPrefix,

    #[token("@base")]
    Base,

    #[token("base", ignore(case))]
    SqBase,

    #[token("[")]
    SqOpen,

    #[token("]")]
    SqClose,

    #[token("(")]
    BraceOpen,

    #[token(")")]
    BraceClose,

    #[token("a")]
    TypeTag,

    #[token(";")]
    Semi,

    #[token(",")]
    Comma,
    #[token(".")]
    Stop,

    #[token("^^")]
    DataTag,

    #[token("true")]
    True,

    #[token("false")]
    False,

    #[regex(r#"(_:((([A-Z]|[a-z]|[\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD\U00010000-\U000EFFFF])|_)|[0-9])((([A-Z]|[a-z]|[\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD\U00010000-\U000EFFFF])|_)|\-|[0-9]|\u00B7|[\u0300-\u036F]|[\u203F-\u2040])*(\.*((([A-Z]|[a-z]|[\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD\U00010000-\U000EFFFF])|_)|\-|[0-9]|\u00B7|[\u0300-\u036F]|[\u203F-\u2040])((([A-Z]|[a-z]|[\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD\U00010000-\U000EFFFF])|_)|\-|[0-9]|\u00B7|[\u0300-\u036F]|[\u203F-\u2040])*)*)"#)]
    BLANK_NODE_LABEL,

    #[regex(r#"([+-]?(([0-9]+\.[0-9]*([eE][+-]?[0-9]+))|(\.([0-9])+([eE][+-]?[0-9]+))|(([0-9])+([eE][+-]?[0-9]+))))"#)]
    DOUBLE,

    #[regex(r#"([+-]?([0-9])*\.([0-9])+)"#)]
    DECIMAL,

    #[regex(r#"([+-]?[0-9]+)"#)]
    INTEGER,

    #[regex(r#"([+-]?[0-9]+\.)"#)]
    INTEGER_WITH_DOT,

    #[regex(r#"(@[a-zA-Z][a-zA-Z]*(\-[a-zA-Z0-9][a-zA-Z0-9]*)*)"#)]
    LANGTAG,

    #[regex(r#"("([^\x22\x5C\x0A\x0D]|(\\[tbnrf\"'\\])|((\\u([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f]))|(\\U([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f]))))*")"#)]
    STRING_LITERAL_QUOTE,

    #[regex(r#"('([^\x27\x5C\x0A\x0D]|(\\[tbnrf\"'\\])|((\\u([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f]))|(\\U([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f]))))*')"#)]
    STRING_LITERAL_SINGLE_QUOTE,

    #[regex(r#"('''(('|'')?([^'\\]|(\\[tbnrf\"'\\])|((\\u([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f]))|(\\U([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f])))))*''')"#)]
    STRING_LITERAL_LONG_SINGLE_QUOTE,

    #[regex(r#"("""(("|"")?([^"\\]|(\\[tbnrf\"'\\])|((\\u([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f]))|(\\U([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f])))))*""")"#)]
    STRING_LITERAL_LONG_QUOTE,

    #[regex(r#"(<([^\x00-\x20<>"{}|^`\\]|((\\u([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f]))|(\\U([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f]))))*>)"#)]
    IRIREF,

    #[regex(r#"((([A-Z]|[a-z]|[\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD\U00010000-\U000EFFFF])((((([A-Z]|[a-z]|[\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD\U00010000-\U000EFFFF])|_)|\-|[0-9]|\u00B7|[\u0300-\u036F]|[\u203F-\u2040])|\.)*((([A-Z]|[a-z]|[\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD\U00010000-\U000EFFFF])|_)|\-|[0-9]|\u00B7|[\u0300-\u036F]|[\u203F-\u2040]))?)?:)"#)]
    PNAME_NS,

    #[regex(r#"(((([A-Z]|[a-z]|[\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD\U00010000-\U000EFFFF])((((([A-Z]|[a-z]|[\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD\U00010000-\U000EFFFF])|_)|\-|[0-9]|\u00B7|[\u0300-\u036F]|[\u203F-\u2040])|\.)*((([A-Z]|[a-z]|[\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD\U00010000-\U000EFFFF])|_)|\-|[0-9]|\u00B7|[\u0300-\u036F]|[\u203F-\u2040]))?)?:)(((([A-Z]|[a-z]|[\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD\U00010000-\U000EFFFF])|_)|:|[0-9]|((%([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f]))|(\\(_|\~|\.|\-|!|\$|\&|\\"|\(|\)|\*|\+|"|'|;|=|,|/|\?|\#|@|%))))(\.|(((([A-Z]|[a-z]|[\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD\U00010000-\U000EFFFF])|_)|\-|[0-9]|\u00B7|[\u0300-\u036F]|[\u203F-\u2040])|:|((%([0-9]|[A-F]|[a-f])([0-9]|[A-F]|[a-f]))|(\\(_|\~|\.|\-|!|\$|\&|\\"|\(|\)|\*|\+|"|'|;|=|,|/|\?|\#|@|%)))))*))"#)]
    PNAME_LN,

    #[regex(r#"#[^\u000D\u000A]*"#)]
    Comment,
}

pub fn parse_tokens_str<'a>(text: &'a str) -> (Vec<Spanned<Token>>, Vec<Simple<char>>) {
    let mut tokens = Vec::new();
    let mut errors = Vec::new();
    let mut lex = TurtleToken::lexer(text);
    while let Some(x) = lex.next() {
        let t = || text[lex.span()].to_string();
        let t2 = |d_start, d_end| {
            let span = lex.span();
            let (start, end) = (span.start, span.end);
            text[start + d_start..end - d_end].to_string()
        };

        match x {
            Ok(token) => {
                let t = match token {
                    TurtleToken::Comment => Token::Comment(t()),
                    TurtleToken::Prefix => Token::PrefixTag,
                    TurtleToken::Base => Token::BaseTag,
                    TurtleToken::SqPrefix => Token::SparqlPrefix,
                    TurtleToken::SqBase => Token::SparqlBase,
                    TurtleToken::SqOpen => Token::SqOpen,
                    TurtleToken::SqClose => Token::SqClose,
                    TurtleToken::BraceOpen => Token::BracketOpen,
                    TurtleToken::BraceClose => Token::BracketClose,
                    TurtleToken::TypeTag => Token::PredType,
                    TurtleToken::Semi => Token::PredicateSplit,
                    TurtleToken::Comma => Token::Comma,
                    TurtleToken::Stop => Token::Stop,
                    TurtleToken::DataTag => Token::DataTypeDelim,
                    TurtleToken::True => Token::True,
                    TurtleToken::False => Token::False,
                    TurtleToken::BLANK_NODE_LABEL => Token::BlankNodeLabel(t2(2, 0)),
                    TurtleToken::DOUBLE => Token::Number(t()),
                    TurtleToken::DECIMAL => Token::Number(t()),
                    TurtleToken::INTEGER => Token::Number(t()),
                    TurtleToken::INTEGER_WITH_DOT => {
                        let span = lex.span();
                        let end = span.end - 1;
                        let start = span.start;
                        tokens.push(spanned(
                            Token::Number(text[start..end].to_string()),
                            start..end,
                        ));
                        tokens.push(spanned(Token::Stop, end..end + 1));

                        continue;
                    }
                    TurtleToken::LANGTAG => Token::LangTag(t2(1, 0)),
                    TurtleToken::STRING_LITERAL_LONG_SINGLE_QUOTE => {
                        Token::Str(t2(3, 3), StringStyle::SingleLong)
                    }
                    TurtleToken::STRING_LITERAL_QUOTE => Token::Str(t2(1, 1), StringStyle::Double),
                    TurtleToken::STRING_LITERAL_LONG_QUOTE => {
                        Token::Str(t2(3, 3), StringStyle::DoubleLong)
                    }
                    TurtleToken::STRING_LITERAL_SINGLE_QUOTE => {
                        Token::Str(t2(1, 1), StringStyle::Single)
                    }
                    TurtleToken::IRIREF => Token::IRIRef(t2(1, 1)),
                    TurtleToken::PNAME_LN | TurtleToken::PNAME_NS => {
                        let st = &text[lex.span()];
                        let ends_with_stop = st.ends_with('.');

                        if ends_with_stop {
                            let span = lex.span();
                            let end = span.end - 1;
                            let start = span.start;
                            if let Some((first, second)) = text[start..end].split_once(":") {
                                tokens.push(spanned(
                                    Token::PNameLN(Some(first.to_string()), second.to_string()),
                                    start..end,
                                ));
                                tokens.push(spanned(Token::Stop, end..end + 1));
                            } else {
                                tokens.push(spanned(
                                    Token::Invalid(text[start..end].to_string()),
                                    start..end,
                                ));
                                tokens.push(spanned(Token::Stop, end..end + 1));
                            }
                            continue;
                        } else {
                            if let Some((first, second)) = text[lex.span()].split_once(":") {
                                Token::PNameLN(Some(first.to_string()), second.to_string())
                            } else {
                                Token::Invalid(t())
                            }
                        }
                    }
                };
                tokens.push(spanned(t, lex.span()));
            }
            Err(_) => {
                tokens.push(spanned(Token::Invalid(t()), lex.span()));
                errors.push(Simple::custom(
                    lex.span(),
                    format!("Unexpected token '{}'", &text[lex.span()]),
                ))
            }
        }
    }

    (tokens, errors)
}
pub fn parse_tokens_str_safe(text: &str) -> Result<Vec<Spanned<Token>>, Vec<Simple<char>>> {
    let (t, e) = parse_tokens_str(text);
    if e.is_empty() {
        Ok(t)
    } else {
        Err(e)
    }
}

pub fn parse_token() -> t!(Token) {
    choice((
        keywords(),
        comment(),
        iri_ref(),
        pname_ns(),
        blank_node_label(),
        lang_tag(),
        integer(),
        strings(),
        tokens(),
    ))
    .recover_with(skip_parser(invalid()))
}

pub fn parse_tokens() -> t!(Vec<Spanned<Token>>) {
    parse_token()
        .map_with_span(spanned)
        .padded()
        .repeated()
        .then_ignore(end().recover_with(skip_then_retry_until([])))
}

#[cfg(test)]
mod tests {
    use std::ops::Range;

    use super::*;

    #[allow(unused)]
    fn log_parse_results(tokens: Vec<(Token, Range<usize>)>, err: &Vec<Simple<char>>) {
        tokens.iter().for_each(|tk| {
            println!("{:?}", tk);
        });

        err.iter().for_each(|er| eprintln!("{:?}", er));
    }

    #[test]
    fn parse_directives() {
        let input = "
            @prefix elm: <http://elm.com/types#> .
            @prefix : <http://elm.com/types#> .
            @base <http://example.com/#> . 
            # Test comment!
        ";

        assert!(parse_tokens().parse(input).is_ok());
    }

    #[test]
    fn parse_strings() {
        for input in ["\"\"\"test\"\"\"", "\"\"\"t\"est\"\"\""] {
            println!("Input {}", input);
            let (tok, err) = long_string_double().parse_recovery(input);
            println!("Found tokens {:?} {:?}", tok, err);
            assert!(tok.is_some());
            assert!(err.is_empty());
        }
    }

    #[test]
    fn parse_named_node() {
        let input = "
            <http://localhost/elmBeta> 
            elm:Beta

            :testing
            ";

        let (tok, err) = parse_tokens().parse_recovery(input);
        assert!(tok.is_some());
        assert!(err.is_empty());
    }

    #[test]
    fn simple_test() {
        let input = "
            @prefix elm: <http://elm.com/types#> .
            @base <http://example.com/#> . 
            
            elm:Beta foaf:string \"cookie\"@en ;
                     foaf:astring \"jar\"^^xsd:string .

            elm:Bnode a [ foaf:name \"Kachan\" ; 
                          foaf:lastName \"Bruh\" ; 
                          foaf:email \"kb@kbc.be\", \"notkb@notkbc.be\" ].
            ";

        let (tok, err) = parse_tokens().parse_recovery(input);
        assert!(tok.is_some());
        assert!(err.is_empty());
    }

    #[test]
    fn complex_test() {
        let input = "
            @prefix rr: <http://www.w3.org/ns/r2rml#> .
            @prefix foaf: <http://xmlns.com/foaf/0.1/> .
            @prefix ex: <http://example.com/> .
            @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
            @prefix rml: <http://semweb.mmlab.be/ns/rml#> .
            @prefix ql: <http://semweb.mmlab.be/ns/ql#> .

            @base <http://example.com/base/> .

            <TriplesMap1>
              a rr:TriplesMap;
                    
              rml:logicalSource [ 
                rml:source \"student.csv\";
                rml:referenceFormulation ql:CSV
              ] ;
                
              rr:subjectMap [ 
                rr:template \"http://example.com/{Name}\" 
              ]; 
                
              rr:predicateObjectMap [ 
                rr:predicate foaf:name ; 
                rr:objectMap [ 
                  rml:reference \"Name\" 
                ]
              ].
            ";

        let (tok, err) = parse_tokens().parse_recovery(input);
        assert!(tok.is_some());
        assert!(err.is_empty());
    }

    #[test]
    fn parse_invalid() {
        let input = "
            @prefix elm: http .
            ";

        let (tok, err) = parse_tokens().parse_recovery(input);
        assert!(tok.is_some());

        println!("tokens {:?}", tok);
        assert_eq!(tok.unwrap().len(), 4);
        assert_eq!(err.len(), 1);
    }
}
