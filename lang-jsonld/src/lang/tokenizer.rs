use chumsky::{chain::Chain as _, prelude::*};
use swls_core::prelude::{spanned, Spanned, StringStyle, Token};

pub fn tokenize(st: &str) -> (Vec<Spanned<Token>>, Vec<Simple<char>>) {
    let parser = parser()
        .then_ignore(end().recover_with(skip_then_retry_until([])))
        .padded();

    let (json, errs) = parser.parse_recovery(st);

    (json.unwrap_or_default(), errs)
}

fn parser() -> impl Parser<char, Vec<Spanned<Token>>, Error = Simple<char>> {
    let tok = just("true")
        .to(Token::True)
        .or(just("false").to(Token::False))
        .or(just("null").to(Token::Null))
        .or(just(']').to(Token::SqClose))
        .or(just('{').to(Token::CurlOpen))
        .or(just('}').to(Token::CurlClose))
        .or(just(':').to(Token::Colon))
        .or(just(',').to(Token::Comma))
        .or(just('[').to(Token::SqOpen));

    let items = tok
        .or(parse_num())
        .or(parse_string().map(|st| Token::Str(st, StringStyle::Double)));

    items.map_with_span(spanned).padded().repeated()
}

fn exponent() -> impl Parser<char, Vec<char>, Error = Simple<char>> {
    one_of("eE")
        .then(one_of("+-").or_not())
        .then(filter(|c: &char| c.is_numeric()).repeated().at_least(1))
        .map(|((x, y), z)| {
            let mut o = Vec::with_capacity(1 + y.is_some() as usize + z.len());
            o.push(x);
            y.append_to(&mut o);
            z.append_to(&mut o);
            o
        })
}

fn parse_num() -> impl Parser<char, Token, Error = Simple<char>> {
    let before_dot = || {
        one_of("+-")
            .or_not()
            .then(filter(|c: &char| c.is_numeric()).repeated().at_least(1))
            .map(|(x, y)| {
                let mut o: Vec<char> = Vec::with_capacity(x.is_some() as usize + y.len());
                x.append_to(&mut o);
                y.append_to(&mut o);
                o
            })
    };

    let no_dot = || {
        filter(|c: &char| c.is_numeric())
            .repeated()
            .at_least(1)
            .then(exponent())
            .map(|(mut x, y)| {
                y.append_to(&mut x);
                x
            })
    };

    let with_dot = || {
        just('.').then(no_dot()).map(|(x, y)| {
            let mut o = Vec::with_capacity(1 + y.len());
            o.push(x);
            y.append_to(&mut o);
            o
        })
    };

    with_dot()
        .or(before_dot().then(with_dot()).map(|(mut x, y)| {
            y.append_to(&mut x);
            x
        }))
        .or(no_dot())
        .or(before_dot())
        .collect()
        .map(|x| Token::Number(x))
}

fn parse_string() -> impl Parser<char, String, Error = Simple<char>> {
    let escape = just('\\').ignore_then(
        just('\\')
            .or(just('/'))
            .or(just('"'))
            .or(just('b').to('\x08'))
            .or(just('f').to('\x0C'))
            .or(just('n').to('\n'))
            .or(just('r').to('\r'))
            .or(just('t').to('\t'))
            .or(just('u').ignore_then(
                filter(|c: &char| c.is_digit(16))
                    .repeated()
                    .exactly(4)
                    .collect::<String>()
                    .validate(|digits, span, emit| {
                        char::from_u32(u32::from_str_radix(&digits, 16).unwrap()).unwrap_or_else(
                            || {
                                emit(Simple::custom(span, "invalid unicode character"));
                                '\u{FFFD}' // unicode replacement character
                            },
                        )
                    }),
            )),
    );

    just('"')
        .ignore_then(filter(|c| *c != '\\' && *c != '"').or(escape).repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .labelled("string")
}

#[cfg(test)]
mod tests {
    use swls_core::prelude::Token::*;

    use super::*;

    #[test]
    fn parse_simple() {
        let (tokens, errs) = tokenize("");
        assert!(tokens.is_empty());
        assert!(errs.is_empty());

        let (tokens, errs) = tokenize(", [ ] { } null true false");
        let tokens: Vec<_> = tokens.into_iter().map(|x| x.into_value()).collect();
        assert_eq!(
            tokens,
            vec![Comma, SqOpen, SqClose, CurlOpen, CurlClose, Null, True, False]
        );
        assert!(errs.is_empty());
    }

    #[test]
    fn parse_string() {
        let (tokens, errs) = tokenize(" \"Epic string!!\"");
        let tokens: Vec<_> = tokens.into_iter().map(|x| x.into_value()).collect();
        assert_eq!(
            tokens,
            vec![Str("Epic string!!".into(), StringStyle::Double)]
        );
        assert!(errs.is_empty());

        let (tokens, errs) = tokenize(" \"Epic string!!");
        let tokens: Vec<_> = tokens.into_iter().map(|x| x.into_value()).collect();
        assert_eq!(tokens, vec![]);
        assert_eq!(errs.len(), 1);
    }

    #[test]
    fn parse_num() {
        let (tokens, errs) = tokenize(" 423");
        let tokens: Vec<_> = tokens.into_iter().map(|x| x.into_value()).collect();
        assert_eq!(tokens, vec![Number(String::from("423"))]);
        assert!(errs.is_empty());
    }
}
