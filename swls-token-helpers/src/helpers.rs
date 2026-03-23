use chumsky::{chain::Chain, prelude::*, Parser};
use swls_core::util::token::{StringStyle, Token};

#[macro_export]
macro_rules! t {
    ($t:ty) => {
        impl Parser<char, $t, Error = Simple<char>>
    };
}

pub fn tok(st: &'static str, tok: Token) -> t!(Token) {
    just::<char, &str, Simple<char>>(st).to(tok)
}

pub fn tokens_ext() -> t!(Token) {
    choice((tok("{", Token::CurlOpen), tok("}", Token::CurlClose)))
}

pub fn keywords() -> t!(Token) {
    choice((
        just("PREFIX").to(Token::PrefixTag),
        just("BASE").to(Token::BaseTag),
    ))
    .or(just('@').ignore_then(choice((
        just("prefix").to(Token::PrefixTag),
        just("base").to(Token::BaseTag),
    ))))
}

pub fn tokens() -> t!(Token) {
    choice((
        // tok("@prefix", Token::PrefixTag),
        // tok("@base", Token::BaseTag),
        tok("PREFIX", Token::SparqlPrefix),
        tok("BASE", Token::SparqlBase),
        tok("[", Token::SqOpen),
        tok("]", Token::SqClose),
        tok("(", Token::BracketOpen),
        tok(")", Token::BracketClose),
        tok("^^", Token::DataTypeDelim),
        tok(".", Token::Stop),
        tok(",", Token::Comma),
        tok(";", Token::PredicateSplit),
        tok("a", Token::PredType),
        tok("true", Token::True),
        tok("false", Token::False),
    ))
}

pub fn comment() -> t!(Token) {
    just('#')
        .ignore_then(none_of("\n\r").repeated().collect())
        .map(|x| Token::Comment(x))
}

pub fn invalid() -> t!(Token) {
    none_of(" \n\r.,;[]")
        .repeated()
        .at_least(1)
        .collect()
        .map(Token::Invalid)
}

pub fn iri_ref() -> t!(Token) {
    let letter = none_of("<>\"{}|^`\\").repeated().at_least(1).or(uchar());

    letter
        .repeated()
        .flatten()
        .collect()
        .delimited_by(just('<'), just('>'))
        .map(|x| Token::IRIRef(x))
}

pub fn pname_ns() -> t!(Token) {
    pn_prefix()
        .collect()
        .or_not()
        .then_ignore(just(':'))
        .then(pn_local().collect().or_not())
        .map(|(x, local)| {
            if let Some(local) = local {
                Token::PNameLN(x, local)
            } else {
                Token::PNameLN(x, String::new())
            }
        })
}

pub fn label_post() -> t!(Vec<char>) {
    just('.')
        .repeated()
        .chain(pn_chars().repeated().at_least(1))
}

pub fn blank_node_label() -> t!(Token) {
    let label = pn_chars()
        .or(filter(|c: &char| c.is_numeric()))
        .repeated()
        .then(label_post().repeated().flatten())
        .map(|(mut x, y)| {
            x.extend(y);
            x
        });

    just('_')
        .then(just(':'))
        .ignore_then(label.collect())
        .map(|x| Token::BlankNodeLabel(x))
}

pub fn lang_tag() -> t!(Token) {
    let rep = just('-').chain(filter(|c: &char| c.is_alphanumeric()).repeated());
    just('@')
        .ignore_then(filter(|c: &char| c.is_alphabetic()).repeated())
        .then(rep.repeated().flatten())
        .map(|(mut x, y)| {
            y.append_to(&mut x);
            x
        })
        .collect()
        .map(|string| Token::LangTag(string))
}

pub fn integer() -> t!(Token) {
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
            .then(exponent().or_not())
            .map(|(mut x, y)| {
                if let Some(exp) = y {
                    exp.append_to(&mut x);
                }
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

pub fn exponent() -> t!(Vec<char>) {
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

pub fn parse_string<const C: char>() -> t!(String) {
    let letter = e_char().or(uchar()).or(filter(|c: &char| {
        *c != '\\' && *c != '\n' && *c != '\r' && *c != C
    })
    .repeated()
    .at_least(1));

    letter
        .repeated()
        .flatten()
        .collect()
        .delimited_by(just(C), just(C))
}

pub fn parse_long_string<const C: char>() -> t!(String) {
    let si = || just::<char, char, Simple<char>>(C);
    let delim = si().repeated().exactly(3);

    let letter = delim.not();

    delim
        .ignore_then(letter.repeated().collect().map(|x| {
            // println!("Found {:?}", x);
            x
        }))
        .then_ignore(delim)
    // letter
    //     .repeated()
    //     .flatten()
    //     .collect()
    //     .delimited_by(delim, delim)
    // delim
    //     .ignore_then(
    //         si().repeated()
    //             .at_most(2)
    //             .then(letter.repeated().flatten())
    //             .map(|(mut x, y)| {
    //                 y.append_to(&mut x);
    //                 x
    //             }),
    //     )
    //     .then_ignore(delim)
    //     .collect()
}

pub fn strings() -> t!(Token) {
    long_string_double()
        .or(long_string_single())
        .or(string_single())
        .or(string_double())
}

pub fn string_single() -> t!(Token) {
    parse_string::<'\''>().map(|x| Token::Str(x, StringStyle::Single))
}
pub fn string_double() -> t!(Token) {
    parse_string::<'"'>().map(|x| Token::Str(x, StringStyle::Double))
}

pub fn long_string_single() -> t!(Token) {
    parse_long_string::<'\''>().map(|x| Token::Str(x, StringStyle::SingleLong))
}

pub fn long_string_double() -> t!(Token) {
    parse_long_string::<'"'>().map(|x| Token::Str(x, StringStyle::DoubleLong))
}

pub fn uchar() -> t!(Vec<char>) {
    let small = just('\\')
        .chain(just('u'))
        .chain(hex())
        .chain(hex())
        .chain(hex())
        .chain(hex());

    let big = just('\\')
        .chain(just('U'))
        .chain(hex())
        .chain(hex())
        .chain(hex())
        .chain(hex())
        .chain(hex())
        .chain(hex())
        .chain(hex())
        .chain(hex());

    small.or(big)
}

pub fn e_char() -> t!(Vec<char>) {
    just('\\')
        .then(one_of("tbnrf\"'\\"))
        .map(|(x, y)| vec![x, y])
}

pub fn pn_chars_base() -> t!(char) {
    filter(|c: &char| c.is_alphabetic())
}

pub fn pn_chars_u() -> t!(char) {
    pn_chars_base().or(just('_'))
}
pub fn varname() -> t!(char) {
    pn_chars_u().or(filter(|c: &char| c.is_numeric()))
}
pub fn pn_chars() -> t!(char) {
    pn_chars_u()
        .or(just('-'))
        .or(filter(|c: &char| c.is_numeric()))
}
pub fn pn_prefix() -> t!(Vec<char>) {
    let ne = just('.')
        .repeated()
        .then(pn_chars().repeated().at_least(1))
        .map(|(x, y)| {
            let mut o: Vec<char> = Vec::with_capacity(x.len() + y.len());
            x.append_to(&mut o);
            y.append_to(&mut o);
            o
        })
        .repeated()
        .flatten();

    pn_chars_base().then(ne.or_not()).map(|(x, y)| {
        if let Some(y) = y {
            let mut o = Vec::with_capacity(y.len() + 1);
            o.push(x);
            o.extend(y);
            o
        } else {
            vec![x]
        }
    })
}

pub fn pn_local() -> t!(Vec<char>) {
    let first_char = pn_chars_u()
        .or(filter(|c: &char| *c == ':' || c.is_numeric()))
        .repeated()
        .at_least(1)
        .or(plx());

    let other = || pn_chars().or(just(':')).or(just('%'));

    let rest = just('.')
        .repeated()
        .then(other().repeated().at_least(1))
        .map(|(x, y)| {
            let mut o: Vec<char> = Vec::with_capacity(x.len() + y.len());
            x.append_to(&mut o);
            y.append_to(&mut o);
            o
        })
        .repeated()
        .flatten();

    first_char.then(rest.or_not()).map(|(mut x, y)| {
        if let Some(y) = y {
            y.append_to(&mut x);
        }
        x
    })
}

pub fn plx() -> t!(Vec<char>) {
    percent().or(pn_local_esc())
}

pub fn percent() -> t!(Vec<char>) {
    just('%')
        .ignore_then(hex().then(hex()))
        .map(|(x, y)| vec![x, y])
}

pub fn hex() -> t!(char) {
    filter(|c: &char| c.is_ascii_hexdigit())
}

pub fn pn_local_esc() -> t!(Vec<char>) {
    just('\\')
        .then(one_of("_~.-!$&'()*+,;=/?#@%"))
        .map(|(x, y)| vec![x, y])
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn parse_keywords() {
        assert!(keywords().parse("@prefix").is_ok());
        assert!(tokens().parse(".").is_ok());
        assert!(iri_ref().parse("<testing>").is_ok());
        assert!(pname_ns().parse(":").is_ok());
        assert!(pname_ns().parse("testing:").is_ok());
        assert!(pname_ns().parse("testing:test").is_ok());
        assert!(blank_node_label().parse("_:test").is_ok());
        assert!(lang_tag().parse("@en").is_ok());
        assert!(integer().parse("14").is_ok());
        assert!(integer().parse("14.0").is_ok());
        assert!(strings().parse("'testing'").is_ok());
        assert!(strings().parse("\"testing\"").is_ok());
        assert!(strings().parse("\"\"\"testing\"\"\"").is_ok());
        assert!(comment().parse("# This is a nice comment").is_ok());
    }

    #[test]
    fn parse_multiple_kws() {
        assert!(tokens()
            .padded()
            .repeated()
            .parse("@prefix @base . .")
            .is_ok());
        assert!(iri_ref()
            .padded()
            .repeated()
            .parse("<testing> <testing>")
            .is_ok());
        assert!(pname_ns()
            .padded()
            .repeated()
            .parse(": testing: testing:test")
            .is_ok());
        assert!(blank_node_label()
            .padded()
            .repeated()
            .parse("_:b1 _:b0")
            .is_ok());
        assert!(lang_tag().padded().repeated().parse("@en @en-nl").is_ok());
        assert!(integer().padded().repeated().parse("14 14").is_ok());
        assert!(strings()
            .padded()
            .repeated()
            .parse("\"testing\" 'testing'")
            .is_ok());
    }
}
