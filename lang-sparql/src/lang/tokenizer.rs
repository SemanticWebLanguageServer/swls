use chumsky::prelude::*;
use logos::Logos;
use swls_core::prelude::{
    spanned, Spanned, SparqlAggregate, SparqlCall, SparqlExpr, SparqlKeyword, StringStyle, Token,
};

#[allow(non_camel_case_types)]
#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f\r]+")] // Ignore this regex pattern between tokens
enum SparqlToken {
    #[token("REGEX", |_| SparqlKeyword::Regex, ignore(case))]
    #[token("SUBSTR", |_| SparqlKeyword::Substr, ignore(case))]
    #[token("REPLACE", |_| SparqlKeyword::Replace, ignore(case))]
    #[token("EXISTS", |_| SparqlKeyword::Exists, ignore(case))]
    #[token("SELECT", |_| SparqlKeyword::Select, ignore(case))]
    #[token("DISTINCT", |_| SparqlKeyword::Distinct, ignore(case))]
    #[token("REDUCED", |_| SparqlKeyword::Reduced, ignore(case))]
    #[token("OPTIONAL", |_| SparqlKeyword::Optional, ignore(case))]
    #[token("UNION", |_| SparqlKeyword::Union, ignore(case))]
    #[token("AS", |_| SparqlKeyword::As, ignore(case))]
    #[token("CONSTRUCT", |_| SparqlKeyword::Construct, ignore(case))]
    #[token("WHERE", |_| SparqlKeyword::Where, ignore(case))]
    #[token("DESCRIBE", |_| SparqlKeyword::Describe, ignore(case))]
    #[token("ASK", |_| SparqlKeyword::Ask, ignore(case))]
    #[token("FROM", |_| SparqlKeyword::From, ignore(case))]
    #[token("NAMED", |_| SparqlKeyword::Named, ignore(case))]
    #[token("GROUP", |_| SparqlKeyword::Group, ignore(case))]
    #[token("BY", |_| SparqlKeyword::By, ignore(case))]
    #[token("HAVING", |_| SparqlKeyword::Having, ignore(case))]
    #[token("ORDER", |_| SparqlKeyword::Order, ignore(case))]
    #[token("ASC", |_| SparqlKeyword::Asc, ignore(case))]
    #[token("DESC", |_| SparqlKeyword::Desc, ignore(case))]
    #[token("LIMIT", |_| SparqlKeyword::Limit, ignore(case))]
    #[token("OFFSET", |_| SparqlKeyword::Offset, ignore(case))]
    #[token("VALUES", |_| SparqlKeyword::Values, ignore(case))]
    #[token("LOAD", |_| SparqlKeyword::Load, ignore(case))]
    #[token("SILENT", |_| SparqlKeyword::Silent, ignore(case))]
    #[token("CLEAR", |_| SparqlKeyword::Clear, ignore(case))]
    #[token("DROP", |_| SparqlKeyword::Drop, ignore(case))]
    #[token("CREATE", |_| SparqlKeyword::Create, ignore(case))]
    #[token("ADD", |_| SparqlKeyword::Add, ignore(case))]
    #[token("MOVE", |_| SparqlKeyword::Move, ignore(case))]
    #[token("COPY", |_| SparqlKeyword::Copy, ignore(case))]
    #[token("INSERT", |_| SparqlKeyword::Insert, ignore(case))]
    #[token("DATA", |_| SparqlKeyword::Data, ignore(case))]
    #[token("DELETE", |_| SparqlKeyword::Delete, ignore(case))]
    #[token("WITH", |_| SparqlKeyword::With, ignore(case))]
    #[token("USING", |_| SparqlKeyword::Using, ignore(case))]
    #[token("DEFAULT", |_| SparqlKeyword::Default, ignore(case))]
    #[token("ALL", |_| SparqlKeyword::All, ignore(case))]
    #[token("GRAPH", |_| SparqlKeyword::Graph, ignore(case))]
    #[token("SERVICE", |_| SparqlKeyword::Service, ignore(case))]
    #[token("BIND", |_| SparqlKeyword::Bind, ignore(case))]
    #[token("UNDEF", |_| SparqlKeyword::Undef, ignore(case))]
    #[token("MINUS", |_| SparqlKeyword::Minus, ignore(case))]
    #[token("FILTER", |_| SparqlKeyword::Filter, ignore(case))]
    Kwd(SparqlKeyword),

    #[token("COUNT", |_| SparqlAggregate::Count, ignore(case))]
    #[token("SUM", |_| SparqlAggregate::Sum, ignore(case))]
    #[token("MIN", |_| SparqlAggregate::Min, ignore(case))]
    #[token("MAX", |_| SparqlAggregate::Max, ignore(case))]
    #[token("AVG", |_| SparqlAggregate::Avg, ignore(case))]
    #[token("SAMPLE", |_| SparqlAggregate::Sample, ignore(case))]
    #[token("GROUP_CONCAT", |_| SparqlAggregate::GroupConcat, ignore(case))]
    Agg(SparqlAggregate),

    #[token("STR", |_| SparqlCall::Str, ignore(case))]
    #[token("LANG", |_| SparqlCall::Lang, ignore(case))]
    #[token("langMatches", |_| SparqlCall::LangMatches, ignore(case))]
    #[token("LANGDIR", |_| SparqlCall::LangDir, ignore(case))]
    #[token("datatype", |_| SparqlCall::Datatype, ignore(case))]
    #[token("BOUND", |_| SparqlCall::Bound, ignore(case))]
    #[token("IRI", |_| SparqlCall::Iri, ignore(case))]
    #[token("URI", |_| SparqlCall::Uri, ignore(case))]
    #[token("BNODE", |_| SparqlCall::Bnode, ignore(case))]
    #[token("RAND", |_| SparqlCall::Rand, ignore(case))]
    #[token("ABS", |_| SparqlCall::Abs, ignore(case))]
    #[token("CEIL", |_| SparqlCall::Ceil, ignore(case))]
    #[token("FLOOR", |_| SparqlCall::Floor, ignore(case))]
    #[token("ROUND", |_| SparqlCall::Round, ignore(case))]
    #[token("CONCAT", |_| SparqlCall::Concat, ignore(case))]
    #[token("STRLEN", |_| SparqlCall::StrLen, ignore(case))]
    #[token("UCASE", |_| SparqlCall::Ucase, ignore(case))]
    #[token("lcase", |_| SparqlCall::Lcase, ignore(case))]
    #[token("ENCODE_FOR_URI", |_| SparqlCall::EncodeForUri, ignore(case))]
    #[token("CONTAINS", |_| SparqlCall::Contains, ignore(case))]
    #[token("STRSTARTS", |_| SparqlCall::StrStarts, ignore(case))]
    #[token("STRENDS", |_| SparqlCall::StrEnds, ignore(case))]
    #[token("STRBEFORE", |_| SparqlCall::StrBefore, ignore(case))]
    #[token("STRAFTER", |_| SparqlCall::StrAfter, ignore(case))]
    #[token("YEAR", |_| SparqlCall::Year, ignore(case))]
    #[token("MONTH", |_| SparqlCall::Month, ignore(case))]
    #[token("DAY", |_| SparqlCall::Day, ignore(case))]
    #[token("HOURS", |_| SparqlCall::Hours, ignore(case))]
    #[token("MINUTES", |_| SparqlCall::Minutes, ignore(case))]
    #[token("SECONDS", |_| SparqlCall::Seconds, ignore(case))]
    #[token("TIMEZONE", |_| SparqlCall::Timezone, ignore(case))]
    #[token("TZ", |_| SparqlCall::Tz, ignore(case))]
    #[token("NOW", |_| SparqlCall::Now, ignore(case))]
    #[token("UUID", |_| SparqlCall::Uuid, ignore(case))]
    #[token("STRUUID", |_| SparqlCall::StrUuid, ignore(case))]
    #[token("MD5", |_| SparqlCall::Md5, ignore(case))]
    #[token("SHA1", |_| SparqlCall::Sha1, ignore(case))]
    #[token("SHA256", |_| SparqlCall::Sha256, ignore(case))]
    #[token("SHA384", |_| SparqlCall::Sha384, ignore(case))]
    #[token("SHA512", |_| SparqlCall::Sha512, ignore(case))]
    #[token("COALESCE", |_| SparqlCall::Coalesce, ignore(case))]
    #[token("IF", |_| SparqlCall::If, ignore(case))]
    #[token("STRLANG", |_| SparqlCall::StrLang, ignore(case))]
    #[token("STRLANGDIR", |_| SparqlCall::StrLangDir, ignore(case))]
    #[token("STRDT", |_| SparqlCall::StrDt, ignore(case))]
    #[token("sameTerm", |_| SparqlCall::SameTerm, ignore(case))]
    #[token("isIRI", |_| SparqlCall::IsIri, ignore(case))]
    #[token("isURI", |_| SparqlCall::IsUri, ignore(case))]
    #[token("isBLANK", |_| SparqlCall::IsBlank, ignore(case))]
    #[token("isLITERAL", |_| SparqlCall::IsLiteral, ignore(case))]
    #[token("isNUMBERIC", |_| SparqlCall::IsNumeric, ignore(case))]
    #[token("hasLANG", |_| SparqlCall::HasLang, ignore(case))]
    #[token("hasLANGDIR", |_| SparqlCall::HasLangDir, ignore(case))]
    #[token("isTRIPLE", |_| SparqlCall::IsTriple, ignore(case))]
    #[token("TRIPLE", |_| SparqlCall::Triple, ignore(case))]
    #[token("SUBJECT", |_| SparqlCall::Subject, ignore(case))]
    #[token("PREDICATE", |_| SparqlCall::Predicate, ignore(case))]
    #[token("OBJECT", |_| SparqlCall::Object, ignore(case))]
    Call(SparqlCall),

    #[token("in", |_| SparqlExpr::In, ignore(case))]
    #[token("not", |_| SparqlExpr::Not, ignore(case))]
    #[token("||", |_| SparqlExpr::Or, ignore(case))]
    #[token("&&", |_| SparqlExpr::And, ignore(case))]
    #[token("=", |_| SparqlExpr::Equal, ignore(case))]
    #[token("!=", |_| SparqlExpr::NotEqual, ignore(case))]
    #[token("<", |_| SparqlExpr::Lt, ignore(case))]
    #[token(">", |_| SparqlExpr::Gt, ignore(case))]
    #[token("<=", |_| SparqlExpr::Lte, ignore(case))]
    #[token(">=", |_| SparqlExpr::Gte, ignore(case))]
    #[token("+", |_| SparqlExpr::Plus, ignore(case))]
    #[token("-", |_| SparqlExpr::Minus, ignore(case))]
    #[token("*", |_| SparqlExpr::Times, ignore(case))]
    #[token("/", |_| SparqlExpr::Divide, ignore(case))]
    #[token("!", |_| SparqlExpr::Exclamation, ignore(case))]
    Expr(SparqlExpr),

    #[token("prefix", ignore(case))]
    SqPrefix,

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

    #[token("{")]
    CurlOpen,

    #[token("}")]
    CurlClose,

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

    #[regex(r#"((\?|\$)((([A-Z]|[a-z]|[\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD\U00010000-\U000EFFFF])|_)|[0-9])((([A-Z]|[a-z]|[\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD\U00010000-\U000EFFFF])|_)|[0-9]|\u00B7|[\u0300-\u036F]|[\u203F-\u2040])*)"#)]
    Variable,
}

pub fn parse_tokens_str<'a>(text: &'a str) -> (Vec<Spanned<Token>>, Vec<Simple<char>>) {
    let mut tokens = Vec::new();
    let mut errors = Vec::new();
    let mut lex = SparqlToken::lexer(text);
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
                    SparqlToken::Comment => Token::Comment(t()),
                    SparqlToken::SqPrefix => Token::SparqlPrefix,
                    SparqlToken::SqBase => Token::SparqlBase,
                    SparqlToken::SqOpen => Token::SqOpen,
                    SparqlToken::SqClose => Token::SqClose,
                    SparqlToken::BraceOpen => Token::BracketOpen,
                    SparqlToken::BraceClose => Token::BracketClose,
                    SparqlToken::TypeTag => Token::PredType,
                    SparqlToken::CurlOpen => Token::CurlOpen,
                    SparqlToken::CurlClose => Token::CurlClose,
                    SparqlToken::Semi => Token::PredicateSplit,
                    SparqlToken::Comma => Token::Comma,
                    SparqlToken::Stop => Token::Stop,
                    SparqlToken::DataTag => Token::DataTypeDelim,
                    SparqlToken::True => Token::True,
                    SparqlToken::False => Token::False,
                    SparqlToken::BLANK_NODE_LABEL => Token::BlankNodeLabel(t2(2, 0)),
                    SparqlToken::DOUBLE => Token::Number(t()),
                    SparqlToken::DECIMAL => Token::Number(t()),
                    SparqlToken::INTEGER => Token::Number(t()),
                    SparqlToken::INTEGER_WITH_DOT => {
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
                    SparqlToken::LANGTAG => Token::LangTag(t2(1, 0)),
                    SparqlToken::STRING_LITERAL_LONG_SINGLE_QUOTE => {
                        Token::Str(t2(3, 3), StringStyle::SingleLong)
                    }
                    SparqlToken::STRING_LITERAL_QUOTE => Token::Str(t2(1, 1), StringStyle::Double),
                    SparqlToken::STRING_LITERAL_LONG_QUOTE => {
                        Token::Str(t2(3, 3), StringStyle::DoubleLong)
                    }
                    SparqlToken::STRING_LITERAL_SINGLE_QUOTE => {
                        Token::Str(t2(1, 1), StringStyle::Single)
                    }
                    SparqlToken::IRIREF => Token::IRIRef(t2(1, 1)),
                    SparqlToken::PNAME_LN | SparqlToken::PNAME_NS => {
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
                    SparqlToken::Kwd(sparql_keyword) => Token::SparqlKeyword(sparql_keyword),
                    SparqlToken::Agg(sparql_aggregate) => Token::SparqlAggregate(sparql_aggregate),
                    SparqlToken::Call(sparql_call) => Token::SparqlCall(sparql_call),
                    SparqlToken::Expr(sparql_expr) => Token::SparqlExpr(sparql_expr),
                    SparqlToken::Variable => Token::Variable(t()),
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

#[cfg(test)]
mod tests {
    use super::parse_tokens_str;

    #[test]
    fn parse_random_tokens_1() {
        let inp = r#"
PREFIX ent:  <http://org.example.com/employees#>
DESCRIBE ?x WHERE { ?x ent:employeeId "1234" }
        "#;

        let (tok, er) = parse_tokens_str(inp);
        assert_eq!(tok.len(), 11);
        assert_eq!(er, vec![]);
    }

    #[test]
    fn parse_random_tokens_2() {
        let inp = r#"
PREFIX  dc:  <http://purl.org/dc/elements/1.1/>
SELECT  ?title
WHERE   { 
    ?x dc:title ?title
    FILTER regex(?title, "^SPARQL") 
}
        "#;

        let (tok, er) = parse_tokens_str(inp);
        assert_eq!(tok.len(), 18);
        assert_eq!(er, vec![]);
    }

    #[test]
    fn parse_random_tokens_3() {
        let inp = r#"
PREFIX  dc:  <http://purl.org/dc/elements/1.1/>
PREFIX  ns:  <http://example.org/ns#>

SELECT  ?title ?price
WHERE   {
    ?x ns:price ?price .
    FILTER (?price < 30.5)
    ?x dc:title ?title . 
}
        "#;

        let (tok, er) = parse_tokens_str(inp);
        assert_eq!(tok.len(), 26);
        assert_eq!(er, vec![]);
    }

    #[test]
    fn parse_random_tokens_4() {
        let inp = r#"
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
SELECT ?name ?mbox
WHERE  {
    ?x foaf:name  ?name .
    OPTIONAL { ?x  foaf:mbox  ?mbox }
}
        "#;

        let (tok, er) = parse_tokens_str(inp);
        assert_eq!(tok.len(), 19);
        assert_eq!(er, vec![]);
    }

    #[test]
    fn parse_random_tokens_5() {
        let inp = r#"
PREFIX foaf:    <http://xmlns.com/foaf/0.1/>
ASK  {
   ?x foaf:name  "Alice" ;
      foaf:mbox  <mailto:alice@work.example>
}
        "#;

        let (tok, er) = parse_tokens_str(inp);
        assert_eq!(tok.len(), 12);
        assert_eq!(er, vec![]);
    }

    #[test]
    fn parse_random_tokens_6() {
        let inp = r#"
PREFIX  dc: <http://purl.org/dc/elements/1.1/>
PREFIX app: <http://example.org/ns#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

CONSTRUCT { ?s ?p ?o } WHERE
{
    GRAPH ?g { ?s ?p ?o } .
    ?g dc:publisher <http://www.w3.org/> .
    ?g dc:date ?date .
    FILTER ( app:customDate(?date) > "2005-02-28T00:00:00Z"^^xsd:dateTime ) .
}
        "#;

        let (tok, er) = parse_tokens_str(inp);
        assert_eq!(tok.len(), 46);
        assert_eq!(er, vec![]);
    }

    #[test]
    fn parse_random_tokens_7() {
        let inp = r#"
PREFIX foaf:    <http://xmlns.com/foaf/0.1/>
PREFIX vcard:   <http://www.w3.org/2001/vcard-rdf/3.0#>

CONSTRUCT {
     ?x  vcard:N _:v .
    _:v vcard:givenName ?gname .
    _:v vcard:familyName ?fname
} WHERE {
    { ?x foaf:firstname ?gname } UNION  { ?x foaf:givenname   ?gname } .
    { ?x foaf:surname   ?fname } UNION  { ?x foaf:family_name ?fname } .
}
        "#;

        let (tok, er) = parse_tokens_str(inp);
        assert_eq!(tok.len(), 47);
        assert_eq!(er, vec![]);
    }

    #[test]
    fn parse_random_tokens_8() {
        let inp = r#"
PREFIX  dc:  <http://purl.org/dc/elements/1.1/>
PREFIX  ns:  <http://example.org/ns#>
SELECT  ?title (?p*(1-?discount) AS ?price)
{ ?x ns:price ?p .
  ?x dc:title ?title . 
  [] ns:discount ?discount 
}
        "#;

        let (tok, er) = parse_tokens_str(inp);
        for t in &tok {
            println!("t {:?}", t);
        }
        assert_eq!(tok.len(), 33);
        assert_eq!(er, vec![]);
    }
}
