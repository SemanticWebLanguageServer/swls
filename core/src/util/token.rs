use std::ops::Range;

use bevy_ecs::prelude::*;
use derive_more::{AsMut, AsRef, Deref, DerefMut, IsVariant};
use tracing::{debug, instrument};

use crate::{components::*, lang::TokenTrait, prelude::*};

/// [`Component`] used to indicate the currently targeted [`Token`] during a request.
#[derive(Component, Debug)]
pub struct TokenComponent {
    pub token: Spanned<Token>,
    pub range: crate::lsp_types::Range,
    pub text: String,
}

/// [`Component`] that contains the parsed tokens.
///
/// [`crate`] defines systems (like [`get_current_token`]) that depend
/// on Tokens to deduce [`TokenComponent`] during the [`CompletionLabel`] schedule.
#[derive(Component, AsRef, Deref, AsMut, DerefMut, Debug)]
pub struct Tokens(pub Vec<Spanned<Token>>);

#[instrument(skip(query, commands))]
pub fn get_current_token(
    mut query: Query<(Entity, &Tokens, &PositionComponent, &RopeC, &DynLang)>,
    mut commands: Commands,
) {
    for (entity, tokens, position, rope, helper) in &mut query {
        commands.entity(entity).remove::<TokenComponent>();
        let Some(offset) = position_to_offset(position.0, &rope.0) else {
            debug!("Couldn't transform to an offset ({:?})", position.0);
            continue;
        };

        let Some(token) = tokens
            .0
            .iter()
            .filter(|x| x.span().contains(&offset))
            .min_by_key(|x| x.span().end - x.span().start)
        else {
            let closest = tokens.0.iter().min_by_key(|x| {
                let start = if offset > x.span().start {
                    offset - x.span().start
                } else {
                    x.span().start - offset
                };

                let end = if offset > x.span().end {
                    offset - x.span().end
                } else {
                    x.span().end - offset
                };

                if start > end {
                    end
                } else {
                    start
                }
            });
            debug!(
                "Failed to find a token, offset {} closest {:?}",
                offset, closest
            );
            continue;
        };

        let (text, range) = helper.get_relevant_text(token, rope);
        let Some(range) = range_to_range(&range, &rope.0) else {
            debug!("Failed to transform span to range");
            continue;
        };

        debug!("Current token {:?} {}", token, text);
        commands.entity(entity).insert(TokenComponent {
            token: token.clone(),
            range,
            text,
        });
    }
}

pub trait Membered: Sized + 'static {
    const ITEMS: &'static [Self];

    fn complete(&self) -> &'static str;
}

macro_rules! derive_enum {
    // entry point
    ($(#$meta:tt)? $vis:vis enum $name:ident {
        $($xs:ident $(@ $st:tt)? $(=> $it:tt)?) ,* $(,)?
    }) => {

        $(#$meta)? $vis enum $name {
            $($xs),*
        }


        derive_enum!(@membered $name {$($xs)* } {}: $($xs $(=> $it)? ),* ,);
        derive_enum!(@fromStr $name {}: $($xs $(@ $st)? ),* ,);
    };

    // fromstr implementation
    (@fromStr $name:ident {$($eout:tt)*}: $member:ident @ $str:tt , $($xs:tt)*) => {
        derive_enum!(@fromStr $name
            { $str => Ok($name::$member), $($eout)*}:
            $($xs)*
        );
    };
    (@fromStr $name:ident {$($eout:tt)*}: $member:ident , $($xs:tt)*) => {
        derive_enum!(@fromStr $name
            { x if x.eq_ignore_ascii_case(stringify!($member))  => Ok($name::$member), $($eout)*}:
            $($xs)*
        );
    };

    (@fromStr $name:ident {$($eout:tt)*}:) => {
        impl std::str::FromStr for $name {
            type Err = ();

            fn from_str(st: &str) -> Result<Self, Self::Err> {
                match st {
                    $($eout)*
                    _ => Err(()),
                }
            }
        }
    };

    // membered implementation
    (@membered $name:ident {$($els:ident)*} {$($eout:tt)*}: $member:ident => $str:tt , $($xs:tt)*) => {
        derive_enum!(@membered $name {$($els)*}
            {
                $name::$member => $str,
                $($eout)*
            }:
            $($xs)*
        );
    };
    (@membered $name:ident {$($els:ident)*} {$($eout:tt)*}: $member:ident , $($xs:tt)*) => {
        derive_enum!(@membered $name {$($els)*}
            {
                $name::$member => stringify!($member),
                $($eout)*
            }:
            $($xs)*
        );
    };

    (@membered $name:ident {$($xs:ident)*} {$($eout:tt)*}:) => {
        impl Membered for $name {
            const ITEMS: &'static [Self] =  &[
                $($name::$xs),*
            ];

            fn complete(&self) -> &'static str {
                match self {
                    $($eout)*
                }
            }
        }
    };
}

derive_enum!(
    #[derive(Debug, Clone, PartialEq)]
    pub enum SparqlExpr2 {
        Or => "PLUS",
        Plus @ "+" => "PLUS"
    }
);

derive_enum!(
    pub enum SparqlExpr3 {
        Or => "PLUS",
        Plus @ "+" => "PLUS",
    }
);

#[cfg(test)]
mod tests {
    use std::str::FromStr as _;

    use super::SparqlExpr2;

    #[test]
    fn test_sparql_expr_2() {
        let or = SparqlExpr2::from_str("or");
        assert_eq!(or, Ok(SparqlExpr2::Or));

        let or = SparqlExpr2::from_str("+");
        assert_eq!(or, Ok(SparqlExpr2::Plus));
    }
}

pub mod semantic_token {
    use crate::lsp_types::SemanticTokenType as STT;
    pub const BOOLEAN: STT = STT::new("boolean");
    pub const LANG_TAG: STT = STT::new("langTag");
}

derive_enum!(
    #[derive(Clone, PartialEq, Eq,Ord, PartialOrd, Hash, Debug)]
    pub enum SparqlExpr {
        Or @ "||",
        And @ "&&",
        Equal @ "=",
        NotEqual @ "!=",
        Lt @ "<",
        Gt @ ">",
        Lte @ "<=",
        Gte @ ">=",
        In,
        Not,
        Plus @ "+",
        Minus @ "-",
        Times @ "*",
        Divide @ "/",
        Exclamation @ "!",
    }
);

derive_enum!(
    #[derive(Clone, PartialEq, Eq, Hash,Ord, PartialOrd, Debug)]
    pub enum SparqlCall {
        Str => "STR",
        Lang => "LANG",
        LangMatches => "langMatches",
        LangDir => "LANGDIR",
        Datatype => "datatype",
        Bound => "BOUND",
        Iri => "IRI",
        Uri => "URI",
        Bnode => "BNODE",
        Rand => "RAND",
        Abs => "ABS",
        Ceil => "CEIL",
        Floor => "FLOOR",
        Round => "ROUND",
        Concat => "CONCAT",
        StrLen => "STRLEN",
        Ucase => "UCASE",
        Lcase => "lcase",
        EncodeForUri => "ENCODE_FOR_URI",
        Contains => "CONTAINS",
        StrStarts => "STRSTARTS",
        StrEnds => "STRENDS",
        StrBefore => "STRBEFORE",
        StrAfter => "STRAFTER",
        Year => "YEAR",
        Month => "MONTH",
        Day => "DAY",
        Hours => "HOURS",
        Minutes => "MINUTES",
        Seconds => "SECONDS",
        Timezone => "TIMEZONE",
        Tz => "TZ",
        Now => "NOW",
        Uuid => "UUID",
        StrUuid => "STRUUID",
        Md5 => "MD5",
        Sha1 => "SHA1",
        Sha256 => "SHA256",
        Sha384 => "SHA384",
        Sha512 => "SHA512",
        Coalesce => "COALESCE",
        If => "IF",
        StrLang => "STRLANG",
        StrLangDir => "STRLANGDIR",
        StrDt => "STRDT",
        SameTerm => "sameTerm",
        IsIri => "isIRI",
        IsUri => "isURI",
        IsBlank => "isBLANK",
        IsLiteral => "isLITERAL",
        IsNumeric => "isNUMBERIC",
        HasLang => "hasLANG",
        HasLangDir => "hasLANGDIR",
        IsTriple => "isTRIPLE",
        Triple => "TRIPLE",
        Subject => "SUBJECT",
        Predicate => "PREDICATE",
        Object => "OBJECT",
    }
);

derive_enum!(
    #[derive(Clone, PartialEq, Eq,Ord, PartialOrd, Hash, Debug)]
    pub enum SparqlAggregate {
        Count => "COUNT",
        Sum => "SUM",
        Min => "MIN",
        Max => "MAX",
        Avg => "AVG",
        Sample => "SAMPLE",
        GroupConcat => "GROUP_CONCAT",
    }
);

derive_enum!(
    #[derive(Clone, PartialEq, Eq,Ord, PartialOrd, Hash, Debug)]
    pub enum SparqlKeyword {
        Regex => "REGEX",
        Substr => "SUBSTR",
        Replace => "REPLACE",
        Exists => "EXISTS",
        Select => "SELECT",
        Distinct => "DISTINCT",
        Reduced => "REDUCED",
        Optional => "OPTIONAL",
        Union => "UNION",
        As => "AS",
        Construct => "CONSTRUCT",
        Where => "WHERE",
        Describe => "DESCRIBE",
        Ask => "ASK",
        From => "FROM",
        Named => "NAMED",
        Group => "GROUP",
        By => "BY",
        Having => "HAVING",
        Order => "ORDER",
        Asc => "ASC",
        Desc => "DESC",
        Limit => "LIMIT",
        Offset => "OFFSET",
        Values => "VALUES",
        Load => "LOAD",
        Silent => "SILENT",
        Clear => "CLEAR",
        Drop => "DROP",
        Create => "CREATE",
        Add => "ADD",
        Move => "MOVE",
        Copy => "COPY",
        Insert => "INSERT",
        Data => "DATA",
        Delete => "DELETE",
        With => "WITH",
        Using => "USING",
        Default => "DEFAULT",
        All => "ALL",
        Graph => "GRAPH",
        Service => "SERVICE",
        Bind => "BIND",
        Undef => "UNDEF",
        Minus => "MINUS",
        Filter => "FILTER",
    }
);

#[derive(Clone, PartialEq, Ord, PartialOrd, Eq, Hash, Debug, IsVariant)]
pub enum Token {
    /// Sparql expression
    SparqlExpr(SparqlExpr),
    /// Sparql keyword
    SparqlKeyword(SparqlKeyword),
    /// Sparql call
    SparqlCall(SparqlCall),
    /// Sparql aggregate
    SparqlAggregate(SparqlAggregate),
    /// Sparql variable
    Variable(String),
    // Turtle Tokens
    /// @prefix
    PrefixTag,
    /// @base
    BaseTag,
    /// sparql prefix
    SparqlPrefix,
    /// sparql base
    SparqlBase,

    /// a
    PredType,

    /// [
    SqOpen,
    /// ]
    SqClose,
    /// {
    CurlOpen,
    /// }
    CurlClose,
    /// (
    BracketOpen,
    /// )
    BracketClose,

    /// ^^
    DataTypeDelim,

    /// .
    Stop,
    /// ;
    PredicateSplit,
    /// ,
    Comma,

    /// true
    True,
    /// false
    False,
    /// <...>
    IRIRef(String),

    /// ..:
    PNameLN(Option<String>, String),
    /// _:...
    BlankNodeLabel(String),
    /// @...
    LangTag(String),

    Number(String),
    /// All string types
    Str(String, StringStyle),

    /// [ ]
    ANON,
    Comment(String),

    /// :
    Colon,
    /// null
    Null,

    Invalid(String),
}

/// Token struct holding the token and the index in the token array
#[derive(Clone, Ord, PartialOrd, Hash, Debug)]
pub struct PToken(pub Token, pub usize);
impl PartialEq for PToken {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
impl Into<PToken> for Token {
    fn into(self) -> PToken {
        PToken(self, 0)
    }
}

impl Eq for PToken {}

impl TokenTrait for Token {
    fn token(&self) -> Option<crate::lsp_types::SemanticTokenType> {
        match self {
            Token::PrefixTag
            | Token::BaseTag
            | Token::SparqlPrefix
            | Token::SparqlBase
            | Token::PredType
            | Token::SparqlKeyword(_)
            | Token::SparqlCall(_) => Some(crate::lsp_types::SemanticTokenType::KEYWORD),
            Token::True | Token::False => Some(semantic_token::BOOLEAN),
            Token::IRIRef(_) => Some(crate::lsp_types::SemanticTokenType::PROPERTY),
            Token::LangTag(_) => Some(semantic_token::LANG_TAG),
            Token::Number(_) => Some(crate::lsp_types::SemanticTokenType::NUMBER),
            Token::Str(_, _) => Some(crate::lsp_types::SemanticTokenType::STRING),
            Token::Comment(_) => Some(crate::lsp_types::SemanticTokenType::COMMENT),
            Token::Variable(_) => Some(crate::lsp_types::SemanticTokenType::VARIABLE),
            _ => None,
        }
    }

    fn span_tokens(
        Spanned(this, span): &Spanned<Self>,
    ) -> Vec<(crate::lsp_types::SemanticTokenType, Range<usize>)> {
        if let Some(t) = this.token() {
            return vec![(t, span.clone())];
        }

        match this {
            Token::PNameLN(p, _) => {
                let s = p.as_ref().map(|x| x.len()).unwrap_or(0);

                vec![
                    (
                        crate::lsp_types::SemanticTokenType::NAMESPACE,
                        span.start..span.start + 1 + s,
                    ),
                    (
                        crate::lsp_types::SemanticTokenType::ENUM_MEMBER,
                        span.start + s + 1..span.end,
                    ),
                ]
            }
            Token::BlankNodeLabel(_) => {
                vec![
                    (
                        crate::lsp_types::SemanticTokenType::NAMESPACE,
                        span.start..span.start + 2,
                    ),
                    (
                        crate::lsp_types::SemanticTokenType::PROPERTY,
                        span.start + 2..span.end,
                    ),
                ]
            }
            _ => vec![],
        }
    }
}

#[derive(Clone, PartialEq, Ord, PartialOrd, Eq, Hash, Debug)]
pub enum StringStyle {
    /// """..."""
    DoubleLong,
    /// "..."
    Double,
    /// '''...'''
    SingleLong,
    /// '...'
    Single,
}

impl StringStyle {
    pub fn quote(&self) -> &'static str {
        match self {
            StringStyle::DoubleLong => "\"\"\"",
            StringStyle::Double => "\"",
            StringStyle::SingleLong => "'''",
            StringStyle::Single => "'",
        }
    }
}
impl std::fmt::Display for PToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::PrefixTag => write!(f, "'@prefix'"),
            Token::BaseTag => write!(f, "'@base'"),
            Token::SparqlPrefix => write!(f, "'PREFIX'"),
            Token::SparqlBase => write!(f, "'BASE'"),
            Token::PredType => write!(f, "'a'"),
            Token::SqOpen => write!(f, "'['"),
            Token::SqClose => write!(f, "']'"),
            Token::BracketOpen => write!(f, "'('"),
            Token::BracketClose => write!(f, "')'"),
            Token::DataTypeDelim => write!(f, "'^^'"),
            Token::Stop => write!(f, "'.'"),
            Token::PredicateSplit => write!(f, "';'"),
            Token::Comma => write!(f, "','"),
            Token::True => write!(f, "'true'"),
            Token::False => write!(f, "'false'"),
            Token::IRIRef(_) => write!(f, "a named node"),
            Token::PNameLN(_, _) => write!(f, "a prefixed node"),
            Token::BlankNodeLabel(_) => write!(f, "a blank node"),
            Token::LangTag(_) => write!(f, "a language tag"),
            Token::Number(_) => write!(f, "a number"),
            Token::Str(_, _) => write!(f, "a string"),
            Token::ANON => write!(f, "an inline blank node"),
            Token::Comment(_) => write!(f, "a comment"),
            Token::Invalid(_) => write!(f, "invalid token"),
            Token::CurlOpen => write!(f, "'{{'"),
            Token::CurlClose => write!(f, "'}}'"),
            Token::Colon => write!(f, "':'"),
            Token::Null => write!(f, "'null'"),
            Token::SparqlExpr(_) => write!(f, "sparql expr token"),
            Token::SparqlKeyword(_) => write!(f, "sparql keyword"),
            Token::SparqlCall(_) => write!(f, "sparql call"),
            Token::SparqlAggregate(_) => write!(f, "sparql aggregate"),
            Token::Variable(_) => write!(f, "sparql variable"),
        }
    }
}
