#[macro_use]
extern crate tracing;

use bevy_ecs::prelude::*;
use swls_core::{lsp_types::SemanticTokenType, prelude::*};
use swls_lang_rdf_base::register_rdf_lang;
use swls_lang_turtle::lang::parser::TurtleParseError;

pub mod ecs;
use crate::ecs::{setup_completion, setup_parse};
pub mod lang;

pub fn setup_world(world: &mut World) {
    register_rdf_lang::<Sparql, SparqlHelper>(world, &["sparql"], &[".sq"]);
    setup_parse(world);
    setup_completion(world);
}

#[derive(Debug, Component, Default)]
pub struct Sparql;

impl Lang for Sparql {
    type Element = rdf_parsers::model::Turtle;
    type ElementError = TurtleParseError;

    const PATTERN: Option<&'static str> = None;
    const LANG: &'static str = "sparql";
    const CODE_ACTION: bool = false;
    const HOVER: bool = true;
    const TRIGGERS: &'static [&'static str] = &[];
    const LEGEND_TYPES: &'static [SemanticTokenType] = &[
        semantic_token::BOOLEAN,
        semantic_token::LANG_TAG,
        SemanticTokenType::COMMENT,
        SemanticTokenType::ENUM_MEMBER,
        SemanticTokenType::KEYWORD,
        SemanticTokenType::NAMESPACE,
        SemanticTokenType::NUMBER,
        SemanticTokenType::PROPERTY,
        SemanticTokenType::STRING,
        SemanticTokenType::VARIABLE,
    ];

    fn semantic_token_type(kind: rowan::SyntaxKind) -> Option<SemanticTokenType> {
        use rdf_parsers::sparql::parser::SyntaxKind as SK;
        match SK::from(kind) {
            SK::Var1 | SK::Var2 => Some(SemanticTokenType::VARIABLE),
            SK::MyString
            | SK::StringLiteral1
            | SK::StringLiteral2
            | SK::StringLiteralLong1
            | SK::StringLiteralLong2 => Some(SemanticTokenType::STRING),
            SK::Integer
            | SK::Decimal
            | SK::Double
            | SK::IntegerPositive
            | SK::IntegerNegative
            | SK::DecimalPositive
            | SK::DecimalNegative
            | SK::DoublePositive
            | SK::DoubleNegative => Some(SemanticTokenType::NUMBER),
            SK::Comment => Some(SemanticTokenType::COMMENT),
            SK::Langtag => Some(semantic_token::LANG_TAG),
            SK::TrueLit | SK::FalseLit => Some(semantic_token::BOOLEAN),
            SK::Iriref => Some(SemanticTokenType::PROPERTY),
            SK::Alit
            | SK::SparqlBaseToken
            | SK::SparqlPrefixToken
            | SK::AbsLit
            | SK::AddLit
            | SK::AllLit
            | SK::AsLit
            | SK::AscLit
            | SK::AskLit
            | SK::AvgLit
            | SK::BindLit
            | SK::BnodeLit
            | SK::BoundLit
            | SK::ByLit
            | SK::CeilLit
            | SK::ClearLit
            | SK::CoalesceLit
            | SK::ConcatLit
            | SK::ConstructLit
            | SK::ContainsLit
            | SK::CopyLit
            | SK::CountLit
            | SK::CreateLit
            | SK::DatatypeLit
            | SK::DayLit
            | SK::DefaultLit
            | SK::DeleteLit
            | SK::DeleteDataLit
            | SK::DeleteWhereLit
            | SK::DescLit
            | SK::DescribeLit
            | SK::DistinctLit
            | SK::DropLit
            | SK::EncodeForUriLit
            | SK::ExistsLit
            | SK::FilterLit
            | SK::FloorLit
            | SK::FromLit
            | SK::GraphLit
            | SK::GroupLit
            | SK::GroupConcatLit
            | SK::HavingLit
            | SK::HoursLit
            | SK::IfLit
            | SK::InLit
            | SK::InsertLit
            | SK::InsertDataLit
            | SK::IntoLit
            | SK::IriLit
            | SK::IsBlankLit
            | SK::IsIriLit
            | SK::IsLiteralLit
            | SK::IsNumericLit
            | SK::IsUriLit
            | SK::LangLit
            | SK::LangmatchesLit
            | SK::LcaseLit
            | SK::LimitLit
            | SK::LoadLit
            | SK::MaxLit
            | SK::Md5Lit
            | SK::MinLit
            | SK::MinusLit
            | SK::MinutesLit
            | SK::MonthLit
            | SK::MoveLit
            | SK::NamedLit
            | SK::NotLit
            | SK::NowLit
            | SK::OffsetLit
            | SK::OptionalLit
            | SK::OrderLit
            | SK::RandLit
            | SK::ReducedLit
            | SK::RegexLit
            | SK::ReplaceLit
            | SK::RoundLit
            | SK::SameTermLit
            | SK::SampleLit
            | SK::SecondsLit
            | SK::SelectLit
            | SK::SeparatorLit
            | SK::ServiceLit
            | SK::Sha1Lit
            | SK::Sha256Lit
            | SK::Sha384Lit
            | SK::Sha512Lit
            | SK::SilentLit
            | SK::StrLit
            | SK::StrafterLit
            | SK::StrbeforeLit
            | SK::StrdtLit
            | SK::StrendsLit
            | SK::StrlangLit
            | SK::StrlenLit
            | SK::StrstartsLit
            | SK::StruuidLit
            | SK::SubstrLit
            | SK::SumLit
            | SK::TimezoneLit
            | SK::ToLit
            | SK::TzLit
            | SK::UcaseLit
            | SK::UndefLit
            | SK::UnionLit
            | SK::UriLit
            | SK::UsingLit
            | SK::UuidLit
            | SK::ValuesLit
            | SK::WhereLit
            | SK::WithLit
            | SK::YearLit => Some(SemanticTokenType::KEYWORD),
            _ => None,
        }
    }

    fn semantic_token_spans(
        kind: rowan::SyntaxKind,
        span: std::ops::Range<usize>,
        text: &str,
    ) -> Vec<(SemanticTokenType, std::ops::Range<usize>)> {
        use rdf_parsers::sparql::parser::SyntaxKind as SK;
        match SK::from(kind) {
            SK::PnameLn => {
                let token_text = text.get(span.clone()).unwrap_or("");
                if let Some((a, _)) = token_text.split_once(':') {
                    let (start, end) = (span.start, span.end);
                    vec![
                        (SemanticTokenType::NAMESPACE, start..start + a.len() + 1),
                        (SemanticTokenType::PROPERTY, start + a.len() + 1..end),
                    ]
                } else {
                    vec![(SemanticTokenType::PROPERTY, span)]
                }
            }
            SK::PnameNs => vec![(SemanticTokenType::NAMESPACE, span)],
            SK::BlankNodeLabel => {
                if span.len() > 2 {
                    vec![
                        (SemanticTokenType::NAMESPACE, span.start..span.start + 2),
                        (SemanticTokenType::PROPERTY, span.start + 2..span.end),
                    ]
                } else {
                    vec![(SemanticTokenType::NAMESPACE, span)]
                }
            }
            _ => Self::semantic_token_type(kind)
                .map(|t| vec![(t, span)])
                .unwrap_or_default(),
        }
    }
}

/// All SPARQL keywords for completion.
pub static SPARQL_KEYWORDS: &[&str] = &[
    "REGEX",
    "SUBSTR",
    "REPLACE",
    "EXISTS",
    "SELECT",
    "DISTINCT",
    "REDUCED",
    "OPTIONAL",
    "UNION",
    "AS",
    "CONSTRUCT",
    "WHERE",
    "DESCRIBE",
    "ASK",
    "FROM",
    "NAMED",
    "GROUP",
    "BY",
    "HAVING",
    "ORDER",
    "ASC",
    "DESC",
    "LIMIT",
    "OFFSET",
    "VALUES",
    "LOAD",
    "SILENT",
    "CLEAR",
    "DROP",
    "CREATE",
    "ADD",
    "MOVE",
    "COPY",
    "INSERT",
    "DATA",
    "DELETE",
    "WITH",
    "USING",
    "DEFAULT",
    "ALL",
    "GRAPH",
    "SERVICE",
    "BIND",
    "UNDEF",
    "MINUS",
    "FILTER",
    // Aggregates
    "COUNT",
    "SUM",
    "MIN",
    "MAX",
    "AVG",
    "SAMPLE",
    "GROUP_CONCAT",
];

#[derive(Debug, Default)]
pub struct SparqlHelper;

impl LangHelper for SparqlHelper {
    fn keyword(&self) -> &[&'static str] {
        SPARQL_KEYWORDS
    }
    fn supports_shape_validation(&self) -> bool {
        false
    }
}
