use std::borrow::Cow;

use super::LocalPrefix;

pub const LOCAL_PREFIXES: &'static [LocalPrefix] = &[
    LocalPrefix {
        location: Cow::Borrowed("http://xmlns.com/foaf/0.1/"),
        namespace: Cow::Borrowed("http://xmlns.com/foaf/0.1/"),
        content: Cow::Borrowed(include_str!("../prefixes/foaf.ttl")),
        name: Cow::Borrowed("foaf"),
        title: Cow::Borrowed("Friend of a Friend vocabulary"),
        rank: 1,
    },
    LocalPrefix {
        location: Cow::Borrowed("http://www.w3.org/2002/07/owl#"),
        namespace: Cow::Borrowed("http://www.w3.org/2002/07/owl#"),
        content: Cow::Borrowed(include_str!("../prefixes/owl.ttl")),
        name: Cow::Borrowed("owl"),
        title: Cow::Borrowed("The OWL 2 Schema vocabulary"),
        rank: 1,
    },
    LocalPrefix {
        location: Cow::Borrowed("http://www.w3.org/2000/01/rdf-schema#"),
        namespace: Cow::Borrowed("http://www.w3.org/2000/01/rdf-schema#"),
        content: Cow::Borrowed(include_str!("../prefixes/rdfs.ttl")),
        name: Cow::Borrowed("rdfs"),
        title: Cow::Borrowed("The RDF Schema vocabulary"),
        rank: 1,
    },
    LocalPrefix {
        location: Cow::Borrowed("http://www.w3.org/1999/02/22-rdf-syntax-ns#"),
        namespace: Cow::Borrowed("http://www.w3.org/1999/02/22-rdf-syntax-ns#"),
        content: Cow::Borrowed(include_str!("../prefixes/rdf.ttl")),
        name: Cow::Borrowed("rdf"),
        title: Cow::Borrowed("The RDF Concepts Vocabulary"),
        rank: 1,
    },
];
