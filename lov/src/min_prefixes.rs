use std::borrow::Cow;

use super::LocalPrefix;

pub const LOCAL_PREFIXES: &'static [LocalPrefix] = &[
    LocalPrefix {
        location: Cow::Borrowed("http://xmlns.com/foaf/0.1/"),
        content: Cow::Borrowed(include_str!("../prefixes/foaf.ttl")),
        name: Cow::Borrowed("foaf"),
        title: Cow::Borrowed("Friend of a Friend vocabulary"),
        rank: 1,
    },
    LocalPrefix {
        location: Cow::Borrowed("https://w3id.org/tree#"),
        content: Cow::Borrowed(include_str!("../prefixes/tree.ttl")),
        name: Cow::Borrowed("tree"),
        title: Cow::Borrowed("TREE"),
        rank: 1,
    },
    LocalPrefix {
        location: Cow::Borrowed("http://w3id.org/rml/core#"),
        content: Cow::Borrowed(include_str!("../prefixes/rml.ttl")),
        name: Cow::Borrowed("rml"),
        title: Cow::Borrowed("RML: Generic Mapping Language for RDF"),
        rank: 1,
    },
    LocalPrefix {
        location: Cow::Borrowed("http://w3id.org/rml/cc/"),
        content: Cow::Borrowed(include_str!("../prefixes/rml-cc.ttl")),
        name: Cow::Borrowed("rml-cc"),
        title: Cow::Borrowed("RML-Containers"),
        rank: 1,
    },
    LocalPrefix {
        location: Cow::Borrowed("http://w3id.org/rml/fnml/"),
        content: Cow::Borrowed(include_str!("../prefixes/rml-fnml.ttl")),
        name: Cow::Borrowed("rml-fnml"),
        title: Cow::Borrowed("RML-FNML"),
        rank: 1,
    },
    LocalPrefix {
        location: Cow::Borrowed("http://w3id.org/rml/io/"),
        content: Cow::Borrowed(include_str!("../prefixes/rml-io.ttl")),
        name: Cow::Borrowed("rml-io"),
        title: Cow::Borrowed("RML-IO: Source and Target"),
        rank: 1,
    },
    LocalPrefix {
        location: Cow::Borrowed("http://w3id.org/rml/star/"),
        content: Cow::Borrowed(include_str!("../prefixes/rml-star.ttl")),
        name: Cow::Borrowed("rml-star"),
        title: Cow::Borrowed("RML-star"),
        rank: 1,
    },
    LocalPrefix {
        location: Cow::Borrowed("http://www.w3.org/2002/07/owl#"),
        content: Cow::Borrowed(include_str!("../prefixes/owl.ttl")),
        name: Cow::Borrowed("owl"),
        title: Cow::Borrowed("The OWL 2 Schema vocabulary"),
        rank: 1,
    },
    LocalPrefix {
        location: Cow::Borrowed("http://www.w3.org/2000/01/rdf-schema#"),
        content: Cow::Borrowed(include_str!("../prefixes/rdfs.ttl")),
        name: Cow::Borrowed("rdfs"),
        title: Cow::Borrowed("The RDF Schema vocabulary"),
        rank: 1,
    },
    LocalPrefix {
        location: Cow::Borrowed("http://www.w3.org/1999/02/22-rdf-syntax-ns#"),
        content: Cow::Borrowed(include_str!("../prefixes/rdf.ttl")),
        name: Cow::Borrowed("rdf"),
        title: Cow::Borrowed("The RDF Concepts Vocabulary"),
        rank: 1,
    },
];
