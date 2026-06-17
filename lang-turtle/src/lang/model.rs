#[cfg(test)]
mod test {
    use swls_lang_rdf_base::traits::{Turtle, TurtleExt};

    fn parse_turtle(inp: &str, base_url: &str) -> Turtle {
        use rdf_parsers::turtle::{convert::convert, Lang, Rule, SyntaxKind};
        let (parse, _) = rdf_parsers::parse_incremental(
            Rule::new(SyntaxKind::TurtleDoc),
            inp,
            None,
            rdf_parsers::IncrementalBias::default(),
        );
        let mut t = convert(&parse.syntax::<Lang>());
        t.set_base = Some(base_url.to_string());
        t
    }

    #[test]
    fn easy_triples() {
        let txt = r#"
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

[] a foaf:Name;
   foaf:knows <abc>;.
"#;
        let output = parse_turtle(txt, "http://example.com/ns#");
        let triples = output.get_simple_triples().expect("Triples found");
        assert_eq!(triples.triples.len(), 3);
        println!("{:?}", triples);
    }

    #[test]
    fn easy_triples_2() {
        let txt = r#"
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

[ foaf:knows <abc>; ]
    a foaf:Name;
    foaf:knows [
        a foaf:Name;
        foaf:knows [
            a foaf:Name; ] ].
"#;
        let output = parse_turtle(txt, "http://example.com/ns#");
        let triples = output.get_simple_triples().expect("Triples found");
        assert_eq!(triples.triples.len(), 6);
    }

    #[test]
    fn triple_spans_are_correct() {
        // "@prefix foaf: <http://xmlns.com/foaf/0.1/>.\n<> foaf:name \"Arthur\"."
        // Byte layout after the newline (offset 44):
        //   44..46  "<>"
        //   47..56  "foaf:name"
        //   57..65  "\"Arthur\""
        let txt = "@prefix foaf: <http://xmlns.com/foaf/0.1/>.\n<> foaf:name \"Arthur\".";
        let output = parse_turtle(txt, "http://example.com/");
        let triples = output.get_simple_triples().expect("Triples found");
        assert_eq!(
            triples.triples.len(),
            1,
            "expected exactly one triple, got: {:#?}",
            triples.triples
        );
        let t = &triples.triples[0];
        println!("triple span:    {:?}", t.span);
        println!("subject span:   {:?}", t.subject.span);
        println!("predicate span: {:?}", t.predicate.span);
        println!("object span:    {:?}", t.object.span);

        // Subject "<>" is at bytes 44..46
        assert!(
            t.subject.span.contains(&44),
            "cursor at start of subject should be in subject span, span={:?}",
            t.subject.span
        );
        // Predicate "foaf:name" is at bytes 47..56
        assert!(
            t.predicate.span.contains(&50),
            "cursor in middle of predicate should be in predicate span, span={:?}",
            t.predicate.span
        );
        // Object "\"Arthur\"" is at bytes 57..65
        assert!(
            t.object.span.contains(&60),
            "cursor in middle of object should be in object span, span={:?}",
            t.object.span
        );

        // The triple outer span must contain all positions
        assert!(t.span.contains(&44));
        assert!(t.span.contains(&50));
        assert!(t.span.contains(&60));
    }

    #[test]
    fn owl_is_valid() {
        let txt = include_str!("../../../lov/prefixes/owl.ttl");
        let output = parse_turtle(txt, "http://example.com/ns#");
        output.get_simple_triples().expect("Triples found");
    }

    #[test]
    fn owl_is_valid_2() {
        let txt = r#"
@prefix dc:    <http://purl.org/dc/elements/1.1/> .
@prefix rdfs:  <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl:   <http://www.w3.org/2002/07/owl#> .
@prefix xsd:   <http://www.w3.org/2001/XMLSchema#> .
@prefix rdf:   <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix xml:   <http://www.w3.org/XML/1998/namespace> .
@prefix grddl: <http://www.w3.org/2003/g/data-view#> .

<http://www.w3.org/2002/07/owl>
        a                              owl:Ontology ;
        rdfs:comment                   "
  This ontology partially describes the built-in " ; .
            "#;
        let output = parse_turtle(txt, "http://example.com/ns#");
        output.get_simple_triples().expect("Triples found");
    }
}
