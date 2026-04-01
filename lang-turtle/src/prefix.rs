use std::{borrow::Cow, collections::HashMap};

use crate::lang::{model::{Turtle, NamedNodeExt, TurtleExt}};
use swls_core::{lsp_types::Url, prelude::*, util::fs::File};

/// Finds a resource.
/// If the location starts with http, it is a remote resource.
/// If the location fails to parse as a url, it is a file path to read.
pub async fn find(location: &str, fs: &Fs, client: &impl Client) -> Option<Vec<(String, Url)>> {
    if location.starts_with("http") {
        let url = Url::parse(location).ok()?;
        let content = client
            .fetch(
                &location,
                &HashMap::from([(String::from("Accept"), String::from("text/turtle"))]),
            )
            .await
            .ok()?
            .body;
        Some(vec![(content, url)])
    } else {
        if let Ok(url) = Url::parse(&location) {
            let content = fs.0.read_file(&url).await?;
            Some(vec![(content, url)])
        } else {
            let files = fs.0.glob_read(&location).await?;
            Some(
                files
                    .into_iter()
                    .flat_map(|File { content, name }| {
                        file_name_to_url(&name).map(|url| (content, url))
                    })
                    .collect(),
            )
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]
pub fn file_name_to_url(name: &str) -> Option<Url> {
    swls_core::lsp_types::Url::from_file_path(name).ok()
}

#[cfg(target_arch = "wasm32")]
pub fn file_name_to_url(_name: &str) -> Option<Url> {
    None
}

pub fn prefix_from_url(url: &Url) -> Option<(Cow<'static, str>, Cow<'static, str>)> {
    let segments = url.path_segments()?;
    let last = segments.last()?;

    let prefix = last.rsplit_once('.').map(|(x, _)| x).unwrap_or(last);
    let prefix: Cow<'static, str> = Cow::Owned(prefix.to_string());

    let url = Cow::Owned(url.to_string());

    Some((prefix, url))
}

pub fn prefix_from_declaration(
    turtle: &Triples2<'_>,
) -> Option<(Cow<'static, str>, Cow<'static, str>)> {
    let subject = turtle
        .iter()
        .find(|t| {
            t.predicate.as_str() == "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
                && t.object.as_str() == "http://www.w3.org/2002/07/owl#Ontology"
        })?
        .subject
        .to_owned();

    let prefix = turtle
        .iter()
        .find(|t| {
            t.subject.eq(&subject)
                && t.predicate.as_str() == "http://purl.org/vocab/vann/preferredNamespacePrefix"
        })?
        .object
        .as_str();
    let prefix = Cow::Owned(String::from(prefix));

    let url = turtle
        .iter()
        .find(|t| {
            t.subject.eq(&subject)
                && t.predicate.as_str() == "http://purl.org/vocab/vann/preferredNamespaceUri"
        })?
        .object
        .as_str();
    let url = Cow::Owned(String::from(url));

    Some((prefix, url))
}

pub fn prefix_from_prefixes(
    model: &Turtle,
    turtle: &Triples2<'_>,
) -> Option<(Cow<'static, str>, Cow<'static, str>)> {
    let shortened: Vec<_> = turtle
        .iter()
        .filter(|x| x.predicate.as_str() == "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
        .flat_map(|t| model.shorten(t.subject.as_str()))
        .collect();

    let mut counts = HashMap::new();
    for short in &shortened {
        if let Some((prefix, _)) = short.split_once(':') {
            let key: &mut usize = counts.entry(prefix).or_default();
            *key += 1;
        }
    }

    let (prefix, _) = counts.into_iter().max_by_key(|(_, e)| *e)?;

    let prefix = model
        .prefixes
        .iter()
        .find(|p| p.prefix.as_str() == prefix)?;

    Some((
        Cow::Owned(String::from(prefix.prefix.as_str())),
        Cow::Owned(prefix.value.0.expand(model)?),
    ))
}

pub fn prefix_from_source(
    url: &Url,
    source: &str,
) -> Option<(Cow<'static, str>, Cow<'static, str>)> {
    use crate::lang::parser::parse_new;
    use crate::lang::model::TurtleExt;
    let (turtle, _, _) = parse_new(source, url.as_str(), None);

    let triples = turtle.get_simple_triples().ok()?;

    prefix_from_declaration(&triples).or_else(|| prefix_from_prefixes(&turtle, &triples))
}
