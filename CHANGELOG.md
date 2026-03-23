# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.1.0] - 2026-03-23

### Added

**Language Support**
- Turtle/TriG parsing, validation and full LSP support
- SPARQL query language support
- JSON-LD support with RDF triple extraction

**Completion**
- Prefix completion with automatic `@prefix` insertion
- Property completion ordered by ontology domain/range
- Class completion for `rdf:type` assertions
- SPARQL variable completion
- Integration with Linked Open Vocabularies (LOV) for unknown prefixes
- Prefix.cc integration with 150K+ common prefixes

**Diagnostics**
- Syntax error detection
- Undefined prefix warnings
- Unused prefix detection
- SHACL shape violation reporting

**Navigation**
- Go to Definition
- Find References

**Refactoring**
- Rename symbol
- Organize imports code action (sort prefix declarations)

**Hover**
- Class and property documentation
- Domain/range information
- Type inference display
- Superclass/subclass hierarchy

**Other LSP features**
- Document formatting for Turtle
- Semantic highlighting
- Inlay hints for inferred types

**SHACL Validation**
- Shape parsing and compilation
- Native validation engine
- Multi-file shape support via `owl:imports`
- Global shapes support

**Integrations**
- Linked Open Vocabularies (LOV) API integration with caching
- Prefix.cc prefix database
- Oxigraph RDF store with RDF 1.2 support

**Editor support**
- VS Code extension (WASM-based)
- JetBrains plugin
- Any LSP-compatible editor via the `swls` binary
- NixOS/nix support
