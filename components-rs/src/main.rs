use std::fs::read_to_string;
use std::future::Future;
use std::os::unix::process;
use std::path::PathBuf;

use clap::{Parser, Subcommand};
use components_rs::components::registry::ComponentRegistry;
use components_rs::components::types::ComponentType;
use components_rs::config::registry::ConfigRegistry;
use components_rs::fs::OsFs;
use components_rs::module_state::ModuleState;
use rdf_parsers::jsonld::convert::{convert_with_loader, ContextLoader, JsonLdVal};
use rdf_parsers::jsonld::{Lang, Rule, SyntaxKind};
use rdf_parsers::IncrementalBias;
use url::Url;

#[derive(Parser)]
#[command(name = "components-js")]
#[command(about = "Analyze Components.js projects — list classes, configs, and modules")]
struct Cli {
    /// Path to the CJS project root (must have node_modules installed)
    #[arg(default_value = ".")]
    project_path: PathBuf,

    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// List all discovered CJS modules
    ListModules,
    /// List all discovered component classes
    ListClasses,
    /// List all discovered configuration instances
    ListConfigs,
    /// Show full summary of the project
    Summary,
    /// Print triples from a config file
    PrintTriples { path: String },
}

#[tokio::main(flavor = "current_thread")]
async fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| tracing_subscriber::EnvFilter::new("warn")),
        )
        .init();

    let cli = Cli::parse();
    let fs = OsFs;

    let abs_path = std::fs::canonicalize(&cli.project_path)?;
    let project_url = Url::from_directory_path(&abs_path)
        .map_err(|_| anyhow::anyhow!("Invalid project path: {}", cli.project_path.display()))?;

    println!("Looking at project url {}", project_url.as_str());
    let state = ModuleState::build(&fs, &project_url).await?;

    let mut comp_registry = ComponentRegistry::new();
    comp_registry
        .register_available_modules(&fs, &state)
        .await?;
    comp_registry.finalize();

    match cli.command {
        Commands::ListModules => {
            println!("CJS Modules ({}):", comp_registry.modules.len());
            println!("{}", "─".repeat(80));
            for module in comp_registry.modules.values() {
                println!(
                    "  {} (require: {})",
                    module.iri,
                    module.require_name.as_deref().unwrap_or("?")
                );
                println!(
                    "    Components: {}  Source: {}",
                    module.components.len(),
                    module.source_file
                );
            }
        }

        Commands::ListClasses => {
            let mut classes: Vec<_> = comp_registry
                .components
                .values()
                .filter(|c| {
                    c.component_type == ComponentType::Class
                        || c.component_type == ComponentType::AbstractClass
                })
                .collect();
            classes.sort_by_key(|c| &c.iri);

            println!("CJS Classes ({}):", classes.len());
            println!("{}", "─".repeat(80));
            for comp in classes {
                let type_label = match comp.component_type {
                    ComponentType::AbstractClass => " [abstract]",
                    _ => "",
                };
                println!("  {}{}", comp.iri, type_label);
                if let Some(ref elem) = comp.require_element {
                    println!("    requireElement: {elem}");
                }
                if !comp.parameters.is_empty() {
                    println!("    parameters:");
                    for param in &comp.parameters {
                        let range_str = param.range.as_deref().unwrap_or("any");
                        let flags: Vec<&str> = [
                            param.required.then_some("required"),
                            param.lazy.then_some("lazy"),
                            param.unique.then_some("unique"),
                        ]
                        .into_iter()
                        .flatten()
                        .collect();
                        let flags_str = if flags.is_empty() {
                            String::new()
                        } else {
                            format!(" [{}]", flags.join(", "))
                        };
                        println!("      - {} : {}{}", param.iri, range_str, flags_str);
                    }
                }
                if !comp.extends.is_empty() {
                    println!("    extends: {}", comp.extends.join(", "));
                }
                println!();
            }
        }

        Commands::ListConfigs => {
            let mut config_registry = ConfigRegistry::new();
            config_registry.discover_configs(&fs, &state).await?;

            println!("CJS Config Instances ({}):", config_registry.configs.len());
            println!("{}", "─".repeat(80));
            for config in &config_registry.configs {
                println!("  {}", config.iri);
                println!("    type: {}", config.component_type_iri);
                println!("    source: {}", config.source_file);
                if !config.parameters.is_empty() {
                    println!("    parameters:");
                    for (key, val) in &config.parameters {
                        let val_str = match val {
                            JsonLdVal::Str(s) => s.clone(),
                            other => format!("{:?}", other),
                        };
                        let truncated = if val_str.len() > 60 {
                            format!("{}...", &val_str[..57])
                        } else {
                            val_str
                        };
                        println!("      {key}: {truncated}");
                    }
                }
                println!();
            }
        }

        Commands::Summary => {
            let mut config_registry = ConfigRegistry::new();
            config_registry.discover_configs(&fs, &state).await?;

            let class_count = comp_registry
                .components
                .values()
                .filter(|c| c.component_type == ComponentType::Class)
                .count();
            let abstract_count = comp_registry
                .components
                .values()
                .filter(|c| c.component_type == ComponentType::AbstractClass)
                .count();

            println!("Components.js Project Summary");
            println!("{}", "═".repeat(80));
            println!("  Project:          {}", cli.project_path.display());
            println!("  Node modules:     {}", state.node_module_paths.len());
            println!("  CJS modules:      {}", comp_registry.modules.len());
            println!("  Classes:          {class_count}");
            println!("  Abstract classes: {abstract_count}");
            println!("  Config instances: {}", config_registry.configs.len());
            println!("  Contexts:         {}", state.contexts.len());
            println!("  Import paths:     {}", state.import_paths.len());
        }

        Commands::PrintTriples { path } => {
            let st = read_to_string(&path)?;

            let (parse, _) = rdf_parsers::parse_incremental(
                Rule::new(SyntaxKind::JsonldDoc),
                st.as_str(),
                None,
                IncrementalBias::default(),
            );

            let syntax = parse.syntax::<Lang>();

            let mut loader = Loader { state };
            let (jsonld_model, _ctx) = convert_with_loader(&syntax, &mut loader, Some(path)).await;

            for t in &jsonld_model.triples {
                println!("{}", t.0);
            }
        }
    }

    Ok(())
}

struct Loader {
    state: ModuleState,
}

impl ContextLoader for Loader {
    fn load<'a>(
        &'a mut self,
        _url: &'a str,
    ) -> std::pin::Pin<Box<dyn Future<Output = Option<String>> + 'a>> {
        Box::pin(std::future::ready(None))
    }

    fn load_val<'a>(
        &'a mut self,
        url: &'a str,
    ) -> std::pin::Pin<Box<dyn Future<Output = Option<JsonLdVal>> + 'a>> {
        Box::pin(async move {
            let val = self.state.contexts.get(url)?.clone();
            // If the context doc has a top-level @context, return that subtree.
            if let Some(ctx) = val.get("@context") {
                return Some(ctx.clone());
            }
            Some(val)
        })
    }
}
