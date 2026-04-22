use std::{collections::HashMap, fmt::Display, pin::Pin};

use crate::lsp_types::request::Request;
use crate::lsp_types::{Diagnostic, MessageType, Url};

#[derive(Debug)]
pub struct Resp {
    pub headers: Vec<(String, String)>,
    pub body: String,
    pub status: u16,
}

#[tower_lsp::async_trait]
pub trait Client: Clone + ClientSync {
    async fn log_message<M: Display + Sync + Send + 'static>(&self, ty: MessageType, msg: M) -> ();
    async fn publish_diagnostics(
        &self,
        uri: Url,
        diags: Vec<Diagnostic>,
        version: Option<i32>,
    ) -> ();
    async fn send_request<R: Request + Sync + Send + 'static>(
        &self,
        params: R::Params,
    ) -> Option<R::Result>
    where
        R::Params: Sync + Send + 'static,
        R::Result: Sync + Send + 'static;
}

pub trait ClientSync {
    fn spawn<F: std::future::Future<Output = ()> + Send + 'static>(&self, fut: F);
    fn spawn_local<F: std::future::Future<Output = ()> + 'static>(&self, fut: F);
    fn fetch(
        &self,
        url: &str,
        headers: &HashMap<String, String>,
    ) -> Pin<Box<dyn Send + std::future::Future<Output = Result<Resp, String>>>>;
}
