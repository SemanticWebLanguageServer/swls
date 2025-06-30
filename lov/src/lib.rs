use std::borrow::Cow;

use bevy_ecs::component::Component;

mod min_prefixes;
#[derive(Debug, Component, Clone)]
pub struct LocalPrefix {
    pub location: Cow<'static, str>,
    pub content: Cow<'static, str>,
    pub name: Cow<'static, str>,
    pub title: Cow<'static, str>,
    pub rank: usize,
}

pub const LOCAL_PREFIXES: &'static [LocalPrefix] = min_prefixes::LOCAL_PREFIXES;
