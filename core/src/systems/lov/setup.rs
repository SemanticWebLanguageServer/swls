use std::{borrow::Cow, collections::HashSet};

use bevy_ecs::prelude::*;

use crate::{prelude::*, systems::prefix::PREFIX_CC};

#[derive(Component, Debug)]
pub struct PrefixEntry {
    pub namespace: Cow<'static, str>,
    pub name: Cow<'static, str>,
    pub rank: usize,
}

pub fn populate_known_ontologies(mut commands: Commands) {
    let mut actual_local = HashSet::new();
    for lov in lov::LOCAL_PREFIXES.iter() {
        actual_local.insert(lov.name.clone());
        commands.spawn(lov.clone());
    }

    for (i, (name, url)) in PREFIX_CC
        .split('\n')
        .flat_map(|x| {
            let mut s = x.split(' ');
            let first = s.next()?;
            let second = s.next()?;
            Some((first.to_string(), second.to_string()))
        })
        .enumerate()
    {
        let name: Cow<'static, str> = name.into();
        if actual_local.contains(&name) {
            continue;
        }
        let namespace: Cow<'static, str> = url.into();
        let lov = PrefixEntry {
            namespace,
            name: name.clone(),
            rank: i,
        };

        commands.spawn(lov);
    }
}

#[derive(Debug, Clone, Component)]
pub struct FromPrefix(pub Prefix);
