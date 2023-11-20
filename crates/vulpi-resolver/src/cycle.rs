use std::collections::HashMap;

use petgraph::{graph::DiGraph, stable_graph::NodeIndex};

use vulpi_location::Span;
use vulpi_report::{Diagnostic, Report};
use vulpi_syntax::{r#abstract::Program, r#abstract::Qualified};

use crate::error::ResolverError;

#[derive(Default)]
pub struct DepHolder {
    nodes: HashMap<Qualified, NodeIndex<u32>>,
    graph: DiGraph<(), ()>,
    spans: HashMap<Qualified, Span>,
}

impl DepHolder {
    pub fn register(&mut self, program: &Program) {
        for let_ in &program.lets {
            let from = *self
                .nodes
                .entry(let_.signature.name.clone())
                .or_insert_with(|| self.graph.add_node(()));

            if let Some(res) = &let_.constant {
                for (to_, span) in res {
                    let to = self
                        .nodes
                        .entry(to_.clone())
                        .or_insert_with(|| self.graph.add_node(()));

                    self.graph.add_edge(from, *to, ());
                    self.spans.insert(to_.clone(), span.clone());
                }
            }
        }
    }

    pub fn report_cycles(&self, report: Report) {
        let inv_nodes = self
            .nodes
            .iter()
            .map(|(k, v)| (v, k))
            .collect::<HashMap<_, _>>();

        let cycles = petgraph::algo::tarjan_scc(&self.graph);

        for cycle in cycles {
            if cycle.len() > 1 {
                let mut cycle = cycle
                    .iter()
                    .map(|n| inv_nodes[n].clone())
                    .collect::<Vec<_>>();
                cycle.sort_by_key(|k| k.to_string());

                let first = cycle[0].clone();

                let span = self.spans[&first].clone();

                report.report(Diagnostic::new(ResolverError {
                    span,
                    kind: crate::error::ResolverErrorKind::CycleBetweenConstants(cycle),
                }))
            }
        }
    }
}
