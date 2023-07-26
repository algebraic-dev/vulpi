use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    ops::Range,
};

#[derive(Debug)]
pub struct TreeDisplay {
    pub label: String,
    pub children: Vec<TreeDisplay>,
}

impl TreeDisplay {
    pub fn pretty_print(
        &self,
        fmt: &mut std::fmt::Formatter,
        indent: String,
        last: bool,
    ) -> std::fmt::Result {
        let indent_now = format!("{}{}", indent, if last { "└" } else { "├" });
        writeln!(fmt, "{}{}", indent_now, self.label)?;
        let indent = format!("{}{}  ", indent, if last { " " } else { "│" });
        for (index, child) in self.children.iter().enumerate() {
            child.pretty_print(fmt, indent.clone(), index == self.children.len() - 1)?;
        }
        Ok(())
    }

    pub fn label(label: &str) -> Self {
        Self {
            label: label.to_string(),
            children: vec![],
        }
    }

    pub fn with(mut self, child: TreeDisplay) -> Self {
        self.children.push(child);
        self
    }
}

impl Display for TreeDisplay {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.pretty_print(fmt, "".to_string(), true)
    }
}

pub trait Show {
    fn show(&self) -> TreeDisplay;
}

impl<T: Show> Show for &T {
    fn show(&self) -> TreeDisplay {
        (*self).show()
    }
}

impl Show for usize {
    fn show(&self) -> TreeDisplay {
        TreeDisplay::label(&self.to_string())
    }
}

impl Show for String {
    fn show(&self) -> TreeDisplay {
        TreeDisplay::label(self)
    }
}

impl Show for &str {
    fn show(&self) -> TreeDisplay {
        TreeDisplay::label(self)
    }
}

impl<T: Show> Show for Box<T> {
    fn show(&self) -> TreeDisplay {
        self.as_ref().show()
    }
}

impl<T: Show> Show for Vec<T> {
    fn show(&self) -> TreeDisplay {
        let mut node = TreeDisplay::label("Vec");
        for child in self {
            node = node.with(child.show());
        }
        node
    }
}

impl<T: Show, U: Show> Show for HashMap<T, U> {
    fn show(&self) -> TreeDisplay {
        let mut node = TreeDisplay::label("HashMap");
        for (key, value) in self {
            node = node.with(
                TreeDisplay::label("Entry")
                    .with(key.show())
                    .with(value.show()),
            );
        }
        node
    }
}

impl<T: Show> Show for HashSet<T> {
    fn show(&self) -> TreeDisplay {
        let mut node = TreeDisplay::label("HashSet");
        for child in self {
            node = node.with(child.show());
        }
        node
    }
}

impl<T: Show, U: Show> Show for (T, U) {
    fn show(&self) -> TreeDisplay {
        let mut node = TreeDisplay::label("Tuple");
        node = node.with(self.0.show());
        node = node.with(self.1.show());
        node
    }
}

impl<T: std::fmt::Debug> Show for Range<T> {
    fn show(&self) -> TreeDisplay {
        TreeDisplay::label(&format!("Range({:?}..{:?})", self.start, self.end))
    }
}

impl Show for bool {
    fn show(&self) -> TreeDisplay {
        TreeDisplay::label(&self.to_string())
    }
}

impl<T: Show> Show for Option<T> {
    fn show(&self) -> TreeDisplay {
        match self {
            Some(value) => value.show(),
            None => TreeDisplay::label("None"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let node = TreeDisplay::label("root")
            .with(TreeDisplay::label("child1"))
            .with(TreeDisplay::label("child2").with(TreeDisplay::label("child3")));
        println!("{}", node);
    }
}
