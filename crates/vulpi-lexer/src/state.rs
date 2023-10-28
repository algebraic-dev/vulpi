pub struct State {
    pub start: usize,
    pub index: usize,
    pub line: usize,
    pub column: usize,
}

impl State {
    pub fn new() -> Self {
        Self {
            start: 0,
            index: 0,
            line: 0,
            column: 0,
        }
    }

    pub fn advance(&mut self, c: char) {
        self.index += c.len_utf8();
        if c == '\n' {
            self.line += 1;
            self.column = 0;
        } else {
            self.column += 1;
        }
    }

    pub fn save(&mut self) {
        self.start = self.index;
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_advance() {
        let mut state = State::new();
        state.advance('a');
        assert_eq!(state.index, 1);
        assert_eq!(state.line, 0);
        assert_eq!(state.column, 1);
        state.advance('\n');
        assert_eq!(state.index, 2);
        assert_eq!(state.line, 1);
        assert_eq!(state.column, 0);
        state.advance('b');
        assert_eq!(state.index, 3);
        assert_eq!(state.line, 1);
        assert_eq!(state.column, 1);
    }
}