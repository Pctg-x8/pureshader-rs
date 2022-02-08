#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Indentation {
    Brace(usize),
    AngleBracket(usize),
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Layout {
    Explicit,
    Implicit(usize),
}

pub struct LayoutContext {
    stack: Vec<Layout>,
}
impl LayoutContext {
    pub fn current(&self) -> Option<Layout> {
        self.stack.last().copied()
    }

    pub fn is_in_layout(&self) -> bool {
        !self.stack.is_empty()
    }

    pub fn is_in_implicit_layout(&self) -> bool {
        self.stack.last().map_or(false, |&x| x != Layout::Explicit)
    }

    pub fn shift(&mut self, new_layout: Layout) {
        self.stack.push(new_layout);
    }

    pub fn unshift(&mut self) {
        self.stack.pop();
    }
}
