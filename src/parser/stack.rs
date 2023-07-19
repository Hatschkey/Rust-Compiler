pub struct Stack<T> {
    top: Option<Box<StackNode<T>>>,
}

impl<T> Stack<T> {
    pub fn new() -> Self {
        Self { top: None }
    }

    pub fn push(&mut self, data: T) {
        self.top = Option::Some(Box::new(StackNode::new(data, self.top.take())));
    }

    pub fn pop(&mut self) -> Option<&T> {
        match self.top.take() {
            None => None,
            Some(stack_node) => {
                self.top = stack_node.next;
                return Option::Some(&stack_node.data);
            }
        }
    }

    pub fn peek(&self) -> Option<&T> {
        match self.top.as_ref() {
            None => None,
            Some(stack_node) => {
                return Option::Some(&stack_node.data);
            }
        }
    }
}

struct StackNode<T> {
    data: T,
    next: Option<Box<StackNode<T>>>,
}

impl<T> StackNode<T> {
    pub fn new(data: T, next: Option<Box<StackNode<T>>>) -> StackNode<T> {
        return StackNode { data, next };
    }
}
