use std::io;
use std::ops::Add;
use std::ops::Div;
use std::ops::Mul;
use std::ops::Sub;

#[derive(Debug)]
enum Expression<T> {
    Value(T),
    Scope(Box<Expression<T>>),
    Addition(Box<Expression<T>>, Box<Expression<T>>),
    Subtraction(Box<Expression<T>>, Box<Expression<T>>),
    Multiplication(Box<Expression<T>>, Box<Expression<T>>),
    Division(Box<Expression<T>>, Box<Expression<T>>),
}

#[derive(Debug)]
enum Token<T> {
    Number(T),
    BeginScope,
    EndScope,
    Addition,
    Subtraction,
    Multiplication,
    Division,
}

#[derive(Debug)]
enum ScopeNode {
    Leaf(Token<i32>),
    // InternalNode(parent_idx, children_tart_idx, nr_children)
    InternalNode(usize, usize, usize),
}

fn create_scope_tree(tokens: Vec<Token<i32>>) -> Vec<ScopeNode> {
    let mut scope_tree: Vec<ScopeNode> = Vec::new();
    let mut current_node_idx: usize = 0;

    scope_tree.push(ScopeNode::InternalNode(0, 1, 0));

    for token in tokens.into_iter() {
        match token {
            Token::BeginScope => {
                // Increment number of children
                match scope_tree[current_node_idx] {
                    ScopeNode::InternalNode(a, b, c) => {
                        scope_tree[current_node_idx] = ScopeNode::InternalNode(a, b, c + 1)
                    }
                    _ => (),
                }

                // Create new InternalNode representing the new scope
                scope_tree.push(ScopeNode::InternalNode(
                    current_node_idx,
                    scope_tree.len() + 1,
                    0,
                ));

                // Set the current node to the newly created node
                current_node_idx = scope_tree.len() - 1;
            }
            Token::EndScope => {
                // Set the current node to its parent
                if let ScopeNode::InternalNode(parent_idx, _, _) = scope_tree[current_node_idx] {
                    current_node_idx = parent_idx;
                }
            }
            _ => {
                // Add leaf as child of the node at current_node_idx
                scope_tree.push(ScopeNode::Leaf(token));

                // Increment number of children
                match scope_tree[current_node_idx] {
                    ScopeNode::InternalNode(a, b, c) => {
                        scope_tree[current_node_idx] = ScopeNode::InternalNode(a, b, c + 1)
                    }
                    _ => (),
                }
            }
        }
    }

    scope_tree
}

fn parse_mult_div(
    scope_tree: &mut Vec<ScopeNode>,
    stack: &mut Vec<Expression<i32>>,
    parent_idx: usize,
    nr_children: usize,
) -> Result<usize, String> {
    let children_idx = parent_idx + 1;
    let mut child_i = children_idx;
    let mut nr_children = nr_children;

    while child_i < children_idx + nr_children {
        if let ScopeNode::Leaf(Token::Multiplication) = scope_tree[child_i] {
            // Bound check for left operand
            if child_i == children_idx {
                return Err(String::from("Unexpected ''"));
            }

            // Bound check for right operand
            if child_i - children_idx >= nr_children {
                return Err(String::from("Expected expression after '*'"));
            }

            let lhs = match scope_tree[child_i - 1] {
                ScopeNode::Leaf(Token::Number(n)) => Expression::Value(n),
                ScopeNode::InternalNode(_, _, _) => stack.pop().unwrap(), // TODO: check if there is an element on the stack
                _ => Expression::Value(0),                                // TODO: Give error
            };
            let rhs = match scope_tree[child_i + 1] {
                ScopeNode::Leaf(Token::Number(n)) => Expression::Value(n),
                ScopeNode::InternalNode(_, _, _) => match stack.pop() {
                    Some(t) => t,
                    None => panic!(
                        "The stack is empty but an expression is needed {:?}",
                        scope_tree[child_i]
                    ),
                }, // TODO: check if there is an element on the stack
                _ => Expression::Value(0), // TODO: Give error
            };

            let expr = Expression::Multiplication(Box::new(lhs), Box::new(rhs));

            stack.push(expr);

            scope_tree.remove(child_i + 1);
            scope_tree.remove(child_i - 1);

            nr_children -= 2;

            child_i -= 1;
            scope_tree[child_i] = ScopeNode::InternalNode(0, 0, 0);
        } else if let ScopeNode::Leaf(Token::Division) = scope_tree[child_i] {
            // TODO: bound check these
            let lhs = match scope_tree[child_i - 1] {
                ScopeNode::Leaf(Token::Number(n)) => Expression::Value(n),
                ScopeNode::InternalNode(_, _, _) => stack.pop().unwrap(), // TODO: check if there is an element on the stack
                _ => Expression::Value(0),                                // TODO: Give error
            };
            let rhs = match scope_tree[child_i + 1] {
                ScopeNode::Leaf(Token::Number(n)) => Expression::Value(n),
                ScopeNode::InternalNode(_, _, _) => stack.pop().unwrap(), // TODO: check if there is an element on the stack
                _ => Expression::Value(0),                                // TODO: Give error
            };

            let expr = Expression::Division(Box::new(lhs), Box::new(rhs));

            stack.push(expr);

            scope_tree.remove(child_i + 1);
            scope_tree.remove(child_i - 1);

            nr_children -= 2;

            child_i -= 1;
            scope_tree[child_i] = ScopeNode::InternalNode(0, 0, 0);
        }

        child_i += 1;
    }

    Ok(nr_children)
}

fn parse_add_sub(
    scope_tree: &mut Vec<ScopeNode>,
    stack: &mut Vec<Expression<i32>>,
    parent_idx: usize,
    nr_children: usize,
) -> Result<usize, String> {
    let children_idx = parent_idx + 1;
    let mut child_i = children_idx;
    let mut nr_children = nr_children;

    while child_i < children_idx + nr_children {
        if let ScopeNode::Leaf(Token::Addition) = scope_tree[child_i] {
            // Bound check for left operand
            if child_i == children_idx {
                return Err(String::from("Unexpected ''"));
            }

            // Bound check for right operand
            if child_i - children_idx >= nr_children {
                return Err(String::from("Expected expression after '+'"));
            }

            let lhs = match scope_tree[child_i - 1] {
                ScopeNode::Leaf(Token::Number(n)) => Expression::Value(n),
                ScopeNode::InternalNode(_, _, _) => stack.pop().unwrap(), // TODO: check if there is an element on the stack
                _ => Expression::Value(0),                                // TODO: Give error
            };
            let rhs = match scope_tree[child_i + 1] {
                ScopeNode::Leaf(Token::Number(n)) => Expression::Value(n),
                ScopeNode::InternalNode(_, _, _) => match stack.pop() {
                    Some(t) => t,
                    None => panic!(
                        "The stack is empty but an expression is needed {:?}",
                        scope_tree[child_i]
                    ),
                }, // TODO: check if there is an element on the stack
                _ => Expression::Value(0), // TODO: Give error
            };

            let expr = Expression::Addition(Box::new(lhs), Box::new(rhs));

            stack.push(expr);

            scope_tree.remove(child_i + 1);
            scope_tree.remove(child_i - 1);

            nr_children -= 2;

            child_i -= 1;
            scope_tree[child_i] = ScopeNode::InternalNode(0, 0, 0);
        } else if let ScopeNode::Leaf(Token::Subtraction) = scope_tree[child_i] {
            // TODO: bound check these
            let lhs = match scope_tree[child_i - 1] {
                ScopeNode::Leaf(Token::Number(n)) => Expression::Value(n),
                ScopeNode::InternalNode(_, _, _) => stack.pop().unwrap(), // TODO: check if there is an element on the stack
                _ => Expression::Value(0),                                // TODO: Give error
            };
            let rhs = match scope_tree[child_i + 1] {
                ScopeNode::Leaf(Token::Number(n)) => Expression::Value(n),
                ScopeNode::InternalNode(_, _, _) => stack.pop().unwrap(), // TODO: check if there is an element on the stack
                _ => Expression::Value(0),                                // TODO: Give error
            };

            let expr = Expression::Subtraction(Box::new(lhs), Box::new(rhs));

            stack.push(expr);

            scope_tree.remove(child_i + 1);
            scope_tree.remove(child_i - 1);

            nr_children -= 2;

            child_i -= 1;
            scope_tree[child_i] = ScopeNode::InternalNode(0, 0, 0);
        }

        child_i += 1;
    }

    Ok(nr_children)
}

fn parse_scope(
    scope_tree: &mut Vec<ScopeNode>,
    stack: &mut Vec<Expression<i32>>,
    parent_index: usize,
    nr_children: usize,
) -> Result<(), String> {
    let children_idx = parent_index + 1;
    let mut nr_children = nr_children;

    // Scan for multiplication and division
    nr_children = match parse_mult_div(scope_tree, stack, parent_index, nr_children) {
        Ok(n) => n,
        Err(error) => return Err(error),
    };

    // Scan for addition and subtraction
    nr_children = match parse_add_sub(scope_tree, stack, parent_index, nr_children) {
        Ok(n) => n,
        Err(error) => return Err(error),
    };

    if nr_children > 0 {
        if let ScopeNode::Leaf(Token::Number(n)) = scope_tree[children_idx] {
            stack.push(Expression::Value(n));
            scope_tree[children_idx] = ScopeNode::InternalNode(0, 0, 0);
        }
    }

    // remove node
    scope_tree.remove(parent_index);

    Ok(())
}

fn parse_expression(tokens: Vec<Token<i32>>) -> Result<Vec<Expression<i32>>, String> {
    // The scope tree contains ScopeNode's that has a reference to their parent and children
    let mut scope_tree = create_scope_tree(tokens);

    // The stack will contain Expressions that represent a part of the input
    let mut stack: Vec<Expression<i32>> = Vec::new();

    // Loop over scope_tree backwards
    let mut index: usize = scope_tree.len() - 1;

    loop {
        if let ScopeNode::InternalNode(parent_idx, children_idx, nr_children) = scope_tree[index] {
            // This means that the node is already parsed and its expression is on the stack
            if parent_idx == children_idx {
                continue;
            }

            if let Result::Err(error) = parse_scope(&mut scope_tree, &mut stack, index, nr_children)
            {
                return Err(error);
            }
        }

        if index == 0 {
            break;
        }

        index -= 1;
    }

    println!("stack[0]:");
    println!("{:?}", stack[0]);

    Ok(stack)
}

// fn build_ast_from_node(tree: &mut Vec<ScopeNode>, node: ScopeNode) -> Expression<i32> {}

fn prompt(prompt: String) -> String {
    let mut input = String::from("");

    println!("{prompt}");

    io::stdin()
        .read_line(&mut input)
        .expect("Could not read user input");

    return input;
}

fn tokenize_expression(value: &String) -> Result<Vec<Token<i32>>, String> {
    let mut tokens: Vec<Token<i32>> = Vec::new();

    let mut number: Option<i32> = Option::None;

    for c in value.chars() {
        match number {
            None => (),
            Some(n) => {
                if !('0'..='9').contains(&c) {
                    tokens.push(Token::Number(n));
                    number = Option::None;
                }
            }
        }

        match c {
            ' ' => (),
            '+' => tokens.push(Token::Addition),
            '-' => tokens.push(Token::Subtraction),
            '*' => tokens.push(Token::Multiplication),
            '/' => tokens.push(Token::Division),
            '(' => tokens.push(Token::BeginScope),
            ')' => tokens.push(Token::EndScope),
            '0'..='9' => {
                if let Option::None = number {
                    number = Some(0);
                }

                let digit: i32 = c as i32 - '0' as i32;

                if let Option::Some(n) = number {
                    number = Some(n * 10 + digit);
                }
            }
            _ => (),
        }
    }

    if let Option::Some(n) = number {
        tokens.push(Token::Number(n));
    }

    return Ok(tokens);
}

fn evaluate_expression<
    T: Add<Output = T> + Sub<Output = T> + Mul<Output = T> + Div<Output = T> + Copy,
>(
    expression: &Expression<T>,
) -> T {
    match expression {
        Expression::Value(value) => *value,
        Expression::Scope(a) => evaluate_expression(a),
        Expression::Addition(a, b) => evaluate_expression(a) + evaluate_expression(b),
        Expression::Subtraction(a, b) => evaluate_expression(a) - evaluate_expression(b),
        Expression::Multiplication(a, b) => evaluate_expression(a) * evaluate_expression(b),
        Expression::Division(a, b) => evaluate_expression(a) / evaluate_expression(b),
    }
}

fn main() {
    let expression = prompt(String::from("Enter expression"));
    let expression = tokenize_expression(&expression)
        .map(&parse_expression)
        .expect("error during parsing I suppose");

    match expression {
        Err(error) => {
            println!("Could not parse user input: {error}");
        }
        Ok(value) => {
            let result = evaluate_expression(&value[0]);
            println!("The result is: {result}");
        }
    }
}
