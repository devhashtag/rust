use std::io;

#[derive(Debug)]
enum Either<L, R> {
    Left(L),
    Right(R),
}

#[derive(Debug, Clone)]
enum Expression<T> {
    Value(T),
    Scope(Box<Expression<T>>),
    Addition(Box<Expression<T>>, Box<Expression<T>>),
    Subtraction(Box<Expression<T>>, Box<Expression<T>>),
    Multiplication(Box<Expression<T>>, Box<Expression<T>>),
    Division(Box<Expression<T>>, Box<Expression<T>>),
}

#[derive(Debug, PartialEq)]
enum Token<T> {
    Number(T),
    BeginScope,
    EndScope,
    Addition,
    Subtraction,
    Multiplication,
    Division,
}

#[derive(Debug, PartialEq)]
enum ScopeNode {
    Leaf(Token<i32>),
    // InternalNode(parent_idx, children_start_idx, nr_children)
    InternalNode(usize, usize, usize),
}

fn main() {
    loop {
        match run() {
            Err(error) => {
                println!("Could not evaluate user input: {error}");
            }
            Ok(value) => {
                println!("The result is: {value}");
            }
        }
    }
}

fn run() -> Result<i32, String> {
    let input = prompt(String::from("Enter expression"));
    let tokens = tokenize_expression(&input)?;

    println!("Tokens: {:?}", tokens);

    let expression = parse_expression(tokens)?;

    evaluate_expression(&expression)
}

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
    let mut iter = value.chars().peekable();

    while let Some(c) = iter.next() {
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

        if let Some(n) = number {
            if iter.peek().is_none() || !('0'..='9').contains(iter.peek().unwrap()) {
                tokens.push(Token::Number(n));
                number = Option::None;
            }
        }
    }

    return Ok(tokens);
}

fn parse_expression(tokens: Vec<Token<i32>>) -> Result<Expression<i32>, String> {
    // The scope tree contains ScopeNodes that have a reference to their parent and children
    let mut scope_tree = create_scope_tree(tokens);

    // Loop over scope_tree backwards
    let mut index: usize = scope_tree.len() - 1;

    loop {
        if let Either::Right(ScopeNode::InternalNode(_, _, nr_children)) = scope_tree[index] {
            if let Result::Err(error) = parse_scope(&mut scope_tree, index, nr_children) {
                return Err(error);
            }
        }

        if index == 0 {
            break;
        }

        index -= 1;
    }

    match &scope_tree[0] {
        Either::Left(expr) => Ok(expr.clone()),
        _ => Err(String::from("ERROR")),
    }
}

fn create_scope_tree(tokens: Vec<Token<i32>>) -> Vec<Either<Expression<i32>, ScopeNode>> {
    let mut scope_tree: Vec<Either<Expression<i32>, ScopeNode>> = Vec::new();
    let mut current_node_idx: usize = 0;

    scope_tree.push(Either::Right(ScopeNode::InternalNode(0, 1, 0)));

    for token in tokens.into_iter() {
        match token {
            Token::BeginScope => {
                // Increment number of children
                match scope_tree[current_node_idx] {
                    Either::Right(ScopeNode::InternalNode(a, b, c)) => {
                        scope_tree[current_node_idx] =
                            Either::Right(ScopeNode::InternalNode(a, b, c + 1))
                    }
                    _ => (),
                }

                // Create new InternalNode representing the new scope
                scope_tree.push(Either::Right(ScopeNode::InternalNode(
                    current_node_idx,
                    scope_tree.len() + 1,
                    0,
                )));

                // Set the current node to the newly created node
                current_node_idx = scope_tree.len() - 1;
            }
            Token::EndScope => {
                // Set the current node to its parent
                if let Either::Right(ScopeNode::InternalNode(parent_idx, _, _)) =
                    scope_tree[current_node_idx]
                {
                    current_node_idx = parent_idx;
                }
            }
            _ => {
                // Add leaf as child of the node at current_node_idx
                scope_tree.push(Either::Right(ScopeNode::Leaf(token)));

                // Increment number of children
                match scope_tree[current_node_idx] {
                    Either::Right(ScopeNode::InternalNode(a, b, c)) => {
                        scope_tree[current_node_idx] =
                            Either::Right(ScopeNode::InternalNode(a, b, c + 1))
                    }
                    _ => (),
                }
            }
        }
    }

    scope_tree
}

fn parse_scope(
    scope_tree: &mut Vec<Either<Expression<i32>, ScopeNode>>,
    parent_index: usize,
    mut nr_children: usize,
) -> Result<(), String> {
    nr_children = parse_operations(
        scope_tree,
        parent_index,
        nr_children,
        Vec::from([Token::Multiplication, Token::Division]),
    )?;

    parse_operations(
        scope_tree,
        parent_index,
        nr_children,
        Vec::from([Token::Addition, Token::Subtraction]),
    )?;

    scope_tree.remove(parent_index);

    Ok(())
}

fn parse_operations(
    scope_tree: &mut Vec<Either<Expression<i32>, ScopeNode>>,
    parent_idx: usize,
    nr_children: usize,
    operations: Vec<Token<i32>>,
) -> Result<usize, String> {
    let children_idx = parent_idx + 1;
    let mut child_i = children_idx;
    let mut nr_children = nr_children;

    while child_i < children_idx + nr_children {
        if let Either::Right(ScopeNode::Leaf(Token::Number(n))) = scope_tree[child_i] {
            scope_tree[child_i] = Either::Left(Expression::Value(n));
        }

        child_i += 1;
    }

    child_i = children_idx;

    while child_i < children_idx + nr_children {
        match &scope_tree[child_i] {
            Either::Right(ScopeNode::Leaf(token)) => {
                if operations.contains(&token) {
                    let lhs = match &scope_tree[child_i - 1] {
                        Either::Left(expr) => expr.clone(),
                        _ => {
                            return Err(String::from("Expected lhs expression, but got scopenode"))
                        }
                    };

                    let rhs = match &scope_tree[child_i + 1] {
                        Either::Left(expr) => expr.clone(),
                        _ => {
                            return Err(String::from("Expected rhs expression, but got scopenode"))
                        }
                    };

                    let expr = match token {
                        Token::Multiplication => {
                            Expression::Multiplication(Box::new(lhs), Box::new(rhs))
                        }
                        Token::Division => Expression::Division(Box::new(lhs), Box::new(rhs)),
                        Token::Addition => Expression::Addition(Box::new(lhs), Box::new(rhs)),
                        Token::Subtraction => Expression::Subtraction(Box::new(lhs), Box::new(rhs)),
                        _ => return Err(String::from("Supplied token is not an operation")),
                    };

                    scope_tree.remove(child_i + 1);
                    scope_tree.remove(child_i - 1);

                    nr_children -= 2;

                    child_i -= 1;
                    scope_tree[child_i] = Either::Left(expr);

                    // Decrease number of children in the parent scopenode
                    if let Either::Right(ScopeNode::InternalNode(a, b, c)) = scope_tree[parent_idx]
                    {
                        scope_tree[parent_idx] =
                            Either::Right(ScopeNode::InternalNode(a, b, c - 2));
                    }
                }
            }
            _ => (),
        }

        child_i += 1;
    }

    Ok(nr_children)
}

fn evaluate_expression(expression: &Expression<i32>) -> Result<i32, String> {
    match expression {
        Expression::Value(value) => Ok(*value),
        Expression::Scope(a) => evaluate_expression(a),
        Expression::Addition(a, b) => {
            let a = evaluate_expression(a)?;
            let b = evaluate_expression(b)?;

            Ok(a + b)
        }
        Expression::Subtraction(a, b) => {
            let a = evaluate_expression(a)?;
            let b = evaluate_expression(b)?;

            Ok(a - b)
        }
        Expression::Multiplication(a, b) => {
            let a = evaluate_expression(a)?;
            let b = evaluate_expression(b)?;

            Ok(a * b)
        }
        Expression::Division(a, b) => {
            let a = evaluate_expression(a)?;
            let b = evaluate_expression(b)?;

            if b == 0 {
                Err(String::from("Division by zero"))
            } else {
                Ok(a / b)
            }
        }
    }
}
