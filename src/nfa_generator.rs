use super::finite_accepter::*;
use super::language::*;

use std::collections::HashMap;
use std::collections::HashSet;

fn add_final_state(
    final_states: &mut HashSet<usize>,
    tokens: &mut HashMap<usize, TokenType>,
    state: usize,
    token: TokenType,
) {
    final_states.insert(state);
    tokens.insert(state, token);
}

fn add_group(
    function: &mut HashMap<(usize, char), Vec<usize>>,
    current_state: usize,
    group: &[char],
    next_states: Vec<usize>,
) {
    //I wonder if there is a way to avoid copying the whole next states vector.
    for &symbol in group {
        function.insert((current_state, symbol), next_states.clone());
    }
}

fn add_reserved_words(
    function: &mut HashMap<(usize, char), Vec<usize>>,
    final_states: &mut HashSet<usize>,
    tokens: &mut HashMap<usize, TokenType>,
    backtrack: &mut HashSet<usize>,
    words: &[&'static str],
    state_id: usize,
) -> Vec<usize> {
    let mut current_state = state_id;
    let mut initial_states = Vec::new();
    for word in words {
        initial_states.push(current_state);
        for letter in word.chars() {
            function.insert((current_state, letter), vec![current_state + 1]);
            current_state += 1;
        }
        add_group(function, current_state, &SYMBOL, vec![current_state + 1]);
        add_group(
            function,
            current_state,
            &WHITESPACE,
            vec![current_state + 1],
        );
        current_state += 1;
        add_final_state(
            final_states,
            tokens,
            current_state,
            TokenType::Keyword(Keyword::from_str(word)),
        );
        backtrack.insert(current_state);
        current_state += 1;
    }
    initial_states
}

pub fn define_nfa_table() -> (
    NonDeterministicFiniteAccepter,
    HashMap<usize, TokenType>,
    HashSet<usize>,
) {
    let mut function: HashMap<(usize, char), Vec<usize>> = HashMap::new();
    let mut final_states: HashSet<usize> = HashSet::new();
    let mut tokens: HashMap<usize, TokenType> = HashMap::new();
    let mut backtrack: HashSet<usize> = HashSet::new();

    let mut initial_states = vec![
        100, 200, 300, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700,
        1800, 1900, 2000, 2100, 2200, 2300, 2400,
    ];

    //This needs to go before id to make sure it has priority.
    //There is probably a better way to do this.
    //Start at 2, after the error state: 1
    let mut reserved_words_states = add_reserved_words(
        &mut function,
        &mut final_states,
        &mut tokens,
        &mut backtrack,
        &RESERVED_WORDS,
        2,
    );
    initial_states.append(&mut reserved_words_states);

    //Need to add epsilon closure
    let mut epsilon_closure: HashMap<usize, Vec<usize>> = HashMap::new();
    epsilon_closure.insert(0, initial_states);

    //Invalid token
    add_final_state(&mut final_states, &mut tokens, 1, TokenType::InvalidToken);
    backtrack.insert(1);

    //IMPORTANT: Here, we assume that we did not go over 100 when adding keywords.
    //To avoid state number collision, jump by 100 for each regular expression

    //id::=letter alphanum*
    add_group(&mut function, 100, &LETTER, vec![101]);
    add_group(&mut function, 101, &SIGMA, vec![102]);
    add_group(&mut function, 101, &DIGIT, vec![101]);
    add_group(&mut function, 101, &LETTER, vec![101]);
    // add_group(&mut function, 101, &SYMBOL, vec![102]);
    // add_group(&mut function, 101, &WHITESPACE, vec![102]);
    add_final_state(&mut final_states, &mut tokens, 102, TokenType::Id);
    //final_states.insert(102, ID);
    backtrack.insert(102);

    //integer::=nonzero digit* |0
    add_group(&mut function, 200, &NONZERO, vec![201]);
    add_group(&mut function, 201, &DIGIT, vec![201]);
    add_group(&mut function, 201, &LETTER, vec![202]);
    add_group(&mut function, 201, &SYMBOL, vec![202]);
    add_group(&mut function, 201, &WHITESPACE, vec![202]);
    add_final_state(&mut final_states, &mut tokens, 202, TokenType::Integer);
    //final_states.insert(202, INTEGER);
    backtrack.insert(202);

    function.insert((300, '0'), vec![301]);
    add_final_state(&mut final_states, &mut tokens, 301, TokenType::Integer);
    //final_states.insert(301, INTEGER);

    // /
    function.insert((500, '/'), vec![501]);
    add_group(&mut function, 501, &SIGMA, vec![502]);
    add_final_state(
        &mut final_states,
        &mut tokens,
        502,
        TokenType::Operator(Operator::Division),
    );
    backtrack.insert(502);
    //Line comment
    function.insert((501, '/'), vec![503]);
    add_group(&mut function, 503, &SIGMA, vec![503]);
    function.insert((503, '\n'), vec![504]);
    add_final_state(
        &mut final_states,
        &mut tokens,
        504,
        TokenType::Comment(Comment::LineComment),
    );
    //final_states.insert(503, LINE_COMMENT);
    backtrack.insert(504);
    //Block comment
    function.insert((501, '*'), vec![505]);
    add_group(&mut function, 505, &SIGMA, vec![505]);
    function.insert((505, '*'), vec![506]);
    add_group(&mut function, 506, &SIGMA, vec![505]);
    function.insert((506, '*'), vec![506]);
    function.insert((506, '/'), vec![507]);
    add_final_state(
        &mut final_states,
        &mut tokens,
        507,
        TokenType::Comment(Comment::BlockComment),
    );

    //OPERATORS
    // < ><= <>
    function.insert((600, '<'), vec![601]);
    add_group(&mut function, 601, &SIGMA, vec![602]);
    add_final_state(
        &mut final_states,
        &mut tokens,
        602,
        TokenType::Operator(Operator::Smaller),
    );
    //final_states.insert(602, SMALLER);
    backtrack.insert(602);
    function.insert((601, '='), vec![603]);
    add_final_state(
        &mut final_states,
        &mut tokens,
        603,
        TokenType::Operator(Operator::SmallerOrEqual),
    );
    //final_states.insert(603, SMALLER_OR_EQUAL);
    function.insert((601, '>'), vec![604]);
    add_final_state(
        &mut final_states,
        &mut tokens,
        604,
        TokenType::Operator(Operator::NotEqual),
    );
    //final_states.insert(604, NOT_EQUAL);

    // > >=
    function.insert((700, '>'), vec![701]);
    add_group(&mut function, 701, &SIGMA, vec![702]);
    add_final_state(
        &mut final_states,
        &mut tokens,
        702,
        TokenType::Operator(Operator::Greater),
    );
    //final_states.insert(702, GREATER);
    backtrack.insert(702);
    function.insert((701, '='), vec![703]);
    add_final_state(
        &mut final_states,
        &mut tokens,
        703,
        TokenType::Operator(Operator::GreaterOrEqual),
    );
    //final_states.insert(703, GREATER_OR_EQUAL);

    // = ==
    function.insert((800, '='), vec![801]);
    add_group(&mut function, 801, &SIGMA, vec![802]);
    add_final_state(
        &mut final_states,
        &mut tokens,
        802,
        TokenType::Operator(Operator::Assignment),
    );
    //final_states.insert(802, ASSIGNMENT);
    backtrack.insert(802);
    function.insert((801, '='), vec![803]);
    add_final_state(
        &mut final_states,
        &mut tokens,
        803,
        TokenType::Operator(Operator::Equal),
    );
    //final_states.insert(803, EQUAL);

    // +
    function.insert((900, '+'), vec![901]);
    add_final_state(
        &mut final_states,
        &mut tokens,
        901,
        TokenType::Operator(Operator::Addition),
    );

    // -
    function.insert((1000, '-'), vec![1001]);
    add_final_state(
        &mut final_states,
        &mut tokens,
        1001,
        TokenType::Operator(Operator::Substraction),
    );

    // *
    function.insert((1100, '*'), vec![1101]);
    add_final_state(
        &mut final_states,
        &mut tokens,
        1101,
        TokenType::Operator(Operator::Multiplication),
    );

    // &&
    //FIXME: Need some way to deal with just & and send an error
    function.insert((1200, '&'), vec![1201]);
    add_group(&mut function, 1201, &SIGMA, vec![1]);
    function.insert((1201, '&'), vec![1202]);
    add_final_state(
        &mut final_states,
        &mut tokens,
        1202,
        TokenType::Operator(Operator::And),
    );

    // !
    function.insert((1300, '!'), vec![1301]);
    add_final_state(
        &mut final_states,
        &mut tokens,
        1301,
        TokenType::Operator(Operator::Not),
    );

    // &&
    //FIXME: Need some way to deal with just | and send an error
    function.insert((1400, '|'), vec![1401]);
    add_group(&mut function, 1401, &SIGMA, vec![1]);
    function.insert((1401, '|'), vec![1402]);
    add_final_state(
        &mut final_states,
        &mut tokens,
        1402,
        TokenType::Operator(Operator::Or),
    );

    // ;
    function.insert((1500, ';'), vec![1501]);
    add_final_state(
        &mut final_states,
        &mut tokens,
        1501,
        TokenType::Separator(Separator::SemiColon),
    );

    // ,
    function.insert((1600, ','), vec![1601]);
    add_final_state(
        &mut final_states,
        &mut tokens,
        1601,
        TokenType::Separator(Separator::Coma),
    );

    // .
    function.insert((1700, '.'), vec![1701]);
    add_final_state(
        &mut final_states,
        &mut tokens,
        1701,
        TokenType::Separator(Separator::Period),
    );

    // : ::
    function.insert((1800, ':'), vec![1801]);
    add_group(&mut function, 1801, &SIGMA, vec![1802]);
    add_final_state(
        &mut final_states,
        &mut tokens,
        1802,
        TokenType::Separator(Separator::Colon),
    );
    backtrack.insert(1802);
    function.insert((1801, ':'), vec![1803]);
    add_final_state(
        &mut final_states,
        &mut tokens,
        1803,
        TokenType::Separator(Separator::ScopeResolution),
    );

    // (
    function.insert((1900, '('), vec![1901]);
    add_final_state(
        &mut final_states,
        &mut tokens,
        1901,
        TokenType::Separator(Separator::LeftParenthesis),
    );

    // )
    function.insert((2000, ')'), vec![2001]);
    add_final_state(
        &mut final_states,
        &mut tokens,
        2001,
        TokenType::Separator(Separator::RightParenthesis),
    );

    // {
    function.insert((2100, '{'), vec![2101]);
    add_final_state(
        &mut final_states,
        &mut tokens,
        2101,
        TokenType::Separator(Separator::LeftCurlyBracket),
    );

    // }
    function.insert((2200, '}'), vec![2201]);
    add_final_state(
        &mut final_states,
        &mut tokens,
        2201,
        TokenType::Separator(Separator::RightCurlyBracket),
    );

    // [
    function.insert((2300, '['), vec![2301]);
    add_final_state(
        &mut final_states,
        &mut tokens,
        2301,
        TokenType::Separator(Separator::LeftSquareBracket),
    );

    // ]
    function.insert((2400, ']'), vec![2401]);
    add_final_state(
        &mut final_states,
        &mut tokens,
        2401,
        TokenType::Separator(Separator::RightSquareBracket),
    );

    let states: HashSet<usize> = epsilon_closure
        .keys()
        .cloned()
        .chain(function.keys().map(|&(key, _)| key))
        .collect();

    (
        NonDeterministicFiniteAccepter {
            states,
            alphabet: HashSet::new(),
            function,
            epsilon_closure,
            initial_state: 0,
            final_states,
        },
        tokens,
        backtrack,
    )
}
