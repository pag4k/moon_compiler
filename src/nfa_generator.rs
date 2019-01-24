use super::finite_accepter::*;
use super::language::*;

use std::collections::HashMap;
use std::collections::HashSet;

// fn add_final_state(
//     final_states: &mut HashSet<usize>,
//     tokens: &mut HashMap<usize, TokenType>,
//     state: usize,
//     token: TokenType,
// ) {
//     final_states.insert(state);
//     tokens.insert(state, token);
// }
//
// fn set_transition(
//     function: &mut HashMap<(usize, char), Vec<usize>>,
//     current_state: usize,
//     group: &[char],
//     next_states: Vec<usize>,
// ) {
//     //I wonder if there is a way to avoid copying the whole next states vector.
//     for &symbol in group {
//         function.insert((current_state, symbol), next_states.clone());
//     }
// }

fn add_reserved_words(
    function: &mut HashMap<(usize, char), Vec<usize>>,
    final_states: &mut HashSet<usize>,
    tokens: &mut HashMap<usize, TokenType>,
    backtrack: &mut HashSet<usize>,
    words: &[&'static str],
    state_id: usize,
) -> Vec<usize> {
    use Input::*;
    let mut current_state = state_id;
    let mut initial_states = Vec::new();
    let mut set_transition = |current_state: usize, input: Input, next_states: usize| {
        match input {
            Input::Char(char) => {
                function.insert((current_state, char), vec![next_states]);
            }
            Input::Array(array) => {
                for &char in array {
                    function.insert((current_state, char), vec![next_states]);
                }
            }
        };
    };
    let mut add_final_state = |state: usize, token: TokenType, add_backtrack: bool| {
        final_states.insert(state);
        tokens.insert(state, token);
        if add_backtrack {
            backtrack.insert(state);
        }
    };
    for word in words {
        initial_states.push(current_state);
        for letter in word.chars() {
            set_transition(current_state, Char(letter), current_state + 1);
            //function.insert((current_state, letter), vec![current_state + 1]);
            current_state += 1;
        }
        set_transition(current_state, Array(&SYMBOL), current_state + 1);
        set_transition(current_state, Array(&WHITESPACE), current_state + 1);
        current_state += 1;
        add_final_state(
            current_state,
            TokenType::Keyword(Keyword::from_str(word)),
            true,
        );
        //backtrack.insert(current_state);
        current_state += 1;
    }
    initial_states
}

enum Input {
    Char(char),
    Array(&'static [char]),
}

enum BacktrackEnum {
    Backtrack,
    NoBacktrack,
}

pub fn define_nfa_table() -> (
    NonDeterministicFiniteAccepter,
    HashMap<usize, TokenType>,
    HashSet<usize>,
) {
    use BacktrackEnum::*;
    use Comment::*;
    use Input::*;
    use LexicalError::*;
    use Operator::*;
    use Separator::*;

    let mut function: HashMap<(usize, char), Vec<usize>> = HashMap::new();
    let mut final_states: HashSet<usize> = HashSet::new();
    let mut tokens: HashMap<usize, TokenType> = HashMap::new();
    let mut backtrack: HashSet<usize> = HashSet::new();

    let mut initial_states = vec![
        100, 200, 300, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700,
        1800, 1900, 2000, 2100, 2200, 2300, 2400, 2500,
    ];

    //This needs to go before id to make sure it has priority.
    //There is probably a better way to do this.
    //ADD RESERVED WORDS
    let mut reserved_words_states = add_reserved_words(
        &mut function,
        &mut final_states,
        &mut tokens,
        &mut backtrack,
        &RESERVED_WORDS,
        1,
    );
    initial_states.append(&mut reserved_words_states);

    //Need to add epsilon closure
    let mut epsilon_closure: HashMap<usize, Vec<usize>> = HashMap::new();
    epsilon_closure.insert(0, initial_states);

    let mut set_transition = |current_state: usize, input: Input, next_states: usize| {
        match input {
            Input::Char(char) => {
                function.insert((current_state, char), vec![next_states]);
            }
            Input::Array(array) => {
                for &char in array {
                    function.insert((current_state, char), vec![next_states]);
                }
            }
        };
    };
    let mut add_final_state = |state: usize, token: TokenType, add_backtrack: BacktrackEnum| {
        final_states.insert(state);
        tokens.insert(state, token);
        if let Backtrack = add_backtrack {
            backtrack.insert(state);
        };
    };

    // fn add_final_state(
    //     final_states: &mut HashSet<usize>,
    //     tokens: &mut HashMap<usize, TokenType>,
    //     state: usize,
    //     token: TokenType,
    // ) {
    //     final_states.insert(state);
    //     tokens.insert(state, token);
    // }

    //IMPORTANT: Here, we assume that we did not go over 100 when adding keywords.
    //To avoid state number collision, jump by 100 for each regular expression

    //TODO: I'm not really hard coding the NFA.
    //In fact, I'm almost defining the DFA since I have to consider all cases.

    //id::=letter alphanum*
    set_transition(100, Array(&LETTER), 101);
    set_transition(101, Array(&SIGMA), 102);
    set_transition(101, Array(&DIGIT), 101);
    set_transition(101, Array(&LETTER), 101);
    set_transition(101, Char('_'), 101);
    //function.insert((101, '_'), 101);
    add_final_state(102, TokenType::Id, Backtrack);
    //backtrack.insert(102);

    //integer::=nonzero digit* |0
    set_transition(200, Array(&NONZERO), 201);
    set_transition(201, Array(&SIGMA), 202);
    set_transition(201, Char('.'), 400);
    //function.insert((201, '.'), 400);
    set_transition(201, Array(&DIGIT), 201);
    add_final_state(202, TokenType::Integer, Backtrack);
    //backtrack.insert(202);

    set_transition(300, Char('0'), 301);
    set_transition(301, Array(&SIGMA), 302);
    set_transition(301, Char('.'), 400);
    add_final_state(302, TokenType::Integer, Backtrack);
    //backtrack.insert(302);

    set_transition(400, Array(&SIGMA), 401);
    add_final_state(401, TokenType::LexicalError(IncompleteFloat), Backtrack);
    //backtrack.insert(401);
    set_transition(400, Char('0'), 400);
    set_transition(400, Array(&NONZERO), 402);
    set_transition(402, Array(&SIGMA), 403);
    set_transition(402, Char('0'), 400);
    //function.insert((402, '0'), 400);
    set_transition(402, Array(&NONZERO), 402);
    add_final_state(403, TokenType::Float, Backtrack);
    //backtrack.insert(403);
    set_transition(402, Char('e'), 404);
    //function.insert((402, 'e'), 404);
    set_transition(404, Array(&SIGMA), 405);
    add_final_state(405, TokenType::LexicalError(IncompleteFloat), Backtrack);
    //backtrack.insert(405);
    set_transition(404, Char('0'), 407);
    set_transition(404, Array(&NONZERO), 409);
    set_transition(404, Char('+'), 406);
    set_transition(404, Char('-'), 406);
    // function.insert((404, '0'), 407);
    // function.insert((404, '+'), 406);
    // function.insert((404, '-'), 406);
    set_transition(406, Array(&SIGMA), 408);
    add_final_state(408, TokenType::LexicalError(IncompleteFloat), Backtrack);
    //backtrack.insert(408);
    set_transition(406, Char('0'), 407);
    // function.insert((406, '0'), 407);
    add_final_state(407, TokenType::Float, NoBacktrack);
    set_transition(406, Array(&NONZERO), 409);
    set_transition(409, Array(&SIGMA), 410);
    set_transition(409, Array(&DIGIT), 409);
    add_final_state(410, TokenType::Float, Backtrack);
    //backtrack.insert(410);

    // /
    set_transition(500, Char('/'), 501);
    set_transition(501, Array(&SIGMA), 502);
    add_final_state(502, TokenType::Operator(Division), Backtrack);
    //backtrack.insert(502);
    //Line comment
    set_transition(501, Char('/'), 503);
    set_transition(503, Array(&SIGMA), 503);
    set_transition(503, Char('\n'), 504);
    add_final_state(504, TokenType::Comment(LineComment), Backtrack);
    //backtrack.insert(504);
    //Block comment
    set_transition(501, Char('*'), 505);
    set_transition(505, Array(&SIGMA), 505);
    set_transition(505, Char('*'), 506);
    set_transition(506, Array(&SIGMA), 505);
    set_transition(506, Char('*'), 506);
    set_transition(506, Char('/'), 507);
    add_final_state(507, TokenType::Comment(BlockComment), NoBacktrack);

    //OPERATORS
    // < <= <>
    set_transition(600, Char('<'), 601);
    set_transition(601, Array(&SIGMA), 602);
    add_final_state(602, TokenType::Operator(Smaller), Backtrack);
    //backtrack.insert(602);
    set_transition(601, Char('='), 603);
    add_final_state(603, TokenType::Operator(SmallerOrEqual), NoBacktrack);
    set_transition(601, Char('>'), 604);
    add_final_state(604, TokenType::Operator(NotEqual), NoBacktrack);

    // > >=
    set_transition(700, Char('>'), 701);
    set_transition(701, Array(&SIGMA), 702);
    add_final_state(702, TokenType::Operator(Greater), Backtrack);
    //backtrack.insert(702);
    set_transition(701, Char('='), 703);
    add_final_state(703, TokenType::Operator(GreaterOrEqual), NoBacktrack);

    // = ==
    set_transition(800, Char('='), 801);
    set_transition(801, Array(&SIGMA), 802);
    add_final_state(802, TokenType::Operator(Assignment), Backtrack);
    //backtrack.insert(802);
    set_transition(801, Char('='), 803);
    add_final_state(803, TokenType::Operator(Equal), NoBacktrack);

    // +
    set_transition(900, Char('+'), 901);
    add_final_state(901, TokenType::Operator(Addition), NoBacktrack);

    // -
    set_transition(1000, Char('-'), 1001);
    add_final_state(1001, TokenType::Operator(Substraction), NoBacktrack);

    // *
    set_transition(1100, Char('*'), 1101);
    add_final_state(1101, TokenType::Operator(Multiplication), NoBacktrack);

    // &&
    set_transition(1200, Char('&'), 1201);
    set_transition(1201, Array(&SIGMA), 1202);
    add_final_state(1202, TokenType::LexicalError(IncompleteAnd), Backtrack);
    set_transition(1201, Char('&'), 1203);
    add_final_state(1203, TokenType::Operator(And), NoBacktrack);

    // !
    set_transition(1300, Char('!'), 1301);
    add_final_state(1301, TokenType::Operator(Not), NoBacktrack);

    // &&
    set_transition(1400, Char('|'), 1401);
    set_transition(1401, Array(&SIGMA), 1402);
    add_final_state(1402, TokenType::LexicalError(IncompleteOr), Backtrack);
    set_transition(1401, Char('|'), 1403);
    add_final_state(1403, TokenType::Operator(Or), NoBacktrack);

    // ;
    set_transition(1500, Char(';'), 1501);
    add_final_state(1501, TokenType::Separator(SemiColon), NoBacktrack);

    // ,
    set_transition(1600, Char(','), 1601);
    add_final_state(1601, TokenType::Separator(Coma), NoBacktrack);

    // .
    set_transition(1700, Char('.'), 1701);
    add_final_state(1701, TokenType::Separator(Period), NoBacktrack);

    // : ::
    set_transition(1800, Char(':'), 1801);
    set_transition(1801, Array(&SIGMA), 1802);
    add_final_state(1802, TokenType::Separator(Colon), Backtrack);
    //backtrack.insert(1802);
    set_transition(1801, Char(':'), 1803);
    add_final_state(1803, TokenType::Separator(ScopeResolution), NoBacktrack);

    // (
    set_transition(1900, Char('('), 1901);
    add_final_state(1901, TokenType::Separator(LeftParenthesis), NoBacktrack);

    // )
    set_transition(2000, Char(')'), 2001);
    add_final_state(2001, TokenType::Separator(RightParenthesis), NoBacktrack);

    // {
    set_transition(2100, Char('{'), 2101);
    add_final_state(2101, TokenType::Separator(LeftCurlyBracket), NoBacktrack);

    // }
    set_transition(2200, Char('}'), 2201);
    add_final_state(2201, TokenType::Separator(RightCurlyBracket), NoBacktrack);

    // [
    set_transition(2300, Char('['), 2301);
    add_final_state(2301, TokenType::Separator(LeftSquareBracket), NoBacktrack);

    // ]
    set_transition(2400, Char(']'), 2401);
    add_final_state(2401, TokenType::Separator(RightSquareBracket), NoBacktrack);

    set_transition(2500, Char('_'), 2501);
    add_final_state(2501, TokenType::LexicalError(InvalidId), NoBacktrack);

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
