use crate::dot_generator::*;
use crate::finite_accepter::*;
use crate::language::*;
use crate::lexical_error::*;

use std::collections::{HashMap, HashSet};

/// Input enum to describe the two types of transitions.
enum Input {
    Char(char),
    Array(&'static [char]),
}

/// BacktrackEnum to make the code more explicit.
enum BacktrackEnum {
    Backtrack,
    NoBacktrack,
}

impl NonDeterministicFiniteAccepter {
    /// Set NFA transitions.
    fn set_transition(
        &mut self,
        transtions: &mut HashMap<(usize, char), usize>,
        current_state: usize,
        input: Input,
        next_states: usize,
    ) {
        match input {
            // If the input is only one character, set it.
            Input::Char(char) => {
                if char == '\n' {
                    transtions.insert((current_state, 'R'), next_states);
                } else {
                    transtions.insert((current_state, char), next_states);
                }
                self.function
                    .insert((current_state, char), vec![next_states]);
            }
            // If the input is a slice of char, iterate over them to set them.
            Input::Array(array) => {
                transtions.insert((current_state, array[0]), next_states);
                // Skip the first char.
                for &char in array.iter().skip(1) {
                    self.function
                        .insert((current_state, char), vec![next_states]);
                }
            }
        };
    }
    // Add a final state with its corresponding token and backtrack.
    fn add_final_state(
        &mut self,
        tokens: &mut HashMap<usize, TokenType>,
        backtrack: &mut HashSet<usize>,
        state: usize,
        token: TokenType,
        add_backtrack: BacktrackEnum,
    ) {
        use BacktrackEnum::*;
        self.final_states.insert(state);
        tokens.insert(state, token);
        if let Backtrack = add_backtrack {
            backtrack.insert(state);
        };
    }
}

/// Returns a NFA and the tokens and backtack corresponding to its final state.
pub fn define_nfa_table() -> (
    NonDeterministicFiniteAccepter,
    HashMap<usize, TokenType>,
    HashSet<usize>,
) {
    use BacktrackEnum::*;
    use CommentType::*;
    use Input::*;
    use LexicalError::*;
    use OperatorType::*;
    use SeparatorType::*;

    let mut nfa = NonDeterministicFiniteAccepter {
        states: HashSet::new(),
        alphabet: HashSet::new(),
        function: HashMap::new(),
        epsilon_closure: HashMap::new(),
        initial_state: 0,
        final_states: HashSet::new(),
    };
    let mut tokens: HashMap<usize, TokenType> = HashMap::new();
    let mut backtrack: HashSet<usize> = HashSet::new();
    // The transitions map is only used for the generation of DOT files.
    let mut transtions: HashMap<(usize, char), usize> = HashMap::new();

    let mut initial_states = Vec::new();

    // TokenType::Id
    initial_states.push(100);
    nfa.set_transition(&mut transtions, 100, Array(&LETTER), 101);
    nfa.set_transition(&mut transtions, 101, Array(&SIGMA), 102);
    nfa.set_transition(&mut transtions, 101, Array(&DIGIT), 101);
    nfa.set_transition(&mut transtions, 101, Array(&LETTER), 101);
    nfa.set_transition(&mut transtions, 101, Char('_'), 101);
    nfa.add_final_state(&mut tokens, &mut backtrack, 102, TokenType::Id, Backtrack);
    DotGraph::generate(
        &nfa,
        &transtions,
        &tokens,
        &backtrack,
        100..=102,
        "nfa/id.gv",
    );

    // TokenType::Integer (non zero)
    initial_states.push(200);
    nfa.set_transition(&mut transtions, 200, Array(&NONZERO), 201);
    nfa.set_transition(&mut transtions, 201, Array(&SIGMA), 202);
    nfa.set_transition(&mut transtions, 201, Char('.'), 400);
    nfa.set_transition(&mut transtions, 201, Array(&DIGIT), 201);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        202,
        TokenType::IntNum,
        Backtrack,
    );
    DotGraph::generate(
        &nfa,
        &transtions,
        &tokens,
        &backtrack,
        200..=202,
        "nfa/nonzero.gv",
    );

    // TokenType::Integer (zero)
    initial_states.push(300);
    nfa.set_transition(&mut transtions, 300, Char('0'), 301);
    nfa.set_transition(&mut transtions, 301, Array(&SIGMA), 302);
    nfa.set_transition(&mut transtions, 301, Char('.'), 400);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        302,
        TokenType::IntNum,
        Backtrack,
    );
    DotGraph::generate(
        &nfa,
        &transtions,
        &tokens,
        &backtrack,
        300..=302,
        "nfa/zero.gv",
    );

    // TokenType::Float
    //initial_states.push(400);
    nfa.set_transition(&mut transtions, 400, Array(&SIGMA), 401);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        401,
        TokenType::LexicalError(FloatMissingFraction),
        Backtrack,
    );
    nfa.set_transition(&mut transtions, 400, Char('0'), 402);
    nfa.set_transition(&mut transtions, 400, Array(&NONZERO), 404);
    nfa.set_transition(&mut transtions, 402, Array(&SIGMA), 403);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        403,
        TokenType::FloatNum,
        Backtrack,
    );
    nfa.set_transition(&mut transtions, 402, Array(&NONZERO), 404);
    nfa.set_transition(&mut transtions, 402, Char('0'), 406);
    nfa.set_transition(&mut transtions, 404, Array(&SIGMA), 405);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        405,
        TokenType::FloatNum,
        Backtrack,
    );
    nfa.set_transition(&mut transtions, 404, Array(&NONZERO), 404);
    nfa.set_transition(&mut transtions, 404, Char('0'), 406);
    nfa.set_transition(&mut transtions, 404, Char('e'), 408);
    nfa.set_transition(&mut transtions, 406, Array(&SIGMA), 407);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        407,
        TokenType::LexicalError(FloatTrailingZeros),
        Backtrack,
    );
    nfa.set_transition(&mut transtions, 406, Array(&NONZERO), 404);
    nfa.set_transition(&mut transtions, 406, Char('0'), 406);
    nfa.set_transition(&mut transtions, 408, Array(&SIGMA), 409);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        409,
        TokenType::LexicalError(FloatMissingExponent),
        Backtrack,
    );
    nfa.set_transition(&mut transtions, 408, Char('0'), 410);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        410,
        TokenType::FloatNum,
        NoBacktrack,
    );
    nfa.set_transition(&mut transtions, 408, Char('+'), 412);
    nfa.set_transition(&mut transtions, 408, Char('-'), 412);
    nfa.set_transition(&mut transtions, 408, Array(&NONZERO), 411);
    nfa.set_transition(&mut transtions, 411, Array(&SIGMA), 413);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        413,
        TokenType::FloatNum,
        Backtrack,
    );
    nfa.set_transition(&mut transtions, 411, Array(&DIGIT), 411);
    nfa.set_transition(&mut transtions, 412, Array(&SIGMA), 409);
    nfa.set_transition(&mut transtions, 412, Char('0'), 410);
    nfa.set_transition(&mut transtions, 412, Array(&NONZERO), 411);
    DotGraph::generate(
        &nfa,
        &transtions,
        &tokens,
        &backtrack,
        400..=413,
        "nfa/float.gv",
    );

    // TokenType::Operator(Division)
    initial_states.push(500);
    nfa.set_transition(&mut transtions, 500, Char('/'), 501);
    nfa.set_transition(&mut transtions, 501, Array(&SIGMA), 502);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        502,
        TokenType::Operator(Division),
        Backtrack,
    );
    // TokenType::Comment(LineComment)
    nfa.set_transition(&mut transtions, 501, Char('/'), 503);
    nfa.set_transition(&mut transtions, 503, Array(&SIGMA), 503);
    nfa.set_transition(&mut transtions, 503, Char('\n'), 504);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        504,
        TokenType::Comment(LineComment),
        Backtrack,
    );
    // TokenType::Comment(BlockComment)
    nfa.set_transition(&mut transtions, 501, Char('*'), 505);
    nfa.set_transition(&mut transtions, 505, Array(&SIGMA), 505);
    nfa.set_transition(&mut transtions, 505, Char('*'), 506);
    nfa.set_transition(&mut transtions, 506, Array(&SIGMA), 505);
    nfa.set_transition(&mut transtions, 506, Char('*'), 506);
    nfa.set_transition(&mut transtions, 506, Char('/'), 507);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        507,
        TokenType::Comment(BlockComment),
        NoBacktrack,
    );
    DotGraph::generate(
        &nfa,
        &transtions,
        &tokens,
        &backtrack,
        500..=507,
        "nfa/slash.gv",
    );

    // TokenType::Operator(Smaller)
    initial_states.push(600);
    nfa.set_transition(&mut transtions, 600, Char('<'), 601);
    nfa.set_transition(&mut transtions, 601, Array(&SIGMA), 602);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        602,
        TokenType::Operator(LT),
        Backtrack,
    );
    // TokenType::Operator(SmallerOrEqual)
    nfa.set_transition(&mut transtions, 601, Char('='), 603);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        603,
        TokenType::Operator(LEq),
        NoBacktrack,
    );
    // TokenType::Operator(NotEqual)
    nfa.set_transition(&mut transtions, 601, Char('>'), 604);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        604,
        TokenType::Operator(NEq),
        NoBacktrack,
    );
    DotGraph::generate(
        &nfa,
        &transtions,
        &tokens,
        &backtrack,
        600..=604,
        "nfa/smaller.gv",
    );

    // TokenType::Operator(Greater)
    initial_states.push(700);
    nfa.set_transition(&mut transtions, 700, Char('>'), 701);
    nfa.set_transition(&mut transtions, 701, Array(&SIGMA), 702);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        702,
        TokenType::Operator(GT),
        Backtrack,
    );
    // TokenType::Operator(GreaterOrEqual)
    nfa.set_transition(&mut transtions, 701, Char('='), 703);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        703,
        TokenType::Operator(GEq),
        NoBacktrack,
    );
    DotGraph::generate(
        &nfa,
        &transtions,
        &tokens,
        &backtrack,
        700..=703,
        "nfa/greater.gv",
    );

    // TokenType::Operator(Assignment)
    initial_states.push(800);
    nfa.set_transition(&mut transtions, 800, Char('='), 801);
    nfa.set_transition(&mut transtions, 801, Array(&SIGMA), 802);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        802,
        TokenType::Operator(Assignment),
        Backtrack,
    );
    // TokenType::Operator(Equal)
    nfa.set_transition(&mut transtions, 801, Char('='), 803);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        803,
        TokenType::Operator(Eq),
        NoBacktrack,
    );
    DotGraph::generate(
        &nfa,
        &transtions,
        &tokens,
        &backtrack,
        800..=803,
        "nfa/equal.gv",
    );

    // TokenType::Operator(Addition)
    initial_states.push(900);
    nfa.set_transition(&mut transtions, 900, Char('+'), 901);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        901,
        TokenType::Operator(Addition),
        NoBacktrack,
    );
    DotGraph::generate(
        &nfa,
        &transtions,
        &tokens,
        &backtrack,
        900..=901,
        "nfa/addition.gv",
    );

    // TokenType::Operator(Subtraction)
    initial_states.push(1000);
    nfa.set_transition(&mut transtions, 1000, Char('-'), 1001);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        1001,
        TokenType::Operator(Subtraction),
        NoBacktrack,
    );
    DotGraph::generate(
        &nfa,
        &transtions,
        &tokens,
        &backtrack,
        1000..=1001,
        "nfa/substraction.gv",
    );

    // TokenType::Operator(Multiplication)
    initial_states.push(1100);
    nfa.set_transition(&mut transtions, 1100, Char('*'), 1101);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        1101,
        TokenType::Operator(Multiplication),
        NoBacktrack,
    );
    DotGraph::generate(
        &nfa,
        &transtions,
        &tokens,
        &backtrack,
        1100..=1101,
        "nfa/multiplication.gv",
    );

    // TokenType::Operator(And)
    initial_states.push(1200);
    nfa.set_transition(&mut transtions, 1200, Char('&'), 1201);
    nfa.set_transition(&mut transtions, 1201, Array(&SIGMA), 1202);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        1202,
        TokenType::LexicalError(IncompleteAnd),
        Backtrack,
    );
    nfa.set_transition(&mut transtions, 1201, Char('&'), 1203);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        1203,
        TokenType::Operator(And),
        NoBacktrack,
    );
    DotGraph::generate(
        &nfa,
        &transtions,
        &tokens,
        &backtrack,
        1200..=1203,
        "nfa/ampersand.gv",
    );

    // TokenType::Operator(Not)
    initial_states.push(1300);
    nfa.set_transition(&mut transtions, 1300, Char('!'), 1301);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        1301,
        TokenType::Operator(Not),
        NoBacktrack,
    );
    DotGraph::generate(
        &nfa,
        &transtions,
        &tokens,
        &backtrack,
        1300..=1301,
        "nfa/not.gv",
    );

    // TokenType::Operator(Or)
    initial_states.push(1400);
    nfa.set_transition(&mut transtions, 1400, Char('|'), 1401);
    nfa.set_transition(&mut transtions, 1401, Array(&SIGMA), 1402);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        1402,
        TokenType::LexicalError(IncompleteOr),
        Backtrack,
    );
    nfa.set_transition(&mut transtions, 1401, Char('|'), 1403);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        1403,
        TokenType::Operator(Or),
        NoBacktrack,
    );
    DotGraph::generate(
        &nfa,
        &transtions,
        &tokens,
        &backtrack,
        1400..=1403,
        "nfa/pipe.gv",
    );

    // TokenType::Separator(SemiColon)
    initial_states.push(1500);
    nfa.set_transition(&mut transtions, 1500, Char(';'), 1501);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        1501,
        TokenType::Separator(SemiColon),
        NoBacktrack,
    );
    DotGraph::generate(
        &nfa,
        &transtions,
        &tokens,
        &backtrack,
        1500..=1501,
        "nfa/semicolon.gv",
    );

    // TokenType::Separator(Coma)
    initial_states.push(1600);
    nfa.set_transition(&mut transtions, 1600, Char(','), 1601);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        1601,
        TokenType::Separator(Coma),
        NoBacktrack,
    );
    DotGraph::generate(
        &nfa,
        &transtions,
        &tokens,
        &backtrack,
        1600..=1601,
        "nfa/coma.gv",
    );

    // TokenType::Separator(Period)
    initial_states.push(1700);
    nfa.set_transition(&mut transtions, 1700, Char('.'), 1701);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        1701,
        TokenType::Separator(Period),
        NoBacktrack,
    );
    DotGraph::generate(
        &nfa,
        &transtions,
        &tokens,
        &backtrack,
        1700..=1701,
        "nfa/period.gv",
    );

    // TokenType::Separator(Colon)
    initial_states.push(1800);
    nfa.set_transition(&mut transtions, 1800, Char(':'), 1801);
    nfa.set_transition(&mut transtions, 1801, Array(&SIGMA), 1802);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        1802,
        TokenType::Separator(Colon),
        Backtrack,
    );
    // TokenType::Separator(ScopeResolution)
    nfa.set_transition(&mut transtions, 1801, Char(':'), 1803);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        1803,
        TokenType::Operator(SR),
        NoBacktrack,
    );
    DotGraph::generate(
        &nfa,
        &transtions,
        &tokens,
        &backtrack,
        1800..=1803,
        "nfa/colon.gv",
    );

    // TokenType::Separator(LeftParenthesis)
    initial_states.push(1900);
    nfa.set_transition(&mut transtions, 1900, Char('('), 1901);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        1901,
        TokenType::Separator(LeftParenthesis),
        NoBacktrack,
    );
    DotGraph::generate(
        &nfa,
        &transtions,
        &tokens,
        &backtrack,
        1900..=1901,
        "nfa/leftparenthesis.gv",
    );

    // TokenType::Separator(RightParenthesis)
    initial_states.push(2000);
    nfa.set_transition(&mut transtions, 2000, Char(')'), 2001);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        2001,
        TokenType::Separator(RightParenthesis),
        NoBacktrack,
    );
    DotGraph::generate(
        &nfa,
        &transtions,
        &tokens,
        &backtrack,
        2000..=2001,
        "nfa/rightparenthesis.gv",
    );

    // TokenType::Separator(LeftCurlyBracket)
    initial_states.push(2100);
    nfa.set_transition(&mut transtions, 2100, Char('{'), 2101);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        2101,
        TokenType::Separator(LeftCurlyBracket),
        NoBacktrack,
    );
    DotGraph::generate(
        &nfa,
        &transtions,
        &tokens,
        &backtrack,
        2100..=2101,
        "nfa/leftcurlybracket.gv",
    );

    // TokenType::Separator(RightCurlyBracket)
    initial_states.push(2200);
    nfa.set_transition(&mut transtions, 2200, Char('}'), 2201);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        2201,
        TokenType::Separator(RightCurlyBracket),
        NoBacktrack,
    );
    DotGraph::generate(
        &nfa,
        &transtions,
        &tokens,
        &backtrack,
        2200..=2201,
        "nfa/rightcurlybracket.gv",
    );

    // TokenType::Separator(LeftSquareBracket)
    initial_states.push(2300);
    nfa.set_transition(&mut transtions, 2300, Char('['), 2301);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        2301,
        TokenType::Separator(LeftSquareBracket),
        NoBacktrack,
    );
    DotGraph::generate(
        &nfa,
        &transtions,
        &tokens,
        &backtrack,
        2300..=2301,
        "nfa/leftsquarebracket.gv",
    );

    // TokenType::Separator(RightSquareBracket)
    initial_states.push(2400);
    nfa.set_transition(&mut transtions, 2400, Char(']'), 2401);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        2401,
        TokenType::Separator(RightSquareBracket),
        NoBacktrack,
    );
    DotGraph::generate(
        &nfa,
        &transtions,
        &tokens,
        &backtrack,
        2400..=2401,
        "nfa/rightsquarebracket.gv",
    );

    // TokenType::LexicalError(InvalidId)
    initial_states.push(2500);
    nfa.set_transition(&mut transtions, 2500, Char('_'), 2501);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        2501,
        TokenType::LexicalError(InvalidId),
        NoBacktrack,
    );
    DotGraph::generate(
        &nfa,
        &transtions,
        &tokens,
        &backtrack,
        2500..=2501,
        "nfa/invalidid.gv",
    );

    // Add all of the first state of all the DFA to the espilon closure.
    nfa.epsilon_closure.insert(0, initial_states);

    nfa.states = nfa
        .epsilon_closure
        .keys()
        .cloned()
        .chain(nfa.function.keys().map(|&(key, _)| key))
        .collect();

    (nfa, tokens, backtrack)
}
