use super::dot_generator::*;
use super::finite_accepter::*;
use super::language::*;

use std::collections::HashMap;
use std::collections::HashSet;

impl NonDeterministicFiniteAccepter {
    fn set_transition(
        &mut self,
        transtions: &mut HashMap<(usize, char), usize>,
        current_state: usize,
        input: Input,
        next_states: usize,
    ) {
        match input {
            Input::Char(char) => {
                transtions.insert((current_state, char), next_states);
                self.function
                    .insert((current_state, char), vec![next_states]);
            }
            Input::Array(array) => {
                transtions.insert((current_state, array[0]), next_states);
                for &char in array.iter().skip(1) {
                    self.function
                        .insert((current_state, char), vec![next_states]);
                }
            }
        };
    }
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

    fn add_reserved_words(
        &mut self,
        transtions: &mut HashMap<(usize, char), usize>,
        tokens: &mut HashMap<usize, TokenType>,
        backtrack: &mut HashSet<usize>,
        words: &[&'static str],
        state_id: usize,
    ) -> Vec<usize> {
        use BacktrackEnum::*;
        use Input::*;

        let mut current_state = state_id;
        let mut initial_states = Vec::new();

        for word in words {
            initial_states.push(current_state);
            for letter in word.chars() {
                self.set_transition(transtions, current_state, Char(letter), current_state + 1);
                current_state += 1;
            }
            self.set_transition(transtions, current_state, Array(&SYMBOL), current_state + 1);
            self.set_transition(
                transtions,
                current_state,
                Array(&WHITESPACE),
                current_state + 1,
            );
            current_state += 1;
            self.add_final_state(
                tokens,
                backtrack,
                current_state,
                TokenType::Keyword(Keyword::from_str(word)),
                Backtrack,
            );
            current_state += 1;
        }
        initial_states
    }
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

    let mut nfa = NonDeterministicFiniteAccepter {
        states: HashSet::new(),
        alphabet: HashSet::new(),
        function: HashMap::new(),
        epsilon_closure: HashMap::new(),
        initial_state: 0,
        final_states: HashSet::new(),
    };

    let mut transtions: HashMap<(usize, char), usize> = HashMap::new();
    let mut tokens: HashMap<usize, TokenType> = HashMap::new();
    let mut backtrack: HashSet<usize> = HashSet::new();

    let mut initial_states = vec![
        100, 200, 300, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700,
        1800, 1900, 2000, 2100, 2200, 2300, 2400, 2500,
    ];

    //This needs to go before id to make sure it has priority.
    //There is probably a better way to do this.
    //ADD RESERVED WORDS
    let mut reserved_words_states = nfa.add_reserved_words(
        &mut transtions,
        &mut tokens,
        &mut backtrack,
        &RESERVED_WORDS,
        1,
    );
    initial_states.append(&mut reserved_words_states);

    //Need to add epsilon closure
    //let mut epsilon_closure: HashMap<usize, Vec<usize>> = HashMap::new();
    nfa.epsilon_closure.insert(0, initial_states);

    //IMPORTANT: Here, we assume that we did not go over 100 when adding keywords.
    //To avoid state number collision, jump by 100 for each regular expression

    //TODO: I'm not really hard coding the NFA.
    //In fact, I'm almost defining the DFA since I have to consider all cases.

    //id::=letter alphanum*
    nfa.set_transition(&mut transtions, 100, Array(&LETTER), 101);
    nfa.set_transition(&mut transtions, 101, Array(&SIGMA), 102);
    nfa.set_transition(&mut transtions, 101, Array(&DIGIT), 101);
    nfa.set_transition(&mut transtions, 101, Array(&LETTER), 101);
    nfa.set_transition(&mut transtions, 101, Char('_'), 101);
    nfa.add_final_state(&mut tokens, &mut backtrack, 102, TokenType::Id, Backtrack);

    //integer::=nonzero digit* |0
    nfa.set_transition(&mut transtions, 200, Array(&NONZERO), 201);
    nfa.set_transition(&mut transtions, 201, Array(&SIGMA), 202);
    nfa.set_transition(&mut transtions, 201, Char('.'), 400);
    nfa.set_transition(&mut transtions, 201, Array(&DIGIT), 201);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        202,
        TokenType::Integer,
        Backtrack,
    );

    nfa.set_transition(&mut transtions, 300, Char('0'), 301);
    nfa.set_transition(&mut transtions, 301, Array(&SIGMA), 302);
    nfa.set_transition(&mut transtions, 301, Char('.'), 400);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        302,
        TokenType::Integer,
        Backtrack,
    );

    DotGraph::generate(
        &nfa,
        &transtions,
        &tokens,
        &backtrack,
        300..=302,
        "dfa/zero.gv",
    );

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
        TokenType::Float,
        Backtrack,
    );
    nfa.set_transition(&mut transtions, 402, Array(&NONZERO), 404);
    nfa.set_transition(&mut transtions, 402, Char('0'), 406);

    nfa.set_transition(&mut transtions, 404, Array(&SIGMA), 405);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        405,
        TokenType::Float,
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
        TokenType::Float,
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
        TokenType::Float,
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
        "dfa/float.gv",
    );

    // /
    nfa.set_transition(&mut transtions, 500, Char('/'), 501);
    nfa.set_transition(&mut transtions, 501, Array(&SIGMA), 502);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        502,
        TokenType::Operator(Division),
        Backtrack,
    );
    //Line comment
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
    //Block comment
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

    //OPERATORS
    // < <= <>
    nfa.set_transition(&mut transtions, 600, Char('<'), 601);
    nfa.set_transition(&mut transtions, 601, Array(&SIGMA), 602);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        602,
        TokenType::Operator(Smaller),
        Backtrack,
    );
    nfa.set_transition(&mut transtions, 601, Char('='), 603);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        603,
        TokenType::Operator(SmallerOrEqual),
        NoBacktrack,
    );
    nfa.set_transition(&mut transtions, 601, Char('>'), 604);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        604,
        TokenType::Operator(NotEqual),
        NoBacktrack,
    );

    DotGraph::generate(
        &nfa,
        &transtions,
        &tokens,
        &backtrack,
        600..=604,
        "dfa/smaller.gv",
    );

    // > >=
    nfa.set_transition(&mut transtions, 700, Char('>'), 701);
    nfa.set_transition(&mut transtions, 701, Array(&SIGMA), 702);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        702,
        TokenType::Operator(Greater),
        Backtrack,
    );
    nfa.set_transition(&mut transtions, 701, Char('='), 703);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        703,
        TokenType::Operator(GreaterOrEqual),
        NoBacktrack,
    );

    // = ==
    nfa.set_transition(&mut transtions, 800, Char('='), 801);
    nfa.set_transition(&mut transtions, 801, Array(&SIGMA), 802);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        802,
        TokenType::Operator(Assignment),
        Backtrack,
    );
    nfa.set_transition(&mut transtions, 801, Char('='), 803);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        803,
        TokenType::Operator(Equal),
        NoBacktrack,
    );

    // +
    nfa.set_transition(&mut transtions, 900, Char('+'), 901);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        901,
        TokenType::Operator(Addition),
        NoBacktrack,
    );

    // -
    nfa.set_transition(&mut transtions, 1000, Char('-'), 1001);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        1001,
        TokenType::Operator(Subtraction),
        NoBacktrack,
    );

    // *
    nfa.set_transition(&mut transtions, 1100, Char('*'), 1101);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        1101,
        TokenType::Operator(Multiplication),
        NoBacktrack,
    );

    // &&
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

    // !
    nfa.set_transition(&mut transtions, 1300, Char('!'), 1301);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        1301,
        TokenType::Operator(Not),
        NoBacktrack,
    );

    // &&
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

    // ;
    nfa.set_transition(&mut transtions, 1500, Char(';'), 1501);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        1501,
        TokenType::Separator(SemiColon),
        NoBacktrack,
    );

    // ,
    nfa.set_transition(&mut transtions, 1600, Char(','), 1601);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        1601,
        TokenType::Separator(Coma),
        NoBacktrack,
    );

    // .
    nfa.set_transition(&mut transtions, 1700, Char('.'), 1701);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        1701,
        TokenType::Separator(Period),
        NoBacktrack,
    );

    // : ::
    nfa.set_transition(&mut transtions, 1800, Char(':'), 1801);
    nfa.set_transition(&mut transtions, 1801, Array(&SIGMA), 1802);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        1802,
        TokenType::Separator(Colon),
        Backtrack,
    );
    nfa.set_transition(&mut transtions, 1801, Char(':'), 1803);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        1803,
        TokenType::Separator(ScopeResolution),
        NoBacktrack,
    );

    // (
    nfa.set_transition(&mut transtions, 1900, Char('('), 1901);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        1901,
        TokenType::Separator(LeftParenthesis),
        NoBacktrack,
    );

    // )
    nfa.set_transition(&mut transtions, 2000, Char(')'), 2001);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        2001,
        TokenType::Separator(RightParenthesis),
        NoBacktrack,
    );

    // {
    nfa.set_transition(&mut transtions, 2100, Char('{'), 2101);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        2101,
        TokenType::Separator(LeftCurlyBracket),
        NoBacktrack,
    );

    // }
    nfa.set_transition(&mut transtions, 2200, Char('}'), 2201);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        2201,
        TokenType::Separator(RightCurlyBracket),
        NoBacktrack,
    );

    // [
    nfa.set_transition(&mut transtions, 2300, Char('['), 2301);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        2301,
        TokenType::Separator(LeftSquareBracket),
        NoBacktrack,
    );

    // ]
    nfa.set_transition(&mut transtions, 2400, Char(']'), 2401);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        2401,
        TokenType::Separator(RightSquareBracket),
        NoBacktrack,
    );

    nfa.set_transition(&mut transtions, 2500, Char('_'), 2501);
    nfa.add_final_state(
        &mut tokens,
        &mut backtrack,
        2501,
        TokenType::LexicalError(InvalidId),
        NoBacktrack,
    );

    nfa.states = nfa
        .epsilon_closure
        .keys()
        .cloned()
        .chain(nfa.function.keys().map(|&(key, _)| key))
        .collect();

    (nfa, tokens, backtrack)
}
