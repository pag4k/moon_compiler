use super::tree::Tree;

use std::fmt::{Display, Formatter};
use std::str::FromStr;

// Define the all the arrays of char describing the language.
// Note that the first char is used as an identifier of the array.
pub const SIGMA: [char; 87] = [
    'Σ', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h',
    'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'A',
    'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T',
    'U', 'V', 'W', 'X', 'Y', 'Z', '!', '(', ')', '*', '+', ',', '-', '.', '/', ':', ';', '<', '=',
    '>', '[', ']', '_', '{', '}', '&', '|', ' ', '\t', '\n',
];
pub const NONZERO: [char; 10] = ['N', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
pub const DIGIT: [char; 11] = ['D', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
pub const LETTER: [char; 53] = [
    'L', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
    's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K',
    'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
];

//#[derive(Clone, Copy)]
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum TokenType {
    Id,
    IntNum,
    FloatNum,
    Keyword(KeywordType),
    Operator(OperatorType),
    Separator(SeparatorType),
    Comment(CommentType),
    LexicalError(LexicalError),
}

impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        use TokenType::*;
        match self {
            Id => write!(f, "id"),
            IntNum => write!(f, "intNum"),
            FloatNum => write!(f, "floatNum"),
            Keyword(keyword) => write!(f, "{:?}", keyword),
            Operator(operator) => write!(f, "{:?}", operator),
            Separator(separator) => write!(f, "{:?}", separator),
            Comment(comment) => write!(f, "{:?}", comment),
            LexicalError(error) => write!(f, "{}", error),
        }
    }
}

impl FromStr for TokenType {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, ()> {
        use KeywordType::*;
        use OperatorType::*;
        use SeparatorType::*;
        use TokenType::*;
        match s {
            "'id'" => Ok(Id),
            "'intNum'" => Ok(IntNum),
            "'floatNum'" => Ok(FloatNum),
            "'if'" => Ok(Keyword(If)),
            "'then'" => Ok(Keyword(Then)),
            "'else'" => Ok(Keyword(Else)),
            "'for'" => Ok(Keyword(For)),
            "'class'" => Ok(Keyword(Class)),
            "'integer'" => Ok(Keyword(Integer)),
            "'float'" => Ok(Keyword(Float)),
            "'read'" => Ok(Keyword(Read)),
            "'write'" => Ok(Keyword(Write)),
            "'return'" => Ok(Keyword(Return)),
            "'main'" => Ok(Keyword(Main)),
            "'lt'" => Ok(Operator(LT)),
            "'leq'" => Ok(Operator(LEq)),
            "'neq'" => Ok(Operator(NEq)),
            "'gt'" => Ok(Operator(GT)),
            "'geq'" => Ok(Operator(GEq)),
            "'='" => Ok(Operator(Assignment)),
            "'eq'" => Ok(Operator(Eq)),
            "'+'" => Ok(Operator(Addition)),
            "'-'" => Ok(Operator(Subtraction)),
            "'*'" => Ok(Operator(Multiplication)),
            "'/'" => Ok(Operator(Division)),
            "'and'" => Ok(Operator(And)),
            "'not'" => Ok(Operator(Not)),
            "'or'" => Ok(Operator(Or)),
            "'sr'" => Ok(Operator(SR)),
            "';'" => Ok(Separator(SemiColon)),
            "','" => Ok(Separator(Coma)),
            "'.'" => Ok(Separator(Period)),
            "':'" => Ok(Separator(Colon)),
            "'('" => Ok(Separator(LeftParenthesis)),
            "')'" => Ok(Separator(RightParenthesis)),
            "'{'" => Ok(Separator(LeftCurlyBracket)),
            "'}'" => Ok(Separator(RightCurlyBracket)),
            "'['" => Ok(Separator(LeftSquareBracket)),
            "']'" => Ok(Separator(RightSquareBracket)),
            _ => Err(()),
        }
    }
}

//#[derive(Debug, Clone, Copy)]
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum KeywordType {
    If,
    Then,
    Else,
    For,
    Class,
    Integer,
    Float,
    Read,
    Write,
    Return,
    Main,
}

impl FromStr for KeywordType {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, ()> {
        use KeywordType::*;
        match s {
            "if" => Ok(If),
            "then" => Ok(Then),
            "else" => Ok(Else),
            "for" => Ok(For),
            "class" => Ok(Class),
            "integer" => Ok(Integer),
            "float" => Ok(Float),
            "read" => Ok(Read),
            "write" => Ok(Write),
            "return" => Ok(Return),
            "main" => Ok(Main),
            _ => Err(()),
        }
    }
}

//#[derive(Debug, Clone, Copy)]
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum OperatorType {
    LT,
    LEq,
    NEq,
    GT,
    GEq,
    Assignment,
    Eq,
    Addition,
    Subtraction,
    Multiplication,
    Division,
    And,
    Not,
    Or,
    SR,
}

//#[derive(Debug, Clone, Copy)]
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum SeparatorType {
    SemiColon,
    Coma,
    Period,
    Colon,
    LeftParenthesis,
    RightParenthesis,
    LeftCurlyBracket,
    RightCurlyBracket,
    LeftSquareBracket,
    RightSquareBracket,
}

//#[derive(Debug, Clone, Copy)]
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum CommentType {
    BlockComment,
    LineComment,
}

//#[derive(Debug, Clone, Copy)]
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum LexicalError {
    InvalidCharacter,
    InvalidToken,
    InvalidId,
    FloatMissingFraction,
    FloatTrailingZeros,
    FloatMissingExponent,
    IncompleteAnd,
    IncompleteOr,
    UnterminatedBlockComment,
}

impl Display for LexicalError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            LexicalError::InvalidCharacter => write!(f, "Lexical Error: Invalid Character"),
            LexicalError::InvalidToken => write!(f, "Lexical Error: Invalid Token"),
            LexicalError::InvalidId => write!(f, "Lexical Error: Invalid Id"),
            LexicalError::FloatMissingFraction => {
                write!(f, "Lexical Error: Float Missing Fraction")
            }
            LexicalError::FloatTrailingZeros => write!(f, "Lexical Error: Float Trailing Zeros"),
            LexicalError::FloatMissingExponent => {
                write!(f, "Lexical Error: Float Missing Exponent")
            }
            LexicalError::IncompleteAnd => {
                write!(f, "Lexical Error: Incomplete And (did you mean '&&'?)")
            }
            LexicalError::IncompleteOr => {
                write!(f, "Lexical Error: Incomplete Or (did you mean '||'?)")
            }
            LexicalError::UnterminatedBlockComment => write!(
                f,
                "Lexical Error: Unterminated Block Comment (you are missing '*/')"
            ),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum VariableType {
    Prog,
    ClassDecl,
    ClassDeclList,
    FuncDecl,
    FuncHead,
    ScopeOp,
    ScopeOpPrime,
    FuncDef,
    FuncDefList,
    FuncBody,
    MemberDecl,
    MemberDeclPrime,
    MemberList,
    NumVarDecl,
    AssignStat,
    Statement,
    StatOrAssign,
    StatOrAssignList,
    StatOrAssignOrVarDecl,
    StatOrAssignOrVarDeclPrime,
    StatOrAssignOrVarDeclList,
    StatBlock,
    Expr,
    ExprPrime,
    RelExpr,
    ArithExpr,
    ArithExprPrime,
    Sign,
    Term,
    TermPrime,
    Factor,
    FactorPrime,
    Variable,
    Idnest,
    IdnestPrime,
    IdnestRep,
    InherListOp,
    InherList,
    Index,
    IndexList,
    Dim,
    DimList,
    Type,
    FParam,
    FParamList,
    FParamListPrime,
    AParam,
    AParamList,
    AParamListPrime,
    AssignOp,
    RelOp,
    AddOp,
    MultOp,
}

impl FromStr for VariableType {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, ()> {
        use VariableType::*;
        match s {
            "prog" => Ok(Prog),
            "classDecl" => Ok(ClassDecl),
            "classDeclList" => Ok(ClassDeclList),
            "funcDecl" => Ok(FuncDecl),
            "funcHead" => Ok(FuncHead),
            "scopeOp" => Ok(ScopeOp),
            "scopeOp'" => Ok(ScopeOpPrime),
            "funcDef" => Ok(FuncDef),
            "funcDefList" => Ok(FuncDefList),
            "funcBody" => Ok(FuncBody),
            "memberDecl" => Ok(MemberDecl),
            "memberDecl'" => Ok(MemberDeclPrime),
            "memberList" => Ok(MemberList),
            "numVarDecl" => Ok(NumVarDecl),
            "assignStat" => Ok(AssignStat),
            "statement" => Ok(Statement),
            "statOrAssign" => Ok(StatOrAssign),
            "statOrAssignList" => Ok(StatOrAssignList),
            "statOrAssignOrVarDecl" => Ok(StatOrAssignOrVarDecl),
            "statOrAssignOrVarDecl'" => Ok(StatOrAssignOrVarDeclPrime),
            "statOrAssignOrVarDeclList" => Ok(StatOrAssignOrVarDeclList),
            "statBlock" => Ok(StatBlock),
            "expr" => Ok(Expr),
            "expr'" => Ok(ExprPrime),
            "relExpr" => Ok(RelExpr),
            "arithExpr" => Ok(ArithExpr),
            "arithExpr'" => Ok(ArithExprPrime),
            "sign" => Ok(Sign),
            "term" => Ok(Term),
            "term'" => Ok(TermPrime),
            "factor" => Ok(Factor),
            "factor'" => Ok(FactorPrime),
            "variable" => Ok(Variable),
            "idnest" => Ok(Idnest),
            "idnest'" => Ok(IdnestPrime),
            "idnestRep" => Ok(IdnestRep),
            "inherListOp" => Ok(InherListOp),
            "inherList" => Ok(InherList),
            "index" => Ok(Index),
            "indexList" => Ok(IndexList),
            "dim" => Ok(Dim),
            "dimList" => Ok(DimList),
            "type" => Ok(Type),
            "fParam" => Ok(FParam),
            "fParamList" => Ok(FParamList),
            "fParamList'" => Ok(FParamListPrime),
            "aParam" => Ok(AParam),
            "aParamList" => Ok(AParamList),
            "aParamList'" => Ok(AParamListPrime),
            "assignOp" => Ok(AssignOp),
            "relOp" => Ok(RelOp),
            "addOp" => Ok(AddOp),
            "multOp" => Ok(MultOp),
            _ => Err(()),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum NodeType {
    Data,
    Epsilon,
    Id,
    Idi,
    Num,
    RelOp,
    Type,
    ClassDeclList,
    FuncDefList,
    Prog,
    MemberList,
    ClassDecl,
    FuncDecl,
    FuncDef,
    StatBlock,
    DimList,
    VarDecl,
    AssignStat,
    AssignStati,
    IfStat,
    ForStat,
    ReadStat,
    WriteStat,
    ReturnStat,
    IndexList,
    RelExpr,
    AddOp,
    MultOp,
    Not,
    Sign,
    VarElementList,
    DataMember,
    FunctionCall,
    InheritList,
    FParam,
    FParamList,
    AParamList,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
enum NodeChildren {
    Single(NodeChildrenGroup),
    List(Vec<NodeChildrenGroup>),
    Leaf,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
enum NodeChildrenGroup {
    One(NodeType),
    OneOf(Vec<NodeType>),
    Many(NodeType),
    ManyOf(Vec<NodeType>),
}

impl NodeType {
    fn get_children(self) -> NodeChildren {
        use NodeChildren::*;
        use NodeChildrenGroup::*;
        use NodeType::*;
        let expr = vec![
            RelExpr,
            AddOp,
            MultOp,
            VarElementList,
            Num,
            FunctionCall,
            Not,
            Sign,
        ];
        let arith_expr = vec![AddOp, MultOp, VarElementList, Num, FunctionCall, Not, Sign];
        let term = vec![AddOp, MultOp, VarElementList, Num, FunctionCall, Not, Sign];
        let factor = vec![AddOp, MultOp, VarElementList, Num, FunctionCall, Not, Sign];

        match self {
            Data => unreachable!(),
            Epsilon => Leaf,
            Id => Leaf,
            Idi => Leaf,
            Num => Leaf,
            RelOp => Leaf,
            Type => Leaf,
            ClassDeclList => Single(Many(ClassDecl)),
            FuncDefList => Single(Many(FuncDef)),
            Prog => List(vec![One(ClassDeclList), One(FuncDefList), One(StatBlock)]),
            MemberList => Single(ManyOf(vec![VarDecl, FuncDecl])),
            ClassDecl => List(vec![One(Id), One(InheritList), One(MemberList)]),
            FuncDecl => List(vec![One(Type), One(Id), One(FParamList)]),
            FuncDef => List(vec![
                One(Type),
                OneOf(vec![Id, Epsilon]),
                One(Id),
                One(FParamList),
                One(StatBlock),
            ]),
            StatBlock => Single(ManyOf(vec![
                VarDecl,
                AssignStati,
                IfStat,
                ForStat,
                ReadStat,
                WriteStat,
                ReturnStat,
            ])),
            DimList => Single(Many(Num)),
            VarDecl => List(vec![One(Type), One(Id), One(DimList)]),
            AssignStat => List(vec![One(VarElementList), OneOf(expr.clone())]),
            AssignStati => List(vec![One(VarElementList), OneOf(expr.clone())]),
            IfStat => List(vec![One(RelExpr), One(StatBlock), One(StatBlock)]),
            ForStat => List(vec![
                One(Type),
                One(Id),
                OneOf(expr.clone()),
                One(RelExpr),
                One(AssignStat),
                One(StatBlock),
            ]),
            ReadStat => Single(One(VarElementList)),
            WriteStat => Single(OneOf(expr.clone())),
            ReturnStat => Single(OneOf(expr.clone())),
            IndexList => Single(ManyOf(expr.clone())),
            RelExpr => List(vec![OneOf(expr.clone()), One(RelOp), OneOf(expr.clone())]),
            AddOp => List(vec![OneOf(arith_expr.clone()), OneOf(term.clone())]),
            MultOp => List(vec![OneOf(term.clone()), OneOf(factor.clone())]),
            Not => Single(OneOf(factor.clone())),
            Sign => Single(OneOf(factor.clone())),
            VarElementList => Single(ManyOf(vec![DataMember, FunctionCall])),
            DataMember => List(vec![One(Id), One(IndexList)]),
            FunctionCall => List(vec![One(Id), One(AParamList)]),
            InheritList => Single(Many(Idi)),
            FParam => List(vec![One(Type), One(Id), One(DimList)]),
            FParamList => Single(Many(FParam)),
            AParamList => Single(ManyOf(expr.clone())),
        }
    }

    fn need_data(self) -> bool {
        use NodeType::*;
        match self {
            Id | Idi | Num | RelOp | Type | AddOp | MultOp | Sign => true,
            _ => false,
        }
    }
}

impl FromStr for NodeType {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, ()> {
        use NodeType::*;
        match s {
            "#GetData" => Ok(Data),
            "#MakeNodeEpsilon" => Ok(Epsilon),
            "#MakeNodeId" => Ok(Id),
            "#MakeNodeIdi" => Ok(Idi),
            "#MakeNodeNum" => Ok(Num),
            "#MakeNodeRelOp" => Ok(RelOp),
            "#MakeNodeType" => Ok(Type),
            "#MakeNodeClassDeclList" => Ok(ClassDeclList),
            "#MakeNodeFuncDefList" => Ok(FuncDefList),
            "#MakeNodeProg" => Ok(Prog),
            "#MakeNodeMemberList" => Ok(MemberList),
            "#MakeNodeClassDecl" => Ok(ClassDecl),
            "#MakeNodeFuncDecl" => Ok(FuncDecl),
            "#MakeNodeFuncDef" => Ok(FuncDef),
            "#MakeNodeStatBlock" => Ok(StatBlock),
            "#MakeNodeDimList" => Ok(DimList),
            "#MakeNodeVarDecl" => Ok(VarDecl),
            "#MakeNodeAssignStat" => Ok(AssignStat),
            "#MakeNodeAssignStati" => Ok(AssignStati),
            "#MakeNodeIfStat" => Ok(IfStat),
            "#MakeNodeForStat" => Ok(ForStat),
            "#MakeNodeReadStat" => Ok(ReadStat),
            "#MakeNodeWriteStat" => Ok(WriteStat),
            "#MakeNodeReturnStat" => Ok(ReturnStat),
            "#MakeNodeIndexList" => Ok(IndexList),
            "#MakeNodeRelExpr" => Ok(RelExpr),
            "#MakeNodeAddOp" => Ok(AddOp),
            "#MakeNodeMultOp" => Ok(MultOp),
            "#MakeNodeNot" => Ok(Not),
            "#MakeNodeSign" => Ok(Sign),
            "#MakeNodeVarElementList" => Ok(VarElementList),
            "#MakeNodeDataMember" => Ok(DataMember),
            "#MakeNodeFunctionCall" => Ok(FunctionCall),
            "#MakeNodeInheritList" => Ok(InheritList),
            "#MakeNodeFParam" => Ok(FParam),
            "#MakeNodeFParamList" => Ok(FParamList),
            "#MakeNodeAParamList" => Ok(AParamList),
            _ => Err(()),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct NodeElement {
    pub node_type: NodeType,
    pub data: Option<String>,
}

impl Display for NodeElement {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match &self.data {
            Some(data) => write!(f, "{:?}: {}", self.node_type, data),
            None => write!(f, "{:?}", self.node_type),
        }
    }
}

impl Tree<NodeElement> {
    pub fn make_node(
        &mut self,
        semantic_stack: &mut Vec<usize>,
        data_stack: &mut Vec<String>,
        new_node_type: NodeType,
    ) {
        use NodeChildren::*;
        use NodeChildrenGroup::*;

        let new_node_id = self.new_node(NodeElement {
            node_type: new_node_type,
            data: if new_node_type.need_data() {
                data_stack.pop()
            } else {
                None
            },
        });

        let node_children = new_node_type.get_children();
        match node_children {
            Single(group) => match group {
                One(node_type) => {
                    match semantic_stack.pop() {
                        Some(top_node_id) => {
                            if !self.add_one(new_node_id, node_type, top_node_id) {
                                unreachable!(
                                    "Make node: Single, One, new: {:?}, expecting: {:?}: top: {:?}.",
                                    new_node_type,
                                    node_type,
                                    self.get_element(top_node_id).node_type
                                );
                            }
                        }
                        None => unreachable!(
                            "Make node: Single, One, new: {:?}, expecting: {:?}: stack is empty.",
                            new_node_type, node_type
                        ),
                    };
                }
                OneOf(node_list) => {
                    match semantic_stack.pop() {
                        Some(top_node_id) => {
                            if !self.add_one_of(new_node_id, &node_list, top_node_id) {
                                unreachable!(
                                    "Make node: Single, OneOf, new: {:?}, expecting: {:?}: top: {:?}.",
                                    new_node_type,
                                    node_list,
                                    self.get_element(top_node_id).node_type
                                );
                            }
                        }
                        None => unreachable!(
                            "Make node: Single, OneOf, new: {:?}, expecting: {:?}: stack is empty.",
                            new_node_type, node_list
                        ),
                    };
                }
                Many(node_type) => {
                    while let Some(top_node_id) = semantic_stack.pop() {
                        if !self.add_one(new_node_id, node_type, top_node_id) {
                            semantic_stack.push(top_node_id);
                            break;
                        }
                    }
                }
                ManyOf(node_list) => {
                    while let Some(top_node_id) = semantic_stack.pop() {
                        if !self.add_one_of(new_node_id, &node_list, top_node_id) {
                            semantic_stack.push(top_node_id);
                            break;
                        }
                    }
                }
            },
            List(group_list) => {
                for group in group_list.iter().rev() {
                    match group {
                        One(node_type) => {
                            match semantic_stack.pop() {
                                Some(top_node_id) => {
                                    if !self.add_one(new_node_id, *node_type, top_node_id) {
                                        unreachable!(
                                            "Make node: List, One, new: {:?}, expecting: {:?}: top: {:?}.",
                                            new_node_type,
                                            node_type,
                                            self.get_element(top_node_id).node_type
                                        );
                                    }
                                }
                                None => unreachable!(
                                    "Make node: List, One, new: {:?}, expecting: {:?}: stack is empty.",
                                    new_node_type,
                                    node_type
                                ),
                            };
                        }
                        OneOf(node_list) => {
                            match semantic_stack.pop() {
                                Some(top_node_id) => {
                                    if !self.add_one_of(new_node_id, &node_list, top_node_id) {
                                        unreachable!(
                                            "Make node: List, OneOf, new: {:?}, expecting: {:?}: top: {:?}.",
                                            new_node_type,
                                            node_list,
                                            self.get_element(top_node_id).node_type
                                        );
                                    }
                                }
                                None => unreachable!(
                                    "Make node: Single, List, new: {:?}, expecting: {:?}: stack is empty.",
                                    new_node_type,
                                    node_list
                                ),
                            };
                        }
                        Many(node_type) => {
                            while let Some(top_node_id) = semantic_stack.pop() {
                                if !self.add_one(new_node_id, *node_type, top_node_id) {
                                    semantic_stack.push(top_node_id);
                                    break;
                                }
                            }
                        }
                        ManyOf(node_list) => {
                            while let Some(top_node_id) = semantic_stack.pop() {
                                if !self.add_one_of(new_node_id, &node_list, top_node_id) {
                                    semantic_stack.push(top_node_id);
                                    break;
                                }
                            }
                        }
                    }
                }
            }
            Leaf => {}
        }

        semantic_stack.push(new_node_id);
    }

    fn add_one(&mut self, new_node_id: usize, node_type: NodeType, top_node_id: usize) -> bool {
        if self.get_element(top_node_id).node_type == node_type {
            self.add_left_child(new_node_id, top_node_id);
            true
        } else {
            false
        }
    }
    fn add_one_of(
        &mut self,
        new_node_id: usize,
        node_list: &[NodeType],
        top_node_id: usize,
    ) -> bool {
        if node_list.contains(&self.get_element(top_node_id).node_type) {
            self.add_left_child(new_node_id, top_node_id);
            true
        } else {
            false
        }
    }
}
