use crate::lexical_analyzer::*;
use crate::memory_table::*;
use crate::symbol_table::*;
use crate::tree::Tree;

use std::fmt::{Display, Formatter};
use std::str::FromStr;

pub type AST = Tree<NodeElement, SymbolTableArena, MemoryTableArena>;

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
    MainFuncBody,
    MemberList,
    ClassDecl,
    FuncDecl,
    FuncDef,
    FuncBody,
    StatBlock,
    DimList,
    VarDecl,
    AssignStat,
    AssignForStat,
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
    EndProgram,
}

impl Display for NodeType {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub enum ASTError {
    TokenStackIsEmpty(NodeType),
    NoNodeOnStackOne(NodeType, NodeType),
    NoNodeOnStackList(NodeType, Vec<NodeType>),
    WrongNodeOnStackOne(NodeType, NodeType, NodeType),
    WrongNodeOnStackList(NodeType, Vec<NodeType>, NodeType),
}

impl Display for ASTError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        use ASTError::*;
        match self {
            TokenStackIsEmpty(node_type) => write!(
                f,
                "AST error: No token found on token stack to make node: {}.",
                node_type
            ),
            NoNodeOnStackOne(node_type, child_node_type) => write!(
                f,
                "AST error: Trying to make node {} and is expecting {}, but there is node on stack.",
                node_type, child_node_type
            ),
            NoNodeOnStackList(node_type, node_list) => write!(
                f,
                "AST error: Trying to make node {} and is expecting {:?}, but there is node on stack.",
                node_type, node_list
            ),
            WrongNodeOnStackOne(node_type, child_node_type, actual_node_type) => write!(
                f,
                "AST error: Trying to make node {} and is expecting {}, but found {} on stack.",
                node_type, child_node_type, actual_node_type
            ),
            WrongNodeOnStackList(node_type, node_list, actual_node_type) => write!(
                f,
                "AST error: Trying to make node {} and is expecting {:?}, but found {} on stack.",
                node_type, node_list, actual_node_type
            ),
        }
    }
}

#[derive(Clone)]
enum NodeChildren {
    Single(NodeChildrenGroup),
    List(Vec<NodeChildrenGroup>),
    Leaf,
}

#[derive(Clone)]
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
        let expr = vec![RelExpr, AddOp, MultOp, VarElementList, Num, Not, Sign];
        let arith_expr = vec![AddOp, MultOp, VarElementList, Num, Not, Sign];
        let term = vec![AddOp, MultOp, VarElementList, Num, Not, Sign];
        let factor = vec![AddOp, MultOp, VarElementList, Num, Not, Sign];

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
            Prog => List(vec![
                One(ClassDeclList),
                One(FuncDefList),
                One(MainFuncBody),
            ]),
            MemberList => Single(ManyOf(vec![VarDecl, FuncDecl])),
            ClassDecl => List(vec![One(Id), One(InheritList), One(MemberList)]),
            FuncDecl => List(vec![One(Type), One(Id), One(FParamList)]),
            FuncDef => List(vec![
                One(Type),
                OneOf(vec![Type, Epsilon]),
                One(Id),
                One(FParamList),
                One(FuncBody),
            ]),
            MainFuncBody => Single(ManyOf(vec![
                VarDecl, AssignStat, IfStat, ForStat, ReadStat, WriteStat, ReturnStat,
            ])),
            FuncBody => Single(ManyOf(vec![
                VarDecl, AssignStat, IfStat, ForStat, ReadStat, WriteStat, ReturnStat,
            ])),
            StatBlock => Single(ManyOf(vec![
                //VarDecl,
                AssignStat, IfStat, ForStat, ReadStat, WriteStat, ReturnStat,
            ])),
            DimList => Single(Many(Num)),
            VarDecl => List(vec![One(Type), One(Id), One(DimList)]),
            AssignStat => List(vec![One(VarElementList), OneOf(expr.clone())]),
            AssignForStat => List(vec![One(VarElementList), OneOf(expr.clone())]),
            IfStat => List(vec![OneOf(expr.clone()), One(StatBlock), One(StatBlock)]),
            ForStat => List(vec![
                One(Type),
                One(Id),
                OneOf(expr.clone()),
                One(RelExpr),
                One(AssignForStat),
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
            EndProgram => unreachable!(),
        }
    }

    fn need_data(self) -> bool {
        use NodeType::*;
        match self {
            Id | Idi | Num | RelOp | Type | AddOp | MultOp | Not | Sign => true,
            _ => false,
        }
    }
}

impl FromStr for NodeType {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
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
            "#MakeNodeMainFuncBody" => Ok(MainFuncBody),
            "#MakeNodeFuncBody" => Ok(FuncBody),
            "#MakeNodeStatBlock" => Ok(StatBlock),
            "#MakeNodeDimList" => Ok(DimList),
            "#MakeNodeVarDecl" => Ok(VarDecl),
            "#MakeNodeAssignStat" => Ok(AssignStat),
            "#MakeNodeAssignForStat" => Ok(AssignForStat),
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
            "#EndProgram" => Ok(EndProgram),
            _ => Err(s.to_string()),
        }
    }
}

#[derive(Clone)]
pub struct NodeElement {
    pub node_type: NodeType,
    pub token: Option<Token>,
    pub symbol_table: Option<usize>,
    pub symbol_table_entry: Option<usize>,
    pub memory_table: Option<usize>,
    pub memory_table_entry: Option<usize>,
    pub data_type: Option<SymbolType>,
}

impl Display for NodeElement {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match &self.token {
            Some(token) => match token.clone().lexeme {
                Some(data) => write!(f, "{:?}: {}", self.node_type, data),
                None => write!(f, "{:?}", self.node_type),
            },
            None => write!(f, "{:?}", self.node_type),
        }
    }
}

impl AST {
    pub fn get_token(&self, node_index: usize) -> Token {
        self.get_element(node_index).token.clone().unwrap()
    }

    pub fn get_child_token(&self, node_index: usize, child_index: usize) -> Token {
        self.get_token(self.get_children(node_index)[child_index])
    }

    pub fn get_leftmost_token(&self, node_index: usize) -> Token {
        let child_index_list = self.get_children(node_index);
        if child_index_list.is_empty() {
            self.get_token(node_index)
        } else {
            self.get_leftmost_token(child_index_list[0])
        }
    }

    pub fn get_lexeme(&self, node_index: usize) -> String {
        self.get_element(node_index)
            .token
            .clone()
            .unwrap()
            .lexeme
            .unwrap()
    }

    pub fn get_child_lexeme(&self, node_index: usize, child_index: usize) -> String {
        self.get_lexeme(self.get_children(node_index)[child_index])
    }

    pub fn get_note_type(&self, node_index: usize) -> NodeType {
        self.get_element(node_index).node_type
    }

    pub fn get_child_data_type(&self, node_index: usize, child_index: usize) -> SymbolType {
        self.get_element(self.get_children(node_index)[child_index])
            .clone()
            .data_type
            .unwrap()
    }

    pub fn make_node(
        &mut self,
        semantic_stack: &mut Vec<usize>,
        token_stack: &mut Vec<Token>,
        new_node_type: NodeType,
    ) -> Result<(), ASTError> {
        use ASTError::*;
        use NodeChildren::*;

        let new_node_id = self.new_node(NodeElement {
            node_type: new_node_type,
            token: if new_node_type.need_data() {
                let token = token_stack.pop();
                if token.is_some() {
                    token
                } else {
                    return Err(TokenStackIsEmpty(new_node_type));
                }
            } else {
                None
            },
            symbol_table: None,
            symbol_table_entry: None,
            memory_table: None,
            memory_table_entry: None,
            data_type: None,
        });

        let node_children = new_node_type.get_children();
        match node_children {
            Single(node_children_group) => self.get_node_children(
                semantic_stack,
                new_node_type,
                new_node_id,
                node_children_group,
            )?,
            List(node_children_group_list) => {
                for node_children_group in node_children_group_list.iter().rev() {
                    self.get_node_children(
                        semantic_stack,
                        new_node_type,
                        new_node_id,
                        node_children_group.clone(),
                    )?
                }
            }
            Leaf => {}
        }

        semantic_stack.push(new_node_id);
        Ok(())
    }

    fn get_node_children(
        &mut self,
        semantic_stack: &mut Vec<usize>,
        new_node_type: NodeType,
        new_node_id: usize,
        node_children_group: NodeChildrenGroup,
    ) -> Result<(), ASTError> {
        use ASTError::*;
        use NodeChildrenGroup::*;

        match node_children_group {
            One(child_node_type) => {
                match semantic_stack.pop() {
                    Some(top_node_id) => {
                        if !self.add_one(new_node_id, child_node_type, top_node_id) {
                            return Err(WrongNodeOnStackOne(
                                new_node_type,
                                child_node_type,
                                self.get_note_type(top_node_id),
                            ));
                        }
                    }
                    None => return Err(NoNodeOnStackOne(new_node_type, child_node_type)),
                };
            }
            OneOf(node_list) => {
                match semantic_stack.pop() {
                    Some(top_node_id) => {
                        if !self.add_one_of(new_node_id, &node_list, top_node_id) {
                            return Err(WrongNodeOnStackList(
                                new_node_type,
                                node_list,
                                self.get_note_type(top_node_id),
                            ));
                        }
                    }
                    None => return Err(NoNodeOnStackList(new_node_type, node_list)),
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
        }
        Ok(())
    }

    fn add_one(&mut self, new_node_id: usize, node_type: NodeType, top_node_id: usize) -> bool {
        if self.get_note_type(top_node_id) == node_type {
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
        if node_list.contains(&self.get_note_type(top_node_id)) {
            self.add_left_child(new_node_id, top_node_id);
            true
        } else {
            false
        }
    }

    pub fn get_parent_node_of_type(
        &self,
        node_index: usize,
        target_node_types: &[NodeType],
        excluded_node_types: &[NodeType],
    ) -> Option<(usize, NodeType)> {
        if target_node_types
            .iter()
            .any(|node_type| excluded_node_types.contains(node_type))
        {
            panic!("Intersection between target_node_types and excluded_node_types.")
        }
        let node_type = self.get_note_type(node_index);
        if target_node_types.contains(&node_type) {
            Some((node_index, node_type))
        } else if excluded_node_types.contains(&node_type) {
            None
        } else {
            let parent_index = self.get_parent(node_index);
            match parent_index {
                Some(parent_index) => self.get_parent_node_of_type(
                    parent_index,
                    target_node_types,
                    excluded_node_types,
                ),
                None => None,
            }
        }
    }

    pub fn get_parent_node_of_type_on_left_branch(
        &self,
        node_index: usize,
        target_node_types: &[NodeType],
        excluded_node_types: &[NodeType],
    ) -> Option<(usize, NodeType)> {
        if target_node_types
            .iter()
            .any(|node_type| excluded_node_types.contains(node_type))
        {
            panic!("Intersection between target_node_types and excluded_node_types.")
        }
        let node_type = self.get_note_type(node_index);
        if target_node_types.contains(&node_type) {
            Some((node_index, node_type))
        } else if excluded_node_types.contains(&node_type)
            || self.get_left_sibling(node_index).is_some()
        {
            None
        } else {
            let parent_index = self.get_parent(node_index);
            match parent_index {
                Some(parent_index) => self.get_parent_node_of_type_on_left_branch(
                    parent_index,
                    target_node_types,
                    excluded_node_types,
                ),
                None => None,
            }
        }
    }

    /// Add node indices of specified typeto Vec.
    pub fn find_node_type(
        &self,
        node_index: usize,
        found_node_indices: &mut Vec<usize>,
        node_types: &[NodeType],
    ) {
        if node_types.contains(&self.get_element(node_index).node_type) {
            found_node_indices.push(node_index);
        }
        for child_index in self.get_children(node_index) {
            self.find_node_type(child_index, found_node_indices, node_types);
        }
    }
}
