use crate::syntactic_analyzer::*;
use crate::tree::Tree;

use std::fmt::{Display, Formatter};
use std::str::FromStr;

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

impl Display for NodeType {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
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
            IfStat => List(vec![OneOf(expr.clone()), One(StatBlock), One(StatBlock)]),
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
            _ => Err(s.to_string()),
        }
    }
}

#[derive(Clone)]
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
    ) -> Result<(), SyntacticError> {
        use NodeChildren::*;
        use SyntacticError::*;

        let new_node_id = self.new_node(NodeElement {
            node_type: new_node_type,
            data: if new_node_type.need_data() {
                let data = data_stack.pop();
                if data.is_some() {
                    data
                } else {
                    return Err(NoNodeOnDataStack(new_node_type));
                }
            } else {
                None
            },
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
    ) -> Result<(), SyntacticError> {
        use NodeChildrenGroup::*;
        use SyntacticError::*;

        match node_children_group {
            One(child_node_type) => {
                match semantic_stack.pop() {
                    Some(top_node_id) => {
                        if !self.add_one(new_node_id, child_node_type, top_node_id) {
                            return Err(WrongNodeOnStackOne(
                                new_node_type,
                                child_node_type,
                                self.get_element(top_node_id).node_type,
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
                                self.get_element(top_node_id).node_type,
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
