use crate::ast_node::*;
use crate::code_generation_common::*;
use crate::language::*;

pub const INDENT: &str = "          ";
pub const ENTRY_MARKER: &str = "% ENTRY MARKER";
pub const FUNCTION_MARKER: &str = "% FUNCTION MARKER";
pub const IF_MARKER: &str = "% IF MARKER";
pub const ELSE_MARKER: &str = "% ELSE MARKER";
pub const FOR_COND_MARKER: &str = "% FOR COND MARKER";
pub const FOR_INCR_MARKER: &str = "% FOR INCR MARKER";
pub const FOR_LOOP_MARKER: &str = "% FOR LOOP MARKER";

pub enum Reference {
    Node(usize),
    Offset(isize),
    SizeAndOffset(usize, isize),
    SizeAndOffsetAndRegister(usize, isize, usize),
}

impl Reference {
    fn get(self, ast: &mut AST, moon_code: &mut Vec<String>) -> (usize, isize, usize) {
        use Reference::*;
        match self {
            Reference::Node(node_index) => {
                get_reference(ast, moon_code, node_index).get(ast, moon_code)
            }
            Offset(offset) => (4, offset, 14),
            SizeAndOffset(size, offset) => (size, offset, 14),
            SizeAndOffsetAndRegister(size, offset, register) => (size, offset, register),
        }
    }
}

pub fn get_instruction(operator: OperatorType) -> String {
    use OperatorType::*;
    match operator {
        LT => String::from("clt"),
        LEq => String::from("cle"),
        NEq => String::from("cne"),
        GT => String::from("cgt"),
        GEq => String::from("cge"),
        Eq => String::from("ceq"),
        Addition => String::from("add"),
        Subtraction => String::from("sub"),
        Multiplication => String::from("mul"),
        Division => String::from("div"),
        _ => unreachable!(),
    }
}

pub fn add_and_or_not(
    ast: &mut AST,
    moon_code: &mut Vec<String>,
    operator: OperatorType,
    register1: usize,
    register2: usize,
    register3: usize,
) {
    use OperatorType::*;

    match operator {
        And => {
            let and_index = ast.register_pool.get_and();
            let zero = format!("zeroand{}", and_index);
            let end = format!("endand{}", and_index);
            moon_code.push(format!("{}bz r{},{}", INDENT, register1, zero,));
            moon_code.push(format!("{}bz r{},{}", INDENT, register2, zero,));
            addi(moon_code, register3, 0, 1);
            moon_code.push(format!("{}j {}", INDENT, end));
            add_label(moon_code, zero.clone());
            addi(moon_code, register3, 0, 0);
            add_label(moon_code, end.clone());
        }
        Or => {
            let or_index = ast.register_pool.get_or();
            let notzero = format!("nzeroor{}", or_index);
            let end = format!("endor{}", or_index);
            moon_code.push(format!("{}bnz r{},{}", INDENT, register1, notzero,));
            moon_code.push(format!("{}bnz r{},{}", INDENT, register2, notzero,));
            addi(moon_code, register3, 0, 0);
            moon_code.push(format!("{}j {}", INDENT, end));
            add_label(moon_code, notzero.clone());
            addi(moon_code, register3, 0, 1);
            add_label(moon_code, end.clone());
        }
        Not => {
            assert!(register2 == 0);
            let not_index = ast.register_pool.get_not();
            let zero = format!("zeroor{}", not_index);
            let end = format!("end0r{}", not_index);
            moon_code.push(format!("{}bz r{},{}", INDENT, register1, zero,));
            addi(moon_code, register3, 0, 0);
            moon_code.push(format!("{}j {}", INDENT, end));
            add_label(moon_code, zero.clone());
            addi(moon_code, register3, 0, 1);
            add_label(moon_code, end.clone());
        }
        _ => unreachable!(),
    };
}

pub fn add_entry_marker(ast: &AST, moon_code: &mut Vec<String>, node_index: usize) {
    use NodeType::*;

    // If the node has children, return.
    if !ast.get_children(node_index).is_empty() {
        return;
    }

    // If the node has a MainFuncBody parent.
    if let Some((_, _)) =
        ast.get_parent_node_of_type_on_left_branch(node_index, &[MainFuncBody], &[])
    {
        moon_code.push(ENTRY_MARKER.to_string());
    }
}

pub fn add_function_marker(ast: &AST, moon_code: &mut Vec<String>, node_index: usize) {
    use NodeType::*;

    // If the node has children, return.
    if !ast.get_children(node_index).is_empty() {
        return;
    }

    // If the node has a MainFuncBody parent.
    if let Some((_, _)) = ast.get_parent_node_of_type_on_left_branch(node_index, &[FuncDef], &[]) {
        moon_code.push(FUNCTION_MARKER.to_string());
    }
}

pub fn add_if_marker(ast: &AST, moon_code: &mut Vec<String>, node_index: usize) {
    use NodeType::*;

    // If the node has children, return.
    if !ast.get_children(node_index).is_empty() {
        return;
    }

    let stat_block_node_index = if let StatBlock = ast.get_node_type(node_index) {
        node_index
    } else {
        // If the node does not have StatBlock parent, return.
        match ast.get_parent_node_of_type_on_left_branch(node_index, &[StatBlock], &[]) {
            Some((stat_block_node_index, _)) => stat_block_node_index,
            None => {
                return;
            }
        }
    };
    let grand_parent_node_index = ast.get_parent(stat_block_node_index).unwrap();
    // If it is a IfStat, add one of the two marker.
    if let IfStat = ast.get_node_type(grand_parent_node_index) {
        // If it has a right sibling, it is the if block, if it has not, it is the else block.
        if ast.get_right_sibling(stat_block_node_index).is_some() {
            moon_code.push(IF_MARKER.to_string());
        } else {
            moon_code.push(ELSE_MARKER.to_string());
        }
    }
}

pub fn add_for_marker(ast: &AST, moon_code: &mut Vec<String>, node_index: usize) {
    use NodeType::*;

    // If the node has children, return.
    if !ast.get_children(node_index).is_empty() {
        return;
    }

    // If the node has a RelExpr parent.
    if let Some((rel_expr_node_index, _)) =
        ast.get_parent_node_of_type_on_left_branch(node_index, &[RelExpr], &[])
    {
        // If parent is a ForStat, add FOR_COND_MARKER.
        let grand_parent_node_index = ast.get_parent(rel_expr_node_index).unwrap();
        if let ForStat = ast.get_node_type(grand_parent_node_index) {
            moon_code.push(FOR_COND_MARKER.to_string());
        }
    }

    // If the node has a AssignForStat parent.
    if let Some((assign_for_star_node_index, _)) =
        ast.get_parent_node_of_type_on_left_branch(node_index, &[AssignForStat], &[])
    {
        // If parent is a ForStat, add FOR_INCR_MARKER.
        let grand_parent_node_index = ast.get_parent(assign_for_star_node_index).unwrap();
        if let ForStat = ast.get_node_type(grand_parent_node_index) {
            moon_code.push(FOR_INCR_MARKER.to_string());
        }
    }

    let stat_block_node_index = if let StatBlock = ast.get_node_type(node_index) {
        Some(node_index)
    } else {
        // If the node has a StatBlock parent.
        match ast.get_parent_node_of_type_on_left_branch(node_index, &[StatBlock], &[]) {
            Some((stat_block_node_index, _)) => Some(stat_block_node_index),
            None => None,
        }
    };

    if let Some(stat_block_node_index) = stat_block_node_index {
        let grand_parent_node_index = ast.get_parent(stat_block_node_index).unwrap();
        // If it is a ForStat, add FOR_LOOP_MARKER.
        if let ForStat = ast.get_node_type(grand_parent_node_index) {
            moon_code.push(FOR_LOOP_MARKER.to_string());
        }
    }
}

pub fn push_at(moon_code: &mut Vec<String>, new_moon_code: Vec<String>, marker: &str) {
    let index = get_marker_index(moon_code, marker);
    moon_code.splice(index..=index, new_moon_code.into_iter());
}

pub fn get_marker_index(moon_code: &mut Vec<String>, marker: &str) -> usize {
    moon_code.len()
        - 1
        - moon_code
            .iter()
            .rev()
            .position(|line| *line == marker)
            .unwrap()
}

pub fn load_inst_addresse(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {
    use Reference::*;

    if let Some((function_node_index, _)) =
        ast.get_parent_node_of_type(node_index, &[NodeType::FuncDef], &[])
    {
        let function_memory_table_index = ast.get_memory_table_index(function_node_index);
        let inst_addr_offset = get_inst_addr_offset(ast, function_memory_table_index).unwrap();
        let register1 = ast.register_pool.pop();
        add_comment(moon_code, format!("Loading instance address to r{}", 12));
        // Size is 4 because it is an address.
        load(ast, moon_code, register1, Offset(inst_addr_offset));

        moon_code.push(format!("{}add r12,r0,r{}", INDENT, register1));
        ast.register_pool.push(register1);
    }
}

pub fn get_reference(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) -> Reference {
    use NodeType::*;
    use Reference::*;

    // If it is a VarElementList, handle differently.
    if let NodeType::VarElementList = ast.get_node_type(node_index) {
        // FIXME: This code seems to work with single inheritence since the
        // inherited  is at the start of the memory table.
        let child_nodes = ast.get_children(node_index);
        let register1 = ast.register_pool.pop();
        let last_node_position = child_nodes.len() - 1;
        let mut reference_register: usize = 14;
        let mut offset: isize = 0;
        let mut size = 4;
        let mut include_array = false;
        for (position, child_index) in child_nodes.into_iter().enumerate() {
            let child_node_type = ast.get_node_type(child_index);
            match child_node_type {
                DataMember => {
                    // Get symbol entry associated with data member.
                    let symbol_entry_index = ast.get_symbol_entry_index(child_index);
                    if position == 0 {
                        //  Check if symbol entry is a member variable.
                        if get_class_node_of_symbol_entry(ast, symbol_entry_index).is_some() {
                            // Set register to instance variable.
                            load_inst_addresse(ast, moon_code, node_index);
                            reference_register = 12;
                        }
                    }

                    let memory_entry = ast
                        .memory_table_arena
                        .get_table_entry(get_memory_from_symbol(ast, child_index).unwrap());
                    if include_array {
                        moon_code.push(format!(
                            "{}addi r{},r{},{}",
                            INDENT,
                            register1,
                            register1,
                            memory_entry.get_offset()
                        ));
                    } else {
                        offset += memory_entry.get_offset();
                    }

                    let index_list_node = ast.get_child(child_index, 1);

                    if ast.has_memory_entry_index(index_list_node) {
                        let memory_entry_index = ast.get_memory_entry_index(index_list_node);
                        if !include_array {
                            add_comment(
                                moon_code,
                                "Array -> Switching to offset register.".to_string(),
                            );
                            moon_code.push(format!(
                                "{}addi r{},r{},{}",
                                INDENT, register1, reference_register, offset
                            ));
                            include_array = true;
                        }
                        moon_code.push(format!(
                            "{}addi r{},r{},{}",
                            INDENT,
                            register1,
                            register1,
                            memory_entry.get_size()
                        ));
                        let index_list_memory_entry =
                            ast.memory_table_arena.get_table_entry(memory_entry_index);
                        let register2 = ast.register_pool.pop();
                        moon_code.push(format!(
                            "{}lw r{},{}(r14)",
                            INDENT,
                            register2,
                            index_list_memory_entry.get_offset()
                        ));
                        moon_code.push(format!(
                            "{}add r{},r{},r{}",
                            INDENT, register1, register1, register2
                        ));
                        ast.register_pool.push(register2);
                    }

                    // Finally, if last element, set the size to output.
                    // If not, adjust offset.
                    let symbol_entry = ast.symbol_table_arena.get_table_entry(symbol_entry_index);
                    let symbol_type = symbol_entry.get_symbol_type().unwrap();
                    if position != last_node_position {
                        // Here, we correctly assume that since it is not the last element,
                        // it should not be an array.
                        assert!(
                            !symbol_type.get_dimension_list().is_empty()
                                == ast.has_memory_entry_index(index_list_node)
                        );
                        let type_size = get_size(ast, &symbol_type.remove_dimensions()).unwrap();
                        if include_array {
                            moon_code.push(format!(
                                "{}addi r{},r{},{}",
                                INDENT, register1, register1, type_size as isize
                            ));
                        } else {
                            offset += type_size as isize;
                        }
                    } else {
                        // The size depends on whether indices are specified.
                        size = if ast.has_memory_entry_index(index_list_node) {
                            get_size(ast, &symbol_type.remove_dimensions()).unwrap()
                        } else {
                            get_size(ast, &symbol_type).unwrap()
                        };
                    }
                }
                // If it encounters a single function call, return the offset of the return
                // value TempVar.
                // We know this is the last element of the VarElementList.
                FunctionCall => {
                    let entry_index = ast.get_memory_entry_index(child_index);
                    let memory_entry = ast.memory_table_arena.get_table_entry(entry_index);
                    include_array = false;
                    size = memory_entry.get_size();
                    offset = memory_entry.get_offset();
                }
                _ => unreachable!(),
            }
        }
        if include_array {
            SizeAndOffsetAndRegister(size, 0, register1)
        } else {
            ast.register_pool.push(register1);
            SizeAndOffsetAndRegister(size, offset, reference_register)
        }
    } else {
        let entry_index = ast.get_memory_entry_index(node_index);
        let memory_entry = ast.memory_table_arena.get_table_entry(entry_index);
        return SizeAndOffsetAndRegister(memory_entry.get_size(), memory_entry.get_offset(), 14);
    }
}

/// Load the content of the node to the specified register.
pub fn load(
    ast: &mut AST,
    moon_code: &mut Vec<String>,
    register_index: usize,
    reference: Reference,
) {
    let (size, offset, address_register) = reference.get(ast, moon_code);
    assert!(size == 4);
    load_from_offset_register(moon_code, register_index, offset, address_register);
    ast.register_pool.push(address_register);
}

pub fn load_from_offset_register(
    moon_code: &mut Vec<String>,
    register_index: usize,
    offset: isize,
    effective_address_register: usize,
) {
    moon_code.push(format!(
        "{}lw r{},{}(r{})",
        INDENT, register_index, offset, effective_address_register
    ));
}

/// Load the content of the register to the specified node.
pub fn store(
    ast: &mut AST,
    moon_code: &mut Vec<String>,
    reference: Reference,
    register_index: usize,
) {
    let (size, offset, address_register) = reference.get(ast, moon_code);
    assert!(size == 4);
    store_at_offset_register(moon_code, offset, address_register, register_index);
    ast.register_pool.push(address_register);
}

pub fn store_at_offset_register(
    moon_code: &mut Vec<String>,
    offset: isize,
    effective_address_register: usize,
    register_index: usize,
) {
    moon_code.push(format!(
        "{}sw {}(r{}),r{}",
        INDENT, offset, effective_address_register, register_index
    ));
}

pub fn add_label(moon_code: &mut Vec<String>, label: String) {
    moon_code.push(format!("{:<10}nop", label));
}

pub fn add_comment(moon_code: &mut Vec<String>, comment: String) {
    moon_code.push(format!("{}% processing: {}", INDENT, comment));
}

pub fn addi(
    moon_code: &mut Vec<String>,
    lhs_register_index: usize,
    rhs_register_index: usize,
    immediate_operand: isize,
) {
    moon_code.push(format!(
        "{}addi r{},r{},{}",
        INDENT, lhs_register_index, rhs_register_index, immediate_operand
    ));
}

pub fn addi_label(
    moon_code: &mut Vec<String>,
    lhs_register_index: usize,
    rhs_register_index: usize,
    label: String,
) {
    moon_code.push(format!(
        "{}addi r{},r{},{}",
        INDENT, lhs_register_index, rhs_register_index, label
    ));
}

pub fn subi(
    moon_code: &mut Vec<String>,
    lhs_register_index: usize,
    rhs_register_index: usize,
    immediate_operand: isize,
) {
    moon_code.push(format!(
        "{}subi r{},r{},{}",
        INDENT, lhs_register_index, rhs_register_index, immediate_operand
    ));
}

pub fn add_instruction(moon_code: &mut Vec<String>, label: Option<String>, instruction: String) {
    match label {
        Some(label) => {
            moon_code.push(format!("{:<10}{}", label, instruction));
        }
        None => {
            moon_code.push(format!("{}{}", INDENT, instruction));
        }
    }
}

pub fn copy_var(
    ast: &mut AST,
    moon_code: &mut Vec<String>,
    lhs_reference: Reference,
    rhs_reference: Reference,
) {
    let (lhs_size, lhs_offset, lhs_address_register) = lhs_reference.get(ast, moon_code);
    let (rhs_size, rhs_offset, rhs_address_register) = rhs_reference.get(ast, moon_code);

    assert!(lhs_size == rhs_size);

    let register1 = ast.register_pool.pop();

    for inner_offset in (0..lhs_size as isize).step_by(4) {
        load_from_offset_register(
            moon_code,
            register1,
            rhs_offset + inner_offset,
            rhs_address_register,
        );
        store_at_offset_register(
            moon_code,
            lhs_offset + inner_offset,
            lhs_address_register,
            register1,
        );
    }

    ast.register_pool.push(lhs_address_register);
    ast.register_pool.push(rhs_address_register);

    ast.register_pool.push(register1);
}

pub fn get_inst_reference(
    ast: &mut AST,
    moon_code: &mut Vec<String>,
    node_index: usize,
) -> Reference {
    use Reference::*;

    let parent_index = ast.get_parent(node_index).unwrap();
    let child_nodes = ast.get_children(parent_index);

    // Find the position of the FunctionCall in the VarElementList.
    let function_call_position = child_nodes
        .iter()
        .position(|&child_index| child_index == node_index)
        .unwrap();

    // If there is no DataMember, return the address of the current instance.
    if function_call_position == 0 {
        let (_, function_memory_table_index) =
            get_function_node_index_and_memory_table_index(ast, node_index).unwrap();
        return Offset(get_inst_addr_offset(ast, function_memory_table_index).unwrap());
    }

    // FIXME: This is a partial copy of the get_reference() function.
    // Maybe there is a better way to do this.
    let register1 = ast.register_pool.pop();
    let mut reference_register: usize = 14;
    let mut offset: isize = 0;
    let mut include_array = false;
    for (position, child_index) in child_nodes.into_iter().enumerate() {
        // If get at the FunctionCall, break.
        if position == function_call_position {
            break;
        }
        // We can assume we have a DataMember.
        // Get symbol entry associated with DataMember.
        let symbol_entry_index = ast.get_symbol_entry_index(child_index);
        if position == 0 {
            //  Check if symbol entry is a member variable.
            if get_class_node_of_symbol_entry(ast, symbol_entry_index).is_some() {
                // Set register to instance variable.
                load_inst_addresse(ast, moon_code, node_index);
                reference_register = 12;
            }
        }

        let memory_entry = ast
            .memory_table_arena
            .get_table_entry(get_memory_from_symbol(ast, child_index).unwrap());
        if include_array {
            moon_code.push(format!(
                "{}addi r{},r{},{}",
                INDENT,
                register1,
                register1,
                memory_entry.get_offset()
            ));
        } else {
            offset += memory_entry.get_offset();
        }

        let index_list_node = ast.get_child(child_index, 1);

        if ast.has_memory_entry_index(index_list_node) {
            let memory_entry_index = ast.get_memory_entry_index(index_list_node);
            if !include_array {
                add_comment(
                    moon_code,
                    "Array -> Switching to offset register.".to_string(),
                );
                moon_code.push(format!(
                    "{}addi r{},r{},{}",
                    INDENT, register1, reference_register, offset
                ));
                include_array = true;
            }
            moon_code.push(format!(
                "{}addi r{},r{},{}",
                INDENT,
                register1,
                register1,
                memory_entry.get_size()
            ));
            let index_list_memory_entry =
                ast.memory_table_arena.get_table_entry(memory_entry_index);
            let register2 = ast.register_pool.pop();
            moon_code.push(format!(
                "{}lw r{},{}(r14)",
                INDENT,
                register2,
                index_list_memory_entry.get_offset()
            ));
            moon_code.push(format!(
                "{}add r{},r{},r{}",
                INDENT, register1, register1, register2
            ));
            ast.register_pool.push(register2);
        }

        let symbol_entry = ast.symbol_table_arena.get_table_entry(symbol_entry_index);
        let symbol_type = symbol_entry.get_symbol_type().unwrap();
        // Here, we correctly assume that no element should be an array.
        assert!(
            !symbol_type.get_dimension_list().is_empty()
                == ast.has_memory_entry_index(index_list_node)
        );
        let type_size = get_size(ast, &symbol_type.remove_dimensions()).unwrap();
        if include_array {
            moon_code.push(format!(
                "{}addi r{},r{},{}",
                INDENT, register1, register1, type_size as isize
            ));
        } else {
            offset += type_size as isize;
        }
    }
    if include_array {
        SizeAndOffsetAndRegister(4, 0, register1)
    } else {
        ast.register_pool.push(register1);
        SizeAndOffsetAndRegister(4, offset, reference_register)
    }
}
