use crate::ast_node::*;
use crate::ast_visitor::*;
use crate::code_generation_common::*;
use crate::code_generation_error::*;
use crate::language::*;
use crate::memory_table::*;
use crate::symbol_table::*;

use std::collections::HashMap;

const INDENT: &str = "          ";
const ENTRY_MARKER: &str = "% ENTRY MARKER";
const FUNCTION_MARKER: &str = "% FUNCTION MARKER";
const IF_MARKER: &str = "% IF MARKER";
const ELSE_MARKER: &str = "% ELSE MARKER";
const FOR_COND_MARKER: &str = "% FOR COND MARKER";
const FOR_INCR_MARKER: &str = "% FOR INCR MARKER";
const FOR_LOOP_MARKER: &str = "% FOR LOOP MARKER";

pub fn code_generator_visitor(ast: &mut AST) -> Vec<String> {
    use NodeType::*;
    let mut moon_code: Vec<String> = Vec::new();
    let mut semantic_actions: SemanticActionMap<String> = HashMap::new();
    semantic_actions.insert(MainFuncBody, main_func_body);
    semantic_actions.insert(FuncDef, func_def);
    semantic_actions.insert(FunctionCall, function_call);

    semantic_actions.insert(DataMember, data_member);

    // Statements
    semantic_actions.insert(IfStat, if_stat);
    semantic_actions.insert(ForStat, for_stat);
    semantic_actions.insert(WriteStat, write_stat);
    semantic_actions.insert(ReturnStat, return_stat);

    // Binary operation
    semantic_actions.insert(RelExpr, binary_op);
    semantic_actions.insert(AddOp, binary_op);
    semantic_actions.insert(MultOp, binary_op);
    // Unary operation
    semantic_actions.insert(AssignStat, unary_op);
    semantic_actions.insert(AssignForStat, unary_op);
    semantic_actions.insert(Not, unary_op);
    semantic_actions.insert(Sign, unary_op);

    // Variables
    semantic_actions.insert(Num, num);

    // Marker
    semantic_actions.insert(Type, leaf);
    semantic_actions.insert(Id, leaf);
    semantic_actions.insert(Idi, leaf);
    semantic_actions.insert(StatBlock, leaf);

    ast_traversal(ast, &mut moon_code, &semantic_actions);
    moon_code
}

fn main_func_body(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {
    // Add entry marker.
    add_entry_marker(ast, moon_code, node_index);

    // Code to add at entry marker.
    let mut entry_moon_code: Vec<String> = Vec::new();
    // Add entry point.
    add_instruction(
        &mut entry_moon_code,
        Some("mainfunc".to_string()),
        "entry".to_string(),
    );
    // Set stack frame pointer.
    addi_label(&mut entry_moon_code, 14, 0, "topaddr".to_string());
    push_at(moon_code, entry_moon_code, ENTRY_MARKER);

    // Add halt program
    add_instruction(moon_code, Some("endmain".to_string()), "hlt".to_string());
    // Add buffer for console output.
    add_comment(
        moon_code,
        "buffer space used for console output".to_string(),
    );
    add_instruction(moon_code, Some("buf".to_string()), "res 20".to_string());
}

fn func_def(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {
    // Get function name and unique index.
    let func_name = get_func_name(ast, node_index);
    let function_index = get_funtion_symbol_table_index(ast, node_index);
    let func_label = format!("func{}", function_index);
    let end_func_label = format!("endfunc{}", function_index);

    // Get return address variable offset.
    let memory_table_index = ast.get_element(node_index).memory_table.unwrap();
    let return_addr_offset = get_return_addr_offset(ast, memory_table_index);

    // Code to add at function marker.
    let mut function_moon_code: Vec<String> = Vec::new();
    add_comment(
        &mut function_moon_code,
        format!("function definition: {} ({})", func_name, func_label),
    );
    // Add function unique label.
    add_label(&mut function_moon_code, func_label.clone());
    // Store return address.
    store_at_offset(&mut function_moon_code, return_addr_offset, 15);
    push_at(moon_code, function_moon_code, FUNCTION_MARKER);

    add_label(moon_code, format!("{}", end_func_label));
    // Load return address.
    load_from_offset(moon_code, 15, return_addr_offset);
    // Jump back to calling function.
    moon_code.push(format!("{}jr r15", INDENT));
    moon_code.push("".to_string());
}

fn function_call(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {
    // Get function name and unique index.
    let func_name = get_func_name(ast, node_index);
    let function_index = get_funtion_symbol_table_index(ast, node_index);
    let func_label = format!("func{}", function_index);

    // Get stack and return variable offsets.
    let current_stack_frame_size = get_current_stack_frame_offset(ast, node_index);
    let called_function_memory_table_index =
        get_memory_table_index_from_symbol(ast, node_index).unwrap();
    let return_var_offset = get_return_var_offset(ast, called_function_memory_table_index);

    add_comment(
        moon_code,
        format!("Function call to {} ({})", func_name, func_label),
    );

    let register1 = ast.register_pool.pop();
    let register2 = ast.register_pool.pop();

    // If member function, set instance address.
    if let Some(inst_addr_var_offset) =
        get_inst_addr_offset(ast, called_function_memory_table_index)
    {
        add_comment(moon_code, "Setting instance address".to_string());
        let inst_offset = get_inst_offset(ast, node_index).unwrap();
        addi(moon_code, register1, 14, inst_offset);
        store_at_offset(
            moon_code,
            current_stack_frame_size + inst_addr_var_offset,
            register1,
        );
    }

    // Pass parameters.
    for (&argument_node_index, param_entry_index) in ast
        .get_children_of_child(node_index, 1)
        .iter()
        .rev()
        .zip(get_params(ast, called_function_memory_table_index))
    {
        let param_entry = ast
            .memory_table_arena
            .get_table_entry(param_entry_index)
            .clone();
        add_comment(
            moon_code,
            format!(
                "{} = {}",
                param_entry.get_name(),
                get_var_name(ast, argument_node_index)
            ),
        );
        load_node(ast, moon_code, register1, argument_node_index);
        store_at_offset(
            moon_code,
            current_stack_frame_size + param_entry.get_offset(),
            register1,
        );
    }

    // Move stack frame pointer to function stack frame.
    add_comment(
        moon_code,
        format!("Move stack frame pointer by: {}", current_stack_frame_size),
    );
    addi(moon_code, 14, 14, current_stack_frame_size);

    // Jump to function label.
    add_comment(moon_code, format!("jump to: {}", func_label));
    moon_code.push(format!("{}jl r15,{}", INDENT, func_label));

    // Once back, move back stack frame pointer to current stack frame.
    add_comment(
        moon_code,
        format!(
            "Move back stack frame pointer by: {}",
            current_stack_frame_size
        ),
    );
    subi(moon_code, 14, 14, current_stack_frame_size);

    // Store return value in its temporary variable.
    // Note that the temporary varible for the return value is on the current node.
    add_comment(
        moon_code,
        format!("{} = return value", get_var_name(ast, node_index)),
    );
    load_from_offset(
        moon_code,
        register1,
        current_stack_frame_size + return_var_offset,
    );
    store_node(ast, moon_code, node_index, register1);

    ast.register_pool.push(register1);
    ast.register_pool.push(register2);
}

fn data_member(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {
    use SymbolKind::*;

    // Get the IndexList node.
    let index_list_node = ast.get_child(node_index, 1);

    // Only proceed if the IndexList node has a memory entry.
    if let Some(_) = ast.get_element(index_list_node).memory_table_entry {
        // Get symbol entry associated with data member.
        let symbol_entry_index = ast.get_element(node_index).symbol_table_entry.unwrap();
        let symbol_entry = ast.symbol_table_arena.get_table_entry(symbol_entry_index);
        let (dimension_list, type_size) = match &symbol_entry.kind {
            Parameter(symbol_type) | Variable(symbol_type) | For(symbol_type) => (
                symbol_type.get_dimension_list(),
                get_size(ast, &symbol_type.remove_dimensions()).unwrap(),
            ),
            _ => unreachable!(),
        };

        let register1 = ast.register_pool.pop();
        let register2 = ast.register_pool.pop();
        let register3 = ast.register_pool.pop();
        add_comment(
            moon_code,
            format!("{} = array offset", get_var_name(ast, index_list_node)),
        );
        // Initialize the accumulator register that will hold the array offset.
        subi(moon_code, register1, 0, type_size as isize);
        //subi(moon_code, register1, 0, 0);

        for (position, index_node) in ast.get_children(index_list_node).into_iter().enumerate() {
            // Get the current dimension multiplier.
            let array_offset: usize =
                dimension_list.iter().skip(position + 1).product::<usize>() * type_size;
            // Load the current index value.
            load_node(ast, moon_code, register2, index_node);
            // Multiply the value and multiplier.
            moon_code.push(format!(
                "{}muli r{},r{},{}",
                INDENT, register3, register2, array_offset,
            ));
            // Sub to accumulator register.
            moon_code.push(format!(
                "{}sub r{},r{},r{}",
                INDENT, register1, register1, register3,
            ));
        }
        // Store the offset in the TempVar.
        store_node(ast, moon_code, index_list_node, register1);

        ast.register_pool.push(register1);
        ast.register_pool.push(register2);
        ast.register_pool.push(register3);
    }
}

fn binary_op(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {
    use NodeType::*;
    use OperatorType::*;

    let (rhs_node_index1, op_node_index, rhs_node_index2) =
        match ast.get_element(node_index).node_type {
            RelExpr => (
                ast.get_child(node_index, 0),
                ast.get_child(node_index, 1),
                ast.get_child(node_index, 2),
            ),
            AddOp | MultOp => (
                ast.get_child(node_index, 0),
                node_index,
                ast.get_child(node_index, 1),
            ),
            _ => unreachable!(),
        };

    // Get operation
    let operator = match ast
        .get_element(op_node_index)
        .token
        .as_ref()
        .unwrap()
        .token_type
    {
        TokenType::Operator(operator) => operator,
        _ => unreachable!(),
    };

    let operator_lexeme = ast
        .get_element(op_node_index)
        .token
        .as_ref()
        .unwrap()
        .lexeme
        .clone()
        .unwrap();

    // FIXME: Need to allow other operation than +
    // FIXME: Next 3 lines assume very simple VarElementList.
    let lhs_node_index = node_index;
    // let lhs_memory_entry_index = get_memory_entry(ast, lhs_node_index);
    let lhs_variable_name = get_var_name(ast, lhs_node_index);
    // let rhs_memory_entry_index1 = ast.get_element(rhs_node_index1).memory_table_entry.unwrap();
    let rhs_variable_name1 = get_var_name(ast, rhs_node_index1);
    // let rhs_memory_entry_index2 = ast.get_element(rhs_node_index2).memory_table_entry.unwrap();
    let rhs_variable_name2 = get_var_name(ast, rhs_node_index2);

    // Then, do the processing of this nodes' visitor
    // allocate registers to this subcomputation
    let register1 = ast.register_pool.pop();
    let register2 = ast.register_pool.pop();
    let register3 = ast.register_pool.pop();
    // generate code

    add_comment(
        moon_code,
        format!(
            "{} := {} {} {}",
            lhs_variable_name, rhs_variable_name1, operator_lexeme, rhs_variable_name2,
        ),
    );

    // load the values of the operands into registers
    load_node(ast, moon_code, register1, rhs_node_index1);
    load_node(ast, moon_code, register2, rhs_node_index2);

    match operator {
        LT => {
            // < operands
            moon_code.push(format!(
                "{}clt r{},r{},r{}",
                INDENT, register3, register1, register2,
            ));
        }
        LEq => {
            // <= operands
            moon_code.push(format!(
                "{}cle r{},r{},r{}",
                INDENT, register3, register1, register2,
            ));
        }
        NEq => {
            // != operands
            moon_code.push(format!(
                "{}cne r{},r{},r{}",
                INDENT, register3, register1, register2,
            ));
        }
        GT => {
            // > operands
            moon_code.push(format!(
                "{}cgt r{},r{},r{}",
                INDENT, register3, register1, register2,
            ));
        }
        GEq => {
            // >= operands
            moon_code.push(format!(
                "{}cge r{},r{},r{}",
                INDENT, register3, register1, register2,
            ));
        }
        Eq => {
            // == operands
            moon_code.push(format!(
                "{}ceq r{},r{},r{}",
                INDENT, register3, register1, register2,
            ));
        }
        Addition => {
            // add operands
            moon_code.push(format!(
                "{}add r{},r{},r{}",
                INDENT, register3, register1, register2,
            ));
        }
        Subtraction => {
            // substract operands
            moon_code.push(format!(
                "{}sub r{},r{},r{}",
                INDENT, register3, register1, register2,
            ));
        }
        Multiplication => {
            // multiply operands
            moon_code.push(format!(
                "{}mul r{},r{},r{}",
                INDENT, register3, register1, register2,
            ));
        }
        Division => {
            // divide operands
            moon_code.push(format!(
                "{}div r{},r{},r{}",
                INDENT, register3, register1, register2,
            ));
        }
        And => {
            // && operands
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
            // || operands
            let or_index = ast.register_pool.get_and();
            let zero = format!("zeroor{}", or_index);
            let end = format!("endor{}", or_index);
            moon_code.push(format!("{}bz r{},{}", INDENT, register1, zero,));
            moon_code.push(format!("{}bz r{},{}", INDENT, register2, zero,));
            addi(moon_code, register3, 0, 1);
            moon_code.push(format!("{}j {}", INDENT, end));
            add_label(moon_code, zero.clone());
            addi(moon_code, register3, 0, 0);
            add_label(moon_code, end.clone());
        }
        _ => unreachable!(),
    }

    // assign the result into a temporary variable (assumed to have been previously created by the symbol table generator)
    store_node(ast, moon_code, lhs_node_index, register3);

    // deallocate the registers
    ast.register_pool.push(register1);
    ast.register_pool.push(register2);
    ast.register_pool.push(register3);
}

fn unary_op(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {
    //use NodeType::*;
    use OperatorType::*;

    let (lhs_node_index, operator, operator_lexeme, rhs_node_index) =
        match ast.get_element(node_index).node_type {
            NodeType::AssignStat | NodeType::AssignForStat => (
                ast.get_child(node_index, 0),
                Assignment,
                "".to_string(),
                ast.get_child(node_index, 1),
            ),
            NodeType::Not => (
                node_index,
                Not,
                ast.get_element(node_index)
                    .token
                    .as_ref()
                    .unwrap()
                    .lexeme
                    .clone()
                    .unwrap(),
                ast.get_child(node_index, 0),
            ),
            NodeType::Sign => (
                node_index,
                Subtraction,
                ast.get_element(node_index)
                    .token
                    .as_ref()
                    .unwrap()
                    .lexeme
                    .clone()
                    .unwrap(),
                ast.get_child(node_index, 0),
            ),
            _ => unreachable!(),
        };

    // FIXME: Need to allow other operation than +
    // FIXME: Next 3 lines assume very simple VarElementList.
    //let lhs_node_index = node_index;
    // let lhs_memory_entry_index = get_memory_entry(ast, lhs_node_index);
    let lhs_variable_name = get_var_name(ast, lhs_node_index);
    // let rhs_memory_entry_index1 = ast.get_element(rhs_node_index1).memory_table_entry.unwrap();
    let rhs_variable_name = get_var_name(ast, rhs_node_index);

    // Then, do the processing of this nodes' visitor
    // allocate registers to this subcomputation
    let register1 = ast.register_pool.pop();
    let register2 = ast.register_pool.pop();
    let register3 = ast.register_pool.pop();
    // generate code

    add_comment(
        moon_code,
        format!(
            "{} := {} {}",
            lhs_variable_name, operator_lexeme, rhs_variable_name,
        ),
    );
    // load the values of the operands into registers
    load_node(ast, moon_code, register1, rhs_node_index);

    match operator {
        Assignment => {
            // assign the result into a temporary variable (assumed to have been previously created by the symbol table generator)
            store_node(ast, moon_code, lhs_node_index, register1);
        }
        Subtraction => {
            moon_code.push(format!("{}sub r{},r0,r{}", INDENT, register2, register1,));
            // assign the result into a temporary variable (assumed to have been previously created by the symbol table generator)
            store_node(ast, moon_code, lhs_node_index, register2);
        }
        Not => {
            // ! operands
            let not_index = ast.register_pool.get_not();
            let zero = format!("zeroor{}", not_index);
            let end = format!("end0r{}", not_index);
            moon_code.push(format!("{}not r{},r{}", INDENT, register2, register1));
            moon_code.push(format!("{}bz r{},{}", INDENT, register2, zero,));
            addi(moon_code, register3, 0, 1);
            moon_code.push(format!("{}j {}", INDENT, end));
            add_label(moon_code, zero.clone());
            addi(moon_code, register3, 0, 0);
            add_label(moon_code, end.clone());
            // assign the result into a temporary variable (assumed to have been previously created by the symbol table generator)
            store_node(ast, moon_code, lhs_node_index, register3);
        }
        _ => unreachable!(),
    }

    // deallocate the registers
    ast.register_pool.push(register1);
    ast.register_pool.push(register2);
    ast.register_pool.push(register3);
}

fn num(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {
    add_entry_marker(ast, moon_code, node_index);
    add_if_marker(ast, moon_code, node_index);
    add_for_marker(ast, moon_code, node_index);

    let data = ast
        .get_element(node_index)
        .token
        .as_ref()
        .unwrap()
        .lexeme
        .as_ref()
        .unwrap()
        .clone();
    // Then, do the processing of this nodes' visitor
    // create a local variable and allocate a register to this subcomputation
    let register1 = ast.register_pool.pop();
    // generate code
    add_comment(
        moon_code,
        format!("{} := {}", get_var_name(ast, node_index), data),
    );
    // create a value corresponding to the literal value
    addi(moon_code, register1, 0, data.parse().unwrap());
    // assign this value to a temporary variable (assumed to have been previously created by the symbol table generator)
    store_node(ast, moon_code, node_index, register1);
    // deallocate the register for the current node
    ast.register_pool.push(register1);
}

fn if_stat(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {
    let cond_node_index = ast.get_child(node_index, 0);
    let cond_variable_name = get_var_name(ast, cond_node_index);

    let register1 = ast.register_pool.pop();

    let if_index = ast.register_pool.get_not();
    let elseif = format!("elseif{}", if_index);
    let endif = format!("endif{}", if_index);

    // ADD THIS BEFORE IF BLOCK
    let mut if_moon_code: Vec<String> = Vec::new();
    add_comment(
        &mut if_moon_code,
        format!("if({}), index: {}", cond_variable_name, if_index),
    );
    load_node(ast, &mut if_moon_code, register1, cond_node_index);

    if_moon_code.push(format!("{}bz r{},{}", INDENT, register1, elseif));
    ast.register_pool.push(register1);
    /////

    push_at(moon_code, if_moon_code, IF_MARKER);

    // ADD THIS BEFORE ELSE BLOCK
    let mut else_moon_code: Vec<String> = Vec::new();
    else_moon_code.push(format!("{}j {}", INDENT, endif));
    add_label(&mut else_moon_code, elseif.clone());
    /////

    push_at(moon_code, else_moon_code, ELSE_MARKER);
    add_label(moon_code, endif.clone());
}

fn for_stat(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {
    let assign_node_index = node_index;
    let assign_variable_name = get_var_name(ast, assign_node_index);
    let cond_node_index = ast.get_child(node_index, 3);

    let register1 = ast.register_pool.pop();
    //let register2 = ast.register_pool.pop();
    //let register3 = ast.register_pool.pop();

    let for_index = ast.register_pool.get_for();
    let condfor = format!("condfor{}", for_index);
    let incrfor = format!("incrfor{}", for_index);
    let loopfor = format!("loopfor{}", for_index);
    let endfor = format!("endfor{}", for_index);

    // ADD THIS BEFORE FOR CONDITION
    let mut cond_moon_code: Vec<String> = Vec::new();

    add_comment(
        &mut cond_moon_code,
        format!("for_cond({}), index: {}", assign_variable_name, for_index),
    );

    // Assignment
    let rhs_node_index = ast.get_child(node_index, 2);
    let rhs_variable_name = get_var_name(ast, rhs_node_index);

    add_comment(
        &mut cond_moon_code,
        format!("{} := {}", assign_variable_name, rhs_variable_name,),
    );

    load_node(ast, &mut cond_moon_code, register1, rhs_node_index);
    store_node(ast, &mut cond_moon_code, assign_node_index, register1);

    // Conditional label
    add_label(&mut cond_moon_code, condfor.clone());

    ast.register_pool.push(register1);
    //ast.register_pool.push(register2);

    push_at(moon_code, cond_moon_code, FOR_COND_MARKER);

    // ADD THIS BEFORE FOR INCREMENT
    let register1 = ast.register_pool.pop();

    let mut incr_moon_code: Vec<String> = Vec::new();

    add_comment(
        &mut incr_moon_code,
        format!("for_incr({}), index: {}", assign_variable_name, for_index),
    );

    load_node(ast, &mut incr_moon_code, register1, cond_node_index);

    incr_moon_code.push(format!("{}bz r{},{}", INDENT, register1, endfor));
    incr_moon_code.push(format!("{}j {}", INDENT, loopfor));
    add_label(&mut incr_moon_code, incrfor.clone());

    ast.register_pool.push(register1);

    push_at(moon_code, incr_moon_code, FOR_INCR_MARKER);

    // ADD THIS BEFORE FOR STATBLOCK
    let mut loop_moon_code: Vec<String> = Vec::new();
    loop_moon_code.push(format!("{}j {}", INDENT, condfor));
    add_label(&mut loop_moon_code, loopfor.clone());

    push_at(moon_code, loop_moon_code, FOR_LOOP_MARKER);

    // ADD THIS AFTER THE LOOP.
    moon_code.push(format!("{}j {}", INDENT, incrfor));
    add_label(moon_code, endfor.clone());
}

fn write_stat(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {
    let child_node_index = ast.get_child(node_index, 0);
    let child_variable_name = get_var_name(ast, child_node_index);
    let function_stack_frame_size = get_current_stack_frame_offset(ast, node_index);
    // Then, do the processing of this nodes' visitor
    // create a local variable and allocate a register to this subcomputation
    let register1 = ast.register_pool.pop();
    //generate code

    add_comment(moon_code, format!("write({})", child_variable_name,));

    // put the value to be printed into a register
    load_node(ast, moon_code, register1, child_node_index);
    add_comment(moon_code, "Put value on stack".to_string());

    // make the stack frame pointer point to the called function's stack frame
    // FIXME: Not sure about the size to add here.
    addi(moon_code, 14, 14, function_stack_frame_size);
    // copy the value to be printed in the called function's stack frame
    store_at_offset(moon_code, -8, register1);
    add_comment(moon_code, "Put buffer address on the stack".to_string());
    addi_label(moon_code, register1, 0, "buf".to_string());
    store_at_offset(moon_code, -12, register1);
    add_comment(moon_code, "Convert int to string for output".to_string());
    moon_code.push(format!("{}jl r15,intstr", INDENT));
    // receive the return value in r13
    store_at_offset(moon_code, -8, 13);
    add_comment(moon_code, "Output to console".to_string());
    // putstr is expecting the address of the string in r1
    moon_code.push(format!("{}jl r15,putstr", INDENT));
    // make the stack frame pointer point back to the current function's stack frame
    subi(moon_code, 14, 14, function_stack_frame_size);
    // deallocate local register
    ast.register_pool.push(register1);
}

fn return_stat(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {
    let (function_node_index, function_memory_table_index) =
        get_function_node_index_and_memory_table_index(ast, node_index).unwrap();
    let end_func_label = format!(
        "endfunc{}",
        get_funtion_symbol_table_index(ast, function_node_index)
    );
    let return_var_offset = get_return_var_offset(ast, function_memory_table_index);

    // Setting return value.
    let register1 = ast.register_pool.pop();
    let argument_node_index = ast.get_child(node_index, 0);
    add_comment(
        moon_code,
        format!("return value = {}", get_var_name(ast, argument_node_index)),
    );
    load_node(ast, moon_code, register1, argument_node_index);
    store_at_offset(moon_code, return_var_offset, register1);
    ast.register_pool.push(register1);

    // Jump at the end of function.
    moon_code.push(format!("{}j {}", INDENT, end_func_label));
}

fn leaf(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {
    add_entry_marker(ast, moon_code, node_index);
    add_function_marker(ast, moon_code, node_index);
    add_if_marker(ast, moon_code, node_index);
    add_for_marker(ast, moon_code, node_index);
}

fn get_var_name(ast: &AST, node_index: usize) -> String {
    let node_type = ast.get_element(node_index).node_type;
    if let NodeType::VarElementList = node_type {
        let last_child_index = *ast.get_children(node_index).iter().last().unwrap();
        ast.symbol_table_arena
            .get_table_entry(
                ast.get_element(last_child_index)
                    .symbol_table_entry
                    .unwrap(),
            )
            .name
            .clone()
    } else {
        // First try to get it from the memory entry and then from the table.
        if let Some(entry_index) = ast.get_element(node_index).memory_table_entry {
            let memory_entry = ast.memory_table_arena.get_table_entry(entry_index);
            memory_entry.get_name()
        } else if let Some(table_index) = ast.get_element(node_index).memory_table {
            let memory_table = ast.memory_table_arena.get_table(table_index);
            memory_table.get_name()
        } else {
            unreachable!("Node has neither a memory table nor an memory entry.");
        }
    }
}

fn get_func_name(ast: &AST, node_index: usize) -> String {
    // First try to get it from the memory entry and then from the table.
    if let Some(entry_index) = ast.get_element(node_index).symbol_table_entry {
        let symbol_entry = ast.symbol_table_arena.get_table_entry(entry_index);
        symbol_entry.name.clone()
    } else if let Some(table_index) = ast.get_element(node_index).symbol_table {
        let symbol_table = ast.symbol_table_arena.get_table(table_index);
        symbol_table.name.clone()
    } else {
        unreachable!("Node has neither a memory table nor an memory entry.");
    }
}

fn get_funtion_symbol_table_index(ast: &AST, node_index: usize) -> usize {
    // First try to get it from the memory entry and then from the table.
    if let Some(entry_index) = ast.get_element(node_index).symbol_table_entry {
        let symbol_entry = ast.symbol_table_arena.get_table_entry(entry_index);
        symbol_entry.link.unwrap()
    } else if let Some(table_index) = ast.get_element(node_index).symbol_table {
        let symbol_table = ast.symbol_table_arena.get_table(table_index);
        symbol_table.index
    } else {
        unreachable!("Node has neither a symbol table nor an symbol entry with a link.");
    }
}

fn get_inst_offset(ast: &AST, node_index: usize) -> Option<isize> {
    let parent_index = ast.get_parent(node_index).unwrap();
    let child_nodes = ast.get_children(parent_index);
    let function_call_position = child_nodes
        .iter()
        .position(|&child_index| child_index == node_index)
        .unwrap();
    if function_call_position == 0 {
        let (_, function_memory_table_index) =
            get_function_node_index_and_memory_table_index(ast, node_index).unwrap();
        return get_inst_addr_offset(ast, function_memory_table_index);
    }
    let mut offset: isize = 0;
    // Note that contrary to when we acces data member, we return the offset
    // at the "top" of the instance variable because it is necessarily an object.
    for child_index in ast.get_children(parent_index) {
        if child_index == node_index {
            break;
        }
        let memory_entry = ast
            .memory_table_arena
            .get_table_entry(get_memory_from_symbol(ast, child_index).unwrap());
        offset += memory_entry.get_offset() + memory_entry.get_size();
    }
    Some(offset)
}

fn get_return_var_offset(ast: &AST, memory_table_index_index: usize) -> isize {
    for &entry_index in ast
        .memory_table_arena
        .get_table_entries(memory_table_index_index)
    {
        let memory_entry = &ast.memory_table_arena.get_table_entry(entry_index);
        if memory_entry.is_return_var() {
            return memory_entry.get_offset();
        }
    }
    unreachable!("Memory table does not has a return variable.");
}

fn get_return_addr_offset(ast: &AST, memory_table_index_index: usize) -> isize {
    for &entry_index in ast
        .memory_table_arena
        .get_table_entries(memory_table_index_index)
    {
        let memory_entry = &ast.memory_table_arena.get_table_entry(entry_index);
        if memory_entry.is_return_addr() {
            return memory_entry.get_offset();
        }
    }
    unreachable!("Memory table does not has a return address.");
}

fn get_inst_addr_offset(ast: &AST, memory_table_index_index: usize) -> Option<isize> {
    for &entry_index in ast
        .memory_table_arena
        .get_table_entries(memory_table_index_index)
    {
        let memory_entry = &ast.memory_table_arena.get_table_entry(entry_index);
        if memory_entry.is_inst_addr() {
            return Some(memory_entry.get_offset());
        }
    }
    None
}

fn get_params(ast: &AST, memory_table_index_index: usize) -> Vec<usize> {
    ast.memory_table_arena
        .get_table_entries(memory_table_index_index)
        .iter()
        .filter(|&&entry_index| {
            ast.memory_table_arena
                .get_table_entry(entry_index)
                .is_param()
        })
        .cloned()
        .collect()
}

fn get_memory_from_symbol(ast: &AST, node_index: usize) -> Option<usize> {
    use NodeType::*;
    let symbol_entry_index = ast.get_element(node_index).symbol_table_entry.unwrap();
    let symbol_entry = ast.symbol_table_arena.get_table_entry(symbol_entry_index);
    let symbol_name = &symbol_entry.name;

    // Check if it is a member variable.
    if let Some(class_node_index) = get_class_node_of_symbol_entry(ast, symbol_entry_index) {
        return find_variable_memory_entry(ast, class_node_index, symbol_name);
    }

    // If not, it must be a local variable.
    if let Some((function_node_index, _)) =
        ast.get_parent_node_of_type(node_index, &[MainFuncBody, FuncDef], &[])
    {
        return find_variable_memory_entry(ast, function_node_index, symbol_name);
    }

    unreachable!();
}

fn find_variable_memory_entry(
    ast: &AST,
    node_index: usize,
    variable_name: &String,
) -> Option<usize> {
    let memory_table_index = ast.get_element(node_index).memory_table.unwrap();
    for &entry_index in ast.memory_table_arena.get_table_entries(memory_table_index) {
        let memory_entry = ast.memory_table_arena.get_table_entry(entry_index);
        if memory_entry.is_named_var() && *variable_name == memory_entry.get_name() {
            return Some(entry_index);
        }
    }
    None
}

fn get_current_stack_frame_offset(ast: &AST, node_index: usize) -> isize {
    let (_, function_memory_table_index) =
        get_function_node_index_and_memory_table_index(ast, node_index).unwrap();
    let memory_table = ast
        .memory_table_arena
        .get_table(function_memory_table_index);
    memory_table.offset
}

fn add_entry_marker(ast: &AST, moon_code: &mut Vec<String>, node_index: usize) {
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

fn add_function_marker(ast: &AST, moon_code: &mut Vec<String>, node_index: usize) {
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

fn add_if_marker(ast: &AST, moon_code: &mut Vec<String>, node_index: usize) {
    use NodeType::*;

    // If the node has children, return.
    if !ast.get_children(node_index).is_empty() {
        return;
    }

    let stat_block_node_index = if let StatBlock = ast.get_element(node_index).node_type {
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
    //println!("{}", stat_block_node_index);
    let grand_parent_node_index = ast.get_parent(stat_block_node_index).unwrap();
    // If it is a IfStat, add one of the two marker.
    if let IfStat = ast.get_element(grand_parent_node_index).node_type {
        // If it has a right sibling, it is the if block, if it has not, it is the else block.
        if ast.get_right_sibling(stat_block_node_index).is_some() {
            moon_code.push(IF_MARKER.to_string());
        } else {
            moon_code.push(ELSE_MARKER.to_string());
        }
    }
}

fn add_for_marker(ast: &AST, moon_code: &mut Vec<String>, node_index: usize) {
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
        if let ForStat = ast.get_element(grand_parent_node_index).node_type {
            moon_code.push(FOR_COND_MARKER.to_string());
        }
    }

    // If the node has a AssignForStat parent.
    if let Some((assign_for_star_node_index, _)) =
        ast.get_parent_node_of_type_on_left_branch(node_index, &[AssignForStat], &[])
    {
        // If parent is a ForStat, add FOR_INCR_MARKER.
        let grand_parent_node_index = ast.get_parent(assign_for_star_node_index).unwrap();
        if let ForStat = ast.get_element(grand_parent_node_index).node_type {
            moon_code.push(FOR_INCR_MARKER.to_string());
        }
    }

    let stat_block_node_index = if let StatBlock = ast.get_element(node_index).node_type {
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
        if let ForStat = ast.get_element(grand_parent_node_index).node_type {
            println!("{}", node_index);
            moon_code.push(FOR_LOOP_MARKER.to_string());
        }
    }
}

fn push_at(moon_code: &mut Vec<String>, new_moon_code: Vec<String>, marker: &str) {
    let index = get_marker_index(moon_code, marker);
    moon_code.splice(index..=index, new_moon_code.into_iter());
}

fn get_marker_index(moon_code: &mut Vec<String>, marker: &str) -> usize {
    moon_code.len()
        - 1
        - moon_code
            .iter()
            .rev()
            .position(|line| *line == marker)
            .unwrap()
}

fn get_memory_table_index_from_symbol(ast: &AST, node_index: usize) -> Option<usize> {
    let symbol_table_index = get_funtion_symbol_table_index(ast, node_index);
    //ast.get_element(node_index).symbol_table.unwrap();
    for memory_table in &ast.memory_table_arena.tables {
        if symbol_table_index == memory_table.get_symbol_index() {
            return Some(memory_table.get_index());
        }
    }
    None
}

fn get_class_node_of_symbol_entry(ast: &AST, symbol_entry_index: usize) -> Option<usize> {
    for class_node_index in ast.get_children_of_child(ast.root.unwrap(), 0) {
        let class_table_index = ast.get_element(class_node_index).symbol_table.unwrap();
        if ast
            .symbol_table_arena
            .get_table_entries(class_table_index)
            .iter()
            .any(|&entry_index| entry_index == symbol_entry_index)
        {
            return Some(class_node_index);
        }
    }
    None
}

fn get_function_node_index_and_memory_table_index(
    ast: &AST,
    node_index: usize,
) -> Option<(usize, usize)> {
    use NodeType::*;
    if let Some((function_node_index, _)) =
        ast.get_parent_node_of_type(node_index, &[MainFuncBody, FuncDef], &[])
    {
        Some((
            function_node_index,
            ast.get_element(function_node_index).memory_table.unwrap(),
        ))
    } else {
        None
    }
}

fn load_inst_addresse(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {
    if let Some((function_node_index, _)) =
        ast.get_parent_node_of_type(node_index, &[NodeType::FuncDef], &[])
    {
        let function_memory_table_index =
            ast.get_element(function_node_index).memory_table.unwrap();
        let inst_addr_offset = get_inst_addr_offset(ast, function_memory_table_index).unwrap();
        let register1 = ast.register_pool.pop();
        add_comment(moon_code, format!("Loading instance address to r{}", 12));
        load_from_offset(moon_code, register1, inst_addr_offset);
        moon_code.push(format!("{}add r12,r0,r{}", INDENT, register1));
        ast.register_pool.push(register1);
    }
}

fn get_reference_and_offset_registers(
    ast: &mut AST,
    moon_code: &mut Vec<String>,
    node_index: usize,
) -> (usize, usize) {
    //add_comment(moon_code, format!("Get registers for: {}", node_index));
    use NodeType::*;
    use SymbolKind::*;
    let node_type = ast.get_element(node_index).node_type;
    let register1 = ast.register_pool.pop();

    let reference_register = if let NodeType::VarElementList = node_type {
        moon_code.push(format!("{}addi r{},r0,0", INDENT, register1,));

        // FIXME: This code seems to work with single inheritence since the
        // inherited  is at the start of the memory table.
        let child_nodes = ast.get_children(node_index);
        let last_node_position = child_nodes.len() - 1;
        //let mut offset: isize = 0;
        let mut reference_register: usize = 14;
        for (position, child_index) in child_nodes.into_iter().enumerate() {
            let child_node_type = ast.get_element(child_index).node_type;
            match child_node_type {
                DataMember => {
                    // Get symbol entry associated with data member.
                    let symbol_entry_index =
                        ast.get_element(child_index).symbol_table_entry.unwrap();
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
                    //offset += memory_entry.get_offset();

                    moon_code.push(format!(
                        "{}addi r{},r{},{}",
                        INDENT,
                        register1,
                        register1,
                        memory_entry.get_offset()
                    ));

                    let index_list_node = ast.get_child(child_index, 1);

                    // Only proceed if the IndexList node has a memory entry.
                    if let Some(memory_entry_index) =
                        ast.get_element(index_list_node).memory_table_entry
                    {
                        add_comment(moon_code, format!("Add total array size."));
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
                        //load_node(ast, moon_code, register2, index_list_node);
                        moon_code.push(format!(
                            "{}add r{},r{},r{}",
                            INDENT, register1, register1, register2
                        ));
                        ast.register_pool.push(register2);
                    }

                    if position != last_node_position {
                        let symbol_entry =
                            ast.symbol_table_arena.get_table_entry(symbol_entry_index);
                        let type_size = match &symbol_entry.kind {
                            Parameter(symbol_type) | Variable(symbol_type) | For(symbol_type) => {
                                get_size(ast, &symbol_type.remove_dimensions()).unwrap()
                            }

                            _ => unreachable!(),
                        };
                        add_comment(moon_code, format!("Add size (excluding last element)."));
                        moon_code.push(format!(
                            "{}addi r{},r{},{}",
                            INDENT, register1, register1, type_size as isize
                        ));

                        //offset += memory_entry.get_size();
                        // add_comment(moon_code, format!("Add size (excluding last element)."));
                        // moon_code.push(format!(
                        //     "{}addi r{},r{},{}",
                        //     INDENT,
                        //     register1,
                        //     register1,
                        //     memory_entry.get_size()
                        // ));
                    }
                }
                FunctionCall => {
                    let entry_index = ast.get_element(child_index).memory_table_entry.unwrap();
                    let memory_entry = ast.memory_table_arena.get_table_entry(entry_index);
                    //offset = memory_entry.get_offset();

                    // Store offset of TempVar for return address.
                    moon_code.push(format!(
                        "{}addi r{},r0,{}",
                        INDENT,
                        register1,
                        memory_entry.get_offset()
                    ));

                    //store_at_offset(moon_code, memory_entry.get_offset(), register1);
                }
                _ => unreachable!(),
            }
        }
        reference_register
    } else {
        let entry_index = ast.get_element(node_index).memory_table_entry.unwrap();
        let memory_entry = ast.memory_table_arena.get_table_entry(entry_index);
        // Store offset of TempVar for return address.
        moon_code.push(format!(
            "{}addi r{},r0,{}",
            INDENT,
            register1,
            memory_entry.get_offset()
        ));

        //store_at_offset(moon_code, memory_entry.get_offset(), register1);
        14
    };
    ast.register_pool.push(register1);
    (reference_register, register1)
}

fn load_node(ast: &mut AST, moon_code: &mut Vec<String>, register_index: usize, node_index: usize) {
    let (reference_register, offset_register) =
        get_reference_and_offset_registers(ast, moon_code, node_index);
    let register1 = ast.register_pool.pop();
    moon_code.push(format!(
        "{}add r{},r{},r{}",
        INDENT, register1, reference_register, offset_register
    ));
    load_from_offset_register(moon_code, register_index, 0, register1);
    ast.register_pool.push(register1);
}

fn load_from_offset(moon_code: &mut Vec<String>, register_index: usize, offset: isize) {
    load_from_offset_register(moon_code, register_index, offset, 14);
}

fn load_from_offset_register(
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

fn store_node(
    ast: &mut AST,
    moon_code: &mut Vec<String>,
    node_index: usize,
    register_index: usize,
) {
    let (reference_register, offset_register) =
        get_reference_and_offset_registers(ast, moon_code, node_index);
    let register1 = ast.register_pool.pop();
    moon_code.push(format!(
        "{}add r{},r{},r{}",
        INDENT, register1, reference_register, offset_register
    ));
    store_at_offset_register(moon_code, 0, register1, register_index);
    ast.register_pool.push(register1);
    // let (register, offset) = get_register_and_offset(ast, moon_code, node_index);
    // store_at_offset_register(moon_code, offset, register, register_index);
}

fn store_at_offset(moon_code: &mut Vec<String>, offset: isize, register_index: usize) {
    moon_code.push(format!("{}sw {}(r14),r{}", INDENT, offset, register_index));
}

fn store_at_offset_register(
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

fn add_label(moon_code: &mut Vec<String>, label: String) {
    moon_code.push(format!("{:<10}nop", label));
}

fn add_comment(moon_code: &mut Vec<String>, comment: String) {
    moon_code.push(format!("{}% processing: {}", INDENT, comment));
}

fn addi(
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

fn addi_label(
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

fn subi(
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

fn add_instruction(moon_code: &mut Vec<String>, label: Option<String>, instruction: String) {
    match label {
        Some(label) => {
            moon_code.push(format!("{:<10}{}", label, instruction));
        }
        None => {
            moon_code.push(format!("{}{}", INDENT, instruction));
        }
    }
}
