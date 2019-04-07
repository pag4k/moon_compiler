use crate::ast_node::*;
use crate::ast_visitor::*;
use crate::code_generation_common::*;
use crate::language::*;
use crate::moon_code::*;

use std::collections::HashMap;

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
    semantic_actions.insert(ReadStat, read_stat);
    semantic_actions.insert(ReturnStat, return_stat);

    // Binary operation
    semantic_actions.insert(RelExpr, binary_op);
    semantic_actions.insert(AddOp, binary_op);
    semantic_actions.insert(MultOp, binary_op);
    // Unary operation
    semantic_actions.insert(AssignStat, assign_stat);
    semantic_actions.insert(AssignForStat, assign_stat);
    semantic_actions.insert(Not, unary_op);
    semantic_actions.insert(Sign, unary_op);

    // Literal variables
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
    use Reference::*;

    // Get function name and unique index.
    let func_name = get_func_name(ast, node_index);
    let function_index = get_funtion_symbol_table_index(ast, node_index);
    let func_label = format!("func{}", function_index);
    let end_func_label = format!("endfunc{}", function_index);

    // Get return address variable offset.
    let memory_table_index = ast.get_memory_table_index(node_index);
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
    store(ast, &mut function_moon_code, Offset(return_addr_offset), 15);

    push_at(moon_code, function_moon_code, FUNCTION_MARKER);

    add_label(moon_code, end_func_label.clone());
    // Load return address.
    load(ast, moon_code, 15, Offset(return_addr_offset));

    // Jump back to calling function.
    moon_code.push(format!("{}jr r15", INDENT));
    moon_code.push("".to_string());
}

fn function_call(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {
    use Reference::*;
    // Get function unique index and label.
    let function_index = get_funtion_symbol_table_index(ast, node_index);
    let func_label = format!("func{}", function_index);

    // Get stack and return variable offsets.
    let current_stack_frame_size = get_current_stack_frame_offset(ast, node_index);
    let called_function_memory_table_index =
        get_memory_table_index_from_symbol(ast, node_index).unwrap();
    let (return_var_size, return_var_offset) =
        get_return_var_size_and_offset(ast, called_function_memory_table_index);

    add_comment(
        moon_code,
        format!(
            "Function call to {} ({})",
            get_func_name(ast, node_index),
            func_label
        ),
    );

    let register1 = ast.register_pool.pop();

    // If member function, set instance address.
    if let Some(inst_addr_var_offset) =
        get_inst_addr_offset(ast, called_function_memory_table_index)
    {
        add_comment(moon_code, "Setting instance address".to_string());
        let inst_offset = get_inst_offset(ast, node_index).unwrap();
        addi(moon_code, register1, 14, inst_offset);
        store(
            ast,
            moon_code,
            Offset(current_stack_frame_size + inst_addr_var_offset),
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
        // Copy parameter.
        copy_var(
            ast,
            moon_code,
            SizeAndOffset(
                param_entry.get_size(),
                current_stack_frame_size + param_entry.get_offset(),
            ),
            Node(argument_node_index),
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

    // Copy return value to local variable.
    copy_var(
        ast,
        moon_code,
        Node(node_index),
        SizeAndOffset(
            return_var_size,
            current_stack_frame_size + return_var_offset,
        ),
    );

    ast.register_pool.push(register1);
}

fn data_member(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {
    use Reference::*;

    // Get the IndexList node.
    let index_list_node = ast.get_child(node_index, 1);

    // Only proceed if the IndexList node has a memory entry.
    if ast.has_memory_entry_index(index_list_node) {
        // Get symbol entry associated with data member.
        let symbol_entry = get_symbol_entry(ast, node_index);

        let symbol_type = symbol_entry.get_symbol_type().unwrap();
        let dimension_list = symbol_type.get_dimension_list();
        let type_size = get_size(ast, &symbol_type.remove_dimensions()).unwrap();

        let register1 = ast.register_pool.pop();
        let register2 = ast.register_pool.pop();
        let register3 = ast.register_pool.pop();
        add_comment(
            moon_code,
            format!("{} = array offset", get_var_name(ast, index_list_node)),
        );
        // Initialize the accumulator register that will hold the array offset.
        subi(moon_code, register1, 0, type_size as isize);

        for (position, index_node) in ast.get_children(index_list_node).into_iter().enumerate() {
            // Get the current dimension multiplier.
            let array_offset: usize =
                dimension_list.iter().skip(position + 1).product::<usize>() * type_size;
            // Load the current index value.
            load(ast, moon_code, register2, Node(index_node));
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
        store(ast, moon_code, Node(index_list_node), register1);

        ast.register_pool.push(register1);
        ast.register_pool.push(register2);
        ast.register_pool.push(register3);
    }
}

fn binary_op(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {
    use NodeType::*;
    use OperatorType::*;
    use Reference::*;

    let lhs_node_index = node_index;

    // Get rhs nodes depending on node type.
    let (rhs_node_index1, op_node_index, rhs_node_index2) = match ast.get_node_type(node_index) {
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

    // Get operation.
    let operator = match ast.get_token_type(op_node_index) {
        TokenType::Operator(operator) => *operator,
        _ => unreachable!(),
    };

    add_comment(
        moon_code,
        format!(
            "{} := {} {} {}",
            get_var_name(ast, lhs_node_index),
            get_var_name(ast, rhs_node_index1),
            ast.get_lexeme(op_node_index),
            get_var_name(ast, rhs_node_index2),
        ),
    );

    let register1 = ast.register_pool.pop();
    let register2 = ast.register_pool.pop();
    let register3 = ast.register_pool.pop();

    // Load rhs values in register1 and register2.
    load(ast, moon_code, register1, Node(rhs_node_index1));
    load(ast, moon_code, register2, Node(rhs_node_index2));

    // Do operation and put result in register3.
    match operator {
        LT | LEq | NEq | GT | GEq | Eq | Addition | Subtraction | Multiplication | Division => {
            moon_code.push(format!(
                "{}{} r{},r{},r{}",
                INDENT,
                get_instruction(operator),
                register3,
                register1,
                register2,
            ));
        }
        And | Or => {
            add_and_or_not(ast, moon_code, operator, register1, register2, register3);
        }
        _ => unreachable!(),
    }

    // Store result in temporary variable.
    store(ast, moon_code, Node(lhs_node_index), register3);

    ast.register_pool.push(register1);
    ast.register_pool.push(register2);
    ast.register_pool.push(register3);
}

fn unary_op(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {
    use OperatorType::*;
    use Reference::*;

    let lhs_node_index = node_index;
    let rhs_node_index = ast.get_child(node_index, 0);

    // Get operation.
    let operator = match ast.get_token_type(lhs_node_index) {
        TokenType::Operator(operator) => *operator,
        _ => unreachable!(),
    };

    add_comment(
        moon_code,
        format!(
            "{} := {} {}",
            get_var_name(ast, lhs_node_index),
            ast.get_lexeme(lhs_node_index).clone(),
            get_var_name(ast, rhs_node_index),
        ),
    );

    let register1 = ast.register_pool.pop();
    let register2 = ast.register_pool.pop();

    // Load only value in register1.
    load(ast, moon_code, register1, Node(rhs_node_index));

    // Do operation and put result in register2.
    match operator {
        Subtraction => {
            moon_code.push(format!(
                "{}{} r{},r{},r{}",
                INDENT,
                get_instruction(operator),
                register2,
                0,
                register1,
            ));
        }
        Not => {
            add_and_or_not(ast, moon_code, operator, register1, 0, register2);
        }
        _ => unreachable!(),
    }

    // Store result in temporary variable.
    store(ast, moon_code, Node(lhs_node_index), register2);

    ast.register_pool.push(register1);
    ast.register_pool.push(register2);
}

fn assign_stat(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {
    use Reference::*;

    let lhs_node_index = ast.get_child(node_index, 0);
    let rhs_node_index = ast.get_child(node_index, 1);

    add_comment(
        moon_code,
        format!(
            "{} := {}",
            get_var_name(ast, lhs_node_index),
            get_var_name(ast, rhs_node_index),
        ),
    );

    // Copy rhs to lhs.
    copy_var(ast, moon_code, Node(lhs_node_index), Node(rhs_node_index));
}

fn num(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {
    use Reference::*;

    add_entry_marker(ast, moon_code, node_index);
    add_if_marker(ast, moon_code, node_index);
    add_for_marker(ast, moon_code, node_index);

    // Get Num data.
    let data = ast.get_lexeme(node_index).clone();

    add_comment(
        moon_code,
        format!("{} := {}", get_var_name(ast, node_index), data),
    );

    // Create a value corresponding to the literal value
    let integer = if let Ok(integer) = data.parse::<usize>() {
        integer
    } else if let Ok(float) = data.parse::<f32>() {
        // FIXME: This is a temporary solution for floats.
        // Their decimal part is just removed.
        float as usize
    } else {
        // This should never happen since the lexical analysis validated.
        unreachable!()
    };

    // Clamp since the compiler does not accept immediate value of 16 bits.
    let integer = if integer > 16384 { 16384 } else { integer };

    // Copy litteral value.
    let register1 = ast.register_pool.pop();
    addi(moon_code, register1, 0, integer as isize);
    store(ast, moon_code, Node(node_index), register1);
    ast.register_pool.push(register1);
}

fn if_stat(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {
    use Reference::*;

    let cond_node_index = ast.get_child(node_index, 0);

    // Get unique index and labels.
    let if_index = ast.register_pool.get_if();
    let elseif = format!("elseif{}", if_index);
    let endif = format!("endif{}", if_index);

    // ADD THIS BEFORE IF BLOCK
    let mut if_moon_code: Vec<String> = Vec::new();
    add_comment(
        &mut if_moon_code,
        format!(
            "if({}), index: {}",
            get_var_name(ast, cond_node_index),
            if_index
        ),
    );
    let register1 = ast.register_pool.pop();
    load(ast, &mut if_moon_code, register1, Node(cond_node_index));
    if_moon_code.push(format!("{}bz r{},{}", INDENT, register1, elseif));
    ast.register_pool.push(register1);
    ////////////////////

    push_at(moon_code, if_moon_code, IF_MARKER);

    // ADD THIS BEFORE ELSE BLOCK
    let mut else_moon_code: Vec<String> = Vec::new();
    else_moon_code.push(format!("{}j {}", INDENT, endif));
    add_label(&mut else_moon_code, elseif.clone());
    ////////////////////

    push_at(moon_code, else_moon_code, ELSE_MARKER);

    // ADD THIS AFTER IF STATBLOCK
    add_label(moon_code, endif.clone());
    ////////////////////
}

fn for_stat(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {
    use Reference::*;

    let assign_node_index = node_index;
    let assign_variable_name = get_var_name(ast, assign_node_index);
    let cond_node_index = ast.get_child(node_index, 3);

    // Get unique index and labels.
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
    let register1 = ast.register_pool.pop();
    load(ast, &mut cond_moon_code, register1, Node(rhs_node_index));
    store(ast, &mut cond_moon_code, Node(assign_node_index), register1);
    // Conditional label
    add_label(&mut cond_moon_code, condfor.clone());
    ast.register_pool.push(register1);
    ////////////////////

    push_at(moon_code, cond_moon_code, FOR_COND_MARKER);

    // ADD THIS BEFORE FOR INCREMENT
    let mut incr_moon_code: Vec<String> = Vec::new();
    add_comment(
        &mut incr_moon_code,
        format!("for_incr({}), index: {}", assign_variable_name, for_index),
    );
    let register1 = ast.register_pool.pop();
    load(ast, &mut incr_moon_code, register1, Node(cond_node_index));
    incr_moon_code.push(format!("{}bz r{},{}", INDENT, register1, endfor));
    incr_moon_code.push(format!("{}j {}", INDENT, loopfor));
    add_label(&mut incr_moon_code, incrfor.clone());
    ast.register_pool.push(register1);
    ////////////////////

    push_at(moon_code, incr_moon_code, FOR_INCR_MARKER);

    // ADD THIS BEFORE FOR STATBLOCK
    let mut loop_moon_code: Vec<String> = Vec::new();
    loop_moon_code.push(format!("{}j {}", INDENT, condfor));
    add_label(&mut loop_moon_code, loopfor.clone());
    ////////////////////

    push_at(moon_code, loop_moon_code, FOR_LOOP_MARKER);

    // ADD THIS AFTER THE LOOP.
    moon_code.push(format!("{}j {}", INDENT, incrfor));
    add_label(moon_code, endfor.clone());
    ////////////////////
}

fn write_stat(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {
    use Reference::*;

    let child_node_index = ast.get_child(node_index, 0);
    let function_stack_frame_size = get_current_stack_frame_offset(ast, node_index);

    add_comment(
        moon_code,
        format!("write({})", get_var_name(ast, child_node_index),),
    );

    let register1 = ast.register_pool.pop();

    // Load value to write.
    load(ast, moon_code, register1, Node(child_node_index));

    // Move function pointer.
    addi(moon_code, 14, 14, function_stack_frame_size);

    // Put write value on the stack,
    add_comment(moon_code, "Put value on stack".to_string());
    store(ast, moon_code, Offset(-8), register1);

    // Copy buffer address on the stack.
    add_comment(moon_code, "Put buffer address on the stack".to_string());
    addi_label(moon_code, register1, 0, "buf".to_string());
    store(ast, moon_code, Offset(-12), register1);

    // Convert value to string using "intstr".
    add_comment(moon_code, "Convert int to string for output".to_string());
    moon_code.push(format!("{}jl r15,intstr", INDENT));

    // Get result of conversion which is in r13.
    store(ast, moon_code, Offset(-8), 13);

    // Write value to console using "putstr".
    add_comment(moon_code, "Output to console".to_string());
    moon_code.push(format!("{}jl r15,putstr", INDENT));

    // Move back function pointer.
    subi(moon_code, 14, 14, function_stack_frame_size);

    ast.register_pool.push(register1);
}

fn read_stat(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {
    use Reference::*;

    let child_node_index = ast.get_child(node_index, 0);
    let function_stack_frame_size = get_current_stack_frame_offset(ast, node_index);

    add_comment(
        moon_code,
        format!("read({})", get_var_name(ast, child_node_index),),
    );

    let register1 = ast.register_pool.pop();

    // Move function pointer.
    addi(moon_code, 14, 14, function_stack_frame_size);

    // Copy buffer address on the stack.
    add_comment(moon_code, "Put buffer address on the stack".to_string());
    addi_label(moon_code, register1, 0, "buf".to_string());
    store(ast, moon_code, Offset(-8), register1);

    // Get the string with function "getstr"
    add_comment(moon_code, "Get inpit from console".to_string());
    moon_code.push(format!("{}jl r15,getstr", INDENT));

    // Convert to integer with function "strint"
    add_comment(moon_code, "Convert string to int for input".to_string());
    moon_code.push(format!("{}jl r15,strint", INDENT));

    // Move back function pointer.
    subi(moon_code, 14, 14, function_stack_frame_size);

    // Get result of conversion which is in r13.
    store(ast, moon_code, Node(child_node_index), 13);

    ast.register_pool.push(register1);
}

fn return_stat(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {
    use Reference::*;

    let argument_node_index = ast.get_child(node_index, 0);

    //  Get function node and table.
    let (function_node_index, function_memory_table_index) =
        get_function_node_index_and_memory_table_index(ast, node_index).unwrap();
    // Get return variable size and offset.
    let (return_var_size, return_var_offset) =
        get_return_var_size_and_offset(ast, function_memory_table_index);

    add_comment(
        moon_code,
        format!("return value = {}", get_var_name(ast, argument_node_index)),
    );

    // Copy temporary variable to return value.
    copy_var(
        ast,
        moon_code,
        SizeAndOffset(return_var_size, return_var_offset),
        Node(argument_node_index),
    );

    // Jump at the end of function.
    moon_code.push(format!(
        "{}j endfunc{}",
        INDENT,
        get_funtion_symbol_table_index(ast, function_node_index)
    ));
}

fn leaf(ast: &mut AST, moon_code: &mut Vec<String>, node_index: usize) {
    add_entry_marker(ast, moon_code, node_index);
    add_function_marker(ast, moon_code, node_index);
    add_if_marker(ast, moon_code, node_index);
    add_for_marker(ast, moon_code, node_index);
}

fn get_var_name(ast: &AST, node_index: usize) -> String {
    let node_type = ast.get_node_type(node_index);
    if let NodeType::VarElementList = node_type {
        let last_child_index = *ast.get_children(node_index).iter().last().unwrap();
        ast.symbol_table_arena
            .get_table_entry(ast.get_symbol_entry_index(last_child_index))
            .get_name_clone()
    } else {
        // First try to get it from the memory entry and then from the table.
        if ast.has_memory_entry_index(node_index) {
            let memory_entry_index = ast.get_memory_entry_index(node_index);
            let memory_entry = ast.memory_table_arena.get_table_entry(memory_entry_index);
            memory_entry.get_name()
        } else if ast.has_memory_table_index(node_index) {
            let memory_table_index = ast.get_memory_table_index(node_index);
            let memory_table = ast.memory_table_arena.get_table(memory_table_index);
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
        symbol_entry.get_name_clone()
    } else if let Some(table_index) = ast.get_element(node_index).symbol_table {
        let symbol_table = ast.symbol_table_arena.get_table(table_index);
        symbol_table.get_name_clone()
    } else {
        unreachable!("Node has neither a memory table nor an memory entry.");
    }
}

fn get_funtion_symbol_table_index(ast: &AST, node_index: usize) -> usize {
    // First try to get it from the memory entry and then from the table.
    if let Some(entry_index) = ast.get_element(node_index).symbol_table_entry {
        let symbol_entry = ast.symbol_table_arena.get_table_entry(entry_index);
        symbol_entry.get_link().unwrap()
    } else if let Some(table_index) = ast.get_element(node_index).symbol_table {
        let symbol_table = ast.symbol_table_arena.get_table(table_index);
        symbol_table.get_index()
    } else {
        unreachable!("Node has neither a symbol table nor an symbol entry with a link.");
    }
}

fn get_return_var_size_and_offset(ast: &AST, memory_table_index_index: usize) -> (usize, isize) {
    for &entry_index in ast
        .memory_table_arena
        .get_table_entries(memory_table_index_index)
    {
        let memory_entry = &ast.memory_table_arena.get_table_entry(entry_index);
        if memory_entry.is_return_var() {
            return (memory_entry.get_size(), memory_entry.get_offset());
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

fn get_current_stack_frame_offset(ast: &AST, node_index: usize) -> isize {
    let (_, function_memory_table_index) =
        get_function_node_index_and_memory_table_index(ast, node_index).unwrap();
    let memory_table = ast
        .memory_table_arena
        .get_table(function_memory_table_index);
    memory_table.offset
}

fn get_memory_table_index_from_symbol(ast: &AST, node_index: usize) -> Option<usize> {
    let symbol_table_index = get_funtion_symbol_table_index(ast, node_index);
    for memory_table in &ast.memory_table_arena.tables {
        if symbol_table_index == memory_table.get_symbol_index() {
            return Some(memory_table.get_index());
        }
    }
    None
}
