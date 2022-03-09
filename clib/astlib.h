#ifndef __ASTLIB_H
#define __ASTLIB_H

#include <stdio.h>
#include <stdint.h>

#define CAML_NAME_SPACE
#include "caml/mlvalues.h"
#include "caml/callback.h"

#include "state.h"

State curr_state;


/*
Mutates AST according to action taken

Input:
    - ast: struct representing the AST
    - action: index of action

Mutates:
    - ast->nodes, ast->edges
*/
void take_action(State *ast, int action);


/*
Test the code given by the AST on the unit test

Input:
    - ast: struct representing the AST
    - unit_test_index: index of unit test

Output:
    - 0 for not passed; 1 for passed
*/
int check_ast();


/*
Find the valid actions given the AST

Input:
    - ast: struct representing the AST

Mutates:
    - ast->permitted_actions
*/
void valid_actions(State *ast);


/*
Get the original ast of the code

Input:
    - ast: struct representing the AST
    - index: index of code submission

Mutates:
    - ast
*/
void init_assignment(State *ast, int assignemnt, int index);


void copy_ast(State *astdst, State *astsrc);


void init_c();


void close_c();


/*
External functions from ocaml interface
*/
extern void change_ast(int action);
extern int run_unit_tests();
extern void load_starter_code(int assignment, int index);
extern void load_tests(int assignment);
extern void print_stuff();

#endif