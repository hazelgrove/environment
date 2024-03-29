#ifndef __ASTLIB_H
#define __ASTLIB_H

#include <stdio.h>
#include <stdint.h>
#include <string.h>

#define CAML_NAME_SPACE
#include "caml/mlvalues.h"
#include "caml/callback.h"

#include "state.h"

State curr_state;

#define MAX_CODE_LENGTH 1000

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
Get the original ast of the code

Input:
    - ast: struct representing the AST
    - index: index of code submission

Mutates:
    - ast
*/
void init_assignment(State *ast, char *dir, int assignemnt, int index, int n, int cursor);

/*
Print the current state as a line of code

Input:
    - ast: struct representing the AST
*/
void print_curr_state(State *ast);

/*
Copy the AST from astsrc to astdst

Input:
    - astdst: destination to copy to
    - astsrc: source to copy from

Mutates:
    - astdst
*/
void copy_ast(State *astdst, const State *astsrc);

/*
Initiate the OCaml code and set default values of curr_state
*/
void init_c(int seed);

/*
Shut down the OCaml Code
*/
void close_c();

/*
External functions from ocaml interface
*/
extern void change_zast(int action);
extern void get_ast();
extern int run_private_tests();
extern int run_public_tests();
extern void load_starter_code(char *dir, int assignment, int index, int n, int cursor);
extern void print_code();
extern void get_cursor_info();
extern void init(int seed);
extern void load_tests(char *dir, int assignment);


#endif
