#include <stdio.h>
#include <stdint.h>

#define MAX_NUM_NODES 10
#define NUM_ACTIONS 10

typedef struct{
    int nodes[MAX_NUM_NODES];
    int edges[MAX_NUM_NODES * MAX_NUM_NODES][2];
    int permitted_actions[NUM_ACTIONS];
} State;

/*
Mutates AST according to action taken

Input:
    - ast: array representing the AST
    - action: index of action

Mutates:
    - ast->nodes, ast->edges
*/
void take_action(State *ast, uint16_t action){

}

/*
Test the code given by the AST on the unit test

Input:
    - ast: array representing the AST
    - unit_test_index: index of unit test

Output:
    - 0 for not passed; 1 for passed
*/
int check_ast(const State *ast, uint16_t unit_test_index){

    return 0;
}

/*
Find the valid actions given the AST

Input:
    - ast: array representing the AST

Mutates:
    - ast->permitted_actions
*/
void valid_actions(State *ast){

}

