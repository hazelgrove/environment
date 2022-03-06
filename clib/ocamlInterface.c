#include <stdio.h>

#define CAML_NAME_SPACE
#include "caml/mlvalues.h"
#include "caml/callback.h"
#include "caml/bigarray.h"

#include "state.h"

extern State curr_state;


int evaluate_ast(int n){
    static const value * evaluate_ast_closure = NULL;
    if (evaluate_ast_closure == NULL) evaluate_ast_closure = caml_named_value("evaluate_ast");
    return Int_val(caml_callback(*evaluate_ast_closure, Val_int(n)));
}

void change_node(int action){
    static const value * change_node_closure = NULL;
    if (change_node_closure == NULL) change_node_closure = caml_named_value("change_node");
    caml_callback(*change_node_closure, Val_int(action));
}


void copy_nodes(int *nodes, int dim, int *dest){
    for (int i = 0; i < dim; i++){
        dest[i] = nodes[i];
    }
}


CAMLprim value pass_nodes(value unit){
    return caml_ba_alloc_dims(CAML_BA_INT64 | CAML_BA_C_LAYOUT, 1, curr_state.nodes, MAX_NUM_NODES);
}


CAMLprim value pass_edges(value unit){
    return caml_ba_alloc_dims(CAML_BA_INT64 | CAML_BA_C_LAYOUT, 2, curr_state.edges, MAX_NUM_NODES * MAX_NUM_NODES, 2);
}


CAMLprim value pass_permitted_actions(value unit){
    return caml_ba_alloc_dims(CAML_BA_INT64 | CAML_BA_C_LAYOUT, 1, curr_state.permitted_actions, NUM_ACTIONS);
}


CAMLprim value get_nodes(value bigarray){
    int dim = Caml_ba_array_val(bigarray)->dim[0];
    copy_nodes(Caml_ba_data_val(bigarray), dim, curr_state.nodes);
    return Val_unit;
}