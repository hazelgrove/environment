#include <stdio.h>
#include <string.h>

#define CAML_NAME_SPACE
#include "caml/mlvalues.h"
#include "caml/callback.h"
#include "caml/bigarray.h"
#include "caml/alloc.h"

#include "state.h"

extern State curr_state;

int run_unit_tests()
{
    static const value *run_unit_test_closure = NULL;
    if (run_unit_test_closure == NULL)
    {
        run_unit_test_closure = caml_named_value("run_unit_tests");
        if (run_unit_test_closure == NULL)
            exit(1);
    }
    return Bool_val(caml_callback(*run_unit_test_closure, Val_int(0)));
}

void change_zast(int action)
{
    static const value *change_zast_closure = NULL;
    if (change_zast_closure == NULL)
    {
        change_zast_closure = caml_named_value("change_zast");
        if (change_zast_closure == NULL)
            exit(1);
    }
    value ser_zast = caml_alloc_initialized_string(strlen(curr_state.zast), curr_state.zast);
    strcpy(curr_state.zast, strdup(String_val(caml_callback2(*change_zast_closure, ser_zast, Val_int(action)))));
}

void get_ast()
{
    static const value *get_ast_closure = NULL;
    if (get_ast_closure == NULL)
    {
        get_ast_closure = caml_named_value("get_ast");
        if (get_ast_closure == NULL)
            exit(1);
    }
    value ser_zast = caml_alloc_initialized_string(strlen(curr_state.zast), curr_state.zast);
    int cursor = Int_val(caml_callback(*get_ast_closure, ser_zast));
    curr_state.cursor = cursor;
}

void load_tests(int assignment)
{
    static const value *load_tests_closure = NULL;
    if (load_tests_closure == NULL)
    {
        load_tests_closure = caml_named_value("load_tests");
        if (load_tests_closure == NULL)
            exit(1);
    }
    caml_callback(*load_tests_closure, Val_int(assignment));
}

void load_starter_code(int assignment, int index)
{
    static const value *load_starter_code_closure = NULL;
    if (load_starter_code_closure == NULL)
    {
        load_starter_code_closure = caml_named_value("load_starter_code");
        if (load_starter_code_closure == NULL)
            exit(1);
    }
    strcpy(curr_state.zast, strdup(String_val(caml_callback2(*load_starter_code_closure, Val_int(assignment), Val_int(index)))));
}

/*
Copy nodes from src to dest
*/
void copy_1d(int *src, int dim, int *dest)
{
    for (int i = 0; i < dim; i++)
    {
        dest[i] = src[i];
    }
}

/*
Copy edges from src to dest
*/
void copy_2d(int *src, int dim1, int dim2, int *dest)
{
    for (int i = 0; i < dim1; i++)
    {
        for (int j = 0; j < dim2; j++)
            dest[i * dim2 + j] = src[i * dim2 + j];
    }
}

CAMLprim value pass_nodes(value unit)
{
    return caml_ba_alloc_dims(CAML_BA_INT32 | CAML_BA_C_LAYOUT, 1, curr_state.nodes, curr_state.num_nodes);
}

CAMLprim value get_nodes(value bigarray)
{
    int dim = Caml_ba_array_val(bigarray)->dim[0];
    copy_1d(Caml_ba_data_val(bigarray), dim, curr_state.nodes);
    curr_state.num_nodes = dim;
    return Val_unit;
}

CAMLprim value pass_edges(value unit)
{
    return caml_ba_alloc_dims(CAML_BA_INT32 | CAML_BA_C_LAYOUT, 2, curr_state.edges, curr_state.num_edges, 3);
}

CAMLprim value get_edges(value bigarray)
{
    int dim1 = Caml_ba_array_val(bigarray)->dim[0];
    int dim2 = Caml_ba_array_val(bigarray)->dim[1];
    copy_2d(Caml_ba_data_val(bigarray), dim1, dim2, (int *)curr_state.edges);
    curr_state.num_edges = dim1;
    return Val_unit;
}

CAMLprim value pass_unit_tests(value unit)
{
    return caml_ba_alloc_dims(CAML_BA_INT32 | CAML_BA_C_LAYOUT, 2, curr_state.tests, curr_state.num_tests, 2);
}

CAMLprim value get_unit_tests(value bigarray)
{
    int dim1 = Caml_ba_array_val(bigarray)->dim[0];
    int dim2 = Caml_ba_array_val(bigarray)->dim[1];
    copy_2d(Caml_ba_data_val(bigarray), dim1, dim2, (int *)curr_state.tests);
    curr_state.num_tests = dim1;
    return Val_unit;
}

CAMLprim value get_actions(value bigarray)
{
    int dim = Caml_ba_array_val(bigarray)->dim[0];
    copy_1d(Caml_ba_data_val(bigarray), dim, curr_state.permitted_actions);
    return Val_unit;
}

void print_code()
{
    static const value *print_code_closure = NULL;
    if (print_code_closure == NULL)
        print_code_closure = caml_named_value("print_code");
    caml_callback(*print_code_closure, Val_int(0));
}

// CAMLprim value pass_permitted_actions(value unit){
//     return caml_ba_alloc_dims(CAML_BA_INT64 | CAML_BA_C_LAYOUT, 1, curr_state.permitted_actions, NUM_ACTIONS);
// }