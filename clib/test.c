#include "astlib.h"
#include "state.h"

int main(){
    init_c();

    State state;
    init_assignment(&state, 1, 1);

    take_action(&state, 1);
    int reward = check_ast(&state);

    printf("%d\n", reward);
}
