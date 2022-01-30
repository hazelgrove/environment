#include <stdio.h>

typedef struct{
    int a[3][2];
    double b;
    char c;
} abc;

void print_struct(abc *test){
    for (int i = 0; i < 3; i++){
        for (int j = 0; j < 2; j++)
            printf("%d ", (test->a)[i][j]);
    }

    printf("\n%f %c\n", test->b, test->c);
}

void plus_one(int len, int *arr){
    for (int i = 0; i < len; i++){
        printf("%d ", arr[i]);
        arr[i]++;
    }
    printf("\n");
}

int get_reward(int action){
    if (action == 0)
        return 1;
    else
        return -1;
}