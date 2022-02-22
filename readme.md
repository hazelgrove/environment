# Code Overview

## Python Side
The file `test.py` includes the `main` function of the project. This file is used to test various types of functionalities written.

The directory `gym_basic/` contains the code for the environment of our reinforcement learning model. The subdirectory envs include the specific definitions of various environments. `ast_env.py` has the class `ASTEnv`, which is the one which we will use in our project. This environment is registered as `gym_basic:ast-v0`. `test_env.py` has the class `TestEnv`, which is used for any testing you want to do with the environment prior to implementing it in our project. This environment is registered as `gym_basic:test-v0`.

## C Side
All the C code are in the directory `clib/`. `State.h` defines the state/observations of our project in C-style. `astlib.h` defines the functions used for linking between the C code and Python code. `ocamlInterface.c` defines the functions used for linking between the C code and OCaml code. `test.c` may be used for your own purposes for testing anything before implementing it in our project. The subdirectory `caml/` includes various linking files used to communicate with OCaml. Please DO NOT modify anything in this directory.

## OCaml Side
All the OCaml code are in the directory `ocamllib/`. `astlib.ml` includes functions for communication with c. The subdirectory `astparser/` includes a parser to turn OCaml source code into an AST defined in `ast.ml`. 

# C Library Build
To complie `astlib.c`, use the following command:
```
make astclib
```

# OCaml Build System
[Dune](https://dune.build/)

# Python Dependency Manager
[Poetry](https://python-poetry.org/)

# Run OCaml Side
```
dune utop
```
