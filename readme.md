# Program Synthesis with Reinforcement Learning of Structured Edits

## Overview
This project focuses on using reinforcement learning to mutate a partially-correct/complete piece of coding homework to a complete and highly scored (e.g. test inputs all give correct output) homework submission. Our domain uses code written in the functional programming language, OCaml. This project extracts and preprocesses data from a large database of homework submission, transforming them into an abstract syntax tree (AST) and passing the through a graph neural network (GNN). 

## Build instructions
To set up, follow the following steps:
1. Set up [run-logger](https://run-logger.readthedocs.io/en/latest/index.html).
2. Configure your `.env` file so that the environment variable `GRAPHQL_ENDPOINT` is the server you have set up. 
```
GRAPHQL_ENDPOINT=server.com:1200/v1/graphql
```
3. Create a docker volume called `rl_checkpoint` by using the command
```
docker volume create rl_checkpoint
```

Now, you can build the project with docker by running the following commands in the terminal:
```
bash run.sh <DOCKER_IMAGE_NAME> <DOCKER_VOLUME_MOUNT_DIR> <DESCRIPTION_ON_LOGGER>
```

## Code Overview
The following directories each have the following functions:
* `agent/`: This directory includes the code for our reinforcement learning agent
* `clib/`: This directory includes the C code for our project. The C code is used for communicating between our Python and OCaml code.
* `envs/`: This directory includes the Python code for our environment. The environment that we are using is in `envs/ast_env.py`. 
* `ocamllib/`: This directory includes the OCaml code for our environment. 

## Bug Fix Notes
1. If there is a sudden error of not finding a child or something like that, check if `max_num_nodes` is sufficient for problem.
