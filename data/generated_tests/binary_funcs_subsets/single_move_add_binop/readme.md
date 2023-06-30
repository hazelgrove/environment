# Single move add binop

This is a variation on the set of 'single move functions': binary functions of two arguments which require a single move to complete. This includes all 2 variable binary functions excluding, Xor, NXor, trivial functions and functions of one variable. It comes out to be all functions which ccan be written in the form `[x1, (!x1)] [&&, || ] [x2, (!x2)]`, or 8 total functions. 

This is a simplified version of that problem. Rather than needing to fill in the whole function body, the agent starts with a term pre-entered. From that point it must fill in the binary operation (`and` or `or`) and the second term, given information spefified in the assert statements. 

The following parameters should be used for these tests in `params.yaml`
```yaml
env: 
  num_assignments: 3
  code_per_assignment: [4,4,8] 
  cursor_start_pos: [0,0,0]
```
