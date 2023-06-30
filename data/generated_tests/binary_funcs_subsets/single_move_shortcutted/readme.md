# Single move shortcutted 

This is a variation on the set of 'single move functions': binary functions of two arguments which require a single move to complete. This includes all 2 variable binary functions excluding, Xor, NXor, trivial functions and functions of one variable. It comes out to be all functions which ccan be written in the form `[x1, (!x1)] [&&, || ] [x2, (!x2)]`, or 8 total functions. 

This is a simplified (shortcutted) version of that problem. Rather than needing to fill in the whole function body, the agent starts with the binary operation and one term already filled in. The agent must then fill in the other term. 





**There are two flavors of this problem.**

The first (easier) *zero move version* starts with the cursor selecting the empty in portion of the function body. Then all the agent must do is fill in that portion. In order to use this version the following parameters should be used in `params.yaml`:

```yaml
env: 
  num_assignments: 3
  code_per_assignment: [4,4,8] 
  cursor_start_pos: [2,3,1]
```


The second, (harder) *single move version* starts with the cursor selecting the root of the function body. The agent mus then move to select the hole, before filling it in appropriately. In order to use this version the following parameters should be used in `params.yaml`:
```yaml
env: 
  num_assignments: 3
  code_per_assignment: [4,4,8] 
  cursor_start_pos: [0,0,0]
```