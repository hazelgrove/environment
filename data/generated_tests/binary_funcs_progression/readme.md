# Binary funcs progression

Using the paradigm of binary funcs as a playground I attempt to push the limits of our model using a progression of tests 

The tests are as follows: 


| n | name | description | Number of moves <br />(+1 for done action) | n. problems | number of asserts relevant | succeded |
|-|--|-------|---|----|--|--|
|1|one_var| binary funcs of one variable. Agent needs to add True, False,or x1 then either do Done action or add negation | 0-1 | 4 | 2 | _ |
|2|one_var_like_filled | binary funcs of one variable, but two inputs. The 'core' of the term (i.e. True, False, x1, or x2) is already filled in. The agent needs to  either do Done action or add negation. Only works when Done action exists | 0-1 | 8 | 3 | _ |
| 3 | one_var_like | Binary funcs of one variable, but two inputs. The agent needs to add the core of the term (i.e. True, False, x1, or x2) is already filled in. The agent needs to  either do Done action or add negation. Only works when Done action exists | 1-2 | 12 | 3| _ |
| 4 | bin_op_shortcut * | Binary funcs of two variables, and two inputs, which involve one `and` or `or`. One term and the binary operation are already added. The agent needs to fill the term with the appropriate x1 or x2 term and negate appropriattely | 1-2 | 32 <br />(but 8 funcs) | 2<br /> (but changes based on first term) | _ |
| 5 | bin_op_shortcut_small * | Same as above (4). Simplified: only have two possible functions. | 1-2 | 2 | 1 | _ |
| 6 | bin_op_shortcut_root * |  Binary funcs of two variables, and two inputs, which involve one `and` or `or`.  One term and the binary operation are already added. The agent needs to navigate to the hole then  fill it with the appropriate x1 or x2 term and negate appropriattely | 2-3 | 32 <br />(but 8 funcs) | 2<br /> (but changes based on first term) | _ |
| 7 | bin_op_shortcut_root_small * | Same as above (6). Simplified: only have two possible functions. | 2-3 | 4 | 1 | _ |
| 8 | bin_op_add_term ** |  Binary funcs of two variables, and two inputs, which involve one `and` or `or`. One term is already added. The agent needs to add the appropriate `binop` then  fill its hole with the appropriate x1 or x2 term and negate appropriattely | 4-5 | 16 <br />(but 8 funcs)| 3<br /> (but changes based on first term) | _ |
| 9 | bin_op_add_term_small ** | Same as above (8), but only with two problems: one `and` and one `or`| 4-5 | 1 | _ |

\*all use folder `bin_op_shortcut`

\*\*all use folder `bin_op_add_term`
