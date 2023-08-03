# Test the following progression of examples to see where our models fail...

Use a set of tests on relatively simple problems, to see if a specific move is causing issues for our agent. 

Use gradually more complicated problems, and then build up to a more complicated problem, using the progression shown below. 


1. `f x1 x2 = [x1|x2]`
1. `f x1 x2 = ~[x1|x2]`
1. `f x1 x2 = ~?[x1|x2]`
1. `f x1 x2 = x1 and ~?x2  `
1. `f x1 x2 = ~?x1 and x2`
1. `f x1 x2 = x1 or ~?x2`
1. `f x1 x2 = ~?x1 or x2`
1. `f x1 x2 = x1 [and|or] x2 `
1. `f x1 x2 = ~?x1 [and|or] x2 `


| test. no.  |Notes|  ex_test|  success |
|--|--| -- |--
|1 | - |`f x1 x2 = <?>` |✅ 
|2 | - |`f x1 x2 = ~<?>` | ✅ 
|3 | Combination of **1** and **2**. Doubles number of cases |`f x1 x2 = ~?` or `f x1 x2 = <?>` | ✅| 
|4 | Succeeded briefly, then converged back to 0.5  | `f x1 x2 = x1 and <x2>` |🤷* | 
|5 | - | `f x1 x2 = <x1> and x2` | ✅ | 
|6 | essentially same as **4** | `f x1 x2 = x1 or <x2>` |🤷| 
|7 | - | `f x1 x2 = <x1> and x2` | ✅ | 
|8 | Much more complicated move wise | `f x1 x2 = <x1> ` |✅ \*\* | 

\* Succeeded briefly, then converged back to 0.5

\*\* Incorrect test suite 

### Key
Symbol| Success state |
 -|-
❌ |Failure: model gets 0 or near 0 (e.g. 0.0001) result. Typically unnable to do better than random 
🤷 | Model is able to succeed on some portion of tests, but not all 
✅ | Model succeeds; gets approx. 100% success

## Discussion

We observe a strange asymmetry between tests 4 and 5, and 6 and 7. In each pair the first example, (that with x2 being edited) fails, and the second succeeds. 

This is strange because based on our representation, we should not expect there to be any difference between numbered children, or indeed variable numbers.
I am carrying out additional testing to try and isolate these causes. In this testing, I am repeating the tests [4,7], with the ordering of `x1` an `x2` flipped to see if the issue is with variables, or ordering. 

One additional possiblity has to do with how we represent currying. If we are currying, it could be that there is an asymmetry in how the variables of each function propagate along the edges. 

# tests, with added edges: 

| test. no.  |Notes|  ex_test|  success |
|--|--| -- |--
|1 | - |`f x1 x2 = <?>` |✅ 
|2 | - |`f x1 x2 = ~<?>` | ✅ 
|3 | Combination of **1** and **2**. Doubles number of cases |`f x1 x2 = ~?` or `f x1 x2 = <?>` | ✅| 
|4 | Succeeded briefly, then converged back to 0.5  | `f x1 x2 = x1 and <x2>` |✅ | 
|5 | - | `f x1 x2 = <x1> and x2` | ✅ | 
|6 | essentially same as **4** | `f x1 x2 = x1 or <x2>` |✅| 
|7 | - | `f x1 x2 = <x1> and x2` | ✅ | 
|8 | Much more complicated move wise | `f x1 x2 = <x1> ` |❌| 


### Key
Symbol| Success state |
 -|-
❌ |Failure: model gets 0 or near 0 (e.g. 0.0001) result. Typically unnable to do better than random 
🤷 | Model is able to succeed on some portion of tests, but not all 
✅ | Model succeeds; gets approx. 100% success
