A curriculum of generally harder and harder tests

Each directory is a new set of tests Some sets have multiple train directories. 


config:

```yaml
env:
  assignment_dir: /RL_env/data/curriculum/train
  code_per_assignment:  
	- 4     # 1 var one input
	- 12	# 1 var like filled
	- 6 	# 1 var like 
	- 4     # binop mo move 
	- 4
	- 8 
	- 4     
	- 4
	- 8 
	- 4     # binop with move 
	- 4
	- 8 
	- 4     
	- 4
	- 8 
  cursor_start_pos: 
	- 0		# 1 var one input
	- 0		# 1 var like filled
	- 0 	# 1 var like 
	- 2     # binop mo move 
	- 3
	- 1
	- 2
	- 3
	- 1
	- 0     # binop with move 
	- 0
	- 0
	- 0
	- 0
	- 0
  max_episode_steps: 6
  num_assignments: 15

```