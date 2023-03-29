from random import random, randint
from math import sqrt
import os
import argparse

def generate_assert_int(num, op, min_int, max_int, range_min, range_max):
    assert_int, result, success = max_int + 1, max_int + 1, True
    try_count = 0
    while (result < min_int or result > max_int):
        assert_int = randint(range_min, range_max)
        # print(assert_int, op, num)
        if op == '+':
            result = assert_int + num
        elif op == '-':
            result = assert_int - num
        elif op == '*':
            result = assert_int * num
        try_count += 1
        if try_count > 20:
            success = False
            break

    return assert_int, result, success


def write_assignments(assignment_list, save_dir):
    if not os.path.exists(save_dir):
        os.mkdir(save_dir)
    else:
        filelist = [f for f in os.listdir(save_dir)]
        for f in filelist:
            os.remove(os.path.join(save_dir, f))
    for i in range(len(assignment_list)):
        path = os.path.join(save_dir, f'{i}.ml')
        with open(path, 'w') as ml_file:
            ml_file.write(assignment_list[i])
            
    with open(os.path.join(save_dir, 'test.ml'), 'w') as test_file:
        test_file.write("[]\n")

def generate_map():
    parser = argparse.ArgumentParser()
    parser.add_argument("--assignment_num", type=int, help="the number of assignments to create", default=10)
    parser.add_argument("--min_int", type=int, help="minimum integer created in assignments", default=-50)
    parser.add_argument("--max_int", type=int, help="maximum integer created in assignments", default=50)
    parser.add_argument("--assert_num", type=int, help="the number of asserts in each assignment", default=2)
    parser.add_argument("--hole_prob", type=float, help="probability of creating holes in assignments", default=1)
    parser.add_argument("--save_dir", help="directory to save assignments")
    parser.add_argument("--test-dir", help="directory to save test assignments")
    args = parser.parse_args() 
    # print(args)
    
    assignment_num = args.assignment_num
    min_int = args.min_int
    max_int = args.max_int
    assert_num = args.assert_num
    use_hole_prob = args.hole_prob
    save_dir = args.save_dir
    
    range_min = max(min_int, min_int // 2, sqrt(abs(min_int)) * abs(min_int) // min_int)
    range_max = min(max_int, max_int // 2, sqrt(abs(max_int)) * abs(max_int) // max_int)
    
    assignment_types = ['map']
    assignment_subtypes_ops = {'plus': '+', 'subtract': '-', 'times': '*'}
    assignment_subtypes = list(assignment_subtypes_ops.keys())
    assignment_list = []
    
    generated_assignment_num = 0
    while generated_assignment_num < assignment_num:
        subtype_id = randint(0, len(assignment_subtypes) - 1)
        subtype = assignment_subtypes[subtype_id]
        op = assignment_subtypes_ops[subtype]

        while op == '*' and max_int < 0:
            subtype_id = randint(0, len(assignment_subtypes) - 1)
            subtype = assignment_subtypes[subtype_id]
            op = assignment_subtypes_ops[subtype]

        num = randint(range_min, range_max)
        hole_num = 0

        if random() < use_hole_prob:
            num = '?'
            hole_num = randint(range_min, range_max)

        assignment_str = f'let f (x1 : int list) =\n\tmap (fun x2 -> x2 {op} {num}) x1\nin\nassert ('

        assert_generation_success = True
        for i in range(assert_num):
            operand_num = num
            if num == '?':
                operand_num = hole_num
            assert_int1, result1, success1 = generate_assert_int(operand_num, op, min_int, max_int, range_min, range_max)
            assert_int2, result2, success2 = generate_assert_int(operand_num, op, min_int, max_int, range_min, range_max)
            if not (success1 and success2):
                assert_generation_success = False
                break

            assert_str = f'(equal ({result1} :: {result2} :: []) (f ({assert_int1} :: {assert_int2} :: [])))'
            assignment_str += assert_str
            if i < assert_num - 1:
                assignment_str += ' && '
        if not assert_generation_success:
            continue
        assignment_str += ')\n'
        generated_assignment_num += 1
        assignment_list.append(assignment_str)
        # print(assignment_str)

    # write assignments to file
    save_dir = os.path.join(save_dir, '0')
    if not os.path.exists(save_dir):
        os.mkdir(save_dir)
    for i in range(assignment_num):
        path = os.path.join(save_dir, f'{i}.ml')
        with open(path, 'w') as ml_file:
            ml_file.write(assignment_list[i])
            
    with open(os.path.join(save_dir, 'test.ml'), 'w') as test_file:
        test_file.write("[]\n")
        
def generate_tuple():
    parser = argparse.ArgumentParser()
    parser.add_argument("--assignment_num", type=int, help="the number of assignments to create", default=10)
    parser.add_argument("--min_int", type=int, help="minimum integer created in assignments", default=-50)
    parser.add_argument("--max_int", type=int, help="maximum integer created in assignments", default=50)
    parser.add_argument("--tuple_num", type=float, help="number of tuples", default=4)
    parser.add_argument("--hole-pos", type=int, help="position of hole", default=0)
    parser.add_argument("--save_dir", help="directory to save assignments")
    parser.add_argument("--test_dir", default=None)
    args = parser.parse_args() 
    
    assignment_num = args.assignment_num
    min_int = args.min_int
    max_int = args.max_int
    tuple_num = args.tuple_num
    save_dir = args.save_dir
    test_dir = args.test_dir
    
    generated_assignment_num = 0
    assignment_list = []
    while generated_assignment_num < assignment_num:
        generated_tuple_num = 0
        int_exp = randint(min_int, max_int)
        
        if generated_tuple_num == args.hole_pos:
            exp = '?'
        else:
            exp = str(int_exp)
        
        sum = int_exp
        while generated_tuple_num < tuple_num - 1:
            generated_tuple_num += 1
            int_exp = randint(min_int, max_int)
            if generated_tuple_num == args.hole_pos:
                exp += f' + ?'
            else:
                exp += f' + {int_exp}'
            sum += int_exp
        
        exp = f'let f (x1 : int) =\n\t{exp}\nin\nassert (f 0 = {sum})\n'
        if sum <= 8:
            assignment_list.append(exp)
            generated_assignment_num += 1
    assignment_list = list(set(assignment_list))
    
    # write assignments to file
    if test_dir is not None:
        length = int(0.85 * len(assignment_list))
        print(f"{length} assignments and {len(assignment_list) - length} test assignments are created.")
        write_assignments(assignment_list[:length], save_dir)
        write_assignments(assignment_list[length:], test_dir)
    else:
        print(f"{len(assignment_list)} assignments are created.")
        write_assignments(assignment_list, save_dir)

            
if __name__ == "__main__":
    generate_tuple()