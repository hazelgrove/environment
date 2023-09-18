import sympy as S 
from typing import List
## --- Test generation section ---
# 
# 
#  
class Node(): 
    def __init__(self,name:str, children): 
        if children is None: 
            self.terminal=True 
            self.children = []
        else: 
            self.terminal=False 
            self.children = children

        self.name=name
    
    def __str__(self): 
        if not self.terminal: 
            children_string = ', '.join(str(child) for child in self.children)
            return f'{self.name}({children_string})'
        else: 
            return self.name
    
    def __repr__(self): 
        return self.__str__()
                
    def to_ocaml(self):
        if self.terminal: 
            return self.name
        elif self.name in ['and','or'] and len(self.children) > 1: 
            operator = {"and":" && ","or":" || "}
            ret_string = self.children[0].to_ocaml()
            for child in self.children[1:]: 
                ret_string = f'( {operator[self.name].join([ret_string,child.to_ocaml()])} )'
            return ret_string
        elif self.name == 'not': 
            return f'(!({self.children[0].to_ocaml()}))'
        else: 
            raise ValueError(f"unknown node: {self.name} {str(self.children)}")

    def copy(self): 
        children = None 
        if not self.terminal: 
            children = list(map(lambda x: x.copy(),self.children))
        return Node(str(self.name),children)
    
    def set_val(self,other): 
        self.terminal = other.terminal
        self.children = other.children 
        self.name = other.name 
    
    def size(self): 
        return sum(child.size() for child in self.children)+1

    def __sizeof__(self) -> int:
        return self.size()

    @staticmethod    
    def Hole(): 
        return Node('?',None)
    
    @staticmethod    
    def Var(x_n:int): 
        assert(x_n > 0)
        return Node(f'x{x_n}',None)
    
    @staticmethod
    def Var_name(x_name:str):
        return Node(x_name,None)
    
    @staticmethod
    def Or(children): 
        assert(len(children)>=2)
        return Node('or',children)

    @staticmethod
    def And(children): 
        assert(len(children)>=2)
        return Node('and',children)

    @staticmethod    
    def Bool(val): 
        name = 'true' if val else 'false' 
        return Node(name,None)

    @staticmethod
    def Not(child_node): 
        return Node('not',[child_node])
    
    @staticmethod
    def from_eda(eda_object): 
        return Node.from_ast(eda_object.to_ast())
    
    @staticmethod
    def from_ast(input):
        if len(input) == 2 and input[0] == 'const': 
            # atom: Boolean constant
            return Node.Bool(input[1])
        elif len(input) == 2 and input[0] == 'lit': 
            # atom: other 
            if input[1] < 0: # negated 
                return Node.Not(Node.Var(-input[1]))
            else: 
                return Node.Var(input[1])
        if input[0] in ['and', 'or']:
            # operator.... 
            return Node(input[0],list(map(Node.from_ast,input[1:])))
        else: 
            print(f'UNKNOWN OPERATOR {input}')
            raise ValueError(f'unknown operator {input}')
        
    @staticmethod    
    def from_sympy(expr): 
        if type(expr) is S.Not: 
            return Node.Not(Node.from_sympy(expr.args[0]))
        elif type(expr) is S.And or type(expr) is S.Or: 
            op =  Node.And if type(expr) is S.And else Node.Or
            return op([Node.from_sympy(child) for child in expr.args])
            # n-ary ops 
        elif type(expr) is S.Symbol: 
            return Node.Var_name(str(expr))
        elif  expr is S.true or expr is S.false:  
            return Node.Bool(expr)
        else: 
            raise NotImplementedError(f'unknown expr {expr}')


def __curriculum_helper(list,root,curr,nth):
    list.append((root.copy(),nth))
    # 1. iterate across children: all but leftmost (right to left) 
    if not curr.terminal: 
        # get children node_nums
        children_node_nums = [] 
        prev_nodes = 1 + nth
        for child in curr.children: 
            children_node_nums.append(prev_nodes)
            prev_nodes += child.size()
        # recurse across children 
        for i in range(len(curr.children)-1,0,-1):
            # 2.   tree, ... = gen_steps(child_n)
            list,curr.children[i] =  __curriculum_helper(list,root,curr.children[i],children_node_nums[i])
            list.append((root.copy(),children_node_nums[i])) 
        # 4.    unwrap child; save; return tree 
        while( not curr.terminal): 
            curr.set_val(curr.children[0])
            # list.append((root.copy(),nth))
            __curriculum_helper(list, root, curr,nth)
        return list, curr
    else: 
    # 6.   replace node with hole (should be done)
    # 7    exit. return tree 
       curr.set_val(Node.Hole())
       return list,curr
    
def make_curriculum(node):
    tests, curr = __curriculum_helper([],node,node,0)
    tests.append((curr,0))

    # unzip 
    tests, cursor_starts = list(map(list, zip(*tests)))
    return tests, cursor_starts

def pretty_print_list(l): 
    for ls,ln in l: 
        print(ln,ls)

if __name__== "__main__": 
    node1  = Node.And([Node.Var(2),Node('or',[Node.Not(Node.Var(1)),Node.Var(3)])])
    print(node1)
    tests, starts = make_curriculum(node1)
    pretty_print_list(zip(tests,starts))

    node2  = Node.Or([Node.And([Node.Var(1),Node.Var(2)]),Node.And([Node.Not(Node.Var(1)),Node.Not(Node.Var(2))])])
    print(node2)
    tests, starts = make_curriculum(node2)
    pretty_print_list(zip(tests,starts))

    node3  = Node.Or([Node.Not(Node.Var(1)),Node.Not(Node.Not(Node.Var(2))),Node.Not(Node.Var(3))])
    print(node3)
    tests, starts = make_curriculum(node3)
    pretty_print_list(zip(tests,starts))
