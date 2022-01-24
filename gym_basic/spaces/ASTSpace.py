from gym import spaces

class ASTSpace(spaces.Space):
    def __init__(self, shape=None, dtype=None) -> None:
        super().__init__(shape, dtype)
        data = spaces.Discrete(2)
        left = None
        right = None
    
    def sample(self):
        return super().sample()
    
    def contains(self, x) -> bool:
        return super().contains(x)