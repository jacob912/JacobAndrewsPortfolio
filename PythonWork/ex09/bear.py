"""File to define Bear class."""


class Bear:
    """Creating the Bear class."""
    
    age: int
    hunger_score: int

    def __init__(self):
        """Initializing values for Bears."""
        self.age = 0
        self.hunger_score = 0
        return None
    
    def one_day(self):
        """Simulating one day going by for the bears."""
        self.age += 1
        self.hunger_score -= 1
        return None
    
    def eat(self, num_fish: int):
        """Simulating a bear eating."""
        self.hunger_score += num_fish
        return None