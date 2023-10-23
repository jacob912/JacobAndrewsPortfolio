"""File to define Fish class."""


class Fish:
    """Creating the Fish class."""

    age: int

    def __init__(self):
        """Initializing values for Fish."""
        self.age = 0
        return None
    
    def one_day(self):
        """Simulating one day going by for fish."""
        self.age += 1
        return None