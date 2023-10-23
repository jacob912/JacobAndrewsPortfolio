"""File to define River class."""

__author__ = "730463543"

from exercises.ex09.fish import Fish
from exercises.ex09.bear import Bear

from fish import Fish
from bear import Bear

class River:
    """Creating the River class."""

    day: int
    bears: list[Bear]
    fish: list[Fish]

    def __init__(self, num_fish: int, num_bears: int):
        """New River with num_fish Fish and num_bears Bears."""
        self.day: int = 0
        self.fish: list[Fish] = []
        self.bears: list[Bear] = []
        # populate the river with fish and bears
        for x in range(0, num_fish):
            self.fish.append(Fish())
        for x in range(0, num_bears):
            self.bears.append(Bear())

    def check_ages(self):
        return None

    def bears_eating(self):
        """Simulating a bear eating."""
        for bear in self.bears:
            if len(self.fish) >= 5:
                bear.eat(3)
                self.remove_fish(3)
        return None
    
    def check_hunger(self):
        return None
                
    def check_ages(self):
        return None
    
    def remove_fish(self, amount: int):
        """Removing fish."""
        for int in range(amount):
            self.fish.pop(0)
        
    def repopulate_fish(self):
        """Repopulating fish."""
        offspring: int = (len(self.fish) // 2) * 4
        for x in range(0, offspring):
            self.fish.append(Fish())
        return None
    
    def repopulate_bears(self):
        """Repopulating bears."""
        offspring: int = len(self.bears) // 2
        for x in range(0, offspring):
            self.bears.append(Bear())
        return None
    
    def view_river(self):
        """Viewing the status of the river."""
        print(f"~~~ Day {self.day}: ~~~")
        print(f"Fish population: {len(self.fish)}")
        print(f"Bear population: {len(self.bears)}")
        return None
            
    def one_river_day(self):
        # Increase day by 1
        self.day += 1
        # Simulate one day for all Bears
        for bear in self.bears:
            bear.one_day()
        # Simulate one day for all Fish
        for fish in self.fish:
            fish.one_day()
        # Simulate Bear's eating
        self.bears_eating()
        # Remove hungry Bear's from River
        self.check_hunger()
        # Remove old Fish and Bear's from River
        self.check_ages()
        # Simulate Fish repopulation
        self.repopulate_fish()
        # Simulate Bear repopulation
        self.repopulate_bears()
        # Visualize River
        self.view_river()

    def one_river_week(self):
        """Simulating one week."""
        for x in range(0, 7):
            self.one_river_day()
            
