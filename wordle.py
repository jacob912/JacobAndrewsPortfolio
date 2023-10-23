"""EX03 - Structured Wordle"""

__author__ = "730463543"



def contains_char(word: str, character: str) -> bool:  #have to return True or False so boolean value
    """Searching the word to see if it contains the character inputed"""
    assert len(character) == 1
    idx_word: int = 0
    while idx_word < len(word):  #setting up while loop so it can only go up to the length of the word
        if word[idx_word] == character:
            return True
        else:
            if idx_word == len(word) - 1 and word[idx_word] != character:  #using "and" operator to say if the letter of the word at that index doesn't match character and we are at the last index 
                return False
            else:
                idx_word = idx_word + 1  #adding 1 to index to go back to top of while loop
    return False
            
def emojified(guess: str, secret: str) -> str:
    """Creating the iconic wordle emoji string for correct, incorrect, and partly correct letters"""
    assert len(guess) == len(secret)
    WHITE_BOX: str = "\U00002B1C"
    GREEN_BOX: str = "\U0001F7E9"
    YELLOW_BOX: str = "\U0001F7E8"
    emoji_string: str = ""
    idx_word: int = 0
    while idx_word < len(secret):
        if guess[idx_word] == secret[idx_word]:  #if the letter at the index is the same in both the guess and secret
            emoji_string = str(emoji_string) + str(GREEN_BOX)  #add green box to emoji string
        else:
            if contains_char(str(secret), str(guess[idx_word])) == True:  #uses our function to see if the letter of the guess is anywhere in the secret word
                emoji_string = str(emoji_string) + str(YELLOW_BOX)  #adds a yellow box to emoji string
            else:  #letter of the guess is not in secret word
                emoji_string = str(emoji_string) + str(WHITE_BOX)  #adds a white box to emoji string
        idx_word = idx_word + 1  #adds one to the index to go back to top of while loop
    return emoji_string 

def input_guess(input_length: int) -> str:
    """Returning an input if it is the same length as the number stated"""
    input_word: str = str(input(f"Enter a {input_length} character word: "))
    inputting: bool = True  #created so I can have a while loop to keep the inputting going if they don't input a word of the right length
    while inputting == True:
        if len(input_word) == input_length:  #input is the correct length
            return input_word
        else:  #input is not the correct length
            input_word = input(f"That wasn't {input_length} chars! Try again: ")
    return input_word

def main() -> None:
    """The entrypoint of the program and main game loop"""
    secret_word: str = "codes"
    turn: int = 1
    has_won: bool = False
    while turn <= 6 and has_won == False:  #makes a loop that goes on until they win or exceed 6 guesses
        print(f"=== Turn {turn}/6 ===")
        i_g: str = input_guess(len(secret_word))  #stores the value returned from input_guess in i_g so I can use it in emojified
        print(emojified(i_g, secret_word))  #prints the value returned from emojifying the comparison of i_g and the secret word
        if i_g == secret_word:  #if the guess is the same as the secret word
            has_won = True
        else:  #if the guess is not the same as the secret word
            turn = turn + 1
    if has_won == True:  #if they won
        print(f"You won in {turn}/6 turns!")
        return
    else:  #if they did not win
        print("X/6 - Sorry, try again tomorrow!")

if __name__ == "__main__":
    main()