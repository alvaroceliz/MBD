# Name of the project: Individual Programing Exercise Python
# Description: Human Behavior Prediction
# Author: Alvaro Céliz Llorente
# Last revision: 5/February/2021

# 0. Pre-game
def random_choice(xi):
    '''Function to generate a number between 0 and 1 based on a model (Level 1). Also, it is used as a random variable (Level 2)'''
    a = 22695477
    b = 1
    m = 2**32
    xi_plus_1 = (a * xi + b) % m
    if xi_plus_1 <= 2**31:
        comp_move = 0
    else:
        comp_move = 1
    return comp_move, xi_plus_1
# Different variables for the results and score in each moment per player
throw10 = 0
throw00 = 0
throw11 = 0
throw01 = 0
previous_move = 2
MS = 0
PS = 0
xi = 1234
player_wins=0
computer_wins=0

def computer_decision(throw00, throw01, throw10, throw11, previous_move, xi):
    '''Function based on the last move of the player and with an algorithm given (Level 2)'''
    if previous_move == 0: 
        if throw10 > throw00:
            comp_dec = 1
        if throw10 < throw00:
            comp_dec = 0
        else:
            comp_dec,xi = random_choice(xi) 
    if previous_move == 1: 
        if throw11 > throw01:
            comp_dec = 1
        if throw11 < throw01:
            comp_dec = 0
        else:
            comp_dec, xi = random_choice(xi)       
    return comp_dec
def playAgain():
    '''Function to play again'''
    print("Do you want to play again? (yes or no)")
    return input().lower().startswith('y')

# 1. Create opening message
print("Welcome to Human Behavior Prediction by Alvaro Celiz")
# 2. Code to create the level selection
gameIsPlaying = True
while gameIsPlaying:
    PS, MS = (0,0)
    while True:    
        try :
            select_difficulty = int(input("Choose the type of game (1:Easy; 2:Dificult):"))
            if select_difficulty == 2:
                break
            elif select_difficulty == 1:
                break
            else:
                print("Pleeease, there are only 2 options")
        except:
            print("You must select a level 1 or 2")    
# 3. Code to create how many moves the player wants to play
    while True:
        try:
            moves= int(input("Enter the number of moves:"))
            if moves > 0:
                break
            else: print("""Seriously... don't you want to play?""")
        except:
            print("Wrong! Enter a number, please")   
# 4. Code to choose your move number
    for i in range (1,moves+1):
        while True:
            try:
                player_move =int(input("Choose your move number (0 or 1):"))
                if player_move == 1:
                    break
                elif player_move == 0:
                    break
                else:
                    print("Sorry but there are only 2 options: 1 or 0")
            except:
                print("The only accepted input are numbers")
        # Here, the 2 options (Easy - random or Difficult - throw) are applied to each case
        if select_difficulty == 1:
            computer_move,xi = random_choice(xi)
        else:
            if i <= 1:
                computer_move, xi = random_choice(xi)
            else:
                computer_move = computer_decision(throw00, throw01, throw10, throw11, previous_move, xi)
        previous_move = player_move
        if player_move == 0 and previous_move == 0:
            throw00 = throw00 + 1
        elif player_move == 1 and previous_move == 1:
            throw11 = throw11 + 1
        elif player_move == 0 and previous_move == 1:
            throw01 = throw01 + 1
        elif player_move == 1 and previous_move == 0:
            throw10 = throw10 + 1
        # Comparison between the player and computer move to update who won in each round
        if player_move == computer_move:
            MS = MS + 1
            result= "Computer wins"
        elif player_move != computer_move:
            PS = PS + 1
            result= "Player wins"
        # Message for each time a round finishes
        print("The choices are: Player:" , player_move ,"","""Machine:""", computer_move, "" ,"-", result)
        print("Cumulative: You: %d Computer %d" % (PS, MS))
        print("PLAYER: " + '*'*PS)
        print("COMPUTER: " + '*'*MS)  
# 5. Code to print that the game has finished
    if select_difficulty == 1:
        print("The easy game has just finished, the final score is Player" , PS , "-" , MS , "Computer")
        if PS > MS:
            player_wins = player_wins + 1
            print("Congratulations. You won!")
            print("TOTAL GAMES. PLAYER:", player_wins, "COMPUTER:", computer_wins)
        elif PS < MS:
            computer_wins = computer_wins + 1
            print("Sorry, next time... Computer won" )
            print("TOTAL GAMES. PLAYER:", player_wins, "COMPUTER:", computer_wins)
        else: 
            print( "It was a tie")
            print("You can play again against the computer and see if you are able to beat it")
    else:
        print("The difficult game has just finished, the final score is Player" , PS , "-" , MS , "Computer")
        if PS > MS:
            player_wins = player_wins + 1
            print("Congratulations. You won!")
            print("TOTAL GAMES. PLAYER:", player_wins, "COMPUTER:", computer_wins)
        elif PS < MS:
            computer_wins = computer_wins + 1
            print( "Sorry, you need to train more... Computer won")
            print("TOTAL GAMES. PLAYER:", player_wins, "COMPUTER:", computer_wins)
        else: 
            print( "It was a tie")
            print("You can play again against the computer and see if you are able to beat it")
    # Ask if the player wants to play again
    if not playAgain():
        break