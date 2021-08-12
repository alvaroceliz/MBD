# Python

## Human Behavior Prediction

The study of the predictability of human behavior is an intense field of research, mainly for its applications in stock exchanges, in games, in government treatises, sports and also to identify which student will give up the python course. Recently, computational methods have been surpassing the human capacity to predict behavior.

In this programming exercise, we will implement a program that "predicts" human behavior using a simple game.

The game consists of a human player and the computer. Both choose either the value 0 or the value 1. If both choose the same numbers, the computer earns a point. If the numbers chosen are different, the human player earns a point. This is repeated n times. The winner is the one who accumulates more points at the end of n moves. The strategy to be implemented for the computer to "predict" the human player's bid is based on a simple "machine learning" algorithm.

This learning assumes that every choice the player makes is influenced by his/her previous move. In this way four counts will be made:

(1) throw00 = count of the number of times the human player chose 0 given that in the previous bid his/her bid was 0,

(2) throw01 = count of the number of times the human player chose 0 given in the bid previous his/her bid was 1,

(3) throw10 = count of the number of times the human player chose 1 given that his/her previous bid was 0,

(4) throw11 = count of the number of times the human player chose 1 given his/her previous bid was 1.

So, the following cases may occur:

If the player's last throw was 0:

1. If throw10 > throw00: then the computer chooses 1

2. If throw10 < throw00: then the computer chooses 0

3. If throw10 = throw00: then the computer chooses randomly 0 or 1.

If the player's last throw was 1:

4. If throw11 > throw01: then the computer chooses 1

5. If throw11 < throw01: then the computer chooses 0

6. If throw11 = throw01: then the computer chooses randomly 0 or 1.

To avoid cheating, in your code, always make the computer choice 0 or 1 before asking the user for its choice of move 0 or 1.

For the first throw of the computer, there is no previous throw of the player, so computer chooses randomly 0 or 1.

For the random throw, proceed as follows. Generate a random number using the linear congruences method. If the random number is less than or equal to 232/2 (=231), the computer will play 0. Otherwise, it will play 1. To facilitate the structure of your program, generate a new random number at the beginning of each move, and use it only if necessary according to the rules above. If the random number is not used it will be discarded at the beginning of the next move when a new number is generated.

Only when you initialize your python game, it will require an initial seed x0, manually choose an integer value of your choice between 1 and 231.

After n rounds, wins who gets the most points (player or computer).

Your Individual Programming Exercise (IPE) should receive as input (via keyboard) the number of moves to be made (positive integer) and difficulty of the game (easy or difficult).

In "easy" mode, the computer has no learning, i.e., all computer throws are random, using the method of linear congruences. If the random number generated is less than or equal to 232/2 (=231), the computer will choose 0 and otherwise, 1.

In “difficult” mode, the computer will use the learning method described above.

During the match, your IPE should show graphically how the game is going, i.e. print a bar that illustrates the score of the computer and the score of the player. The score will be simulated by the impression of asterisks.

At the end of the game (after the n-th0 round), you must print on the screen who was the winner or if there was a tie.
