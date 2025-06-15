
This document is aimed at summing up some ideas for the poker simulator modelisation.

# Representation of state :
    - a vector of binary (or boolean) variables whose size is the number of players on the hand. 1 if active in the hand, 0 otherwise.
    - a vector of integers that represent the amount of money players bet in the hand
    - a vector of integers that represent the amount of money they have in hand
    - a vector of cards in hand that represent cards the player has
    - an integer that stands for the maximal bet for this hand 
    - an integer that represents the player who has to do an action
    - the cards displayed (a list of cards)
    - the state of the game (preflop, flop, turn,river)

We have also to define an ending state.

# Representation of actions : 
    - Fold at any moment
    - Check if maximal bet on the hand = money bet by the player (who has to play)
    - Bet (bet/raise/call) if cash in hand >= (maximal bet - money bet by the player before)

# Representation of dynamics : 

Here, we think about how to design the relation (S,A)-> S. In other terms, we have to describe for any state action couple its outcome state. Let's express our ideas "en vrac" : 

    - For any state, if fold, the state is modified with the three possibilities : (1) the following player has to play, (2) the hand ends (if one player is remaining) or the player was the last to talk in the river=> ending state, (3) we go to the next step

    - For any state, if the action check is possible => two possibilites (1), if the player was the last person having to take action in the step, we go to the next step (flop, turn, river) or the hand ends. (2) if there are some players that have to take action, we simply make the state being modified such that the following players has to speak

    - For any state, if the action bet/raise is possible, we set the states such that the following player still active in the hand that has its bet on the hand < maximal bet has to play

    - For any state, if the action call occurs, there are several possibilities : (1) if this was the last player to talk, we go to the next step (possible that it is the ending state) (2) we set the state so that the following player has to take action