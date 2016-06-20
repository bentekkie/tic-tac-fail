# tic-tac-fail
To test it out run tictactoe.rkt.

Then send the application the state of the game board using the url localhost:8000/move/{state}

An example of an empty board state is ...,...,...

Each dot represents an empty space and commas separate rows

Your http get call will get a response of the state after the computer has played its move, if this state is the same as the one you sent the game is over due to one player winning or a tie.

You are always playing as X, Use an X in place of a dot to play a move
