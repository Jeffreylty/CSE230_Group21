# CSE230 Group21

**group member**

Yuxin Li, Tianyi Liu, Yiwen Zou, Jialu Xu

## Proposal

4th Nov 2022

For the project, we propose to implement the Tic-Tac-Toe game. The game in which two players seek in alternate turns to complete a row, a column, or a diagonal with either three O's or three X's drawn in the spaces of a grid of nine squares. We first plan to fulfill the basic game and implement more advanced features.

- Future Plan
    - Introduce AI-based players to the game using the min-max algorithm.

    - Network based multiplayer function (if extra time available). Use the `Network.Socket` module to build a TCP client-server application that support two players to join the game room and play against each other.

    - Extend the basic 3x3 tic tac toe into a 9x9 [`ultimate tic tac toe`](https://ultimate-t3.herokuapp.com/rules). Our 9x9 game board is divided into 9 sub 3x3 tic tac toe games, which means the 81-square grid is made of 3x3 cells, each is a 9-square grid. The winner of each 9-square game will occupy the whole 3x3 cell in the larger board. Similar to the normal tic tac toe, once someone wins three larger cells in a row, a column, or a diagonal then he/she wins the whole game.

We choose this game because it is very famous and challenging. It contributes to children’s development growth including predictability and problem solving. Meanwhile, it is a good start up project to let us deeply understand the haskell, lambda function. 


