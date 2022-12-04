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

## Updates

### Architecture of application

```Haskell
data State 
  = Intro -- select the game mode, intro to the rules
  | BaseGame PlayState
  | UltimateGame PlayState
  | Outro -- record the result and show it
```

```haskell
data PlayState = PS
  { psX      :: Player.Player   -- ^ player X info
  , psO      :: Player.Player   -- ^ player O info
  , psScore  :: Score.Score     -- ^ current score
  , psBoard  :: Board.Board     -- ^ current board
  , psTurn   :: Board.XO        -- ^ whose turn 
  , psPos    :: Board.Pos       -- ^ current cursor
  , psResult :: Board.Result () -- ^ result      
 } 
```

```haskell
data Player = Player 
  { plName  :: String 
  , plStrat :: Strategy  -- (random or min max)
 } 
```

The project structure, mostly follow the base code, using the MVC framework:

```bash
├── src
│   ├── Control.hs
│   ├── Main.hs
│   ├── Model
│   │   ├── Board.hs
│   │   ├── Player.hs
│   │   └── Score.hs
│   ├── Model.hs
│   └── View.hs
└── tests
    └── Test.hs
```

### Challenge so far

- Understanding the base code (we hold several group meetings to discuss the code with each other) 
- Difficulty in dividing tasks for four group members (we separate the application to three parts, UI, ultimate tic-tac-toe and min-max algo implementation)
- the game logic and models for ultimate tic-tac-toe game (still in progress)
- the implementation of min-max algorithm in Haskell (still in progress)

### Expect to meet goals?

It takes longer than we expected to understand the base code, so we plan to finish up two features mentioned in the proposal. Comparing to the original future plan, we decide to keep the 9\*9 random opponent (ultimate tic tac toe) and min max 3*3 idea and delete the network socket functionality. In this way, the workload is more reasonable, and the task could be distributed evenly among two 2-people groups.
We choose this game because it is very famous and challenging. It contributes to children’s development growth including predictability and problem solving. Meanwhile, it is a good start up project to let us deeply understand the haskell, lambda function.


## Instruction 
* Compile with `stack build`
* Play with `stack run`
