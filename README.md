# Hex-Game README file
## PPM College Project

## Documentation

### BoardState
This case class / companion object is used to represent a GameState. <br>
Each GameState is composed by a **Board** and a **UnionFind**.
<br><br>
* ##### draw and drawFold
Unit/String functions that allow for a visual representation of the **Board** via Text User Interface.<br>draw uses 2 tail recursive nested functions while drawFold uses a really extended foldRight. Although the latter one occupies fewer lines of code, it is a lot slower.
<br><br>
* ##### getInput
This function returns a valid coordinate chosen by the user
<br><br>
* ##### play (coordinates, piece) 
This function returns a new **BoardState** with *piece* played at *coordinates*
<br><br>
* ##### playCPU(randomState)
This function returns a valid random coordinate 
<br><br>
* ##### playCPUAdj(randomState, oldCoord)
This function returns a valid coordinate adjacent to *oldCoord*, if no valid coordinate is found then it returns a random valid coordinate
<br><br>
* ##### valid(coordinates)
Used to check if *coordinates* corresponds to an empty slot on the **Board** and is within the boundaries
<br><br>
* ##### hasContinuousLine
Uses a **UnionFind** data structure to check for a possible winner
<br><br>
* ##### defineBoard(size)
A companion object function to initialize the BoardState of length *size*
<br><br>

### UnionFind
This case class / companion object represents a simple Union Find data structure, it is useful to check for a possible winner while having a low time complexity
<br><br>
* ##### createUnions(coordinates, board, piece)
This function returns a new UnionFind where the tile *coordinates* gets connected to the adjacent tiles that have matching pieces
<br><br>
* ##### percolates(board)
This function uses foldRights to check for a possible winner in *board* with a quadratic time complexity.
<br>Note that it would be possible for a constant time complexity if we had implemented the Quick-Find algorithm, however we agreed that a quadratic time complexity would already be a good enough optimization since the brute-force/naive approach would be an estimate of O(6^n) 
* ##### init(size)
This companion object function initializes the base data for the Union Find structure

