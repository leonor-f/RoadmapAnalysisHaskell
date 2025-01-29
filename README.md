# Roadmap Analysis in Haskell
Analysis of a roadmap using Haskell, including shortest path calculation using BFS and Traveling Salesman Problem (TSP) solution with dynamic programming.

#### Grade: 19/20

## Project description
This project involved the development of several functions to allow manipulation, navigation and analysis of a roadmap. The roadmap was represented in a graph, where vertices are cities and edges are roads defined by tuples containing the city names and the distance between them. The graph is undirected, allowing traversal in both directions.

## Project execution
The program is compiled by typing ```ghci TP1.hs``` on the terminal on the root of the project’s directory.<br><br>
Once GHCi is loaded, each function is run by typing the function’s name on the GHCi terminal, followed by the roadmap to analyze and then the values of the parameters of that function.<br><br>
For testing the program with the provided test cases, the name of the roadmap to analyze is gTestNo, where No corresponds to the test case number (1, 2, or 3).

## Developed functions
```haskell
cities :: RoadMap -> [City]
-- returns a list of all unique cities in the roadmap.
areAdjacent :: RoadMap -> City -> City -> Bool
-- checks if two cities are directly connected.
distance :: RoadMap -> City -> City -> Maybe Distance
-- returns the distance between two adjacent cities, or Nothing if they are not adjacent.
adjacent :: RoadMap -> City -> [(City, Distance)]
-- returns all adjacent cities to a given city and the distances to it.
pathDistance :: RoadMap -> Path -> Distance
-- returns the sum of all distances in the given path.
rome :: RoadMap -> [City]
-- returns the city or cities with the highest number of direct connections (highest degree).
isStronglyConnected :: RoadMap -> Bool
-- determines whether all cities in the roadmap are reachable from every other city.
shortestPath :: RoadMap -> City -> City -> [Path]
-- computes all shortest paths between two cities.
travelSales :: RoadMap -> Path
-- solves the Traveling Salesman Problem and returns an optimal path.
```
