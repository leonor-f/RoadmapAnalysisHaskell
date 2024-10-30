import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

type AdjList = [(City,[(City,Distance)])]
type AdjMatrix = Data.Array.Array (Int,Int) (Maybe Distance)
data AdjPointers = Place City [(AdjPointers, Distance)]



-- 1. Return all the cities in the graph

cities :: RoadMap -> [City]
cities [] = []
cities ((c1, c2, _):resto) = addCity c1 (addCity c2 (cities resto))                                 -- if the road map is not empty, add the first city of the first tuple to the list of cities, then add the second city
                                                                                                    -- of the first tuple to the list of cities, then call the function recursively with the rest of the road map
    where
        addCity city citiesList = if city `elem` citiesList then citiesList else city : citiesList  -- helper function that adds a city to a list of cities if it is not already there



-- 2. Return a boolean indicating whether two cities are linked directly

areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent rm c1 c2 = any (\(x, y, _) -> (x == c1 && y == c2) || (x == c2 && y == c1)) rm          -- if both cities are in the list of cities, check if there exists a tuple (x, y, _) in the
                                                                                                    -- road map such that (x == c1 && y == c2) or (x == c2 && y == c1), if it is, return True
                                                                                                    -- otherwise, return False



-- 3. Return a Just value with the distance between two cities connected directly, given two city names, and Nothing otherwise

distance :: RoadMap -> City -> City -> Maybe Distance
distance rm c1 c2
    | c1 == c2 = Just 0                                                                             -- if the two cities are the same, distance is 0
    | otherwise = case filter (\(x, y, _) -> (x == c1 && y == c2) || (x == c2 && y == c1)) rm of
        [] -> Nothing                                                                               -- if c1 and c2 aren't connected directly, return Nothing
        (_, _, d):_ -> Just d                                                                       -- if c1 and c2 are a tuple in the road map (connected directly), return the distance



-- 4. Return a list of cities adjacent to a given city and the respective distances to them

adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent rm c = [(c2, d) | (c1, c2, d) <- rm, c1 == c] ++ [(c1, d) | (c1, c2, d) <- rm, c2 == c]    -- for each tuple (c1, c2, d) in the road map:
                                                                                                    -- if c1 = c, add (c2, d) to the list
                                                                                                    -- if c2 = c, add (c1, d) to the list



{- 5. Return the sum of all individual distances in a path between two cities in a Just value,
      if all the consecutive pairs of cities are directly connected by roads.
      Otherwise, return a Nothing.
-}

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance rm path = if all (\(c1, c2) -> areAdjacent rm c1 c2) (getPair path)                     -- if all the consecutive pairs of cities are directly connected by roads
                       then Just (sum [d | (c1, c2) <- getPair path, Just d <- [distance rm c1 c2]]) -- return the sum of all individual distances in the path, using the distance function
                       else Nothing


-- Auxiliary function that creates pairs with the elements of a list 2 by 2
getPair :: [a] -> [(a, a)]
getPair xs = zip xs (tail xs)



-- 6. Return the names of the cities with the highest nº of roads connecting to them

rome :: RoadMap -> [City]
rome rm = [c | c <- cities rm, length (adjacent rm c) == maxAdjacents]                              -- for each unique city c in the road map, add it to the list if the nº of adjacent cities to c =
                                                                                                    -- = max nº of adjacent cities, using the length of the adjacent function to that city c
    where
        maxAdjacents = maximum [length (adjacent rm c) | (c, _, _) <- rm]                           -- maximum number of adjacent cities to a city in the road map (highest nº of roads connecting to a city)



-- 7. Check if the roadmap is fully connected

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected rm = all (\c -> all (\c' -> not (null (findPaths rm c c'))) (cities rm)) (cities rm) -- for each city c in the road map, check if there exists a path from c to c' for all cities c' in the road map
                                                                                                         -- if it is, return True, otherwise, return False


-- Auxiliary function to find all paths
findPaths :: RoadMap -> City -> City -> [Path]
findPaths roadmap cityA cityB = findPaths' cityA cityB [] where
    findPaths' current target visited
        | current == target = [[current]]
        | otherwise = [current:path | (next, _) <- adjacent roadmap current, next `notElem` visited, path <- findPaths' next target (current:visited)]



-- 8. Return all shortest paths between two cities

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath rm c1 c2
    | c1 == c2  = [[c1]]                                                                                -- if the two cities are the same, return a single-element path
    | otherwise = filter ((== minDist) . pathDistance') paths                                           -- filter paths with the minimum distance
    where
        paths = bfsPaths rm c1 c2                                                                       -- find all possible paths from c1 to c2
        minDist = minimum (map pathDistance' paths)                                                     -- calculate the minimum path distance
        pathDistance' p = case pathDistance rm p of                                                     -- helper function to get distance from Maybe Distance
            Just d  -> d
            Nothing -> maxBound :: Int                                                                  -- use maxBound to ignore invalid paths


-- Auxiliary function that performs BFS and accumulates paths between two cities
bfsPaths :: RoadMap -> City -> City -> [Path]
bfsPaths rm start goal = bfs [[start]] []
  where
    bfs [] paths = paths                                                                                -- if there are no more paths to explore, return all found paths
    bfs (currentPath:remainingPaths) foundPaths
        | last currentPath == goal = bfs remainingPaths (currentPath : foundPaths)                      -- if we reached the goal, add to results
        | otherwise =
            let nextCities = [next | (next, _) <- adjacent rm (last currentPath), next `notElem` currentPath] -- get next cities
                newPaths = [currentPath ++ [next] | next <- nextCities]                                 -- extend paths to each next city
            in bfs (remainingPaths ++ newPaths) foundPaths



-- 9. Solve the Traveling Salesman Problem using dynamic programming

travelSales :: RoadMap -> Path
travelSales = undefined



tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function




-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]
