import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int
type Bit = Integer

type RoadMap = [(City,City,Distance)]

type AdjList = [(City,[(City,Distance)])]
type AdjMatrix = Data.Array.Array (Int,Int) (Maybe Distance)
data AdjPointers = Place City [(AdjPointers, Distance)]



-- 1. Return all the cities in the graph

-- | Returns all the unique cities in the roadmap.
-- Arguments:
--   rm: The roadmap to extract cities from.
-- Returns:
--   A list of cities in the roadmap.
-- Time Complexity: O(n^2), where n is the number of cities in the roadmap due to the list traversal and `elem` checks.

cities :: RoadMap -> [City]
cities [] = []
cities ((c1, c2, _):resto) = addCity c1 (addCity c2 (cities resto))                                 -- if the roadmap is not empty, add the first city of the first tuple to the list of cities, then add the second city
                                                                                                    -- of the first tuple to the list of cities, then call the function recursively with the rest of the roadmap
    where
        addCity city citiesList = if city `elem` citiesList then citiesList else city : citiesList  -- helper function that adds a city to a list of cities if it is not already there



-- 2. Return a boolean indicating whether two cities are linked directly

-- | Checks if two cities are directly connected in the roadmap.
-- Arguments:
--   rm: The roadmap to check.
--   c1: The first city.
--   c2: The second city.
-- Returns:
--   True if the cities are directly connected, False otherwise.
-- Time Complexity: O(m), where m is the number of roads in the roadmap since it checks each road for adjacency.

areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent rm c1 c2 = any (\(x, y, _) -> (x == c1 && y == c2) || (x == c2 && y == c1)) rm          -- if both cities are in the list of cities, check if there exists a tuple (x, y, _) in the
                                                                                                    -- roadmap such that (x == c1 && y == c2) or (x == c2 && y == c1)
                                                                                                    -- if it is, return True, otherwise, return False



-- 3. Return a Just value with the distance between two cities connected directly, given two city names, and Nothing otherwise

-- | Returns the distance between two cities if they are directly connected.
-- Arguments:
--   rm: The roadmap to check.
--   c1: The first city.
--   c2: The second city.
-- Returns:
--   Just distance if the cities are directly connected, Nothing otherwise.
-- Time Complexity: O(m), where m is the number of roads in the roadmap because of filtering through all roads to find a connection.

distance :: RoadMap -> City -> City -> Maybe Distance
distance rm c1 c2
    | c1 == c2 = Just 0                                                                             -- if the two cities are the same, distance is 0
    | otherwise = case filter (\(x, y, _) -> (x == c1 && y == c2) || (x == c2 && y == c1)) rm of
        [] -> Nothing                                                                               -- if c1 and c2 aren't connected directly, return Nothing
        (_, _, d):_ -> Just d                                                                       -- if c1 and c2 are a tuple in the roadmap (connected directly), return the distance



-- 4. Return a list of cities adjacent to a given city and the respective distances to them

-- | Returns the list of cities adjacent to a given city.
-- Arguments:
--   rm: The roadmap to check.
--   c: The city to find adjacent cities for.
-- Returns:
--   A list of cities adjacent to the given city.
-- Time Complexity: O(m), where m is the number of roads in the roadmap as it checks both directions for adjacency.

adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent rm c = [(c2, d) | (c1, c2, d) <- rm, c1 == c] ++ [(c1, d) | (c1, c2, d) <- rm, c2 == c]    -- for each tuple (c1, c2, d) in the roadmap:
                                                                                                    -- if c1 = c, add (c2, d) to the list
                                                                                                    -- if c2 = c, add (c1, d) to the list



{- 5. Return the sum of all individual distances in a path between two cities in a Just value,
      if all the consecutive pairs of cities are directly connected by roads.
      Otherwise, return a Nothing.
-}

-- | Returns the sum of all individual distances in a path between two cities.
-- Arguments:
--   rm: The roadmap to check.
--   path: The path to calculate the distance for.
-- Returns:
--   Just the sum of distances if all consecutive pairs of cities are directly connected, Nothing otherwise.
-- Time Complexity: O(k * m), where k is the length of the path and m is the number of roads in the roadmap due to the checks for adjacency and distance calculations.

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance rm path = if all (\(c1, c2) -> areAdjacent rm c1 c2) (getPair path)                     -- if all the consecutive pairs of cities are directly connected by roads
                       then Just (sum [d | (c1, c2) <- getPair path, Just d <- [distance rm c1 c2]]) -- return the sum of all individual distances in the path, using the distance function
                       else Nothing


-- | Auxiliary function that creates pairs with the elements of a list 2 by 2.
-- Arguments:
--   xs: The list to create pairs from.
-- Returns:
--   A list of pairs created from the input list.
-- Time Complexity: O(n), where n is the length of the input list `xs`, as it traverses the list once to create pairs with the head and tail of the list.

getPair :: [a] -> [(a, a)]
getPair xs = zip xs (tail xs)



-- 6. Return the names of the cities with the highest nº of roads connecting to them

-- | Returns the names of the cities with the highest number of roads connecting to them.
-- Arguments:
--   rm: The roadmap to check.
-- Returns:
--   A list of cities with the highest number of roads connecting to them.
-- Time Complexity: O(n * m), where n is the number of cities and m is the number of roads as it checks the adjacency for each city.

rome :: RoadMap -> [City]
rome rm = [c | c <- cities rm, length (adjacent rm c) == maxAdjacents]                              -- for each unique city c in the roadmap, add it to the list if the nº of adjacent cities to c =
                                                                                                    -- = max nº of adjacent cities, using the length of the adjacent function to that city c
    where
        maxAdjacents = maximum [length (adjacent rm c) | (c, _, _) <- rm]                           -- maximum number of adjacent cities to a city in the roadmap (highest nº of roads connecting to a city)



-- 7. Check if the roadmap is fully connected

-- | Checks if the roadmap is strongly connected.
-- A roadmap is strongly connected if there is a path between any two cities.
-- Arguments:
--   rm: The roadmap to check.
-- Returns:
--   True if the roadmap is strongly connected, False otherwise.
-- Time Complexity: O(m + n), where n is the number of cities and m is the number of roads, due to the BFS search.

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected rm = all (\c -> all (\c' -> not (null (bfsPaths rm c c'))) (cities rm)) (cities rm) -- for each city c in the roadmap, check if there exists a path from c to c' for all cities c' in the roadmap
                                                                                                        -- if it is, return True, otherwise, return False



-- 8. Return all shortest paths between two cities

-- | Finds all shortest paths between two cities in a roadmap.
-- Arguments:
--   rm: The roadmap to search.
--   c1: The starting city.
--   c2: The destination city.
-- Returns:
--   A list of shortest paths, where each path is a list of cities.
-- Time Complexity: O(n + m), where n is the number of cities and m is the number of roads, for the BFS search to explore the graph.

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


-- | Auxiliary function that finds all paths between two cities in a roadmap using breadth-first search.
-- Arguments:
--   rm: The roadmap to search.
--   start: The starting city.
--   dest: The destination city.
-- Returns:
--   A list of paths from the start city to the end city.
-- Time Complexity: O(n + m), where n is the number of cities and m is the number of roads, as it performs a BFS traversal through the graph.

bfsPaths :: RoadMap -> City -> City -> [Path]
bfsPaths rm start dest = bfs [[start]] [] where
    bfs [] _ = []                                                                                       -- if there are no more paths to explore, return an empty list
    bfs (currentPath : remainingPaths) visited
        | last currentPath == dest = currentPath : bfs remainingPaths visited                           -- if we reach the destination, add it to visited and continue exploring other paths
        | otherwise = bfs (remainingPaths ++ newPaths) (last currentPath : visited)                     -- otherwise, add the last city of the current path to visited and continue exploring from there
        where
            newPaths = [currentPath ++ [next] | (next, _) <- adjacent rm (last currentPath), next `notElem` visited] -- extend paths to each next city (adjacent cities)



-- 9. Solve the Traveling Salesman Problem using dynamic programming
-- | Finds the shortest path that visits all cities in the roadmap exactly once and returns to the start.
-- Arguments:
--   rm: The roadmap to find a TSP solution for.
-- Returns:
--   A path representing one of the optimal routes and an empty list if that path don't exist.
-- Time Complexity: O(2^n * n^2), due to dynamic programming approach for TSP.

travelSales :: RoadMap -> Path
travelSales rm
    | not (isStronglyConnected rm) = []                                                                 -- If the roadmap is not fully connected, return an empty path
    | otherwise = 
        let cityList = cities rm                                                                        -- List of all cities in the roadmap
            adjMatrix = convertToAdjMatrix rm                                                           -- Convert roadmap to adjacency matrix for easier distance lookup
            cityCount = length cityList                                                                 -- Total number of cities
            targetMask = createAllVisitedMask cityCount                                                 -- Mask with all cities marked as visited
            initialMask = Data.Bits.bit 0                                                               -- Start mask with the first city visited
            startCityIndex = 0                                                                          -- Index of the start city (head of cityList)
        in case findTSPPath adjMatrix initialMask startCityIndex startCityIndex targetMask [] of
               Just (_, resultPath) -> map (cityList !!) (resultPath ++ [startCityIndex])               -- Convert indices to city names and return path
               Nothing -> []   


-- | Converts a roadmap to an adjacency matrix representation.
-- Arguments:
--   rm: The roadmap to convert.
-- Returns:
--   An adjacency matrix with distances between each pair of cities.
-- Time Complexity: O(n^2), where n is the number of cities.

convertToAdjMatrix :: RoadMap -> AdjMatrix
convertToAdjMatrix rm =
    let cityList = cities rm
        cityCount = length cityList
        bounds = ((0, 0), (cityCount - 1, cityCount - 1))
        getDistance i j = distance rm (cityList !! i) (cityList !! j)
    in Data.Array.array bounds [((i, j), getDistance i j) | i <- [0..cityCount-1], j <- [0..cityCount-1]]


-- | Creates a mask where all cities are marked as visited.
-- Arguments:
--   n: The total number of cities.
-- Returns:
--   A bitmask representing that all cities are visited.
-- Time Complexity: O(1).

createAllVisitedMask :: Int -> Bit
createAllVisitedMask n = (Data.Bits.shiftL 1 n) - 1


-- | Checks if a city is marked as visited in the bitmask.
-- Arguments:
--   mask: The current bitmask.
--   cityIndex: The index of the city to check.
-- Returns:
--   True if the city is visited, False otherwise.
-- Time Complexity: O(1).

hasVisitedCity :: Bit -> Int -> Bool
hasVisitedCity mask cityIndex = (mask Data.Bits..&. Data.Bits.shiftL 1 cityIndex) /= 0


-- | Marks a city as visited in the bitmask.
-- Arguments:
--   mask: The current bitmask.
--   cityIndex: The index of the city to mark as visited.
-- Returns:
--   An updated bitmask with the city marked as visited.
-- Time Complexity: O(1).

markCityVisited :: Bit -> Int -> Bit
markCityVisited mask cityIndex = mask Data.Bits..|. Data.Bits.shiftL 1 cityIndex


-- | Auxiliary function to find the minimum-cost TSP path starting and ending at a given city.
-- Arguments:
--   adjMatrix: The adjacency matrix of the roadmap.
--   visitedMask: The current bitmask representing visited cities.
--   currentCity: The index of the current city.
--   startCity: The index of the city to start and end at.
--   targetMask: A bitmask representing all cities visited.
--   currentPath: The path traversed so far.
-- Returns:
--   A Maybe tuple containing the minimum distance and corresponding path, if valid.
-- Time Complexity: O(2^n * n^2), where n is the number of cities.

findTSPPath :: AdjMatrix -> Bit -> Int -> Int -> Bit -> [Int] -> Maybe (Distance, [Int])
findTSPPath adjMatrix visitedMask currentCity startCity targetMask currentPath
    | visitedMask == targetMask =                                                                       -- If all cities are visited
        case adjMatrix Data.Array.! (currentCity, startCity) of
            Just returnDist -> Just (returnDist, reverse (currentCity : currentPath))                   -- Return completed path if there's a direct route back
            Nothing -> Nothing                                                                          -- Return Nothing if no route back to start city exists
    | otherwise =
        let ((_, _), (maxIdx, _)) = Data.Array.bounds adjMatrix
            nextSteps = [ (dist + newDist, newPath)
                        | nextCity <- [0..maxIdx],                                                      -- Iterate over all possible next cities
                          not (hasVisitedCity visitedMask nextCity),                                    -- Check that the next city hasn't been visited
                          let dist = maybe maxBound id (adjMatrix Data.Array.! (currentCity, nextCity)), -- Get distance or set to max if no direct path
                          dist < maxBound,                                                              -- Ensure there's a valid direct path
                          Just (newDist, newPath) <- [findTSPPath adjMatrix (markCityVisited visitedMask nextCity) nextCity startCity targetMask (currentCity : currentPath)]
                        ]
        in if null nextSteps 
           then Nothing                                                                                 -- If no valid path exists, return Nothing
           else Just (minimum nextSteps)    



tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function




-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]

