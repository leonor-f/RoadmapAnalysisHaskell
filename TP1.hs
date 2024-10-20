import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

cities :: RoadMap -> [City]
cities [] = []
cities ((c1, c2, _):resto) = addCity c1 (addCity c2 (cities resto)) -- if the road map is not empty, add the first city of the first tuple to the list of cities, then add the second city
                                                                    -- of the first tuple to the list of cities, then call the function recursively with the rest of the road map
    where
        addCity city citiesList = if city `elem` citiesList then citiesList else city : citiesList -- helper function that adds a city to a list of cities if it is not already there

areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent rm c1 c2 = any (\(x, y, _) -> (x == c1 && y == c2) || (x == c2 && y == c1)) rm -- if both cities are in the list of cities, check if there exists a tuple (x, y, _) in the
                                                                                           -- road map such that (x == c1 && y == c2) or (x == c2 && y == c1), if it is, return True
                                                                                           -- otherwise, return False

distance :: RoadMap -> City -> City -> Maybe Distance
distance rm c1 c2
    | c1 == c2 = Just 0         -- if the two cities are the same, distance is 0
    | otherwise = case filter (\(x, y, _) -> (x == c1 && y == c2) || (x == c2 && y == c1)) rm of
        [] -> Nothing           -- if c1 and c2 aren't connected directly, return Nothing
        (_, _, d):_ -> Just d   -- if c1 and c2 are a tuple in the road map (connected directly), return the distance

adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent rm c = [(c2, d) | (c1, c2, d) <- rm, c1 == c] ++ [(c1, d) | (c1, c2, d) <- rm, c2 == c] -- for each tuple (c1, c2, d) in the road map:
                                                                                                 -- if c1 = c, add (c2, d) to the list
                                                                                                 -- if c2 = c, add (c1, d) to the list

pathDistance :: RoadMap -> Path -> Maybe Distance
-- auxiliary function that creates pairs with the elements of a list 2 by 2
getPair :: [a] -> [(a, a)]
getPair (x:y:xs) = (x, y) : getPair xs  -- create pairs
getPair _ = []                          -- if the list is empty or has only 1 element, return an empty list

pathDistance rm path = if all (\(c1, c2) -> areAdjacent rm c1 c2) (getPair path)                     -- if all the consecutive pairs of cities are directly connected by roads
                       then Just (sum [d | (c1, c2) <- getPair path, Just d <- [distance rm c1 c2]]) -- return the sum of all individual distances in the path, using the distance function
                       else Nothing

rome :: RoadMap -> [City]
rome = undefined

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected = undefined

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined

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
