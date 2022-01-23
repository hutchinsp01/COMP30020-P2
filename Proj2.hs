--------------------------------------------------------------------------------
--              Written by Paul Hutchins for Project 2 COMP30020              --
--                            Student ID - 1160468                            --
--------------------------------------------------------------------------------
--                                  Summary                                   --
-- This program works in conjunction with Main.hs and works to solve a        --
-- battleship style puzzle based on "FeedBack" recieved from Main.hs. This    --
-- program gives an initial HardCoded guess, which was derived from basic     --
-- basic testing, and also returns a nextGuess based on the "Feedback"        --
-- recieved. This is acomplished through the culling down of a list of all    --
-- possible guesses of ship locations by comparing possible guesses and       --
-- seeing which ones are consistent with the prev guess and feedback.         --
-- We then choose the guess that will provide us with the smallest amount of  --
-- possible candidates after we have guessed that guess                       --
--                                                                            --
--------------------------------------------------------------------------------

module Proj2 (Location, toLocation, fromLocation, feedback,
              GameState, initialGuess, nextGuess) where
              
import Data.List

-- Location implemented as (Int, Int) to remove need for int in code
-- and for ease of math calculations in the future
type Location = (Int, Int)

type GameState = [[Location]]
type Feedback = (Int, Int, Int)



--------------------------------------------------------------------------------
--                  ALL FUNCTIONS ASSOCIATED WITH toLocation                  --

-- toLocation :: String -> Maybe Location
-- If length of location not 2 or location not in range returns nothing
-- else returns String location converted to (Int, Int) location
toLocation :: String -> Maybe Location
toLocation loc = if length loc /= 2
                     then Nothing
                     else if locx == -1 || locy == -1
                          then Nothing
                          else Just (locx, locy)
    where
        (locx, locy) = toLoc loc


-- toLoc :: String -> Location
-- takes a valid length string, and returns (Int, Int) after conversion
toLoc :: String -> Location
toLoc (x:y:[]) = (toX x, toY y)


-- toX :: Char -> Int
-- takes any Char - if in range [A..H] returns Int [1..8] else returns -1
toX :: Char -> Int
toX char 
    | num >=1 && num <=8 = num
    | otherwise          = -1
    where
        num = fromEnum char - 64


-- toy :: Char -> Int
-- takes any Char - if in range [1..4] returns Int [1..4] else returns -1
toY :: Char -> Int
toY char 
    | num >=1 && num <=4 = num
    | otherwise          = -1
    where
        num = fromEnum char - 48
    


--------------------------------------------------------------------------------
--                 ALL FUNCTIONS ASSOCIATED WITH fromLocation                 --

-- fromLocation :: Location -> String
-- Converts (Int, Int) back to String location form [A1..H4]
fromLocation :: Location -> String
fromLocation (locx, locy) = [fromX locx] ++ [fromY locy]


-- fromX :: Int -> Char
-- Takes Int [1..8] returns char [A..H]
fromX :: Int -> Char
fromX num = toEnum (num + 64)


-- fromy :: Int -> Char
-- Takes Int [1..4] returns char [1..4]
fromY :: Int -> Char
fromY num = toEnum (num + 48)



--------------------------------------------------------------------------------
--                   ALL FUNCTIONS ASSOCIATED WITH feedback                   --

-- feedback :: [Location] -> [Location] -> (Int, Int, Int)
-- Takes targets and a guess and returns feedback based on how close each guess
-- is to a target. Doesnt differentiate between targets, all 3 guesses can
-- give feedback about the same target if guesses are close to each other
-- (# guesses that = target, # guesses 1 square away, # guesses 2 squares away)
feedback :: [Location] -> [Location] -> Feedback
feedback targets (g1 : g2 : g3 : empty) = (a0+b0+c0, a1+b1+c1, a2+b2+c2)
    where 
        (a0, a1, a2) = computeFeedback targets g1
        (b0, b1, b2) = computeFeedback targets g2
        (c0, c1, c2) = computeFeedback targets g3
 
 
-- computeFeedback :: [Location] -> Location -> (Int, Int, Int)
-- Takes list of target locations and a guess and returns the feedback the guess
computeFeedback :: [Location] -> Location -> (Int, Int, Int)
computeFeedback targets guess
    | dist == 0  = (1,0,0)
    | dist == 1  = (0,1,0)
    | dist == 2  = (0,0,1)
    | otherwise = (0,0,0)
    where
        dist = foldr1 min (map (findDistance guess) targets)

-- findDistance :: Location -> Location -> Int
-- Takes two locations and returns the distance between them
findDistance :: Location -> Location -> Int
findDistance (guesx, guesy) (targx, targy) = distance
    where 
        distance = max (abs (guesx  - targx)) (abs (guesy - targy))



--------------------------------------------------------------------------------
--                 ALL FUNCTIONS ASSOCIATED WITH initialGuess                 --

-- initialGuess :: ([Location],GameState)
-- Returns my hardcoded initialGuess and initialised the gameState with all
-- combinations of guesses minus hardcoded guess
initialGuess :: ([Location] ,GameState)
initialGuess = (guess, gameState)
    where 
        -- Hardcoded guess
        
        -- Havn't fully tested for best initial guess, but this seems to be good
        -- Just tried grouping vs spreading, and keeping 2 together seems to
        -- offer decent results
        -- Tried to create a test kit, but couldnt get my code to work
        guess = [(1,3), (2,3), (8,1)]
        
        fillGameState = [[(a,b),(c,d),(e,f)] |
                            a <- [1..8], b <- [1..4], c <- [1..8],
                            d <- [1..4], e <- [1..8], f <- [1..4]]
                                           
        gameState = remove guess (removeInvalid fillGameState)
        
-- removeInvalid :: GameState -> GameState        
-- Ensures all locations in guess are in ascending order
-- This ensures we will not have duplicate / permutations of the same guess
removeInvalid :: GameState -> GameState
removeInvalid [] = []
removeInvalid ([(a,b),(c,d),(e,f)]:xs)
    | a > c || (a == c && b >= d) = removeInvalid xs
    | c > e || (c == e && d >= f) = removeInvalid xs
    | otherwise                   = ([(a,b),(c,d),(e,f)]):removeInvalid xs



--------------------------------------------------------------------------------
--                  ALL FUNCTIONS ASSOCIATED WITH nextGuess                   --

-- nextGuess :: ([Location],GameState) -> Feedback -> ([Location],GameState)
-- Initially culls the current gameState for guesses that are inconsistent
-- with the feedback we recieved
-- Then we calculate best guess and select that as out nextGuess
-- Then we remove next guess from the gamestate
-- Finally return nextGuess and gameState
nextGuess :: ([Location],GameState) -> Feedback -> ([Location], GameState)
nextGuess (prevGuess, gameState) feedBack = (nextGuess, nextGameState)
    where
        nextGameState1 = [guess | guess <- gameState,
                                    feedback guess prevGuess == feedBack]
        nextGuess = bestGuess nextGameState1 nextGameState1 prevGuess
        nextGameState = remove nextGuess nextGameState1

-- bestGuess :: GameState -> GameState -> [Location] -> [Location]
-- If gameState is empty prevGuess must be the bestGuess
-- Else we obtain the guess that gives us the lowest expected future feedback
bestGuess :: GameState -> GameState -> [Location] -> [Location]
bestGuess [] fullState prevGuess         = prevGuess
bestGuess gameState fullState prevGuess  = bestguess
    where
        -- prevCount initialises our current lowest value
        prevCount = avgCandidates prevGuess fullState
        bestguess = getLowest prevCount prevGuess gameState fullState

-- getLowest :: Double -> [Location] -> GameState -> GameState -> [Location]
-- if gameState is empty - we have tried all guesses, return the lowest
-- else for current guess in gamestate we get expected candidates and compare
-- against our lowest expected candidates
getLowest :: Double -> [Location] -> GameState -> GameState -> [Location]
getLowest _ lowGuess [] _ = lowGuess
getLowest lowCount lowGuess (guess:rest) fullState
    | curCount < lowCount = getLowest lowCount lowGuess rest fullState
    | otherwise           = getLowest curCount guess rest fullState
    where
        curCount = avgCandidates guess fullState

-- avgCandidates :: [Location] -> GameState -> Double
-- returns the avg number of candidates if we select the current guess
avgCandidates :: [Location] -> GameState -> Double
avgCandidates guess gameState = avgCandidates
    where
        -- For every guess in feedback, we obtain all the feedback we would 
        -- recieve if we selected that as our next guess
        -- Then sort and group by feedback
        -- We then map the length of each grouped feedback so we can use
        -- our hint formula to calculate the avg candidates for a guess
        feedbackNum = map length (group (sort (map (feedback guess) gameState)))
        avgCandidates = sum (map (hintFormula (sum feedbackNum)) feedbackNum)
 
-- hintFormula :: Int -> Int -> Double
-- Formula provided to us in hints, used to calc expected # candidates of guess
hintFormula :: Int -> Int -> Double
hintFormula f t = (f2 * f2) / t1
    where
        f2 = fromIntegral (f * f)
        t1 = fromIntegral t
    


--------------------------------------------------------------------------------
--                              OTHER FUNCTIONS                               --

-- remove :: [Location] -> GameState -> GameState
-- Removes a guess from the GameState
remove :: [Location] -> GameState -> GameState
remove _ [] = []
remove guess (curGuess:gameState)
    | guess == curGuess = remove guess gameState
    | otherwise         = curGuess:remove guess gameState
    

--                                End of File                                 --
--------------------------------------------------------------------------------