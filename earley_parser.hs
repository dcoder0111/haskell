import Data.List (nub)

type Rule = (String, [String])
type Item = (String, Int, Int)

earleyParser :: String -> [Rule] -> [String]
earleyParser input rules = 
    let chart = earleyChart input rules
        lastState = chart !! length input
        results = nub $ [lhs | (lhs, start, end) <- lastState, lhs == "S"] 
    in results

earleyChart :: String -> [Rule] -> [[Item]]
earleyChart input rules = foldl (flip ($)) (initialState rules) (zip input [0..length input])
    where
        initialState :: [Rule] -> [[Item]]
        initialState rules = [[("S'", 0, 0)]]
        
        predict :: [Rule] -> [Item] -> Int -> [Item]
        predict rules items j = 
            [ (lhs, 0, j) | (lhs, rhs) <- rules, rhs == [head] ]
            where
                head = snd $ items !! j
        
        scan :: String -> [Item] -> Int -> Char -> [Item]
        scan input items j c =
            if j < length items && input !! j == c then [(lhs, start, end + 1)] else []
            where
                (lhs, start, end) = items !! j
        
        complete :: [Item] -> Int -> [Item]
        complete items j = 
            [(lhs, start, i) | (lhs, start, k) <- items, k == j, (rhs, i, j) <- items, rhs == lhs]
        
        processState :: [Rule] -> [[Item]] -> Int -> [[Item]]
        processState rules chart j = 
            let predicted = concat [ predict rules items j | items <- chart ]
                scanned = concat [ scan input items j c | items <- chart, c <- input ]
                completed = concat [ complete items j | items <- chart ]
            in chart ++ [nub $ predicted ++ scanned ++ completed]
        
        earleyStep :: [Rule] -> [[Item]] -> (Char, Int) -> [[Item]]
        earleyStep rules chart (c, j) = processState rules chart j
        
        earleyChart' :: [Rule] -> [[Item]] -> String -> [(Char, Int)] -> [[Item]]
        earleyChart' rules chart input steps = foldl (flip ($)) chart (map (earleyStep rules) steps)
        
        earleySteps :: String -> [(Char, Int)]
        earleySteps input = zip input [0..length input]
        
        earleyChart :: String -> [Rule] -> [[Item]]
        earleyChart input rules = earleyChart' rules (initialState rules) input (earleySteps input)
