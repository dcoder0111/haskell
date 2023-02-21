

runGame :: Int -> IO ()
runGame incorrectGuess = do
    let secretNumber = "5"

    if incorrectGuess == 3
    then putStrLn "Sorry, you lost the game"
    else do
        putStrLn "Enter secret number (guess between 1 to 10): " 
        userGuess <- getLine
        if userGuess == secretNumber
        then putStrLn "You win!"
        else runGame (incorrectGuess + 1)

main :: IO ()
main = do
    runGame 0