import System.Random

check :: String -> String -> Char -> (Bool, String)
check word guessed guessChar =
  let
    correctGuess = guessChar `elem` word
    newDisplay = [ if x == guessChar then guessChar else current | ( x, current ) <- zip word guessed ]
  in
    (correctGuess, newDisplay)


turn :: String -> String -> Int -> IO ()
turn word guessed lives
  | lives == 0 = putStrLn $ "You lose!, it was: " ++ word
  | guessed == word = putStrLn $ "You Win! It was: " ++ word
  | otherwise = makeGuess word guessed lives


makeGuess :: String -> String -> Int -> IO ()
makeGuess word guessed lives =
  do
    putStrLn (guessed ++ "  " ++ replicate lives '*')
    putStrLn ""
    putStrLn " Enter your guess, or die: "
    q <- getLine
    checkAndPlay q word guessed lives


checkAndPlay :: String -> String -> String -> Int -> IO ()
checkAndPlay char word guessed lives =
  case char of
    (c:_) ->
      let
        (correct, display) = check word guessed c
        newLives = if correct then lives else lives - 1
      in
        turn word display newLives
    _ ->
        turn word guessed lives

starman :: String -> Int -> IO ()
starman word =
  turn word (replicate (length word) '-')


play :: IO ()
play =
  do
    content <- readFile "words.txt"
    let words = lines content
    pick <- randomRIO (0, length words :: Int)
    starman (words !! pick) 5
