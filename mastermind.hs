import Data.Char (toLower)
import System.Random

data Peg
  = White
  | Blue
  | Green
  | Red
  | Yellow
  | Orange
  deriving (Show, Eq, Enum, Bounded)

instance Random Peg where
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')

  random = randomR (minBound, maxBound)


data MasterCode
  = MasterCode Peg Peg Peg Peg
  deriving (Eq, Show)


data FeedbackPeg
  = CorrectPosition
  | CorrectColor
  deriving (Eq)


data Feedback
  = Miss [FeedbackPeg]
  | Hit

data Action
  = TryAgain [FeedbackPeg] Int
  | Fail
  | Win
  | Invalid


check :: MasterCode -> MasterCode -> Feedback
check masterCode guess
  | guess == masterCode = Hit
  | otherwise = Miss (calculateFeedback guess masterCode)


calculateFeedback :: MasterCode -> MasterCode -> [FeedbackPeg]
calculateFeedback (MasterCode g1 g2 g3 g4) (MasterCode m1 m2 m3 m4) =
  let
    guessList =
      [g1, g2, g3, g4]

    codeList =
      [m1, m2, m3, m4]

    rightPosition =
      [ CorrectPosition | x <- zipWith (==) guessList codeList, x ]

    rightColor =
      [ CorrectColor | (x, y) <- zip guessList codeList, x `elem` codeList,  x /= y]
  in
    rightPosition ++ rightColor


parseCode :: String -> Maybe MasterCode
parseCode input =
  let
    pegs =
      words input
  in
    case pegs of
      [_, _, _, _] ->
        parseWords pegs
      _ ->
        Nothing


parseWords :: [String] -> Maybe MasterCode
parseWords ps =
  let
    pegs =
      map (parseWord . map toLower) ps
  in
    case pegs of
      [Just w1, Just w2, Just w3, Just w4] ->
        Just $ MasterCode w1 w2 w3 w4

      _ ->
        Nothing


parseWord :: String -> Maybe Peg
parseWord word
  | word == "white" || word == "w" = Just White
  | word == "blue" || word == "b" = Just Blue
  | word == "green" || word == "g" = Just Green
  | word == "red" || word == "r" = Just Red
  | word == "yellow" || word == "y" = Just Yellow
  | word == "orange" || word == "o" = Just Orange
  | otherwise = Nothing


printFeedback :: [FeedbackPeg] -> IO ()
printFeedback feedback =
  let
    veryGood =
      length $ filter (CorrectPosition ==) feedback
    almostThere =
      length $ filter (CorrectColor ==) feedback
  in
    putStrLn $ show veryGood ++ " perfect hit(s), " ++ show almostThere ++ " with the right color."


masterMind :: MasterCode -> Int -> IO ()
masterMind masterCode tries =
  do
    putStrLn ""
    putChar '>'
    input <- getLine
    case turn masterCode tries (parseCode input) of
      Win ->
        putStrLn "You Win!"

      Fail ->
        putStrLn "You lose, and will never see the secret code!"

      Invalid ->
        do
          putStrLn "Invalid code sequence, please use the right colors and separate with space"
          masterMind masterCode tries

      TryAgain feedback triesLeft ->
        do
          printFeedback feedback
          masterMind masterCode triesLeft


turn :: MasterCode -> Int -> Maybe MasterCode -> Action
turn masterCode tries guess =
  case guess of
    Nothing ->
      Invalid

    Just code ->
      case (check masterCode code, tries) of
        (Hit, _) ->
          Win

        (_, 1) ->
          Fail

        (Miss f, _) ->
          TryAgain f (tries - 1)

main :: IO ()
main =
  do
    putStrLn "Master Code generated, try guessing by entering your color sequence separated by spaces:"
    putStrLn ""
    putStrLn "- white or w"
    putStrLn "- blue or b"
    putStrLn "- green or g"
    putStrLn "- red or r"
    putStrLn "- yellow or y"
    putStrLn "- orange or o"
    putStrLn ""
    p1 <- randomRIO (White, Orange :: Peg)
    p2 <- randomRIO (White, Orange :: Peg)
    p3 <- randomRIO (White, Orange :: Peg)
    p4 <- randomRIO (White, Orange :: Peg)
    masterMind (MasterCode p1 p2 p3 p4) 10
