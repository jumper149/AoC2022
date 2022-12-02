module Main

import AoC
import Generics.Derive
%language ElabReflection

data Hand = Rock
          | Paper
          | Scissors
%runElab (derive "Hand" [ Eq, Generic, Meta, Ord, Show ])

record Round where
  constructor MkRound
  enemy : Hand
  you : Hand
%runElab (derive "Round" [ Eq, Generic, Meta, Ord, Show ])

Game = List1 Round

-- Input.
InputType : Type
InputType = List1 Game

-- Lexer.
data Token = MkTokenNewline
           | MkTokenSpace
           | MkTokenA
           | MkTokenB
           | MkTokenC
           | MkTokenX
           | MkTokenY
           | MkTokenZ
%runElab (derive "Token" [ Eq, Generic, Meta, Ord, Show ])
tokenizer : Tokenizer Token
tokenizer = match newline (const MkTokenNewline)
        <|> match (is ' ') (const MkTokenSpace)
        <|> match (is 'A') (const MkTokenA)
        <|> match (is 'B') (const MkTokenB)
        <|> match (is 'C') (const MkTokenC)
        <|> match (is 'X') (const MkTokenX)
        <|> match (is 'Y') (const MkTokenY)
        <|> match (is 'Z') (const MkTokenZ)

-- Parser.
grammar : Grammar () Token True InputType
grammar = do
  let grammarNewline = terminal "Newline" $ \ x =>
                       case x of
                            MkTokenNewline => Just ()
                            _ => Nothing
  let grammarSpace = terminal "Space" $ \ x =>
                       case x of
                            MkTokenSpace => Just ()
                            _ => Nothing
  let grammarEnemyHand = terminal "Enemy Hand" $ \ x =>
                         case x of
                              MkTokenA => Just Rock
                              MkTokenB => Just Paper
                              MkTokenC => Just Scissors
                              _ => Nothing
  let grammarYourHand = terminal "Your Hand" $ \ x =>
                        case x of
                             MkTokenX => Just Rock
                             MkTokenY => Just Paper
                             MkTokenZ => Just Scissors
                             _ => Nothing
  let grammarRound = MkRound <$> (grammarEnemyHand <* grammarSpace) <*> grammarYourHand <* grammarNewline
  let grammarGame = someTill grammarNewline grammarRound
  someTill eof grammarGame

scoreRound : Round -> Nat
scoreRound x = scoreResult + scoreYourShape
  where scoreYourShape : Nat
        scoreYourShape =
          case x.you of
               Rock => 1
               Paper => 2
               Scissors => 3
        data Result = YouWin
                    | Draw
                    | YouLose
        result : Result
        result =
          case (x.you, x.enemy) of
               (Rock,Rock) => Draw
               (Rock,Paper) => YouLose
               (Rock,Scissors) => YouWin
               (Paper,Rock) => YouWin
               (Paper,Paper) => Draw
               (Paper,Scissors) => YouLose
               (Scissors,Rock) => YouLose
               (Scissors,Paper) => YouWin
               (Scissors,Scissors) => Draw
        scoreResult : Nat
        scoreResult =
          case result of
               YouLose => 0
               Draw => 3
               YouWin => 6

-- Part 1.
part1 : InputType -> IO ()
part1 input = do
  let inputLists = toList <$> toList input
  printLn $ sum $ (sum . (scoreRound <$>)) <$> inputLists

data RoundEnd = YouWin
              | Draw
              | YouLose

roundEnd : Round -> RoundEnd
roundEnd x = case x.you of
                  Rock => YouLose
                  Paper => Draw
                  Scissors => YouWin

translateRound : Round -> Round
translateRound x = MkRound x.enemy yourHand
  where yourHand : Hand
        yourHand = case (x.enemy, roundEnd x) of
                        (Rock,YouLose) => Scissors
                        (Rock,Draw) => Rock
                        (Rock,YouWin) => Paper
                        (Paper,YouLose) => Rock
                        (Paper,Draw) => Paper
                        (Paper,YouWin) => Scissors
                        (Scissors,YouLose) => Paper
                        (Scissors,Draw) => Scissors
                        (Scissors,YouWin) => Rock

-- Part 2.
part2 : InputType -> IO ()
part2 input = do
  let inputLists = toList <$> toList input
  let newInputLists = (translateRound <$>) <$> inputLists
  printLn $ sum $ (sum . (scoreRound <$>)) <$> newInputLists

main : IO ()
main = solveAoC solution
  where solution : AoCSolution Token InputType
        solution = MkAoCSolution
          { inputFilePath = "./input"
          , tokenizer = tokenizer
          , grammar = grammar
          , part1 = part1
          , part2 = part2
          }
