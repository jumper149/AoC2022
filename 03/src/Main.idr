module Main

import AoC
import Data.Nat.Views
import Data.Vect
import Generics.Derive
%language ElabReflection

-- Input.
InputType : Type
InputType = List1 (List1 Char)

-- Lexer.
data Token = MkTokenNewline
           | MkTokenChar String
%runElab (derive "Token" [ Eq, Generic, Meta, Ord, Show ])
tokenizer : Tokenizer Token
tokenizer = match newline (const MkTokenNewline)
        <|> match alpha MkTokenChar

-- Parser.
grammar : Grammar () Token True InputType
grammar = do
  let grammarNewline = terminal "Newline" $ \ x =>
                       case x of
                            MkTokenNewline => Just ()
                            _ => Nothing
  let grammarChar = terminal "Char" $ \ x =>
                    case x of
                         MkTokenChar x => fst <$> strUncons x
                         _ => Nothing
  let grammarLine = someTill grammarNewline grammarChar
  someTill eof grammarLine

Alpha = Fin 52

charToAlpha : Char -> Maybe Alpha
charToAlpha x = case n of
                     Z => Nothing
                     S m => natToFin m 52
  where n : Nat
        n = if isLower x
               then S $ cast x `minus` cast 'a'
               else if isUpper x
                    then (26 `plus`) $ S $ cast x `minus` cast 'A'
                    else 0

listToVect : List a -> (n ** Vect n a)
listToVect Nil = (0 ** Nil)
listToVect (x :: xs) = let (m ** ys) = listToVect xs in (S m ** x :: ys)

record Rucksack where
  constructor MkRucksack
  firstCompartment : Vect n Alpha
  secondCompartment : Vect n Alpha

lineToRucksack : List Alpha -> Maybe Rucksack
lineToRucksack list =
  case listToVect list of
       (m ** vect) => case half m of
                      HalfOdd _ => Nothing
                      HalfEven n => let (firstCompartment, secondCompartment) = splitAt n vect
                                     in Just $ MkRucksack firstCompartment secondCompartment

duplicateItem : Rucksack -> Maybe Alpha
duplicateItem (MkRucksack firstCompartment secondCompartment) =
  case filter (`elem` x) y of
       a :: _ => Just a
       _ => Nothing
  where
    x : List Alpha
    x = toList firstCompartment
    y : List Alpha
    y = toList secondCompartment

priority : Alpha -> Integer
priority a = 1 + finToInteger a

-- Part 1.
part1 : InputType -> IO ()
part1 input = do
  let inputLists : List (List Char)
      inputLists = toList <$> toList input
  printLn inputLists
  let inputAlphas : Maybe (List (List Alpha))
      inputAlphas = traverse (traverse charToAlpha) inputLists
  printLn inputAlphas
  let inputRucksacks : Maybe (List Rucksack)
      inputRucksacks = traverse lineToRucksack =<< inputAlphas
  let inputDuplicates : Maybe (List Alpha)
      inputDuplicates = traverse duplicateItem =<< inputRucksacks
  printLn inputDuplicates
  duplicates <- case inputDuplicates of
                     Nothing => ?aslkjdfdlskdhf
                     Just x => pure x
  printLn $ sum $ priority <$> duplicates

alphasToGroups : List (List Alpha) -> List (List Alpha, List Alpha, List Alpha)
alphasToGroups list =
  case list of
       Nil => Nil
       (a :: (b :: (c :: xs))) => (a, b, c) :: alphasToGroups xs
       _ => ?wantedGroupsOf3

groupDuplicates : (List Alpha, List Alpha, List Alpha) -> List Alpha
groupDuplicates (a, b, c) = filter (`elem` (filter (`elem` a) b)) c

groupDuplicate : (List Alpha, List Alpha, List Alpha) -> Alpha
groupDuplicate x = case groupDuplicates x of
                        d :: _ => d
                        _ => ?aslkjhlashdl

-- Part 2.
part2 : InputType -> IO ()
part2 input = do
  let inputLists : List (List Char)
      inputLists = toList <$> toList input
  printLn inputLists
  let inputAlphas : Maybe (List (List Alpha))
      inputAlphas = traverse (traverse charToAlpha) inputLists
  printLn inputAlphas
  alphas <- case inputAlphas of
                 Nothing => ?aslkjdfdlskdhfa
                 Just x => pure x
  let groups = alphasToGroups alphas
  printLn groups
  let duplicates = groupDuplicate <$> groups
  printLn duplicates
  let prios = priority <$> duplicates
  printLn $ sum prios

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
