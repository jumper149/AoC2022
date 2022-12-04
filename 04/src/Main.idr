module Main

import AoC
import Generics.Derive
%language ElabReflection

namespace Input
  record Range where
    constructor MkRange
    min : Nat
    max : Nat
    sorted : LTE rangeMin rangeMax

  implementation Show Range where
    show (MkRange {min, max, sorted}) = show min ++ "-" ++ show max

  record Assignment where
    constructor MkAssignment
    one : Range
    two : Range

  implementation Show Assignment where
    show (MkAssignment {one, two}) = "(" ++ show one ++ "," ++ show two ++ ")"

-- Input.
InputType : Type
InputType = List1 Assignment

-- Lexer.
data Token = MkTokenNewline
           | MkTokenComma
           | MkTokenMinus
           | MkTokenNumber Nat
%runElab (derive "Token" [ Eq, Generic, Meta, Ord, Show ])
tokenizer : Tokenizer Token
tokenizer = match newline (const MkTokenNewline)
        <|> match (is ',') (const MkTokenComma)
        <|> match (is '-') (const MkTokenMinus)
        <|> match digits (MkTokenNumber . cast)

-- Parser.
grammarNewline : Grammar () Token True ()
grammarNewline = terminal "Newline" $ \ x =>
                 case x of
                      MkTokenNewline => Just ()
                      _ => Nothing

grammarComma : Grammar () Token True ()
grammarComma = terminal "Comma" $ \ x =>
               case x of
                    MkTokenComma => Just ()
                    _ => Nothing

grammarMinus : Grammar () Token True ()
grammarMinus = terminal "Minus" $ \ x =>
               case x of
                    MkTokenMinus => Just ()
                    _ => Nothing

grammarNumber : Grammar () Token True Nat
grammarNumber = terminal "Number" $ \ x =>
                case x of
                     MkTokenNumber n => Just n
                     _ => Nothing

grammarRangeTuple : Grammar () Token True (Nat, Nat)
grammarRangeTuple = (,) <$> grammarNumber <*> (grammarMinus *> grammarNumber)

grammarRange : Grammar () Token True Range
grammarRange = grammarRangeTuple >>= f
  where f : (Nat, Nat) -> Grammar () Token False Range
        f (min, max) = case isLTE min max of
                            Yes sorted => Text.Parser.Core.pure $ MkRange {min, max, sorted}
                            No _ => Text.Parser.Core.fail "Range not sorted."

grammarAssignment : Grammar () Token True Assignment
grammarAssignment = do
  one <- grammarRange
  grammarComma
  two <- grammarRange
  pure $ MkAssignment {one, two}

grammar : Grammar () Token True InputType
grammar = someTill eof (grammarAssignment <* grammarNewline)

redundantPair : Assignment -> Bool
redundantPair (MkAssignment {one, two}) = oneContained || twoContained
  where oneContained : Bool
        oneContained = one.min <= two.min && one.max >= two.max
        twoContained : Bool
        twoContained = two.min <= one.min && two.max >= one.max

-- Part 1.
part1 : InputType -> IO ()
part1 input = printLn $ length $ filter redundantPair $ toList input

overlappingPair : Assignment -> Bool
overlappingPair (MkAssignment {one, two}) = oneMinOverlaps || oneMaxOverlaps || twoMinOverlaps || twoMaxOverlaps
  where oneMinOverlaps : Bool
        oneMinOverlaps = one.min >= two.min && one.min <= two.max
        oneMaxOverlaps : Bool
        oneMaxOverlaps = one.max <= two.max && one.max >= two.min
        twoMinOverlaps : Bool
        twoMinOverlaps = two.min >= one.min && two.min <= one.max
        twoMaxOverlaps : Bool
        twoMaxOverlaps = two.max <= one.max && two.max >= one.min

-- Part 2.
part2 : InputType -> IO ()
part2 input = printLn $ length $ filter overlappingPair $ toList input

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
