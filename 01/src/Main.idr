module Main

import AoC
import Generics.Derive
%language ElabReflection

-- Input.
InputType : Type
InputType = List1 (List Nat)

-- Lexer.
data Token = MkTokenNewline
           | MkTokenNumber Nat
%runElab (derive "Token" [ Eq, Generic, Meta, Ord, Show ])
tokenizer : Tokenizer Token
tokenizer = match newline (const MkTokenNewline)
        <|> match digits (MkTokenNumber . cast)

-- Parser.
grammar : Grammar () Token True InputType
grammar = do
  let grammarNewline = terminal "Newline" $ \ x =>
                       case x of
                            MkTokenNewline => Just ()
                            _ => Nothing
  let grammarNumber = terminal "Number" $ \ x =>
                      case x of
                           MkTokenNumber n => Just n
                           _ => Nothing
  let grammarItem = grammarNumber <* grammarNewline
  let grammarInventory = manyTill grammarNewline grammarItem
  someTill eof grammarInventory

-- Part 1.
part1 : InputType -> IO ()
part1 input = do
  let result = foldl max Z $ sum <$> toList input
  print result

-- Part 2.
part2 : InputType -> IO ()
part2 input = do
  let result = sum $ take 3 $ reverse $ sort $ sum <$> toList input
  print result

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
