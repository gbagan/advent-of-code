-- https://adventofcode.com/2020/day/19
module Day19 (solve) where
import           AOC.Prelude hiding (sequence)
import           AOC (aoc)
import           AOC.Parser (Parser, sepBy1, sepEndBy1, eol, decimal, hspace, letterChar, some)
import qualified Text.ParserCombinators.ReadP as P
import Text.ParserCombinators.ReadP (ReadP, readP_to_S)
import qualified Data.Vector as V
import           AOC.List (count)

data Rule = Terminal !Char | NonTerminal [[Int]] deriving (Show)  
type Grammar = Vector Rule
data Input = Input !Grammar [String]

parser :: Parser Input
parser = Input <$> grammar <* eol <*> messages where
    grammar = V.fromList . map snd . sortOn fst <$> rule `sepEndBy1` eol
    rule = (,) <$> (decimal :: Parser Int) <* ":" <* hspace <*> (terminal <|> nonterminal)    
    terminal = Terminal <$> ("\"" *> letterChar) <* "\""
    nonterminal = NonTerminal <$> (decimal `sepEndBy1` hspace) `sepBy1` "| "
    messages = some letterChar `sepEndBy1` eol

parseGrammar :: Grammar -> ReadP ()
parseGrammar grammar = rule (grammar V.! 0) where
    rule (Terminal c) = P.char c $> ()
    rule (NonTerminal disj) = P.choice (map sequence disj)
    sequence = traverse_ (rule . (grammar V.!))

solveFor :: (Grammar -> Grammar) -> Input -> Int
solveFor f (Input grammar messages) = count (not . null) results where
    results = map (readP_to_S (parseGrammar grammar' >> P.eof)) messages
    grammar' = f grammar

modifyRules :: Grammar -> Grammar
modifyRules = (V.// [ (8, NonTerminal [[42], [42, 8]])
                    , (11, NonTerminal [[42, 31], [42, 11, 31]])
                    ]
              )

solve :: Text -> IO ()
solve = aoc parser (solveFor id) (solveFor modifyRules)