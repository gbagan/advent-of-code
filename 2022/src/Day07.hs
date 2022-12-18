-- https://adventofcode.com/2022/day/7
module Day07 (solve) where
import           RIO hiding (some)
import           RIO.List.Partial (minimum)
import           Text.Megaparsec (some)
import           Text.Megaparsec.Char (alphaNumChar, char, eol, string)
import           Text.Megaparsec.Char.Lexer (decimal)
import qualified Data.Tree as T
import qualified Data.Tree.Zipper as Z
import           Util (Parser, aoc')

data FileType = RegularFile | Directory
data File = File !FileType !String !Integer
data Cmd = Cd String | Ls [File]
type FileSystem = T.Tree File
type ZFS = Z.TreePos Z.Full File

parser :: Parser [Cmd]
parser = some (cd <|> ls) where
    cd = Cd <$> (string "$ cd " *> filename <* eol)
    ls = Ls <$> (string "$ ls" *> eol *> some file)
    filename = some (alphaNumChar <|> char '.' <|> char '/')
    file = (rfile <|> dir) <* eol
    rfile = flip (File RegularFile) <$> (decimal <* char ' ') <*> filename
    dir = flip (File Directory) 0 <$> (string "dir " *> filename)

changeDir :: String -> ZFS -> Maybe ZFS
changeDir name = go <=< Z.firstChild where
    go zipper | fileName (Z.label zipper) == name = Just zipper
              | otherwise = go =<< Z.nextTree (Z.nextSpace zipper)
    fileName (File _ n _) = n

createFS :: [Cmd] -> Maybe FileSystem
createFS cmds = Z.toTree <$> zipper where
    zipper = foldM (flip exec) emptyFS cmds
    emptyFS = Z.fromTree $ T.Node (File Directory "/" 0) []
    exec (Cd "/") = pure . Z.root
    exec (Cd "..") = Z.parent
    exec (Cd dir) = changeDir dir
    exec (Ls files) = pure . Z.modifyTree (\t -> t{T.subForest = [T.Node file [] | file <- files]})

computeDirSizes :: FileSystem -> FileSystem
computeDirSizes (T.Node (File typ name s) children) = T.Node
                                            (File typ name (s + sum [s' | (T.Node (File _ _ s') _) <- children']))
                                            children' where
    children' = map computeDirSizes children

precomp :: [Cmd] -> Maybe FileSystem
precomp = fmap computeDirSizes . createFS

part1 :: FileSystem -> Integer
part1 fs = sum [s | (File Directory _ s) <- T.flatten fs, s<= 100000]

part2 :: FileSystem -> Integer
part2 fs@(T.Node (File _ _ totalSize) _) = minimum [s | (File Directory _ s) <- T.flatten fs
                                                      , totalSize - s <= 40000000
                                                   ]

solve :: MonadIO m => Text -> m ()
solve = aoc' parser precomp part1 part2
