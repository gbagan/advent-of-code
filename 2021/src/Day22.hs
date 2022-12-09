module Day22 (solve) where
import           RIO
import           Text.Megaparsec (sepEndBy1)
import           Text.Megaparsec.Char (eol, string)
import           Util (Parser, aoc, signedInteger)

data Cube = Cube !Bool !(Int, Int) !(Int, Int) !(Int, Int)

parser :: Parser [Cube]
parser = cube `sepEndBy1` eol where
    cube = Cube <$> onoff <* string " x=" <*> pair <* string ",y=" <*> pair <* string ",z=" <*> pair 
    pair = (,) <$> signedInteger <* string ".." <*> signedInteger
    onoff = True <$ string "on" <|> False <$ string "off"

disjoint :: Cube -> Cube -> Bool
disjoint (Cube _ (xmin, xmax) (ymin, ymax) (zmin, zmax))
         (Cube _ (xmin', xmax') (ymin', ymax') (zmin', zmax')) =
            xmax < xmin' || ymax < ymin' || zmax < zmin' || xmax' < xmin || ymax' < ymin || zmax' < zmin

intersect :: Bool -> Cube -> Cube -> Maybe Cube
intersect onoff
          c1@(Cube _ (xmin, xmax) (ymin, ymax) (zmin, zmax))
          c2@(Cube _ (xmin', xmax') (ymin', ymax') (zmin', zmax'))
            | disjoint c1 c2 = Nothing 
            | otherwise      = Just $ Cube onoff (max xmin xmin', min xmax xmax')
                                                 (max ymin ymin', min ymax ymax')
                                                 (max zmin zmin', min zmax zmax')

addCube :: [Cube] -> Cube -> [Cube]
addCube cubes c@(Cube onoff _ _ _) = cubes'' ++ cubes where
    cubes' = mapMaybe (\c'@(Cube onoff' _ _ _) -> intersect (not onoff') c c') cubes
    cubes'' = if onoff then c : cubes' else cubes'

computeCubes :: [Cube] -> [Cube]
computeCubes = foldl' addCube [] 

volume :: Cube -> Int
volume (Cube onoff (xmin, xmax) (ymin, ymax) (zmin, zmax)) =
    (if onoff then 1 else -1) * (xmax - xmin + 1) * (ymax - ymin + 1) * (zmax - zmin + 1)

totalVolume :: [Cube] -> Int 
totalVolume = sum . map volume

part1 :: [Cube] -> Int
part1 = totalVolume . computeCubes . mapMaybe (intersect' cube)
            where cube = Cube True (-50,50) (-50,50) (-50,50)
                  intersect' c1 c2@(Cube onoff _ _ _) = intersect onoff c1 c2

part2 :: [Cube] -> Int 
part2 = totalVolume . computeCubes

solve :: (HasLogFunc env) => Text -> RIO env ()
solve = aoc parser part1 part2