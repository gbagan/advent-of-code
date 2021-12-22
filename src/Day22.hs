module Day22 (solve) where
import           Data.List (foldl')
import           Data.Maybe (mapMaybe)
import           Text.Megaparsec (sepEndBy1, (<|>))
import qualified Text.Megaparsec.Char as P
import           Util (Parser, aocTemplate, signedInteger)

data Cube = Cube Bool (Int, Int) (Int, Int) (Int, Int)

parser :: Parser [Cube]
parser = cube `sepEndBy1` P.eol where
    cube = Cube <$> onoff <* P.string " x=" <*> pair <* P.string ",y=" <*> pair <* P.string ",z=" <*> pair 
    pair = (,) <$> signedInteger <* P.string ".." <*> signedInteger
    onoff = True <$ P.string "on" <|> False <$ P.string "off"

disjoint :: Cube -> Cube -> Bool
disjoint (Cube _ (xmin, xmax) (ymin, ymax) (zmin, zmax))
         (Cube _ (xmin', xmax') (ymin', ymax') (zmin', zmax')) =
            xmax < xmin' || ymax < ymin' || zmax < zmin' || xmax' < xmin || ymax' < ymin || zmax' < zmin

intersect :: Bool -> Cube -> Cube -> Maybe Cube
intersect on
          c1@(Cube _ (xmin, xmax) (ymin, ymax) (zmin, zmax))
          c2@(Cube _ (xmin', xmax') (ymin', ymax') (zmin', zmax')) =
            if disjoint c1 c2 then
                Nothing 
            else
                Just $ Cube on (max xmin xmin', min xmax xmax') (max ymin ymin', min ymax ymax') (max zmin zmin', min zmax zmax')

addCube :: [Cube] -> Cube -> [Cube]
addCube cubes c@(Cube on _ _ _) =
    let cubes' = mapMaybe (\c'@(Cube on' _ _ _) -> intersect (not on') c c') cubes
        cubes'' = if on then c : cubes' else cubes'
    in cubes'' ++ cubes

computeCubes :: [Cube] -> [Cube]
computeCubes = foldl' addCube [] 

volume :: Cube -> Int
volume (Cube on (xmin, xmax) (ymin, ymax) (zmin, zmax)) =
    (if on then 1 else -1) * (xmax - xmin + 1) * (ymax - ymin + 1) * (zmax - zmin + 1)

totalVolume :: [Cube] -> Int 
totalVolume = sum . map volume

part1 :: [Cube] -> Int
part1 = totalVolume . computeCubes . mapMaybe (intersect' cube)
            where cube = Cube True (-50,50) (-50,50) (-50,50)
                  intersect' c1 c2@(Cube on _ _ _) = intersect on c1 c2

part2 :: [Cube] -> Int 
part2 = totalVolume . computeCubes

solve :: String -> IO ()
solve = aocTemplate parser pure (pure . part1) (pure . part2)