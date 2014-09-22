import Monad (guard)

type Point = (Integer, Integer)
type Triangle = [Point]

parseTriangle :: String -> Triangle
parseTriangle str =  toTriangle $ read ('[':str ++ "]")
    where toTriangle (x:y:xs) = (x,y) : toTriangle xs
          toTriangle [] = []
       
main = do
    contents <- readFile "triangles.txt"
    let triangles = map parseTriangle $ lines contents
    print . length $ filter hasOrigin triangles
    
hasOrigin :: Triangle -> Bool
hasOrigin [a,b,c] = ss a b c && ss c a b && ss b c a

ss p a b = (cross (b `less` a) ((0,0) `less` a)) * (cross (b `less` a) (p `less` a)) >= 0
    where less (x1,x2) (y1,y2) = (x1-y1,x2-y2)
          cross (u1,u2) (v1,v2) = u1*v2 - u2*v1