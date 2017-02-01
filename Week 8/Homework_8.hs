module Homework_8 where
import Employee
import Data.Monoid
import Data.Tree
import Data.List

-- Exercise 1 solution

glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp nm fun) (GL emps funt)
  = GL (e:emps) (funt+fun)
  

instance Monoid (GuestList) where
  mempty = GL [] 0
  mappend (GL emps funt) (GL oemps ofunt) = GL (emps ++ oemps) (funt + ofunt)
  
moreFun :: GuestList -> GuestList -> GuestList
moreFun glLeft@(GL _ funtLeft) glRight@(GL _ funtRight)
  | funtLeft >= funtRight = glLeft
  | otherwise             = glRight
  

-- Exercise 2 solution

treeFold :: b -> (a -> [b] -> b) -> Tree a -> b
treeFold e f (Node rt sbTs) = f rt (map (treeFold e f) sbTs)

-- Exercise 3 solution

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel emp gsLs = (GL [emp] (empFun emp), optimal)
  where
    findMaxGL :: [GuestList] -> GuestList
    findMaxGL gsLst = foldr max (mempty :: GuestList) gsLst
   
    -- Do something like a power set here. To decide which to add
    -- from a subtree, we figure out 
    combine :: (GuestList, GuestList) -> [GuestList] -> [GuestList]
    combine (bosses, emps) [] = [bosses, emps]
    combine (bosses, emps) gsLs = [
           findMaxGL ((map (mappend bosses) gsLs) ++ (map (mappend emps) gsLs))
        ]
	  
    optimal = findMaxGL (foldr combine [] gsLs)
	
-- Exercise 4 solution

maxFun :: Tree Employee -> GuestList
maxFun tree = uncurry max (treeFold (mempty, mempty) nextLevel tree)

-- Exercise 5 solution

printEmployees :: GuestList -> String
printEmployees (GL emps _) = unlines (sort (map (empName) emps))

printGuestList :: Tree (Employee) -> String
printGuestList tree = "Total fun: " ++ (show fun) 
                   ++ "\n" ++ (printEmployees opt) 
  where
    opt@(GL emps fun) = maxFun tree
	
main = do
         contents <- readFile "company.txt"
         writeFile "company-out.txt" (printGuestList (read contents))
