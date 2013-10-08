--Dmitry Petuhov
--MM 2013

data Tree a = Empty | Node (Tree a) a (Tree a) deriving (Show, Eq)

insert Empty x = Node Empty x Empty
insert (Node l a r) x
	| x < a= Node (insert l x) a r
	| x > a = Node l a (insert r x)
	| otherwise = Node l a r

find Empty x = Nothing
find (Node l a r) x 
	| a == x 	= Just a
	| a > x 	= find l x
	| a < x 	= find r x

findMin (Node l a r) 
    | l == Empty    = Just a
    | otherwise     = findMin l

--delElem Empty x = Empty
--delElem (Node left value right)  x      
--    | x < value = Node (delElem left x) value right  
--    | x > value = Node left value (delElem right x)
--    | x == value = 
--      if left == Empty && right == Empty then Empty
--      else if left == Empty then right
--      else if right == Empty then left
--      else left --else Node left findMin right (delElem right (findMin right))

isBST (Node p@(Node l n r) a  Empty            ) = a > n && isBST p
isBST (Node Empty          a  p@(Node l n r)   ) = a < n && isBST p
isBST (Node p@(Node l n r) a  q@(Node l' n' r')) = a > n && a < n' && isBST p && isBST q
isBST (Node _ a _ )                              = True
isBST Empty                                      = True

treeToList l = f l []
	where 
		f Empty        lst  =  lst
		f (Node l a r) lst  = f l (a:f r lst)

listToTree []     = Empty
listToTree (x:xs) = Node (listToTree (filter (<x) xs)) x (listToTree (filter (>x) xs))

testTree = Node (Node Empty 2 (Node Empty 4 Empty)) 7 (Node Empty 8 Empty)
