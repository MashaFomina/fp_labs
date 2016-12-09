-- Structure of Tree
data BinaryTree = EmptyTree
                  | Leaf Integer
                  | Node Integer BinaryTree BinaryTree


-- insert
insert                          :: BinaryTree -> Integer -> BinaryTree
insert EmptyTree x              = Leaf x
insert (Leaf c) x               | (c == x) = Leaf c
                                | (c > x)  = Node c (Leaf x) EmptyTree
                                | (c < x)  = Node c EmptyTree (Leaf x)
insert (Node c l r) x           | (c == x) = Node c l r
                                | (c > x)  = Node c (insert l x) r
                                | (c < x)  = Node c l (insert r x)

-- remove
remove                          :: BinaryTree -> Integer -> BinaryTree
remove EmptyTree x              = EmptyTree
remove (Leaf c) x               | (c == x) = EmptyTree
                                | otherwise  = Leaf c
remove (Node c l r) x           | (c == x) = rebuildTree l (listFromTree r)
                                | (c > x)  = Node c (remove l x) r
                                | (c < x)  = Node c l (remove r x)
-- rebuildTree
rebuildTree                      :: BinaryTree -> [Integer] -> BinaryTree
rebuildTree tree []              = tree
rebuildTree tree (h:t)           = rebuildTree (insert tree h) t

-- emptyTree
emptyTree                       :: BinaryTree
emptyTree                       = EmptyTree

-- containsElement
containsElement EmptyTree _     = False
containsElement (Leaf x) y      | (x == y) = True
                                | otherwise = False
containsElement (Node c l r) y  | (c == y) = True
                                | (c < y) = containsElement l y
                                | (c > y) = containsElement r y

-- nearestGE
nearestGE                              :: BinaryTree -> Integer -> Integer
nearestGE EmptyTree x                   = error "Empty Tree" -- exception
nearestGE (Leaf c) x                    = c
nearestGE (Node c EmptyTree r) x        | (c == x) = c
                                        | (c < x) = nearestGE r x
                                        | otherwise = c                  
nearestGE (Node c l EmptyTree) x        | (c == x) = c
                                        | (c > x) = if (nearestLeft >= x) then nearestLeft else c
                                        | otherwise = c
                                        where nearestLeft = nearestGE l x
nearestGE (Node c l r) x                | (c == x) = c
                                        | (c > x) = if (nearestLeft >= x) then nearestLeft else c
                                        | (c < x) = nearestGE r x
                                        where nearestLeft = nearestGE l x

-- treeFromList
treeFromList                    :: [Integer] -> BinaryTree
treeFromList []                 = EmptyTree
treeFromList (h:t)              = treeFromList' EmptyTree (h:t)
-- function for convertion List to Tree
treeFromList'                   :: BinaryTree -> [Integer] -> BinaryTree
treeFromList' tree []           = tree
treeFromList' tree (h:t)        = treeFromList' (insert tree h) t

-- listFromTree
listFromTree                      :: BinaryTree -> [Integer]
listFromTree (EmptyTree)          = []
listFromTree (Leaf x)             = [x]
listFromTree (Node x left right)  = listFromTree left ++ [x] ++ listFromTree right


