module HaskellDatatypes where

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Eq)

insert :: (Ord a) => a -> Tree a -> Tree a
insert x EmptyTree = Node x EmptyTree EmptyTree
insert x (Node v left right)
    | x < v = Node v (insert x left) right
    | otherwise = Node v left (insert x right)

contains :: (Ord a) => a -> Tree a -> Bool
contains x EmptyTree = False
contains x (Node v left right)
    | x == v = True
    | x < v = contains x left
    | otherwise = contains x right

toList :: Tree a -> [a]
toList EmptyTree = []
toList (Node v left right) = (toList left) ++ [v] ++ (toList right)

remove :: (Ord a) => a -> Tree a -> Tree a
remove x EmptyTree = EmptyTree
remove x (Node v left right)
    | x == v =
        case (left, right) of
          (EmptyTree, EmptyTree) -> EmptyTree
          (EmptyTree, child) -> child
          (child, EmptyTree) -> child
          (Node vl _ _, Node vr _ _) ->
              if True
              then Node vl (remove vl left) right
              else Node vr left (remove vr right)
    | x < v = Node v (remove x left) right
    | otherwise = Node v left (remove x right)

printInOrder :: (Show a) => Tree a -> IO ()
printInOrder EmptyTree = return ()
printInOrder (Node v left right) = do
  printInOrder left
  putStrLn $ show v
  printInOrder right
  return ()

bstSort :: (Ord a) => [a] -> [a]
bstSort lst = toList $ foldl (flip insert) EmptyTree lst

streamToTree :: IO (Tree String)
streamToTree = do
  x <- getLine
  if null x
  then return EmptyTree
  else do
    tree <- streamToTree
    return $ insert x tree

sortStream :: IO [String]
sortStream = do
  tree <- streamToTree
  return $ toList tree
