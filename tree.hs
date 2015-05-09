data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq)  

instance Show a => Show (Tree a) where
                show Empty = "Empty"
                show (Node val Empty Empty) = "Node " ++ show val ++ " Empty Empty"
                show (Node val left Empty) = "Node " ++ show val ++ " (" ++ show left ++ ") Empty"
                show (Node val Empty right) = "Node " ++ show val ++ " Empty (" ++ show right ++ ")"
                show (Node val left right) = "Node " ++ show val ++ " (" ++ show left ++ ") (" ++ show right ++ ")"

insert el Empty = Node el Empty Empty
insert el tree@(Node val left right)
               | val == el = tree
               | val > el = Node val (insert el left) right
               | val < el = Node val left (insert el right)

empty tree = tree == Empty

search val Empty = False

search needle tree@(Node val left right) 
	| needle == val = True
	| needle < val = search needle left
	| needle > val = search needle right 

isBalanced Empty = True
isBalanced tree@(Node val Empty right@(Node valR leftR rightR))
                | (leftR /= Empty || rightR /= Empty) = False
isBalanced tree@(Node val left@(Node valL leftL rightL) Empty)
                | (leftL /= Empty || rightL /= Empty) = False
isBalanced tree@(Node val left right) = isBalanced left && isBalanced right


vlr Empty = []
vlr tree@(Node val left right) = [val] ++ vlr left ++ vlr right

lvr Empty = []
lvr tree@(Node val left right) = lvr left ++ [val] ++ lvr right

lrv Empty = []
lrv tree@(Node val left right) = lrv left ++ lrv right ++ [val]

vrl Empty = []
vrl tree@(Node val left right) = [val] ++ vrl right ++ vrl left

rvl Empty = []
rvl tree@(Node val left right) = rvl right ++ [val] ++ rvl left

rlv Empty = []
rlv tree@(Node val left right) = rlv right ++ rlv left ++ [val]

toString Empty = []
toString tree@(Node val left right) = show val ++ "(" ++ toString left ++ "," ++ toString right ++ ")"

leaves Empty = []
leaves tree@(Node val Empty Empty) = [val]
leaves tree@(Node val left right) = leaves left ++ leaves right

nnodes Empty = 0
nnodes tree@(Node val left right) = nnodes left + nnodes right + 1

nsum Empty = 0
nsum tree@(Node val Empty Empty) = val
nsum tree@(Node val left right) = val + nsum left + nsum right

tmap op Empty = Empty
tmap op tree@(Node val left right) = Node (op val) (tmap op left) (tmap op right)

