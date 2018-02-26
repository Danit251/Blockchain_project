import Sha256
import Numeric
import Data.Maybe

a = createGenesisBlock
m = 2^253 :: Integer

zeroBlock = (Block {index = 0, text = "ZERO", prevHash = "0", hash = "0"})
chain = BlockChain zeroBlock [one, three]
one = BlockChain (Block {index = 1, text = "ONE", prevHash = "0", hash = ""}) []


two = Block {index = 2, text = "TWO", prevHash = "0", hash = ""}
three = BlockChain (Block {index = 3, text = "THREE", prevHash = "0", hash = ""}) []




data BlockChain a = BlockChain a [BlockChain a] deriving (Show)

data Block = Block {index :: Int,
                    text :: String,
                    prevHash :: String,
                    hash :: String
                    } deriving (Show) 
                    
instance Functor BlockChain where
    fmap f (BlockChain root childs) = BlockChain (f root) (fmap (fmap f) childs)   

instance Applicative BlockChain where

   pure a =  BlockChain a []
   (<*>) (BlockChain f []) tra = f <$> tra
   (<*>) tab@(BlockChain f tf0) (BlockChain x l0) = BlockChain (f x) l1
                  where l1 = [fu <*> a | fu <- tf0 , a <- l0]

instance Monad BlockChain where
   return a = pure a

   (>>=) (BlockChain x []) amb = amb x
   (>>=) (BlockChain x l0) amb = BlockChain b (m1 <$> l0)
         where (BlockChain b _) = amb x
               m1 ta = ta >>= amb  

instance Monoid (Block) where
    mempty = createGenesisBlock
    x `mappend` y = Block {index = (index x) + (index y),
                           text = (text x) ++ (text y),
                           prevHash = (prevHash x) ++ (prevHash y),
                           hash = (hash x) ++ (hash y)}                
                    
numericHash hash = (fst $ head $ readHex hash) :: Integer

createGenesisBlock = Block {index = 0, text = "This is the first text", prevHash = "0", hash = (sha256 "0This is the first text0")}

mineBlock prevBlock newText nonce 
                                 |(numericHash currHash) > m = mineBlock prevBlock newText (nonce + 1)
                                 |otherwise = (createBlock prevBlock newText currHash, seed)
                                 where
                                   seed = (show ((index prevBlock) + 1)) ++ newText ++ (hash prevBlock) ++ (show nonce)
                                   currHash = sha256 (seed)

                                   
createBlock prevBlock newText blockHash = Block {index = (index prevBlock) + 1,
                                       text = newText, 
                                       prevHash = hash prevBlock, 
                                       hash = blockHash}
                                       
try2AddBlock bc b prevId seed = if (numericHash $ sha256 seed) < m 
                                then addBlock prevId bc (BlockChain b []) 
                                else bc


addBlock id (BlockChain x []) _
                               |(index x) /= id = (BlockChain x []) 
addBlock id (BlockChain x ys) block 
                                  |(index x == id) = BlockChain x $ ys ++ [block]
                                  |otherwise = BlockChain x $ map (\z -> addBlock id z block) ys


blockLength :: Block -> Int
blockLength block = length (text block)

blockIndex :: Block -> Int
blockIndex block = index block

blockIndex' :: Block -> Int
blockIndex' block = (index block) + 1

hamsa block = BlockChain newBlock []
  where newBlock = Block {index = 555, text = "555", prevHash = "555", hash = "555"}



