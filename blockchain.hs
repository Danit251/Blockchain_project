import Sha256
import Numeric

m = 2^253 :: Integer

data BlockChain a = BlockChain a [BlockChain a] deriving (Show)

data Block = Block {index :: Int,
                    text :: String,
                    prevHash :: String,
                    hash :: String
                    } 
                          
instance Show Block where
    show (Block index text prevHash hash) = "\nindex:         " ++ show index ++
                                          "\ntext:          " ++ show text ++
                                          "\nprevious hash: " ++ show prevHash ++
                                          "\nhash:          " ++ show hash ++ 
                                          "\n*************"
       
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
    
createGenesisBlock :: Block
createGenesisBlock = Block {index = 0,
                            text = "This is the genesis block, first of it's kind.",
                            prevHash = "0",
                            hash = (sha256 "")}
                            
mineBlock :: Block -> String -> Integer -> (Block, String)
mineBlock prevBlock newText nonce 
    |(numericHash currHash) > m = mineBlock prevBlock newText (nonce + 1)
    |otherwise = (createBlock prevBlock newText currHash, seed)
      where
        seed = (show ((index prevBlock) + 1)) ++ newText ++ (hash prevBlock) ++ (show nonce)
        currHash = sha256 (seed)

createBlock :: Block -> String -> String -> Block                                   
createBlock prevBlock newText blockHash = Block {index = (index prevBlock) + 1,
                                       text = newText, 
                                       prevHash = hash prevBlock, 
                                       hash = blockHash}
               
try2AddBlock :: BlockChain Block -> Block -> Int -> String -> BlockChain Block                                       
try2AddBlock bc b prevId seed = if (numericHash $ sha256 seed) < m 
                                  then addBlock prevId bc (BlockChain b []) 
                                else bc

addBlock :: Int -> BlockChain Block -> BlockChain Block -> BlockChain Block
addBlock id (BlockChain x []) _
    |(index x) /= id = (BlockChain x []) 
addBlock id (BlockChain x ys) block 
    |(index x == id) = BlockChain x $ ys ++ [block]
    |otherwise = BlockChain x $ map (\z -> addBlock id z block) ys

numericHash :: String -> Integer
numericHash hash = (fst $ head $ readHex hash) :: Integer


blockLength :: Block -> Int
blockLength block = length (text block)

blockIndex :: Block -> Int
blockIndex block = index block

hamsa :: Block -> BlockChain Block
hamsa block = BlockChain newBlock []
    where newBlock = Block {index = 555, text = "555", prevHash = "555", hash = "555"}
 
--------------------------tests------------------ 
genesis = createGenesisBlock
bc0 = BlockChain genesis []
(block1, seed1) = mineBlock genesis "The lottery numbers for tomorrow are 4 8 15 16 23 42" 0
bc1 = try2AddBlock bc0 block1 0 seed1
--Trying to insert block without proofing my work on sha256.
fakeBlock = Block {index = 666, text = "I'm a FAKE BLOCK", prevHash = hash block1, hash = sha256 "abc"} 
--should be the same blockchain as bc1.
fakeBc = try2AddBlock bc1 fakeBlock 1 "abc" 
--adding the second block
(block2, seed2) = mineBlock block1 "We <3 Haskell" 0
bc2 = try2AddBlock bc1 block2 1 seed2
--functor
bcLengths = fmap blockLength bc2
--applicative
appBlockChain = (BlockChain blockLength [BlockChain blockIndex []]) <*> bc2
--monad
hamsaBlockChain = bc2 >>= hamsa

