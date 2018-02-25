import Sha256
import Numeric
import Data.Maybe

a = createGenesisBlock
m = 2^253



data BlockChain Block = Node Block [BlockChain Block] deriving (Show)



data Block = Block {index :: Int,
                    text :: String,
                    prevHash :: String,
                    hash :: String
                    } deriving (Show) 
                    
                    
numericHash hash = fst $ head $ readHex hash

createGenesisBlock = Block {index = 0, text = "This is the first text", prevHash = "0", hash = (sha256 "0This is the first text0")}

mineBlock prevBlock newText nonce 
                                 |(numericHash currHash) > m = mineBlock prevBlock newText (nonce + 1)
                                 |otherwise = createBlock prevBlock newText currHash
                                 where
                                   currHash =  sha256 ((show ((index prevBlock) + 1)) ++ newText ++ (hash prevBlock) ++ (show nonce))

                                   
createBlock prevBlock newText blockHash = Block {index = (index prevBlock) + 1,
                                       text = newText, 
                                       prevHash = hash prevBlock, 
                                       hash = blockHash}
                                       
try2AddBlock blockChain block prevId seed = if (sha256 seed) < m 
                             then addBlock blockChain block prevId -- "Just newBlockChain"
                             else blockChain -- IMPORTENT not the real return value should be "Nothing"
                             
addBlock prevId blockChain block 


addBlock :: Int -> Block -> BlockChain Block -> BlockChain Block
addBlock prevId block (Node root childs)
                                        |childs == [] && prevId /= (index root) = Nothing
                                        |prevId == (index root) = Just (Node root childs ++ [block])
                                        |otherwise = msum (map addBlock' childs)
                                        where
                                          addBlock' = (addBlock prevId block)
