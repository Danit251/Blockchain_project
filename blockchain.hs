import Sha256
import Numeric
import Data.Maybe

a = createGenesisBlock
m = 2^253

zero = Block {index = 0, text = "ZERO", prevHash = "0", hash = ""}
one = Block {index = 1, text = "ONE", prevHash = "0", hash = ""}
two = Block {index = 2, text = "TWO", prevHash = "0", hash = ""}
three = BlockChain (Block {index = 3, text = "THREE", prevHash = "0", hash = ""}) []
--retree = BlockChain 5 [BlockChain 3 [BlockChain 1 [], BlockChain 4[]], BlockChain 7 []]

rosetree = BlockChain zero [BlockChain one [], BlockChain two []]

data BlockChain a = BlockChain a [BlockChain a] deriving (Show)



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
                                       
--try2AddBlock blockChain block prevId seed = if (sha256 seed) < m 
--                             then addBlock blockChain block prevId -- "Just newBlockChain"
--                             else blockChain -- IMPORTENT not the real return value should be "Nothing"


add id (BlockChain x []) _
             |(index x) /= id = (BlockChain x []) 
add id (BlockChain x ys) block 
                          |(index x == id) = BlockChain x $ ys ++ [block]
                          |otherwise = BlockChain x $ map (\z -> add id z block) ys

