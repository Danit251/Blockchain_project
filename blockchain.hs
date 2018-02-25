import Sha256
import Numeric

a = createGenesisBlock
m = 2^240


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
                                       
--addBlock indexPrev newBlock =  
