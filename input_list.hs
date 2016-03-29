import System.IO


main = do
         hSetBuffering stdout NoBuffering            
         putStr   "Input the first value: "        
         x1 <- readNum 
         putStr   "Input count of elements: "          
         x2 <- readNum
         putStr   "Input the multiplicity: "          
         x3 <- readNum                          
         putStr  ("result = " ++ show ([x | x<-[x1..x2], (x `mod` x3 == 0)]) ++ "\n")
       where readNum :: IO Integer
             readNum = readLn