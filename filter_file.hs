import System.IO
import System.Environment
import Data.Char

replace :: Char->Char->String->String;
replace pat1 pat2 list = [if x==pat1 then pat2 else x | x<-list]

main :: IO ()
main = do
    [arg1,arg2,arg3] <- getArgs
    src <- readFile arg1
    let arg = head(arg3)
    let res = (replace ',' arg src)
    writeFile arg2 res

--Example
--:main file.txt output.txt !