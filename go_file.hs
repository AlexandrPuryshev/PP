import System.Environment   
import System.Directory  
import System.IO  
import Data.List
import Data.Char

view :: [String] -> IO ()  
view [fileName] = do  
    contents <- readFile fileName  
    let todoTasks = lines contents  
    	numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks  
    putStr $ unlines numberedTasks 

view_without_lines :: [String] -> IO () 
view_without_lines[fileName] = do
	withFile fileName ReadMode (\handle -> do  
	contents <- hGetContents handle  
	putStr contents
	putStr "\n")

remove :: [String] -> IO ()  
remove [fileName, numberString] = do  
    handle <- openFile fileName ReadMode  
    (tempName, tempHandle) <- openTempFile "." "temp"  
    contents <- hGetContents handle  
    let number = read numberString  
        todoTasks = lines contents  
        newTodoItems = delete (todoTasks !! number) todoTasks  
    hPutStr tempHandle $ unlines newTodoItems  
    hClose handle  
    hClose tempHandle  
    removeFile fileName  
    renameFile tempName fileName

add :: [String] -> IO ()  
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

copyWithisAsciiFilter :: [String] -> IO ()  
copyWithisAsciiFilter [fileName1, fileName2] = do
    content <- readFile fileName1
    writeFile fileName2 (filter isAscii content)

copyWithisHexDigitFilter :: [String] -> IO ()  
copyWithisHexDigitFilter [fileName1, fileName2] = do
    content <- readFile fileName1
    writeFile fileName2 (filter isHexDigit content)


input1 :: String -> IO [String]
input1 arg1 = do
    putStrLn arg1
    somethingArg1 <- getLine
    let string = [somethingArg1]
    return string

input2 :: String -> String -> IO [String]
input2 arg1 arg2 = do
    putStrLn arg1
    somethingArg1 <- getLine
    putStrLn arg2
    somethingArg2 <- getLine
    let string = [somethingArg1, somethingArg2]
    return string

--stdin/stdout обрабатываются в текстовом режиме, с автоматической трансляцией \n в OS-specific line delimiter

--По умолчанию для stdout используется построчная буферизация, и это означает, что строки не выводятся до появления символа конца строки ('\n'). 
--Если вы хотите написать интерактивную программу, то вам может потребоваться отключение буферизации stdout:

main = do
    hSetBuffering stdout NoBuffering
    putStrLn "Файл обработки текстовых файлов включает следующее:"
    putStrLn "1: Добавить в файл текст: (имя файла) (текст)"
    putStrLn "2: Просмотреть содержимое файла: (имя файла)"
    putStrLn "3: Просмотреть содержимое файла без нумерации строк: (имя файла)" 
    putStrLn "4: Удалить строку в файле: (имя файла) (номер строки)"
    putStrLn "5: Копирование содержимого в новый файл с фильтрацией Ascii символов: (имя файла1) (имя файла2)"
    putStrLn "6: Копирование содержимого в новый файл с фильтрацией на 16-чные символы: (имя файла1) (имя файла2)"
    res <- readNum

    case res of
         1 -> do
            let a = "Введите (имя файла)"
            let b = "Введите (текст)"
            parametrs <- input2 a b
            add parametrs
         2 -> do
            let a = "Введите (имя файла)"
            parametrs <- input1 a
            view parametrs
         3 -> do
            let a = "Введите (имя файла)"
            parametrs <- input1 a
            view_without_lines parametrs
         4 -> do
            let a = "Введите (имя файла)"
            let b = "Введите (номер строки)"
            parametrs <- input2 a b
            remove parametrs
         5 -> do
            let a = "Введите (имя файла1)"
            let b = "Введите (имя файла2)"
            parametrs <- input2 a b
            copyWithisAsciiFilter parametrs
         6 -> do
            let a = "Введите (имя файла1)"
            let b = "Введите (имя файла2)"
            parametrs <- input2 a b
            copyWithisHexDigitFilter parametrs
    putStrLn "Команда выполнена"
    where readNum :: IO Integer; readNum = readLn		            