import System.Environment   
import System.Directory  
import System.IO  
import Data.List 

dispatch :: [(String, [String] -> IO ())]  
dispatch =  [ ("add", add)  
            , ("view", view)  
            , ("remove", remove)
            , ("view_without_lines", view_without_lines) 
            ]

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

main = do
		let (Just action) = lookup command dispatch
		putStrLn "Файл обработки текстовых файлов включает следующее:"
		putStrLn "1: Добавить в файл текст аргументы: (имя файла) (текст)"
		putStrLn "2: Просмотреть содержимое файла: (имя файла)"
		putStrLn "3: Просмотреть содержимое файла без нумерации строк: (имя файла)" 
		putStrLn "4: Удалить строку в файле: (имя файла) (номер строки)"
		res = readNum
		args <- getArgs  
		f res = case res 
						1 -> let (Just action) = lookup add dispatch
				        2 -> let (Just action) = lookup view dispatch
				        3 -> let (Just action) = lookup view_without_lines dispatch
				        4 -> let (Just action) = lookup remove dispatch - x
		action args
		where readNum :: IO Integer
		readNum = readLn		            
        --let (Just action) = lookup command dispatch  
        --action args