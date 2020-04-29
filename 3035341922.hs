import Data.Char
import System.IO
type Sudoku= [(Int,Char)]
type Move= [(Int,Char)]

-- This function will start the Jigsaw sudoku program
-- Input: No parameters
-- Output: IO 

main = do 
          putStrLn "Welcome to Jigsaw Sudoku"
          optionsPanel [] [] []

-- This function will display the possible game options
-- Input:Jigsaw board of type Sudoku, Moves and Undone Moves
-- Output: IO
optionsPanel::Sudoku->Move->Move->IO()
optionsPanel jigsaw moves undone= do 
                        putStrLn "The available options are:"
                        putStrLn "1. Load Board"
                        putStrLn "2. Save Board"
                        putStrLn "3. Display Board"
                        putStrLn "4. Make my move"
                        putStrLn "5. Quit"
                        putStrLn "6. Solve board"
                        putStrLn "7. Undo my move"
                        putStrLn "8. Redo my move"
                        putStrLn "9. Need a hint"
                        putStrLn "What would you like to do?"
                        putStrLn "Please enter the integer corresponding to your choice and press enter"
                        chooseOption jigsaw moves undone
-- This function will allow the user to choose a specific option from the options displayed
-- Input:Jigsaw Board of type Sudoku, Moves and Undone moves
-- Output: IO
chooseOption:: Sudoku->Move->Move->IO()
chooseOption jigsaw moves undone=do
                       hSetBuffering stdout NoBuffering 
                       putStr "My choice is :"
                       choice <- getLine 
                       case choice of "1" -> do jigsaw_new <- readSudoku
                                                optionsPanel jigsaw_new moves undone
                                      "2" -> do saveSudoku jigsaw
                                                optionsPanel jigsaw moves undone
                                      "3" -> do 
                                                if length jigsaw == 0 then
                                                    do
                                                        putStrLn "Cannot display board since no board loaded"
                                                        optionsPanel jigsaw moves undone
                                                else
                                                    do
                                                        display jigsaw
                                                        optionsPanel jigsaw moves undone
                                      "4" -> do 
                                                if length jigsaw == 0 then
                                                    do
                                                        putStrLn "Cannot make a move until board is loaded"
                                                        optionsPanel jigsaw moves undone
                                                else
                                                    do
                                                        putStrLn "\n Next move:"
                                                        putStrLn "Enter Row number:"
                                                        r <- getLine  
                                                        putStrLn "Enter Column number:"
                                                        c <- getLine
                                                        putStrLn "Enter the number:"
                                                        num <- getLine
                                                        let row=read(r)
                                                        let col=read(c)
                                                        let n=row*9+col
                                                        let number=read(num)
                
                                                        if row `elem` [0..8] && col `elem` [0..8]  && number `elem` [1..9]then

                                                            if  (validation row col  (read num) jigsaw ) then
                                                                do 
                                                                    let moves_new=[(n,intToDigit number)]++moves
                                                                    let jigsaw_new=take (n) jigsaw ++ [(fst(jigsaw!!n),intToDigit number)] ++ drop (n+1) jigsaw
                                                                    if completeBoard jigsaw_new then
                                                                        do
                                                                            putStrLn "Congratulations you win the game"
                                                                            display jigsaw_new
                                
                                                                    else
                                                                        do
                                                                            display jigsaw_new
                                                                            optionsPanel jigsaw_new moves_new []
                                                            else do
                                                                putStrLn "Invalid Move"
                                                                optionsPanel jigsaw moves undone 
                                                        else
                                                            do 
                                                            putStrLn "Row or Column or Number out of index"
                                                            optionsPanel jigsaw moves undone
                                      "5" -> putStrLn "Thank you for playing Jigsaw Sudoku"
                                      "6" -> do
                                                if length jigsaw == 0 then
                                                    do
                                                        putStrLn "Cannot solve until board is not loaded"
                                                        optionsPanel jigsaw moves undone
                                                else
                                                    do 
                                                        let jigsaw_new = (solver jigsaw)
                                                        if length jigsaw_new /=0 then
                                                            do
                                                                display jigsaw_new
                                                                putStrLn "This is the solution"
                                                                optionsPanel jigsaw moves undone
                                                        else
                                                            do
                                                                putStrLn "No solution for this board"
                                      "7" -> do 
                                                if length jigsaw == 0 then
                                                    do
                                                        putStrLn "No undo possible until board is loaded"
                                                        optionsPanel jigsaw moves undone
                                                else
                                                    do
                                                        putStrLn "Un-doing your move"
                                                        if length moves /= 0 then
                                                            do
                                                                let current_move=moves!!0
                                                                let moves_new=drop 1 moves
                                                                let undone_new=[current_move]++undone
                                                                let index=fst(current_move)
                                                                let jigsaw_new=take index jigsaw ++ [(fst(jigsaw!!index),'.')]++ drop (index +1) jigsaw
                                                                display jigsaw_new
                                                                optionsPanel jigsaw_new moves_new undone_new
                                                        else
                                                            do
                                                                putStrLn "No moves to undo"
                                                                optionsPanel jigsaw moves undone
                                      "8" -> do 
                                                if length jigsaw == 0 then
                                                    do
                                                        putStrLn "No redo possible until board is loaded"
                                                        optionsPanel jigsaw moves undone
                                                else
                                                    do      
                                                        putStrLn "Re-doing your move"
                                                        if length undone /= 0 then
                                                            do
                                                                let redo_move=undone!!0                                                                
                                                                let num=snd(redo_move)
                                                                let moves_new=[redo_move]++moves
                                                                let index=fst(redo_move)
                                                                let undone_new=drop 1 undone
                                                                let jigsaw_new=take index jigsaw ++ [(fst(jigsaw!!index),num)]++ drop (index +1) jigsaw
                                                                display jigsaw_new
                                                                optionsPanel jigsaw_new moves_new undone_new
                                                        else
                                                            do
                                                                putStrLn "No moves to redo"
                                                                optionsPanel jigsaw moves undone
                                      "9" -> do
                                                if length jigsaw == 0 then
                                                    do
                                                        putStrLn "No hint possible until board is loaded"
                                                        optionsPanel jigsaw moves undone
                                                else
                                                    do 
                                                        let jigsaw_new = (solver jigsaw)
                                                        if length jigsaw_new /= 0 then
                                                            do
                                                            let hints=[a | (a,(b,c))<- zip [0..] jigsaw, c=='.']
                                                            let hint=hints!!0
                                                            let num=digitToInt (snd(jigsaw_new!!hint))
                                                            let hintText="Move to Row:"++ show(hint`div`9)++", Column:"++show(hint `mod` 9)++" and put number:"++ show num
                                                            putStrLn hintText
                                                            optionsPanel jigsaw moves undone
                                                        else
                                                            do
                                                            putStrLn "No possible correct hint for this configuration"
                                                            optionsPanel jigsaw moves undone

                                      _   -> do 
                                                putStrLn "INVALID choice entry. Please enter the correct number"
                                                optionsPanel jigsaw moves undone
-- This function will generate a board based on the filename specified by the user
-- Input:No Parameters
-- Output: IO Sudoku
readSudoku :: IO Sudoku
readSudoku= do putStr "Enter the file name:"
               fileName <- getLine
               fileContents <- readFile fileName
               let rows = lines fileContents
               let jigsaw= createJigsaw (take 9 rows) (drop 9 rows) [0..8]
               putStrLn "\n Here is the board"
               display jigsaw 
               return jigsaw

-- This function will create the jigsaw sudoku board
-- Input:2 strings of blocks and values and their indices of type [Int]
-- Output: Jigsaw Sudoku board
createJigsaw :: [String] -> [String] -> [Int] -> Sudoku
createJigsaw pos vals indices=  [ (digitToInt (a!!index), b!!index)   |  (a,b)<- zip pos vals , index<-indices]
               
-- This function will display the jigsaw sudoku board
-- Input: Sudoku board
-- Output: IO                                        
display::Sudoku->IO()
display jigsaw= do putStr (displaySudoku jigsaw (-1))
                   
-- This function will act as a helper function for displaying the board
-- Input:Sudoku board and an integer representing the 81 possible indices
-- Output: String representation of board
displaySudoku :: Sudoku -> Int -> String
displaySudoku jigsaw cur  
                     | cur == 81 = ""
                     | cur == 80 = " "++ [snd (jigsaw!!cur)]++" " ++ "|\n'" ++ endLine jigsaw 72 73 0
                     | cur == -1 = "." ++initialLine jigsaw 0 1 0  ++ displaySudoku jigsaw (cur + 1)  
                     | cur `mod` 9 == 0 = "|" ++ " "++ [snd (jigsaw!!cur)] ++ " " ++ checkborder jigsaw cur (cur+1) ++ displaySudoku jigsaw (cur +1) 
                     | cur `mod` 9 == 8 = " "++ [snd (jigsaw!!cur)]++" " ++ "|\n" ++ drawLine jigsaw (cur -8) (cur +1) 0 ++ displaySudoku jigsaw (cur + 1) 
                     | otherwise = " "++ [snd (jigsaw!! cur)] ++ " "++ checkborder jigsaw cur (cur+1) ++ displaySudoku jigsaw (cur+1)
 -- This function will draw the line after every row of the board
-- Input:Sudoku board and 3 Integers representating the 2 adjacent cells and index respectively
-- Output: String representing the display
drawLine :: Sudoku -> Int -> Int -> Int -> String
drawLine jigsaw first second index
 | index == 9 && second < 72 && fst(jigsaw!!(first-1))/=fst(jigsaw!!(second-1)) = ":\n"
 | index == 9 = "|\n"
 | index == 0 && fst(jigsaw!!first)/=fst(jigsaw!!second) = ":---" ++ drawLine jigsaw (first+1) (second+1) (index+1)
 | index == 0 && fst(jigsaw!!first)==fst(jigsaw!!second) = "|   " ++ drawLine jigsaw (first+1) (second+1) (index+1)
 | fst(jigsaw!!first)/=fst(jigsaw!!second) = connector jigsaw first second index "-" ++ "---" ++ drawLine jigsaw (first+1) (second+1) (index+1)
 | fst(jigsaw!!first)==fst(jigsaw!!second) = connector jigsaw first second index " " ++ "   " ++ drawLine jigsaw (first+1) (second+1) (index+1)

 -- This function will select the various possible connectors like ., :,' and |
-- Input:Sudoku board and 3 Integers representating the 2 adjacent cells and index respectively
-- Output: String representing the display
connector :: Sudoku -> Int -> Int -> Int -> String -> String
connector jigsaw first second index val
 | fst(jigsaw!!(first-1))/=fst(jigsaw!!first) && fst(jigsaw!!(second-1))/=fst(jigsaw!!second) = if (fst(jigsaw!!first)==fst(jigsaw!!second) && fst(jigsaw!!(first-1))==fst(jigsaw!!(second-1))) then "|" else ":"
 | fst(jigsaw!!(first-1))/=fst(jigsaw!!first) = "'"
 | fst(jigsaw!!(second-1))/=fst(jigsaw!!second) = "."
 | fst(jigsaw!!(first-1))==fst(jigsaw!!first) && fst(jigsaw!!(second-1))==fst(jigsaw!!second) = val


-- This function will generate the last horizontal line of the board
-- Input:Sudoku board and 3 Integers representating the 2 adjacent cells and index respectively
-- Output: String representing the display
endLine:: Sudoku -> Int -> Int -> Int -> String
endLine jigsaw first second index
                     | index==8 = "---'\n"
                     | fst(jigsaw!!first)==fst(jigsaw!!second)="----"++ endLine jigsaw (first+1) (second+1) (index+1)
                     | otherwise= "---'"++ endLine jigsaw (first+1) (second+1) (index+1)
-- This function will generate the initial horizontal line of the board
-- Input:Sudoku board and 3 Integer representing the 2 adjacent cells and index respectively
-- Output: String representing the display
initialLine:: Sudoku -> Int -> Int -> Int -> String
initialLine jigsaw first second index 
                     | index == 8 = "---.\n"
                     | fst(jigsaw!!first)==fst(jigsaw!!second)="----"++ initialLine jigsaw (first+1) (second+1) (index+1)
                     | otherwise= "---."++ initialLine jigsaw (first+1) (second+1) (index+1)
-- This function is a helper function to decide the border cases for the display
-- Input:Sudoku board and 3 Integer representing the 2 adjacent cells and index respectively
-- Output: String representing the display
checkborder:: Sudoku -> Int-> Int-> String
checkborder jigsaw first second
                     | fst(jigsaw!!first)== fst(jigsaw!!second) = " "
                     |  otherwise = "|"
-- This function will save the board in a file
-- Input:Sudoku board 
-- Output: IO
saveSudoku:: Sudoku-> IO()
saveSudoku jigsaw= do 
                      if length jigsaw == 0 then
                        putStrLn "Unable to save board since no board loaded"
                      else   
                        do               
                            putStr "Enter the file name:"
                            fileName<-getLine
                            let boxes=intToString jigsaw 
                            let values=charToString jigsaw
                            let finalText= fileFormat boxes ++ fileFormat values
                            writeFile fileName finalText
-- This is a helper function to store the blocks of the board as a string
-- Input:Sudoku board 
-- Output: String representing the blocks
intToString :: Sudoku -> String
intToString jigsaw= concat [  show a | (a,b) <- jigsaw]
-- This is a helper function to store the values of the board as a string
-- Input:Sudoku board
-- Output: String representing the values                      
charToString:: Sudoku -> String
charToString jigsaw=  [  b | (a,b) <-jigsaw]
-- This is a helper function to generate the string in correct file format
-- Input:String
-- Output: Formatted String                      
fileFormat:: String -> String
fileFormat [] = []
fileFormat xs= take 9 xs  ++ "\n" ++ fileFormat (drop 9 xs)


-- This function will check if the input number satisfies the sudoku conditions
-- Input:3 INT representing the row, column and number respectively and the Sudoku board
-- Output: Boolean
validation:: Int->Int->Int->Sudoku->Bool
validation row col num jigsaw= snd(jigsaw!!index)=='.' && row `elem` [0..8] && col `elem` [0..8] && rowCheck row num 0 jigsaw && columnCheck col num 0 jigsaw && boxCheck  (fst(jigsaw!!index)) num jigsaw   where index= row*9 + col


-- This function will check the row condition of the input number
-- Input:3 INT representing the row, index and number respectively and the Sudoku board
-- Output: Boolean
rowCheck :: Int->Int->Int->Sudoku->Bool
rowCheck row num count []= True
rowCheck row num count ((a,b):xs) 
                                  |  count `div`9 == row && b /= '.' && digitToInt (b) == num = False
                                  | otherwise = rowCheck row num (count +1) xs
                                
-- This function will check the column condition of the input number
-- Input:3 INT representing the column, number and index respectively and the Sudoku board
-- Output: Boolean
columnCheck :: Int->Int->Int->Sudoku->Bool
columnCheck col num count []= True
columnCheck col num count ((a,b):xs) 
                                  |  count `mod`9 == col && b /= '.' && digitToInt (b) == num = False
                                  | otherwise = columnCheck col num (count +1) xs
-- This function will check the block/box condition of the input number
-- Input:2 INT representing the block/box number and number respectively and the Sudoku board
-- Output: Boolean
boxCheck :: Int->Int->Sudoku->Bool
boxCheck boxNum num  []= True
boxCheck boxNum num  ((a,b):xs) 
                                  |  a==boxNum &&  b /= '.' && digitToInt (b) == num = False
                                  | otherwise =  boxCheck boxNum num  xs



-- This function will check the sudoku jigsaw board is completed
-- Input:Sudoku Board 
-- Output: Boolean
completeBoard:: Sudoku->Bool
completeBoard jigsaw= sum [1 | (a,b)<-jigsaw, b/='.'] == 81
                                

-- This function will generate possible number candidates that can fit in the same block
-- Input:Int representing the index and Sudoku Board
-- Output: [Int] with candidates
boxCandidate:: Int->Sudoku->[Int]
boxCandidate index jigsaw=[ digitToInt b| (a,b)<-jigsaw, a==fst(jigsaw!!index), b/='.']

-- This function will generate possible number candidates that can fit in the same row
-- Input:Int representing the index and Sudoku Board
-- Output: [Int] with candidates
rowCandidate::Int->Sudoku->[Int]
rowCandidate index jigsaw=[  digitToInt b | (i,(a,b)) <- zip [0..] jigsaw ,  i `div`9==index `div` 9 , b/='.']


-- This function will generate possible number candidates that can fit in the same column
-- Input:Int representing the index and Sudoku Board
-- Output: [Int] with candidates
columnCandidate::Int->Sudoku->[Int]
columnCandidate index jigsaw=[  digitToInt b | (i,(a,b)) <- zip [0..] jigsaw , i`mod`9==index `mod`9, b /='.' ]

-- This function will generate possible solutions
-- Input:Int representing the index and Sudoku Board
-- Output: [Int] with solutions
solutions:: Int-> Sudoku->[Int]
solutions index jigsaw 
                        | index > length jigsaw = []
                        | snd (jigsaw!!index) =='.' = filter (\z-> not (z `elem` removeDuplicate (columnCandidate index jigsaw ++ rowCandidate index jigsaw ++ boxCandidate index jigsaw ))) [1..9] 
                        | otherwise     = [digitToInt (snd(jigsaw!!index))]


-- This function will solve the sudoku board if possible
-- Input:Sudoku Board
-- Output: Solved Sudoku Board if possible                   
solver:: Sudoku -> Sudoku
solver jigsaw=  solve 0 jigsaw (solutions 0 jigsaw)
-- This is a helper function which will solve the sudoku board
-- Input: Int representing the index, sudoku board and [Int] contaning possible solutions to try with
-- Output: Sudoku Board updated
solve:: Int->Sudoku->[Int]-> Sudoku
solve 80 jigsaw [] = []
solve 80 jigsaw (x:[])= updateJigsaw 80 jigsaw x
solve 80 jigsaw (x:_)=[]
solve _ jigsaw [] = []
solve index jigsaw (x:xs) 
                        | solvedNext == [] = solve index jigsaw xs
                        | otherwise = solvedNext
                        where solveNext index jigsaw= solve (nextCandidate index jigsaw) jigsaw (solutions (nextCandidate index jigsaw) jigsaw)
                              solvedNext=solveNext index (updateJigsaw index jigsaw x)
-- This is a helper function to generate the new sudoku board
-- Input:Int representing the index, sudoku board and Int representing the number
-- Output: Updated Sudoku Board
updateJigsaw:: Int->Sudoku->Int->Sudoku
updateJigsaw index jigsaw x= take index jigsaw ++ [(fst(jigsaw!!index),intToDigit x)]++ drop (index +1) jigsaw
-- This is a helper function to find the next empty cell
-- Input:Int representing the index and sudoku board
-- Output:Int representing the index
nextCandidate:: Int->Sudoku->Int
nextCandidate index jigsaw| index==80 =80
                      | snd(jigsaw!!(index+1))=='.' = index +1
                      | otherwise = nextCandidate (index+1) jigsaw
-- This is a helper function to remove duplicate elements in list
-- Input:[Int]
-- Output:[Int] with no duplicates
removeDuplicate::[Int]->[Int]
removeDuplicate []=[]
removeDuplicate (y:ys)=y:removeDuplicate (filter ((/=)y)ys)
