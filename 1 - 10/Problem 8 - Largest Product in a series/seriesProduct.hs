import System.Environment  
import System.IO
import System.IO.Error
import Control.Exception
import System.Directory
import Data.Char
import Debug.Trace

main = parseAndStart `catch` onError

parseAndStart :: IO ()
parseAndStart = do (lengthStr:fileName:_) <- getArgs
                   fileExists <- doesFileExist fileName                    
                   if fileExists
                       then do contents <- readFile fileName
                               print (findProduct (maybeRead lengthStr) contents)
                       else do putStrLn "The file doesn't exist!"
                   
onError :: IOError -> IO ()
onError e  
    | isDoesNotExistError e = putStrLn "The file doesn't exist!"  
    | otherwise             = putStrLn "Error reading command line arguements. This program needs a product length and a file path to function properly."
   
maybeRead :: String -> Maybe Int
maybeRead n = case reads n of
                [(x,_)] -> Just x
                _       -> Nothing

findProduct :: Maybe Int -> [Char] -> String
findProduct Nothing _              = "The product length was incorrectly entered. Please enter an integer value"
findProduct (Just length) contents
                                = let digitList       = map (digitToInt) (filter (/='\n')contents)
                                      (firstList, xs) = splitAt length digitList
                                      productList     = reverse firstList
                                      firstProduct    = foldr(\x acc -> acc * (x)) 1 productList :: Int
                                  in  show (productIterate (firstProduct,[]) productList xs)
                                      where productIterate (max,ls) cl@(p:ps)  []    =  let cur = foldr(\x acc -> acc * x) 1 cl
                                                                                        in if (max > cur) then (max,ls) else (cur,cl)
                                            productIterate (max,ls) cl@(p:ps) (z:zs) =  let cur = foldr(\x acc -> acc * x) 1 cl
                                                                                        in trace(show (cur,cl)) $if(cur > max)
                                                                                                   then productIterate (cur,cl) (ps ++ [z]) zs
                                                                                                   else productIterate (max,ls) (ps ++ [z]) zs