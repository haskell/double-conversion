import Data.Double.Conversion.Convertable
import Data.Text ()
import System.Environment
import System.Exit
import System.IO

main = do
  args <- getArgs
  case args of
    ("shortest":xs) -> print (map (toShortest . read) xs)
    ("fixed":n:xs) -> print (map (toFixed (read n) . read) xs)
    ("exponential":n:xs) -> print (map (toExponential (read n) . read) xs)
    ("precision":n:xs) -> print (map (toPrecision (read n) . read) xs)
    _ -> do
         hPutStrLn stderr "oops! no mode specified!"
         exitWith (ExitFailure 1)
