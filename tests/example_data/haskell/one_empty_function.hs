import Prelude (IO, putStr, String, (++))
main :: IO ()
main = putStr fun_a

fun_b :: String
fun_b = ""

fun_a :: String
fun_a = fun_b ++ "C"
