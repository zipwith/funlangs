> import Data.Char
> import System.Random

Play a simple guessing game, which starts with
generating a (random) secret number:

> game :: IO ()
> game  = do secret <- randomRIO (1, 100::Int)
>            ask secret

Ask the player to guess the secret number:

> ask       :: Int -> IO ()
> ask secret = do putStr "Please guess the number? "
>                 l <- getLine
>                 case atoi l of
>                    Just guess -> check secret guess
>                    Nothing    -> do putStr "Haha, please enter a number\n"
>                                     ask secret

Test to see if the player's guess can be treated as
an integer value:

> atoi                                  :: String -> Maybe Int
> atoi s | not (null s) && all isDigit s = Just (read s)
>        | otherwise                     = Nothing

Check the player's guess against the secret, and give
a hint if the guess was not correct:

> check :: Int -> Int -> IO ()
> check secret guess
>   | cheat guess     = do putStr ("The secret is " ++ show secret ++ "\n")
>                          ask secret
>   | secret == guess = putStr "You won!!!!"
>   | secret < guess  = do putStr "That's too big\n"
>                          ask secret
>   | secret > guess  = do putStr "That's too small\n"
>                          ask secret

Check for a "cheat code":

> cheat  :: Int -> Bool
> cheat n = n==457 || n==557

