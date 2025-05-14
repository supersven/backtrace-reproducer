module Main where

import Server

main :: IO ()
main = do
  putStrLn "Listening on http://localhost:8085"
  runServer
