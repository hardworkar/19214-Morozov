import Control.Concurrent
import Control.Monad
import Control.DeepSeq

import qualified Data.List as L
import qualified Data.ByteString
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Base16 as Base16
import Data.ByteString.UTF8 as BSU

taskList = forM [1..1] (\x-> [' '..'~']) ++ forM [1..2] (\x-> [' '..'~']) ++ forM [1..3] (\x-> [' '..'~']) ++ forM [1..4] (\x-> [' '..'~']) ++ forM [1..5] (\x-> [' '..'~'])

hash :: String -> ByteString
hash x = Base16.encode . MD5.hash . BSU.fromString $ x

workerLoop :: MVar [String] -> MVar String -> ByteString -> MVar Int -> Int -> IO ()
workerLoop taskQueue result magic cntCompleted all = do
  maybeTasks <- modifyMVar taskQueue
                 (\q -> return $ case q of
                                   [] -> ([], [])
                                   xs -> (L.drop 30 xs, L.take 30 xs))

  case maybeTasks of
    [] -> do putStrLn "process ended";
    tasks -> do
    	case L.find (\x -> hash x == magic) tasks of
			Nothing -> do
				workerLoop taskQueue result	magic cntCompleted all
			Just t -> do
					t `deepseq` putMVar result t
					return ()
    				


main = do
  --input <- getLine
  --let x = hash $ input
  let x = BSU.fromString "ed790e134796eb704dd092dde146a792"
  workerNumber <- getNumCapabilities
  putStrLn $ "Workers: " ++ show workerNumber
  taskQueue <- newMVar taskList
  result <- newEmptyMVar
  putStrLn $ "md5: " ++ BSU.toString x ++ "\n" ++ "searching..."
  cntCompleted <- newMVar 0
  let all = L.length taskList
  workerNumber `replicateM_` forkIO (workerLoop taskQueue result x cntCompleted all)


  out <- modifyMVar result (\q -> return ("", q))
  putStrLn $ "result: " ++ out