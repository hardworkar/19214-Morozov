import qualified Data.ByteString
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Base16 as Base16
import Data.ByteString.UTF8 as BSU
import Data.List
import Control.Monad

hash :: String -> ByteString
hash x = Base16.encode . MD5.hash . BSU.fromString $ x


search :: ByteString -> Maybe String
search nemo = find (\x -> hash x == nemo) taskList

taskList = forM [1..1] (\x-> [' '..'~']) ++ forM [1..2] (\x-> [' '..'~']) ++ forM [1..3] (\x-> [' '..'~']) ++ forM [1..4] (\x-> [' '..'~']) ++ forM [1..5] (\x-> [' '..'~'])

main = do
	input <- getLine
	let magic = BSU.fromString input
	putStrLn $ "hash: " ++ input
	print . search $ magic