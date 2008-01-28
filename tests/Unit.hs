
import Text.JSON
import Network.RPC.JSON
import System.Exit

main = do
    case (decode test1 :: Result JSRequest) of
         Ok _ -> exitWith ExitSuccess


test1 = 
    "{\"method\":\"feed.add\",\"params\":{\"uri\":\"http://rss.slashdot.org/Slashdot/slashdot\"},\"version\":\"1.1\" }"
