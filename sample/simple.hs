
import System.Prefork
import Network.Socket

data ServerResource = ServerResource [Socket]

data ServerOption = ServerOption1 deriving (Show, Read)

main :: IO ()
main = do
  defaultMain defaultSettings { psUpdateConfig = updateConfig } $ \so -> return ()

updateConfig :: IO (Maybe ServerOption)
updateConfig = do
  return (Just ServerOption1)

newServerResource = ServerResource []
