import qualified Data.ByteString.Char8 as BS


main :: IO ()
main = do
  n <- BS.getLine
  if n /= BS.pack "42" then do BS.putStrLn n; main else return ()
