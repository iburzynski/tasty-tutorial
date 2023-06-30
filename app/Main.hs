module Main where

import Control.Monad.Catch (Handler (..), MonadThrow (throwM), catches)
import IPTypes (InvalidArgsException (..))
import LookupIP (reportIP)
import Options.Applicative (Parser, ParserInfo, argument, execParser, fullDesc, help, helper, info, metavar, progDesc, str)
import ParseIP (parseIP, parseIPRanges)
import System.Exit (ExitCode)

data Params = Params FilePath String

mkParams :: Parser Params
mkParams =
  Params
    <$> argument str (metavar "FILE" <> help "IP range database")
    <*> argument str (metavar "IP" <> help "IP address to check")

run :: Params -> IO ()
run (Params fp ipStr) = do
  ipRanges <- parseIPRanges <$> readFile fp
  case (ipRanges, parseIP ipStr) of
    (_, Nothing) -> throwM (InvalidIP ipStr)
    (Left parseErr, _) -> throwM (LoadIPRangesError parseErr)
    (Right iprDB, Just ip) -> putStrLn (reportIP iprDB ip)

main :: IO ()
main = do
  params <- execParser opts
  run params `catches` [Handler parserExit]
  where
    parserExit :: ExitCode -> IO ()
    parserExit _ = pure ()
    opts :: ParserInfo Params
    opts =
      info
        (helper <*> mkParams)
        ( fullDesc
            <> progDesc
              "Answers YES/NO depending on whether an IP address belongs to the IP range database"
        )