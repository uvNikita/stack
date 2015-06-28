{-# LANGUAGE OverloadedStrings #-}
-- | Parsing argument-like things.

module Data.Attoparsec.Args (EscapingMode(..), argsParser, withRunStackArgs) where

import           Control.Applicative
import           Data.Attoparsec.Text ((<?>))
import qualified Data.Attoparsec.Text as P
import           Data.Attoparsec.Types (Parser)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8')
import           System.Directory (doesFileExist)
import           System.Environment (getArgs, withArgs)
import           System.IO (IOMode (ReadMode), withBinaryFile)

-- | Mode for parsing escape characters.
data EscapingMode
    = Escaping
    | NoEscaping
    deriving (Show,Eq,Enum)

-- | A basic argument parser. It supports space-separated text, and
-- string quotation with identity escaping: \x -> x.
argsParser :: EscapingMode -> Parser Text [String]
argsParser mode = many (P.skipSpace *> (quoted <|> unquoted)) <*
                  P.skipSpace <* (P.endOfInput <?> "unterminated string")
  where
    unquoted = P.many1 naked
    quoted = P.char '"' *> string <* P.char '"'
    string = many (case mode of
                     Escaping -> escaped <|> nonquote
                     NoEscaping -> nonquote)
    escaped = P.char '\\' *> P.anyChar
    nonquote = P.satisfy (not . (=='"'))
    naked = P.satisfy (not . flip elem ("\" " :: String))

withRunStackArgs :: ([String] -> IO a) -> IO a
withRunStackArgs inner = do
    args <- getRunStackArgs
    print args
    withArgs args $ inner args

getRunStackArgs :: IO [String]
getRunStackArgs = do
    args0 <- getArgs
    case args0 of
        [x] -> do
            isFile <- doesFileExist x
            print (args0, x, isFile)
            if isFile
                then do
                    margs <-
                        withBinaryFile x ReadMode $ \h ->
                        CB.sourceHandle h
                            $= CB.lines
                            $= CL.map killCR
                            $$ sinkRunStackArgs
                    return $ fromMaybe args0 margs
                else return args0
        _ -> return args0
  where
    killCR bs
        | S.null bs || S.last bs /= 13 = bs
        | otherwise = S.init bs

sinkRunStackArgs :: Monad m => Sink ByteString m (Maybe [String])
sinkRunStackArgs =
    await >>= maybe (return Nothing) checkShebang
  where
    checkShebang bs
        | "#!" `S.isPrefixOf` bs = fmap (maybe Nothing parseArgs) await
        | otherwise = return (parseArgs bs)

    parseArgs bs =
        case decodeUtf8' bs of
            Left _ -> Nothing
            Right t ->
                case P.parseOnly (argsParser Escaping) t of
                    Right ("--":"stack":rest) -> Just rest
                    _ -> Nothing
