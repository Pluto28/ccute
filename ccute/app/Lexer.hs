module Lexer where

import Data.Array
import Data.Maybe
import Text.Regex.TDFA

-- TODO: When we become more proficient at handling errors, come back here and
-- do so instead of just throwing errors everywhere. We don't even know the file
-- that is being lexed, for god's sake

data Keywords
  = Int
  | Void
  | Return
  deriving (Show)

data Token
  = Identifier String
  | Constant Integer
  | Keyword Keywords
  | OpenPar
  | ClosePar
  | OpenBrace
  | CloseBrace
  | Semicolon
  deriving (Show)

matchRegex :: Regex
matchRegex =
  makeRegexOpts
    defaultCompOpt {newSyntax = True, multiline = True}
    defaultExecOpt
    "[0-9]+\\b\
    \|[a-zA-Z_][a-zA-Z0-9_]*\\b\
    \|[{}();]\
    \|\n\
    \|/[*/]"

dismemberString :: String -> Integer -> [String]
dismemberString [] _ = []
dismemberString program line
  | length beforeSrc /= 0 =
      error
        ( (show line)
            ++ ":"
            ++ (show matchOffset)
            ++ ": Possible malformed identifier"
        )
  -- This is pretty bad, gotta come back and handle this the correct manner afterwards
  | tok == "/*" =
      dismemberString
        newProgram
        (line + commentLin)
  | tok == "//" =
      dismemberString
        (ignoreLine afterSrc)
        (line + 1)
  | tok == "\n" =
      dismemberString
        afterSrc
        (line + 1)
  | otherwise = tok : dismemberString afterSrc line
  where
    (beforeSrc, val, afterSrc) =
      fromJust
        ( matchOnceText
            matchRegex
            (trimSpace program)
        )
    (tok, (matchOffset, _)) = val ! 0
    (newProgram, commentLin) = spanningHandle program

spanningHandle :: String -> (String, Integer)
spanningHandle source = (afterSource, (lineCount notMatch))
  where
    myReg = (makeRegexOpts defaultCompOpt {multiline = False} defaultExecOpt "\\*/") :: Regex
    (notMatch, afterSource) =
      case (matchOnceText myReg (trimSpace source)) of
        Nothing -> error "Possible unfinished comment"
        Just (ignoredSource, _, remainingSource) ->
          (ignoredSource, remainingSource)

-- (notMatch, _, afterSource) =
--  fromJust
--    ( matchOnceText
--        myReg
--        (trimSpace source)
--    )

lineCount :: String -> Integer
lineCount [] = 0
lineCount (x : xs)
  | x == '\n' = 1 + lineCount xs
  | otherwise = lineCount xs

ignoreLine :: String -> String
ignoreLine [] = []
ignoreLine (x : xs)
  | x /= '\n' = ignoreLine xs
  | otherwise = xs

-- spanCommentHandler :: String -> (String, Integer)
-- spanCommentHandler program skipped =

trimSpace :: String -> String
trimSpace [] = []
trimSpace (x : xs)
  | x == ' ' || x == '\t' = trimSpace xs
  | otherwise = x : xs

lexData :: [String] -> [Token]
lexData [] = []
lexData (tkn : tokens)
  | tkn == "int" = Keyword Int : (lexData tokens)
  | tkn == "void" = Keyword Void : (lexData tokens)
  | tkn == "return" = Keyword Return : (lexData tokens)
  | tkn == "(" = OpenPar : (lexData tokens)
  | tkn == ")" = ClosePar : (lexData tokens)
  | tkn == "{" = OpenBrace : (lexData tokens)
  | tkn == "}" = CloseBrace : (lexData tokens)
  | tkn == ";" = Semicolon : (lexData tokens)
  | isInteger tkn = Constant (read tkn :: Integer) : (lexData tokens)
  | otherwise = Identifier tkn : (lexData tokens)

-- An identifier is malformed if there is a constant followed by an identifier
-- isIdentifierMalformed :: String ->

isInteger :: String -> Bool
isInteger val = val =~ "[0-9]+"
