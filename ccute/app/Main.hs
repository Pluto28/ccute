module Main where

import GHC.IO.Exception (ExitCode)
import Options.Applicative
import System.Directory
import System.FilePath
import System.Process
import Lexer


-- TODO: go back to https://www.prborges.com/2023/introduction-to-optparse-applicative/ and
-- reread it
data CompOpts = CompOpts
  { filename :: !String,
    lexer :: !Bool,
    parse :: !Bool,
    codegen :: !Bool,
    asm :: !Bool
  }
  deriving (Show)

arrangerOpts :: IO CompOpts
arrangerOpts = execParser $ info (helper <*> compOptsParser) mempty

compOptsParser :: Parser CompOpts
compOptsParser =
  CompOpts
    <$> filenameParser
    <*> lexerParser
    <*> parseParser
    <*> codegenParser
    <*> asmParser

filenameParser :: Parser String
filenameParser =
  strArgument $
    metavar
      "FILE"
      <> help "Name of c source file to compile"

lexerParser :: Parser Bool
lexerParser =
  switch $
    long "lex"
      <> help "Runs the lexer, stops before parsing"

parseParser :: Parser Bool
parseParser =
  switch $
    long "parse"
      <> help "Runs the lexer and parser, stops before assembly generation"

codegenParser :: Parser Bool
codegenParser =
  switch $
    long "codegen"
      <> help "Performs lexing, parsing and assembly generation, stops before code emission"

asmParser :: Parser Bool
asmParser =
  switch $
    short 'S'
      <> help "Emmits an assembly file"

-- modifier :: Mod FlagFields Bool
-- modifier =

main :: IO CompOpts
main =
  do
    args <- arrangerOpts
    
    let CompOpts {filename, lexer, parse, codegen, asm} = args
    -- TODO: when we write the lexer and parser, codegen functions, then implement this
    filedata <- readFile filename
    --lexData <- Lexer.dismemberString filedata
    --data <- case (lexer, parse, codegen, asm) of
    --  (True, False, False, False) -> lexer filedata
      --(False, False, False, True) -> preprocess filename >> assembly filename
    print (lexData (dismemberString filedata 1))
    print filedata
    return args

-- _ <- preprocess filename
-- _ <- assembly filename
-- _ <- assemble filename
-- opts <- arrangerOpts
-- print opts
-- return opts

-- optsExec :: IO CompOpts
-- optsExec CompOpts {filename, lexer, parse, codegen}
--  | lexer == True = preprocess filename
--  | lexer ==  True = assembly filename
--  | lexer ==  True = assemble filename
--  | otherwise = do
--      _ <- preprocess filename
--      _ <- assemble filename
--      preprocess filename

preprocess :: String -> IO ExitCode
preprocess filename =
  let output = replaceExt filename ".i"
   in do
        (_, _, _, procHndl) <-
          createProcess
            ( proc
                "gcc"
                ["-E", "-P", filename, "-o", output]
            )
        waitForProcess procHndl

assembly :: String -> IO ExitCode
assembly filename =
  let source = replaceExt filename ".i"
   in let output = replaceExt filename ".s"
       in do
            (_, _, _, procHndl) <-
              createProcess
                ( proc
                    "gcc"
                    ["-S", source, "-o", output]
                )
            exit <- waitForProcess procHndl
            removeFile source
            return exit

assemble :: String -> IO ExitCode
assemble filename =
  let source = replaceExt filename ".s"
   in let output = replaceExt filename ""
       in do
            (_, _, _, procHndl) <-
              createProcess
                ( proc
                    "gcc"
                    [source, "-o", output]
                )
            exit <- waitForProcess procHndl
            removeFile source
            return exit

replaceExt :: String -> String -> String
replaceExt filename newext = noExt ++ newext
  where
    (noExt, _) = splitExtension filename
