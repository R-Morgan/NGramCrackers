\usepackage{listings}
\lstloadlanguages{Haskell}
\lstnewenvironment{code}
    {\lstset{}%
      \csname lst@SetFirstLabel\endcsname}
    {\csname lst@SaveFirstLabel\endcsname}
    \lstset{
      basicstyle=\small\ttfamily,
      flexiblecolumns=false,
      basewidth={0.5em,0.45em},
      literate={+}{{$+$}}1 {/}{{$/$}}1 {*}{{$*$}}1 {=}{{$=$}}1
               {>}{{$>$}}1 {<}{{$<$}}1 {\\}{{$\lambda$}}1
               {\\\\}{{\char`\\\char`\\}}1
               {->}{{$\rightarrow$}}2 {>=}{{$\geq$}}2 {<-}{{$\leftarrow$}}2
               {<=}{{$\leq$}}2 {=>}{{$\Rightarrow$}}2 
               {\ .}{{$\circ$}}2 {\ .\ }{{$\circ$}}2
               {>>}{{>>}}2 {>>=}{{>>=}}2
               {|}{{$\mid$}}1               
    }

\begin{code}
{-# LANGUAGE OverloadedStrings #-}


module NGramCrackers.Parsers.Paragraph
( parseSent
, parseParagraph
, parseMultiPara
, wordString
) where

\end{code}

\begin{code}
import Control.Applicative ((<$>), (<*), (*>), (<*>), (<|>), liftA3)
import Data.Functor (void)
import Data.List (concat, unwords)
import Data.Text as T                    
import Text.Parsec.Text as PT 
import Text.ParserCombinators.Parsec hiding ((<|>))
import NGramCrackers.DataTypes

\end{code}

\begin{code}
parseSent :: T.Text -> Either ParseError [T.Text]
parseSent = parse sentence "unknown"

\end{code}

\begin{code}
parseParagraph :: T.Text -> Either ParseError [[T.Text]]
parseParagraph = parse paragraph "unknown"

\end{code} 

\begin{code}
parseMultiPara :: T.Text ->  Either ParseError [[[T.Text]]]
parseMultiPara = parse docBody "unknown"

\end{code} 

\begin{code}
docMetadata :: [MetaTag]
docMetadata = undefined

\end{code} 

\begin{code}
docBody :: PT.Parser [[[T.Text]]]
docBody = endBy paragraph eop

\end{code} 

\begin{code}
paragraph :: PT.Parser [[T.Text]]
paragraph = endBy sentence eos

\end{code} 

\begin{code}
sentence :: PT.Parser [T.Text]
sentence = sepBy sentParts seppr

\end{code} 

\begin{code}
sentParts :: PT.Parser T.Text
sentParts = word <|> number

\end{code} 

\begin{code}
wordString :: PT.Parser T.Text
-- Useful for non-sentence word strings where no numbers need to be parsed.
-- Probably useful for parsing MetaTags
wordString = T.unwords <$> sepBy word seppr
\end{code} 

\begin{code}
word :: PT.Parser T.Text
-- The use of T.pack <$> is necessary because of the type many1 letter returns.
-- fmapping T.pack into the Parser makes it possible to return a parser of the
-- appropriate type.
word = T.pack <$> many1 letter 
\end{code} 

\begin{code}
number :: PT.Parser T.Text
number = T.pack <$> many1 digit
\end{code} 
                                                     
\begin{code}
seppr :: PT.Parser ()
-- Since the results of this parser are just thrown away, we need the `void`
-- function from Data.Functor
seppr =  void sepprs <|> void newLn
           where sepprs =    space'
                         <|> (char ',' *> space')
                         <|> (char ';' *> space')
                         <|> (char ':' *> space')
                 newLn  =    many1 (char '\n')
                 space' = char ' '
\end{code} 

\begin{code}
eos :: PT.Parser ()
eos = void sepprs -- <|> void sngls
        where sepprs =    (char '.' <* space')
                      <|> (char '!' <* space')
                      <|> (char '?' <* space')
{-              sngls  =    (char '.')
                      <|> (char '!')
                      <|> (char '?')
-}
              space' = many (char ' ')
\end{code} 

\begin{code}
eop :: PT.Parser ()
eop = void $ 
  char '<' >> many1 letter >> char '>' <* (void space' <|> void newLn)
    where space' = char ' '
          newLn  = many1 (char '\n')
\end{code} 
