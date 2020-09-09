\section{Parsing}

Parsers used for producing data structres from input to the QuasiQuoter.

The syntax here is more similar to the ones found in vis or lf than
|additionalKeysP|.

TODO: better error messages for just about everything here.

\begin{code}
module Parsers where

import Control.Monad
import Control.Applicative
import Text.ParserCombinators.Parsec hiding ((<|>), many, optional)

import            Data.List
import            Data.List.NonEmpty  (NonEmpty, some1)
import qualified  Data.List.NonEmpty  as NE
import            Data.Set            (Set)
import qualified  Data.Set            as S

import XMonad

import Types
\end{code}

\subsection{Utilities}

\begin{code}
someOf      = some   . oneOf
tryMany     = many   . try
trySome     = some1  . try
testAhead  = \p -> lookAhead (True <$ p) <|> pure False
\end{code}

\subsection{Parsing regular keys}

Some keys are allowed to specified literally, i.e., by the actual character
instead of the symbolic name.

\begin{code}
allowedLiterally = ['!'..'~'] \\ "<->"
\end{code}

Parser for a single, non-whitespace, ASCII character.

\begin{code}
asciiKeysym  ::  CharParser st KeySym
asciiKeysym  =   choice xs <?> "an ASCII character"
  where xs = zipWith (\c s -> s <$ char c) ['!'..'~'] [xK_exclam..xK_asciitilde]
\end{code}

Parser for known symbolic key name.

\begin{code}
keysymName  ::  CharParser st KeySym
keysymName  =   do
  str <- someOf allowedLiterally
  let sym = stringToKeysym str
  if sym == noSymbol  then fail $ "Invalid key symbol name \"" ++ str ++ "\""
                      else return sym
\end{code}

Parser that tries to read a symbolic name, or ASCII character if that fails.

\begin{code}
keysym'  ::  CharParser st KeySym
keysym'  =   try keysymName <|> asciiKeysym
\end{code}

Same as |keysym'| except the names are surrounded with angled brackets.

TODO: this |isSpecial| pattern is ugly.

\begin{code}
keysym  ::  CharParser st KeySym
keysym  =   do
  isSpecial <- testAhead (char '<')
  if isSpecial  then  char '<' *> keysymName <* char '>'
                else  asciiKeysym 
\end{code}

\subsection{Parsing modifier keys}

Parser that reads an abbreviation for a modifier key and returns its mask.

\begin{code}
keymask'  ::  CharParser st KeyMask
keymask'  =   choice xs
  where xs = zipWith (\s m -> m <$ try (string s))
              ["S", "C", "M1", "M2", "M3", "M4", "M5"]
              [shiftMask, controlMask, mod1Mask, mod2Mask, mod3Mask, mod4Mask, mod5Mask]
\end{code}

Same as |keymask'| but also accepts |"M"| for |modMask|, represented as
|Nothing|.

\begin{code}
keymask  ::  CharParser st (Maybe KeyMask)
keymask  =   Just <$> keymask' <|> Nothing <$ char 'M'
\end{code}

\subsection{Parsing key combinations}

Parser that either reads a single |asciiKeysym|, or many (or no) hypen-seperated
|keymask|s and a |keysym'| between angled brackets.

\begin{code}
keyCombo  ::  CharParser st (Set (Maybe KeyMask), KeySym)
keyCombo  =   do
  isSpecial <- testAhead (char '<')
  if isSpecial  then  char '<' *> pure withMasks <*> tryMany (keymask <* char '-') <*> keysym' <* char '>'
                else  noMasks <$> asciiKeysym
                where  noMasks    = \s    -> (mempty,        s)
                       withMasks  = \m s  -> (S.fromList m,  s)
\end{code}

\subsection{Parsing actions}

Parser that reads a |spawn| action.

\begin{code}
keyActionSpawn  ::  CharParser st KeyAction
keyActionSpawn  =   char '$' *>
  (ActionSpawn <$> ((:)  <$> (noneOf "|" <?> "a command")
                         <*> (manyTill anyChar $ lookAhead $ void (char '|') <|> eof)))
\end{code}

Parser that reads any action.

\begin{code}
keyAction  ::  CharParser st KeyAction
keyAction  =   keyActionSpawn
\end{code}

\subsection{Parsing key bindings}

Parser that reads a sequence space-sperated of key combinations followed by an
action.

\begin{code}
keybind  ::  CharParser st (SM (Set (Maybe KeyMask), KeySym) KeyAction)
keybind  =   toSM <$> trySome (keyCombo <* some space) <*> keyAction
\end{code}

Parser that reads a list of |keybind|s, seperated by pipes (i.e., @|@).

\begin{code}
keybinds  ::  CharParser st (SM (Set (Maybe KeyMask), KeySym) KeyAction)
keybinds  =   spaces *> pure mconcat <*> keybind `sepBy1` (char '|' *> spaces) <* spaces
\end{code}

\subsection{Putting it all together}

Wraps |keybinds| with |parse|. Intended to the be main export to other modules
(so they do not also need to import Parsec).

\begin{code}
parseKeymap  ::  String -> Either ParseError (SM (Set (Maybe KeyMask), KeySym) KeyAction)
parseKeymap  =   parse keybinds ""
\end{code}
