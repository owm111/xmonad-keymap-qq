\section{QuasiQuoters}

Module that defines the QuasiQuoter.

\begin{code}
module QQ where

import Language.Haskell.TH.Quote

import Parsers
import Lift
\end{code}

Quotes a keymap from a string or throws an error if it can't.

\begin{code}
quoteKeymap = either (fail . show) liftKeymap . parseKeymap
\end{code}

The QuasiQuoter. It does not support quoting anything other than expressions.

\begin{code}
keymap  ::  QuasiQuoter
keymap  =   QuasiQuoter
  {  quoteExp   =  quoteKeymap
  ,  quotePat   =  fail "keymap can only quote expressions."
  ,  quoteDec   =  fail "keymap can only quote expressions."
  ,  quoteType  =  fail "keymap can only quote expressions."
  }
\end{code}
