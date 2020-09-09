\section{Utilities}

Utility functions to be exported by the main module alongside the quasiquoter.

\begin{code}
module Utilities where

import Data.Map (Map)
import XMonad
\end{code}

Roughly equivalent to |XMonad.Util.EZConfig.addtionalKeys| except that the
argument here is the same type as |keys| from |XConfig|.

\begin{code}
additionalKeys  ::     XConfig l
                ->  (  XConfig Layout -> Map (KeyMask, KeySym) (X ()))
                ->     XConfig l
addtionalKeys xc newKeys = xc { keys = newKeys <+> keys xc }
\end{code}

The same function, with the arguments flipped. Fits better with functions like
|withUrgencyHook|, etc.

\begin{code}
withAdditionalKeys = flip addtionalKeys
\end{code}
