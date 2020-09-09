\section{Lifting}

This module defines functions for lifting values into Template Haskell typed
expressions.

\begin{code}
{-# LANGUAGE TemplateHaskell #-}

module Lift where
\end{code}

The ``goal'' function is one that works on the output of the parser and returns
an expression representing a lambda taking an |XConfig| and returning the
key map; with a type signature

\begin{spec}
goal  ::  SM     (Set (Maybe KeyMask), KeySym) KeyAction
      ->  TExpQ  (XConfig Layout -> Map (KeyMask, KeySym) (X ()))
\end{spec}

This function will be broken up into smaller ``steps'':

\begin{enumerate}
  \item Lift the key actions to expressions representing |X| actions.
  \item Lift the key combinations to expressions representing pairs of masks and
    symbols.
  \item Lift the |ESM|s to expressions representing single |X|
    actions, taking right values directly and merging left ones with
    |submap|.
  \item Lift the |SM|s to expressions representing plain |Map|s.
  \item Finally, splice resulting expression into one with the |XConfig|
    lambda.
\end{enumerate}

\subsection{Imports}

For the |Map| and |Set| data structures:

\begin{code}
import            Data.Map     (Map)
import qualified  Data.Map as  M
import            Data.Set     (Set)
import qualified  Data.Set as  S
\end{code}

For Template Haskell types and functions:

\begin{code}
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax
\end{code}

For working with |KeyMask|s and |KeySym|s:

\begin{code}
import XMonad           (KeyMask, KeySym, (.|.))
import Foreign.C.Types  (CUInt)
\end{code}

For working with |XConfig|:

\begin{code}
import XMonad (Layout, XConfig (XConfig, modMask))
\end{code}

For working with |KeyAction|s:

\begin{code}
import XMonad                 (X, spawn)
import XMonad.Actions.Submap  (submap)
\end{code}

Finally, custom types:

\begin{code}
import Types
\end{code}

\subsection{Lifting key actions}

\begin{code}
liftKeyAction :: KeyAction -> TExpQ (X ())
liftKeyAction (ActionSpawn x) = [|| spawn x ||]
\end{code}

\subsection{Lifting key combinations}

Before key masks can be lifted, a |Lift| instance for its ``root'' type,
|CUInt|, must be declared:

\begin{code}
instance Lift CUInt where lift = litE . integerL . toInteger
\end{code}

Compared to the rest, this function takes an extra argument, |varName :: Name|.
This represents the value of |modMask|.

\begin{code}
liftKeyCombo :: Name -> (Set (Maybe KeyMask), KeySym) -> TExpQ (KeyMask, KeySym)
liftKeyCombo varName (set, sym)  =
  let  knownMasks     = foldr (\mask rest -> maybe rest (.|. rest) mask) 0 set
       varExpression  = TExp <$> varE varName
  in   if  Nothing `elem` set
           then  [|| (knownMasks .|. $$(varExpression), sym)  ||]
           else  [|| (knownMasks, sym)                        ||]
\end{code}

\subsection{Lifting |ESM|s}

Right values can be returned as-is, but left values require more work:

\begin{enumerate}
  \item Recurse on child maps.
  \item Convert the map to a (sorted) list of pairs.
  \item Lift each pair in the list.
  \item Lift the whole list.
  \item Splice the list's expression into one turning it back into a map.
  \item Splice the map's expression into one calling |submap| on it.
\end{enumerate}

\begin{code}
liftESM :: ESM (TExpQ (KeyMask, KeySym)) (TExpQ (X ())) -> TExpQ  (X ())
liftESM (RSM x)            =   x
liftESM (LSM (SM rawMap))  =
  let  rightMap         =  fmap liftESM rawMap
       rightList        =  M.toAscList rightMap
       rightList'       =  fmap   tupToExp rightList
                           where  tupToExp (comboExp, actionExp) =
                                    [|| ($$(comboExp), $$(actionExp)) ||]
       rightExpression  =  fmap TExp $ listE $ map (fmap unType) rightList'
       mapExpression    =  [|| M.fromDistinctAscList $$(rightExpression) ||]
  in   [|| submap $$(mapExpression) ||]
\end{code}

\subsection{Lifting |SM|s}

\begin{enumerate}
  \item Call |liftESM| on child maps.
  \item Convert the map to a (sorted) list of pairs.
  \item Lift each pair in the list.
  \item Lift the whole list.
  \item Splice the list’s expression into one turning it back into a map.
\end{enumerate}

TODO: switch name with |liftSM|? feels like I've been using ``submap'' to refer
more general functions, unlike this.

\begin{code}
liftSubmaps  ::  SM     (TExpQ  (KeyMask, KeySym))  (TExpQ (X ()))
             ->  TExpQ  (Map    (KeyMask, KeySym)   (X ()))
liftSubmaps (SM rawMap) =
  let  flatMap         =  fmap liftESM rawMap
       flatList        =  M.toAscList flatMap
       flatList'       =  fmap   tupToExp flatList
                          where  tupToExp (comboExp, actionExp) =
                                   [|| ($$(comboExp), $$(actionExp)) ||]
       listExpression  =  fmap TExp $ listE $ map (fmap unType) flatList'
       mapExpression   =  [|| M.fromDistinctAscList $$(listExpression) ||]
  in   mapExpression
\end{code}

\subsection{Putting it all together}

\begin{code}
liftSM  ::  SM (Set (Maybe KeyMask), KeySym) KeyAction
        ->  TExpQ (XConfig Layout -> Map (KeyMask, KeySym) (X ()))
liftSM sm = do
  mmVarName     <-  newName "mm"
  let  expressionSM   =  smMapKeysMonotonic (liftKeyCombo mmVarName) $ fmap liftKeyAction sm
       mmVarPattern   =  [p| XConfig { modMask = $(varP mmVarName) } |]
       mapExpression  =  liftSubmaps expressionSM
  fmap TExp $ lamE [mmVarPattern] $ fmap unType mapExpression
\end{code}

Primary export of this module.

\begin{code}
liftKeymap :: SM (Set (Maybe KeyMask), KeySym) KeyAction -> ExpQ
liftKeymap = fmap unType . liftSM
\end{code}
