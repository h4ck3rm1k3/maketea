{-
 - maketea -- generate C++ AST infrastructure
 - (C) 2006-2007 Edsko de Vries and John Gilbert
 - License: GNU General Public License 2.0
 -}

module ContextResolution where

import Control.Monad
import Data.List

import DataStructures
import MakeTeaMonad
import Util
import PrettyPrinter

{-
 - Find context finds the context for a symbol in the grammar; if the symbol is
 - never used, no context is known for the symbol and we assume (c,c,Single)
 -}

findContext :: Some Symbol -> MakeTeaMonad Context
findContext s = withContexts $ \cxs -> case (find (\(s',_,_) -> s == s')) cxs of
	Nothing -> return (s,s,Single)
	Just cx -> return cx

{-
 - Find the original contexts for a symbol (that is, the contexts before
 - context resolution
 -}

findOrigContexts :: Some Symbol -> MakeTeaMonad [Context]
findOrigContexts s = withOrigContexts $ \cxs -> case filter (\(s',_,_) -> s == s') cxs of
	[] -> return [(s,s,Single)]
	cxs' -> return cxs'

{-
 - Context resolution
 -}

contextResolution :: MakeTeaMonad () 
contextResolution = do
	init <- withConj (concatMapM initContexts)
	-- We sort the original contexts by the "destination" symbol in the context
	-- Useful for BasicClasses.hs, where we need the "most specific" context
	sorted <- sortOrig init
	setOrigContexts sorted
	-- Before we reduce the contexts, we sort them by the "source" symbol, so 
	-- that we can easily pair-wise compare them
	reduced <- reduce $ sortBy (\(s,_,_) (s',_,_) -> compare s s') init
	setContexts reduced

sortOrig :: [Context] -> MakeTeaMonad [Context]
sortOrig cxs = do
	top <- withTopological $ return . reverse
	let ord = [(s,n) | s <- top | n <- [1..]]
	return $ sortBy (\(_,s,_) (_,s',_) -> compare (lookup' s ord) (lookup' s' ord)) cxs

{-
 - reduce does context resolution, reducing two contexts (i,t1,m1) and
 - (i,t2,m2) to a context (i,t,m), where t is the greatest lower bound
 - of t1 and t2 in the inheritance hierarchy, and m is the meet of m1
 - and m2, where the meet of two multiplicities is defined separately,
 - below.
 -}

reduce :: [Context] -> MakeTeaMonad [Context]
reduce [] = return []
reduce [c] = return [c]
reduce ((i1,t1,m1):(i2,t2,m2):cs)
	| i1 == i2 = do
		c <- resolve (i1,t1,m1) (i2,t2,m2)
		reduce (c:cs)
	| otherwise = do
		cs' <- reduce ((i2,t2,m2):cs)
		return ((i1,t1,m1) : cs')

resolve :: Context -> Context -> MakeTeaMonad Context
resolve (i,t1,m1) (_,t2,m2) = do
	t <- elim3 classMeet i t1 t2
	return (i,t,multMeet m1 m2)

{-
 - When a symbol X is used in two contexts Y and Z (with X <: Y and X <: Z),
 - then we want to find the most general class A such that X <: A and 
 - A <: Y, A <: Z.
 -}

classMeet :: Symbol x -> Symbol y -> Symbol z -> MakeTeaMonad (Some Symbol)
classMeet x y z = do
	super_x <- allSuperclasses [Exists x]
	inst_y  <- allInstances y
	inst_z  <- allInstances z
	let common_inst = inst_y `intersect` inst_z 
	    meet        = common_inst `intersect` super_x
	return (head meet) -- we want the most general

{-
 - The meet m of two multiplicities is defined such that if we have two
 - contexts (i,t,m1) and (i,t,m2), m is the most permissive multiplicity
 - that is safe in a context where m1 is required and in a context where
 - m2 is required.
 -}

multMeet :: Multiplicity -> Multiplicity -> Multiplicity
multMeet m1 m2 | m1 == m2 = m1
multMeet Single _ = Single
multMeet Optional Vector = Single
multMeet Optional VectorOpt = Single 
multMeet Optional OptVector = Single 
multMeet Vector VectorOpt = Vector
multMeet Vector OptVector = Vector
multMeet VectorOpt OptVector = Vector
multMeet m1 m2 = multMeet m2 m1 -- multMeet is commutative 

{-
 - initContexts traverses the grammar once. It skips all disjunctions,
 - but traverses the body of each conjunction h ::= ... ; for every term
 - (t,m) it finds in the body of the conjunction, it adds a context
 - (i,t,m), for every instance i of t (see GrammarAnalysis for a
 - definition of "instance of").
 -}

initContexts :: Rule Conj -> MakeTeaMonad [Context]
initContexts (Conj h ts) = concatMapM (elim f) ts
	where
		f :: Term a -> MakeTeaMonad [Context]
		f (Term _ t m) = do 
		 	is <- elim allInstances t	
			return (map (\i -> (i,t,m)) is)
		f (Marker _ _) = return []
