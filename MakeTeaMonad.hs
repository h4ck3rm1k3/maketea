{-
 - maketea -- generate C++ AST infrastructure
 - (C) 2006-2007 Edsko de Vries and John Gilbert
 -}

module MakeTeaMonad where

import Control.Monad.State
import Data.Maybe
import Data.List

import DataStructures
import Util

{-
 - Convenient access to components of the maketea state
 -}

withGrammar :: (Grammar -> MakeTeaMonad a) -> MakeTeaMonad a
withGrammar f = get >>= f . grammar 

withConj :: ([Rule Conj] -> MakeTeaMonad a) -> MakeTeaMonad a
withConj f = withGrammar $ f . catMaybes . map (elim conj) 
	where
		conj :: Rule a -> Maybe (Rule Conj)
		conj r@(Conj _ _) = Just r
		conj (Disj _ _) = Nothing

withDisj :: ([Rule Disj] -> MakeTeaMonad a) -> MakeTeaMonad a
withDisj f = withGrammar $ f . catMaybes . map (elim disj)
	where
		disj :: Rule a -> Maybe (Rule Disj)
		disj (Conj _ _) = Nothing
		disj r@(Disj _ _) = Just r

withTokens :: ([Symbol Terminal] -> MakeTeaMonad a) -> MakeTeaMonad a
withTokens f = withGrammar $ f . allTokens 

withNonMarkers :: ([Term NonMarker] -> MakeTeaMonad a) -> MakeTeaMonad a
withNonMarkers f = withConj $ f . allNonMarkers

withClasses :: ([Class] -> MakeTeaMonad a) -> MakeTeaMonad a
withClasses f = get >>= f . fromJust . classes 

withContexts :: ([Context] -> MakeTeaMonad a) -> MakeTeaMonad a
withContexts f = get >>= f . fromJust . contexts

withPrefix :: (String -> MakeTeaMonad a) -> MakeTeaMonad a
withPrefix f = get >>= f . prefix

withSymbols :: ([Some Symbol] -> MakeTeaMonad a) -> MakeTeaMonad a
withSymbols f = do
	nts <- withGrammar $ return . map (Exists . elim ruleHead)
	ts <- withTokens $ return . (map Exists)
	f (nts ++ ts)

getNextClassID :: MakeTeaMonad Integer
getNextClassID = do
	s <- get
	let cid = nextClassID s
	put (s { nextClassID = cid + 1 })
	return cid

setContexts :: [Context] -> MakeTeaMonad ()
setContexts cxs = do
	s <- get
	put (s { contexts = Just cxs })

setClasses :: [Class] -> MakeTeaMonad ()
setClasses cs = do
	s <- get
	put (s { classes = Just cs })

initState :: String -> Grammar -> MakeTeaState
initState pr gr = MTS {
	  prefix = pr
	, grammar = gr
	, nextClassID = 1 
	, contexts = Nothing
	, classes = Nothing
	}

{-
 - Filtering
 -}

allTerms :: [Rule Conj] -> [Some Term]
allTerms =  concatMap conjBody 

allTokens :: Grammar -> [Symbol Terminal]
allTokens = nub . tokens . concatMap (elim body)
	where
		body :: Rule a -> [Some Symbol]
		body (Disj _ ss) = ss
		body (Conj _ ts) = [s | (Exists (Term _ s _)) <- ts]

tokens :: [Some Symbol] -> [Symbol Terminal]
tokens = catMaybes . map (elim f) 
	where
		f :: Symbol a -> Maybe (Symbol Terminal)
		f t@(Terminal _ _) = Just t
		f _ = Nothing

allNonMarkers :: [Rule Conj] -> [Term NonMarker]
allNonMarkers = nonMarkers . allTerms

conjBody :: Rule Conj -> [Some Term]
conjBody (Conj _ body) = body

nonMarkers :: [Some Term] -> [Term NonMarker]
nonMarkers = catMaybes . map (elim f) 
	where
		f :: Term a -> Maybe (Term NonMarker)
		f t@(Term _ _ _) = Just t
		f _ = Nothing

ruleHead :: Rule a -> Symbol NonTerminal 
ruleHead (Disj h _) = h
ruleHead (Conj h _) = h
