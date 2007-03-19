{-
 - maketea -- generate C++ AST infrastructure
 - (C) 2006-2007 Edsko de Vries and John Gilbert
 - License: GNU General Public License 2.0
 -}

module MakeTeaMonad where

import Control.Monad.State
import Data.Maybe
import Data.List
import qualified Data.Graph.Inductive as FGL

import DataStructures
import Util
import PrettyPrinter

{-
 - Convenient access to components of the maketea state
 -}

withGrammar :: (Grammar -> MakeTeaMonad a) -> MakeTeaMonad a
withGrammar f = get >>= f . grammar 

withConj :: ([Rule Conj] -> MakeTeaMonad a) -> MakeTeaMonad a
withConj f = withGrammar $ f . conjunctions 

withDisj :: ([Rule Disj] -> MakeTeaMonad a) -> MakeTeaMonad a
withDisj f = withGrammar $ f . disjunctions 

withTokens :: ([Symbol Terminal] -> MakeTeaMonad a) -> MakeTeaMonad a
withTokens f = withGrammar $ f . allTokens 

withNonMarkers :: ([Term NonMarker] -> MakeTeaMonad a) -> MakeTeaMonad a
withNonMarkers f = withConj $ f . allNonMarkers

withClasses :: ([Class] -> MakeTeaMonad a) -> MakeTeaMonad a
withClasses f = get >>= f . fromJust . classes 

withContexts :: ([Context] -> MakeTeaMonad a) -> MakeTeaMonad a
withContexts f = get >>= f . fromJust . contexts

withSymbols :: ([Some Symbol] -> MakeTeaMonad a) -> MakeTeaMonad a
withSymbols f = withGrammar $ f . allSymbols

withTopological :: ([Some Symbol] -> MakeTeaMonad a) -> MakeTeaMonad a
withTopological f = get >>= f . topological

withConfig :: (Config -> MakeTeaMonad a) -> MakeTeaMonad a
withConfig f = get >>= f . config

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

{-
 - Access to the configuration
 -}

isExternal :: Name Class -> MakeTeaMonad Bool
isExternal c = withConfig $ return . (c `elem`) . external_classes 

getPrefix :: MakeTeaMonad String
getPrefix = withConfig $ return . prefix

getListClass :: MakeTeaMonad String
getListClass = withConfig $ return . listClass

getStringClass :: MakeTeaMonad String
getStringClass = withConfig $ return . stringClass

{-
 - Initial state for the monad
 -}

initState :: Config -> Grammar -> MakeTeaState
initState cf gr
	| not (null unreachable) = error $ "The inheritance hierarchy does not have a unique root.\nThe following nodes are unreachable from " ++ show (fromJust (FGL.lab ih (head top))) ++ ": " ++ show (map (fromJust . FGL.lab ih) unreachable)
	| FGL.hasLoop ih = error $ "There are self-referential rules in some of: " ++ show (map (map (fromJust . FGL.lab ih)) (FGL.scc ih))
	| not (null cycles) = error $ "The inheritance graph is cyclic.\nCycles: " ++ show (map (map (fromJust . FGL.lab ih)) cycles)
	| otherwise = MTS {
		  grammar = gr
		, nextClassID = 1 
		, contexts = Nothing
		, classes = Nothing
		, inheritanceGraph = ih 
		, topological = FGL.topsort' ih 
		, config = cf 
		}
	where
		ih = findInheritanceGraph gr
		top = FGL.topsort ih
		reachable = FGL.bfs (head top) ih
		unreachable = FGL.nodes ih \\ reachable
		cycles = filter ((> 1) . length) (FGL.scc (FGL.trc ih))

findInheritanceGraph :: Grammar -> FGL.Gr (Some Symbol) () 
findInheritanceGraph grammar = 
	let 
		labels = [(s,no) | s <- allSymbols grammar | no <- [1..]]
		labelFor s = fromJust (lookup s labels)
		nodes = map (\(a,b) -> (b,a)) labels
		edges = concatMap edgesFor (disjunctions grammar) 
		edgesFor (Disj nt body) =
			let lr = labelFor (Exists nt) 
			in [(lr,labelFor s,()) | s <- body] 
	in 
		(FGL.mkGraph nodes edges)

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

disjunctions :: Grammar -> [Rule Disj]
disjunctions = catMaybes . map (elim disj)
	where
		disj :: Rule a -> Maybe (Rule Disj)
		disj (Conj _ _) = Nothing
		disj r@(Disj _ _) = Just r

conjunctions :: Grammar -> [Rule Conj]
conjunctions = catMaybes . map (elim conj) 
	where
		conj :: Rule a -> Maybe (Rule Conj)
		conj r@(Conj _ _) = Just r
		conj (Disj _ _) = Nothing

allSymbols :: Grammar -> [Some Symbol]
allSymbols gr = nts ++ ts 
	where
		nts = map (Exists . elim ruleHead) gr
		ts = map Exists (allTokens gr)
