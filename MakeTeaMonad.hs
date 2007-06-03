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

withClasses :: ([Class] -> MakeTeaMonad a) -> MakeTeaMonad a
withClasses f = get >>= f . fromJust . classes 

withContexts :: ([Context] -> MakeTeaMonad a) -> MakeTeaMonad a
withContexts f = get >>= f . fromJust . contexts

withOrigContexts :: ([Context] -> MakeTeaMonad a) -> MakeTeaMonad a
withOrigContexts f = get >>= f . fromJust . origContexts

withSymbols :: ([Some Symbol] -> MakeTeaMonad a) -> MakeTeaMonad a
withSymbols f = withGrammar $ f . allSymbols

withTopological :: ([Some Symbol] -> MakeTeaMonad a) -> MakeTeaMonad a
withTopological f = get >>= f . topological

withConfig :: (Config -> MakeTeaMonad a) -> MakeTeaMonad a
withConfig f = get >>= f . config

getMixin :: MakeTeaMonad [Class]
getMixin = get >>= return . mixinCode

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

setOrigContexts :: [Context] -> MakeTeaMonad ()
setOrigContexts cxs = do
	s <- get
	put (s { origContexts = Just cxs })

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

initState :: Config -> Grammar -> [Class] -> MakeTeaState
initState cf gr mixin
	| not (null unreachable) = error $ "The inheritance hierarchy does not have a unique root.\nThe following nodes are unreachable from " ++ show (fromJust (FGL.lab ih (head top))) ++ ": " ++ show (map (fromJust . FGL.lab ih) unreachable)
	| FGL.hasLoop ih = error $ "There are self-referential rules in some of: " ++ show (map (map (fromJust . FGL.lab ih)) (FGL.scc ih))
	| not (null cycles) = error $ "The inheritance graph is cyclic.\nCycles: " ++ show (map (map (fromJust . FGL.lab ih)) cycles)
	| otherwise = MTS {
		  grammar = gr
		, nextClassID = 1 
		, contexts = Nothing
		, origContexts = Nothing
		, classes = Nothing
		, inheritanceGraph = ih 
		, topological = FGL.topsort' ih 
		, config = cf
		, mixinCode = mixin
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

{-
	All non-marker terms that explicitely appear in the grammar, plus an 
	implicit term (Nothing, s, Single) for every concrete non-terminal in the
	grammar that does not appear anywhere else.
-}

allNonMarkers :: MakeTeaMonad [Term NonMarker] 
allNonMarkers = 
	do
		explicitOcc <- withConj (return . nonMarkers . allTerms)
		reachable <- withConj $ concatMapM findReachable
		implicitOcc <- withConj $ return . map (Exists . ruleHead)
		return $ explicitOcc ++ [Term Nothing t Single | t <- implicitOcc, not (t `elem` reachable)] 
	where
		findReachable :: Rule Conj -> MakeTeaMonad [Some Symbol]
		findReachable (Conj _ body) = concatMapM g (nonMarkers body)
		g :: Term NonMarker -> MakeTeaMonad [Some Symbol] 
		g (Term _ t _) = elim allInstances t 

{-
 - isAbstract nt is true if nt is defined by a disjunction
 -}

isAbstract :: Name NonTerminal -> MakeTeaMonad Bool
isAbstract nt = do
	r <- findRuleFor nt
	return (elim isDisj r)

{-
 - Is a rule a disjunction?
 -}

isDisj :: Rule a -> Bool
isDisj (Disj _ _) = True
isDisj (Conj _ _) = False 

{-
 - Is a symbol a non-terminal?
 -}

isNonTerminal :: Some Symbol -> Bool
isNonTerminal = elim f
	where
		f :: Symbol a -> Bool
		f (NonTerminal _) = True
		f _ = False

{-
 - findRuleFor nt finds the rule in the grammar that defines nt
 -}

findRuleFor :: Name NonTerminal -> MakeTeaMonad (Some Rule)
findRuleFor nt = withGrammar $ (fromJustM $ "Unknown non-terminal " ++ nt) . find (elim f)
	where
		f :: Rule a -> Bool
		f r = nameOf (ruleHead r) == nt 

{-
 - The instances of a symbol s always includes s itself. For abstract symbols
 - defined by a disjunction x1 .. xn, the instances of s also include all
 - instances of the xs. The instances will be returned in order from general to
 - more specific (that is, if t is an instance of t', t' will be reported
 - before t).
 -}

allInstances :: Symbol a -> MakeTeaMonad [Some Symbol]
allInstances i@(NonTerminal nt) = 
	do
		r <- findRuleFor nt
		elim f r	
	where
		f :: Rule a -> MakeTeaMonad [Some Symbol]
		f (Disj _ xs) = do
			is <- mapM (elim allInstances) xs
			return (Exists i:concat is)
		f (Conj _ _) = return [Exists i]
allInstances i@(Terminal _ _) = return [Exists i]

{-
 - Like allInstances, but filtered to include the concrete instances only
 -}

concreteInstances :: Symbol a -> MakeTeaMonad [Some Symbol]
concreteInstances i@(NonTerminal nt) = 
	do
		r <- findRuleFor nt
		elim f r	
	where
		f :: Rule a -> MakeTeaMonad [Some Symbol]
		f (Disj _ xs) = do
			is <- mapM (elim concreteInstances) xs
			return (concat is)
		f (Conj _ _) = return [Exists i]
concreteInstances i@(Terminal _ _) = return [Exists i]

{-
 - commonInstance finds the most general common instance of two symbols, if it
 - exists.
 -}

commonInstance :: Symbol a -> Symbol b -> MakeTeaMonad (Maybe (Some Symbol))
commonInstance s1 s2 = do
	is1 <- allInstances s1
	is2 <- allInstances s2
	return (find (`elem` is2) is1)

{-
 - The direct superclasses of a symbol c are all those symbols s which are
 - defined by a rule s ::= ... | c | ...
 -}

directSuperclasses :: Some Symbol -> MakeTeaMonad [Name NonTerminal]
directSuperclasses c = withDisj $ return . catMaybes . (map f)
	where
		f :: Rule Disj -> Maybe (Name NonTerminal)
		f (Disj s cs) = if c `elem` cs then Just (nameOf s) else Nothing

{-
 - Transitive reflexive closure of directSuperclasses
 -}

allSuperclasses :: [Some Symbol] -> MakeTeaMonad [Some Symbol]
allSuperclasses [] = return []
allSuperclasses c = do
	ds <- concatMapM directSuperclasses c
	let ds' = (nub . map (Exists . NonTerminal)) ds
	dds <- allSuperclasses ds' 
	return (nub (c ++ ds' ++ dds))

{-
 - isVector is true for Vector, VectorOpt and OptVector
 -}

isVector :: Multiplicity -> Bool
isVector Single = False
isVector Optional = False
isVector Vector = True
isVector VectorOpt = True
isVector OptVector = True

{-
 - All concrete symbols (that is, non-terminal symbols that are defined as a
 - conjunction, and terminal symbols) in the grammar
 -}

concreteSymbols :: MakeTeaMonad [Some Symbol]
concreteSymbols = do
	nts <- withConj $ return . (map (Exists . ruleHead))
	tokens <- withTokens $ return . map Exists
	return (nts ++ tokens)

{-
 - All abstract symbols (that is, non-terminal sysbols that are defined as a
 - disjunction)
 -}

abstractSymbols :: MakeTeaMonad [Name NonTerminal]
abstractSymbols = withDisj $ return . (map (nameOf . ruleHead))

{-
 - All used symbols; that is, all symbols that appear in the RHS of a
 - conjunction 
 -}

usedSymbols :: MakeTeaMonad [Some Symbol]
usedSymbols = withConj $ return . nub . concatMap f
	where
		f :: Rule Conj -> [Some Symbol]
		f (Conj _ body) = [s | Exists (Term _ s _) <- body]

{-
 - All used abstract symbols
 -}

usedAbstractSymbols :: MakeTeaMonad [Name NonTerminal]
usedAbstractSymbols 
		= usedSymbols >>= filterM isAbstract . catMaybes . map (elim f)
	where
		f :: Symbol a -> Maybe (Name NonTerminal)
		f (NonTerminal n) = Just n
		f _ = Nothing

{-
 - 	All symbols that appear in a list
 -}

allLists :: MakeTeaMonad [Some Symbol]
allLists = 
	do
		terms <- withConj (return . allTerms)
		return . nub . catMaybes . map (elim f) $ terms
	where
		f :: Term a -> Maybe (Some Symbol)
		f (Term _ s m) | isVector m = Just s
		f _ = Nothing


{-
 - Root of the inheritance graph
 -}

rootSymbol :: MakeTeaMonad (Some Symbol)
rootSymbol = withTopological $ return . head

{-
 - Find the name of stuff
 -}

class NameOf a where
	nameOf :: a -> String

instance NameOf (Some Symbol) where
	nameOf = elim nameOf 

instance NameOf (Symbol a) where
	nameOf (NonTerminal name) = name
	nameOf (Terminal name _) = name

instance NameOf (Some Term) where
	nameOf = elim nameOf

instance NameOf (Term a) where
	nameOf (Term (Just label) _ _) = label
	nameOf (Term Nothing s m) 
		| isVector m = nameOf s ++ "s"
		| otherwise = nameOf s
	nameOf (Marker (Just label) _) = label
	nameOf (Marker Nothing name) = name

instance NameOf Class where
	nameOf = name

instance NameOf Member where
	nameOf (Attribute _ (_, name)) = name
	nameOf (StaticConst _ (_, name) _) = name
	nameOf (Method _ _ _ (_, name) _ _) = name
	nameOf (PureVirtual _ (_, name) _) = name
