{-
 - maketea -- generate C++ AST infrastructure
 - (C) 2006-2007 Edsko de Vries and John Gilbert
 - License: GNU General Public License 2.0
 -}

module Cpp where

import Data.Maybe
import Data.List
import Data.Char

import DataStructures
import MakeTeaMonad
import GrammarAnalysis
import PrettyPrinter
import Util

{-
 - Class templates
 -}

defMethod :: Decl Method -> [Decl Variable] -> Body -> Member
defMethod = Method [] Virtual NonStatic

defConstr :: Decl Method -> [Decl Variable] -> Body -> Member
defConstr = Method [] NonVirtual NonStatic

emptyClass :: Name Class -> MakeTeaMonad Class 
emptyClass n = do
	cid <- getNextClassID
	let getID = defMethod ("int", "classid") [] ["return " ++ show cid ++ ";"]
	return $ Class {
		  name = n
		, comment = []
		, extends = []
		, sections = [Section [] Private [getID]]
		, classid = cid
		, friends = []
		, origin = Nothing
		}

emptyAbstractClass :: Name Class -> Class
emptyAbstractClass n = Class {
		  name = n
		, comment = []
		, extends = []
		, sections = [Section [] Private [getID]]
		, classid = 0
		, friends = []
		, origin = Nothing
		}
	where
		getID = PureVirtual [] ("int", "classid") []

emptyClassNoID :: Name Class -> Class
emptyClassNoID n = Class {
		  name = n
		, comment = []
		, extends = []
		, sections = []
		, classid = 0
		, friends = []
		, origin = Nothing
		}

findClassID :: Some Symbol -> MakeTeaMonad Integer
findClassID s = withClasses $ \classes -> do
	cn <- toClassName s
	let cl = find (\c -> name c == cn) classes
	case cl of
		Just cl -> return (classid cl) 
		Nothing -> fail $ "Unknown symbol " ++ show s

{-
 - Class ordering
 - 
 - Sort the list of classes topologically based on their inheritance relation
 - (this is a stable sort). Classes that are inherited from but not defined
 - anywhere, are assumed "outside" classes and are not taken into account in
 - the ordering (i.e., if a class A inherits from a class B, but we have no
 - definition of class B, class B is not required to be defined before class
 - A).
-}

orderClasses :: [Class] -> [Class]
orderClasses classes = orderClasses' outsideClasses classes
	where
		outsideClasses = allInh \\ allClasses
		allInh = nub (concatMap extends classes)
		allClasses = map name classes

orderClasses' :: [Name Class] -> [Class] -> [Class]
orderClasses' _ [] = [] 
orderClasses' visited toVisit = 
		if null next
		then error $ "cyclic inheritance hierarchy:\n" 
			++ unlines (map errMsg toVisit) 
		else next ++ (orderClasses' visitedR toVisitR) 
	where
		(next, toVisitR) = partition parentsVisited toVisit
		visitedR = visited ++ map name next
		parentsVisited c = all (`elem` visited) (extends c)
		errMsg c = (name c) ++ " inherits from " ++ show (extends c)

{-
 - Translation to C++ class names
 -}

class ToClassName a where
	toClassName :: a -> MakeTeaMonad (Name Class)

instance ToClassName (Symbol a) where
	toClassName = symbolToClassName

instance ToClassName (Some Symbol) where
	toClassName = elim symbolToClassName

instance ToClassName (Term NonMarker) where
	toClassName = termToClassName

symbolToClassName :: Symbol a -> MakeTeaMonad (Name Class)
symbolToClassName (NonTerminal n) = withPrefix $ return . (++ n)
symbolToClassName (Terminal n _) = return $ "Token_" ++ (map toLower n) 

termToClassName :: Term NonMarker -> MakeTeaMonad CType 
termToClassName (Term _ s m) = do
	cn <- elim symbolToClassName s
	if isVector m 
		then return ("list<" ++ cn ++ "*>")
		else return cn

{-
 - Translation to C++ variable names
 -}

class ToVarName a where
	toVarName :: a -> Name Variable 

instance ToVarName (Term a) where
	toVarName = termToVarName

instance ToVarName (Some Term) where
	toVarName = elim termToVarName

instance ToVarName (Symbol a) where
	toVarName = symbolToVarName

instance ToVarName (Some Symbol) where
	toVarName = elim symbolToVarName

termToVarName :: Term a -> Name Variable 
termToVarName (Term Nothing s m) 
	| isVector m = toVarName s ++ "s" 
	| otherwise = toVarName s
termToVarName (Term (Just n) _ _) = n
termToVarName (Marker Nothing m) = "is_" ++ m
termToVarName (Marker (Just n) _) = n

symbolToVarName :: Symbol a -> Name Variable 
symbolToVarName (NonTerminal n) = n
symbolToVarName (Terminal n _) = map toLower n

mapMethods :: Monad m => (Member -> m Member) -> Class -> m Class
mapMethods f cls = 
	do
		sections' <- mapM mapSection (sections cls)
		return $ cls { sections = sections' }
	where
		mapSection (Section c a ms) = do
			ms' <- mapM f ms
			return $ Section c a ms'

{-
 - Search for a method in a class
 -}

hasMethod :: Name Method -> Class -> Bool
hasMethod name cls = case mapMethods f cls of
		Found found _ -> found
	where
		f :: Member -> Search Member
		f m 
			| nameOf m == name = (Found True m)
			| otherwise = return m

{-
 - Naive check if a C type is a pointer
 -}

isPointer :: CType -> Bool
isPointer t = last t == '*'
