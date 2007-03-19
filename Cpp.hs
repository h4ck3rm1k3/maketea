{-
 - maketea -- generate C++ AST infrastructure
 - (C) 2006-2007 Edsko de Vries and John Gilbert
 - License: GNU General Public License 2.0
 -}

module Cpp where

import Data.Maybe
import Data.List
import Data.Char

import Debug.Trace
import Text.PrettyPrint


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

{-
 - Find classes
 -}

findClass :: Name Class -> MakeTeaMonad Class
findClass cn = withClasses $ fromJustM ("could not find class " ++ cn) . find (\c -> name c == cn)

findClassID :: Some Symbol -> MakeTeaMonad Integer
findClassID s = do 
	cn <- toClassName s
	c <- findClass cn
	return (classid c)

{-
 - Transitive reflexive closure of "extends"
 -}

allExtends :: [Name Class] -> MakeTeaMonad [Name Class]
allExtends [] = return []
allExtends (c:cn) = do
	isExt <- isExternal c
	if isExt 
		then allExtends cn
		else do
			cls <- findClass c	
			ss <- allExtends (extends cls ++ cn)
			return $ nub (c:ss)

{-
 - Class ordering
 - 
 - Sort the list of classes topologically based on their inheritance relation
 - (this is a stable sort). External classes are presumed defined.
 -}

orderClasses :: MakeTeaMonad ()
orderClasses = withClasses $ \cs -> do
	ext <- withConfig $ return . external_classes
	setClasses (orderClasses' ext cs)

orderClasses' :: [Name Class] -> [Class] -> [Class]
orderClasses' _ [] = [] 
orderClasses' visited toVisit = 
		if null next
		then error $ "cyclic inheritance hierarchy or unknown class:\n" 
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
symbolToClassName (NonTerminal n) = do
	p <- getPrefix
	return (p ++ "_" ++ n)
symbolToClassName (Terminal n _) = return $ "Token_" ++ (map toLower n) 

termToClassName :: Term NonMarker -> MakeTeaMonad CType 
termToClassName (Term _ s m) = do
	cn <- elim symbolToClassName s
	if isVector m 
		then do
			list <- getListClass
			return (list ++ "<" ++ cn ++ "*>")
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

mapMembers :: Monad m => (Member -> m Member) -> Class -> m Class
mapMembers f cls = 
	do
		sections' <- mapM mapSection (sections cls)
		return $ cls { sections = sections' }
	where
		mapSection (Section c a ms) = do
			ms' <- mapM f ms
			return $ Section c a ms'

{-
 - Search for a member in a class
 -}

findMember :: (Member -> Bool) -> Class -> [Member]
findMember f cls = fromSearch (mapMembers (compareUsing f) cls)

allMembers :: Class -> [Member]
allMembers = findMember (const True) 

hasMethod :: Name Method -> Class -> Bool 
hasMethod name cls = not $ null (findMember (\m -> nameOf m == name) cls)

{-
 - Does cls have a member with the same signature as the given member?
 -}

hasSig :: Class -> Member -> Bool
hasSig cls m = not (null (findMember (sameSig m) cls))

sameSig :: Member -> Member -> Bool
sameSig (Attribute _ d) (Attribute _ d') = d == d' 
sameSig (Method _ _ _ d as _) (Method _ _ _ d' as' _) 
	| d == d' && length as == length as' = all sameType (zip as as')
	| otherwise = False
sameSig (PureVirtual _ d as) (PureVirtual _ d' as')
	| d == d' && length as == length as' = all sameType (zip as as')
	| otherwise = False
sameSig _ _ = False

sameType :: (Decl a, Decl a) -> Bool
sameType ((t, _), (t', _)) = t == t'

{-
 - Naive check if a C type is a pointer
 -}

isPointer :: CType -> Bool
isPointer t = last t == '*'
