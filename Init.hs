{-
 - maketea -- generate C++ AST infrastructure
 - (C) 2006-2007 Edsko de Vries and John Gilbert
 -}

module Init where

import Control.Monad.Identity

import DataStructures
import Cpp
import GrammarAnalysis

addInit :: Class -> Class
addInit cls = 
		if hasInit cls 
		then runIdentity $ mapMethods addInit' cls 
		else cls
	where
		addInit' :: Member -> Identity Member
		addInit' m@(Method c v s (t, n) a b) | n == name cls 
			= return $ Method c v s (t, n) a (b ++ ["_init();"])
		addInit' m = return m

{-
 - We use mapMethods with a specialized monad to check if we have an _init
 - method. Slightly overkill perhaps, but fun :)
 - 
 - In this particular case, mapMethods maps a function that searches methods
 - (i.e., of type Search Method) to a function that searches classes
 - (i.e., of type Search Class). 
 -}

data Search a = Found Bool a

instance Monad Search where
	return a = Found False a
	-- m a -> (a -> m b) -> m b
	(Found found a) >>= f = case f a of
		Found found' b -> Found (found || found') b

hasInit :: Class -> Bool
hasInit cls = case mapMethods f cls of
		Found found _ -> found
	where
		f :: Member -> Search Member
		f m 
			| nameOf m == "_init" = (Found True m)
			| otherwise = return m
