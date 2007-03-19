{-
 - maketea -- generate C++ AST infrastructure
 - (C) 2006-2007 Edsko de Vries and John Gilbert
 - License: GNU General Public License 2.0
 -}

module Constructors where

import DataStructures
import MakeTeaMonad
import Cpp

addConstructor :: (Class -> Member) -> MakeTeaMonad ()
addConstructor mkConstr = do
	cs <- withClasses $ mapM $ \cls -> do
		let constr = mkConstr cls
		return $ if hasSig cls constr 
			then cls
			else (cls { sections = Section [] Public [constr] : sections cls })
	setClasses cs

initConstr :: Class -> Member
initConstr cls = defConstr ("", name cls) args (concatMap init fields)
	where
		fields = allMembers cls
		args = [(t,n) | Attribute _ (t,n) <- fields]
		init :: Member -> Body
		init (Attribute _  (_,n)) = ["this->" ++ n ++ " = " ++ n ++ ";"]
		init _ = []

nullConstr :: Class -> Member
nullConstr cls = defConstr ("", name cls) [] (concatMap initNull fields)
	where
		fields = allMembers cls
		args = [(t,n) | Attribute _ (t,n) <- fields]
		initNull :: Member -> Body
		initNull (Attribute _  (_,n)) = ["this->" ++ n ++ " = 0;"]
		initNull _ = []

	
