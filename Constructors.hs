{-
 - maketea -- generate C++ AST infrastructure
 - (C) 2006-2007 Edsko de Vries and John Gilbert
 - License: GNU General Public License 2.0
 -}

module Constructors where

import DataStructures
import MakeTeaMonad
import Cpp

addConstructors :: MakeTeaMonad ()
addConstructors = 
	do
		cs <- withClasses $ mapM $ \cls -> do
			let ic = initConstr cls
			let nc = nullConstr cls
			if numArgs ic == 0 
				then return cls { sections = 
					  Section [] Public [initConstr cls]
					: sections cls 
					}
				else return cls { sections = 
					  Section [] Public [initConstr cls]
					: Section [] Protected [nullConstr cls] 
					: sections cls 
					}
		setClasses cs
	where
		numArgs (Method _ _ _ _ args _) = length args 

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
