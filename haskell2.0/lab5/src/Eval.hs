
module Eval where

import Exp
import Data.List ( union, delete )

vars :: Exp -> [IndexedVar]
vars (X x) = [x]
vars (App t1 t2 ) = (vars t1) `union` (vars t2)
vars (Lam x t) = [x] `union` (vars t)

-- >>> vars (Lam (makeIndexedVar "x") (X (makeIndexedVar "y")))
-- [IndexedVar {ivName = "x", ivCount = 0},IndexedVar {ivName = "y", ivCount = 0}]

-- >>> vars (App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "y", ivCount = 0}))) (X (IndexedVar {ivName = "z", ivCount = 0})))
-- [IndexedVar {ivName = "x", ivCount = 0},IndexedVar {ivName = "y", ivCount = 0},IndexedVar {ivName = "z", ivCount = 0}]

-- >>> vars (App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "x", ivCount = 0}))) (X (IndexedVar {ivName = "x", ivCount = 0})))
-- [IndexedVar {ivName = "x", ivCount = 0}]

freeVars :: Exp -> [IndexedVar]
freeVars (X x) = [x]
freeVars (App t1 t2 ) = (freeVars t1) `union` (freeVars t2)
freeVars (Lam x t) = delete x (freeVars t)

-- >>> freeVars (Lam (makeIndexedVar "x") (X (makeIndexedVar "y")))
-- [IndexedVar {ivName = "y", ivCount = 0}]

-- >>> freeVars  (App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "y", ivCount = 0}))) (X (IndexedVar {ivName = "z", ivCount = 0})))
-- [IndexedVar {ivName = "y", ivCount = 0},IndexedVar {ivName = "z", ivCount = 0}]

-- >>> freeVars (App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "x", ivCount = 0}))) (X (IndexedVar {ivName = "x", ivCount = 0})))
-- [IndexedVar {ivName = "x", ivCount = 0}]

-- >>> freeVars (Lam (IndexedVar {ivName = "x", ivCount = 0}) (App (X (IndexedVar {ivName = "x", ivCount = 0})) (X (IndexedVar {ivName = "x", ivCount = 0}))))
-- []

occursFree :: IndexedVar -> Exp -> Bool
occursFree x e = elem x (freeVars e)

-- >>> makeIndexedVar "x" `occursFree` Lam (makeIndexedVar "x") (X (makeIndexedVar "y"))
-- False

-- >>> makeIndexedVar "y" `occursFree` Lam (makeIndexedVar "x") (X (makeIndexedVar "y"))
-- True

freshVar :: IndexedVar -> [IndexedVar] -> IndexedVar
freshVar x xs = x { ivCount = m+1 }
    where 
        nxs = [ivCount y| y<- x:xs, ivName y = ivName x]
        m = maximum nxs

-- >>> freshVar (makeIndexedVar "x") [makeIndexedVar "x"]
-- IndexedVar {ivName = "x", ivCount = 1}

-- >>> freshVar (makeIndexedVar "x") [makeIndexedVar "x", IndexedVar {ivName = "x", ivCount = 1}, IndexedVar {ivName = "y", ivCount = 2}] 
-- IndexedVar {ivName = "x", ivCount = 2}

renameVar :: IndexedVar -> IndexedVar -> Exp -> Exp
renameVar toReplace replacement = go
    where
        go(X x) = if x== toReplace then (x replacement)
                    else (X x)
        go (App t1 t2) = App(go t1)(go t2)
        go (Lam x t) = Lam (if x == toReplace then replacement
                            else x) (go t)

-- >>> renameVar (IndexedVar {ivName = "x", ivCount = 0}) (IndexedVar {ivName = "z", ivCount = 0}) (App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "x", ivCount = 0}))) (X (IndexedVar {ivName = "x", ivCount = 0})))
-- App (Lam (IndexedVar {ivName = "z", ivCount = 0}) (X (IndexedVar {ivName = "z", ivCount = 0}))) (X (IndexedVar {ivName = "z", ivCount = 0}))

substitute :: IndexedVar -> Exp -> Exp -> Exp
substitute toReplace replacement = go
    where
        go (X x) = if x == toReplace then replacement else (X x)
        go(App e1 e2) = App(go e1)(go e2)
        go(Lam x e) = 
        | x== toReplace = Lam x e
        | occursFree x replacement = 
            let f = freshVar x (vars e `union` vars replacement)
            in Lam f (go (renameVarx f e))
        | otherwise = Lam x (go e)

-- >>> substitute (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "z", ivCount = 0})) (App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "x", ivCount = 0}))) (X (IndexedVar {ivName = "x", ivCount = 0})))
-- App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "x", ivCount = 0}))) (X (IndexedVar {ivName = "z", ivCount = 0}))

-- >>> substitute (IndexedVar {ivName = "y", ivCount = 0}) (X (IndexedVar {ivName = "x", ivCount = 0})) (App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "y", ivCount = 0}))) (X (IndexedVar {ivName = "z", ivCount = 0})))
-- App (Lam (IndexedVar {ivName = "x", ivCount = 1}) (X (IndexedVar {ivName = "x", ivCount = 0}))) (X (IndexedVar {ivName = "z", ivCount = 0}))

normalize :: Exp -> Exp
normalize = undefined

-- >>> normalize (X (makeIndexedVar "x"))
-- X (IndexedVar {ivName = "x", ivCount = 0})

-- >>> normalize (App (Lam (IndexedVar {ivName = "y", ivCount = 0}) (X (IndexedVar {ivName = "x", ivCount = 0}))) (App (Lam (IndexedVar {ivName = "y", ivCount = 0}) (App (X (IndexedVar {ivName = "y", ivCount = 0})) (X (IndexedVar {ivName = "y", ivCount = 0})))) (Lam (IndexedVar {ivName = "y", ivCount = 0}) (App (X (IndexedVar {ivName = "y", ivCount = 0})) (X (IndexedVar {ivName = "y", ivCount = 0}))))))
-- X (IndexedVar {ivName = "x", ivCount = 0})

