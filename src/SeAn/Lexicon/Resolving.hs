{-|
  Resolving ambiguity adds indices to the declarations, and disambiguates
  the resulting expressions using these new names. Note that the added indices
  are separated by the `+` token. This is a hint to the type inference that
  at least one of these definitions should succeed.
  |-}
module SeAn.Lexicon.Resolving where

import SeAn.Lexicon.Base hiding (not)

import Text.Printf (printf)
import Data.Map (Map)
import qualified Data.Map as M
import Data.List (groupBy)

-- |Determine whether or not a program is ambiguous.
isAmbiguous :: Eq n => Prog n -> Bool
isAmbiguous p = any ((> 1) . (freq names)) names
  where
    names = declaredNames p

type Resolver n = (n,Label) -> [(n,Label)]
type Frequency a = a -> Int

-- |Calculate a frequency table for a list.
freq :: Eq a => [a] -> Frequency a
freq [    ] = \_ -> 0
freq (x:xs) = \y -> (if x == y then 1 else 0) + freq xs y

-- |Add labels to all names, initializing at zero.
labelZero :: HasNames p => p a -> p (a,Label)
labelZero = mapNames (\n -> (n,[]))

-- |Disambiguate a program until a fixed-point is reached.
resolveProg :: IsName n => Prog n -> Prog (n,Label)
resolveProg = until (not . isAmbiguous) resolveProg1 . labelZero

-- |Disambiguate a program a single step, not resolving arising ambiguities.
resolveProg1 :: IsName n => Prog (n,Label) -> Prog (n,Label)
resolveProg1 p@(Prog decs) = Prog decs'
  where
    names = declaredNames p
    freqs = freq names
    resol = resolveName freqs
    decs' = concatMap (resolveDecl resol) (relabelDecls decs)

-- |Disambiguate declarations based on name resolvers.
resolveDecl :: IsName n => Resolver n -> (Decl (n,Label) -> [Decl (n,Label)])
resolveDecl r (Decl n e) = resolveExpr r e >>= return . Decl n

-- |Disambiguate expressions based on name resolvers.
resolveExpr :: IsName n => Resolver n -> (Expr (n,Label) -> [Expr (n,Label)])
resolveExpr r (Con n)
  | isReservedName (base n)  = return (Con n)
  | otherwise                = do n' <- r n
                                  return (Con n)
resolveExpr r (Var n)        = do return (Var n)
resolveExpr r (Abs n e)      = do let r' = bindName n r
                                  e' <- resolveExpr r' e
                                  return (Abs n e')
resolveExpr r (App e1 e2)    = do e1' <- resolveExpr r e1
                                  e2' <- resolveExpr r e2
                                  return (App e1' e2')
resolveExpr r (Let n e1 e2)  = do let r' = bindName n r
                                  e1' <- resolveExpr r' e1
                                  e2' <- resolveExpr r' e2
                                  return (Let n e1' e2')
resolveExpr r (Hole t)       = do return (Hole t)
resolveExpr r (Inst n w)     = do n' <- r n
                                  return (Inst n' w)

-- |Disambiguates names based on a program.
resolveName :: IsName n => Frequency (n,Label) -> Resolver n
resolveName freqs n
  | k <= 0 = error ("labelName: unbound name " ++ collapse n)
  | k == 1 = [ n ]
  | k >= 2 = map (\i -> addIndexToName i n) [0 .. k - 1]
  where
    k = freqs n

-- |Removes an entry for @n@ from the name resolver.
bindName :: Eq n => (n,Label) -> Resolver n -> Resolver n
bindName n r m = if m == n then [ m ] else r m

-- |Indexes all declarations.
relabelDecls :: Eq n => [Decl (n,Label)] -> [Decl (n,Label)]
relabelDecls = concat . map addIndexIfAmb . groupBy eqName
  where
    addIndexIfAmb :: [Decl (n,Label)] -> [Decl (n,Label)]
    addIndexIfAmb [ ] = [ ]
    addIndexIfAmb [d] = [d]
    addIndexIfAmb ds  = addIndexToDecls 0 ds

    addIndexToDecls :: Int -> [Decl (n,Label)] -> [Decl (n,Label)]
    addIndexToDecls _ [] = []
    addIndexToDecls i (d:ds) = addIndexToDecl i d : addIndexToDecls (i + 1) ds

addIndexToDecl :: Int -> Decl (n,Label) -> Decl (n,Label)
addIndexToDecl i (Decl n e) = Decl (addIndexToName i n) e

addIndexToName :: Int -> (n,Label) -> (n,Label)
addIndexToName l (n,ls) = (n,l:ls)
