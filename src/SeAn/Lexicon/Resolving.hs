module SeAn.Lexicon.Resolving where

import SeAn.Lexicon.Base

import Text.Printf (printf)
import Data.Map (Map)
import qualified Data.Map as M
import Data.List (groupBy)

{-|

  Resolving ambiguity adds indices to the declarations, and disambiguates
  the resulting expressions using these new names. Note that the added indices
  are separated by the `+` token. This is a hint to the type inference that
  at least one of these definitions should succeed.

  |-}

-- |Determines whether or not a program is ambiguous.
isAmbiguous :: Prog Name -> Bool
isAmbiguous p = any ((> 1) . fs) ns
  where
    ns = declaredNames p
    fs = freq ns

-- * Name resolving

-- |Disambiguates a program until a fixed-point is reached.
resolveProg :: Prog Name -> Prog Name
resolveProg = until (Prelude.not . isAmbiguous) resolveProg1

-- |Disambiguates a program a single step.
resolveProg1 :: Prog Name -> Prog Name
resolveProg1 p@(Prog ds) = Prog ds'
  where
    fs  = freq (declaredNames p)
    r   = resolveName fs
    ds' = concatMap (resolveDecl r) (indexDecls ds)

-- |Disambiguates declarations based on name resolvers.
resolveDecl :: (Name -> [Name]) -> (Decl Name -> [Decl Name])
resolveDecl r (Decl n e) = do e' <- resolveExpr r e
                              return (Decl n e')

-- |Disambiguates expressions based on name resolvers.
resolveExpr :: (Name -> [Name]) -> (Expr Name -> [Expr Name])
resolveExpr r (Con n)       = return (Con n)
resolveExpr r (Var n)       = do n' <- r n; return (Var n')
resolveExpr r (Abs n e1)    = do let r' = rebindName n r
                                 e1' <- resolveExpr r' e1
                                 return (Abs n e1')
resolveExpr r (App e1 e2)   = do e1' <- resolveExpr r e1
                                 e2' <- resolveExpr r e2
                                 return (App e1' e2')
resolveExpr r (Let n e1 e2) = do let r' = rebindName n r
                                 e1' <- resolveExpr r' e1
                                 e2' <- resolveExpr r' e2
                                 return (Let n e1' e2')
resolveExpr r (Inst n w)    = do n' <- r n; return (Inst n' w)


-- |Disambiguates names based on a program.
resolveName :: (Name -> Int) -> (Name -> [Name])
resolveName f n
  | k <= 0 = error ("resolveName: unbound name " ++ n)
  | k == 1 = [ n ]
  | k >= 2 = map (\i -> indexName i n) [0 .. k - 1]
  where
    k = f n -- compute the frequency of n

-- |Removes an entry for @n@ from the name resolver.
rebindName :: Name -> (Name -> [Name]) -> (Name -> [Name])
rebindName n r m = if m == n then [ m ] else r m


-- * Indices

-- |Indexes all declarations.
indexDecls :: [Decl Name] -> [Decl Name]
indexDecls = concat . map (indexIfAmb 0) . groupBy eqName
  where
    indexIfAmb :: Int -> [Decl Name] -> [Decl Name]
    indexIfAmb _ [ ] = [ ]
    indexIfAmb _ [d] = [d]
    indexIfAmb i ds  = indexDecls i ds

    indexDecls :: Int -> [Decl Name] -> [Decl Name]
    indexDecls _ [] = []
    indexDecls i (d:ds) = indexDecl i d : indexDecls (i + 1) ds

-- |Indexes a declaration.
indexDecl :: Int -> Decl Name -> Decl Name
indexDecl i (Decl n e) = Decl (indexName i n) e

-- |Indexes a single name.
indexName :: Int -> Name -> Name
indexName i n = printf "%s+%d" n i


-- * Utility functions

-- |Determines whether two declarations declare the same name.
eqName :: Decl Name -> Decl Name -> Bool
eqName (Decl m _) (Decl n _) = m == n

-- |Calculates frequencies for all elements in a list.
freq :: Eq a => [a] -> (a -> Int)
freq [    ] = \_ -> 0
freq (x:xs) = \y -> (if x == y then 1 else 0) + freq xs y

-- |Extracts all declared names from a program.
declaredNames :: Prog Name -> [Name]
declaredNames (Prog ds) = map (\(Decl n _) -> n) ds
