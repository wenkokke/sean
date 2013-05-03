{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module SeAn.Utils.AG where

-- |Allows abstraction over attribute synthesis.
class (HasSem a sem, HasInh a inh, HasWrap sem inh syn)
    => HasSyn a sem inh syn | a -> sem, a -> inh, a -> syn where
  syn :: a -> syn
  syn a = wrap (sem a) (inh a)
  
-- |Allows abstraction over attribute wrappers.
class HasWrap sem inh syn | sem -> inh,sem -> syn where
  wrap :: sem -> inh -> syn
  
-- |Allows abstraction over semantics generation.
class HasSem a sem | a -> sem where
  sem :: a -> sem
  
-- |Allows default inherited arguments.
class HasInh a inh | a -> inh where
  inh :: a -> inh
