{-# LANGUAGE
  DefaultSignatures,
  FlexibleContexts,
  TypeOperators,
  UndecidableInstances,
  FlexibleInstances
#-}

module Simplify(
  simplify
  ) where

  import GHC.Generics(Generic, Rep, from, to)
  import Language.Haskell.TH.Syntax(Name(Name), NameFlavour(NameS))
  import GHC.Generics(M1(M1), D1)
  import GHC.Generics((:+:)(L1, R1))
  import GHC.Generics(C1)
  import GHC.Generics((:*:)((:*:)))
  
  import GHC.Generics(S1, K1(K1))
  import GHC.Generics(U1(U1))
  import Language.Haskell.TH.Syntax
  import Data.Ratio(Ratio)
  import Data.Char(Char)
  import Data.Word(Word8)

  class Simplified e where
    simplify :: e -> e
    default simplify :: (Generic e, GSimplified (Rep e)) => e -> e
    simplify = to . gsimplify . from
  instance Simplified Name where
    simplify (Name occ _) = Name occ NameS
  class GSimplified f where
    gsimplify :: f r -> f r
  instance (GSimplified a) => GSimplified (D1 d a) where
    gsimplify (M1 a) = M1 $ gsimplify a
  instance (GSimplified a, GSimplified b) => GSimplified (a :+: b) where
      gsimplify (L1 x) = L1 $ gsimplify x
      gsimplify (R1 x) = R1 $ gsimplify x
  instance (GSimplified a) => GSimplified (C1 c a) where
      gsimplify (M1 x) = M1 $ gsimplify x
  instance (GSimplified a, GSimplified b) => GSimplified (a :*: b) where
      gsimplify (a :*: b) = gsimplify a :*: gsimplify b
  instance (GSimplified a) => GSimplified (S1 e a) where
      gsimplify (M1 x) = M1 $ gsimplify x
  instance (Simplified a) => GSimplified (K1 i a) where
      gsimplify (K1 f) = K1 $ simplify f
  instance GSimplified U1 where
      gsimplify U1 = U1
  instance Simplified t => Simplified (Maybe t)
  instance Simplified t => Simplified [t]
  instance (Simplified t1, Simplified t2) => Simplified (t1, t2)
  instance (Simplified t1, Simplified t2, Simplified t3) => Simplified (t1, t2, t3)
  instance Simplified Exp
  instance Simplified Guard
  instance Simplified Stmt
  instance Simplified Dec
  instance Simplified Clause
  instance Simplified Pat
  instance Simplified Lit
  instance Simplified Type
  instance Simplified TyVarBndr
  instance Simplified TyLit
  instance Simplified Body
  instance Simplified Con
  instance Simplified Bang
  instance Simplified SourceUnpackedness
  instance Simplified SourceStrictness
  instance Simplified DerivClause
  instance Simplified DerivStrategy
  instance Simplified FunDep
  instance Simplified Overlap
  instance Simplified Foreign
  instance Simplified Callconv
  instance Simplified Safety
  instance Simplified Fixity
  instance Simplified FixityDirection
  instance Simplified Pragma
  instance Simplified Inline
  instance Simplified RuleMatch
  instance Simplified Phases
  instance Simplified RuleBndr
  instance Simplified AnnTarget
  instance Simplified TySynEqn
  instance Simplified TypeFamilyHead
  instance Simplified FamilyResultSig
  instance Simplified InjectivityAnn
  instance Simplified Role
  instance Simplified Match
  instance Simplified PatSynArgs
  instance Simplified PatSynDir
  instance Simplified Range
  instance Simplified Int where
    simplify = id
  
  instance Simplified Word8 where
    simplify = id
  
  instance Simplified Char where
    simplify = id
  
  instance Simplified Integer where
    simplify = id
  
  instance Simplified (Ratio Integer) where
    simplify = id
  
  instance Simplified Bytes where
    simplify = id
