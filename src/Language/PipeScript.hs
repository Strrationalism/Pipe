module Language.PipeScript
  ( Identifier (..),
    Variable (..),
    Constant (..),
    Platform (..),
    PlatformSet (..),
    PlatformFilter (..),
    Expression (..),
    Statement (..),
    BlockDefination (..),
    OperationType (..),
    TopLevel (..),
    AST (..),
  )
where

newtype Identifier = Identifier String
  deriving (Eq, Read, Show)

newtype Variable = Variable Identifier
  deriving (Eq, Read, Show)

newtype Platform = Platform Identifier
  deriving (Eq, Read, Show)

data PlatformSet
  = AnyPlatform
  | Platforms [Platform]
  deriving (Eq, Read, Show)

data PlatformFilter = PlatformFromAndTo
  { platformFor :: PlatformSet,
    platformTo :: PlatformSet
  }
  deriving (Eq, Read, Show)

data Constant
  = ConstInt Int
  | ConstStr String
  | ConstNumber Double
  | ConstBool Bool
  deriving (Eq, Read, Show)

data Expression
  = IdentifierExpr Identifier
  | VariableExpr Variable
  | ConstantExpr Constant
  | ExpandExpr Expression
  | DoubleExpandExpr Expression
  | ApplyExpr Expression [Expression]
  | ListExpr [Expression]
  deriving (Eq, Read, Show)

data Statement
  = ExprStat Expression
  | IfStat [(Expression, [Statement])] [Statement]
  deriving (Eq, Read, Show)

data BlockDefination = BlockDefination
  { name :: Identifier,
    parameters :: [Variable],
    platformUsage :: PlatformFilter,
    block :: [Statement]
  }
  deriving (Eq, Read, Show)

data OperationType
  = NormalOperation
  | BeforeAction Identifier
  | AfterAction Identifier
  deriving (Eq, Read, Show)

data TopLevel
  = OperationDefination BlockDefination OperationType
  | TaskDefination BlockDefination
  | ActionDefination BlockDefination
  | Include String
  deriving (Eq, Read, Show)

type AST = [TopLevel]
