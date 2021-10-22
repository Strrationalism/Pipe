module Language.PipeScript
  ( Identifier (..),
    Variable (..),
    Constant (..),
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

data PlatformSet
  = AnyPlatform
  | PlatformSet [Identifier]
  deriving (Eq, Read, Show)

data PlatformFilter = PlatformFilter
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
    platformFilter :: PlatformFilter,
    block :: [Statement]
  }
  deriving (Eq, Read, Show)

data OperationType
  = NormalOperation
  | BeforeAction
  | AfterAction
  deriving (Eq, Read, Show)

data TopLevel
  = OperationDefination OperationType BlockDefination
  | TaskDefination BlockDefination
  | ActionDefination BlockDefination
  | Include FilePath
  deriving (Eq, Read, Show)

type AST = [TopLevel]
