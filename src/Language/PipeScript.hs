module Language.PipeScript
  ( Identifier,
    Variable,
    Platform,
    PlatformSet,
    PlatformFilter,
    Expression,
    Statement,
    BlockDefination,
    OperationType,
    TopLevel,
    AST,
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

data Expression
  = CallExpr Expression Expression
  | IdentifierExpr Identifier
  | VariableExpr Variable
  | ExpandExpr Expression
  | DoubleExpandExpr Expression
  | ConstantExpr String
  | ListExpr [Expression]
  deriving (Eq, Read, Show)

data Statement
  = AssignmentStat Variable Expression
  | ExpressionStat Expression
  | IfStat Expression [Statement] [Statement]
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
