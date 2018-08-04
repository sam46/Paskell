{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen where

import Data.ByteString.Short
import Data.Monoid ((<>))
import Data.Word
import Data.String
import Data.List
import Data.Function
import qualified Data.Map as Map

import Control.Applicative
import Control.Monad.State

import LLVM.AST
import LLVM.AST.Typed (typeOf)
import LLVM.AST.AddrSpace
import LLVM.AST.Type
import LLVM.AST.Global
import qualified LLVM.AST as AST

import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.IntegerPredicate as IP


import LLVM.Context
import LLVM.Module
import qualified Data.ByteString.Char8 as BS

-------------------------------------------------------------------------------
-- Module Level
-------------------------------------------------------------------------------

newtype LLVM a = LLVM (State AST.Module a)
  deriving (Functor, Applicative, Monad, MonadState AST.Module )

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM mod (LLVM m) = execState m mod

emptyModule :: ShortByteString -> AST.Module
emptyModule label = defaultModule { moduleName = label }

addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs ++ [d] }

define ::  Type -> ShortByteString -> [(Type, Name)] -> Codegen a -> LLVM ()
define retty label argtys body = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = Name label
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = bls
  }
  where
    bls = createBlocks $ execCodegen $ 
      do
        -- body ptrThisType
        body
    -- ptrThisType = PointerType {
    --     pointerReferent = FunctionType {
    --         resultType = retty
    --       , argumentTypes = map fst argtys
    --       , isVarArg = False
    --     }
    --   , pointerAddrSpace = AddrSpace 0
    --   }

external ::  Type -> ShortByteString -> [(Type, Name)] -> LLVM ()
external retty label argtys = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = Name label
  , linkage     = L.External
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = []
  }

fnPtr :: Name -> LLVM Type
fnPtr nm = findType <$> gets moduleDefinitions
  where
    findType defs =
      case fnDefByName of
        []   -> error $ "Undefined function: " ++ show nm
        [fn] -> PointerType (typeOf fn) (AddrSpace 0)
        _    -> error $ "Ambiguous function name: " ++ show nm
      where
        globalDefs  = [g | GlobalDefinition g <- defs]
        fnDefByName = [f | f@(Function { name = nm' }) <- globalDefs, nm' == nm]

---------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- IEEE 754 double
double :: Type
double = FloatingPointType DoubleFP

void :: Type
void = AST.VoidType

-------------------------------------------------------------------------------
-- Names
-------------------------------------------------------------------------------

type Names = Map.Map ShortByteString Int

uniqueName :: ShortByteString -> Names -> (ShortByteString, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm,  Map.insert nm 1 ns)
    Just ix -> (nm <> fromString (show ix), Map.insert nm (ix+1) ns)

-------------------------------------------------------------------------------
-- Codegen State
-------------------------------------------------------------------------------

type SymbolTable = [(ShortByteString, Operand)]

data CodegenState
  = CodegenState {
    currentBlock :: Name                     -- Name of the active block to append to
  , blocks       :: Map.Map Name BlockState  -- Blocks for function
  , symtab       :: SymbolTable              -- Function scope symbol table
  , blockCount   :: Int                      -- Count of basic blocks
  , count        :: Word                     -- Count of unnamed instructions
  , names        :: Names                    -- Name Supply
  } deriving Show

data BlockState
  = BlockState {
    idx   :: Int                            -- Block index
  , stack :: [Named Instruction]            -- Stack of instructions
  , term  :: Maybe (Named Terminator)       -- Block terminator
  } deriving Show

-------------------------------------------------------------------------------
-- Codegen Operations
-------------------------------------------------------------------------------

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState )

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, (BlockState _ s t)) = BasicBlock l (reverse s) (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: " ++ (show l)

entryBlockName :: ShortByteString
entryBlockName = "entry"

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

emptyCodegen :: CodegenState
emptyCodegen = CodegenState (Name entryBlockName) Map.empty [] 1 0 Map.empty

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen

-- increase instructions count. For unique names
fresh :: Codegen Word
fresh = do
  i <- gets count  -- (codegen, word codegen)
  modify $ \s -> s { count = 1 + i } -- (codegen,())
  return $ i + 1

instr :: Type -> Instruction -> Codegen (Operand)
instr ty ins = do
  n <- fresh
  let ref = (UnName n)
  blk <- current
  let i = stack blk
  modifyBlock (blk { stack = (ref := ins) : i } ) -- add named instruction to current block's stack
  return $ local ty ref

unnminstr :: Instruction -> Codegen ()
unnminstr ins = do
  blk <- current
  let i = stack blk
  modifyBlock (blk { stack = (Do ins) : i } )

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
  blk <- current
  modifyBlock (blk { term = Just trm })
  return trm

-------------------------------------------------------------------------------
-- Block Stack
-------------------------------------------------------------------------------

entry :: Codegen Name
entry = gets currentBlock

-- given block name
addBlock :: ShortByteString -> Codegen Name
addBlock bname = do
  bls <- gets blocks
  ix <- gets blockCount
  nms <- gets names
  let new = emptyBlock ix
      (qname, supply) = uniqueName bname nms
  modify $ \s -> s { blocks = Map.insert (Name qname) new bls
                   , blockCount = ix + 1
                   , names = supply
                   }
  return (Name qname)

setBlock :: Name -> Codegen Name
setBlock bname = do
  modify $ \s -> s { currentBlock = bname }
  return bname

getBlock :: Codegen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = Map.insert active new (blocks s) }

current :: Codegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x -> return x
    Nothing -> error $ "No such block: " ++ show c

-------------------------------------------------------------------------------
-- Symbol Table
-------------------------------------------------------------------------------

assign :: ShortByteString -> Operand -> Codegen ()
assign var x = do
  lcls <- gets symtab
  modify $ \s -> s { symtab = [(var, x)] ++ lcls }

getvar :: ShortByteString -> Codegen Operand
getvar var = do
  syms <- gets symtab
  case lookup var syms of
    Just x  -> return x
    Nothing -> error $ "Local variable not in scope: " ++ show var

-------------------------------------------------------------------------------

-- References:
--  refer to already declared variables

local ::  Type -> Name -> Operand   -- refer to variable defined in local scope (function)
local = LocalReference

global :: Type -> Name -> C.Constant -- refer to a global function
global = C.GlobalReference

externf :: Type -> Name -> Operand 
externf ty nm = ConstantOperand (C.GlobalReference ty nm)

-- Arithmetic and Constants
fadd :: Operand -> Operand -> Codegen Operand
fadd a b = instr float $ FAdd NoFastMathFlags a b []

fsub :: Operand -> Operand -> Codegen Operand
fsub a b = instr float $ FSub NoFastMathFlags a b []

fmul :: Operand -> Operand -> Codegen Operand
fmul a b = instr float $ FMul NoFastMathFlags a b []

fdiv :: Operand -> Operand -> Codegen Operand
fdiv a b = instr float $ FDiv NoFastMathFlags a b []

fcmp :: FP.FloatingPointPredicate -> Operand -> Operand -> Codegen Operand
fcmp cond a b = instr float $ FCmp cond a b []

icmp :: IP.IntegerPredicate -> Operand -> Operand -> Codegen Operand
icmp cond a b = instr bool $ ICmp cond a b []

cons :: C.Constant -> Operand
cons = ConstantOperand

uitofp :: Type -> Operand -> Codegen Operand
uitofp ty a = instr float $ UIToFP a ty []

sitofp :: Type -> Operand -> Codegen Operand
sitofp ty a = instr float $ SIToFP a ty []

nowrap :: Bool
nowrap = False

int :: Type
int = IntegerType 32
bool :: Type
bool = IntegerType 1

iadd :: Operand -> Operand -> Codegen Operand
iadd a b = instr int $ Add nowrap nowrap a b []

imul :: Operand -> Operand -> Codegen Operand
imul a b = instr int $ Mul nowrap nowrap a b []

isub :: Operand -> Operand -> Codegen Operand
isub a b = instr int $ Sub nowrap nowrap a b []


toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

-- Effects
call :: Operand -> [Operand] -> Codegen Operand
call fn args = instr float $ Call Nothing CC.C [] (Right fn) (toArgs args) [] []

alloca :: Type -> Codegen Operand
alloca ty = instr float $ Alloca ty Nothing 0 []

store :: Operand -> Operand -> Codegen ()
store ptr val = unnminstr $ Store False ptr val Nothing 0 []

load :: Operand -> Codegen Operand
load ptr = instr float $ Load False ptr Nothing 0 []

-- Control Flow
br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

phi :: Type -> [(Operand, Name)] -> Codegen Operand
phi ty incoming = instr float $ Phi ty incoming []

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []

retvoid :: Codegen (Named Terminator)
retvoid = terminator $ Do $ Ret Nothing []


  
module_ :: AST.Module
module_ = defaultModule
    { moduleName = "basic"
    , moduleDefinitions = []
    }


toLLVM :: AST.Module -> IO ()
toLLVM mod = withContext $ \ctx -> do
    llvm <- withModuleFromAST ctx mod moduleLLVMAssembly
    BS.putStrLn llvm