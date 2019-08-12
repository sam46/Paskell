-- Based on Stephen Diehl's work

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen where

import Data.ByteString.Short hiding (length)
import Data.Monoid ((<>))
import Data.Word
import Data.String
import Data.List
import Data.Function
import qualified Data.Map as Map

import Control.Applicative
import Control.Monad.State
import Control.Monad.Fail (fail, MonadFail)
import LLVM.AST
import LLVM.AST.Typed (typeOf)
import LLVM.AST.AddrSpace
import LLVM.AST.Type hiding (double)
import LLVM.AST.Global as G
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

import Data.Char (ord)

instance MonadFail Codegen where
    fail = error

-------------------------------------------------------------------------------
-- Module Level
-------------------------------------------------------------------------------

newtype LLVM a = LLVM (State AST.Module a)
  deriving (Functor, Applicative, Monad, MonadState AST.Module )

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM mod (LLVM m) = execState m mod

emptyModule :: ShortByteString ->  AST.Module
emptyModule label = defaultModule { moduleName = label }

-- | Include file path
emptyModulePath :: ShortByteString -> ShortByteString ->  AST.Module
emptyModulePath label path = defaultModule { moduleName = label, moduleSourceFileName = path }

addDefns :: [Definition] -> LLVM ()
addDefns ds = do
  forM ds $ \d -> 
    addDefn d
  return ()

addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs ++ [d] }

define ::  Type -> ShortByteString -> [(Type, Name, [A.ParameterAttribute])] -> Codegen [Definition] -> LLVM ()
define retty label argtys body = (addDefns bodydefs) >> addDefn (
  GlobalDefinition $ functionDefaults {
    name        = Name label
  , parameters  = ([Parameter ty nm att | (ty, nm, att) <- argtys], False)
  , returnType  = retty
  , basicBlocks = bls
  })
  where
    (bodydefs, bodystate) = runStateCodegen body (label)
    bls = createBlocks $ bodystate

external ::  Type -> ShortByteString -> [(Type, Name)] -> LLVM ()
external retty label argtys = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = Name label
  , linkage     = L.External
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = []
  }

gvar :: Type -> Name -> LLVM ()
gvar ty name  = addDefn $ gvar' ty name

gvar' :: Type -> Name -> Definition 
gvar' ty name  = 
  GlobalDefinition globalVariableDefaults
    { name = name
    , G.type' = ty
    , linkage = L.Weak
    , initializer = Nothing
    }

gstrVal :: Name -> String -> LLVM ()
gstrVal name val = addDefn $ gstrVal' name val

-- String literals are declared in global scope
gstrVal' :: Name -> String -> Definition
gstrVal' name val =    
  GlobalDefinition globalVariableDefaults
    { name = name
    , G.type' = arrType (length val) char
    , linkage = L.Private
    , unnamedAddr = Just GlobalAddr
    , isConstant = True
    , initializer = Just $ C.Array (IntegerType 8) (map constchar val)
    }
  where constchar c = C.Int 8 (toInteger $ ord c)

printf :: Definition
printf = GlobalDefinition $ functionDefaults
  { returnType = int
  , name = Name "printf"
  , parameters = ([Parameter str (UnName 0) []], True)
  }

printfTy :: Type
printfTy = PointerType {
  pointerReferent  = (FunctionType int [str] True), 
  pointerAddrSpace = AddrSpace 0
  }

-- construct function type given ret type and signature
toLLVMfnType :: Type -> [Type] -> Type
toLLVMfnType t ts = PointerType {
  pointerReferent  = (FunctionType t ts False), 
  pointerAddrSpace = AddrSpace 0
  }

fnPtr :: Name -> LLVM Type
fnPtr nm = findType <$> gets moduleDefinitions
  where
    findType defs =
      case fnDefByName of
        []   -> error $ "Undefined function: " ++ show nm
        [fn] -> ptr (typeOf fn)
        _    -> error $ "Ambiguous function name: " ++ show nm
      where
        globalDefs  = [g | GlobalDefinition g <- defs]
        fnDefByName = [f | f@(Function { name = nm' }) <- globalDefs, nm' == nm]

---------------------------------------------------------------------------------
-- Types (https://hackage.haskell.org/package/llvm-hs-pure-8.0.0/docs/src/LLVM.AST.Type.html#Type)
-------------------------------------------------------------------------------

-- IEEE 754 double

-- | An abbreviation for 'FloatingPointType' 'DoubleFP'
double :: Type
double = FloatingPointType DoubleFP

-- | An abbreviation for 'VoidType'
void :: Type
void = VoidType

int :: Type
int = i32

bool :: Type
bool = i1

char :: Type
char = i8

str :: Type
str = ptr i8

arrType :: Int -> Type -> Type
arrType len ty = ArrayType (fromIntegral $ len) ty

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
    fnName       :: Name
  , currentBlock :: Name                     -- Name of the active block to append to
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

-- emptyCodegen :: CodegenState
-- emptyCodegen = CodegenState (Name "block") (Name entryBlockName) Map.empty [] 1 0 Map.empty

emptyCodegen' :: ShortByteString -> CodegenState
emptyCodegen' name = CodegenState (Name name) (Name entryBlockName) Map.empty [] 1 0 Map.empty

-- execCodegen :: Codegen a -> CodegenState
-- execCodegen m = execState (runCodegen m) emptyCodegen

runStateCodegen :: Codegen a -> ShortByteString -> (a, CodegenState)
runStateCodegen m name = runState (runCodegen m) (emptyCodegen' name) 

-- increase instructions count. For unique names
fresh :: Codegen Word
fresh = do
  i <- gets count  -- (codegen, word codegen)
  modify $ \s -> s { count = 1 + i } -- (codegen,())
  return $ i + 1

freshStrName :: Codegen String
freshStrName = do
  n <- fresh
  (Name bname) <- getBlock
  return $ "str." ++ (BS.unpack.fromShort $ bname) ++ "." ++ (show n)

instr :: Type -> Instruction -> Codegen (Operand)
instr ty ins = do
  n <- fresh
  let ref = (UnName n)
  blk <- current
  let i = stack blk
  modifyBlock (blk { stack = (ref := ins) : i } ) -- add named instruction to current block's stack
  return $ local ({-ptr-} ty) ref

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
  (Name fname) <- getFnName
  bls <- gets blocks
  ix <- gets blockCount
  nms <- gets names
  let new = emptyBlock ix
      (qname, supply) = uniqueName (toShortBS $ (toString fname) ++ "." ++ (toString bname)) nms
  modify $ \s -> s { blocks = Map.insert (Name qname) new bls
                   , blockCount = ix + 1
                   , names = supply
                   }
  return (Name qname)
  where toShortBS = toShort . BS.pack 
        toString  = BS.unpack . fromShort

setBlock :: Name -> Codegen Name
setBlock bname = do
  modify $ \s -> s { currentBlock = bname }
  return bname

getBlock :: Codegen Name
getBlock = gets currentBlock

getFnName :: Codegen Name
getFnName = gets fnName

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
  modify $ \s -> s { symtab = (var, x) : lcls }

getvar :: ShortByteString -> Type -> Codegen Operand
getvar var ty = do
  syms <- gets symtab
  case lookup var syms of
    Just x  -> return x
    Nothing -> return $ getGvar var ty -- if not in symtab then it's a global, or doesn't exist
      -- error $ "unkown variable" ++ show var

getGvar :: ShortByteString -> Type -> Operand
getGvar var ty = ConstantOperand $ global ({-ptr-} ty) (Name var)

-------------------------------------------------------------------------------

local ::  Type -> Name -> Operand 
local = LocalReference

global :: Type -> Name -> C.Constant
global = C.GlobalReference

externf :: Type -> Name -> Operand 
externf ty nm = ConstantOperand (global ty nm)

-- Arithmetic and Constants
fadd :: Operand -> Operand -> Codegen Operand
fadd a b = instr float $ FAdd noFastMathFlags a b []

fsub :: Operand -> Operand -> Codegen Operand
fsub a b = instr float $ FSub noFastMathFlags a b []

fmul :: Operand -> Operand -> Codegen Operand
fmul a b = instr float $ FMul noFastMathFlags a b []

fdiv :: Operand -> Operand -> Codegen Operand
fdiv a b = instr float $ FDiv noFastMathFlags a b []

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

iadd :: Operand -> Operand -> Codegen Operand
iadd a b = instr int $ Add nowrap nowrap a b []

imul :: Operand -> Operand -> Codegen Operand
imul a b = instr int $ Mul nowrap nowrap a b []

isub :: Operand -> Operand -> Codegen Operand
isub a b = instr int $ Sub nowrap nowrap a b []

idiv :: Operand -> Operand -> Codegen Operand
idiv a b = instr int $ SDiv False a b []  

imod :: Operand -> Operand -> Codegen Operand
imod a b = instr int $ SRem a b []  

band :: Operand -> Operand -> Codegen Operand
band a b = instr bool $ And a b []  

bor :: Operand -> Operand -> Codegen Operand
bor a b = instr bool $ Or a b []  

-- toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
-- toArgs = map (\x -> (x, []))

-- Effects
call :: Operand -> [(Operand, [A.ParameterAttribute])] -> Codegen Operand
call fn args = do  -- figure out the signature, and typecast args as necessary
  let (opers, attrs) = unzip args
  let ts = extractParams fn
  tcastOpers <- mapM tycast (zip ts opers)
  -- error $ show tcastOpers
  instr (extractFnRetType fn) $ Call Nothing CC.C [] (Right fn) (zip tcastOpers attrs) [] []

callNoCast :: Operand -> [(Operand, [A.ParameterAttribute])] -> Codegen Operand
callNoCast fn args = instr (extractFnRetType fn) $ Call Nothing CC.C [] (Right fn) args [] []

-- UnNamed instruction Call. Used when return type is void
call' :: Operand -> [(Operand, [A.ParameterAttribute])] -> Codegen ()
call' fn args = do -- figure out the signature, and typecast args as necessary
  let (opers, attrs) = unzip args
  let ts = extractParams fn
  tcastOpers <- mapM tycast (zip ts opers)
  unnminstr $ Call Nothing CC.C [] (Right fn) (zip tcastOpers attrs) [] []
  -- unnminstr $ Call Nothing CC.C [] (Right fn) args [] []

alloca :: Type -> Codegen Operand
alloca ty = instr ty $ Alloca ty Nothing 0 []

-- same as alloca but returns a pointer to given data type (*ty)
alloca' :: Type -> Codegen Operand
alloca' ty = instr ty $ Alloca (ptr ty) Nothing 0 []

store :: Operand -> Operand -> Codegen ()
store ptrv val = 
  if (isFloat' ptrv) && (isIntVal $ val) 
  then (sitofp double val) >>=    -- typecast int to real
       \val' -> unnminstr $ Store False ptrv val' Nothing 0 []
  else unnminstr $ Store False ptrv val Nothing 0 [] 

load :: Type -> Operand -> Codegen Operand
load ty ptrv = instr ty $ Load False ptrv Nothing 0 []

-- Control Flow
br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

phi :: Type -> [(Operand, Name)] -> Codegen Operand
phi ty incoming = instr ty $ Phi ty incoming []

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []

retvoid :: Codegen (Named Terminator)
retvoid = terminator $ Do $ Ret Nothing []

-------------------------------------------------------------------------------
-- Helpers used for typecasting stuff
-------------------------------------------------------------------------------

zero :: Operand
zero = cons $ C.Int 32 0

getElementPtr :: Type -> Operand -> Codegen Operand
getElementPtr ty op = instr ty $ GetElementPtr True op [zero, zero] []

isIntVal x = case x of
  LocalReference (IntegerType _) _ -> True
  ConstantOperand (C.Int _ _) -> True
  _ -> False

isFloat x = case x of
  (FloatingPointType _) -> True
  (PointerType (FloatingPointType _) _) -> True
  _ -> False

isFloat' x = case x of
  (LocalReference (FloatingPointType _) _) -> True
  (ConstantOperand (C.GlobalReference (PointerType (FloatingPointType _) _) _) ) -> True
  _ -> False

tycast (dst, oper) = 
  if (isFloat dst) && (isIntVal oper)
  then sitofp double oper    -- typecast int to real
  else return oper 

extractParams (ConstantOperand (C.GlobalReference fn _)) = 
  case fn of (PointerType (FunctionType _ ts _) _) -> ts

extractFnRetType (ConstantOperand (C.GlobalReference fn _)) =
  case fn of (PointerType (FunctionType t _ _) _) -> t
