{-# LANGUAGE Safe #-}

module GrzegorzOlszewski (typecheck, eval) where
import AST
import DataTypes

type Env a = [(Var,a)]
data Val = VInt Integer | VBool Bool deriving (Eq)    
data Type = TInt | TBool
type Error p = String 
data RunErr = RunErr

makeTIntPair :: Var -> (Var,Type)
makeTIntPair x = (x, TInt)

env_type_init :: [Var] -> [(Var,Type)]
env_type_init [] = []
env_type_init (x:xs) = map makeTIntPair (x:xs)

makeVIntPair :: (Var,Integer) -> (Var, Val)
makeVIntPair (var, int)= (var , (VInt int))

env_val_init :: [(Var,Integer)] -> [(Var,Val)]
env_val_init [] = []
env_val_init (x:xs) = map makeVIntPair (x:xs)

typecheck :: [Var] -> Expr p -> TypeCheckResult p
typecheck vars ex =
    case res of
        Left err -> Error (getData ex) err
        Right TBool -> Error (getData ex) "Program powinien zwracać wartość typu Int, a zwraca wartość typu bool."
        Right TInt -> Ok
        where 
            env = env_type_init vars
            res = infer env ex

eval :: [(Var,Integer)] -> Expr p -> EvalResult
eval xs ex = 
    case res of
        Left RunErr ->  RuntimeError
        Right (VInt val) -> (Value val)
        where 
            env = env_val_init xs
            res = evaluate env ex

check_if_arith_bin_op_type :: BinaryOperator -> Int

check_if_arith_bin_op_type op 
    -- return 1 if op is arithmetic operator
    | elem op [BAdd, BSub, BMul, BDiv, BMod] = 1
    -- return 2 if op is an operator that compares numbers
    | elem op [BEq, BNeq, BLt, BGt, BLe, BGe] = 2
    -- return 3 if op is logic operator
    | elem op [BAnd, BOr] = 3


infer :: Env Type -> Expr p -> Either (Error p) Type

infer env (ENum _ _) = Right TInt
infer env (EBool _ _) = Right TBool

infer env (EUnary _ UNeg e) =
    case infer env e of 
        Left err -> Left err
        Right TBool -> Left  "Jest bool, powinien być int."
        Right TInt -> Right TInt
infer env (EUnary _ UNot e) =
    case infer env e of 
        Left err -> Left err
        Right TInt -> Left  "Jest int, powinien być bool."
        Right TBool -> Right TBool



infer env (EBinary _ op e1 e2) =
    case check_if_arith_bin_op_type op of
        1 -> case infer env e1 of
            Left err -> Left err
            Right TBool -> Left  "Jest bool, powinien być int."
            Right TInt -> case infer env e2 of
                Left err -> Left err
                Right TBool -> Left  "Jest bool, powinien być int"
                Right TInt -> Right TInt
        2 -> case infer env e1 of
            Left err -> Left err
            Right TBool  -> Left  "Jest bool, powinien być int"
            Right TInt  -> case infer env e2 of
                Left err -> Left err
                Right TBool  -> Left  "Jest bool, powinien być int"
                Right TInt -> Right TBool
        3 -> case infer env e1 of
            Left err -> Left err
            Right TInt  -> Left  "Jest int, powinien być bool."
            Right TBool -> case infer env e2 of
                Left err -> Left err
                Right TInt  -> Left  "Jest int, powinien być bool."
                Right TBool -> Right TBool

infer env (EIf _ e1 e2 e3) =
    case infer env e1 of
        Left err -> Left err
        Right TInt -> Left  "Jest int powinien być bool"
        Right TBool -> case infer env e2 of
            Left err -> Left err
            Right TInt -> case infer env e3 of
                Left err -> Left err
                Right TBool -> Left  "Jest bool powienien być int"
                Right TInt -> Right TInt
            Right TBool-> case infer env e3 of
                Left err -> Left err
                Right TInt -> Left  "Jest int powienien być bool"
                Right TBool -> Right TBool

infer gamma (EVar _ x) = 
    case lookup x gamma of
        Just y -> Right y
        Nothing -> Left "Nie ma takiej zmiennej w środowisku."
infer gamma (ELet _ var e1 e2) =
    case infer gamma e1 of
        Left err -> Left err
        Right typ1 -> case infer gamma1 e2 of
            Left err -> Left err
            Right typ2 -> Right typ2
            where
                gamma1 = extendEnv var typ1 gamma

extendEnv :: Eq k => k -> v -> [(k, v)] -> [(k, v)]

extendEnv key value assoc = (key,value):(filter ((key /=).fst) assoc)

check_if_arith_bin_op_val :: BinaryOperator -> Integer 
check_if_arith_bin_op_val op 
    -- return 1 if op is arithmetic operator
    | elem op [BAdd, BSub, BMul] = 1
    -- return 2 if op is arithmetic operator and second argument shouldn't be 0
    | elem op [BDiv,BMod] = 2
    -- return 3 if op is an operator that compares numbers
    | elem op [BEq, BNeq, BLt, BGt, BLe, BGe] = 3
    -- return 4 if op is logic operator
    | elem op [BAnd, BOr] = 4

get_op op (x:xs) = y where
    Just y = lookup op (x:xs)

evaluate :: Env Val -> Expr p -> Either RunErr Val 

evaluate env (ENum _ x) =  Right (VInt x)
evaluate env (EBool _ x) =  Right (VBool x)
evaluate env (EUnary p UNeg x) = Right (VInt (-e))
    where Right (VInt e) = evaluate env x

evaluate env (EUnary p UNot x) =
    case evaluate env x of
        Right (VBool True) -> Right (VBool False)
        Right (VBool False) -> Right (VBool True)

evaluate env (EBinary p op ex1 ex2) = 
    case (check_if_arith_bin_op_val op) of
        1 -> case (evaluate env ex1, evaluate env ex2) of
            (Left RunErr, _) -> Left RunErr
            (_ , Left RunErr) -> Left RunErr
            (Right (VInt x1), Right (VInt x2)) -> Right (VInt (func x1 x2)) where
                func = get_op op [(BAdd, (+)), (BSub,(-)), (BMul,(*))]
        2 -> case (evaluate env ex1, evaluate env ex2) of
            (Left RunErr, _) -> Left RunErr
            (_ , Left RunErr) -> Left RunErr
            (_, Right (VInt 0)) -> Left RunErr
            (Right (VInt x1), Right (VInt x2)) -> Right (VInt (func x1 x2)) where
                func = get_op op [(BDiv, div), (BMod, mod)]
        3 -> case (evaluate env ex1, evaluate env ex2) of
            (Left RunErr, _) -> Left RunErr
            (_ , Left RunErr) -> Left RunErr
            (Right (VInt x1), Right (VInt x2)) -> Right (VBool (func x1 x2)) where
                func = get_op op [(BEq, (==)), (BNeq, (/=)), (BLt, (<)), (BGt, (>)), (BLe, (<=)), (BGe, (>=))]
        4 -> case (evaluate env ex1, evaluate env ex2) of
            (Left RunErr, _) -> Left RunErr
            (_ , Left RunErr) -> Left RunErr
            (Right (VBool x1), Right (VBool x2)) -> Right (VBool (func x1 x2)) where
                func = get_op op [(BAnd, (&&)), (BOr,(||))]  
                
evaluate env (EIf p ex1 ex2 ex3) = 
    case (evaluate env ex1,evaluate env ex2,evaluate env ex3) of
        (Left RunErr, _, _ ) -> Left RunErr
        (Right (VBool True), Left RunErr, _ ) -> Left RunErr
        (Right (VBool True), Right (VInt val) , _) -> Right (VInt val)
        (Right (VBool True), Right (VBool val) , _) -> Right (VBool val)
        (Right (VBool False), _, Left RunErr) -> Left RunErr
        (Right (VBool False),_, Right (VInt val)) -> Right (VInt val)
        (Right (VBool False ), _, Right (VBool val)) -> Right (VBool val)

evaluate env (EVar _ x) = Right y where
    Just y = lookup x env

evaluate env (ELet _ var e1 e2) = 
    case evaluate env e1 of
        Left RunErr -> Left RunErr
        Right (VInt x1Int) -> case evaluate env2 e2 of
            Left RunErr -> Left RunErr
            Right (VInt x2) -> Right (VInt x2)
            where env2 = extendEnv var (VInt x1Int) env
        Right (VBool x1Bool) -> case evaluate env2 e2 of
            Left RunErr -> Left RunErr
            Right (VInt x2) -> Right (VInt x2)
            where env2 = extendEnv var (VBool x1Bool) env
