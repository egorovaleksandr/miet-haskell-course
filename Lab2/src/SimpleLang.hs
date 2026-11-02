module SimpleLang where

data Expression =
    Var String
  | Val Integer
  | Op Expression Bop Expression
  deriving (Show, Eq)

data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt
  | Ge
  | Lt
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement
  | For      Statement  Expression Statement Statement
  | Block [Statement]
  | Skip
  deriving (Show, Eq)

type State = String -> Integer

-- Задание 1 -----------------------------------------

empty :: State
empty _ = 0

extend :: State -> String -> Integer -> State
extend state var newVal v = if v == var then newVal else state v

-- Задание 2 -----------------------------------------

eval :: State -> Expression -> Integer
eval state (Var s) = state s
eval _     (Val n) = n
eval state (Op e1 op e2) =
    let v1 = eval state e1
        v2 = eval state e2
    in case op of
        Plus   -> v1 + v2
        Minus  -> v1 - v2
        Times  -> v1 * v2
        Divide -> v1 `div` v2
        Gt     -> if v1 > v2  then 1 else 0
        Ge     -> if v1 >= v2 then 1 else 0
        Lt     -> if v1 < v2  then 1 else 0
        Le     -> if v1 <= v2 then 1 else 0
        Eql    -> if v1 == v2 then 1 else 0

-- Задание 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar Skip               = DSkip
desugar (Assign s e)       = DAssign s e
desugar (Incr s)           = DAssign s (Op (Var s) Plus (Val 1))
desugar (If e s1 s2)       = DIf e (desugar s1) (desugar s2)
desugar (While e s)        = DWhile e (desugar s)
desugar (For init cond step body) =
    DSequence (desugar init)
              (DWhile cond (DSequence (desugar body) (desugar step)))
desugar (Block [])         = DSkip
desugar (Block [s])        = desugar s
desugar (Block (s:ss))     = DSequence (desugar s) (desugar (Block ss))

-- Задание 4 -----------------------------------------

runSimpler :: State -> DietStatement -> State
runSimpler state DSkip = state
runSimpler state (DAssign s e) = extend state s (eval state e)
runSimpler state (DIf e s1 s2) =
    if eval state e /= 0 then runSimpler state s1 else runSimpler state s2
runSimpler state (DWhile e s) =
    if eval state e /= 0
    then runSimpler (runSimpler state s) (DWhile e s)
    else state
runSimpler state (DSequence s1 s2) = runSimpler (runSimpler state s1) s2

run :: State -> Statement -> State
run state = runSimpler state . desugar

-- Программы -------------------------------------------

factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))

-- Задание 5 -----------------------------------------

squareRoot :: Statement
squareRoot = Block
    [ Assign "B" (Val 0)
    , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B"))) (Incr "B")
    , Assign "B" (Op (Var "B") Minus (Val 1))
    ]

-- Задание 6 -----------------------------------------

fibonacci :: Statement
fibonacci = Block
    [ Assign "F0" (Val 1)
    , Assign "F1" (Val 1)
    , If (Op (Var "In") Eql (Val 0))
         (Assign "Out" (Var "F0"))
         (If (Op (Var "In") Eql (Val 1))
             (Assign "Out" (Var "F1"))
             (For (Assign "C" (Val 2))
                  (Op (Var "C") Le (Var "In"))
                  (Incr "C")
                  (Block
                    [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                    , Assign "F0" (Var "F1")
                    , Assign "F1" (Var "T")
                    , Assign "Out" (Var "T")
                    ])))
    ]