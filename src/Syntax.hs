{-# OPTIONS_GHC -Wall #-}
module Syntax where 

data Expr = Var String          -- Змінна
          | Const Integer       -- константа
          | BinOp Op Expr Expr  -- Операція
             deriving (Show, Eq)
-- Бінарні (2-аргумента) оператори
data Op =  Plus | Minus | Times | Div | Mod  
             deriving (Show, Eq)
data Stmt = Assign String Expr
          | Read String 
          | Write Expr
          | If Expr Stmt 
          | While Expr Stmt       
          | Call String [String] 
          | Block [String] [(String,Proc)] [Stmt]        
             deriving (Show, Eq)
--  В операторах if (iv) s і while (iv) s значення v>0 цілого виразу iv 
--                еквівалентно логічному значенню True 			 
type Proc =  ([String],Stmt)
type Program = Stmt

data Static  = VarSt | ProcSt Int deriving (Show, Eq)
type EnvStat = [(String,Static)]

type Work = ([Integer], [Maybe Integer], [Integer])

data ParseOpt = ParserO | LibraryO deriving (Eq, Show) 
data SemanOpt = WorkO | StateO | ReaderO | ApplicO deriving (Eq, Show)


-- Програми -------------------------------------------
{- Вводить ціле значення n, якщо воно невідємне,
   то обчислює і виводить його факторіал.
   В іншому випадку виводиться 0. 
   { int n, p, f;  
     proc fact (p,f)
       { if (p) {
		   f:= f*p; p:=p-1;
           fact (p,f) 		   
		 }
	   } 	 
	 read n; f := 0;
     if (n+1) 
      {f:=1; p := n; fact (p,f)};
	 write f 
   }
-}

factorial :: Program
factorial = Block ["n","p","f"]
     [("fact",(["p","f"],
          Block [] [] 
           [ If (Var "p") 
              (Block [] []
               [ Assign "f" (BinOp Times (Var "f") (Var "p"))
               , Assign "p" (BinOp Minus (Var "p")  (Const 1))
               , Call "fact" ["p","f"]
               ])
           ] ))
     ]
     [ Read "n", Assign "f" (Const 0)
     , If (BinOp Plus (Var "n") (Const 1)) 
          (Block [] [] 
             [ Assign "f" (Const 1) 
             , Assign "p" (Var "n")
             , Call "fact" ["p","f"]
             ])
      , Write (Var "f")
     ]

factorialTest :: String 
factorialTest = 
   "{ int n, p, f; \n\
   \   proc fact (p,f)\n\
   \    {if (p) {\n \
   \       f:= f*p; p:=p-1; fact (p,f)}\n \
   \    }\n\
   \   read n; f := 0; if (n+1) \n\
   \    {f:=1; p := n; fact (p,f)}; \n\
   \   write f  \n\
   \}"

{- Вводить ціле значення n, 
   якщо воно додатнє, то обчислює і виводить 
   всі меньші від n прості числа. 
   { int n, i, s;  
     proc smp (i1,s1)
       { int j; j:=2; s1 := 1;
         while (i1-j) {
		   if (1 - i1 % j) {s1:=0;j:=i1};
           j:=j+1 		   
		 }
       } 	   
     read n; i:=2;
	 while (n-i)
       {smp(i,s); if(s) write i; 
		i := i+1} 
    }
-}

prime :: Program 
prime = Block ["n","i","s"]
              [("smp",(["i1","s1"],
                     Block ["j"] [] 
                        [ Assign "j" (Const 2), Assign "s1" (Const 1)
                        , While (BinOp Minus (Var "i1") (Var "j"))
                          (Block [][]
                             [If (BinOp Minus (Const 1) (BinOp Mod (Var "i1") (Var "j")))
                                   (Block [] [] [ Assign "s1" (Const 0) 
                                                , Assign "j" (Var "i1")])
                             , Assign "j" (BinOp Plus (Var "j") (Const 1)) ])  
                        ]
                       ))]
              [ Read "n", Assign "i" (Const 2)
              ,  While (BinOp Minus (Var "n") (Var "i"))
                   (Block [][]
                     [ Call "smp" ["i","s"]
                     , If (Var "s")(Write (Var "i"))
                     , Assign "i" (BinOp Plus (Var "i") (Const 1))
                     ])
              ]

primeTest :: String
primeTest =
   "{ int n, i, s; \n\
   \  proc smp (i1,s1)\n\
   \    { int j; j:=2; s1 := 1;\n \
   \      while (i1-j) {\n\
   \         if (1 - i1 % j) {s1:=0;j:=i1};\n\
   \         j:=j+1 \n\
   \      }\n\
   \    }    \n\
   \  read n; i:=2;\n\
   \  while (n-i)\n\
   \     {smp(i,s); if(s) write i;\n\
   \      i := i+1}\n\
   \}"

{- Вводить два цілі значення a і b, 
   процедура print - виводить іх значення,
   процедура exchange - міняє їх значення місцями
   exch: [6,8] ==> [6,8,8,6] 

   { int a, b;  
     proc print() {write a; write b}
     proc exchange (a1,b1) 
       {int j; j:= a1; a1 :=b1; b1:=j}	 
     read a; read b;
	 read a; read b; print();
	 exchange (a,b); print () 
   }
-}

exch :: Program
exch = Block ["a","b"]
             [("print",
                 ([],Block [] []
                      [Write (Var "a"), Write (Var "b")]
                  ))
             ,("exchange",
                  (["a1","b1"], Block ["j"] []
                    [Assign "j" (Var "a1"),Assign "a1" (Var "b1"), Assign "b1" (Var "j")]
                  )
              )
             ] 
             [ Read "a", Read "b", Call "print" []
             , Call "exchange" ["a", "b"]
             , Call "print" [] 
             ]

exchTest :: String
exchTest =
   "{ int a, b; \n\
   \   proc print() {write a; write b}\n\
   \   proc exchange (a1,b1)\n \
   \     {int j; j:= a1; a1 :=b1; b1:=j}\n \
   \   read a; read b; print(); \n\
   \   exchange (a,b); print () \n\
   \       \n\
   \}"
