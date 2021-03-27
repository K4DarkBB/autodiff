module Lib where

data Expr
  = X -- 変数 x
  | Lit Double -- 定数
  | Neg Expr -- -a
  | Add Expr Expr -- a + b
  | Mul Expr Expr -- a * b
  | Sin Expr -- sin a
  | Cos Expr -- cos a
  | Exp Expr -- e^a
  deriving (Show)

grad :: Expr -> Expr
grad X = Lit 1
grad (Lit _) = Lit 0
grad (Neg e) = Neg (grad e)
grad (Add e1 e2) = grad e1 `Add` grad e2
grad (Mul e1 e2) = (grad e1 `Mul` e2) `Add` (grad e2 `Mul` e1)
grad (Sin e) = Cos e
grad (Cos e) = Neg $ Cos e
grad (Exp e) = e

trans :: Expr -> Expr
trans (Mul (Lit 1) e) = trans e
trans (Neg (Neg e)) = trans e
trans (Mul e1 e2) = trans e2 `Mul` trans e1
trans (Add e1 e2) = trans e2 `Add` trans e1
trans e = e

--trans (Add Mul((Lit a) e) Mul((Lit b) e))=Mul (Lit (a+b)) e

eval :: Double -> Expr -> Double
eval a X = a
eval _ (Lit b) = b
eval a (Add e1 e2) = eval a e1 + eval a e2
eval a (Mul e1 e2) = eval a e1 * eval a e2
eval a (Sin e) = sin $ eval a e
eval a (Cos e) = cos $ eval a e
eval a (Exp e) = exp $ eval a e
