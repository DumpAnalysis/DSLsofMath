\begin{code}
{-# LANGUAGE GADTs #-}
module Solution_202606_Algebra where
import qualified Prelude
import Prelude(Bool(..), (&&), (/=), Double, String, (==), map, id)
\end{code}

\paragraph{Solution}

\begin{enumerate}
\item \textbf{[5p]} Define a type class |Field|.
\begin{code}
class Field f where
    add     :: f -> f -> f
    mul     :: f -> f -> f
    zero    :: f
    one     :: f
    negate  :: f -> f
    recip   :: f -> f
\end{code}

\item \textbf{[5p]} Define a datatype |F v| and its |Field| instance.
\begin{code}
data F v where
    Add     :: F v -> F v -> F v
    Mul     :: F v -> F v -> F v
    Zero    :: F v
    One     :: F v
    Negate  :: F v -> F v
    Recip   :: F v -> F v
    Var     :: v -> F v

instance Field (F v) where
    add     = Add
    mul     = Mul
    zero    = Zero
    one     = One
    negate  = Negate
    recip   = Recip
\end{code}

\item \textbf{[5p]} Find and implement two other instances of the |Field| class.
\begin{code}
instance Field Double where
    add     = (Prelude.+)
    mul     = (Prelude.*)
    zero    = 0
    one     = 1
    negate  = Prelude.negate
    recip   = Prelude.recip

xor :: Bool -> Bool -> Bool
xor = (/=)

instance Field Bool where
    add     = xor
    mul     = (&&)
    zero    = False
    one     = True
    negate  = id
    recip   = id
\end{code}

\item \textbf{[5p]} Give a type signature for, and define, a general evaluator.
\begin{code}
eval :: Field f => (v -> f) -> F v -> f
eval env = e where
    e (Add x y)   = add  (e x)  (e y)
    e (Mul x y)   = mul  (e x)  (e y)
    e Zero        = zero
    e One         = one
    e (Negate x)  = negate  (e x)
    e (Recip x)   = recip   (e x)
    e (Var v)     = env v
\end{code}

\item \textbf{[5p]} Specialise the evaluator and compute results.
\begin{code}
evalD :: (v -> Double)  ->  (F v -> Double)
evalD = eval
evalB :: (v -> Bool)    ->  (F v -> Bool)
evalB = eval

-- Three expressions
e1, e2, e3 :: F String
e1 = Mul (Var "x") (Recip (Var "x"))      -- |x/x|
e2 = Recip (Add (Add One One) One)        -- |1/3|
e3 = Add (Var "y") (Negate (Var "z"))     -- |y-z|

-- Assignments
envD :: String -> Double
envD "x" = 3.14
envD "y" = 7
envD "z" = 2

envB :: String -> Bool
envB "x" = True
envB "y" = False
envB "z" = False

-- Hand-computation of results expected on the exam:
-- evalD on e1: 3.14 * (1/3.14) = 1.0
-- evalD on e2: 1 / (1+1+1) = 0.333...
-- evalD on e3: 7 + (-2) = 5.0

-- evalB on e1: True && True = True
-- evalB on e2: id(True /= True /= True) = True
-- evalB on e3: False /= False = False

tests :: (Bool, Bool)
tests = (  map (evalD envD)  [e1,e2,e3]  ==  [1, 1 Prelude./ 3, 5]
        ,  map (evalB envB)  [e1,e2,e3]  ==  [True, True, False] )
\end{code}
\end{enumerate}
