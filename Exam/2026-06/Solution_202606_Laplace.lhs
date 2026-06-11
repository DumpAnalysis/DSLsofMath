\begin{code}
{-# LANGUAGE RebindableSyntax #-}
module Solution_202606_Laplace where
import Prelude (Bool, Int, Rational, Eq((==)), (!!), otherwise, error, Ord((<), (>=)), length)
import DSLsofMath.Algebra
import DSLsofMath.PSDS
\end{code}
\paragraph{Solution}
% f''(t) + 4f'(t) + 3f(t) = -6e^{-2t},\quad f(0) = 0,\quad  f'(0) = 12
\begin{enumerate}
\item Power series version:
%
let |fs, fs', fs'', expm2 :: PS Rational| be such that
\begin{spec}
  map eval [fs, fs', fs'', expm2] == [f, f', f'', exp.((-2)*)]
\end{spec}

Then we have
\begin{spec}
  fs'' + scaleP 4 fs' + scaleP 3 fs = scaleP (-6) expm2
\end{spec}
which can be rearranged to
\begin{code}
fs, fs', fs'', expm2 :: Field a => PS a
fs''   = scaleP (-6) expm2 - scaleP 4 fs' - scaleP 3 fs
\end{code}

We also have the integrations and the base exponential series:
\begin{code}
fs     = integP 0   fs'
fs'    = integP 12  fs''
expm2  = integP 1   (scaleP (-2) expm2)
\end{code}

Now we can start filling in the coefficients:
\begin{enumerate}
\item Step 0:
\begin{spec}
  fs     !0 =  0
  fs'    !0 =  12
  expm2  !0 =  1
  fs''   !0 = -6*1 - 4*(12) - 3*0 = -6 - 48 = -54
\end{spec}

\item Step 1:
\begin{spec}
  fs     !1 =  12/1 = 12
  fs'    !1 = -54/1 = -54
  expm2  !1 = -2/1 = -2
  fs''   !1 = -6*(-2) - 4*(-54) - 3*(12) = 12 + 216 - 36 = 192
\end{spec}

\item Step 2:
\begin{spec}
  fs     !2 = -54/2 = -27
  fs'    !2 = 192/2 = 96
\end{spec}

\item Step 3:
\begin{spec}
  fs     !3 = 96/3 = 32
\end{spec}
\end{enumerate}

Thus
\begin{code}
test :: Bool
test = takeP 4 fs == P [0, 12, -27, 32 :: Rational]
\end{code}

\item Analytic solution with Laplace transform
\begin{enumerate}
  \item Step 0: Let |F = L f| below:
\begin{spec}
  LHS t = f''(t) + 4f'(t) + 3f(t)
  RHS t = -6*exp(-2*t)
\end{spec}
and |f 0  = 0|,  |f' 0 = 12|.
\item Step 1: Use the Laplace-D-law to compute |L f'|
\begin{spec}
  L f' s = L (D f) s = -0 + s*L f s = s*F s
\end{spec}
\item Step 2: Use the Laplace-D-law to compute |L f''|
\begin{spec}
  L f'' s         = -- Def. of |D|
  L (D f') s      = -- |f' 0 = 12|
  -12 + s*L f' s  = -- comp. above
  -12 + s*(s*F s) = -- simplify
  s^2*F s - 12
\end{spec}
\item Then we apply L to LHS:
\begin{spec}
  L (\t -> f''(t) + 4*f'(t) + 3*f(t)) s = -- Linearity, def. of |F|
  L f'' s + 4*L f' s + 3*L f s          = -- L-D-law results from above
  (s^2*F s - 12) + 4*s*F s + 3*F s      = -- Simplify
  (s^2 + 4*s + 3)*F s - 12              = -- Factor
  (s+1)*(s+3)*F s - 12
\end{spec}
\item and we apply L to the RHS
\begin{spec}
  L (\t -> -6*exp(-2*t)) s
= -- Linearity and Laplace law for exponentials
  -6/(s+2)
\end{spec}
By combining LHS=RHS, moving the constant, and dividing both sides by |(s+1)*(s+3)| we get:
\begin{spec}
  (s+1)*(s+3)*F s = 12 - 6/(s+2) = (12*(s+2) - 6)/(s+2) = 6*(2s + 3)/(s+2)
  F s = 6*(2s + 3)/((s+1)*(s+2)*(s+3))
\end{spec}
\item and can start with partial fraction decomposition. Ansatz:
\begin{spec}
  F s = A/(s+1) + B/(s+2) + C/(s+3)
\end{spec}
\item Multiply by |(s+1)*(s+2)*(s+3)| to get a polynomial equation:
\begin{spec}
  6*(2s + 3) = A*(s+2)*(s+3) + B*(s+1)*(s+3) + C*(s+1)*(s+2)
\end{spec}
\item Solve the polynomial equation by specialising to three cases (s=-1, -2, -3):
\begin{spec}
s=-1:    6 = A*(1)*(2)     <=>    6 =  2A   <=>  A = 3
s=-2:   -6 = B*(-1)*(1)    <=>   -6 =  -B   <=>  B = 6
s=-3:  -18 = C*(-2)*(-1)   <=>  -18 =  2C   <=>  C = -9 
\end{spec}
\item Thus
\begin{spec}
  F s   = 3/(s+1) + 6/(s+2) - 9/(s+3)
\end{spec}
\item which we can recognize as the transform of
\begin{spec}
  f t    =  3*exp(-t) + 6*exp(-2*t) - 9*exp(-3*t)
  f t    =  3*e1 + 6*e2 - 9*e3
\end{spec}
To simplify the checking expressions I write:
  e1 = exp(-t)
  e2 = exp(-2*t)
  e3 = exp(-3*t)
\item Checking: 
First, compute the derivatives symbolically:
\begin{spec}
  f' t   =  -3*e1 - 12*e2 + 27*e3
  f'' t  =   3*e1 + 24*e2 - 81*e3
\end{spec}
Then, check initial conditions, and the main equation.
\begin{spec}
f   0 =  3 +  6 -  9 =  0 -- OK!
f'  0 = -3 - 12 + 27 = 12 -- OK!
LHS t  =     ( 3*e1 + 24*e2 - 81*e3)
         + 4*(-3*e1 - 12*e2 + 27*e3)
         + 3*( 3*e1 +  6*e2 -  9*e3)
       = (3-12+9)*e1 + (24-48+18)*e2 + (-81+108-27)*e3
       = 0*e1 - 6*e2 + 0*e3
       = -6*exp(-2*t)
       = RHS t    -- OK!
\end{spec}

\end{enumerate}
\end{enumerate}
