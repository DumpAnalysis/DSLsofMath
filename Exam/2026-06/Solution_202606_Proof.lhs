\paragraph{Solution}

\begin{enumerate}
\item \textbf{[5p]} Define the additive unit |zeroE| and prove it is the right-additive unit.

\begin{code}
zeroE :: Additive a => Endo a
zeroE = \_ -> zero
\end{code}

\begin{spec}
    addE f zeroE
  = -- Def. addE
    lift2 (+) f zeroE
  = -- Def. lift2
    \x -> (+) (f x) (zeroE x)
  = -- Def. zeroE
    \x -> (+) (f x) zero
  = -- Additive group axiom
    \x -> f x
  = -- Eta reduction
    f
\end{spec}

\item \textbf{[5p]} Define the multiplicative unit |oneE| and prove it is the right-multiplicative unit.

\begin{code}
oneE :: Endo a
oneE = \x -> x  -- or oneE = id
\end{code}

\begin{spec}
    mulE f oneE
  = -- Def. mulE
    f . oneE
  = -- Def. (.)
    \x -> f (oneE x)
  = -- Def. oneE
    \x -> f x
  = -- Eta reduction
    f
\end{spec}

\item \textbf{[15p]} Prove distributivity: |Endo(f) => mulE f (addE g h) == addE (mulE f g) (mulE f h)|

Assume |Endo(f)|, meaning |f ((+) x y) == (+) (f x) (f y)|.
\begin{spec}
    mulE f (addE g h) 
  = -- Def. mulE
    f . addE g h
  = -- Def. (.)
    \x -> f (addE g h x)
  = -- Def. addE
    \x -> f (lift2 (+) g h x)
  = -- Def. lift2
    \x -> f ((+) (g x) (h x))
  = -- Use Endo(f) = H2(f,(+),(+))
    \x -> (+) (f (g x)) (f (h x))
  = -- Def. (.) twice (backwards)
    \x -> (+) ((f . g) x) ((f . h) x)
  = -- Def. lift2 (backwards)
    lift2 (+) (f . g) (f . h)
  = -- Def. addE (backwards)
    addE (f . g) (f . h)
  = -- Def. mulE twice (backwards)
    addE (mulE f g) (mulE f h)
\end{spec}
\end{enumerate}
