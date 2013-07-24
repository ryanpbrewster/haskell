\documentclass{article}
%include polycode.fmt
\usepackage{listings, amsmath, amssymb}
\lstnewenvironment{code}{\lstset{language=Haskell,basicstyle=\small}}{}


\newcommand{\pp}[2]{\frac{\partial #1}{\partial #2}}

\newcommand{\atan}{\text{atan}}
\usepackage{amsmath, amssymb, fullpage}
\begin{document}
\section*{Problem Statement}
Let $S_m = (x_1, x_2, \ldots, x_m)$ be the $m$-tuple of positive real numbers
with $x_1 + x_2 + \cdots + x_m = m$ for which $P_m = x_1 x_2^2 \cdots x_m^m$ is
maximised. For example, it can be verified that $\lfloor P_{10} \rfloor = 4112$.

Find \[ \sum_{m=2}^{15} \lfloor P_m \rfloor. \]

\section*{Solution}
Using the method of undetermined multipliers, we can maximize $P_m$ with
respect to the constraint that $x_1 + \cdots + x_m = m$. We do this by
maximizing the auxiliary function
\[ \Lambda = x_1x_2^2\cdots x_m^m - \lambda (x_1 + \cdots + x_m - m), \]
or, even easier, by maximizing
\begin{align*}
\Lambda' &= \log(x_1x_2^2\cdots x_m^m) - \lambda (x_1 + \cdots + x_m - m) \\
         &= \log x_1 + 2\log x_2 + \cdots + m\log x_m - \lambda (x_1+\cdots+x_m-m).
\end{align*}
This is fairly straightforward. Set the gradient equal to zero, so
\[ \pp{\Lambda'}{x_i} = 0 \implies \frac{i}{x_i} - \lambda = 0 \implies x_i = \frac{i}{\lambda}, \]
where $\lambda$ is determined by
\[ x_1 + \cdots + x_m = \frac{i}{\lambda} + \cdots + \frac{m}{\lambda} = \frac{1}{\lambda}\frac{m(m+1)}{2} = m \implies \lambda = \frac{m+1}{2}. \]

Finally, we can directly calculate $P_m$.
\[ P_m = x_1 x_2^2 \cdots x_m^m = \left( \frac{1}{\lambda} \right)^1 \left( \frac{2}{\lambda} \right)^2 \cdots  \left( \frac{m}{\lambda} \right)^m. \]

The Haskell code for this is pretty straightforward.
\lstinputlisting[language=Haskell, firstline=10,lastline=13]{190.hs}
\end{document}
