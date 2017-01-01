\documentclass{article}
%include polycode.fmt
\usepackage{listings}
\lstnewenvironment{code}{\lstset{language=Haskell,basicstyle=\small}}{}

\newcommand{\atan}{\text{atan}}
\usepackage{amsmath, amssymb, fullpage}
\begin{document}
\section*{Problem Statement}
A firecracker explodes at a height of 100 m above level ground. It breaks into
a large number of very small fragments, which move in every direction; all of
them have the same initial velocity of 20 m/s.

We assume that the fragments move in a uniform gravitational field with
$g=9.81~\text{m}/\text{s}^2$.

Find the volume (in m$^3$) of the region through which the fragments move before
reaching the ground. Give your answer rounded to four decimal places.

\section*{Initial Analysis}
The objects start at a point $(0,0,h)$ and move with initial velocity $\vec{v}
= (v_x,v_y,v_z)$ where $v_x^2+v_y^2+v_z^2 = 20^2$.

Clearly the particles will trace out some sort of envelope. The envelope is
easiest to describe in polar coordinates. For instance, consider how high the
envelope is at $r=0$. The highest point will clearly be obtained by a particle
launched straight up at $v_0$. This particle reaches it's max height when all its vertical velocity has turned into potential energy, so
\[ \frac{1}{2}mv_0^2 = mg \Delta y \implies \Delta y = \frac{v_0^2}{2g}. \]
Thus, the height of the envelope at $r=0$ is
\[ z(0) = h + \frac{v_0^2}{2g}. \]
Intuitively, we know that the envelope gets smaller as $r$ grows. Let's see
exactly what the height of the envelope, $z(r)$, does as a function of $r$.

For a particle launched at angle $\theta$ from the horizontal, it traces out
a path
\[ (v_0 \cos \theta \cdot t, h + v_0 \sin\theta \cdot t - \frac{1}{2}gt^2 ). \]
The particle will reach a distance $r$ from the $z$-axis when
\[ v_0 \cos\theta \cdot t = r \implies t = \frac{r}{v_0 \cos\theta}. \]
The particle's height at that point is
\begin{align*}
z(r;\theta) &= h + v_0 \sin \theta \cdot t - \frac{1}{2}gt^2 \\
            &= h + v_0 \sin\theta \cdot \frac{r}{v_0\cos\theta} - \frac{1}{2}g \left(\frac{r}{v_0\cos\theta}\right)^2 \\
            &= h + r\tan \theta - \frac{gr^2}{2v_0^2\cos^2\theta} \\
\end{align*}
The highest particle at a distance $r$ is one where
\[ \frac{dz}{d\theta} = 0 \implies \theta = \atan \frac{v_0^2}{gr}. \]
Thus, the maximum height of any particle at a distance $r$ is
\[ z(r) = h + r \tan \theta - \frac{gr^2}{2v_0^2\cos^2\theta} = h + \frac{v_0^4-g^2r^2}{2gv_0^2}. \]

The envelope is positive everywhere where
\[ z(r) \geq 0 \implies r \leq r_\text{max} = \frac{v_0}{g}\sqrt{v_0^2+2gh}. \]
Thus, the volume of the envelope is
\begin{align*}
V &= \int_0^{r_\text{max}} \int_0^{2\pi} z(r) \cdot r dr d\theta \\
  &= 2\pi \int_0^{r_\text{max}} r z(r) \, dr = \frac{\pi}{4g^3} \left(v_0^3+2ghv_0\right)^2.
\end{align*}
\section*{Program}
Thus, the Haskell program solving this problem is very simple.
\begin{code}
import Text.Printf

main = printf "%.4f\n" (solveProblem 9.81 100.0 20.0)

solveProblem :: Double -> Double -> Double -> Double
solveProblem g h v0 = (0.25*pi/g**3) * (v0**3 + 2.0*g*h*v0)**2
\end{code}
\end{document}
