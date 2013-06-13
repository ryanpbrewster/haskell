-- 317.hs
{-
 - See 317.lhs for the problem statement and solution analysis.
 - Use:
 -     lhs2TeX -o 317.tex 317.lhs
 -     pdflatex 317.tex
 -
 - to generate 317.pdf, which contains the details.
 -}
import Text.Printf

main = printf "%.4f\n" (solveProblem 9.81 100.0 20.0)

solveProblem :: Double -> Double -> Double -> Double
solveProblem g h v0 = (0.25*pi/g**3) * (v0**3 + 2.0*g*h*v0)**2
