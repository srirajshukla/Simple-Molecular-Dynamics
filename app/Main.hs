module Main where

import Lib (mainNewton, mainVerlet, mainVerletSquare)

-- Possible Options to start simulation
-- 1. mainNewton
-- 2. mainVerlet
-- 3. mainVerletSquare
-- 
-- mainNewton Two atoms with newtonian calculations
-- mainVerlet Two atoms with verlet integration
-- mainVerletSquare A lattice of atoms with verlet integration

main :: IO ()
main = mainVerletSquare
