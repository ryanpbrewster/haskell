foo x = let y = 2*x in bar y
    -- where bar z = y*x (does not compile, y is not in scope)
    where bar z = x*z
