roll k xs = (take k xs):(roll k $ tail xs)
