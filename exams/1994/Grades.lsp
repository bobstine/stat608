#|> (length grades)  108> (mean grades)  65.0093> (standard-deviation grades)  13.0244> (boxplot grades)> (quantile grades '(.25 .5 .75))  (56 65.5 74)> (plot-lines (kernel-dens grades))> (plot-points (sort-data grades)  ((/ (iseq 108) 108) )> (plot-points (sort-data grades)  (normal-quant (/ (iseq 1 108) 109)))|#(def grades (list64 81 71 6375 69 68 6279 57 79 72783454705652358083724958576378768471695855604843788938746764744861785575716451665971733446746445607458557862656170867369466175528474817267757455634369638859645382715689657471835438527556836536))