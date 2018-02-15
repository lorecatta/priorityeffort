parameters_exp_10 <- list(
    Exp = 10,
    FocusSites = seq(1,865,1),
    FocusSpecies = seq(1,138,1),
    NoIter = 1000000,
    NoLinks = 94,
    StartProp = 0.2,
    Temp_0 = 1,
    CoolingFactor = 0.99999,
    Runs = 1,
    Estimate = c(1,2,3), #with 1=PP_bestGuess; 2=norm_Lb; 3=norm_Ub
    CPValues = 0,
    TargetLevel = seq(250,10000,250),
    ProbPers = c(0.6, 0.7, 0.8, 0.9)
)                      

#Sets of targets
#c(seq(100,5000,100),seq(6000,15000,1000)) 
#seq(250,10000,250)
#1000

#Sets of CPValues
#0
#seq(0.0001,0.01,0.0003)
