
## TODO


-[ ] Check package to paper documentation consistency
-[ ] default value for spdur distribution? Document in paper and roxygen
-[ ] Can Table 1 include more variables? at risk
-[ ] Add method used to estimate parameter variance
-[ ] Short discussion of different cure rates and utility of split approach (simulations)

## Reviewer comments

> (1) check the consistence between the R documentation of the package and the paper. (e.g. the # of observations and variables on manual are not consistent with what it’s described in paper) 

> (2) for ‘spdur’ function, does user need to specify ‘distr’ or the default is Weibull? The first example ‘weib_model’ didn’t specify distribution but the manual doesn’t mention the default value. 

> (3) Can Table 1 include more variables? e.g. atrisk was included in the model but not listed in the table. I think listing those variables would help users better understand the data structure. 

> (4) add the method that was used to estimate the parameter variance. 

> (5) because the paper does not include any simulation results, would a short description of the performance of the split population model for different ‘cure’ rates enhance users’ confidence in using this package to re-analyze their data.