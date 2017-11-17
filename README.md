# LCAvarsel
Variable selection for latent class analysis for model-based clustering of multivariate categorical data. 
 
The package implements a general framework for selecting the subset of variables with relevant clustering information 
and discard those that are redundant and/or not informative. Different algorithms are available to perform the selection: 
stepwise, swap-stepwise and evolutionary stochastic search. The package also allows the inclusion of concomitant 
covariates in the latent class analysis model, used to predict the class membership probabilities. 
The selection procedure can be run in parallel on multiple cores machines.

To install the package:
```
# install.packages("devtools")
devtools::install_github("michaelfop/LCAvarsel")
```

### References
Fop, M., Smart, K. and Murphy, T. B. (2017).<br>
**Variable selection for latent class analysis with application to low back pain diagnosis**.<br>
*Annals of Applied Statistics*. Forthcoming.

Dean, N. and Raftery, A. E. (2010).<br> 
**Latent class analysis variable selection.**<br> 
*Annals of the Institute of Statistical Mathematics*. 62(1):11-35.
