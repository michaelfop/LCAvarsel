# LCAvarsel
**Variable selection for latent class analysis for model-based clustering of multivariate categorical data**
 
Variable selection for latent class analysis for model-based clustering of multivariate categorical data. The package implements a general framework for selecting the subset of variables with relevant clustering information and discard those that are redundant and/or not informative. The variable selection method is based on the approach of Fop et al. (2015) and Dean and Raftery (2010). Different algorithms are available to perform the selection: stepwise, swap-stepwise and evolutionary stochastic search. Concomitant covariates used to predict the class membership probabilities can also be included in the latent class analysis model. The selection procedure can be run in parallel on multiple cores machines.

The package is available on CRAN, to install it:
```
install.packages("LCAvarsel")
```

To install the development version from GitHub:
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
