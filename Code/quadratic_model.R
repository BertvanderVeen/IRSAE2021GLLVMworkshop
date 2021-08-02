# Full vignette here: https://jenniniku.github.io/gllvm/articles/vignette5.html
# Other vignettes here:  https://jenniniku.github.io/gllvm/articles/

# Load gllvm R-package
library(gllvm)
library(corrplot) # Please install if you haven't yet

# Load spider data
data("spider")

# Assign the data, feel free to replace with your own!
Y <- spider$abund
family <-  "poisson" # We use the Poisson distribution, the data are counts

# Assigning for purpose of reproducibility of the exercise
# Could be any number, or could be NULL
seed = 98

# First, fit a model with equal tolerances
# This means a random row intercept
ftEqTol <- gllvm(Y, family = family, row.eff = "random", num.lv = 2, seed = seed)

# Second, fit a model with species-common tolerances
# This means one quadratic coefficient per LV
ftComTol <- gllvm(Y, family = family, num.lv = 2, quadratic = "LV", seed = seed)

# Third, fit a model with species-specific tolerances
# This means a quadratic coefficient per species and LV
ftUneqTol <- gllvm(Y, family = family, num.lv = 3, quadratic = TRUE, seed = seed)

# Compare them
AIC(ftEqTol,ftComTol,ftUneqTol)

# Have a look at the model summary
summary(ftUneqTol)

# Can also show parameter estimates
summary(ftUneqTol, theta = T, spp.intercepts = T) # Woah, that's a lot!

# Extract species optima for LVs
optima(ftUneqTol)

# Exctract species tolerances
tolerances(ftUneqTol)

#Residual variance per latent variable
# For the linear term
getResidualCov(ftUneqTol)$var.q

# For the quadratic term
getResidualCov(ftUneqTol)$var.q2

# Species correlations
spec_cor <- getResidualCor(ftUneqTol)
corrplot::corrplot(spec_cor, type = "lower", diag = F, order = "AOE")

# Ordination diagram
ordiplot(ftUneqTol, biplot=TRUE, spp.arrows = TRUE)
