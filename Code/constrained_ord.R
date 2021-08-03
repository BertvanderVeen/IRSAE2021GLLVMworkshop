# Full vignette here: https://jenniniku.github.io/gllvm/articles/vignette6.html
# Other vignettes here:  https://jenniniku.github.io/gllvm/articles/
# Requires gllvm version 1.3.1 or later

# Load gllvm R-package
library(gllvm)

# Load spider data
data(spider)
Y <- spider$abund
X <- spider$x  # Predictors of spider data
family <- "negative.binomial" # We use the negative binomial distribution

# Assigning for purpose of reproducibility of the exercise
# Could be any number, or could be NULL
seed = 3521

colnames(X) # Have a quick look what's in here!
X <- scale(X) # Good practice to scale the predictors, to improve convergence

# Fit a multivariate GLM
MGLM <- gllvm(Y, X=X, family = family, num.lv = 0, seed = seed)

# Now fit a constrained ordination, purely fixed-effects.
# We could use lv.formula to specify predictors
# If we don't, all predictors in X are used
RRGLM <- gllvm(Y, X=X, family = family, num.RR = 2, seed = seed)

# Now fit a constrained ordination, but including the residual
CGLLVM <- gllvm(Y, X=X, family = family, num.lv.c = 2, seed = seed)

# Now we use the formula interface to perform partial constrained ordination
# formula is the argument for the predictors outside of the ordination
# lv.formula is the argument for the predictors in the ordination
PCGLLVM <- gllvm(Y, X=X, family = family, num.lv.c = 2, seed = seed,
                 lv.formula = ~bare.sand+fallen.leaves+moss+herb.layer+reflection,
                 formula = ~soil.dry)

# Have a look at the different models
summary(MGLM) # Woah, that's a lot!
summary(RRGLM) # OK, this is a little better  :)
summary(CGLLVM) # Ew, significance has changed.
summary(PCGLLVM) # Spot the difference  with the previous models?

# Plot the three ordination diagrams next to each other
par(mfrow=c(1,3))

# The arrow.scale argument allows us to make the arrows shorter/longer
# Intensity of the arrow presents the statistical uncertainty
# Opaque arrows indicate CI that includes zero for at least one LV
# CI can be turned off using the arrow.ci = F argument
# Sorted by AIC from (high) left to right (low)
ordiplot(RRGLM, biplot = TRUE, main = "Constrained", arrow.scale = 0.7)
ordiplot(CGLLVM, biplot=TRUE, main = "Constrained with residual")
ordiplot(PCGLLVM, biplot = TRUE,  main = "paritally Constrained with residual")

# So we haven't yet fitted the model "partially Constrained without residual".
# Do you know how to do that?
