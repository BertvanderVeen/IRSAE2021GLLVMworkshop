# Introduction to R package gllvm
# IRSAE Summer School 4.8.2021
# author: Jenni Niku

# Load gllvm package
library(gllvm)

# Load spider data from mvabund package
# library(mvabund)
data("spider")
?spider
spider$abund
boxplot(spider$abund)
spider$x

## Model fitting ##
# 
# The main function of the package is gllvm(), see help:
?gllvm

# Fit GLLVM without covariates g(E(y_{ij})) = \beta_{0j} + u_i'\theta_j 
# with gllvm, number of latent variables can be defined with num.lv: 
fitnb <- gllvm(y = spider$abund, family = "negative.binomial", num.lv = 2)
fitnb
logLik(fitnb)
# predicted latent variables
getLV(fitnb)
# Species specific intercepts \beta_{0j}
fitnb$params$beta0



## Residual analysis ##
# 
# - Residual analysis can be used to  assess the appropriateness of 
#   the fitted model (eg. in terms of mean-variance relationship). 
# 
# - Randomized quantile/Dunn-Smyth residuals are used in the package, 
#   as they provide standard normal distributed residuals, even for 
#   discrete responses, in the case of a proper model.
#
# Function `residuals()` calculates randomized quantile residuals for the model:
?residuals
res <- residuals(fitnb)

# `plot()` function provides residual plots.
?plot.gllvm
par(mfrow = c(3,2))
plot(fitnb)



## Model selection ##
# 
# - Information criterias can be used for model selection.
#
# - For example, compare distributions or choose suitable number of latent variables.
#
# Compare the suitability of the response distributions:
fitp <- gllvm(y = spider$abund, family = poisson(), num.lv = 2)
fitnb <- gllvm(y = spider$abund, family = "negative.binomial", num.lv = 2)
AIC(fitp)
AIC(fitnb)
# NB seems to fit better
# Residual plots support the choice:
par(mfrow = c(2,2))
plot(fitp, which = 1:2)
plot(fitnb, which = 1:2)


# Choose the optimal number of latent variables:
# We already fitted a gllvm with 2 latent variables:
fitnb
# How about 1 or 3 LVs
fitnb1 <- gllvm(y=spider$abund, family = "negative.binomial", num.lv = 1)
fitnb1
fitnb3 <- gllvm(y=spider$abund, family = "negative.binomial", num.lv = 3)
fitnb3
# Lets look at the AIC values
AIC(fitnb1)
AIC(fitnb)
AIC(fitnb3)
# Lowest AIC with two latent variables


## Studying species associations
# 
# - Latent variables induce correlation across response variables, 
#   and so provide means of estimating correlation patterns across species, 
#   and the extent to which they can be explained by environmental variables.
# 
# - Information on correlation is stored in the LV loadings $\boldsymbol\theta_j$, 
#   so the residual covariance matrix, storing information on species co-occurrence 
#   that is not explained by environmental variables, can be calculated as 
#   $\boldsymbol\Sigma=\boldsymbol\Gamma \boldsymbol\Gamma^\top$, 
#   where $\boldsymbol\Gamma = [\boldsymbol\theta_1\dots\boldsymbol\theta_m]'$. 
# 
# - Let's consider first the correlation matrix based on a model without predictors (fitnb):
#   $g(E(y_{ij})) = \beta_{0j} + \boldsymbol{u}_i'\boldsymbol{\theta}_j$
# 
# `getResidualCov` function can be used to estimate the residual covariance 
#  matrix of the linear predictor across species. 
# `getResidualCor` function can be used to calculate the correlation matrix:
?getResidualCov
getResidualCov(fitnb)
?getResidualCor
cr <- getResidualCor(fitnb)
cr

# The obtained correlation matrix then does not take into account 
# the environmental conditions driving species abundances at sites (as they are not included in the model), 
# and reflects only what has been observed.

# The residual correlation matrix can be visualized using, eg., 
# a  `corrplot()` function from `corrplot` package:
library(corrplot);
par(mfrow = c(1,1))
corrplot(cr, diag = FALSE, type = "lower", method = "square", tl.srt = 25)


## GLLVM as a model based ordination method ##
# 
# -  GLLVMs can be used as a model-based approach to unconstrained ordination 
#    by including two latent variables in the model: 
#    g(E(y_{ij})) = \beta_{0j} + u_i'\theta_j
# 
# - The latent variable term try to capture the underlying factors 
#   driving species abundances at sites.
# 
# - Predictions for the two latent variables, \hat u_i=(\hat u_{i1}, \hat u_{i2}), 
#   then provide coordinates for sites in the ordination plot and then provides a graphical 
#   representation of which sites are similar in terms of their species composition.
# 
# We already fitted the GLLVM with 2 LVs (fitnb)
# `ordiplot()` produces ordination plots based on fitted GLLVMs.
?ordiplot
ordiplot(fitnb)
# Uncertainty of the ordination points in model based ordination can 
# be assessed with prediction errors of latent variables (calculated by 'getPredictErr()').
# Using predict.region = TRUE, 95% prediction ellipses can be plotted to the ordiplot.
par(mfrow=c(1,1))
ordiplot(fitnb, predict.region = TRUE, ylim=c(-2.5,2.5), xlim=c(-3,3.5))


## Biplot
# 
# - The ordination can also be used for visualizing the species associations 
#   by producing a biplot, (argument `biplot = TRUE` in `ordiplot()`), that is, 
#   by adding latent variable loadings ${\theta}_j$ to the ordination of sites.
# - In a biplot latent variables and their loadings are rotated so that 
#   the LV loadings of the species are in the same direction with the sites 
#   where they are most abundant.
# - The biplots can be used for finding groups of correlated species or 
#   finding indicator species common at specific sites.
par(mfrow=c(1,1))
ordiplot(fitnb, biplot = TRUE)
abline(h = 0, v = 0, lty=2)

# Environmental gradients
# 
# The potential impact of environmental variables on species communities 
# can be viewed by coloring ordination points according to the variables:

# The next is an arbitrary color palette, a vector length of 20, produced with 
# a function rbPal from package grDevices
# library(grDevices)
# rbPal <- colorRampPalette(c('mediumspringgreen', 'blue'))(20)
rbPal <- c("#00FA9A", "#00EC9F", "#00DFA4", "#00D2A9", "#00C5AF", "#00B8B4", 
           "#00ABB9", "#009DBF", "#0090C4", "#0083C9", "#0076CF", "#0069D4", 
           "#005CD9", "#004EDF", "#0041E4", "#0034E9", "#0027EF", "#001AF4", 
           "#000DF9", "#0000FF")
X <- spider$x
par(mfrow = c(2,3))
for(i in 1:ncol(X)){
  Col <- rbPal[as.numeric(cut(X[,i], breaks = 20))]
  ordiplot(fitnb, symbols = T, s.colors = Col, main = colnames(X)[i], 
           biplot = TRUE)
}


## Studying effects of environmental variables
# 
# - The effects of environmental variables on species can be 
#   studied by including environmental variables x_i to GLLVM: 
#   g(E(y_{ij})) = \beta_{0j} + x_i'\beta_{j} + u_i'\theta_j$.
# - \beta_{j} is a vector of species specific coefficients for environmental variables.
# Environmental covariates can be included with an argument X, 
# it's also a good practice to scale the environmental variables:
Xs <- scale(spider$x)
fitx <- gllvm(y = spider$abund, X = Xs, family = "negative.binomial")
fitx
# Point estimates for the environmental coefficients:
fitx$params$Xcoef

# Again, choose the number of LVs
fitx1 <- gllvm(y = spider$abund, X = Xs, family = "negative.binomial", num.lv = 1)
fitx2 <- gllvm(y = spider$abund, X = Xs, family = "negative.binomial", num.lv = 2)
fitx3 <- gllvm(y = spider$abund, X = Xs, family = "negative.binomial", num.lv = 3)
AIC(fitx1)
AIC(fitx2)
AIC(fitx3)
# Just one LV needed here

## Coefficient plot
# 
# `coefplot()` plots point estimates of the species specific environmental 
# coefficients \beta_{j} with confidence intervals:
coefplot(fitx1, mfrow = c(2,3), cex.ylab = 0.8)

# These can be compared to the ordination plot with colored environmental gradients:
# For example an environmental variable, `soil.dry` (soil dry mass) 
# which showed clear environmental gradient in the ordination: 
# Lets define colors for the two groups and draw the biplot again:
spc = ((fitnb$params$theta%*%diag(fitnb$params$sigma.lv))[,1]>0)*1+1
par(mfrow = c(1,2))
Col <- rbPal[as.numeric(cut(spider$x[,1], breaks = 20))]
ordiplot(fitnb, symbols = T, s.colors = Col, main = colnames(spider$x)[1], 
         biplot = TRUE, spp.colors = spc, xlim= c(-3,3))
abline(h=0,v=0, lty=2)
coefplot(fitx1, which.Xcoef = 1, cex.ylab = 0.8)


## Correlation matrix
# Correlation matrix for model with predictors shows correlation 
# patterns between species when the effect of the predictors are 
# taken into account.
crx <- getResidualCor(fitx1)
par(mfrow = c(1,1))
corrplot(crx, diag = FALSE, type = "lower", method = "square", tl.srt = 25)





#------------------------------------------------------------------
# Extra #
#------------------------------------------------------------------

#------------------------------------------------------------------
# Constrained ordination
#------------------------------------------------------------------
# - By fitting a constrained ordination, we can model the latent variables 
#   using the predictors. 
# The constrained model can be fitted by defining the number of constrained LVs
# using an argument 'num.lv.c':
fitC <- gllvm(y=spider$abund,X=X,family="poisson", num.lv.c = 2)
rbPal <- c("#00FA9A", "#00EC9F", "#00DFA4", "#00D2A9", "#00C5AF", "#00B8B4", "#00ABB9", "#009DBF", "#0090C4", "#0083C9", "#0076CF", "#0069D4", "#005CD9", "#004EDF", "#0041E4", "#0034E9", "#0027EF", "#001AF4", "#000DF9", "#0000FF")
par(mfrow = c(2,3))
for(i in 1:6){
  Col <- rbPal[as.numeric(cut(X[,i], breaks = 20))]
  ordiplot(fitC, symbols = TRUE, s.colors = Col, main = colnames(X)[i], biplot = TRUE)
}

#------------------------------------------------------------------
# Fourth corner models
#------------------------------------------------------------------
# - If species trait variables t_j, measuring eg. species behaviour 
#   or physical appearance, would be available, fourth corner models 
#   should be considered: 
# g(E(y_{ij})) = \beta_{0j} + x_i'\beta_{j} + x_i'B_{I}t_j  + u_i'\theta_j 
# 
# - Such models can also be fitted with `gllvm()` function by including 
#   a matrix of traits with an argument `TR`.
# 
# Using gllvm, also fourth corner models can be fitted if species trait variables are recorded
# Example for the antTraits data from mvabund package
data("antTraits")
antY<- antTraits$abund
antX<- antTraits$env
antT<- antTraits$traits
fitAnt <- gllvm(y = antY, X = antX, TR = antT, family = "negative.binomial")
fitAnt
# Between species correlations
rcx <- getResidualCor(fitAnt)
rcx
library(corrplot)
par(mfrow=c(1,1))
corrplot(rcx, diag = FALSE, type = "lower", method = "square", tl.srt = 25)

# biplot with 10 species
ordiplot(fitAnt, biplot = TRUE, ind.spp = 10, predict.region = TRUE)


# Coefficient plot
coefplot(fitAnt)
# Fourth corner coefficients:
fitAnt$fourth.corner
# Fourth corner can be plotted also with next lines
fourth = fitAnt$fourth.corner
library(lattice)
a = max( abs(fourth) )
colort = colorRampPalette(c("blue","white","red"))
plot.4th = levelplot(t(as.matrix(fourth)), xlab = "Environmental Variables",
                     ylab = "Species traits", col.regions = colort(100),
                     at = seq( -a, a, length = 100), scales = list( x = list(rot = 45)))
print(plot.4th)
