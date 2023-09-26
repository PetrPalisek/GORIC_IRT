# Install mirt if needed
# install.packages("mirt")

library(mirt)

df <- read.csv("transreas.csv")


# Getting the elements ----------------------------------------------------


# Fit a unidimensional 2PL model
# Two parameters per item: difficulty (intercept) and discrimination (loading)
# The SE = TRUE needs to be used, otherwise, no vcov element is output
fit <- mirt(df[,3:12], model = 1, itemtype = "2PL", SE = T)

# Get parameter estimates
mirt::coef(fit)

# You can also get the traditional IRT parametrization, which would be more useful to users
mirt::coef(fit, IRTpars = T)

# Get fit statistics
mirt::M2(fit)

# Get the var-cov matrix of estimates
mirt::vcov(fit)

# Get AIC
mirt::extract.mirt(fit, "AIC")


# Constraints --------------------------------------------------------------


# (1) No constraints, just to show the syntax style
# F1 loads on items 1-5, F2 on 6-10 and these latents are correlated
model_2fac <- "
               F1 = 1-5
               F2 = 6-10
               COV = F1*F2"

fit_2fac <- mirt(df[,3:12], model = model_2fac, itemtype = "2PL", SE = T)


# (2) Constrain all discrimination parameters (loadings) to 1
# This corresponds to parameters a1 for F1 and a2 for F2
model_2fac_constrained <- "
               F1 = 1-5
               F2 = 6-10
               COV = F1*F2
               CONSTRAIN = (1-5, a1), (6-10, a2)"

fit_2fac_constrained <- mirt(df[,3:12], model = model_2fac_constrained, itemtype = "2PL", SE = T)

# Gather the usual suspects
mirt::coef(fit_2fac_constrained)
mirt::vcov(fit_2fac_constrained)
mirt::extract.mirt(fit_2fac_constrained, "AIC")
mirt::M2(fit_2fac_constrained)

# Possible hypotheses:
#                       - do my data conform to the Rasch measurement requirements?
#                          (i.e., are all as = 1)
#                       - are the items ordered by difficulty like I hypothesized?
#                          (e.g., are bundles of items *increasing* in difficulty?)
#                       - are some items really discriminating worse than others?
#                       - in 3PL models, are some items easily guessed than others?
#                          (e.g., based on a priori information on item principles)


# (3) Multigroup constraints

# Fabricate a grouping var so it's simpler
df$gender <- rbinom(nrow(df), 1, .5)

# Loadings and difficulties should be equal in both groups in all items
# This can be done using an argument to multipleGroup, but this way is more flexible
group_model <- "F1 = 1-10
          CONSTRAINB = (1-10, a1), (1-10, d)"

# Fit a multigroup 2PL model with gender as grouping var, estimate group means and vars for both groups
fit_group <- mirt::multipleGroup(df[,3:12], model = group_model, SE = T, group = factor(df$gender),
                                 itemtype = "2PL", invariance = c("free_means", "free_vars"))

mirt::coef(fit_group)
mirt::vcov(fit_group) # names look weird
mirt::extract.mirt(fit_group, "AIC")
mirt::M2(fit_group)

# Possible hypotheses:
#                       - are the latent means rising / decreasing as expected by theory?
#                         (e.g., older children should perform better and better)
#                       - is a specific item harder, the younger you are?
#                       - are older children able to better guess the correct answer?