setwd("Z:/Predict Voter Position/")
library(sqldf)

library(haven)
library(psych)

CHES_experts <- read_dta("CHES2019_experts.dta")


CHES_experts_reduced6 <- sqldf("SELECT immigrate_policy, redistribution, environment, econ_interven, civlib_laworder, sociallifestyle FROM CHES_experts")
# convert covariances to correlations
correlations <- cov2cor(covariances)
correlations

correlations6 <- cov2cor(covariances6)
correlations6

fa.promax <- fa(correlations, nfactors = 14, rotate="none", fm = "pa")
fa.promax$e.values



fa.promax <- fa(correlations6, nfactors = 6, rotate="none")
fa.promax$e.values

CHES_experts_reduced <- sqldf("SELECT immigrate_policy, multiculturalism, redistribution, environment, spendvtax, deregulation, econ_interven, civlib_laworder, sociallifestyle, religious_principles, ethnic_minorities, nationalism, urban_rural, protectionism, regions FROM CHES_experts")
####Exploratory factor analysis

CHES_experts_reduced_wo_NA <- na.omit(CHES_experts_reduced)
CHES_experts_reduced6_wo_NA <- na.omit(CHES_experts_reduced6)

covariances <- cov(CHES_experts_reduced_wo_NA)
covariances6 <- cov(CHES_experts_reduced6_wo_NA)
# convert covariances to correlations
correlations <- cov2cor(covariances)
correlations

correlations6 <- cov2cor(covariances6)
correlations6


# determine number of factors to extract
set.seed(1234) # make results reproducible

library(psych)
library(psychTools)

psych::scree(correlations6, main = "Scree plot for 6 variables", factors = TRUE, pc = TRUE)

psych::scree(correlations, main = "Scree plot for 15 variables", factors = TRUE, pc = FALSE)

#for the table
fa.parallel(correlations6)$fa.values
fa.parallel(correlations)$fa.values




vss(correlations, n = 10, n.obs = 1969, rotate = "promax")
vss(correlations6, n = 5, n.obs = 2912, rotate = "promax")

# Listing 14.8 - Factor extraction with oblique rotation
fa.promax <- fa(correlations, nfactors=2, rotate="promax", fm="pa")

# calculate factor loading matrix
fsm <- function(oblique) {
  if (class(oblique)[2]=="fa" & is.null(oblique$Phi)) {
    warning("Object doesn't look like oblique EFA")
  } else {
    P <- unclass(oblique$loading)
    F <- P %*% oblique$Phi
    colnames(F) <- c("PA1", "PA2")
    return(F)
  }
}
fsm(fa.promax)


# Listing 14.8 - Factor extraction with oblique rotation
fa.promax <- fa(correlations, nfactors=2, rotate="promax", fm="pa")

colnames(fa.promax$loadings) <- c("Factor A", "Factor B")
rownames(fa.promax$loadings) <- c("immigration", "multiculturalism", "redistribution", "environment", "spending vs. taxes", "deregulation", "state intervention", "civil liberties", "social lifestyle", "religious principles", "ethnic minorities", "nationalism", "urban vs. rural", "trade liberalization", "decentralization")

fa.promax6 <- fa(correlations6, nfactors=2, rotate="promax", fm="pa")

colnames(fa.promax6$loadings) <- c("Factor A", "Factor B")
rownames(fa.promax6$loadings) <- c("immigration", "redistribution", "environment", "state intervention", "civil liberties", "social lifestyle")

#factor.plot(fa.promax, labels=c("immigration policy", "multiculturalism vs. assimilation", "redistribution", "environment", "spending vs. taxes", "deregulation of markets", "state intervention in the economy", "civil liberties vs. law and order", "social lifestyle (e.g. rights for homosexuals, gender equality)", "role of religious principles in politics", "position towards ethnic minorities", "cosmopolitanism vs. nationalism", "urban vs. rural interests", "trade liberalization vs. protectionism", "people vs. elected representatives"))
fa.diagram(fa.promax, simple=FALSE, rsize = 0.25, adj = 2, main = "Factor analysis for the CHES2019 (15 policies)")
fa.diagram(fa.promax6, simple=FALSE, rsize = 0.25, adj = 2, main = "Factor analysis for the CHES2019 (6 policies)")

# plot factor solution
factor.plot(fa.promax, labels=c("immigration policy", "multiculturalism vs. assimilation", "redistribution", "environment", "spending vs. taxes", "deregulation of markets", "state intervention in the economy", "civil liberties vs. law and order", "social lifestyle (e.g. rights for homosexuals, gender equality)", "role of religious principles in politics", "position towards ethnic minorities", "cosmopolitanism vs. nationalism", "urban vs. rural interests", "trade liberalization vs. protectionism", "people vs. elected representatives"))
fa.diagram(fa.promax,  simple=FALSE, main = "Factor analysis for the CHES2019 (15 variables)")
#?fa.diagram
# factor scores


png("fa_diagram_CHES2019.png", width = 3000, height = 1600, res = 300, type = "cairo")
fa.diagram(
  fa.promax,
  rsize = 0.2,
  simple = FALSE,
  main = "Factor analysis for the CHES2019\n(15 variables)",
  adj = 2,
  size = 0.5,
  marg = c(.5, 0, 2, .5)   # <-- increase left margin (2nd number)
)
dev.off()

png("fa_diagram_CHES2019.png", width = 1600, height = 2200, res = 300)

fa.diagram(
  fa.promax,
  simple = FALSE,
  main = "Factor analysis for the CHES2019 (15 variables)",
  marg = c(.5, 1, 1, .5),  # bottom, LEFT, top, right  <-- increase this
  cex  = 6,              # shrink box text a bit
  l.cex = 1              # shrink numbers on arrows a bit
)

dev.off()

fa.promax$weights

png("fa_diagram.png", width = 2400, height = 1600, res = 300)
psych::fa.diagram(fa_fit)
dev.off()
