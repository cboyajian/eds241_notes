
library(tidyverse)
library(MASS)
library(vtable)
library(stargazer)
library(estimatr)

### Directory

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set's directory where script is located
getwd()


# IMPORT CSV DATA
CGL <- read.csv("cgl_collapse_data_extract.csv")


# SUMMARY STATISTICS
stargazer(CGL, type="text", digits=2)


# EXAMINE BALANCE IN COVARIATES
# COVARIATE MEAN DIFFERENCES by DAPever
m1 <- lm(formula = LME ~ DAPever, data=CGL)
m2 <- lm(formula = genus ~ DAPever, data=CGL)
m3 <- lm(formula = species ~ DAPever, data=CGL)
se_models = starprep(m1, m2, m3, stat = c("std.error"), se_type = "HC2", alpha = 0.05)
stargazer(m1, m2, m3, se = se_models, type="text")

# BOXPLOTS TO EXAMINE BALANCE IN COVARIATES
ggplot(CGL, aes(x=as.factor(DAPever), y=LME)) + 
  geom_boxplot(fill="cyan") + xlab("Ever Collapsed")

ggplot(CGL, aes(x=as.factor(DAPever), y=genus)) + 
  geom_boxplot(fill="cyan") + xlab("Ever Collapsed")

ggplot(CGL, aes(x=as.factor(DAPever), y=species)) + 
  geom_boxplot(fill="cyan") + xlab("Ever Collapsed")


# BASIC OLS by DAPever -- THEN ADD INDICATORS FOR OTHER COVARIATES 
# NOTE DO NOT INCLUDE SPECIES IN MODELS TO KEEP RUNNING TIME FAST
mA <- lm(formula = collapse ~ DAPever, data=CGL)
mB <- lm(formula = collapse ~ DAPever + as.factor(LME), data=CGL)
mC <- lm(formula = collapse ~ DAPever + as.factor(LME) + as.factor(genus), data=CGL)
se_models = starprep(mA, mB, mC, stat = c("std.error"), se_type = "HC2", alpha = 0.05)
stargazer(mA, mB, mC, se = se_models, type="text", omit = "(LME)|(genus)|(species)")



# BASIC PROPENSITY SCORE --- THIS IS A TOY MODEL
# ESTIMATE PROPENSITY SCORE MODEL AND PREDICT (EPS)
ps_model <- glm(DAPever ~ LME + genus, family = binomial(), data = CGL)
summary(ps_model)
EPS <- predict(ps_model, type = "response")
PS_WGT <- (CGL$DAPever/EPS) + ((1-CGL$DAPever)/(1-EPS))


# COLLECT ALL RELEVANT VARIABLES IN DATAFRAME
DF <- data.frame(years = CGL$years, collapse = CGL$collapse, DAPever = CGL$DAPever, 
                 LME = CGL$LME, genus = CGL$genus, species = CGL$species, EPS, PS_WGT)


# BOXPLOTS TO EXAMINE OVERLAP IN P-SCORE DISTRIBUTIONS
ggplot(DF, aes(x=as.factor(DAPever), y=EPS)) + 
  geom_boxplot(fill="cyan") + xlab("Ever Collapsed")


# WLS USING EPS WEIGHTS
wls1 <- lm(formula = collapse ~ DAPever, data=DF, weights=PS_WGT)
wls2 <- lm(formula = collapse ~ DAPever + LME + genus, data=DF, weights=PS_WGT)
se_models = starprep(wls1, wls2, stat = c("std.error"), se_type = "HC2", alpha = 0.05)
stargazer(wls1, wls2, se = se_models, type="text", omit = "(LME)|(genus)|(species)")