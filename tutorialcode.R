library("umx")

data <- mtcars
  #disp = displacement of engine in cub.in
  #wt = weight of the engine 
  #mpg = miles per gallon of gas 

#Independence model (all factors are independent of each other)--------------------------------------------------------

model1 <- umxRAM("independence_model", data = mtcars,           
                 umxPath(v.m. = c("disp", "wt", "mpg")))        #this function assigns variances and means to factors
umxSummary(model1)
plot(model1)

#Model 2 -------------------------------------------------------------------------------------------------------------

model2 <- umxRAM("big and heavy", data = data,
                 umxPath(c("disp", "wt"), to = "mpg"),        #one headed paths from disp and wt to mpg 
                 umxPath(cov = c("disp", "wt")),              #allow predictors to covary
                 umxPath(v.m. = c("disp", "wt", "mpg"))        #give objects variance and mean
)
plot(model2)

umxCompare(model2, model1) #note, the AIC indicates the information that is "lost". THe lower the number, the better the model.
umxSummary(model2, show = "std")  #shows the parameter/path estimates. The standardized path co-efficients are shown. 

plot(model2, means = FALSE) #plots the model without the "mean" information

confint(model2, run = TRUE) #gives the confidence intervals of the model parameters 

#perform linear regression. Notice that the coefficients are different because you are taking into account different paths with UMX
linearmodel1 <- lm(mpg ~ 1 + disp + wt, data = mtcars)
coef(linearmodel1)
confint(linearmodel1)


# Dropping paths, to ultimately compare models ----------------------------------------------------------

#umxModify allows you to set a certain path coefficient to 0, effectively dropping it. 
model3 <- umxModify(model2, update = "disp_to_mpg", name = "disp_doesnt_matter")
umxCompare(model2, model3)

#you can add the "compare" command to the umxModify function 
model4 <- umxModify(model2, update = "wt_to_mpg", name = "drop effect of weight", comparison = TRUE)
  #again, models with higher AIC are worse models. 

#to discover labels/paths in a model:
umxGetParameters(model2)


# Chapter 9 - What you expect is what you get -----------------------------

##example 1
df <- myFADataRaw[, 1:2]
names(df) <- c("A", "B")
summary(lm(B ~ A, data = df))

manifests = names(df)
m1 <- umxRAM("A_causes_B", data = df, showEstimates = "std",
             umxPath("A", to = "B"),
             umxPath(var = manifests),
             umxPath(means = manifests)
             )

tmx_show(m1)
plot(m1)

##example 2
dimnames = c("RespOccAsp", "RespEduAsp", "FrndOccAsp", "FrndEduAsp", "RespParAsp", "RespIQ", "RespSES", "FrndSES", "FrndIQ", "FrndParAsp")
tmp = c(
  c(0.6247,
    0.3269, 0.3669,
    0.4216, 0.3275, 0.6404,
    0.2137, 0.2742, 0.1124, 0.0839,
    0.4105, 0.4043, 0.2903, 0.2598, 0.1839,
    0.3240, 0.4047, 0.3054, 0.2786, 0.0489, 0.2220,
    0.2930, 0.2407, 0.4105, 0.3607, 0.0186, 0.1861, 0.2707,
    0.2995, 0.2863, 0.5191, 0.5007, 0.0782, 0.3355, 0.2302,  0.2950,
    0.0760, 0.0702, 0.2784, 0.1988, 0.1147, 0.1021, 0.0931, -0.0438, 0.2087)
)
duncan = umx_lower2full(tmp, diag = FALSE, dimnames = dimnames)
str(duncan)
duncan <- mxData(duncan, type = "cov", numObs = 300)

respondentFormants   = c("RespSES", "FrndSES", "RespIQ", "RespParAsp")
friendFormants       = c("FrndSES", "RespSES", "FrndIQ", "FrndParAsp")
respondentOutcomeAsp = c("RespOccAsp", "RespEduAsp")
friendOutcomeAsp     = c("FrndOccAsp", "FrndEduAsp")
latentAspiration     = c("RespLatentAsp", "FrndLatentAsp")

m3 <- umxRAM("Duncan", data = duncan,
             umxPath(unique.bivariate = c(friendFormants, respondentFormants)), #allow exogenous variables to co-varry 
             umxPath(var = c(friendFormants, respondentFormants), fixedAt = 1), #fix variance of exogenous manifests/variables at 1
             umxPath(respondentFormants, to = "RespLatentAsp"), #paths from manifests to latent variables
             umxPath(friendFormants, to = "FrndLatentAsp"), 
             umxPath(var = latentAspiration), #I suppose this allows there to be residual variance in the latent variables
             umxPath(fromEach = latentAspiration, lbound = 0, ubound = 1), #allowing the latents to predict each other (not covary), but only positively. 
             umxPath("RespLatentAsp", to = respondentOutcomeAsp, firstAt = 1), #allowing latents to predict several outcome measures. Not sure why set at 1 at first.
             umxPath("FrndLatentAsp", to = friendOutcomeAsp, firstAt = 1), #something about firstAt = 1 setting the "scale" for latent paths. I suppose to make paths from latents comarable?
             umxPath(var = c(respondentOutcomeAsp, friendOutcomeAsp)), #allowing endogenous manifests have residual variances
             autoRun = TRUE
             )
plot(m3, std = TRUE)
