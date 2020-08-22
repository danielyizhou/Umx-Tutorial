library("umx")

data <- mtcars
  #disp = displacement of engine in cub.in
  #wt = weight of the engine 
  #mpg = miles per gallon of gas 

#Independence model (all factors are independent of each other)

model1 <- umxRAM("independence_model", data = mtcars,           
                 umxPath(v.m. = c("disp", "wt", "mpg")))        #this function assigns variances and means to factors
umxSummary(model1)
plot(model1)

#Model 2 

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


# Dropping paths ----------------------------------------------------------

#umxModify allows you to set a certain path coefficient to 0, effectively dropping it. 
model3 <- umxModify(model2, update = "disp_to_mpg", name = "disp_doesnt_matter")
umxCompare(model2, model3)

#you can add the "compare" command to the umxModify function 
model4 <- umxModify(model2, update = "wt_to_mpg", name = "drop effect of weight", comparison = TRUE)
  #again, models with higher AIC are worse models. 

#to discover labels/paths in a model:
umxGetParameters(model2)
