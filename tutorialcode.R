library("umx")

data <- mtcars

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

umxCompare(model2, model1)
