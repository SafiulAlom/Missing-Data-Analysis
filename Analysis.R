
#------------------------------------------------------------------------------------#  
#-----------------------Tree-based Multiple Imputation methods-----------------------#
#----A simulation study with a large sample empirical data basis, where RF-MI and----# 
#-------------------------CART-MI methods are compared-------------------------------#
#------------------------------------------------------------------------------------#

#first remove all active objects
remove(list = ls())

#function for installing and loading required packages
install.load.packages = function(pack, packagePath){
  all.packages = pack[!(pack %in% installed.packages(lib.loc = packagePath)[, 1])]
  if (length(all.packages)) 
    install.packages(all.packages, lib = packagePath, dependencies = TRUE)
  sapply(pack, require, lib.loc = packagePath, character.only = TRUE)
}

#list of all necessary packages 
packages =  c('dplyr', 'rpart', 'randomForest', 'mice', 'VIM')

#install and load packages
libraryPath = 'C:\\Users\\Himel\\OneDrive\\Studium\\R\\Packages'
install.load.packages(packages, libraryPath)


#load dataset
data = read.csv("C:\\Users\\Himel\\OneDrive\\Studium\\M.Sc. Statistics\\3_Semester
\\Statistical analysis of missing data\\presentation\\bank-additional\\bank-additional\\bank-additional-full.csv", sep = ";")
#data source
browseURL('https://archive.ics.uci.edu/ml/datasets/Bank+Marketing#')

#-----------------------
#Input variables:
#-----------------------
# bank client data:
#1 - age (numeric)
#2 - job : type of job (categorical: 'admin.','blue-collar','entrepreneur','housemaid',
#    'management','retired','self-employed','services','student','technician','unemployed','unknown')
#3 - marital : marital status (categorical: 'divorced','married','single','unknown'; note: 'divorced' 
#    means divorced or widowed)
#4 - education (categorical: 'basic.4y','basic.6y','basic.9y','high.school','illiterate',
#    'professional.course','university.degree','unknown')
#5 - default: has credit in default? (categorical: 'no','yes','unknown')
#6 - housing: has housing loan? (categorical: 'no','yes','unknown')
#7 - loan: has personal loan? (categorical: 'no','yes','unknown')

# related with the last contact of the current campaign:
#8 - contact: contact communication type (categorical: 'cellular','telephone')
#9 - month: last contact month of year (categorical: 'jan', 'feb', 'mar', ..., 'nov', 'dec')
#10 - day_of_week: last contact day of the week (categorical: 'mon','tue','wed','thu','fri')
#11 - duration: last contact duration, in seconds (numeric). Important note: 
#     this attribute highly affects the output target (e.g., if duration=0 then y='no'). 
#     Yet, the duration is not known before a call is performed. Also, after the end of the 
#     call y is obviously known. Thus, this input should only be included for benchmark purposes 
#     mand should be discarded if the intention is to have a realistic predictive model.

# other attributes:
#12 - campaign: number of contacts performed during this campaign and for this client (numeric, includes last contact)
#13 - pdays: number of days that passed by after the client was last contacted from a previous campaign 
#    (numeric; 999 means client was not previously contacted)
#14 - previous: number of contacts performed before this campaign and for this client (numeric)
#15 - poutcome: outcome of the previous marketing campaign (categorical: 'failure','nonexistent','success')

# social and economic context attributes:
#16 - emp.var.rate: employment variation rate - quarterly indicator (numeric)
#17 - cons.price.idx: consumer price index - monthly indicator (numeric)
#18 - cons.conf.idx: consumer confidence index - monthly indicator (numeric)
#19 - euribor3m: euribor 3 month rate - daily indicator (numeric)
#20 - nr.employed: number of employees - quarterly indicator (numeric)

#Output variable (desired target):
#21 - y - has the client subscribed a term deposit? (binary: 'yes','no')



#variable selection: age, education, marital, duration, campaign, 
#cons.price.idx, cons.conf.idx, y
data = data %>% dplyr::select(age, education, marital, duration, campaign,
                              cons.price.idx, cons.conf.idx, y)

#check if there exists any variable with NA-values
sapply(data, function(x) any(is.na(x)))

#reducing the levels of marital to marrried, single and others
data$marital = as.character(data$marital)
data$marital = factor(ifelse(data$marital %in% c("married", "single"),
                             data$marital, "others"), levels = c("married",
                                                                 "single", "others"))

#reducing the levels of education to university.degree, prof.course and others
data$education = as.character(data$education)
data$education[data$education == "professional.course"] = "prof.course"
data$education = factor(ifelse(data$education %in% c("university.degree", "prof.course"),
                               data$education, "others"), levels = c("university.degree",
                                                                     "prof.course","others"))
#discriptive statistics(data-type, number of levels and their distribution)
str(data)
summary(data)

#since y is a binary variable, we choose logistic regression for modeling
logistic.mod = glm(y ~ age + education + marital + duration + campaign + 
                     cons.price.idx + cons.conf.idx, data = data,
                   family = "binomial")

#model-coeficients(by using all available data   )
TrueVal = as.vector(coef(logistic.mod))

# create missing value with MAR mechanism, frequency of missing value of all columns is 0.33
dat_mice = data
set.seed(123)
create_miss_mice = ampute(data = dat_mice,freq = rep(0.33, ncol(data)), mech = "MAR")
miss.data_mice = create_miss_mice$amp

#by ampute function, factor variables are also converted numeric 
#because the calculation of weights requires numeric data 
#that's why we need to convert those vadiables to original factor variables
#after creating missing values          
for(i in 1:ncol(dat_mice)){
  if(is.factor(dat_mice[,i])){
    miss.data_mice[,i] = ifelse(is.na(miss.data_mice[,i]), NA, as.character(dat_mice[,i]))
    miss.data_mice[,i] = as.factor(miss.data_mice[,i])
  }
}

# Missingness pattern can be visualised in VIM package by
aggr(miss.data_mice, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, 
     labels=names(miss.data_mice), cex.axis=.7, gap=3, 
     ylab=c("Proportion of missingness","Missingness Pattern"))

# The margin plot of the pairs can be plotted using VIM package as
marginplot(miss.data_mice[, c("cons.price.idx", "cons.conf.idx")], 
           col = mdc(1:2), cex.numbers = 1.2, pch = 19)

#miltiple imputation with CART
start_time.cart = Sys.time()
imp.cart_mice <- mice(miss.data_mice, m=5, 
                      maxit= 10, printFlag=FALSE, method = "cart", seed = 12345)
end_time.cart = Sys.time()
runtime_cart = end_time.cart - start_time.cart

#miltiple imputation with Random forest
start_time.rf = Sys.time()
imp.rf_mice <- mice(miss.data_mice, m=5, maxit=10, printFlag=FALSE,
                    method = 'rf', ntree = 10, mtry = 3, seed = 12345)
end_time.rf = Sys.time()
runtime_rf = end_time.rf - start_time.rf

#save all objects
object.cart.rf = list(create_miss_mice, dat_mice, data, end_time.cart, end_time.rf,
                      imp.cart_mice, imp.rf_mice, logistic.mod, miss.data_mice, runtime_cart,
                      runtime_rf, TrueVal)

names(object.cart.rf) = c("create_miss_mice", "dat_mice", "data, end_time.cart", "end_time.rf",
                          "imp.cart_mice", "imp.rf_mice", "logistic.mod", "miss.data_mice", 
                          "runtime_cart", "runtime_rf", "TrueVal")
saveRDS(object.cart.rf, "C:\\Users\\Himel\\OneDrive\\Studium\\M.Sc. Statistics\\
3_Semester\\Statistical analysis of missing data\\presentation\\object.cart.rf.rds")



#diagonistic plots for multiple imputation with cart
stripplot(imp.cart_mice, pch = 20, cex = 1.2)

#png(filename="C:\\Users\\Himel\\OneDrive\\Studium\\faithful_cart.png")
xyplot(imp.cart_mice, cons.price.idx ~ cons.conf.idx | .imp, pch = 20, cex = 1.4)
#dev.off()

#convergence plot
plot(imp.cart_mice, c("cons.price.idx", "cons.conf.idx"))
plot(imp.cart_mice, c("age", "duration", "campaign"))

#density plot
densityplot(imp.cart_mice)


#diagonistic plots for multiple imputation with random forest
stripplot(imp.rf_mice, pch = 20, cex = 1.2)

#png(filename="C:\\Users\\Himel\\OneDrive\\Studium\\faithful_rf.png")
xyplot(imp.rf_mice, cons.price.idx ~ cons.conf.idx | .imp, pch = 20, cex = 1.4)
#dev.off()

#convergence plot
plot(imp.rf_mice, c("cons.price.idx", "cons.conf.idx"))
plot(imp.rf_mice, c("age", "duration", "campaign"))

#density plot
densityplot(imp.rf_mice)

#------------------------------------------------------------------------
#conduct a MC simulation study to estimate bias, MSE and coverage rate 
#of model coeficients
#------------------------------------------------------------------------

# Setup of the MC study:
R <- 500 # number of simulation cycles
M <- 5 # number of multiple imputations
n <- 500 # sample size
niter <- 10 # number of iterations for the chained equations
k = length(TrueVal)# number of coeficients
#initialize empty arrays for simulation results
resBD = resMI.cart = resMI.rf = resCC  = array(dim=c(R, 4, k))

start_time <- Sys.time()
set.seed(12345)
for (r in 1:R) {
  if(r %% 10 == 0) cat("iteration", r, "of", R,"\n")
ind <- sample(nrow(data), n) # draw sample from our population
dat <- BDdat <- data[ind,]

# create missing value with MAR mechanism, frequency of missing value for every column is 0.33
#set.seed(123)
create_miss = ampute(data = dat,freq = rep(0.33, ncol(data)), mech = "MAR")
miss.data = create_miss$amp

#by ampute function, factor variables are also converted numeric 
#because the calculation of weights requires numeric data 
#that's why we need to convert those vadiables to original factor variables
#after creating missing values
for(i in 1:ncol(dat)){
  if(is.factor(dat[,i])){
    miss.data[,i] = ifelse(is.na(miss.data[,i]), NA, as.character(dat[,i]))
    miss.data[,i] = as.factor(miss.data[,i])
  }
}

dat = miss.data
#miltiple imputation with CART
imp.cart <- mice(dat, m=M, maxit=niter, printFlag=FALSE, # with CART for each incomplete variable
             method = "cart")
#miltiple imputation with Random forest
imp.rf <- mice(dat, m=M, maxit=niter, printFlag=FALSE, # with rf for each incomplete variable
              method = 'rf', ntree = 10, mtry = 3)


# use imputed datasets with CART-method for logistic-regression estimates
fit = with(imp.cart,  glm(y ~ age + education + marital + duration + campaign + 
                            cons.price.idx + cons.conf.idx, 
                          family = "binomial"))
tab = summary(pool(fit),conf.int=T)[,c(1,2,6,7)]
# Standard errors instead of variance 
tab[, 2] <- tab[, 2]^2
resMI.cart[r, , 1:length(TrueVal)] <- t(tab)


### use imputed datasets with RF-method for logistic-regression estimates
fit = with(imp.rf,  glm(y ~ age + education + marital + duration + campaign + 
                            cons.price.idx + cons.conf.idx, 
                          family = "binomial"))
tab = summary(pool(fit),conf.int=T)[,c(1,2,6,7)]
# Standard errors instead of variance 
tab[, 2] <- tab[, 2]^2
resMI.rf[r, , 1:length(TrueVal)] <- t(tab)


# Complete case analysis 
complete.dat = dat[complete.cases(dat),]
CCmod = glm(y ~ age + education + marital + duration + campaign + 
              cons.price.idx + cons.conf.idx, data = complete.dat,
                   family = "binomial")

resCC[r, , 1:length(TrueVal)] <- rbind(coef(CCmod), 
                         summary(CCmod)$coefficients[, 2]^2,
                         t(confint(CCmod)))

# And finally -- as benchmark -- the 'before deletion' results
BDmod = glm(y ~ age + education + marital + duration + campaign + 
              cons.price.idx + cons.conf.idx, data = BDdat,
            family = "binomial")
resBD[r, , 1:length(TrueVal)] <- rbind(coef(BDmod), summary(BDmod)$coefficients[, 2]^2,
                         t(confint(BDmod)))
}

#calculate the run-time of simulation
end_time <- Sys.time()
RunTime_simulation = end_time - start_time



#Diagonistic checking
Bias = MSE = Coverage = matrix(nrow=length(TrueVal), ncol=4)

#calculate Bias of coeficients for all considered models
Bias[, 1] = apply(apply(resBD, 1, FUN=function(x) x[1,]-TrueVal), 
                  1, function(x) mean(x, na.rm = TRUE))
Bias[, 2] = apply(apply(resMI.cart, 1, FUN=function(x) x[1,]-TrueVal), 
                  1, function(x) mean(x, na.rm = TRUE))
Bias[, 3] = apply(apply(resMI.rf, 1, FUN=function(x) x[1,]-TrueVal), 
                  1, function(x) mean(x, na.rm = TRUE))
Bias[, 4] = apply(apply(resCC, 1, FUN=function(x) x[1,]-TrueVal), 
                  1, function(x) mean(x, na.rm = TRUE))
Bias = round(Bias, digits = 3)


#calculate MSE of coeficients for all considered models
MSE[, 1] = apply(apply(resBD, 1, FUN=function(x) (x[1,]-TrueVal)^2), 
                 1, function(x) mean(x, na.rm = TRUE))
MSE[, 2] = apply(apply(resMI.cart, 1, FUN=function(x) (x[1,]-TrueVal)^2), 
                 1, function(x) mean(x, na.rm = TRUE))
MSE[, 3] = apply(apply(resMI.rf, 1, FUN=function(x) (x[1,]-TrueVal)^2), 
                 1, function(x) mean(x, na.rm = TRUE))
MSE[, 4] = apply(apply(resCC, 1, FUN=function(x) (x[1,]-TrueVal)^2), 
                 1, function(x) mean(x, na.rm = TRUE))
MSE = round(MSE, digits = 3)


#calculate coverage rate of coeficients for all considered models
Coverage[, 1] <- apply(apply(resBD, 1,
                             FUN=function(x) {
                               ifelse(x[3,] < TrueVal & x[4,] > TrueVal, 1, 0)
                             }), 1, function(x) mean(x, na.rm = TRUE))

Coverage[, 2] <- apply(apply(resMI.cart, 1,
                             FUN=function(x) {
                               ifelse(x[3,] < TrueVal & x[4,] > TrueVal, 1, 0)
                             }), 1, function(x) mean(x, na.rm = TRUE))

Coverage[, 3] <- apply(apply(resMI.rf, 1,
                             FUN=function(x) {
                               ifelse(x[3,] < TrueVal & x[4,] > TrueVal, 1, 0)
                             }), 1, function(x) mean(x, na.rm = TRUE))

Coverage[, 4] <- apply(apply(resCC, 1,
                             FUN=function(x) {
                               ifelse(x[3,] < TrueVal & x[4,] > TrueVal, 1, 0)
                             }), 1, function(x) mean(x, na.rm = TRUE))
Coverage = round(Coverage, digits = 3)


colnames(Bias) = colnames(MSE) = colnames(Coverage) = c("BD", "mice.cart", "mice.rf", "CC")
rownames(Bias) = rownames(MSE) = rownames(Coverage) = c("(Intercept)", "age",
                                                           "educationprof.course","educationothers",
                                                           "maritalsingle", "maritalothers", 
                                                           "duration", "campaign", "cons.price.idx",
                                                           "cons.conf.idx")


#save all objects from simulation as rds data
all.objects = list(BDdat, BDmod, Bias, CCmod, complete.dat, 
                   Coverage, create_miss, dat, data, fit,
                   imp.cart, imp.rf, miss.data, MSE, resBD, resCC, 
                   resMI.cart, resMI.rf, tab, TrueVal, RunTime_simulation)
names(all.objects) = c("BDdat", "BDmod", "Bias", "CCmod",
                       "complete.dat", "Coverage", "create_miss",
                       "dat", "data", "fit", "imp.cart", "imp.rf", "miss.data", 
                       "MSE", "resBD", "resCC", "resMI.cart", 
                       "resMI.rf", "tab","TrueVal", "RunTime_simulation")

#saveRDS(all.objects, "C:\\Users\\Himel\\OneDrive\\Studium\\M.Sc. Statistics\\3_Semester\\Statistical analysis of missing data\\presentation\\object_mi.rds")
