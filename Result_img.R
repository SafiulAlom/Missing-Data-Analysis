remove(list = ls())
library(VIM)
library(mice)

Result_mice = readRDS("C:\\Users\\Himel\\OneDrive\\Studium\\M.Sc. Statistics\\3_Semester\\Statistical analysis of missing data\\presentation\\object.cart.rf.rds")
setwd("C:\\Users\\Himel\\OneDrive\\Studium\\M.Sc. Statistics\\3_Semester\\Statistical analysis of missing data\\presentation\\image")

# Missingness pattern can be visualised in VIM package by
#png("pattern_miss.png", height = 350, width = 600)
miss.data_mice = Result_mice$miss.data_mice
aggr(miss.data_mice, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, 
     labels=names(miss.data_mice), cex.axis=.7, gap=3, 
     ylab=c("Proportion of missingness","Missingness Pattern"))
#dev.off()

#png("marginPlot.png", height = 350, width = 500)
# The margin plot of the pairs can be plotted using VIM package as
marginplot(miss.data_mice[, c("cons.price.idx", "cons.conf.idx")], 
           col = mdc(1:2), cex.numbers = 1.2, pch = 19)
#dev.off()

imp.cart_mice = Result_mice$imp.cart_mice
#diagonistic plots for multiple imputation with cart
#png("stripplot_cart.png", height = 350, width = 600)
stripplot(imp.cart_mice, pch = 20, cex = 1.2)
#dev.off()


#png(filename="convergencePlot_cart1.png", height = 350, width = 600)
#convergence plot
plot(imp.cart_mice, c("cons.price.idx", "cons.conf.idx"))
#dev.off()

#png(filename="convergencePlot_cart2.png", height = 350, width = 600)
plot(imp.cart_mice, c("age", "duration", "campaign"))
#dev.off()

#density plot
#png(filename="density_cart.png", height = 350, width = 600)
densityplot(imp.cart_mice)
#dev.off()

imp.rf_mice = Result_mice$imp.rf_mice
#diagonistic plots for multiple imputation with random forest
#png("stripplot.png", height = 350, width = 600)
stripplot(imp.rf_mice, pch = 20, cex = 1.2)
#dev.off()


#convergence plot
#png(filename="convergencePlot_rf1.png", height = 350, width = 600)
plot(imp.rf_mice, c("cons.price.idx", "cons.conf.idx"))
#dev.off()

#png(filename="convergencePlot_rf2.png", height = 350, width = 600)
plot(imp.rf_mice, c("age", "duration", "campaign"))
#dev.off()

#density plot
#png(filename="densityplot_rf.png", height = 350, width = 600)
densityplot(imp.rf_mice)
#dev.off()

Result_simulation = readRDS("C:\\Users\\Himel\\OneDrive\\Studium\\M.Sc. Statistics\\3_Semester\\Statistical analysis of missing data\\presentation\\object_mi.rds")
write.csv2(Result_simulation$Bias, "Bias.csv")
write.csv2(Result_simulation$MSE, "MSE.csv")
write.csv2(Result_simulation$Coverage, "Coverage.csv")
