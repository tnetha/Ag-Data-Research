install.packages('pls')
library(pls)
data(yarn)
data("gasoline")
View(yarn)
View(gasoline)
class(yarn$NIR)

gasTrain <- gasoline[1:50,]
gasTest <- gasoline[51:60,]

gas1 <- plsr(octane ~ NIR, ncomp = 10, data = gasTrain, validation = "LOO")

summary(gas1)

plot(RMSEP(gas1), legendpos = 'topright')
plot(gas1,ncomp=2,asp=1,line=T)
plot(gas1, plottype = 'scores', comps = 1:3)

predict(gas1, ncomp = 2, newdata = gasTest)
dim(gasoline$NIR)
length(NA)
