setwd("C:\\Users\\aitnm\\Desktop\\sdma_tp2")

tab = read.table("usa_indicators.txt", header=TRUE,sep=";")
n=nrow(tab)
p=ncol(tab)

#La variable contenant le la quantité de CO2 par an est EN.ATM.CO2E.KT

plot(tab$EN.ATM.CO2E.KT,type="l")
tab = tab[,-c(1)]
tab = as.data.frame(scale(tab, center=FALSE))

##Ridge regression
require("MASS")
ridge.reg = lm.ridge("EN.ATM.CO2E.KT~.",tab,lambda=100)
coefs = sort(abs(ridge.reg$coef), decreasing=TRUE)
head(coefs,6)

lambdas=seq(0.1,100,0.01) 
ridge.reg2 = lm.ridge("EN.ATM.CO2E.KT~.",tab,lambda=lambdas)
lambd_min = lambdas[which.min(as.numeric(ridge.reg2$GCV))]

plot(lambdas,ridge.reg2$GCV)
plot(ridge.reg2)

coefridge = coef(ridge.reg2)
coefridge = coefridge[1,]

X = as.matrix(tab[,-c(21)])
X = cbind(matrix(data=1,nrow=nrow(X),ncol=1),X)
Yridge = X%*%as.vector(coefridge)
Y = tab[,21]
err = mean((Y-Yridge)^2)

##Régression Lasso
require(lars)
lasso.reg = lars(X,Y,type="lasso")

