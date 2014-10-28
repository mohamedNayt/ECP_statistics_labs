setwd('~/Documents/cours_3A/sdma/labs/tp1')

#1
tab = read.table("UsCrime.txt", header=T)
n = nrow(tab)
p = ncol(tab) - 1
plot(tab)
corr_matrix = cor(tab)

res = lm('R~.', data=tab)
print(res)
summary(res)
attributes(res)
# La valeur de la p-value du F test suggère que le modèle n'est pas globalement significatif

# On utilise des T tests pour estimer la signifiance des coefficients
# Les coefficients les plus important suivant la p value sont l'intercept, Ed, X, Age, U2

confint(res, level=0.95)
confint(res, level=0.99)

# Les variables dont le T test n'était pas significatif ont des intervalles de confiance centré 
# sur 0.

plot(tab$R,res$fitted.values, col="blue")
abline(a=0,b=1)

# Intervalles de confiance pour les prédictions
predicted = predict(res, interval="confidence", level=0.95)
plot(tab$R, res$fitted.values)
abline(a=qnorm(0.025,sd=sd(res$residuals)),b=1, col="orange")
abline(a=qnorm(0.975,sd=sd(res$residuals)),b=1, col="orange")
abline(a=0,b=1, col="red")
# Erreur quadratique des résidus:
err_resid = mean(res$residuals^2)
err_resid_nb = err_resid*(n/(n-p))
# Erreur résiduel en fonction de Y
plot(res$residuals,tab$R)
# QQPlot
qqnorm(res$residuals)
qqline(res$residuals, col="blue") #Le modèle parait etre normal
# Shapiro test
shapiro.test(res$residuals)

# Performance sur de nouvelles données
indTest = seq(1,n,3)
tabTest = tab[indTest,]
tabTrain = tab[-indTest,]
res = lm('R~.', data=tabTrain)
predicted = predict(res, newdata=tabTest)
err_mean = mean((predicted-tabTest$R)^2)
err_sd = sd((predicted-tabTest$R)^2)

#Analyse graphique
x11()
par(mfrow=c(2,2))
plot(res)

#Model selection
#Backward regression
regbackward = step(res, director="backward")
summary(regbackward)
plot(regbackward)

#Forward regression
regforward = step(res, director="forward")
summary(regforward)

#Stepwise regression
regboth = step(res, director="both")
summary(regboth)

formula(regboth)

#Logistic regression
tab = read.csv("SAheart.txt", header=T)
data = tab[,c("sbp","tobacco","ldl","famhist","obesity","alcohol","age","chd")]
pairs(tab,pch=22,bg=c("red","blue"))
