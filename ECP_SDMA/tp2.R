setwd('~/Documents/cours_3A/sdma/labs/ECP_SDMA/data')

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
# La valeur de la p-value du F test suggère que le modèle est globalement significatif

# On utilise des T tests pour estimer la signifiance des coefficients
# Les coefficients les plus important suivant la p value sont l'intercept, Ed, X, Age, U2

confint(res, level=0.95)
confint(res, level=0.99)

# Les variables dont le T test n'était pas significatif ont des intervalles de confiance plus centré 
# sur 0.

# Intervalles de confiance pour les prédictions
pred = predict(res, interval="confidence", level=0.95)
plot(tab$R, res$fitted.values)

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
pairs(tab,pch=22,bg=c("red","blue")[unclass(factor(tab[,"chd"]))])
logreg = glm('chd~.',data=data,family=binomial(logit))
summary(logreg)
predicted = (logreg$fitted.values > 0.5)
t=table(data$chd,predicted)

#Confusion matrix
t/(t%*%matrix(data=1,nrow=2,ncol=2))
#Le taux de faux négatif est très élevé.

#Validation croisée
validation_croisee <- function(formula,data,K){
  errors = rep(0, times=K)
  for(k in 1:K){
    ind=1:nrow(data)
    train_ind = sample.int(nrow(data), size=0.75*nrow(data))
    test_ind = ind[!(ind %in% train_ind)]
    glm.out = glm(formula,data=data[train_ind,],family=binomial(logit))
    predicted = predict.glm(glm.out, newdata=data[test_ind,],type='response')
    predicted = (predicted>0.5)
    errors[k]= mean(predicted!=data[test_ind,c('chd')])
  }
  return(errors)
}

errors = validation_croisee('chd~.',data,50)
max(errors)
min(errors)
mean(errors)

#La validation croisée permet de valider le modèle en donnant une estimation non biaisé de l'erreur.

#Selection de variables
logregboth=step(logreg, director="both")
#Les coeffs les plus significatifs sont tobacco, ldl, famhist,  age
#Ceci montre aussi les facteurs qui influent le plus sur la probabilité d'un crise cardiaque
errors = validation_croisee(formula(logregboth),data,50)
max(errors)
min(errors)
mean(errors)
#Le modèle réduit et le modèle complet ont le même niveau d'erreur.