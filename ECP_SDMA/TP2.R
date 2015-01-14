setwd('/home/mohamed/Documents/cours_3A/sdma/labs/ECP_SDMA/data')

#################################################
#         SDMA: TP 2                            #
# Mohammed El Hamdi                             #
# Mohamed N'AITN'BARK                           #
#################################################

#####Application 1: Régression linéaire
###I - Modèle

##1/
tab=read.table('UsCrime.txt',header=1)
n = nrow(tab)
p = ncol(tab) - 1
plot(tab)
corr_matrix = cor(tab)

res = lm('R~.', data=tab)
print(res)
summary(res)
attributes(res)
# La valeur de la p-value du F test suggère que le modèle est globalement significatif

# On utilise des T tests pour estimer la significativité des coefficients
# Les coefficients les plus importants suivant la p value sont l'Ed, X, Age, U2 et la constante du modèle 'intercept'

##2/ Le modèle globale:
# Le coefficient de détermination représente la variance du la variable estimée,
# divisée par la variance de la variable expliquée (en d'autres termes la part de la variance expliquée par le 
# modèle exprimée en pourcentage).
# Dans ce cas sa valeur est de 0.77, cette valeur même si elle n'est pas > 0.90, reste toutefois 
# acceptable, la valeur du R2 ajustée est cependant moins bonne: 0,65.
# De plus le F-Test indique que le test est globalement significatif (voir notre commentaire ci-desssus).

##3/ Les coefficientsdu modèle:
# La p-value de la loi de student (47-14=33 degrés de liberté) est à peu près égal à 2, 
# donc seules les t-values supérieures (en val absolues) sont significatives: Ed, X, Age, U2
# Ainsi la plupart des coefficients à l'exception de ceux précités (et d'autres dont la valeur de t test est
# proches de 2 en val abs) ne présentent pas d'intérêt majeur dans le modèle ou sont très corrélés
# aux variables significatives, donc redondants.

# La signification des '*' est liée aux différentes valeurs du risque prises dans le test de student,
# Ainsi *** correspond à un risque de 0.1%, **: 1% , et *: 5%.

resume = summary(res)
coef = coef(resume)
IC  = confint(res,level=0.95)
IC

#Intervalle de confiance sur la variance des résidus
c(resume$sigma^2*res$df.res/qchisq(0.975,res$df.res),
  resume$sigma^2*res$df.res/qchisq(0.025,res$df.res))

#On utilise des ellipsoides de confiance pour former des régions de confiance jointes entre les coefficients.
#nous remarquons que plusieurs variables sont fortement corrélées
#library(ellipse)
#x11()
#par(mfrow=c(2,3))
#for(i in 1:3){
#  for(j in (i+1):4){
#   plot(ellipse(res,c(i,j),level=0.95),type="l",
#           xlab=paste("beta",i,sep=""),ylab=paste("beta",j,sep=""))
#    points(coef(res)[i], coef(res)[j],pch=3)
#    lines(c(IC[1,i],IC[1,i],IC[2,i],IC[2,i],IC[1,i]),
#           c(IC[1,j],IC[2,j],IC[2,j],IC[1,j],IC[1,j]),lty=2)
#    }}


confint(res, level=0.95)
confint(res, level=0.99)

# Les variables dont le T test n'était pas significatif ont des intervalles de confiance plus centré
# sur 0.

##4/ Etude des valeurs prédites:

plot(tab$R,res$fitted.values, col="blue")
abline(a=0,b=1)

# Intervalles de confiance pour les prédictions

#Première approche (naive), on suppose que les beta estimés sont connus et que l'estimateur de l'erreur quadratique a convergé
#on construit des intervalles de confiance par rapport à l'erreur résiduelle (approche non-traitée avec predict.lm)
predicted = predict(res, interval="confidence", level=0.95)
plot(tab$R, res$fitted.values)
abline(a=qnorm(0.025,sd=sd(res$residuals)),b=1, col="orange")
abline(a=qnorm(0.975,sd=sd(res$residuals)),b=1, col="orange")
abline(a=0,b=1, col="red")

#Deuxième approche, "confidence" avec la fonction predict.lm n'incluant pas le terme d'erreur 
p_conf <- predict(res,interval="confidence")
p_conf1=p_conf[order(res$fit),]
tab$R

#Troisième approche, "prediction" avec la fonction predict.lm incluant le terme d'erreur 
p_pred <- predict(res,interval="prediction")
p_pred1=p_pred[order(res$fit),]


#Synthèse des différentes approches
#Si la première approche est naive et donnent une marge d'erreur simple, les deux dernières méthodes
#suggérées par le TP donnent des intervalles de confiance, plus large pour l'approche 'prediction" ce
#ce qui est logique vu que le termer erreur est inclus cf.graphe

plot(tab$R, res$fitted.values)
abline(a=0,b=1, col="red")
matlines(sort(tab$R,decreasing=FALSE),p_conf1[,c("lwr","upr")],col=2,lty=1,type="b",pch="+")
matlines(sort(tab$R,decreasing=FALSE),p_pred1[,c("lwr","upr")],col=3,lty=2,type="b",pch=1)
abline(a=qnorm(0.025,sd=sd(res$residuals)),b=1, col="orange")
abline(a=qnorm(0.975,sd=sd(res$residuals)),b=1, col="orange")



##5/ Etudes des résidus:

# Erreur quadratique des résidus:
err_resid = mean(res$residuals^2)
err_resid_nb = err_resid*(n/(n-p))

# Erreur r?siduel en fonction de Y
plot(tab$R,res$residuals)  
#Les résidus ne semble pas avoir de tendance, ainsi notre modèle nous semble être homoscedastique
                           

# QQPlot
qqnorm(res$residuals)
qqline(res$residuals, col="blue") 
#Le résidus semblent suivre une loi normale

# Shapiro test
shapiro.test(res$residuals)
# Le test de Shapiro nous indique que la loi des résidus est bien normal

##6/ Performance sur de nouvelles données

indTest = seq(1,n,3)
tabTest = tab[indTest,]
tabTrain = tab[-indTest,]
res2 = lm('R~.', data=tabTrain)
predicted = predict(res2, newdata=tabTest)
err_mean = mean((predicted-tabTest$R)^2)
err_mean 
err_sd = sd((predicted-tabTest$R))
err_sd
#Notre modèle semble être assez robuste au niveau de l'erreur vu qu'il renvoie pour un échantillon
#partiel une erreur moyenne, et un écart-type assez proche de ceux sur l'échantillon global


##7/ Analyse graphique
x11()
par(mfrow=c(2,2))
plot(res)

#Analyse des graphiques propos?s:
#Res vs FittedVal: Nous remarquons un léger effet de levier sur les points ayant des valeurs R grandes
#ou faible, qui est du au peu d'observation dans ces régions là.
#Cependant avec le graphique res vs leverage nous constaton grâce à la distance de Cook qu'il y a un
#faible effet de levier et faible résidu
#Le graphique scale location confirme l'hypothèse d'homoscedasticité vu qu'il y a une trés faible tendance


###I - Selection de modèles

##1/ Backward regression
regbackward = step(res, director="backward")
summary(regbackward)
x11()
par(mfrow=c(2,2))
plot(regbackward)
#Le modèle backward regression élimine la variable explicative la moins significative (plus petit
#student) à condition qu'il soit non négatif, ce qui fait qu'on commence par éliminer la variable N, 
#et ainsi de suite jusqu'à ce que le retrait d'aucune variable ne permet d'améliorer l'AIC
#Comparé au modèle initial, nous obtenons un modèle plus significatif (en terme de student), 
#nous perdons légèrement en R2 global (mais nous gagnons en R2 ajusté qui est le plus important)
#Un léger levier se crée et aussi une augmentation des résidus (point 29), et le qqplot se détériore.



##2/ Forward regression
regforward=step(lm(R~1,data=tab),list(upper=res),direction="forward")
summary(regforward)
x11()
par(mfrow=c(2,2))
plot(regforward)
#La regression Forward consiste à commencer par le plus petit modèle et d'introduire la variable
#la plus significative, on s'arrête quand le critère de choix ne peut-etre amélioré par le rajout
#d'une nouvelle variable
#Ici la Forward regression renvoie exactement le même résultat que la regression backward
#Cependant plus généralement le risque consiste en la séléction d'une variable significative au début
#mais qui ne le reste plus vers la fin (par exemple en cas de présence de variable corrélées)
#d'ou l'intérêt d'effectuer une regression stepwise ou on s?lectionne dans les deux sens




##3/ Stepwise regression
regboth = step(res,  direction="both",k=2)
summary(regboth)
formula(regboth)
x11()
par(mfrow=c(2,2))
plot(regboth)

##4/ 
formula(regboth)
reg0=lm(formula(regboth),data=tab)
summary(reg0)

##5/ Régression stepwise avec critère BIC
regbothBIC=step(res, direction="both", k= log(n))
summary(regbothBIC)
x11()
par(mfrow=c(2,2))
plot(regbothBIC)
#Nous remarquons que le critère en log(n) est plus discriminant(log(n) est trés supérieur à 2), 
#et donc les variables gardées sont plus réduites (il ne garde que les variables avec student
#significatif), Cependant nous perdons  un peu en R2, mais nous obtenons un modèle 
#plus significatif (coef par coef)

#II Logistic regression
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
#Les coefficients les plus significatifs sont tobacco, ldl, famhist,  age
#Ceci montre aussi les facteurs qui influent le plus sur la probabilité d'un crise cardiaque
errors = validation_croisee(formula(logregboth),data,50)
max(errors)
min(errors)
mean(errors)
#Le modèle réduit et le modèle complet ont le même niveau d'erreur, ce qui est intéréssant vu qu'on
#a obtenu un modèle moins assujetti au surapprentissage sans une perte de performance.
