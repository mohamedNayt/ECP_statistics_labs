## TP1: SVM et Modèles de mélange 						##
## Auteurs: Mohammed El Hamdi et Mohamed N'AITN'BARK	##
##########################################################

rm(list=ls())
setwd('~/Documents/cours_3A/sdma/labs/tp1')
library(kernlab)

## SVM classification:
######################
#Load data:
tab=read.table('spam.txt', header=T,sep=';')
cols = ncol(tab)
rows = nrow(tab)
names(tab)
Y = tab$spam
levels(Y)
nlevels(Y)
table(Y)/length(Y) #Levels proportions
plot(Y)

kernels = c("rbfdot","vanilladot","polydot", "tanhdot")
K = 20
res = data.frame(data=matrix(0,nrow=K,ncol=length(kernels)))
names(res) = kernels

#Sample tarining set and test set:
for(ker in kernels){
  for(i in 1:K){
    index = sample(rows, size=3*rows/4)
    train_x = tab[index,-c(58)]
    train_y = Y[index]
    test_x = tab[-index,-c(58)]
    test_y = Y[-index]
    #Train SVM model
    classif = ksvm(x=as.matrix(train_x), y=train_y, type="C-svc", kernel=ker,C=10)
    test_predict = predict(classif, test_x)
    res[i,ker] = mean(test_y==test_predict)
  }
}
boxplot(res)
colMeans(res)

# Conclusion: Nous remarquons que le meilleur résultat s'obtient avec le noyau gaussien, 
# les autres noyaux donnent des résultats tout aussi satisfaisants, par contre le noyau à base 
# de tangeante hyp n'est pas satisfaisant.

#Création de la matrice de confusion
index = sample(rows, size=3*rows/4)
train_x = tab[index,-c(58)]
train_y = Y[index]
test_x = tab[-index,-c(58)]
test_y = Y[-index]
#Train SVM model
classif = ksvm(x=as.matrix(train_x), y=train_y, type="C-svc", kernel="rbfdot",C=10)
test_predict = predict(classif, test_x)
confusion = table(test_y, test_predict)
confusion = confusion/replicate(2,rowSums(confusion))

#On remarque la proportion des emails mal classé est trés faible par rapport à celle des spams
#mal classés, ce qui est bon signe.

## EM Algorithm and mixture models:
###################################
#Load and view data
irm=as.matrix(read.table("irm_thorax.txt",header=F,sep=";"))
N = length(irm)
image(irm)
d_irm = density(as.vector(irm), kernel="gaussian")
hist(irm, freq=F)
lines(d_irm, col="blue")
legend("topleft",legend=c("hist","kernel density"),col = c("black","blue"),
       bty="n", pch = 15)

# L'image suggère qu'on peut effectuer un clustering sur l'intensité des couleurs,
# Chose que confirme l'histogramme vu qu'on a l'impression de l'existence d'un mélange
# d'au moins deux gaussiennes, c'est ce qu'on va essayer de vérifier ci-dessous à l'aide 
# de l'algorithme EM


x_range = seq(min(irm), max(irm), length.out=floor(3*length(irm)/4))

em_algo = function(data, means, vars, p, n_iter)
{
  for(i in 1:n_iter)
  {
    for(j in 1:n_kers)
    {
      resp[j,] = p[j]*dnorm(as.vector(data),mean=means[j], sd=sqrt(vars[j]))
    }
    resp = resp / t(replicate(n_kers, colSums(resp)))
    p = rowMeans(resp)
    means = rowSums(resp*t(replicate(n_kers, as.vector(data))))/rowSums(resp)
    centred_data = t(replicate(n_kers, as.vector(data))) - replicate(N,means)
    vars = rowSums(resp*centred_data^2)/rowSums(resp)
  }  
  return(list(p,means, vars))
}

dmixtureNorm = function(x,p,means,vars)
{
  result = rep(0, length(x))
  n_kers = length(means)
  for(i in 1:n_kers)
  {
    result = result + p[i]*dnorm(x, mean=means[i], sd=sqrt(vars[i]))
  }
  return(result)
}

# Deux gaussiennes:
n_kers = 2
means = seq(100,250,length.out=n_kers)
vars = rep(50^2,n_kers)
p = rep(1/n_kers, n_kers)
n_iter = 3000
result2 = em_algo(irm,means,vars,p,n_iter)
dist2 = dmixtureNorm(x_range, result2[[1]], result2[[2]], result2[[3]])
hist(irm, freq=F, ylim=c(0,max(dist2)))
lines(d, col="blue")
lines(x_range, dist2, col="red")
legend("topleft",legend=c("hist","kernel density","EM result"),col = c("black","blue", "red"),
       bty="n", pch = c(15,15,15))

# Trois gaussiennes:
n_kers = 3
means = seq(100,250,length.out=n_kers)
vars = rep(50,n_kers)
p = rep(1/n_kers, n_kers)
n_iter = 3000
result3 = em_algo(irm,means,vars,p,n_iter)
dist3 = dmixtureNorm(x_range, result3[[1]], result3[[2]], result3[[3]])
hist(irm, freq=F, ylim=c(0,max(dist3)))
lines(d, col="blue")
lines(x_range, dist3, col="red")
legend("topleft",legend=c("hist","kernel density","EM result"),col = c("black","blue", "red"),
       bty="n", pch = c(15,15,15))

# Cinq gaussiennes:
n_kers = 5
means = seq(100,250,length.out=n_kers)
vars = rep(50,n_kers)
p = rep(1/n_kers, n_kers)
n_iter = 3000
result5 = em_algo(irm,means,vars,p,n_iter)
dist5 = dmixtureNorm(x_range, result5[[1]], result5[[2]], result5[[3]])
hist(irm, freq=F, ylim=c(0,max(dist5)))
lines(d, col="blue")
lines(x_range, dist5, col="red")
legend("topleft",legend=c("hist","kernel density","EM result"),col = c("black","blue", "red"),
       bty="n", pch = c(15,15,15))

#A l'aide de l'histogramme mais aussi en utilisant une estimation non-
#paramétrique de la densité (en utilisant des convolutions gaussiennes fct 
#density sur R) nous obtenons une meilleure séparation avec deux gaussiennes
#qu'avec 3 et 5 gaussiennes où on a l'impression de faire du sur-apprentissage,
#en particulier pour les grandes intensités (la gaussienne à l'extrémité droite du graphique est très aigu).


## Regression mixture with EM algorithm
library(mixtools)
reg_double = read.table("regression_double.txt", sep=';')
plot(reg_double)
model = regmixEM(reg_double[,2], reg_double[,1], k=2)
plot.mixEM(model, whichplots=2)

#On observe deux clusters chacun peut être expliqué par un 
#modèle linéaire, une des raisons selon nous qui peuvent expliquer 
#ce comportement est l'existence de deux sources distinctes qui génèrent 
#les observations.
#Une régression linéaire simple n'est donc pas adapté à ce modèle vu le nombre
#des observations dans chaque cluster on finira par faire des erreurs grossières.
#Cependant nous pouvons d'ores et déjà s'attendre à une méthode de classification type EM
#pour détecter les deux clusters et déterminer un mélande de deux régressions linéaires


residuals = function(model)
{
  return((model$y - model$posterior[,1]*(model$x%*%model$beta[,1])
          - model$posterior[,2]*(model$x%*%model$beta[,2]))[,1])
}

hist(residuals(model), freq=F)
resid0 = mean(residuals(model)^2)

#Les résidus sont centré sur 0

#maxit = 1
model1 = regmixEM(reg_double[,2], reg_double[,1], maxit=1)
resid1 = mean(residuals(model1)^2)
plot.mixEM(model1, whichplots=2)

#maxit = 3
model3 = regmixEM(reg_double[,2], reg_double[,1], maxit=3)
resid3 = mean(residuals(model3)^2)
plot.mixEM(model3, whichplots=2)

#maxit = 5
model5 = regmixEM(reg_double[,2], reg_double[,1], maxit=5)
resid5 = mean(residuals(model5)^2)
plot.mixEM(model5, whichplots=2)

maxit = 1:20
resid = rep(0, length(maxit))
for(i in 1:length(maxit))
{
  model = regmixEM(reg_double[,2], reg_double[,1], maxit=maxit[i])
  resid[i] = mean(residuals(model)^2)
}
plot(resid)
plot.mixEM(model, whichplots=1)
plot(resid[4:length(resid)])

#Nous observons que la convergence de l'algorithme EM est très rapide vu qu'au
#bout de la cinquième itération nous commençons presque à avoir une convergence
#(cf. graphique log-like vs itérations et  celui des résidus vs itérations), 
#le programme par défaut de mixtools s'arrête à la 15ième itération.

