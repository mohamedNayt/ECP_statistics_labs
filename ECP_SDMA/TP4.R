setwd("/home/mohamed/Documents/cours_3A/sdma/labs/ECP_SDMA")
require(lattice)
# Exercice 1:
# Partie A
n=50
p=5
X = matrix(data=rnorm(p*n),nrow=n,ncol=p)
S = cov(X)
det(S)
levelplot(S)
res = svd(S)
U = res$u
V = res$v
E = diag(res$d)
# La matrice V s'appelle matrice "d'entrée" ou "d'analyse" et ses vecteurs sont orthonormés
# La matrice U est appelée de "sortie" et ses vecteurs aussi sont orthonormées
# La matrice E contient sur sa diagonale les valeur propres de la matrice S
sum(diag(E)) - sum(diag(S)) 
# Comme prévu les deux traces sont égales puisque les deux matrices ont les mêmes valeures propres
# L'explication théorique vient du fait que Trace(U%*%E%*%t(V))=Trace(t(V)%*%U%*%E)=Trace(E)
# Car t(V)%*%U est une matrice unitaire = isométrie
norm(U-V)
# On constate que U est sensiblement égale à V
norm(U%*%E%*%t(V) - S)
# On remarque que U%*%E%*%t(V) est sensiblement égale à S

#Partie B

S1 = cov(matrix(data=rnorm(p*10),nrow=10,ncol=p))
S2 = cov(matrix(data=rnorm(p*100),nrow=100,ncol=p))
S3 = cov(matrix(data=rnorm(p*1000),nrow=1000,ncol=p))
gb.palette <- colorRampPalette(c("blue", "red"), space = "rgb")
levelplot(S1,col.regions=gb.palette(20),main="Covariance matrix for n=10")
levelplot(S2,col.regions=gb.palette(20),main="Covariance matrix for n=100")
levelplot(S3,col.regions=gb.palette(20),main="Covariance matrix for n=1000")
# On remarque que plus la taille des vectuers augmente plus la matrice de covarance 
# Converge vers une matrice diagonale
barplot(svd(S1)$d,main="Singular values of the covariance matrix for n=10")
barplot(svd(S2)$d,main="Singular values of the covariance matrix for n=100")
barplot(svd(S3)$d,,main="Singular values of the covariance matrix for n=1000")
# On remarque que les valeurs singulières de la matrices de covariacnes convergent vers 1
# Lorsque n augmente, ceci est prévisible vu que les vecteurs générés sont indépendants est de
# variance 1 chacun, la matrice de covariance empirique converge donc vers la matrice de covariance
# théorique qui est la matrice identité.

Y=matrix(data=runif(p*1000,max=12),nrow=1000,ncol=p)
# Comme n est assez grand on espère retrouver une matrice assez proche de la matrice 
# de covariance théorique de nos data qui est de 12*Identity
S=cov(Y)
S
levelplot(S,col.regions=gb.palette(20),main="Covariance matrix for uniform distribution")
#On retrouve bien le résultat espéré

# Partie C:
Z=cbind(X,X)
S = cov(Z)
levelplot(S,col.regions=gb.palette(20),main="Covariance matrix for part C")
barplot(svd(S)$d,main="Valeurs propres de la matrice de covariance de Z")
# On voit que le rang de la matrice de covariance est de 5
# La matrice de covariance de Z est constituée des matrice de covariances de X:
#         | cov(X)\ cov(X)\
#cov(Z) = |----------------
#         | cov(X)\ cov(X) \  
# D'aprés la construction de Z il est clair que le rang de la matrice de covariance est de Z
# Doit être le même que celui de X et aussi pour les matrice de covariance.
# La méthode SVD permet donc dans le cas général effectuer une réduction de dimension
# en retrouvant les dimensions independantes des données et quantifie pour chacunes d'elle
# sa participation à la variance globale.

#Partie D

#Reprener la partie A avec n= 10 et p = 20.
#Calculer la matrice de covariance S
# Effectuer une SVD de S a l'aide du logiciel R.
#Visualiser les valeurs propres. Conclusion. Expliciter théoriquement les résultats obervés.

n=10
p=20
X = matrix(data=rnorm(p*n),nrow=n,ncol=p)
S = cov(X)
barplot(svd(S)$d,main="Valeurs propres dans le cas p=20,n=10")
#On remarque que le rang de la matrice S est 9, ceci est du au fait que p>n.

## Exercice 2

data = read.table("cardata.txt",header=T,sep=";",row.names=1)
# Le fichier contient des caractéristiques de certains modèles de voitures
plot(data)
#Ce plot permet de visualiser les scattr plot des différents couples de variables
nrow(data)
# On a 24 individus (voitures)
ncol(data)
# et 6 variables (caractéristiques)
cor(data)
levelplot(cor(data))
#On remarque d'aprés le plot et la matrice de corrélation des fortes corrélations entres
#les variables sauf peut être les couples (vit,larg),(larg,puiss),(long,vit),(poids,vit)

#ACP
acp.res = prcomp(data,scale=TRUE)
attributes(acp.res)
acp.res$sdev
# le champs sdev contient les écarts types de chacunes des directions principales calculées
# elles sont ordonées par ordre décroissant
acp.res$rotation
# le champs rotation contient les directions principale de nos data de la plus explicative
# à la moins explicative
acp.res$center
# center contient les valeurs moyennes utlisées pour centrées les variables originelles
acp.res$scale
# center contient les valeurs utilisées pour normaliser les variables originelles
acp.res$x
# x donne la représentation des individus dans l'espace des composantes principale

valpr = (acp.res$sdev)^2
barplot(valpr,main="Variance des composantes principales")
cumvalpr = cumsum(valpr/sum(valpr))
cumvalpr[1]
plot(cumvalpr)
abline(a=0.95,b=0)
abline(a=0.98,b=0)
# On remarque que le premier plan factoriel explique 77,6% de la variance des données
# les 3 premieres composantes principales permettent d'expliquer plus que 95% de la variance totale
# les 4 premieres composantes principales permettent d'expliquer plus que 98% de la variance totale
acp.res$rotation
# PC1 est une sorte de "moyenne" des variables
# PC2 ne retient pas nd, est croissant par rapport à la vitesse et la puissance, décriossant 
#     par rapport au poids, longueur et largeur, on peut dire qu'il représente la performance
#     du véhicule
# PC3 est croissant par rapport à la largeur et la vitesse et décroissant par rapport au poids
#     et nd.
# PC4 ne retient que la puissance la vitesse et la longueur
biplot(acp.res)
abline(h=0)
abline(v=0)
# On constate la formation de clusters sur le graphe. 

# Kmeans
means2=kmeans(acp.res$x,2)
means3=kmeans(acp.res$x,3)
means4=kmeans(acp.res$x,4)
# Cluster: un tableau associant à chaque individu son cluster
# centers: les coordonnées de chaque cluster
# totss: somme des distances euclidiennes carrées à l'origines
# withinss: la distortion relative à chaque cluster
# tot.withinss: somme de vecteur précédent, représente la distortion du clustering
# betweens: totss - tot.withins
# size: le nombre des éléments dans chaque cluster
# iter: nombre d'itérations effectuées


# Exercice 3

#Partie A

load("digits3-8.RData")
mImage<-function(row)
{
  mat=t(matrix(row,16,16))
  image(mat,axes=F,col=gray((0:255)/255))
}
mImage(d8[342,])
mImage(d3[42,])

# Partie B
# Split en train et test
trainI = sample(nrow(d3),1000)
d3train = d3[trainI,]
d3test = d3[-c(trainI),]
d8train = d8[trainI,]
d8test = d8[-c(trainI),]
# Calcul du "3" et "8" moyens
average3 = colMeans(d3train)
mImage(average3)
average8 = colMeans(d8train)
mImage(average8)
# Matrices de covariances
d3train = scale(d3train)
d8train = scale(d8train)
S3 = cov(d3train)
S8 = cov(d8train)
# Calcul des composantes principales
# Pour une matrice symétrique, la matrice d'entrée/sortie produite par SVD représente les
# composantes principales des données. Les valeurs singulières étant les variances
# suivant chacune des composantes.
acp3 = eigen(S3)
mod3_1 = acp3$vectors[,1]%*%t(acp3$vectors[,1])
mod3_2 = acp3$vectors[,2]%*%t(acp3$vectors[,2])
mImage(mod3_1)
mImage(mod3_2)
# Les modes propres ressemble au motif des données
# On construit la matrice de projections sur les 5 premières composantes principales
proj3 = solve(t(acp3$vectors[,1:5])%*%acp3$vectors[,1:5])%*%t(acp3$vectors[,1:5])
svd(proj3)$d
# toutes les valeures singluières de proj3 sont égales à 1 c'est donc un projecteur
var = cumsum(acp3$values/sum(acp3$values))
plot(var)
var[5]
min(which(var>0.95))
# Les 5 premières composantes principales permettent d'expliquer 32% de la variance
# En utilisant un projecteur de type proj3 on peut représenter les images représentant
# le chiffre 3 avec sa projection sur l'espace des 5 (ou plus) premiers plans factoriels.
# En projettant sur les 102 on garde 95% de la variance tout réduisant la taille des images
# par plus de 50%.
proj3 = solve(t(acp3$vectors[,1:102])%*%acp3$vectors[,1:102])%*%t(acp3$vectors[,1:102])
# La reconstruction se fera par le pseudo inverse du projecteur:
inv_proj3 = solve(t(proj3)%*%proj3+diag(rep(0.01,256)))%*%t(proj3)
# Le projecteur (compression) est donc u->proj3((u-centers)/scales)
# La reconstruction : v->(inv_proj3(v)*scales)+centers
center3 = attr(d3train,"scaled:center")
scale3 = attr(d3train,"scaled:scale")
center8 = attr(d8train,"scaled:center")
scale8 = attr(d8train,"scaled:scale")

reduire3<-function(u)
{
  return(proj3%*%((u-center3)/scale3))
}

reconstruire3<-function(v)
{
  return((inv_proj3%*%v)*scale3+center3)
}
# Test
v=reduire3(d3test[4,])
mImage(reconstruire3(v))
mImage(d3test[4,])

# Reconstruction avec la présence de bruit
#On masque 40% des pixels d'une image et on essaie de la reconstruire
maskI = sample(256,0.4*256)
masked<-function(u)
{
  u[maskI]=0
  return(u)
}
v = masked(d3test[5,])
mImage(v)
u=reconstruire3(reduire3(v))
mImage(u)
mImage(d3test[10,])

# Classification 8 vs 3

# Une méthode de calssification serait de comparer la distance d'une image au sous espace
# engendré par les n premieres composantes principales de chaque classes. 
# Le critère de décision serait donc de chiosir le sous espce le plus proche.
acp8 = eigen(S8)
mean(res3==3)
mean(res8==8)
# On remarque qu'on a une précision globale de 68,5% sur la classe 3 de 95% et sur la classe
# 8 de 42%. On peut améliorer la précision sur la classe 8 en projettant sur les deux premiers
# plans factoriels.

scale_3<-function(u){
  return((u-center3)/scale3)
}

scale_8<-function(u){
  return((u-center8)/scale8)
}

classifier<-function(u,n=100)
{
  dist3 = scale_3(u)%*%acp3$vectors[,1:n]%*%t(acp3$vectors[,1:n])%*%scale_3(u)
  dist8 = scale_8(u)%*%acp8$vectors[,1:n]%*%t(acp8$vectors[,1:n])%*%scale_8(u)
  if(dist3>dist8) return(8) else return(3)
}

res3=apply(d3test, 1,classifier)
res8=apply(d8test, 1,classifier)
mean(res3==3)
mean(res8==8)
# Une deuxième méthode serait de comparer les distances aux moyennes:

classifier2<-function(u)
{
  if(sum((u-average3)^2)>sum((u-average8)^2)) return(8) else return(3)
}

res2_3 = apply(d3test,1,classifier2)
res2_8 = apply(d8test,1,classifier2)
mean(res2_3==3)
mean(res2_8==8)
