
#installation du package tidyverse:
install.packages("tidyverse")
library(tidyverse)

#Chemin du fichier
file <- "C:/Users/fares/Documents/Far�s Almia/(7) Master Pro Data Science 2019-2020/Semestre 2/Partie 2/Atelier fouille de donn�es et machine learning/Projet/cars.csv"

#Lecture du fichier
Cars <- read.csv(file,sep = ";", header = 1)
Cars <- Cars[, names(Cars)!="Origin"]
Cars <- Cars[,-1]

#V�rifications
View(Cars)
print(Cars)

#Statistiques descriptives
summary(Cars)

#Nuage de points
pairs(Cars)

n <- nrow(Cars)
print(n)

#Centrage et r�duction des donn�es (cor = T) et calcul des coordonn�es factorielles (scores = T)
acp.Cars <- princomp(Cars, cor = T, scores = T)

#Print
print(acp.Cars)
#Summary
print(summary(acp.Cars))
#Propri�t�s associ�es
print(attributes(acp.Cars))

#Valeurs propres
Val.propres <- acp.Cars$sdev^2
print(Val.propres)

#scree plot (graphique des �boulis des valeurs propres)
plot(Val.propres,type="b",ylab="Valeurs propres",xlab="Composante",main="Scree plot")

#intervalle de confiance des val.propres � 95% (cf.Saporta, page 172)
val.basse <- Val.propres * exp(-1.96 * sqrt(2.0/(n-1)))
val.haute <- Val.propres * exp(+1.96 * sqrt(2.0/(n-1)))

#affichage sous forme de tableau
tableau <- cbind(val.basse,Val.propres,val.haute)
colnames(tableau) <- c("B.Inf.","Val.","B.Sup")
print(tableau,digits=3)

#**** corr�lation variables-facteurs ****
c1 <- acp.Cars$loadings[,1]*acp.Cars$sdev[1]
c2 <- acp.Cars$loadings[,2]*acp.Cars$sdev[2]
#affichage
correlation <- cbind(c1,c2)
print(correlation,digits=2)

#carr�s de la corr�lation (cosinus�)
print(correlation^2,digits=2)

#cumul carr�s de la corr�lation
print(t(apply(correlation^2,1,cumsum)),digits=2)

#*** cercle des corr�lations ***
plot(c1,c2,xlim=c(-1,+1),ylim=c(-1,+1),type="n")
abline(h=0,v=0)
text(c1,c2,labels=colnames(Cars),cex=0.5)
symbols(0,0,circles=1,inches=F,add=T)

#l'option "scores" demand� dans princomp est tr�s important ici
plot(acp.Cars$scores[,1],acp.Cars$scores[,2],type="n",xlab="Comp.1 - 74%",ylab="Comp.2 - 14%")
abline(h=0,v=0)
text(acp.Cars$scores[,1],acp.Cars$scores[,2],labels=rownames(Cars.actifs),cex=0.75)
