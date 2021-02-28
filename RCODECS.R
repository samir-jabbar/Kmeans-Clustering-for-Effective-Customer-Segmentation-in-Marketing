library(readxl)
quantité <- read.csv("C:/Users/user/Desktop/projet_costumers_segmentations/quantité.csv")
View(quantité)

X<-quantité$X2011.11
Y<-quantité$X2011.12

par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(X, main="quantité en 2011/11", sub=paste("Outlier rows: ", 
                                                 boxplot.stats(X)$out))
boxplot(Y, main="quantité en 2011/12", sub=paste("Outlier rows: ", 
                                                 boxplot.stats(Y)$out))

par(mfrow=c(1, 2))  # divide graph area in 2 columns

plot(density(X),xlim = c(0, 5000) ,main="Plot de densité: quantité en 2011/11",ylab="La densité", xlab="quantité")
polygon(density(X), col="blue")

plot(density(Y),xlim = c(0, 3000), main="Plot de densité: quantité en 2011/12",ylab="La densité", xlab="quantité")
polygon(density(Y), col="red")

#barplot(X)
#barplot(Y)


tc1=table(Y,X)
tc1

cor(X,Y)

library(lsr)
chisq.test(X,Y)

cramersV(X,Y)
plot(X,Y ,type="p",col=rainbow(25),xlab="quantité du mois actuel",ylab="quantité du mois prochain", main="Diagrame de Dispersión") 
plot(X,Y,xlim = c(0, 3000), ylim = c(0, 1000),type="p",col=rainbow(25),xlab="quantité du mois actuel",ylab="quantité du mois prochain", main="Diagrame de Dispersión") 
scatter.smooth(X, Y) 

X[is.na(X)] <- 0
Y[is.na(Y)] <- 0
sum(X)
sum(Y)
sum(X*Y)
sum(X*X)
sum(Y*Y)

cov(X,Y)

#calcul de a et b
a = cov(X,Y)/var(X)
a
b = mean(Y)-a*mean(X)
b
abline(b,a)

#CVT
SCT<-3951*var(Y)#n*var(Y)
SCT
#sce
SCE <- 3951*a*a*var(X)
SCE
#SCR
SCR<-SCT-SCE
SCR
#R
R<-1-SCR/SCT
R

[1] 173815543
[1] 0.7560262

