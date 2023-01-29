# calcolo della media dei primi 5 numeri naturali

#n=5

#media <- (n*(n+1))/(2*n)

#str(n)

######################
# data frame

X <- data.frame(a=1:4 , sesso =c("M","F",'F','M'))

class(X)

dim(X)

X$eta<-c(2,3,4,5) # aggiungo una nuova colonna al frame 

X[X$sesso=='M' , 'eta']

summary(X$eta) # fornisce min quartile1 mediana media quartile2 quartile3 max

#distribuzione di frequenza

table(X$sesso) # frequenza assoluta

table(X$sesso)/lenght(X$sesso) # frequenza relativa

n=dim(X)[1]

table(X$sesso)/n


######






