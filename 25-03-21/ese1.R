##########esercitazione 25/03###################

################################################################################
#ese1

#data la seguente serie di dati calcolare la media aritmetica, la mediana
#la varianza, la deviazione standard,il valore piu grande 
#il piu piccolo,la somma di tutti i valori, il quadrato della somma di tutti i valori,
# la somma dei quadrati di tutti i valori

# 10,2,19,24,6,23,47,24,54,77


# vi sono delle funzioni che fanno gia tutto questo 
# intanto salviamo all'interno di un array tutti i valori

x = c(10,2,19,24,6,23,47,24,54,77)


media <- mean(x) 
mediana <- median(x) 
varianza <- var(x) #  calcolo della varianza campionaria
dev_std <- sd(x) # deviazione standard campionaria
massimo <- max(x)
minimo <- min(x)
somma <- sum(x)
q_somma <- sum(x)^2
somma_quadrati <- sum(x^2)

# adesso calcolo la varianza della popolazione 

n <- length(x)

var_p <- varianza * (n-1)/n


###########################################################################
#ese2

#rappresentare i seguenti dati utilizzando il grafico a torta
#i dati sono relativi alle elezioni europarlamentari del 2009
#su un totale di 736 europarlamentari, questi sono i numeri dei singoli
#partiti presenti :

# EPP 265
# S&D 164
# ALDE 84
# Greens-EFA 55
# ECR 54
# EUL-NGL 35
# EFD 32
# NI 27

slices <- c(265,164,84,55,54,35,32,27)
lbls = c('EPP','S&D','ALDE','Greens-EFA','ECR','EUL-NGL','EFD','NI')

pie(slices , labels=lbls)

# nel caso in cui i dati si trovassero all'interno del data frame

dati <- data.frame(slices,lbls)


pie(dati$slices , labels = dati$lbls , main = 'grafico tramite data frame')

# nel caso in cui  dati si trovassero all'interno di una matrice

dati2 <- cbind(slices , lbls)

pie(as.numeric(dati2[,1]) , labels=dati2[,2] , main='grafico tramite matrice')

# aggiungo adesso le percentuali


percentuale <- round(slices/sum(slices) *100)
lbls <- paste(lbls,percentuale)
lbls <- paste(lbls ,'%',sep='')




###############################################################################
#ese3

y <- c(2,2,2,3,2,3,4,5,6,6,5,5,4,5,6,7,8,9)

media2<- mean(y)

somma2<-sum(y)


hist(y)

hist(y , nclass = 3)

hist(y,freq=FALSE,main='istogramma con densità ')

# per estrapolare dall'istogramma le frequenze assolute

freq_ass <- hist(y)$counts

###############################################################################
#ese4

setwd('../../../Desktop/CODICI/R/dati/dati/')
#importo i dati

dati<-read.table('meteo1.txt')

colnames(dati) = c('anno','mese','giorno','giornoanno','giornosett',
                   'ora','min','temp','um','press','velvento','dirvento',
                   'cielounito')















































