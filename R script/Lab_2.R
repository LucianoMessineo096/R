
################################################
########################### ESERCIZIO 1
################################################

# Data la seguente serie di dati, calcolare la media aritmetica, la mediana, 
# la varianza, la deviazione standard, il valore più grande, il valore più 
# piccolo, la somma di tutti i valori, il quadrato della somma di tutti i 
# valori, la somma del quadrato di tutti i valori

# 10, 2, 19, 24, 6, 23, 47, 24, 54, 77

x = c(10, 2, 19, 24, 6, 23, 47, 24, 54, 77)

########################

mean(x) #calcola la media aritmetica
median(x) #calcola la mediana
var(x) #calcola la varianza
sd(x) #calcola la deviazione standard
max(x) #riporta il valore più grande della sequenza
min(x) #riporta il valore più piccolo della sequenza
sum(x) #somma tutti i valori
sum(x)^2 #calcola il quadrato della somma di tutti i valori
sum(x^2) #calcola la somma dei quadrati di tutti i valori

################################################
########################### ESERCIZIO 2
################################################
# Rappresentare i seguenti dati utilizzando il grafico a torta. 
# I dati sono relativi alle elezioni dell'EuroParlamento del 2009: 
# su un totale di 736 europarlamentari, questi sono i numeri dei singoli 
# partiti presenti:

# EPP: 265
# S&D: 184
# ALDE: 84
# Greens-EFA: 55
# ECR: 54
# EUL-NGL: 35
# EFD: 32
# NI: 27

########################

slices <- c(265, 184, 84, 55, 54, 35, 32, 27)
lbls <- c("EPP", "S&D", "ALDE", "Greens-EFA", "ECR", "EUL-NGL", "NFD", "NI")
pie(slices, labels = lbls, main="Grafico a torta dei partiti presenti 
    all'EuroParlamento 2009")

# Aggiungere le percentuali:

pct <- round(slices/sum(slices)*100) #calcolo delle percentuali
lbls <- paste(lbls, pct) # aggiungo il numero percentuale alle etichette
lbls <- paste(lbls,"%",sep="") # aggiungo il simbolo % alle etichette
pie(slices, labels = lbls, main="Grafico a torta dei partiti presenti 
    all'EuroParlamento 2009")


################################################
########################### ESERCIZIO 3
################################################

# Sia y l'insieme di dati

y <- c(2,2,2,3,2,3,4,5,6,6,5,5,4,5,6,7,8,9)

########################

# calcolare la media di y
mean(y)

# calcolare la somma di y
sum(y)

# visualizzare in un istogramma la serie di dati
hist(y)

# nell'istogramma cambiare il numero di classi, selezionarne 10
hist(y, nclass=3)

# nell'istogramma visualizzare la densità
hist(y, freq=F)

# dall'istogramma potete estrarre tutti i valori delle frequenze
freq_ass <- hist(y)$counts

# verificare che la somma delle frequenze sia uguale al 
# numero totale di elementi di y

sum(freq_ass)==length(y)

################################################
########################### ESERCIZIO 4
################################################

# Importare i dati dal file meteo1.txt
# Il file da leggere contiene le seguenti variabili incolonnate:
# 
# anno mese giorno giornoanno giornosett ora minuti temp umid press velvento dirvento cielounito 
#
# ATTENZIONE: il file meteo1.txt non contiene una intestazione che specifichi 
# il nome delle variabili.

dati1 <- read.table("meteo1.txt")

# associamo i nomi alle variabili

colnames(dati1) <- c("anno", "mese", "giorno", "giornoanno", "giornosett", "ora", "minuti", "temp", "umid", "press", "velvento", "dirvento", "cielounito")


################################################
########################### ESERCIZIO 5
################################################

# Importare i dati dal file meteo2.txt
# Il file da leggere contiene le seguenti variabili incolonnate:
# 
# anno mese giorno giornoanno giornosett ora minuti temp umid press velvento dirvento cielounito 
#
# ATTENZIONE: il file meteo2.txt contiene già l'intestazione che specifica
# il nome delle variabili

dati2 <- read.table("meteo2.txt", header=TRUE)














