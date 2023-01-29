
# cariachiamo le librerie aggiuntive per utilizzare alcune funzioni
# che non sono incluse nei pacchetti base
library(readxl) # per import file con estensione .xlsx

# solo per rendere i grafici più gradevoli, personalizzo i colori
myPalette <- c(rgb(0,30,98,max = 255), rgb(0,128,128, max = 255), 
               rgb(220,20,60, max = 255), "lightsteelblue")

# impostiamo la working directory per comodità
setwd("Desktop/UniPa/Docenza/Triennale/06644 - Statistica (9 CFU)/dati")

# importiamo i dati
dati <- read_excel("Corso di Statistica 2020_2021 (Risposte).xlsx", 
                   sheet = "Risposte")

# controlliamo che i dati siano stati importati correttamente
View(dati)

# vediamo la struttura dei dati:
# variabili e modalità
str(dati)

# numerosità campionaria e numero di variabili
n = nrow(dati)
p = ncol(dati)

# elenchiamo i nomi delle variabili, in questo caso corrispondono alle 
# domande del questionario
domande <- variable.names(dati)
# [1] "ID"                                                                                                                                                                                                                                                                                                                                                                   
# [2] "Indicare l'anno di corso"                                                                                                                                                                                                                                                                                                                                             
# [3] "Di che tipo è la tua scuola di provenienza?"                                                                                                                                                                                                                                                                                                                          
# [4] "Nel tuo piano di studi, quale curriculum/profilo pensi di scegliere?"                                                                                                                                                                                                                                                                                                 
# [5] "Che lavoro ti piacerebbe fare?"                                                                                                                                                                                                                                                                                                                                       
# [6] "Qual è la tua altezza (esprimerla in cm)"                                                                                                                                                                                                                                                                                                                             
# [7] "Quanti fratelli/sorelle hai?"                                                                                                                                                                                                                                                                                                                                         
# [8] "Quale delle seguenti relazioni corrisponde al seguente diagramma di Venn?...8"                                                                                                                                                                                                                                                                                        
# [9] "Quale delle seguenti relazioni corrisponde al seguente diagramma di Venn?...9"                                                                                                                                                                                                                                                                                        
# [10] "Cosa indica questo simbolo?"                                                                                                                                                                                                                                                                                                                                          
# [11] "Quale linguaggio di programmazione conosci?"                                                                                                                                                                                                                                                                                                                          
# [12] "Nel tuo percorso accademico, pensi di voler partecipare al progetto Erasmus?"                                                                                                                                                                                                                                                                                         
# [13] "A cena al ristorante in compagnia di un gruppo di amici, è il momento di pagare il conto. Tutti hanno ordinato una pizza ed una bibita. Quale metodo di pagamento preferiresti?"                                                                                                                                                                                      
# [14] "A cena al ristorante in compagnia di un gruppo di amici, è il momento di pagare il conto. C’è chi ha mangiato con appetito: dall’antipasto al dolce, senza indugi. C’è chi non ha potuto rinunciare al buon vino, magari per sfoggiare le sue conoscenze enologiche. C’è chi è sempre a dieta e si è limitato ad un’insalata. Quale metodo di pagamento preferiresti?"
# [15] "Quanto ti incuriosisce questo corso?"                                                                                                                                                                                                                                                                                                                                 
# [16] "Qual è la tua altezza (esprimerla in cm) [CLEAN]"                                                                                                                                                                                                                                                                                                                     
# [17] "Che lavoro ti piacerebbe fare? [GROUP]"  

# per comodità, assegno nuovi nomi alle variabili, ovvero il numero della domanda
colnames(dati) <- paste0("X",1:p)

### analizziamo la domanda 4
# calcoliamo la distribuzione delle frequenze assolute
freq_ass <- table(dati$X4)
sum(freq_ass)

freq_rel <- freq_ass/n
sum(freq_rel)

pie(freq_ass, border="white", col=myPalette, main = domande[4])
barplot(freq_ass, border="white", col=myPalette, main = domande[4], ylab = "Frequenze assolute")

### analizziamo la domanda 6
str(dati$X6)
table(dati$X6)

# La variabile altezza è una variabile quantitativa continua
# Nell'importazione viene letta come char, dunque c'è qualche problema 
# nella lettura del dato:
# Nel file .xlsx ho creato una nuova colonna contenente il dato "pulito"
str(dati$X16)
table(dati$X16)

x = dati$X16
summary(x)
# Rappresentiamo le frequenze assolute tramite un istogramma
hist(x, col=myPalette[2], freq = T)

# Rappresentiamo le frequenze relative
# In questo caso coincidono con le densità, perchè l'ampiezza delle
# classi è sempre uguale
hist(x, col=myPalette[2], probability = T)

range(x)

# scegliamo un numero di classi diverso:
k = floor(sqrt(n))
a <- (max(x) - min(x))/k
estremi_classi <- seq(from = min(x), to = max(x), by = a)
hist(x, col=myPalette[2], probability = T, breaks = estremi_classi)

# scegliamo di unire delle classi
estremi_classi_group <- estremi_classi[-c(2:3,10:11)]
hist(x, col=myPalette[2], probability = T, breaks = estremi_classi_group)

# SBAGLIATO, guardate solo la differenza
# tra l'altro R vi da un Warning
hist(x, col=myPalette[2], freq = T, breaks = estremi_classi_group)

# specifichiamo un numero diverso di classi
hist(x, col=myPalette[2], probability = T, nclass = 40)
# in questo modo emergono i picchi legati all'arrotondamento, 165, 170, 175, 180
# però ci sono "buchi"

# differenza calcolo mediana
x_ordinati <- sort(x)

mediana_caso_1 <- quantile(x, probs = .5)

position <- 0.5*(n+1)
pos <- floor(position)
delta <- position - pos
mediana_caso_2 <- (1 - delta) * x_ordinati[pos] + delta * x_ordinati[pos + 1]

quartile_1 <- quantile(x, probs = .25)

position <- 0.25*(n+1)
pos <- floor(position)
delta <- position - pos
(1 - delta) * x_ordinati[pos] + delta * x_ordinati[pos + 1]

# Simuliamo dei dati a caso
set.seed(1) # fissiamo il seme casuale
n=200
y <- rnorm(n)
summary(y)
quantile(y)
m1 <- quantile(y,probs = .5)

y_ordinati <- sort(y)
position <- 0.5*(n+1)
pos <- floor(position)
delta <- position - pos
m2 <- (1 - delta) * y_ordinati[pos] + delta * y_ordinati[pos + 1]

freq_cum <- cumsum(table(y)/n)
m3 <- as.numeric(names(freq_cum[freq_cum==.5]))

hist(y)
abline(v=m1, col=1, lty=2, lwd=2)
text("m1", x = .3, y=40, cex = 2)
abline(v=m3, col=2, lty=1, lwd=2)
text("m3", x = -.8, y=40, cex = 2, col=2)




###
library(wordcloud2)
words <- table(dati$X17)
wordcloud2(data=words, size=1.6)

words_gestionale <- table(dati$X17[dati$X4=="Gestionale"])
wordcloud2(data=words, size=1.6)
words_informatico <- table(dati$X17[dati$X4=="Informatico"])
wordcloud2(data=words, size=1.6)

###





