
# Rifacciamo gli esercizi svolti durante la prima esercitazione
# (vedi file Ex_1_soluzioni.pdf)

################################################
########################### ESERCIZIO 1
################################################

# A pochi mesi dall’apertura del negozio “pilota”, TIR ha elaborato i dati sui clienti che si
# sono iscritti al programma fedeltà del negozio. La tabella seguente mostra la
# distribuzione della frequenza della variabile "numero di acquisti durante i primi tre mesi
# di adesione al programma fedeltà":

Num_acquisti <- c(1,2,3,5,6)
Num_clienti <- c(11,28,14,13,8)

(dati <- data.frame(Num_acquisti, Num_clienti))

########################

# 2. Calcolare i cinque numeri di riepilogo per la variabile "N. di acquisti";
n = sum(dati$Num_clienti)
dati$freq_rel <- dati$Num_clienti/n
dati$freq_cum <- cumsum(dati$freq_rel)
dati
plot(stepfun(dati$Num_acquisti, c(0,dati$freq_cum)), verticals = F)

################################################
########################### ESERCIZIO 2
################################################

# TIR è una catena di negozi internazionale che vende prodotti innovativi per la cura
# del corpo. La catena non ha negozi in Italia. Tuttavia, un negozio “pilota” è stato
# aperto in una piccola città per valutare la redditività in questo mercato. I seguenti
# dati sono stati ottenuti da un campione di n = 11 clienti che hanno effettuato acquisti
# nel negozio pilota. Sono state raccolte le seguenti variabili:
#   - SPESE   -->   Importo della spesa (in Euro)
#   - TEMPO   -->   Tempo trascorso nel negozio (in minuti)
#   - TOP     -->   Numero di articoli acquistati che appartengono alla categoria di 
#                   prodotti “top” (ovvero con un costo maggiore)

SPESE	<- c(8.9,	12.6,	86.4,	75.2,	28.4,	26.5,	15.4,	67.8,	13.2,	10.0,	18.6)
TEMPO	<- c(5.7,	7.6,	15.8,	22.8,	17.3,	10.5,	9.5,	25.7,	18.2,	8.7,	15.2)
TOP	<- c(1,	2, 4, 5, 3, 4, 2, 4, 3, 1, 1)

########################

# 3. Dopo aver classificato la variabile SPESE nelle classi [5; 10), [10; 20), [20; 70) e
#   [70; 150), rappresentare l’istogramma corrispondente;
estremi_classi <- c(5,10,20,70,150)
h <- hist(SPESE, breaks = estremi_classi, right = F)
h$counts # frequenze assolute
h$density # densità

# calcoliamo le distribuzioni di frequenza a partire dai dati, senza usare la funzione "hist" 
spese_classi <- cut(SPESE, breaks = c(5,10,20,70,150), right = F)
n = length(SPESE)
freq_ass <- table(spese_classi)
freq_rel <- freq_ass/n
ampiezza_classi <- diff(estremi_classi)
densità <- freq_rel/ampiezza_classi

########################

# 4. In base alla classificazione eseguita al punto precedente, calcolare la media della
#   variabile SPESE. Discutere la validità del risultato ottenuto;
valori_centrali <- (estremi_classi[2:5] + estremi_classi[1:4])/2
# usiamo le frequenze relative
sum(valori_centrali * freq_rel)
# oppure quelle assolute
sum(valori_centrali * freq_ass)/n

# esiste una funzione già implementata che prevede come parametro i pesi
weighted.mean(x = valori_centrali, w = freq_rel)

########################

# 5. Calcolare la porzione di clienti che ha speso più di 20 euro;
# In questo caso consideriamo i dati grezzi
spese_maggiore_20 <- SPESE > 20
sum(spese_maggiore_20)/n

################################################
########################### ESERCIZIO 3
################################################

# Un tour operator ha selezionato dal proprio database un campione di n = 200 persone
# che hanno acquistato una vacanza organizzata per una determinata destinazione nel
# 2018. Per i clienti selezionati sono stati analizzate le variabili genere e importo (in
# euro) speso per escursioni extra (non incluse nel pacchetto).

dati <- matrix(c(47, 45, 29, 34, 28, 17), nrow = 2, byrow = T)
rownames(dati) <- c("Maschi","Femmine")
colnames(dati) <- c("[0, 50)", "[50, 200)", "[200, 500]")
dati

########################

# 1. Costruire l’istogramma della variabile “Importo” considerando l’intero campione;
importo <- colSums(dati)
freq_rel <- importo/sum(importo)
ampiezza_classi <- c(50, 150, 300)
densità <- freq_rel/ampiezza_classi
# con i dati raggruppati non possiamo usare la funzione hist
# riadattiamo la funzione barplot PERO' dobbiamo fare qualche modifica:
# annullare lo spazio tra le barre, specificare l'ampiezza delle classi
barplot(densità, space = 0, width = c(50, 150, 300),
        ylab = "Densità", xlab = "Importo")

################################################
########################### ESERCIZIO 4
################################################

# Un imprenditore ha deciso di aprire un nuovo bar vicino all’Università e ha svolto
# un’indagine su un campione di studenti. Li ha interrogati sulle loro preferenze delle
# caratteristiche di un bar:

preferenza <- c("Varietà di pietanze", "Abbondanza dei piatti", "Qualità dei panini",
                "Pulizia del locale", "Prezzi moderati", "Vicinanza all’uni")
Num_studenti <- c(13, 35, 52, 10, 43, 7)

(data.frame(preferenza, Num_studenti))

########################

# 1. Determinare una misura di sintesi adeguata per tale variabile;
indice_moda <- which(Num_studenti == max(Num_studenti))
preferenza[indice_moda]

########################

# 2. Costruire una rappresentazione grafica adeguata.
barplot(Num_studenti, names.arg = preferenza, 
        xlab = "Motivo di preferenza", ylab = "frequenze assolute")

################################################
########################### ESERCIZIO 5
################################################

# Uno studente ha esaminato tutti i bar e le caffetterie vicino all’Università raccogliendo i
# dati per le seguenti variabili:
# - POSTO     --> è facile trovare un posto a sedere? (0 = NO 1 = SI)
# - FINESTRE  --> numero di finestre/vetrine
# - PREZZO    --> prezzo del panino preferito
# - DISTANZA  --> Distanza dall’Università (in metri)

POSTO	<- c(1, 0, 1, 1, 1, 0, 1, 1, 0)
FINESTRE <- c(3, 1, 2, 1, 1, 2, 2, 3, 3)
PREZZO	<- c(2.8, 3.5, 3, 2.4, 3.2, 4, 4.5, 4.2, 3.6)
DISTANZA	<- c(650, 500, 700, 850, 200, 400, 350, 150, 200)

########################

# 1. Calcolare la media della variabile DISTANZA; 
mean(DISTANZA)

########################

# 2. Calcolare i quartili della variabile PREZZO e rappresentare il boxplot. 
# Inoltre, verificare la presenza di eventuali outliers
quantile(PREZZO, probs = c(0.25, 0.5, 0.75))
# oppure
summary(PREZZO)

# oppure, se richiesto di calcolarli senza usare la funzione di R:
n = length(PREZZO)

# visto che R lavora in forma vettoriale, possiamo calcolarli contemporaneamente
p = c(0.25, 0.5, 0.75)
prezzo_ordinato <- sort(PREZZO)
position <- p*(n+1)
pos <- floor(position)
delta <- position - pos
(quartili <- (1-delta) * prezzo_ordinato[pos] + delta * prezzo_ordinato[pos+1])

# controlliamo la presenza di outliers:
iqr <- quartili[3] - quartili[1]
quartili[1] - 1.5*iqr > min(PREZZO)
quartili[3] + 1.5*iqr < max(PREZZO)
# essendo entrambi FALSE possiamo concludere che non sono presenti valori
# anomali e che L=min e U=max

# per il boxplot, utilizziamo la funzione di R
boxplot(PREZZO)
#oppure, se lo preferite in orizzonatel:
boxplot(PREZZO, horizontal = T)

########################

# 4. Calcolare la varianza e la deviazione standard della variabile DISTANZA
# ATTENZIONE!!! le funzioni sd() e var() sono implementate per i CAMPIONI
# quindi hanno al denominatore (n-1)!!!
n = length(DISTANZA)
(n-1)/n * sd(DISTANZA)
(n-1)/n * var(DISTANZA)

# oppure, senza usare le formule
media <- sum(DISTANZA)/n
(varianza <- sum(DISTANZA^2)/n - media^2)
# equivalentemente: 
sum((DISTANZA - media)^2)/n

################################################
########################### ESERCIZIO 6
################################################

x <- c(
  44,47,53,43,47,52,45,52,46,52,
  48,44,46,43,48,51,49,49,45,46,
  46,48,49,48,49,53,44,47,46,49,
  48,48,52,50,47,45,49,46,50,54,
  48,52,52,50,47,51,46,50,50,48,
  52,44,41,49,50,48,49,49,53,47,
  46,46,47,52,47,42,44,44,46,50,
  51,47,48,51,51,47,44,47,45,44,
  47,47,44,48,45,48,50,49,46,47,
  53,50,46,43,50,52,45,49,50,47,
  45,51,51,47,42,49,51,44,43,46,
  49,47,50,46,49,43,51,41,48,53,
  50,47,47,47,49,41,43,46,47,49,
  46,46,50,52,51,44,50,48,49,45
)

########################

# 1. Qual è la percentuale di osservazioni presenti nell'intervallo (43.73, 51.73)?

indici <- which(x > 43.73 & x < 51.73)
length(x[indici])/length(x) * 100

########################

# 2. Ipotizzando di essere a conoscenza solo della media e della varianza:
media = 47.73
varianza = 8.45
# possiamo ugualmente calcolare la frequenza nell'intervallo?

# Applichiamo la diseguaglianza di Chebyshev 
# Poniamo 
a = 43.73
b = 51.73
# otteniamo il valore di delta
(delta = media - a);(delta = b - media)

# Otteniamo l'estremo inferiore della frequenza nell'intervallo: 
1 - varianza/delta^2

# In questo caso l'approssimazione l’approssimazione ottenuta con la 
# diseguaglianza di Chebishev è poco soddisfacente.











