#############esercitazione quaderno 1########################################

#ese1

#A pochi mesi dall'apertura del negozio "pilota", TIR ha elaborato i dati sui clienti che si
#sono iscritti al programma fedeltà del negozio. La tabella seguente mostra la
#distribuzione della frequenza della variabile "numero di acquisti durante i primi tre mesi
#di adesione al programma fedeltà"


setwd('../../ese_quaderno/esercitazione1/')

N_acquisti = c(1,2,3,5,6)
N_clienti = c(11,28,14,13,8)

dati <- data.frame(N_acquisti , N_clienti)


freq_ass <- dati$N_clienti

num_campionaria <- sum(dati$N_acquisti)

N <- sum(dati$N_clienti)

freq_rel <- freq_ass/N

freq_cum <- cumsum(freq_rel)

dati$frequenze_relative <- freq_rel
dati$frequenze_cumulate <- freq_cum

# calcolo dei 5 numeri di riepilogo

minimo <- min(N_acquisti)
massimo <- max(N_acquisti)
mediana <- N_acquisti[2]
Q1 <- N_acquisti[2]
Q3 <- N_acquisti[4]

plot(stepfun(N_acquisti , c(0,freq_cum)) , main= 'funzione_ripartizione_empirica')
