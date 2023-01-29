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



##############################################################################
###############################################################################
#ese3

y <- c(2,2,2,3,2,3,4,5,6,6,5,5,4,5,6,7,8,9)

media2<- mean(y)

somma2<-sum(y)


hist(y)

hist(y , nclass = 3)

hist(y,freq=FALSE,main='istogramma con densit‡ ')

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


###############################################################################
#ESE6
setwd('../../../Desktop/CODICI/R/eserciziario/n6/')

dati <- c("0", "A", "0", "AB", "A", "A", "0", "0", "B", "A", "0", "A",
          "AB", "B", "0", "0", "0", "A", "B", "A", "A", "0", "A", "A", 
          "0", "B", "A","0", "AB", "A", "0", "0", "A", "B", "A", "A", 
          "A", "0", "B", "0", "0", "A","0", "A", "B", "0", "AB", "A",
          "0", "B")

modalit‡ <- c('O','A','B','AB')

n <- length(dati)

freq_ass <- table(dati)

freq_rel <- freq_ass/n

freq_cum <- cumsum(freq_rel)


distr_freq <- data.frame(modalit‡ , freq_ass , freq_rel , freq_cum)


barplot(freq_ass , ylab='frequenze assolute' , main='distribuzione gruppo sanguigno')


###############################################################################
#ESE7
setwd('../../../Desktop/CODICI/R/eserciziario/n7/')

concentrazioni <- c(6.2, 9.1, 2.4, 3.6, 1.9, 1.7, 4.5, 4.2, 3.3, 5.1, 6.0, 1.8,
                    2.3, 4.9, 3.7, 3.8, 5.5, 6.4, 8.6, 9.3, 7.7, 5.4, 7.2, 4.9, 6.2)

estremi_classi <- c(0,2,4,7,10)

freq_ass <- cut(concentrazioni , breaks=estremi_classi)

table(freq_ass)



################################################################################
#ESE9
setwd('../../../Desktop/CODICI/R/eserciziario/n9/')

dati <-  c(1, 3, 2, 5, 4, 2, 2, 3, 3, 2, 3, 4, 4, 3, 2, 5, 4, 3, 3, 1)


n=length(dati)

modalit‡ <- c(1,2,3,4,5)

freq_ass <- table(dati)

freq_rel <- freq_ass/n

freq_cum <- cumsum(freq_rel)

distr_freq <- data.frame(freq_ass , freq_rel , freq_cum)

plot(stepfun(modalit‡, c(0,freq_cum)) , main= 'funzione_ripartizione_empirica')


###############################################################################
#ESE10

setwd('../../../Desktop/CODICI/R/eserciziario/')

##ASSUMENDO CHE I DATI NON SIANO DISTRIBUITI IN CLASSI

dati <- c(28, 5, 4, 12, 17, 12, 14, 5, 4, 4, 11, 8, 4, 26, 17, 6, 0, 19, 8, 38)

N <- length(dati)

freq_ass<- table(dati)

freq_rel<- freq_ass/N

freq_cum<- cumsum(freq_rel)

distr_freq<-data.frame(freq_ass,freq_rel,freq_cum)

fun_rip<-plot(stepfun(distr_freq$dati , c(0,freq_cum)),main='funzione di ripartizione')

media<-mean(dati)

mediana<-median(dati)


##############################################################################
###############################################################################

#MODA MEDIA E MEDIANA ESERCIZI



###############################################################################
#ESE21

setwd('../../../Desktop/CODICI/R/eserciziario/')

estremi_classi <- seq(from = 0, to = 120, by = 30)

classi<-c('[0,30)','[30,60)','[60,90)','[90,120)')

freq_ass<-c(20,60,80,40)

totale_ascoltatori<-sum(freq_ass)

freq_rel<-freq_ass/totale_ascoltatori

freq_cum<-cumsum(freq_rel)

centrali<-c(15,45,75,105)


#COSTRUISCO LA DISTRIBUZIONE DELLE FREQUENZE
distr_freq<-data.frame(classi)
distr_freq$freq_ass<-freq_ass
distr_freq$freq_rel<-freq_rel
distr_freq$freq_cum<-freq_cum
distr_freq$centrali<-centrali

#COSTRUISCO LA FUNZIONE DI RIPARTIZIONE EMPIRICA

plot(stepfun(estremi_classi, c(0,freq_cum)) , main= 'funzione_ripartizione_empirica')
################################################################################
#ESE22

setwd('../../../Desktop/CODICI/R/eserciziario/')


dati<-c(52,44,43,44,40,29,31,39,35,39)

freq_ass<-table(dati) # permette di ordinare i dati e calcolare le frequenze assolute

distr_freq<-data.frame(freq_ass)

n<-sum(freq_ass)


freq_rel<-freq_ass/n

freq_cum<-cumsum(freq_rel)


distr_freq$freq_rel<-freq_rel
distr_freq$freq_cum<-freq_cum

#1) ricavo la moda , ovvero l'indice di posizione a cui corrisponde la maggiore frequenza assoluta o relativa

moda<-max(freq_rel)

#2) calcolo i 5 numeri di riepilogo

massimo<-max(dati)
minimo<-min(dati)
media<-mean(dati)

#2.1) calcolo il primo quantile corrispondente a p=0.25
p1<-0.25
posizione1<-p1*(n+1)
pos1<-2
decimale1<-posizione1-pos1

quantile1<-(1-decimale1)*dati[pos1]+decimale1*dati[pos1+1]

#2.1) calcolo il terzo quantile corrispondente a p=0.75
p3<-0.75
posizione3<-p3*(n+1)
pos3<-8
decimale3<-posizione3-pos3

quantile3<-(1-decimale3)*dati[pos3]+decimale3*dati[pos3+1]

#calcolo altri indici di posizione e di forma

mediana<-median(dati)
varianza<-var(dati)
dev_std<-sd(dati)

#calcolo funzione di ripartizione empirica , e in quanto variabile quantitativa continua , calcolo l'istogramma

istogramma<-hist(dati,freq=TRUE,main='istogramma')

# per quanto riguarda la distribuzione delle frequenze possiamo notare che 

quantile3-mediana>mediana-quantile1
media>mediana

#dunque la distribuzione risulta asimmetrica a destra , con media centrata in 39.6 circa in 40 

dati_ordinati<-c(29,31,35,39,40,43,44,52)
fun_rip<-plot(stepfun(dati_ordinati, c(0,freq_cum)))

###############################################################################
#ese 23

distr<-c(37.3,39.2,44.2,44.5,53.8,56.6,59.3,62.4,66.5)

dim<-length(distr)

#1. di che tipo di carattere si tratta?

#si tratta di un carattere quantitativo continuo

#2. Di che tipo di distribuzione si tratta?

#sono dati grezzi

#3. Calcolare la moda di questa distribuzione;

dati<-data.frame(distr)

freq_ass<-table(distr)

dati$freq_ass<-freq_ass

freq_rel<-freq_ass/dim

dati$freq_rel<-freq_rel

#dalle frequenze si puo evincere che la moda non Ë definita in quanto ogni valore della ditribuzione ha la stessa frequenza

#4. Calcolare la media;

media<-mean(distr)

#5. Calcolare la mediana.

mediana<-median(distr)

###############################################################################
#ese 24

distr<-c(20,40,22,22,21,21,20,10,20,20,
         20,13,18,50,20,18,15,8,22,26,
         22,10,20,22,22,21,15,23,30,12,
         9,20,40,22,29,19,15,20,20,20,
         20,15,19,21,14,22,21,35,20,22)

dim<-50
#1. Di che carattere si tratta?

# ha carattere quantitativo continuo

#2. Costruire la distribuzione in classi di questo carattere, utilizzando le seguenti classi:
#  (7.96; 18.5] (18.5; 29] (29; 39.5] (39.5; 50]

estremi_classi <- c(7.96,18.5,29,39.5,50)

freq_ass <- cut(distr, breaks=estremi_classi)


dati<-data.frame(table(freq_ass))

freq_rel<-dati$Freq/dim

dati$freq_rel<-freq_rel

#3. Determinare la classe modale.

massimo<-max(freq_rel)

#dunque la classe modale corrisponder‡ con la frequenza relativa maggiore , ed in questo caso e la classe (18.5 , 29]


################################################################################

#ese25

#1. poiche la distibuzione Ë fortemenete influenzata dai valori del reddito dei 2 nuovi clienti , cio comporta che la media subir‡
#un aumento significativo , cio comporta uno spostamento della distribuzione , poiche la media viene dunque influenzata in tal  odo sicuramente 
#la mediana Ë l'indice che esprime meglio il reddito dei 42 clienti , infatti risulta maggiormente robusto rispetto all'indice precedente 

#2.come si evince dalla tabella la deviazione standard subisce un aumento significativo , cio comporta il fatto che la distribuzione dei redditi si
#discosti dalla mediana di molto , pertanto il range interquartile risulta l'indice piu appropriato poiche maggiormente robusto rispetto al precedente 


##############################################################################

#ese26

distr_1<-c(3,5,6,7,9)
distr_2<-c(3,5,6,7,20)

media1<-mean(distr_1)
media2<-mean(distr_2)

#calcolo quantili distr_1

posizione1<-0.25*6
pos1<-1
dec1<-posizione1-pos1

quant1<-(1-dec1)*distr_1[pos1]+dec1*distr_1[pos1+1]

posizione2<-0.75*6
pos2<-4
dec2<-posizione2-pos2

quant3<-(1-dec2)*distr_1[pos2]+dec2*distr_1[pos2+1]

IQR1<-quant3-quant1


#calcolo quantili distr2

posizione3<-0.25*6
pos3<-1
dec3<-0.5

quantile1<-(1-dec3)*distr_2[pos3]+dec3*distr_2[pos3+1]


posizione4<-0.75*6
pos4<-4
dec4<-0.5

quantile3<-(1-dec4)*distr_2[pos4]+dec4*distr_2[pos4+1]

IQR2<-quantile3-quantile1

#calcolo delle medie 

mediana1<-median(distr_1)
mediana2<-median(distr_2)

#ci sono da fare anche gli altri ma seguono la stessa logica
###############################################################################

#ese27

sample_length<-3000

#poiche il primo quantile corrisponde al 25% delle osservazioni

quantile1<-sample_length*25/100

testi_piu_20_char<-3000-750

percentuale<-2250/3000*100

#dunque la percentuale di testi che hanno piu di 20 caratteri Ë del 75%

###############################################################################
#ese28

rendimenti<-c(37.3,39.2,44.2,44.5,53.8,56.6,59.3,62.4,66.5)

rendimenti_ord<-sort(rendimenti)

n<-length(rendimenti_ord)

#calcolo dei 5 numeri di sintesi
posizione<-c(0.25,0.5,0.75)*(n+1)
pos<-floor(posizione)
delta<-posizione-pos
quantili<-(1-delta)*rendimenti_ord[pos]+delta*rendimenti_ord[pos+1]

minimo<-min(rendimenti_ord)
massimo<-max(rendimenti_ord)

#boxplot della distribuzione

boxplot(rendimenti , ylab='rendimenti')

#calcolo della varianza e devianza

varianza<-var(rendimenti_ord)
devianza<-sd(rendimenti_ord)

#calcolo coefficente di variazione 

CV<-devianza/mean(rendimenti_ord)
###############################################################################
#ese29


media_peso<-9
devianza_peso<-1.5

media_volume<-2.7
devianza_volume<-0.6


CV1<-devianza_peso/media_peso
CV2<-devianza_volume/media_volume

#il volume e maggiormente variabile 
###############################################################################

#ese30

#riprendo l'esercizio 24

distr<-c(20,40,22,22,21,21,20,10,20,20,
         20,13,18,50,20,18,15,8,22,26,
         22,10,20,22,22,21,15,23,30,12,
         9,20,40,22,29,19,15,20,20,20,
         20,15,19,21,14,22,21,35,20,22)

dim<-50
#1. Di che carattere si tratta?

# ha carattere quantitativo continuo

#2. Costruire la distribuzione in classi di questo carattere, utilizzando le seguenti classi:
#  (7.96; 18.5] (18.5; 29] (29; 39.5] (39.5; 50]

estremi_classi <- c(7.96,18.5,29,39.5,50)

freq_ass <- cut(distr, breaks=estremi_classi)


dati<-data.frame(table(freq_ass))

freq_rel<-dati$Freq/dim

dati$freq_rel<-freq_rel

#3. Determinare la classe modale.

massimo<-max(freq_rel)

#dunque la classe modale corrisponder‡ con la frequenza relativa maggiore , ed in questo caso e la classe (18.5 , 29]


# adesso ricavo varianza e deviazione standard

#ricavo per prima cosa i centrali delle classi


centrali<-c(13.23,23.75,34.25,44.75)

dati$centrali<-centrali

media<-sum(centrali*freq_rel)


varianza<- (dim/(dim-1))*((sum(centrali^2 * freq_rel))-media^2)

deviazione_standard<-sqrt(varianza)
###############################################################################

#ese31

estremi_classi<-c(1,3,6,12,24)
classi<-c('(1,3]','(3,6]','(6,12]','(12,24)')

freq_ass<-c(10,42,38,8)

n<-sum(freq_ass)

dati<-data.frame(classi,freq_ass)

centrali<-c(2,3.5,9,18)

dati$centrali<-centrali

freq_rel<-freq_ass/n

dati$freq_rel<-freq_rel

#media=sum(ci*fi) per i from 0 to n


media<-sum(centrali*freq_rel)

varianza<-(n/(n-1))*((sum((centrali^2)*freq_rel))- media^2)

dev_std<-sqrt(varianza)


media_sett<-media*4

varianza_sett<-varianza*16

dev_std_sett<-sqrt(varianza_sett)

CV<-dev_std/media
CV_sett<-dev_std_sett/media_sett

#cambiando l'unit‡ di misura non vi e differenza
###############################################################################

#ese32

sesso<-c('F','M','F','F','F')
et‡<-c(51,24,33,17,76)
stato_c<-c('Coniugato/a','celibe/Nubile','Coniugato/a','Celibe/Nubile','Vedovo/a')
reddito_l<-c('2600-5200','10400-15600','10400-15600','2600-5200','2600-5200')
q_week<-c(20,20,20,20,20)
q_day<-c(20,15,10,15,20)

dati<-data.frame(sesso,et‡,stato_c,reddito_l,q_week,q_day)

media_week<-sum(q_week)/length(q_week)
media_day<-sum(q_day/length(q_day))

var_week<-sum((q_week-media_week)^2)/length(q_week)
dev_std_week<-sqrt(var_week)

var_day<-sum((q_day-media_day)^2)/length(q_day)
dev_std_day<-sqrt(var_day)

###############################################################################

#ese33

media<-30

varianza<-36

dev_std<-sqrt(varianza)

intervallo<-c(22,38)

#applico chebishev  I(delta)={a=media-delta < x < media+delta=b}

#delta=a-media
#delta=b-media

#Fr[a<x<b]>= 1-b/delta^2


delta=media-intervallo[1]

FR<-1-(38/delta^2)

##############################################################################

#ese34


media<-3
dev_std<-0.8
varianza<-dev_std^2

# FR>= 1-varianza/delta^2 =0.75

delta<-sqrt(varianza/(1-0.75))

#I(delta)={a=media-delta <x<b=media+delta}

a<-media-delta
b<-media+delta

#l'intervallo e 1.4 ,4.6
###############################################################################

#ese35

#Un uomo d'affari nell'ultimo mese Ë andato in viaggio a Londra per 10 volte. Il costo medio del
#biglietto aereo Ë 120, con una varianza pari a 7. Se l'uomo avesse prenotato tutti i voli da Londra, sapendo
#che il cambio Ë 1 euro = 0.87 sterline e che c'Ë un costo fisso della commissione pari a una sterlina per ciascun
#cambio, quanto avrebbe speso? Calcolare il costo medio in sterline e la varianza.

costo_m_b_euro<-120
varianza<-7

costo_fisso<-1 # espresso in sterline
n_viaggi<-10

#se fosse partito da londra avrebbe speso


costo_m_b_sterline<-costo_m_b_euro*0.87

costo_tot<-costo_m_b_sterline * n_viaggi + costo_fisso *10


media_sterline<-costo_m_b_sterline + 1

varianza_sterline<-(varianza^2) * (0.87^2)
###############################################################################

#ese36

#In una classe di 25 studenti, 24 hanno svolto un esame in classe e un solo studente Ë stato sottoposto
#a una prova di recupero il giorno successivo. Il professore ha valutato il primo blocco di esami, per i quali il
#punteggio medio Ë risultato di 74 punti con una deviazione standard di 8.9 punti. La prova di recupero dello
#studente del giorno dopo ha riportato un punteggio di 64 punti.
#1. Il punteggio del nuovo studente fa aumentare o diminuire il punteggio medio?
#2. Quale Ë la nuova media?

n_studenti<-25

studenti_esame<-24

studente_rec<-1

media_esame<-74

dev_std_esame<-8.9

punteggio_rec<-64

# calcolo la media

nuova_media<-(media_esame *studenti_esame + punteggio_rec*studente_rec)/n_studenti

#la nuova media si discosta dalla precedente , in quanto il voto dello studente che ha svolto la prova di recupero,piu basso rispetto
#alla media, ne ha fatto abbassare il valore
################################################################################

#ese39

#Nel censimento del 2000 ogni persona residente negli USA doveva scegliere da un lungo elenco la
#propria razza. La categoria "Ispanico/latin" Ë un caso a parte poichÈ in essa vi possono essere tante razze
#diverse. Se scegliamo un residente negli USA in modo casuale, in base ai dati del censimento del 2000 abbiamo
#le seguenti probabilit‡:
 

razze<-c('Asiatici','Neri','Bianchi','Altro')
colonna2<-c(0.000,0.003,0.060,0.062)
colonna3<-c(0.036,0.121,0.691,0.027)

dati<-data.frame(razze)

dati$Ispanici<-colonna2
dati$Non_Ispanici<-colonna3

#Verifica che questa tabella di probabilit‡ sia corretta.

#lo spazio campionario si suddivide in Ispanici e Non ispanici , cio comporta che questo sia 
#stato analizzato nella sua interezza , dunque :

Prob_Ispanici<-sum(dati$Ispanici)
Prob_Non_Ispanici<-1-Prob_Ispanici
Prob_Omega<-Prob_Ispanici + Prob_Non_Ispanici

#poi ogni valore di probabilit‡ come possiamo notare e compreso tra 0 e 1 

#2. Quanto vale la probabilit‡ che un americano scelto in modo casuale sia ispanico?

#la probabilita e data dalla variabile Prob_Ispanici = 0.125

#3. I bianchi di origine non ispanica rappresentano da sempre la maggioranza di residenti negli USA. Quale
#    Ë la probabilit‡ che un americano scelto in modo casuale non sia membro di questo gruppo?

Prob_Bianchi_no_ispanici<-dati$Non_Ispanici[3]

Prob_NO_Bianchi_No_ispanici<-1-Prob_Bianchi_no_ispanici
###############################################################################
#esercizi Probabilita
###############################################################################
#ese40

#» stato chiesto a 500 soggetti (maschi e femmine) abitanti di un'area metropolitana se amano fare
#shopping. 136 dei 250 uomini intervistati e 224 delle 250 donne hanno risposto affermativamente. Scelto a caso
#un soggetto, qual Ë la probabilit‡ che:
#1. ami fare lo shopping;
#2. sia una donna e ami fare shopping;
#3. sia una donna o ami fare shopping;
#4. sia un uomo o una donna.

n_soggetti<-500
n_uomini<-n_soggetti/2
n_donne<-n_uomini

uomini_SI<-136
uomini_NO<-n_uomini-uomini_SI
donne_SI<-224
donne_NO<-n_donne-donne_SI

n_soggetti_SI<-uomini_SI+donne_SI
n_soggetti_NO<-uomini_NO+donne_NO

#1. A=evento soggetto che ama fare shopping

Prob_A<-n_soggetti_SI/n_soggetti

Prob_NON_A<-1-Prob_A


#2. B=evento donna acui piace fare shopping = (evento donna) intersezione (evento soggetto a cui piace fare shopping)

Prob_B<- donne_SI/n_soggetti
Prob_NON_B<-1-Prob_B

#3.C=evento donna

# P(A U C)=P(A)+P(C)-P(B)

Prob_AUC<-Prob_A+1/2-Prob_B

#4. Siano C=evento donna e D=evento uomo , poiche sono eventi dsgiunti in quanto uno complementare dell'altro 
# la probabilita dell'evento e uomo o donna e dato dalla somma delle probibilita dei due eventi 

Prob_CUD<-1/2+1/2
###############################################################################

#ese41

#siano A=evento auto Americana 
#B=evento riparazione in garanzia

prob_A<-0.6
prob_B<-0.04
prob_B???A<-0.025

#la prima e data dal testo, ovvero prob_B
# la due e data dall'intersezione degli eventi A e B, gia fornita dal testo
#la 3 indica la probabilita dell'unione tra A e B

prob_BUA<-prob_B+prob_A-prob_B???A

#le 4 indica la probabilita dell'unione tra B e non A

prob_non_A<-1-prob_A

prob_BUnonA<-prob_B+prob_non_A-prob_B???nonA


# sappiamo che la probabilita totale di B e 0.04 , dunque per trovare la proabilita dell'intersezione tra non A e B devo fare 

prob_B???nonA<-prob_B-prob_B???A
###############################################################################
#variabili aleatorie
###############################################################################

#ese46


#poiche il valore che assume x e di natura dicotomica o binaria , ciÚ lascia presagire che 
#si tratti di una distribuzine Bernoulliana

# X~Bernoulli(p=0.90)

# la distribuzione bernoulliana e una variabile aleatoria discreta 

# la funzione di probabilit‡ Ë:

# P(X=x)= 0.9^x(1-0.9)^1-x

# la media Ë pari a 0.9
#la varianza e p(1-p)=0.9(1-0.9)

###############################################################################

#ese47

#osserviamo che per 1 singolo dado la probabilita che esca un 6 e di 1/6
#poiche tale evento e osservato su 4 dadi sicuramente la distribuzione sara di tipo binomiale

n<-4
p<-1/6

#X=evento 'esce un sei nel lancio di 4 dadi'

#probabilita che esca almeno 1 sei

Prob1<-1-pbinom(q=0,size=n,prob=p)

#probabilita che esca esattamente 1 sei

Prob2<-dbinom(x=1,size=n,prob=p)

#probabilita che esca 6 almeno 2 volte

Prob3<-1-pbinom(q=1,size=n,prob=1/6)

#valore atteso e varianza

val_att<-n*p
varianza<-n*p*(1-p)

x<-0:n
prob<-dbinom(x=x,size=n,prob=1/6)

plot(x=x,y=prob,ylab='funzione di probabilita',type='h', main='distribuzione binomiale')

abline(v=val_att,col=2,lty=2,lwd=2)
###############################################################################

#ese48

l<-1.7

#X=evento numero di telefonate


#1.probabilita che tra le 11 e le 13 non giungano telefonate

Prob<-dpois(x=0,lambda=2*l)

#2.probabilit‡ che tra le 16 e le 17 30 giungano al piu 2 telefonate 

Prob2<-ppois(2,lambda=l*1,5)

#3.calcolo numero medio di telefonate che giungono durante l'intera giornata di domenica

media<-l*24
###############################################################################

#ese49

intervallo<-c(0,1)

#X=evento numero compreso tra 0 e 1

#1. calcolare la probabilita che 0<=X<=0.4

Prob1<-punif(q=0.4,min=0,max=1)

#2.calcolare la probabilit‡ che 0.4<=X<=1

Prob2<-punif(q=1,min=0,max=1)-Prob1

#3.calcolare la probabilit‡ che 0.3<=X<=0.5

prob3<-punif(q=0.5,min=0,max=1)-punif(q=0.3,min=0,max=1)
###############################################################################
###############################################################################

#ESERCITAZIONE 6
###############################################################################

#ese1


X<-c(1.6,2,3.5,3,3.2,4)
Y<-c(10,15,20,21,24,30)

dati<-data.frame(X,Y)

#la retta di regressione sar‡ : Y = bo+b1x

b1<-cov(X,Y)/var(X)

b0<-mean(Y)-b1*mean(X)

#dunque la retta di regressione sar‡ data da : 

yhat<-bo+b1*X


det_index<-sum((yhat-mean(Y))^2)/sum((Y-mean(Y))^2)

plot(X,Y)

#sostituendo a X il valore 2.8 otteniamo :

yhat_new<-b0+b1*2.8

points(2.8 , yhat_new , col='red')


#il sistema di ipotesi e il seguente :

 #H0 : beta1 = 0
 #H1 : beta2 !=0

modello_stimato<-lm(formula= dati$Y ~ dati$X)

summ<-summary(modello_stimato)

p_value<-0.0056 # lo si ricava dal summary , in particolare in Coefficent  sotto la probabilit‡ 

#poiche p_value<alpha allora l'ipotesi che beta1=0 e falsa

###############################################################################
###############################################################################
#ESERCIZI REGRESSIONE

#ESE1

temperatura<-c(20,24,28,30,32,36)
tmp_sopravvivenza<-c(10,12,18,24,22,20)

plot(temperatura , tmp_sopravvivenza)

b1<-cov(temperatura, tmp_sopravvivenza)/var(temperatura)
bo<-mean(tmp_sopravvivenza)-(b1*mean(temperatura))

yhat<-bo+b1*temperatura

#coefficente di correlazione

mediax<-mean(temperatura)
mediay<-mean(tmp_sopravvivenza)

corr<-sum((yhat-mediay)^2)/sum((tmp_sopravvivenza-mediay)^2)

new_temperatura <- (15-bo)/b1
############################################################################

#ESE2

anni_studio<-c(3,4,4,2,5,3,4,5,3,2)
punteggio<-c(57,78,72,58,89,63,73,84,75,48)

mediax<-mean(punteggio)
mediay<-mean(anni_studio)

b1<-cov(punteggio,anni_studio)/var(punteggio)
bo<-mediay-b1*mediax

yhat<-bo+b1*punteggio

plot(punteggio , anni_studio)

corr<-sum((yhat-mediay)^2)/sum((anni_studio-mediay)^2)
##############################################################################
#ESE3

dose<-c(8,13,16,21,23)
diminuzione<-c(9,17,19,24,24)

mediax<-mean(dose)
mediay<-mean(diminuzione)

b1<-cov(dose,diminuzione)/var(dose)
bo<-mediay-b1*mediax

yhat<-bo+b1*dose

plot(dose,diminuzione)

corr<-sum((yhat-mediay)^2)/sum((diminuzione-mediay)^2)

##############################################################################

#ESE4

velocit‡<-c(0,1,2,3,4,5,6,7,8)
ossigeno<-c(19,20,20.5,21.5,22,23,23,23.5,24)

scatterplot<-plot(velocit‡,ossigeno)
s
mediax<-mean(velocit‡)
mediay<-mean(ossigeno)

b1<-cov(velocit‡,ossigeno)/var(velocit‡)
bo<-mediay-b1*mediax

yhat<-bo+b1*velocit‡

corr<-sum((yhat-mediay)^2)/sum((ossigeno-mediay)^2)
###############################################################################
###############################################################################
#ESERCIZI VERIFICA DELLE IPOTESI

#ESE1

val<-c(1.1,3.1,4.2,4.6,5,5.2,5.3,6.5,8.4,9.6)

#sistema di ipotesi , livello di significativit‡=0.01

# H0 : mi=4
# H1 : mi!=4

n<-length(val)
media<-mean(val)
mi0<-4
dev_std<-sd(val)
alpha<-0.01

z_alpha<-qnorm(1-alpha)

Z<-(media-mi0)/(dev_std/sqrt(n))

Z<z

#dall'ultima relazione possiamo evincere che l'ipotesi H0 deve essere accettata
###############################################################################

#ESE2


val<-c(10,12,15,16,16,17,20,22)

#sistema di ipotesi  significativit‡=0.01

#H0 : mi=15
#H1 : mi!=15


media<-mean(val)
mi0<-15
dev_std<-sd(val)
n<-lenght(val)
alpha<-0.01
z<-qnorm(1-alpha)

Z<-(media-mi0)/(dev_std/sqrt(n))

Z<z 

# l'ipotesi H0 deve essere accettata

##############################################################################

#ESE3

n<-200
media<-24
mi0<-25
var<-40
alpha<-0.05
dev_std<-sqrt(var)

#sistema di ipotesi

# H0: mi=25
# H1: mi!=25

z<-qnorm(1-alpha/2)

Z<-(media-mi0)/(dev_std/sqrt(n))

Z<z

#l'ipotesi nulla non viene rifiutata (? da rivedere perche errato rispetto al risultato sotto)

#calcolo il p-value , osservo che il test essendo bilaterale ha due code

p_value<-2*pnorm(-abs(Z))

p_value<alpha

#l'ipotesi nulle viene rifiutata

#altro approccio :

zhat<-qnorm(1-alpha/2)
val_critico<- media-zhat*(dev_std/sqrt(n))

media<val_critico

#poiche la media si trova oltre il valore critico(valore che non deve essere superato)l'ipotesi H0 viene rifiutata
###############################################################################

#ESE4

n<-1000
media<-11.2
var<-8
dev_std<-sqrt(8)
mi0<-10
alpha<-0.1

#sistema di ipotesi
# H0: media=10
# H1: media>10

# approccio 1:

Z<-(media-mi0)/(dev_std/sqrt(n))

z<-qnorm(1-alpha)

Z<z

#approccio p-value

p_value<-1-pnorm(Z)

p_value<alpha

#approccio 3:

zhat<-qnorm(1-alpha/2)
val_critico<-media-zhat*(dev_std/sqrt(n))

media>val_critico

#tutti gli approcci portano al rifiuto di H0
###############################################################################
##############################################################################

#ESERCIZI DI PROBABILITA'

#ese1

n<-200
ragazze<-120
ragazzi<-200-ragazze

ragioneriaM<-60
NONragioneriaM<-ragazzi-ragioneriaM

totNONRagioneria<-90
NONragioneriaF<-totNONRagioneria-NONragioneriaM
ragioneriaF<-ragazze-NONragioneriaF
totRagioneria<-ragioneriaM+ragioneriaF

#A=evento ragazza P(A)=ragazze/n=120/200=12/20=6/10=3/5=0.6 -> P(A)=60%
#B=evento ragazzo P(B)=1-P(A)=1-60%=40%
#C=evento persona che non ha fatto Ragioneria P(C)=totNONRagioneria/n=90/200=9/20=0.45=45%
#D=evento ragazza cha ha fatto Ragioneria P(D)=ragioneriaF/ragazze=50/200=5/20=1/4=0.25=25%
#F=evento ragazza o persona che ha fatto ragioneria =P(A U G) ove G=evento persona che ha fatto Ragioneria

#P(F)=P(A U G)=P(A)+P(G)-P(A int G)
# P(G)=1-P(C)=1-45%=55%
#P(A int G) = P(G|A)*P(A)

#poiche abbiamo le cardinalit‡ degli insiemi che ci interessano possiamo operare in questo modo

#P(F)=(ragazze+NONragioneriaF)/n=190/200=19/20=90%
################################################################################

#ese2

#A=evento vincita squadra in casa  P(A)=50%
#B=evento vincita squadra ospite   P(B)=20%
#C=evento pareggio  P(C)=1-0.5-0.2=0.3=30%
###############################################################################

#ese3

n<-52
estratte<-2

#A=evento estrazione di due assi

#P(A)=favorevoli/totali
# casi totali = numero totale di coppie che si possono estrarre 
# casi favorevoli = numero di coppie di assi che si possono estrarre

#casi totali = 52!/2!*50!= 52*51*50!/2!*50!=52*51/2=1326
#casi favorevoli = 4!/2!*2!=4*3/2=6

#P(A)=6/1326=0.004=0.4%






###############################################################################
###############################################################################
#SVOLGIMENTO COMPITI D'ESAME


#simulazione


###############################################################################################
# Es. 1 ‚Äî [1 punto] Il principio delle probabilit√† totali per due eventi incompatibili dice che la
# probabilit√† dell‚Äôunione dei due eventi √®:
# (a)uguale al prodotto delle probabilit√† associate ai singoli eventi
# (b)sempre nulla
# --> (c)uguale alla somma delle probabilit√† dei singoli eventi
# 
###############################################################################################
# Es. 2 ‚Äî [1 punto] La distribuzione cumulata di un carattere qualitativo non ordinabile o
# sconnesso:
# (a)√® sempre definita
# --> (b)non ha senso
# (c)pu√≤ essere definita solo per le frequenze assolute e non per quelle relative
# 
###############################################################################################
# Es. 3 ‚Äî [1 punto] Se ad ogni valore di una serie di media m aggiungiamo una stessa costante
# c arbitraria:
# --> (a)la media diventa m + c e la varianza rimane uguale
# (b)la media diventa m + c e la varianza viene moltiplicata per c
# (c)la media diventa m + c e la varianza viene moltiplicata per sqrt(c)
# 
###############################################################################################
# Es. 4 ‚Äî [2 punti] Quali definizioni sono corrette?
# --> (a)La popolazione statistica √® un insieme di unit√† statistiche tra loro omogenee
# (b)La popolazione statistica √® costituita dai ricercatori incaricati dallo Stato per condurre le
# indagini
# (c)Unit√† statistica e popolazione statistica sono sinonimi, indicano lo stesso concetto
# --> (d)L‚Äôunit√† statistica √® il pi√π piccolo elemento sul quale si effettua un‚Äôosservazione
# 
###############################################################################################
# Es. 5 ‚Äî [1 punto] Quale tra i seguenti caratteri oggetto di indagine statistica √® qualitativo?
# --> (a)Le malattie pi√π diffuse tra i dipendenti pubblici
# (b)Le assenze per malattia dei dipendenti pubblici
# (c)Gli stipendi dei dipendenti pubblici
# 
###############################################################################################
# Es. 6 ‚Äî [2 punti] Nel seguente istogramma sono state rappresentate le percentuali di ragazzi
# in base al voto ricevuto in matematica. Quali affermazioni sono vere?
# (a)Il 30% dei ragazzi ha preso 7
# --> (b)La percentuale di ragazzi che ha preso insufficiente √® 17%
# (c)I ragazzi che hanno preso 10 sono stati 3
# --> (d)Nessuno ha preso meno di 3
#   
###############################################################################################
# Es. 7 ‚Äî [7 punti] ‚Äî In un campione casuale di 12 utenti del servizio di Internet mobile si sono
# rilevate l‚Äôattivit√† svolta durante l‚Äôultima connessione e la relativa durata (in minuti) ottenendo:
Attivita <- c("Game", "Social", "Game", "News", "Game", "News", "Game",
              "Altro", "News", "Social", "Altro", "Social")
Durata <- c(4, 10, 5, 2, 4, 3, 6, 3, 5, 4, 3, 17)

#### 1. Si enunci la disuguaglianza di Chebychev.
# Data una qualsiasi distribuzione statistica con media mu e deviazione standard sigma, sia
# I(delta) l‚Äôinsieme dei termini della distribuzione stessa definito come
# I(delta) = {x: mu - delta < X < mu + delta}
# in cui delta √® una quantit√† arbitraria positiva. Allora, la frequenza relativa complessiva dei
# termini che si trovano nell‚Äôinsieme soddisfa la disuguaglianza
# Fr [I(delta)] >= 1 - sigma^2/delta^2
# 
# I valori di una variabile aleatoria X si dispongano intorno alla media mu e vorremmo 
# stimare la probabilit√† che X ha di differire dalla media per pi√π di k volte sigma.

#### 2. Si rappresenti graficamente la variabile ‚ÄúTipo di attivit√†‚Äù e se ne fornisca un opportuno
# indice di sintesi.

myPalette <- c(rgb(0,30,98,max = 255), rgb(0,128,128, max = 255), 
               'lightblue', "lightsteelblue")

n = 12
freq_ass <- table(Attivita)
freq_rel <- freq_ass/n

barplot(freq_ass, col = myPalette, main = 'Diagramma a barre del tipo di attivit√† svolta su internet')
pie(freq_ass, main="Grafico a torta del tipo di attivit√† svolta su internet", col = myPalette)

lbls = names(freq_ass)
pct <- round(freq_rel*100) 
lbls <- paste(lbls, pct) 
lbls <- paste(lbls,"%",sep="") 
pie(freq_ass, labels = lbls, main="Grafico a torta del tipo di attivit√† svolta su internet", 
    col = myPalette)

# Il miglior indice di sintesi in questo caso √® la moda, ovvero la modalit√† con maggiore frequenza.
# La moda del tipo di attivit√† √® Game, quindi la maggior parte dei ragazzi utilizza internet per
# giocare

#### 3. Supponendo che, nella popolazione, la durata in minuti delle connessioni (X) sia distribuita
# normalmente con deviazione standard pari a 1.5, si determini l‚Äôintervallo di confidenza,
# a livello del 90%, per la durata media delle connessioni.

sigma = 1.5
alpha = 1 - 0.9
n = length(Durata)
DurataMedia = mean(Durata)
z = qnorm(1-alpha/2)

l1 <- DurataMedia - z * sigma/sqrt(n)
l2 <- DurataMedia + z * sigma/sqrt(n)

l1;l2

# Essendo valida l'assunzione di Normalit√†, possiamo costruire l'intervallo di confidenza per 
# la media mu, con varianza nota, dunque utilizzando la statistica test Z ~ N(0,1)
# L'intervallo di confidenza ottenuto √®
# IC_{0.90}(mu) = [4.79; 6.21]

#### 4. Se, a parit√† del livello di confidenza, si richiedesse per mu un intervallo ampio al massimo
# 1 minuto, quale dovrebbe essere la numerosit√† campionaria minima?

# Per avere un Intervallo di confidenza con ampiezza <=1, √® necessario che il margine di errore
# sia <= 0.5, dunque
# 2*d <= 1 --> d <= 0.5 --> d = z_{1-alpha/2} * sigma/sqrt(n)
d = 0.5
(sigma * qnorm(1 - alpha/2) /d)^2

# quindi, per ottenere un IC_{0.90}(mu) con ampiezza inferiore al minuto, 
# sar√† necessaria una numerosit√† campionaria maggiore o uguale di 25.

###############################################################################################
# Es. 8 ‚Äî [6 punti] ‚Äî Un gestore di telefonia mobile vuole sapere se il numero di indirizzi IP
# di cui dispone √® sufficiente a soddisfare la domanda al servizio di Internet mobile. A tale scopo
# registra il numero di connessioni in ogni fascia oraria a Roma e a Torino. I seguenti box-plot
# rappresentano, nelle due citt√† campione, il numero di connessioni (in migliaia) registrato nelle
# 24 fasce orarie della giornata-tipo.

#### 1. Quali sono gli indici di variabilit√† calcolabili a partire dalla rappresentazione grafica
# fornita? Si riportino i valori di tali indici per la citt√† di Roma.
# Gli unici indici di variabilit√† calcolabili dal grafico sono il range = max(x) - min(x), e
# il range interquartile = Q3 - Q1. Considerando i dati della citt√† di Roma abbiamo:
# Il range uguale a:
120 - 30
# L'intervallo interquartile uguale a:
80 - 50

#### 2. Qual √® la forma delle due distribuzioni illustrate graficamente? Esistono degli outlier?
# In entrambi, Roma e Torino, i casi il traffico in internet presenta una distribuzione lievemente 
# asimmetrica a destra.
# Per verificarlo possiamo confrontare le distanze tra i quartili:
# (Q3-Q2) > (Q2-Q1)
80-50 > 50-30
80-60 > 60-50
# Verifichiamo la presenza di outlier (superiori) solo per la citt√† di Roma visto che 
# presenta un baffo pi√π lungo: 
# Range interquartile
IQR=80-50
80 + 1.5*IQR
# Confermiamo l'assenza di outlier superiori per la citt√† di Roma dato che il valore massimo
# osservato √® 120

#### 3. Poich√© ogni connessione richiede un indirizzo, per quante fasce orarie della giornata-tipo,
# a Roma, sono utilizzati come minimo 50 000 indirizzi? E a Torino?

0.75*24
0.50*24
# Roma per 18 fasce orarie (75%) in quanto 50000 corrisponde al primo quartile
# Torino per 12 fasce orarie (50%) in quanto 50000 corrisponde alla mediana

###############################################################################################
# Es. 9 ‚Äî [7 punti] ‚Äî Sulla base di 250 rilevazioni settimanali si √® stimato il seguente modello
#   lineare, per spiegare il rendimento delle azioni della societ√† di telefonia mobile TLCSPA (Y) in
#   funzione del rendimento del mercato di riferimento, rappresentato dall‚Äôindice Standard&Poor‚Äôs
#   500 (X).
#   ^yi = 0.0117 + 2.5074 xi        i = 1; 2; ... ; 250
#### 1. Qual √® l‚Äôinterpretazione della stima del coefficiente angolare del modello?

# Il coefficiente angolare indica che un aumento nel rendimento del mercato di un‚Äôunit√† genera 
# un aumento di 2.5074 nel rendimento delle azioni della societ√† TLCSPA. 

#### 2. Sapendo che, nel periodo considerato, valgono, rispettivamente,
# sum(y - ^y)2 = 0.7221
# sum(y - ybar)2 = 5.4033
# si valuti la bont√† del modello in esame e si traggano le opportune conclusioni.

# L'indice di bont√† di adattamento del modello √® l'indice di determinazione 
# R2 = SSR/SST = 1 - SSE/SST = (SST - SSE)/SST ovvero
# R2 = sum(^y - ybar)^2 / sum(y - ybar)^2 = 1 - sum(y - ^y)2 / sum(y - ybar)2

1 - 0.7221/5.4033

# il coefficiente di determinazione, pari a 0.87 √® abbastanza elevato e 
# indica una buona bont√† della retta di regressione in termini di percentuale di 
# variabilit√† di Y spiegata dal modello

#### 3. Si preveda inoltre il rendimento medio del titolo TLCSPA nelle settimane in cui il
# rendimento dell‚Äôindice S&P500 √® pari a 0.025.

0.0117 + 2.5074*0.025 

# La previsione del rendimento medio √® pari a 0.07

#### 4. Supponiamo di disporre di n coppie di osservazioni (x1; y1), ...,(xn; yn). Quale finalit√† ha
# e in che cosa consiste il metodo dei minimi quadrati?

# Il metodo dei minimi quadrati permette di determinare gli stimatori b0 e b1 dei coefficienti 
# dell‚Äôequazione lineare ^y = b0 + b1 x minimizzando la somma dei quadrati degli errori 
# ei, ovvero la quantit√† SSE = sum(ei)^2 = sum(y - ^y)^2.
# In tal modo si identifica la migliore interpolante lineare per un insieme di n coppie di osservazioni.

###############################################################################################
# Es. 10 ‚Äî [3 punti] Il gestore di una catena di sale giochi ha recentemente attivato, in uno
# dei suoi locali, il servizio Wi-Fi gratuito per i clienti. Prima di estenderlo agli altri locali della
# catena vuole verificare se, a fronte delle notevoli spese da sostenere, il nuovo servizio favorisca
# l‚Äôincremento degli incassi giornalieri. Nella tabella seguente sono indicati gli incassi (in centinaia
# di Euro) di una settimana-tipo a tre mesi dall‚Äôattivazione. La media degli incassi, registrati in
# una settimana-tipo precedente all‚Äôattivazione era 13.1429.
# 
incasso_dopo <- c(10, 12, 16, 15, 16, 23, 21)
# 
#### 1. Si definiscano le ipotesi da sottoporre a verifica e si motivi la scelta.
# Le ipotesi sono specificate come segue:
#   H0: mu <= mu0
#   H1: mu > mu0 
# considerando mu0 = 13.1429, con l'obiettivo di verificare se l‚Äôincasso dopo 
# l‚Äôintroduzione del servizio Wi-Fi gratuito sia aumentato

#### 2. Dopo aver specificato le necessarie assunzioni, sulla base di un test con livello di significativit√†
# del 5%, si pu√≤ concludere che il gestore estender√† il servizio? Motivare la
# risposta.

# Si tratta di un test (unilaterale) sulla media di una popolazione che dobbiamo 
# ipotizzare Normale, con varianza incognita

alpha = 0.05
mu0 = 13.1429
t.test(incasso_dopo, alternative = 'greater', mu = mu0)

# oppure
xbar = mean(incasso_dopo)
s = sd(incasso_dopo)
n = length(incasso_dopo)

# stima della statistica test:
t_oss <- (xbar - mu0)/(s/sqrt(n))
t_oss

t_teo <- qt(1-alpha, n-1)
t_teo

# Per decidere se rifiutare o no l'ipotesi nulla:
t_oss > t_teo
p_value = alpha_oss = 0.06753
alpha_oss < alpha

# In entrambi i casi non rifiutiamo l'ipotesi nulla, dunque non abbiamo sufficiente 
# evidenza empirica per affermare che possa essere vantaggioso estendere il 
# wi-fi gratuito agli altri locali della catena

# Nel primo caso il p-value = 0.06753 > alpha = 0.05, nel secondo caso t_oss < t_teo, 
# dunque il valore della statistica test osservato non cade nella zona di rifiuto.
#############################################################################################

#compito 21/6/2021

#ese1

#1.
  #il tipo di variabile genere dell'intervistato ha carattere qualitativo nominale , mentre
  #il livello di soddisfazione ha carattere qualitativo ordinale 

#2 Calcolare i cinque numeri di riepilogo del costo del servizio e fornire una rappresentazione
#  grafica dei dati. Descrivere i risultati.


servizio <- c("Test", "Visita", "Test", "Test", "Visita", "Visita",
              "Test", "Visita")
genere <- c("M", "M", "M", "F", "M", "F", "M", "F")
soddisfazione <- c("Alto", "Medio", "Basso", "Medio", "Alto", "Medio",
                   "Basso", "Alto")
costo <- c(20, 150, 40, 70, 260, 45, 30, 185)
dati_sanitari <- data.frame(servizio, genere, soddisfazione, costo)

num_riep<-summary(costo)

boxplot<-boxplot(costo)

q3<-quantile(costo, 0.75)
q1<-quantile(costo, 0.25)

media<-mean(costo)
median<-median(costo)

media>median

q3-media>media-q1

#la distribuzione dei dati e asimmetrica negativa 

#3.Verificare se la distribuzione del costo del servizio contiene outliers.

IQR<-q3-q1

q3+(1.5*IQR) > max(costo)
q1-(1.5*IQR) <min(costo)

#presenta outliers


#4. Da un box-plot Ë possibile concludere qualcosa sulla presenza di mode nella distribuzione?
#   Motivare la risposta.

#dal boxplot e possibile trarre conclusioni sui quantili e sulla media , se volessi 
#concludere qualcosa sulla moda allora il grafico piu adatto sarebbe un istogramma , cosi da poter vedere 
#qual'Ë il valore/classe con frequenza maggiore

#5.Indicando con TD le spese sostenute da chi si Ë sottoposto ad un test diagnostico e con
#VS le spese sostenutie da chi ha usufruito della visita specialistica, 
#quale delle due sotto distribuzioni dei costi presenta maggiore variabilit‡? 
#Giustifica la tua risposta calcolando il pi˘ appropriato indice di dispersione

TD<-costo[servizio=='Test']
VS<-costo[servizio=='Visita']

sd_TD<-sd(TD)
sd_VS<-sd(VS)

sd_TD/mean(TD)
sd_VS/mean(VS)

# la variabilita dei costi delle visite specialistiche e maggiore rispetto a qualla dei test


#ese2

costo_medio<-100
dev_std<-87
n<-length(costo)


#1.Fornire le assunzioni necessarie per verificare l'affermazione di cui sopra e specificare il
#sistema di ipotesi.

#assumo la normalita dei costi , dunque utilizzo la statistica test Z ~ N(0,1)
#il sistema di ipotesi e il seguente :

# ipotesi nulla      H0 : media = costo_medio
#ipotesi alternativa H1 : media < costo_medio

#2.Eseguire il test sopra specificato ad un livello di significativit‡ del 5%, 
#spiegando chiaramente i passaggi per raggiungere la decisione finale attraverso l'uso del p-value.

#1-alpha=5% 

alpha<- 1-0.05

z_oss<-(mean(costo)-costo_medio)/(dev_std/sqrt(n))

#il p-value per definizione e P(Z>z | mu=mu0)

p_value<-pnorm(z_oss)

p_value<alpha

#poiche il p_value e minore rispetto ad alpha , cio significa che mi trovo nella regione di rifiuto
#dunque rifiuto l'ipotesi nulla , pertanto i pazienti intervistati utilizzano maggiormente i servizi 
#meno costosi

#3.La decisione precedente cambierebbe se il livello di significativit‡ fosse ridotto all'1%?
#  PerchÈ?

new_alpha<-1-0.01

p_value<new_alpha

#la decisione non cambierebbe in quanto la relazione p_value<alpha rimane valida

#4.Per completare l'analisi, i responsabili delle strutture hanno bisogno di stimare l'intervallo
#della proporzione dei test diagnostici, con ?? = 0.1. In che modo si potrebbe ridurre
#l'ampiezza dell'intervallo d'interesse?


new_alpha2<-0.1

#utilizzo la statistica test che fa uso della proporzione

phat<-sum(costo[servizio=='Test'])/n

z<-qnorm(1-new_alpha2/2)

I1<- phat - z*sqrt(phat*(1-phat)/n)
I2<- phat +(z*sqrt(phat*(1-phat)/n))

#ese3

prob = 35/100

#1.Calcolare la probabilit‡ che su 10 individui che 
#  entrano nei negozi di propriet‡ dell'azienda, esattamente 2 effettuino un acquisto


#visto che i clienti possono o meno acquistare , definisco X=evento cliente entra in negozio e acquista
# X ~ Binomiale(prob , n)
 
# P(X=2)=1-P(x<2)=1-P(x=0)+P(X=1)

prob_X2<-dbinom(2,10,prob = prob)


#2.Supponendo che l'importo speso in un singolo acquisto da un cliente sia distribuito secondo 
#una distribuzione normale con media pari a 80 euro e deviazione standard pari a 15
#euro, qual Ë la probabilit‡ che un cliente effettuer‡ un acquisto per un importo superiore
#a 70 euro?
  

#sia A=evento importo speso in un solo acquisto da un cliente
# A ~ N(mi=80 , sigma=15)

#P(A>70)=1-P(A<=70)=1-P(A=0)+......+P(A=70)

prob_A<-1-pnorm(70,80,15)

#la probabilit‡ e del 74.7%

#ese4

#E[T] = E[1/12 X1 + 1/4 X2 + 1/3 X3] = 1/12 E[X1] + 1/4 E[X2] + 1/3 E[X3] =
# = (1/12+1/4+1/3) mi = 0.66 mi -> E[T]!=mi -> lo stimatore e distorto


############################################################################

#esercizi esame 14/09

#n1

# X = mesi di esperienza nello stesso ambito lavorativo
# Y = profitto dell'azienda

n = 20
set.seed(3)
x <- round(rnorm(n, 30, 15))



# Rappresentare il boxplot della varibile X
# Calcolare gli opportuni indici riassuntivi
# Cosa si puÚ dire sulla forma della distribuzione?

boxplot(x,horizontal = T)
summary(x)


q3<-33.25
me<-28
q1<-18.25

q3-me<me-q1

#la distribuzione risulta essere asimmetrica negativa

media<-mean(x)
mediana<-median(x)
dev_std<-sd(x)
alpha<-(media-mediana)/dev_std

hist(x)

estremi_classi<-c(10,20,30,40,50)
freq_ass<-table(cut(x,breaks=estremi_classi))

hist(freq_ass)

#n2

y <- x - runif(n, -15, 15)

plot(x,y)

#dallo scatterplot e possibile evidenziare la sparsit‡ della distribuzione , probabilmente
#i dati sono discordanti

meanx<-mean(x)
meany<-mean(y)

b1<-cov(x,y)/var(x)
b0<-meany-b1*meanx

yhat<-b0+b1*x

points(x,yhat,col='red')


#n3

#A, B e C sono tre eventi tali che B e C sono s-indipendenti, mentre A e B
#sono incompatibili. Anche A e C sono incompatibili. Se P(A) = 0.1, P(B) = 0.6, P(C) = 0.6

#n4

set.seed(5)
x <- c(rnorm(48, 70, 10), 300, 450)
#1. Indicare il tipo di variabile del livello di soddisfazione e del genere dell'intervistato

#il carattere e di tipo quantitativo continuo

#2. Calcolare i cinque numeri di riepilogo del costo del servizio e fornire una rappresentazione
#grafica dei dati. Descrivere i risultati.

summary(x)

hist(x)

#l'istogramma evidenzia due buche al suo interno , cio implica che sarebb consigliabile 
#procedere con una rappresentazione in classi
#la media > mediana cio implica che con una certa probabilita la distribuzione,
#sia asimmetrica positiva 

q1<-quantile(x,0.25)
q2<-median(x)
q3<-quantile(x,0.75)

q3-q2>q2-q1

#effettivamente la distribuzione risulta essere asimmetrica positiva
media<-mean(x)
mediana<-median(x)
dev_std<-sd(x)
alpha<-(media-mediana)/dev_std


#3. Verificare se la distribuzione del costo del servizio contiene outliers.

IQR<-q3-q1
massimo<-max(x)
minimo<-min(x)

q3+(1.5*IQR)>massimo
q1-(1.5*IQR)<minimo


#4. Da un box-plot Ë possibile concludere qualcosa sulla presenza di mode nella distribuzione?

boxplot(x ,horizontal = T)

#no in quanto il boxplot evdenza i 5 numeri di riepilogo , per evidenziare la moda sarebbe necessario
#analizzare l'istogramma , poiche come sappiamo la moda e l'indice di posizione a cui e associata la maggiore frequenza 
#assoluta o relativa , nel caso di classi parleremo di classe modale 

#  Motivare la risposta.

#n5

#I dipendenti dell'azienda asseriscono che il costo medio di un servizio
#sia di almeno 90 euro. Vogliono saggiare 
#l'ipotesi che i clienti intervistati utilizzino maggiormente i servizi
#meno costosi sapendo che la variabilit‡ dei costi dei servizi Ë pari a 63 euro.

# H0 : mu0 = 90
# H1 : mu0<90

#siamo in presenza di un sistema di ipotesi unilaterale 

#approccio del p_value : alpha_oss= P(Z>z_xhat | mu0=mu)

xbar<-mean(x)
mu0<-90
dev_std<-63
n<-length(x)

z<-(xbar-mu0)/dev_std/sqrt(n)

p_value<-pnorm(-abs(z))

alpha<-0.1

p_value<alpha

#non abbiamo abbastanza evidenza empirica per rifiutare l'ipotesi nulla

#approccio Z_oss

z_teo<-qnorm(1-alpha)

z>z_teo

#accettiamo H0

#approccio valore critico :

media<-mean(x)
val_critico<-media-(qnorm(1-alpha/2)*(dev_std/sqrt(n)))

media<val_critico

###############################################################################
##############################################################################

#ESERCITAZIONI

###############################################################################
#esercitazione 1

#n1
n_acquisti<-c(1,2,3,5,6)
n_iscritti<-c(11,28,14,13,8)

distr_freq<-data.frame(n_acquisti,n_iscritti)

#la variabile n_acquisti e quantitativa discreta

#numeri di riepilogo:

num_riep<-summary(n_acquisti)

#completo la distribuzione di frequenza:

freq_rel<-n_iscritti/sum(n_iscritti)
freq_cum<-cumsum(freq_rel)

distr_freq$freq_rel<-freq_rel
distr_freq$freq_cum<-freq_cum

#funzione di ripartizione empirica:

plot(stepfun(n_acquisti,c(0,freq_cum)))



#n2

#per distribuzione di frequenza si intende una tabella che riassume le frequenze
#in modo tale da poter osservare le informazioni relative alla distribuzione in esame

spese<-c(8.9,12.6,86.4,75.2,28.4,26.5,15.4,67.8,13.2,10,18.6)
tot_spese<-sum(spese)

estremi_classi<-c(5,10,20,70,150)

freq_ass<-table(cut(spese,breaks = estremi_classi))

distr_freq<-data.frame(freq_ass)

freq_rel<-distr_freq$Freq/sum(distr_freq$Freq)
freq_cum<-cumsum(freq_rel)
centrali<-c(7.5,15,45,110)
ampiezze<-c(5,10,50,80)

distr_freq$freq_rel<-freq_rel
distr_freq$freq_cum<-freq_cum
distr_freq$centrali<-centrali
distr_freq$ampiezze<-ampiezze

#poiche le classi non sono di pari ampiezza calcolo le densit‡:

densita<-freq_rel/ampiezze

distr_freq$densita<-densita

#calcolo della media

media<-sum(freq_rel*centrali)

#calcolo porzione di clienti che ha speso piu di 20 euro

#Fr[x>20]=f3+f4

porzione<-freq_rel[3]+freq_rel[4]


#n3

n<-200

classi<-c('[0,50)','[50,200)','[200,500]')
freq_ass<-c(81,73,46)

distr_freq<-data.frame(classi,freq_ass)

freq_rel<-freq_ass/200
freq_cum<-cumsum(freq_rel)
centrali<-c(25,125,350)
ampiezze<-c(50,150,300)
densit‡<-freq_rel/ampiezze

distr_freq$freq_rel<-freq_rel
distr_freq$freq_cum<-freq_cum
distr_freq$centrali<-centrali
distr_freq$ampiezze<-ampiezze
distr_freq$densit‡<-densit‡

# Fr[X>95]=f3+f2*(200-95)/ampiezza2

porzione<-freq_rel[3]+freq_rel[2]*((200-95)/ampiezze[2])

# la mediana e l'indice di posizione che lascia sia alla sua sinistra che alla sua destra il 50% delle
#osservazioni
# la classe mediana risulta essere [50,200) poiche la freq_cum associata e 0.770


#n4

preferenza<-c('variet‡ di pietanze','abbondanza dei piatti','qualit‡ dei panini',
              'pulizia del locale','prezzi moderati','vicinanza all uni')

n_studenti<-c(13,35,52,10,43,7)

distr_freq<-data.frame(preferenza,n_studenti)

#la preferenza Ë qualitatita nominale

barplot(n_studenti,space=1,names.arg=preferenza)

#n5

distanza<-c(650,500,700,850,200,400,350,150,200)

media_d<-mean(distanza)

prezzo<-c(2.8,3.5,3,2.4,3.2,4,4.5,4.2,3.6)

summary(prezzo)

#oppure 

q1<-quantile(prezzo,0.25)
q2<-quantile(prezzo,0.5)
q3<-quantile(prezzo,0.75)
##############################################################################

#esercitazione3


#n1

#A=evento tiratore centra il bersaglio con un colpo
#P(A)=0.23

#X=evento numero di tiri al bersaglio in 8 colpi sparati

#X distr Binom(p=0.23)

#P(X=0) : 

Prob_x0<-dbinom(x=0,size=8,p=0.23)

#P(X>=1)=1-P(x<1)=1-P(X=0)

Prob_Xmajor1<-1-Prob_x0

media<-0.23*8
varianza<-media*(1-0.23)


#n2

















