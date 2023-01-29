
# Nel suo utilizzo più semplice, R può essere utilizzato come calcolatrice:
3+5*3.5

# Quando il comando non è terminato risulta un prompt +; a questo punto è 
# sufficiente continuare a scrivere sulla riga successiva
2^3 -
3

# "[1]" indica il numero della linea in cui compare il risultato dell'operazione. Ad
# esempio eseguendo dalla stessa riga due operazioni distinte, intervallate con ";"
# si ha
3+5*5-(2/6*4); 3+2

x <- 2+(3-4*5)/2 #costruisci un oggetto
x #..visualizzalo
x-2 #..utilizzalo
x=5 #..sovrascrivilo
2.5->x.x #crea un altro oggetto
x/x.x #..fai una qualsiasi operazione con gli oggetti

# per maggiore sicurezza si potrebbe interrogare R prima dell'assegnazione.
A
# così A è un nome da poter essere usato. è forse opportuno osservare che
x<-2
X
# cioè l'ambiente è case-sensitive, ovvero fa distinzione fra lettere minuscole e
# maiuscole, così x e X sono oggetti differenti.

# Esiste in R una lista di parole "riservate" che non possono essere utilizzate
# dall'utente a proprio piacimento, ma solo per le finalità per le quali sono state
# costruite; queste sono:
# FALSE, TRUE, Inf, NA, NaN, NULL, break, else, for, function, if,
# in, next, repeat, while

TRUE<-2
TRUe<-2
TRUe

# Le seguenti linee illustrano l'utilizzo dei 6 operatori: == (uguale); >=/<=
# (maggiore/minore uguale); >/< (maggiore/minore); != (diverso).

2*2==4
2*3==4
2*2>=4
2*3>4
2*3>=4
2*3!=4

# Un insieme di comandi puµo essere scritto per assolvere un determinato compito.
# Ad esempio potremmo essere interessati a calcolare la media aritmetica dei
# primi 5 numeri, e allora si potrµa scrivere
(1+2+3+4+5)/5
#Oppure utilizzando la nota formula n(n + 1)/2 potremmo procedere nel seguente modo:
n=5
n*(n+1)/(2*n)

# Quando si lavora in R può essere molto utile specificare la directory di lavoro, 
# ovvero la cartella dove salvare i file relativi a quella sessione.

getwd() #questa µe l'attuale dir di lavoro..
setwd("/Users/giuliamarcon/Desktop/") #cambiala..

# I numeri con cui siamo abituati a ragionare sono in realtà un caso particolare
# di una famiglia piµu grande, i vettori. Un vettore di dimensione n può essere
# definito come una sequenza ordinata di n numeri; ad esempio, (2; 5; 9.5;-3)
# rappresenta un vettore di dimensione 4 in cui il primo elemento è 2 ed il quarto
# è -3. Esistono molti modi per costruire un vettore, il più comune è utilizzare
# la funzione c():
x<-c(2,5,9.5,-3) #costruisci un vettore
x[2] #seleziona il suo secondo elemento
x[c(2,4)] #seleziona i suoi elementi nelle posizioni 2 e 4
x[-c(1,3)] #escludi quelli nelle posizioni 1 e 3
x[x>0] #seleziona i sui elementi positivi
x[!(x<=0)] #escludi i suoi elementi non strettamente positivi
x[x>0]-1 #sottrai uno scalare agli elementi positivi
x[x>0][2] #seleziona il secondo elemento tra quelli positivi

# Come è facile notare, le parentesi quadre dopo il vettore definiscono quali 
# componenti (posizioni) dell'oggetto selezionare: un indice ([2]) o un vettore di
# indici ([c(2,4)]) o una condizione ([x>0]), sono tutte scritture ammesse.

# E' importante notare che la riga x[x>0]-1 sottrae 1 da ogni elemento del vettore
# così come x[x>0]*2, diciamo, moltiplicherebbe ogni elemento per 2. Questa è
# ancora un'altra caratteristica del linguaggio: le operazioni con oggetti `multipli'
# (quali vettori e in seguito vedremo matrici) sono eseguite su ogni elemento dell'oggetto;
x[x>0]+c(2,5,3)
x[x>0]+c(2,5) # sbagliato

# Una utilissima funzione è la funzione length() che quando applicata a vettori, 
# ne restituisce la dimensione:
length(x)
length(x[x>0])

# R consente la costruzione di vettori non-numerici
v<-c("barletta",'bracciano',"palermo")
#si noti l'equivalenza fra i simboli " e '
length(v)

# Sebbene sia possibile formare vettori che comprendano sia caratteri sia numeri, 
# i numeri inseriti in vettori `misti' vengono comunque considerati come dei caratteri:
x<-c(2,"d",5)
x
# le virgolette indicano che effettivamente si tratta di caratteri;

#crea un dataframe con una variabile `quantitativa' ed una `qualitativa':
X<-data.frame(a=1:4, sesso=c("M","F","F","M"))
X
dim(X) #la `dimensione' (numero dei casi e di variabili)
X$eta<-c(2.5,3,5,6.2) #aggiungi una variabile di nome eta

# Il dataframe creato sopra µe definito da 4 casi e 2 variabili 
# che possono essere selezionate utilizzando sia il nome (ad es., X$sesso o X[,"sesso"]),
# sia il numero di colonna che occupa (ad es., X[,2]); 
# il risultato sarà comunque un vettore.

#seleziona i valori della variabile eta per i maschi:
X[X$sesso=="M","eta"]
#seleziona i valori della variabile sesso per cui eta<=3:
X$sesso[X$eta<=3]

# help
?mean

# capire la struttura dell'oggetto
str(X)
dim(X)

# info variabile
summary(X$sesso)
summary(X$eta)

# distribuzione di frequenza
# frequenze assolute
table(X$sesso)
table(X$eta)

# frequenze relative
table(X$sesso)/length(X$sesso)

# più comodo se attribuiamo n
n=dim(X)[1] # oppure
n=nrow(X)

table(X$eta)/n

# controllo se sommano 1
sum(table(X$sesso)/length(X$sesso))

# attribuisco la distribuzione di frequenza ad un oggetto
freq_ass <- table(X$eta)
sum(freq_ass)

# frequenze cumulate
cumsum(freq_ass)

# grafici
pie(table(X$sesso))
barplot(table(X$sesso))

hist(X$eta)
hist(X$eta, probability = T)

plot(ecdf(X$eta)) # funzione di ripartizione empirica
