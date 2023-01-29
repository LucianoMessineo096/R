#esercitazione 3
########################################################################
#ese1

#indico con A=bersaglio centrato , nonA=bersaglio mancato
#p(A)=0.23 , p(nonA)=1-p(A)=1-0.23=0.77
#X=numero di tiri al bersaglio in 8 tiri sparati

# 1) qual'e la probabilita che in 8 colpi sparati nessuno centri il bersaglio?
#intanto possiamo dire che la X è distribuita secondo una Binomiale , in quanto abbiamo espresso le probabilita secondo 
#variabili dicotomiche , e sopratutto stiamo considerando un numero di campioni osservati , ovvero i colpi sparati, maggiore di 1
#Dunque -> X~Binomiale(p=0.23 , n=8)

#p(X=0)= factorial(8)/(factorial(0)*factorial(8-0)) * 0.23^0 * (1-0.23)^8

val1<-dbinom(x=0,size=8,prob=0.23)

#questa funzione ci fornisce il valore della funzione densita di probabilità , in corrispondenza dei parametri assegnati

# 2) qual'è la probabilita che in 8 colpi sparati almeno 1 centri il bersaglio?

#p(X>=1)=1-P(X=0)

val2<-1-val1

# 3)determinare la media e la varianza di X

# la media di una v.a distribuita secondo una binomiale e E(X)=n*p
# la varianza di una v.a distribuita secondo una binomiale e V(X)=n*p(1-p)

n=8
p=0.23

media<-n*p

varianza<-n*p*(1-p)

###############################################################################
#ese2

# 2) Una busta contiene semi di una pianta che produce fiori bianchi nel 40% dei casi o
#    rossi altrimenti. Qual è la probabilità che piantando 5 semi si abbiano:

# a- 3 piante a fiori rossi?

# A=semi pianta con fiori bianchi         p(A)=40%=0.4
# nonA=semi pianta con fiori rossi        p(nonA)=1-p(A)=0.6
# X=piantare semi fiori rossi

#p(X=3) = factorial(5)/(factorial(3)*factorial(2)) *0.6^3 *(1-0.6)^2


val3<-dbinom(x=3,size=5,prob=0.6)

# b- 3 o più piante a fiori bianchi?

#p(X<3)=P(X=0)+p(X=1)+p(X=2) , la seguente espressione lascia pensare che si tratti di una funzione di ripartizione
#in quanto stiamo calcolando i valori delle funzione massa di probabilità cumulati sino a 2.

val4<-pbinom(q=2,size=5,prob=0.6)



# c- Quanti semi bisogna piantare affinchè la probabilità di avere una o più piante a
#    fiori rossi sia maggiore dell'80%?


# per risolvere questo quesito dobbiamo ragionare per formule inverse , in quanto sappiamo :

#p(X>=1)>80%

#p(X>=1)=1-p(X<1)=1-p(X=0)=1 - [factorial(n)/factorial(0)*factorial(n)] * 0.6^0 * (1-0.6)^n >0.8

# dunque bisogna risolvere la disequazione 1-0.4^n > 0.8

val5<- log(x=0.2 , base=0.4)

# dunque sono necessari 2 semi affinchè si abbia tale probabilità

###############################################################################

#ese3

#Un'azienda ha due linee di montaggio e ognuna di queste si ferma mediamente 2.4
#volte la settimana. Assumendo che il funzionamento delle linee sia indipendente,
#calcolare la probabilità che almeno una delle due linee si fermi almeno una volta in una
#determinata settimana.


#dal testo possiamo evincere che la v.a sia di tipo discreto , in quanto stiamo considerando 
#la variabile X= numero di volte che la linea di produzione rimane ferma in un insieme di valori discreto  ,
#e visto che stiamo effettuando un conteggio , o comunque stiamo controllando che un certo evento si verifichi periodicamente 
#la distribuzione piu adatta è quella di Poisson
#dunque X~Poisson(??=2.4)


# visto che sto considerando le linee di produzione indipendenti tra loro , allora 
# N1=numero di volte che la linea di produzione 1 rimane ferma in una settimana
# N2=numero di volte che la linea di produzione 2 rimane ferma in una settimana


#calcolo dunque la probabilita per entrambe

#p(N1>=1)= p(N2>=1)= 1-p(N1=0)

val6<-1-dpois(x=0,lambda=2.4)

# la probabilita richiesta e data dall'unione dei due eventi 

#p(N1>=1 U N2>=1)= p(N1>=1)+p(N2>=1) - p(N1>=1 ?? N2>=1)
#visto che si tratta di eventi indipendenti tra loro la probabilità dell'intersezione può esprimersi come prodotto delle probabilità

prob1<-val6+val6-val6^2

###############################################################################
#ese4

# X una variabile casuale che segue una distribuzione uniforme sull'intervallo (0,10).
#Calcolare la probabilità che tale variabile casuale differisca dal suo valore medio per
#meno di 4???3/5 volte la sua deviazione standard. Quale sarebbe un estremo inferiore
#per tale probabilità se non si conoscesse la forma della distribuzione?

#per prima cosa non ci vengono forniti media e deviazione standard
a=0
b=10
val<-4sqrt(3/5)
media1<- (a+b)/2
varianza1<-(b-a)^2/12  
dev_std1<-sqrt(varianza1)

#dunque calcolo la probalilita richiesta p(abs(X-media)<(val*dev_std))

prob<-punif(q=9,min=a,max=b) - punif(q=1,min=a,max=b)

#per quanto riguarda l'estremo inferiore utilizzo la disuguaglianza di chebischev


###############################################################################
#ese5

#Supponiamo che la durata della vita di ogni membro di un gruppo segua una legge
#esponenziale di parametro ?? = 1/50 (in anni). Per un membro di tale popolazione si
#calcoli:


#a- la probabilità di arrivare alla pensione(65 anni)

#sia X=durata della vita 

#p(X>65)=1-p(X<=65)

prob1<-1-pexp(q=65,rate=1/50)


#b- la probabilità di arrivare a 70 anni se ha appena compiuto il 40 compleanno
#si tratta di una probabilita condizionata dunque p(X>70|x>40)=p(70-40)

prob2<-1-pexp(q=30,rate=1/50)


#c-per quale valore di x si ha che P(X > x) = 1/2 (dove X è la durata di vita
#  dell'individuo considerato)?


# per formule inverse

x=50*log(2)


###############################################################################
#ese6

#L'età dei clienti che acquistano uno specifico prodotto segue una distribuzione Normale
#con un'età media µ = 30 e con una varianza ??= 36.
#1. Calcola la percentuale di clienti nell'intervallo di età (20, 42)?

#p(20<X<42) -> opero con la distribuzione standardizzata , cosi da ottenere media=0 e varianza=1
#p((20-media)/sqrt(varianza) < Z < (42-media)/sqrt(varianza)) = ??(2)-??(-1.67)

z1<-(20-5)/6
z2<-(42-5)/6
prob<-pnorm(q=z2)-pnorm(q=z1)






#2. Quale età viene raggiunta solo dal 5% più anziano dei clienti?
  


















