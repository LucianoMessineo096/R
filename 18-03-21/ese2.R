##### esercizio sul data set questionario statistica 2020-2021####################

# importiamo i dati
#dati <- read_excel("Corso di Statistica 2020_2021 (Risposte).xlsx", sheet = "Risposte")

# numerosit� campionaria e numero di variabili
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
# [14] "A cena al ristorante in compagnia di un gruppo di amici, è il momento di pagare il conto. � chi ha mangiato con appetito: dall’antipasto al dolce, senza indugi. C’è chi non ha potuto rinunciare al buon vino, magari per sfoggiare le sue conoscenze enologiche. C’è chi è sempre a dieta e si è limitato ad un’insalata. Quale metodo di pagamento preferiresti?"
# [15] "Quanto ti incuriosisce questo corso?"                                                                                                                                                                                                                                                                                                                                 
# [16] "Qual è la tua altezza (esprimerla in cm) [CLEAN]"                                                                                                                                                                                                                                                                                                                     
# [17] "Che lavoro ti piacerebbe fare? [GROUP]"

#rinomino le colonne del dataset

colnames(dati) <-paste0('X', 1:p)


#####################ANALISI DATI COLONNA 4#######################
# a questo punto posso calcolare la distribuzione delle frequenze

freq_ass <- table(dati$X4)
sum(freq_ass)


freq_rel <- freq_ass/n

sum(freq_rel)

freq_cum <- cumsum(freq_rel)


#plot

pie(freq_ass)
barplot(freq_ass, ylab = "Frequenze assolute")

###############ANALISI DATI COLONNA 6










