setwd('../../../Desktop/CODICI/R/eserciziario/n6/')

dati <- c("0", "A", "0", "AB", "A", "A", "0", "0", "B", "A", "0", "A",
          "AB", "B", "0", "0", "0", "A", "B", "A", "A", "0", "A", "A", 
          "0", "B", "A","0", "AB", "A", "0", "0", "A", "B", "A", "A", 
          "A", "0", "B", "0", "0", "A","0", "A", "B", "0", "AB", "A",
          "0", "B")

modalità <- c('O','A','B','AB')

n <- length(dati)

freq_ass <- table(dati)

freq_rel <- freq_ass/n

freq_cum <- cumsum(freq_rel)


distr_freq <- data.frame(modalità , freq_ass , freq_rel , freq_cum)


barplot(freq_ass , ylab='frequenze assolute' , main='distribuzione gruppo sanguigno')













