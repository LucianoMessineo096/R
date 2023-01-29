setwd('../../../Desktop/CODICI/R/eserciziario/n7/')

concentrazioni <- c(6.2, 9.1, 2.4, 3.6, 1.9, 1.7, 4.5, 4.2, 3.3, 5.1, 6.0, 1.8,
                    2.3, 4.9, 3.7, 3.8, 5.5, 6.4, 8.6, 9.3, 7.7, 5.4, 7.2, 4.9, 6.2)

estremi_classi <- c(0,2,4,7,10)

freq_ass <- cut(concentrazioni , breaks=estremi_classi)

table(freq_ass)
