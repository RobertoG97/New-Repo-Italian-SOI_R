rm(list = ls()) 
dev.off()

######### Code SOI_R new data
df_new <- readxl::read_excel(file.choose())#SOI_R new data copia
df_cod <- df_new

df_cod <- data.frame(AGE=df_cod$`Età:`, EDU=df_cod$`Titolo di studio:`, GENDER=df_cod$`Sesso:`,
                     SEXUAL_ORIENTATION=df_cod$`Orientamento sessuale`, 
                     SOI_1=df_cod$`Con quanti partner diversi hai avuto rapporti sessuali negli ultimi 12 mesi?`,
                     SOI_2=df_cod$`Con quanti partner diversi hai avuto un rapporto sessuale in una sola e unica occasione?`,
                     SOI_3=df_cod$`Con quanti partner diversi hai avuto un rapporto sessuale senza avere interesse in una relazione seria e duratura con la persona?`,
                     SOI_4=df_cod$`Sesso senza amore va bene`,
                     SOI_5=df_cod$`Riesco ad immaginarmi a mio agio nel fare sesso occasionale con partner diversi`,
                     SOI_6=df_cod$`Non voglio fare sesso con una persona se non sono sicuro/a che avremo una relazione seria e a lunga durata`,
                     SOI_7= df_cod$`Quante volte ti capita di avere delle fantasie sessuali su una persona con cui non hai una relazione sentimentale seria?`,
                     SOI_8= df_cod$`Quante volte provi eccitazione sessuale quando sei a contatto con qualcuno con cui non hai una relazione sentimentale seria?`,
                     SOI_9= df_cod$`Nella vita quotidiana, quanto spesso hai fantasie sessuali spontanee su una persona che hai appena incontrato?`)
#View(df_cod)

library(admisc)

##### ITEM
df_cod[,5:7] <- lapply(df_cod[,5:7], 
                        FUN = function(X)recode(X, "0=1; 1=2; 2=3; 3=4;4=5; 5 - 6=6; 7 - 9=7;10 - 19 =8; 20 o più=9"))
df_cod[,8:10]# sono già codificati da 1 a 9


# Mai, Molto raramente, Circa una volta ogni due o tre mesi, Circa una volta al mese, Circa una volta ogni due settimane, Circa una volta alla settimana,
# Diverse volte alla settimana, Quasi ogni giorno, Almeno una volta al giorno
df_cod[,11:13] <- lapply(df_cod[,11:13], 
                       FUN = function(X)recode(X, "Mai=1; Molto raramente=2; Circa una volta ogni due o tre mesi=3; 
                                               Circa una volta al mese=4;Circa una volta ogni due settimane=5; 
                                               Circa una volta alla settimana=6; Diverse volte alla settimana=7;
                                               Quasi ogni giorno=8; Almeno una volta al giorno=9"))


#####EDU
#1 =“Primary school”, 2 = “Secondary school”, 3 = “Bachelor degree”, 4 = “Master degree”, 5 = “Ph.D/Medical specialization”
df_cod$EDU <-  lapply(df_cod$EDU, 
                      FUN = function(X)recode(X, "Licenza media=1; Diploma di istruzione secondaria superiore=2; Laurea triennale=3;
                                              Laurea magistrale=4"))
df_cod$EDU <- as.numeric(df_cod$EDU)

###### AGE
df_cod[160,1] <- 25
df_cod[211,1] <- 24
df_cod[293,1] <- 22                     
df_cod[353,1] <- 28                  
df_cod[469,1] <- NA



###### Gender
df_cod$GENDER <-  lapply(df_cod$GENDER, 
                      FUN = function(X)recode(X,"Femmina=F; Maschio=M;Genere non binario=NA; Preferisco non dirlo=NA"))
df_cod$GENDER <- as.character(df_cod$GENDER)



#### SEXUAL ORIENTATION Heterosexual= 0, others =1

df_cod$SEXUAL_ORIENTATION <-  ifelse(df_cod$SEXUAL_ORIENTATION=="Eterosessuale", 0,
                                    df_cod$SEXUAL_ORIENTATION)

df_cod$SEXUAL_ORIENTATION <-  ifelse(df_cod$SEXUAL_ORIENTATION!=0, 1,
                                     df_cod$SEXUAL_ORIENTATION)

### Relationship 
#In a relationship=1, single=0


df_cod$RELATIONSHIP <- df_new$`Stato civile`
df_cod$RELATIONSHIP <- lapply(df_cod$RELATIONSHIP, 
                              FUN = function(X)recode(X, "Celibe/Nubile=0; Separato/a=0; Divorziato/a=0; Coniugato/a=1;
                              In una relazione sentimentale=1; Convivente=1; Riconiugato/a=1"))


df_cod$RELATIONSHIP <- as.numeric(df_cod$RELATIONSHIP )

library(dplyr)
df_191 <- df_191[, 4:17]
str(df_cod)
df_cod <- select(df_cod, AGE, EDU, GENDER, SEXUAL_ORIENTATION, RELATIONSHIP, SOI_1, SOI_2,, SOI_3, SOI_4,SOI_5,SOI_6, SOI_7, SOI_8, SOI_9)
nrow(df_cod)
write.csv(df_cod, "Data_SOIR522.csv")                                             
                                              
                                              
                                              
                                              
                                              
                                              
