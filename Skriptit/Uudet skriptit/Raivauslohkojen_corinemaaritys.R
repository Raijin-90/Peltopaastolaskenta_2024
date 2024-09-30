

#RAIVATTUJEN LOHKOJEN UUDELLEENMÄÄRITTELY CORINEN PERUSTEELLA

#Sisäänotetaan uudet raivatut lohkot, jotka määritetty corinen perusteella
#Vanha määrittely (kahden peltodatan overlay) korvataan. Datan dimensiot tai muuttujajärjestys ei saa tässä muuttua, koska silloin myös päästölaskennan dimensioissa tapahtuu muutoksia.m 

library(tidyverse);library(readxl);library(here);library(stringr)

Raivatut_lohkot_corinepohjaisesti <- read_excel("D:/PROJEKTIT/JUSTFOOD/Raivaukset_corinepohjalta/Raivatut_lohkot_corinepohjaisesti.xlsx")

#Näiden perusteella korvataan vanha raivatuiksi määritettyjen lohkojen kohdistus. 



#Sisäänluenta 

Yhdistetty_peltodata_raivaukset_rehuvilja <- read_delim(here("Data", "Yhdistetty_peltodata_raivaukset_rehuvilja.csv"), 
                                                        delim = ",", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                                                        trim_ws = TRUE)

#Raivausmäärittely perustuu Peruslohkokoodin ja kasvulohkokoodin yhdistelmälle. Esim. 123456A ja 123456B ovat omia lohkopolygonejaan. 
#Tehdään vastaava koodiyhdistelmä peltolohkodataan

Yhdistetty_peltodata_raivaukset_rehuvilja$koodit<- str_c(Yhdistetty_peltodata_raivaukset_rehuvilja$PLTUNNUS,Yhdistetty_peltodata_raivaukset_rehuvilja$KLTUNNUS) 

#Nollataan aikaisempi raivausmerkintä

Yhdistetty_peltodata_raivaukset_rehuvilja$Raivattu<-NA


#Liitetään niille koodeille, jotka ovat corinen perusteella määrätty raivatuiksi, vastaava merkintä "2000 jälkeen". Muutoin jätetään NA.  

Yhdistetty_peltodata_raivaukset_rehuvilja<-Yhdistetty_peltodata_raivaukset_rehuvilja %>% 
  mutate(Raivattu = if_else(koodit %in% Raivatut_lohkot_corinepohjaisesti$Koodit,"2000 jälkeen",NA))

#Poistetaan koodit-muuttuja, jotta datan dimensiot eivät muutu
Yhdistetty_peltodata_raivaukset_rehuvilja$koodit<-NULL


#Vanhat määreet sisältävä peltodata-csv on kansioitu erilleen, joten alkuperäinen voidaan nyt ylikirjoitta 

write.csv(Yhdistetty_peltodata_raivaukset_rehuvilja, file=here("Data/Yhdistetty_peltodata_raivaukset_rehuvilja.csv"))

rm(list=ls())
gc()