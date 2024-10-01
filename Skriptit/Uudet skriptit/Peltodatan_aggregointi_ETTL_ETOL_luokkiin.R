
#GTK DATA ####
#TUOTANTOSUUNTIEN YHDENMUKAISTUS ETTL KANSSA ####
#Mineraaliala ####
#GTK-datasta tehdyt viljelyala-aggregaatit sisään. 
#Näihin liitetään etol ja ettl koodit.
library(here)
library(readxl)
GTK_mineraali <-
  read_excel(here("Output/AreaAggregates", "Pinta_ala_aggregointi_gtk.xlsx"), 
             sheet = "Mineraaliala") #2 085 909 ha

#Yhdenmukainen kirjoitusasu
GTK_mineraali$Tuotantosuunta[GTK_mineraali$Tuotantosuunta == "Lammas- ja vuohitilat"]<-"Lammas_ja_vuohitilat"
GTK_mineraali$Tuotantosuunta[GTK_mineraali$Tuotantosuunta =="Muut nautakarjatilat"]<-"Muut_nautakarjatilat"
GTK_mineraali$Tuotantosuunta[GTK_mineraali$Tuotantosuunta == "Vihannekset ja juurekset"]<-"Vihannekset_juurekset"
GTK_mineraali$Tuotantosuunta[GTK_mineraali$Tuotantosuunta ==   "Rypsi ja rapsi" ]<-"Rypsi_rapsi"
GTK_mineraali$Tuotantosuunta[GTK_mineraali$Tuotantosuunta ==  "Öljyhampun ja pellavan viljely"]<-"Öljyhampun ja -pellavan viljely"
GTK_mineraali$Tuotantosuunta[GTK_mineraali$Tuotantosuunta ==  "Palkokasvit pl. tarhaherne"]<- "Palkokasvit_pl_tarhaherne"
GTK_mineraali$Tuotantosuunta[GTK_mineraali$Tuotantosuunta ==  "Tattari ja kinoa"]<-"Tattari_kinoa"


#Modifioidaan tuotantosuunnat siten, että vastaavat ETOL-luokkia 1:1. Muutoin tuloksena on tuplalaskentaa, jos samaan etoliin kuuluu monia peltodatan luokkia-.

#Yhdistetään "Viljat pl ohra" ja "Rehuohra". Vastaa silloin etolia "Kauran, ohran, rukiin ja vehnän viljely".

library(tidyverse)

#Summattavat tuotantosuunnat irti
Yhdistettavat_viljat <-
  filter(GTK_mineraali,
         GTK_mineraali$Tuotantosuunta %in% c("Viljat pl. ohra"))
Yhdistettavat_rehuohra <-
  filter(GTK_mineraali,
         GTK_mineraali$Tuotantosuunta %in% c("Rehuohra"))

#Mergellä yhteen tuotekoodin mukaisesti, NA:t nolliksi. Mineraalimaata_x on tuotteen ala viljatuotantosuunnalla, y rehuohralla
Yhdistettavat_vilja_rehuohra <-
  merge(Yhdistettavat_viljat,
        Yhdistettavat_rehuohra,
        by = "Kasvikoodi",
        all = T)
Yhdistettavat_vilja_rehuohra$Mineraalimaata.y[is.na(Yhdistettavat_vilja_rehuohra$Mineraalimaata.y)] <-
  0
Yhdistettavat_vilja_rehuohra$Mineraalimaata.x[is.na(Yhdistettavat_vilja_rehuohra$Mineraalimaata.x)] <-
  0

#ALojen summaus tuotteittain.

Yhdistettavat_vilja_rehuohra$Mineraalimaata <-
  Yhdistettavat_vilja_rehuohra$Mineraalimaata.x + Yhdistettavat_vilja_rehuohra$Mineraalimaata.y


#Tarkistettu, että yhdistetyn taulun viljeltyalasumma = ala rehuohra + ala viljat

#Uusi sarake yhdistetylle tuotantosuunnalle ja kasvinimille.

Yhdistettavat_vilja_rehuohra$Tuotantosuunta <-
  "Kauran, ohran, rukiin ja vehnän viljely" #Uusi nimi vastaa ettl nimeä suoraan

#Jompi kumpi yhdistettävistä, vilja tai rehuohra, viljelee aina tuotetta ja omistaa kasvinimen. Jos a on tyhjä, otetaan nimi b:ltä. Ovat samoja joka tapauksessa.
Yhdistettavat_vilja_rehuohra$Kasvinimi <-
  Yhdistettavat_vilja_rehuohra$Kasvinimi.x
Yhdistettavat_vilja_rehuohra$Kasvinimi[is.na(Yhdistettavat_vilja_rehuohra$Kasvinimi)] <-
  Yhdistettavat_vilja_rehuohra$Kasvinimi.y[is.na(Yhdistettavat_vilja_rehuohra$Kasvinimi)]


Yhdistettavat_vilja_rehuohra <-
  select(Yhdistettavat_vilja_rehuohra, 1, 8, 9, 10)

rm(Yhdistettavat_viljat, Yhdistettavat_rehuohra)


#Yhdistetään "Nurmet, laitumet, hakamaat" sekä "Energiakasvit".
#Vanha luokka Nurmet jne sisältää myös rehukasvit, koristekasvit ja taimitarhat.

Yhdistettavat_nurmet <-
  filter(GTK_mineraali,
         GTK_mineraali$Tuotantosuunta %in% c("Nurmet, laitumet, hakamaat"))
Yhdistettavat_energia <-
  filter(GTK_mineraali,
         GTK_mineraali$Tuotantosuunta %in% c("Energiakasvit"))

Yhdistettavat_nurmet_energia <-
  merge(Yhdistettavat_nurmet,
        Yhdistettavat_energia,
        by = "Kasvikoodi",
        all = T)

Yhdistettavat_nurmet_energia$Mineraalimaata.y[is.na(Yhdistettavat_nurmet_energia$Mineraalimaata.y)] <-
  0
Yhdistettavat_nurmet_energia$Mineraalimaata.x[is.na(Yhdistettavat_nurmet_energia$Mineraalimaata.x)] <-
  0

Yhdistettavat_nurmet_energia$Mineraalimaata <-
  Yhdistettavat_nurmet_energia$Mineraalimaata.x + Yhdistettavat_nurmet_energia$Mineraalimaata.y


Yhdistettavat_nurmet_energia$Tuotantosuunta <-
  "Nurmet_laitumet_hakamaat_rehukasvit_koristekasvit_energiakasvit_taimet"#Uusi nimi vastaa ettl nimeä suoraan


Yhdistettavat_nurmet_energia$Kasvinimi <-
  Yhdistettavat_nurmet_energia$Kasvinimi.x
Yhdistettavat_nurmet_energia$Kasvinimi[is.na(Yhdistettavat_nurmet_energia$Kasvinimi)] <-
  Yhdistettavat_nurmet_energia$Kasvinimi.y[is.na(Yhdistettavat_nurmet_energia$Kasvinimi)]


Yhdistettavat_nurmet_energia <-
  select(Yhdistettavat_nurmet_energia, 1, 8, 9, 10)

rm(Yhdistettavat_nurmet, Yhdistettavat_energia)

#Yhdistetään "Öljyhamppu" ja "Öljyellava". Vastaa ettl:ää "Öljyhampun ja pellavan viljely.

Yhdistettavat_öljyhamppu <-
  filter(GTK_mineraali,
         GTK_mineraali$Tuotantosuunta %in% c("Öljyhamppu"))
Yhdistettavat_pellava <-
  filter(GTK_mineraali,
         GTK_mineraali$Tuotantosuunta %in% c("Öljypellava"))

Yhdistettavat_oljyhamppu_pellava <-
  merge(Yhdistettavat_öljyhamppu,
        Yhdistettavat_pellava,
        by = "Kasvikoodi",
        all = T)


Yhdistettavat_oljyhamppu_pellava$Mineraalimaata.y[is.na(Yhdistettavat_oljyhamppu_pellava$Mineraalimaata.y)] <-
  0
Yhdistettavat_oljyhamppu_pellava$Mineraalimaata.x[is.na(Yhdistettavat_oljyhamppu_pellava$Mineraalimaata.x)] <-
  0

Yhdistettavat_oljyhamppu_pellava$Mineraalimaata <-
  Yhdistettavat_oljyhamppu_pellava$Mineraalimaata.x + Yhdistettavat_oljyhamppu_pellava$Mineraalimaata.y

Yhdistettavat_oljyhamppu_pellava$Tuotantosuunta <-
  "Öljyhampun ja pellavan viljely" #Uusi nimi vastaa ettl nimeä suoraan

Yhdistettavat_oljyhamppu_pellava$Kasvinimi <-
  Yhdistettavat_oljyhamppu_pellava$Kasvinimi.x
Yhdistettavat_oljyhamppu_pellava$Kasvinimi[is.na(Yhdistettavat_oljyhamppu_pellava$Kasvinimi)] <-
  Yhdistettavat_oljyhamppu_pellava$Kasvinimi.y[is.na(Yhdistettavat_oljyhamppu_pellava$Kasvinimi)]

Yhdistettavat_oljyhamppu_pellava <-
  select(Yhdistettavat_oljyhamppu_pellava, 1, 8, 9, 10)


rm(Yhdistettavat_pellava, Yhdistettavat_öljyhamppu)

#Yhdistetyt luokat samaan frameen.

Yhdistaminen <-
  rbind(Yhdistettavat_vilja_rehuohra,
        Yhdistettavat_oljyhamppu_pellava)
Yhdistaminen <- rbind(Yhdistaminen, Yhdistettavat_nurmet_energia)

rm(
  Yhdistettavat_nurmet_energia,
  Yhdistettavat_oljyhamppu_pellava,
  Yhdistettavat_vilja_rehuohra
)

#Poistetaan uusien summaluokkien komponentit aggregaatista, ja liitetään niiden tilalle aggregoidut luokat. Pinta-ala säilyy samana.

x <- c(
  "Viljat pl. ohra",
  "Rehuohra",
  "Nurmet, laitumet, hakamaat",
  "Energiakasvit",
  "Öljyhamppu",
  "Öljypellava"
)

GTK_mineraali <-
  filter(GTK_mineraali,!(GTK_mineraali$Tuotantosuunta %in% x))

GTK_mineraali <- rbind(GTK_mineraali, Yhdistaminen)

rm(Yhdistaminen)

#Pinta-ala edelleen sama, 2.08 milj ha

#Turveala ####

library(readxl)
GTK_turve <-
  read_excel(here("Output/AreaAggregates", "Pinta_ala_aggregointi_gtk.xlsx"),
             sheet = "Eloperäinen ala") #243 572.9 ha

GTK_turve$Tuotantosuunta[GTK_turve$Tuotantosuunta == "Lammas- ja vuohitilat"]<-"Lammas_ja_vuohitilat"
GTK_turve$Tuotantosuunta[GTK_turve$Tuotantosuunta =="Muut nautakarjatilat"]<-"Muut_nautakarjatilat"
GTK_turve$Tuotantosuunta[GTK_turve$Tuotantosuunta == "Hunajatuotanto"]<-"Hunajantuotanto"
GTK_turve$Tuotantosuunta[GTK_turve$Tuotantosuunta == "Vihannekset ja juurekset"]<-"Vihannekset_juurekset"
GTK_turve$Tuotantosuunta[GTK_turve$Tuotantosuunta ==   "Rypsi ja rapsi" ]<-"Rypsi_rapsi"
GTK_turve$Tuotantosuunta[GTK_turve$Tuotantosuunta ==  "Öljyhampun ja pellavan viljely"]<-"Öljyhampun ja -pellavan viljely"
GTK_turve$Tuotantosuunta[GTK_turve$Tuotantosuunta ==  "Palkokasvit pl. tarhaherne"]<- "Palkokasvit_pl_tarhaherne"
GTK_turve$Tuotantosuunta[GTK_turve$Tuotantosuunta ==  "Tattari ja kinoa"]<-"Tattari_kinoa"

#Yhdistetään "Viljat pl ohra" ja "Rehuohra". Vastaa silloin etolia "Kauran, ohran, rukiin ja vehnän viljely".

library(tidyverse)
#Summattavat tuotantosuunnat irti
Yhdistettavat_viljat <-
  filter(GTK_turve,
         GTK_turve$Tuotantosuunta %in% c("Viljat pl. ohra"))
Yhdistettavat_rehuohra <-
  filter(GTK_turve,
         GTK_turve$Tuotantosuunta %in% c("Rehuohra"))

#Mergellä yhteen tuotekoodin mukaisesti, NA:t nolliksi. Mineraalimaata_x on tuotteen ala viljatuotantosuunnalla, y rehuohralla
Yhdistettavat_vilja_rehuohra <-
  merge(Yhdistettavat_viljat,
        Yhdistettavat_rehuohra,
        by = "Kasvikoodi",
        all = T)
Yhdistettavat_vilja_rehuohra$EloperäistäMaata.y[is.na(Yhdistettavat_vilja_rehuohra$EloperäistäMaata.y)] <-
  0
Yhdistettavat_vilja_rehuohra$EloperäistäMaata.x[is.na(Yhdistettavat_vilja_rehuohra$EloperäistäMaata.x)] <-
  0

#ALojen summaus tuotteittain.

Yhdistettavat_vilja_rehuohra$EloperäistäMaata <-
  Yhdistettavat_vilja_rehuohra$EloperäistäMaata.x + Yhdistettavat_vilja_rehuohra$EloperäistäMaata.y


#Tarkistettu, että yhdistetyn taulun viljeltyalasumma = ala rehuohra + ala viljat

#Uusi sarake yhdistetylle tuotantosuunnalle ja kasvinimille.

Yhdistettavat_vilja_rehuohra$Tuotantosuunta <-
  "Kauran, ohran, rukiin ja vehnän viljely" #Uusi nimi vastaa ettl nimeä suoraan

#Jompi kumpi yhdistettävistä, vilja tai rehuohra, viljelee aina tuotetta ja omistaa kasvinimen. Jos a on tyhjä, otetaan nimi b:ltä. Ovat samoja joka tapauksessa.
Yhdistettavat_vilja_rehuohra$Kasvinimi <-
  Yhdistettavat_vilja_rehuohra$Kasvinimi.x
Yhdistettavat_vilja_rehuohra$Kasvinimi[is.na(Yhdistettavat_vilja_rehuohra$Kasvinimi)] <-
  Yhdistettavat_vilja_rehuohra$Kasvinimi.y[is.na(Yhdistettavat_vilja_rehuohra$Kasvinimi)]


Yhdistettavat_vilja_rehuohra <-
  select(Yhdistettavat_vilja_rehuohra, 1, 8, 9, 10)

rm(Yhdistettavat_viljat, Yhdistettavat_rehuohra)


#Yhdistetään "Nurmet, laitumet, hakamaat" sekä "Energiakasvit".
#Vanha luokka Nurmet jne sisältää myös rehukasvit, koristekasvit ja taimitarhat.

Yhdistettavat_nurmet <-
  filter(GTK_turve,
         GTK_turve$Tuotantosuunta %in% c("Nurmet, laitumet, hakamaat"))
Yhdistettavat_energia <-
  filter(GTK_turve,
         GTK_turve$Tuotantosuunta %in% c("Energiakasvit"))

Yhdistettavat_nurmet_energia <-
  merge(Yhdistettavat_nurmet,
        Yhdistettavat_energia,
        by = "Kasvikoodi",
        all = T)

Yhdistettavat_nurmet_energia$EloperäistäMaata.y[is.na(Yhdistettavat_nurmet_energia$EloperäistäMaata.y)] <-
  0
Yhdistettavat_nurmet_energia$EloperäistäMaata.x[is.na(Yhdistettavat_nurmet_energia$EloperäistäMaata.x)] <-
  0

Yhdistettavat_nurmet_energia$EloperäistäMaata <-
  Yhdistettavat_nurmet_energia$EloperäistäMaata.x + Yhdistettavat_nurmet_energia$EloperäistäMaata.y


Yhdistettavat_nurmet_energia$Tuotantosuunta <-
  "Nurmet_laitumet_hakamaat_rehukasvit_koristekasvit_energiakasvit_taimet"#Uusi nimi vastaa ettl nimeä suoraan


Yhdistettavat_nurmet_energia$Kasvinimi <-
  Yhdistettavat_nurmet_energia$Kasvinimi.x
Yhdistettavat_nurmet_energia$Kasvinimi[is.na(Yhdistettavat_nurmet_energia$Kasvinimi)] <-
  Yhdistettavat_nurmet_energia$Kasvinimi.y[is.na(Yhdistettavat_nurmet_energia$Kasvinimi)]


Yhdistettavat_nurmet_energia <-
  select(Yhdistettavat_nurmet_energia, 1, 8, 9, 10)

rm(Yhdistettavat_nurmet, Yhdistettavat_energia)

#Yhdistetään "Öljyhamppu" ja "Öljyellava". Vastaa ettl:ää "Öljyhampun ja pellavan viljely.

Yhdistettavat_öljyhamppu <-
  filter(GTK_turve,
         GTK_turve$Tuotantosuunta %in% c("Öljyhamppu"))
Yhdistettavat_pellava <-
  filter(GTK_turve,
         GTK_turve$Tuotantosuunta %in% c("Öljypellava"))

Yhdistettavat_oljyhamppu_pellava <-
  merge(Yhdistettavat_öljyhamppu,
        Yhdistettavat_pellava,
        by = "Kasvikoodi",
        all = T)


Yhdistettavat_oljyhamppu_pellava$EloperäistäMaata.y[is.na(Yhdistettavat_oljyhamppu_pellava$EloperäistäMaata.y)] <-
  0
Yhdistettavat_oljyhamppu_pellava$EloperäistäMaata.x[is.na(Yhdistettavat_oljyhamppu_pellava$EloperäistäMaata.x)] <-
  0

Yhdistettavat_oljyhamppu_pellava$EloperäistäMaata <-
  Yhdistettavat_oljyhamppu_pellava$EloperäistäMaata.x + Yhdistettavat_oljyhamppu_pellava$EloperäistäMaata.y

Yhdistettavat_oljyhamppu_pellava$Tuotantosuunta <-
  "Öljyhampun ja pellavan viljely" #Uusi nimi vastaa ettl nimeä suoraan

Yhdistettavat_oljyhamppu_pellava$Kasvinimi <-
  Yhdistettavat_oljyhamppu_pellava$Kasvinimi.x
Yhdistettavat_oljyhamppu_pellava$Kasvinimi[is.na(Yhdistettavat_oljyhamppu_pellava$Kasvinimi)] <-
  Yhdistettavat_oljyhamppu_pellava$Kasvinimi.y[is.na(Yhdistettavat_oljyhamppu_pellava$Kasvinimi)]

Yhdistettavat_oljyhamppu_pellava <-
  select(Yhdistettavat_oljyhamppu_pellava, 1, 8, 9, 10)


rm(Yhdistettavat_pellava, Yhdistettavat_öljyhamppu)

#Yhdistetyt luokat samaan frameen.

Yhdistaminen <-
  rbind(Yhdistettavat_vilja_rehuohra,
        Yhdistettavat_oljyhamppu_pellava)
Yhdistaminen <- rbind(Yhdistaminen, Yhdistettavat_nurmet_energia)

rm(
  Yhdistettavat_nurmet_energia,
  Yhdistettavat_oljyhamppu_pellava,
  Yhdistettavat_vilja_rehuohra
)

#Poistetaan uusien summaluokkien komponentit aggregaatista, ja liitetään niiden tilalle aggregoidut luokat. Pinta-ala säilyy samana.

x <- c(
  "Viljat pl. ohra",
  "Rehuohra",
  "Nurmet, laitumet, hakamaat",
  "Energiakasvit",
  "Öljyhamppu",
  "Öljypellava"
)

GTK_turve <-
  filter(GTK_turve,!(GTK_turve$Tuotantosuunta %in% x))

GTK_turve <- rbind(GTK_turve, Yhdistaminen)

rm(Yhdistaminen)

#Sama ala, 243572.9

#ETTL ja ETOL muunnot ####

#Otetaan sisään muuntoavain peltodatan kasvikoodien ja ettl-tuotteiden välille
#ETTL liittäminen kasvikoodeille
#Muuntoavaimessa yhdistetään jokainen kasvi envimatin ettl-tuotteeseen.

library(readxl)
Tuotemuunto <-
  read_excel(
    here("Data","Ruokavirasto-ENVIMATFOOD-muuntoavain_tuotteet_tuotantosuunnat.xlsx"),
    sheet = "Ruokavirasto-ETTL tuotemuunto",
    col_types = c("text", "text", "numeric",
                  "text", "skip")
  )
colnames(Tuotemuunto) <-
  c("ETTL_koodi", "ETTL_nimi", "Kasvikoodi", "Kasvinimi")

GTK_mineraali_ETTL <-
  merge(GTK_mineraali, Tuotemuunto, by = "Kasvikoodi") #sama hehtaarimäärä 2085909

GTK_mineraali_ETTL<-select(GTK_mineraali_ETTL,1:6)
colnames(GTK_mineraali_ETTL)[3]<-"Kasvinimi"

rm(GTK_mineraali)

GTK_turve_ETTL <-
  merge(GTK_turve, Tuotemuunto, by = "Kasvikoodi") #Sama hehtaarimäärä 243572.9
GTK_turve_ETTL<-select(GTK_turve_ETTL,1:6)
colnames(GTK_turve_ETTL)[3]<-"Kasvinimi"

rm(GTK_turve)

#Toimialamuunto


TSMuunto <-
  read_excel(here("Data/Ruokavirasto-ENVIMATFOOD-muuntoavain_tuotteet_tuotantosuunnat.xlsx"),sheet = "TSMuunto")
colnames(TSMuunto)<-c("ETOL","ETOL_koodi","Tuotantosuunta")

filter(GTK_mineraali_ETTL, !(Tuotantosuunta %in% TSMuunto$Tuotantosuunta))

GTK_mineraali_ETTL_ETOL<-merge(GTK_mineraali_ETTL, TSMuunto, by="Tuotantosuunta")

GTK_turve_ETTL_ETOL<-merge(GTK_mineraali_ETTL, TSMuunto, by="Tuotantosuunta")

GTK_turve_ETTL_ETOL<-merge(GTK_turve_ETTL,TSMuunto, by="Tuotantosuunta")

rm(GTK_mineraali_ETTL, GTK_turve_ETTL)

Mineraali_aggre <-
  aggregate(
    GTK_mineraali_ETTL_ETOL$Mineraalimaata,
    by = list(
      GTK_mineraali_ETTL_ETOL$ETOL_koodi,
      GTK_mineraali_ETTL_ETOL$ETOL,
      GTK_mineraali_ETTL_ETOL$ETTL_koodi,
      GTK_mineraali_ETTL_ETOL$ETTL_nimi
    ), sum
  )
colnames(Mineraali_aggre)<-c("ETOL-toimialakoodi","ETOL-toimiala","ETTL-tuotekoodi","ETTL-tuotenimi","Mineraalimaata ha")

Mineraali_aggre<-arrange(Mineraali_aggre, Mineraali_aggre$`ETTL-tuotekoodi`)



Turve_aggre <-
  aggregate(
    GTK_turve_ETTL_ETOL$EloperäistäMaata,
    by = list(
      GTK_turve_ETTL_ETOL$ETOL_koodi,
      GTK_turve_ETTL_ETOL$ETOL,
      GTK_turve_ETTL_ETOL$ETTL_koodi,
      GTK_turve_ETTL_ETOL$ETTL_nimi
    ), sum
  )
colnames(Turve_aggre)<-c("ETOL-toimialakoodi","ETOL-toimiala","ETTL-tuotekoodi","ETTL-tuotenimi","Turvemaata ha")

#GTK datan aggregaatin tulostus ####
library(openxlsx)
PeltodataEnvimatAggregointi<-createWorkbook()
addWorksheet(PeltodataEnvimatAggregointi, "GTK_mineral")
writeData(PeltodataEnvimatAggregointi, "GTK_mineral", Mineraali_aggre)
addWorksheet(PeltodataEnvimatAggregointi, "GTK_elop")
writeData(PeltodataEnvimatAggregointi, "GTK_elop", Turve_aggre)
saveWorkbook(PeltodataEnvimatAggregointi, file = "D:/Paastolaskenta_peltodata/Perunatuotteiden disaggregointi/Peltodata_envimatfood_aggre_gtk.xlsx", overwrite = T)

rm(list=ls())
gc()



#VILJAVUUSDATA ####
#Sisäänotto

library(readxl)
library(tidyverse)
Viljavuusdata <-
  read_excel(here("Output/AreaAggregates","Viljavuusdata_aggregointi_multavuudesta.xlsx"),
             sheet = "KaikkiPellot") 
Viljavuus_mineraali <-
  filter(Viljavuusdata,
         Viljavuusdata$`Maalaji multavuuden perusteella` == "Mineraali") #1 366942 ha
Viljavuus_turve <-
  filter(Viljavuusdata,
         Viljavuusdata$`Maalaji multavuuden perusteella` == "Eloperäinen") # 180055 ha


#Yht. 1546997 ha

rm(Viljavuusdata)


#Yhdenmukainen kirjoitusasu
Viljavuus_mineraali$Tuotantosuunta[Viljavuus_mineraali$Tuotantosuunta == "Lammas- ja vuohitilat"]<-"Lammas_ja_vuohitilat"
Viljavuus_mineraali$Tuotantosuunta[Viljavuus_mineraali$Tuotantosuunta =="Muut nautakarjatilat"]<-"Muut_nautakarjatilat"
Viljavuus_mineraali$Tuotantosuunta[Viljavuus_mineraali$Tuotantosuunta == "Hunajatuotanto"]<-"Hunajantuotanto"
Viljavuus_mineraali$Tuotantosuunta[Viljavuus_mineraali$Tuotantosuunta == "Vihannekset ja juurekset"]<-"Vihannekset_juurekset"
Viljavuus_mineraali$Tuotantosuunta[Viljavuus_mineraali$Tuotantosuunta ==   "Rypsi ja rapsi" ]<-"Rypsi_rapsi"
Viljavuus_mineraali$Tuotantosuunta[Viljavuus_mineraali$Tuotantosuunta ==  "Öljyhampun ja pellavan viljely"]<-"Öljyhampun ja -pellavan viljely"
Viljavuus_mineraali$Tuotantosuunta[Viljavuus_mineraali$Tuotantosuunta ==  "Palkokasvit pl. tarhaherne"]<- "Palkokasvit_pl_tarhaherne"
Viljavuus_mineraali$Tuotantosuunta[Viljavuus_mineraali$Tuotantosuunta ==  "Tattari ja kinoa"]<-"Tattari_kinoa"


#Yhdenmukainen kirjoitusasu
Viljavuus_turve$Tuotantosuunta[Viljavuus_turve$Tuotantosuunta == "Lammas- ja vuohitilat"]<-"Lammas_ja_vuohitilat"
Viljavuus_turve$Tuotantosuunta[Viljavuus_turve$Tuotantosuunta =="Muut nautakarjatilat"]<-"Muut_nautakarjatilat"
Viljavuus_turve$Tuotantosuunta[Viljavuus_turve$Tuotantosuunta == "Hunajatuotanto"]<-"Hunajantuotanto"
Viljavuus_turve$Tuotantosuunta[Viljavuus_turve$Tuotantosuunta == "Vihannekset ja juurekset"]<-"Vihannekset_juurekset"
Viljavuus_turve$Tuotantosuunta[Viljavuus_turve$Tuotantosuunta ==   "Rypsi ja rapsi" ]<-"Rypsi_rapsi"
Viljavuus_turve$Tuotantosuunta[Viljavuus_turve$Tuotantosuunta ==  "Öljyhampun ja pellavan viljely"]<-"Öljyhampun ja -pellavan viljely"
Viljavuus_turve$Tuotantosuunta[Viljavuus_turve$Tuotantosuunta ==  "Palkokasvit pl. tarhaherne"]<- "Palkokasvit_pl_tarhaherne"
Viljavuus_turve$Tuotantosuunta[Viljavuus_turve$Tuotantosuunta ==  "Tattari ja kinoa"]<-"Tattari_kinoa"


#Mineraalimaa ####

#Modifioidaan tuotantosuunnat siten, että vastaavat ETOL-luokkia 1:1. Muutoin tuloksena on tuplalaskentaa, jos samaan etoliin kuuluu monia peltodatan luokkia-.

#Yhdistetään "Viljat pl ohra" ja "Rehuohra". Vastaa silloin etolia "Kauran, ohran, rukiin ja vehnän viljely".

library(tidyverse)

#Summattavat tuotantosuunnat irti
Yhdistettavat_viljat <-
  filter(Viljavuus_mineraali,
         Viljavuus_mineraali$Tuotantosuunta %in% c("Viljat pl. ohra"))
Yhdistettavat_rehuohra <-
  filter(Viljavuus_mineraali,
         Viljavuus_mineraali$Tuotantosuunta %in% c("Rehuohra"))

#Mergellä yhteen tuotekoodin mukaisesti, NA:t nolliksi. Mineraalimaata_x on tuotteen ala viljatuotantosuunnalla, y rehuohralla
Yhdistettavat_vilja_rehuohra <-
  merge(Yhdistettavat_viljat,
        Yhdistettavat_rehuohra,
        by = "Kasvikoodi",
        all = T)
Yhdistettavat_vilja_rehuohra$`ala ha.y`[is.na(Yhdistettavat_vilja_rehuohra$`ala ha.y`)] <-
  0
Yhdistettavat_vilja_rehuohra$`ala ha.x`[is.na(Yhdistettavat_vilja_rehuohra$`ala ha.x`)] <-
  0

#ALojen summaus tuotteittain.

Yhdistettavat_vilja_rehuohra$Mineraalimaata <-
  Yhdistettavat_vilja_rehuohra$`ala ha.x` + Yhdistettavat_vilja_rehuohra$`ala ha.y`


Vilja_rehuohra<-sum(Yhdistettavat_vilja_rehuohra$Mineraalimaata)

#Tarkistettu, että yhdistetyn taulun viljeltyalasumma = ala rehuohra + ala viljat. 559051.9


#Uusi sarake yhdistetylle tuotantosuunnalle ja kasvinimille.

Yhdistettavat_vilja_rehuohra$Tuotantosuunta <-
  "Kauran, ohran, rukiin ja vehnän viljely" #Uusi nimi vastaa ettl nimeä suoraan

#Jompi kumpi yhdistettävistä, vilja tai rehuohra, viljelee aina tuotetta ja omistaa kasvinimen. Jos a on tyhjä, otetaan nimi b:ltä. Ovat samoja joka tapauksessa.
Yhdistettavat_vilja_rehuohra$Kasvinimi <-
  Yhdistettavat_vilja_rehuohra$Kasvinimi.x
Yhdistettavat_vilja_rehuohra$Kasvinimi[is.na(Yhdistettavat_vilja_rehuohra$Kasvinimi)] <-
  Yhdistettavat_vilja_rehuohra$Kasvinimi.y[is.na(Yhdistettavat_vilja_rehuohra$Kasvinimi)]

colnames(Yhdistettavat_vilja_rehuohra)[8]<-c("Maalaji multavuuden perusteella")
Yhdistettavat_vilja_rehuohra$`Maalaji multavuuden perusteella`<-"Mineraali"
Yhdistettavat_vilja_rehuohra <-
  select(Yhdistettavat_vilja_rehuohra, 1,10:12,8)

#Yhdistetään "Nurmet, laitumet, hakamaat" sekä "Energiakasvit".
#Vanha luokka Nurmet jne sisältää myös rehukasvit, koristekasvit ja taimitarhat.

Yhdistettavat_nurmet <-
  filter(Viljavuus_mineraali,
         Viljavuus_mineraali$Tuotantosuunta %in% c("Nurmet, laitumet, hakamaat"))
Yhdistettavat_energia <-
  filter(Viljavuus_mineraali,
         Viljavuus_mineraali$Tuotantosuunta %in% c("Energiakasvit"))

Yhdistettavat_nurmet_energia <-
  merge(Yhdistettavat_nurmet,
        Yhdistettavat_energia,
        by = "Kasvikoodi",
        all = T)

Yhdistettavat_nurmet_energia$`ala ha.y`[is.na(Yhdistettavat_nurmet_energia$`ala ha.y`)] <-
  0
Yhdistettavat_nurmet_energia$`ala ha.x`[is.na(Yhdistettavat_nurmet_energia$`ala ha.x`)] <-
  0

Yhdistettavat_nurmet_energia$Mineraalimaata <-
  Yhdistettavat_nurmet_energia$`ala ha.x` + Yhdistettavat_nurmet_energia$`ala ha.y`


Yhdistettavat_nurmet_energia$Tuotantosuunta <-
  "Nurmet_laitumet_hakamaat_rehukasvit_koristekasvit_energiakasvit_taimet"#Uusi nimi vastaa ettl nimeä suoraan


Yhdistettavat_nurmet_energia$Kasvinimi <-
  Yhdistettavat_nurmet_energia$Kasvinimi.x
Yhdistettavat_nurmet_energia$Kasvinimi[is.na(Yhdistettavat_nurmet_energia$Kasvinimi)] <-
  Yhdistettavat_nurmet_energia$Kasvinimi.y[is.na(Yhdistettavat_nurmet_energia$Kasvinimi)]

colnames(Yhdistettavat_nurmet_energia)[8]<-"Maalaji multavuuden perusteella"
Yhdistettavat_nurmet_energia$`Maalaji multavuuden perusteella`<-"Mineraali"
Yhdistettavat_nurmet_energia <-
  select(Yhdistettavat_nurmet_energia, 1,10:12,8) #68223.66 ha

Nurmet_energia<-sum(Yhdistettavat_nurmet_energia$Mineraalimaata)


rm(Yhdistettavat_nurmet, Yhdistettavat_energia)

#Yhdistetään "Öljyhamppu" ja "Öljyellava". Vastaa ettl:ää "Öljyhampun ja pellavan viljely.

Yhdistettavat_öljyhamppu <-
  filter(Viljavuus_mineraali,
         Viljavuus_mineraali$Tuotantosuunta %in% c("Öljyhamppu"))
Yhdistettavat_pellava <-
  filter(Viljavuus_mineraali,
         Viljavuus_mineraali$Tuotantosuunta %in% c("Öljypellava"))

Yhdistettavat_oljyhamppu_pellava <-
  merge(Yhdistettavat_öljyhamppu,
        Yhdistettavat_pellava,
        by = "Kasvikoodi",
        all = T)


Yhdistettavat_oljyhamppu_pellava$`ala ha.y`[is.na(Yhdistettavat_oljyhamppu_pellava$`ala ha.y`)] <-
  0
Yhdistettavat_oljyhamppu_pellava$`ala ha.x`[is.na(Yhdistettavat_oljyhamppu_pellava$`ala ha.x`)] <-
  0

Yhdistettavat_oljyhamppu_pellava$Mineraalimaata <-
  Yhdistettavat_oljyhamppu_pellava$`ala ha.x` + Yhdistettavat_oljyhamppu_pellava$`ala ha.y`

Yhdistettavat_oljyhamppu_pellava$Tuotantosuunta <-
  "Öljyhampun ja pellavan viljely" #Uusi nimi vastaa ettl nimeä suoraan

Yhdistettavat_oljyhamppu_pellava$Kasvinimi <-
  Yhdistettavat_oljyhamppu_pellava$Kasvinimi.x
Yhdistettavat_oljyhamppu_pellava$Kasvinimi[is.na(Yhdistettavat_oljyhamppu_pellava$Kasvinimi)] <-
  Yhdistettavat_oljyhamppu_pellava$Kasvinimi.y[is.na(Yhdistettavat_oljyhamppu_pellava$Kasvinimi)]

colnames(Yhdistettavat_oljyhamppu_pellava)[8]<-"Maalaji multavuuden perusteella"
Yhdistettavat_oljyhamppu_pellava$`Maalaji multavuuden perusteella`<-"Mineraali"
Yhdistettavat_oljyhamppu_pellava <-
  select(Yhdistettavat_oljyhamppu_pellava, 1,10:12,8)


Oljyhamppu_pellava<-sum(Yhdistettavat_oljyhamppu_pellava$Mineraalimaata)

rm(Yhdistettavat_pellava, Yhdistettavat_öljyhamppu) #Yhteensä 1419.16 ha




#Yhdistetyt luokat samaan frameen.

Yhdistaminen <-
  rbind(Yhdistettavat_vilja_rehuohra,
        Yhdistettavat_oljyhamppu_pellava)


Yhdistaminen <- rbind(Yhdistaminen, Yhdistettavat_nurmet_energia)

rm(
  Yhdistettavat_nurmet_energia,
  Yhdistettavat_oljyhamppu_pellava,
  Yhdistettavat_vilja_rehuohra
)


sum(Yhdistaminen$Mineraalimaata) == Nurmet_energia+Oljyhamppu_pellava+Vilja_rehuohra

#Poistetaan uusien summaluokkien komponentit aggregaatista, ja liitetään niiden tilalle aggregoidut luokat. Pinta-ala säilyy samana.

x <- c(
  "Viljat pl. ohra",
  "Rehuohra",
  "Nurmet, laitumet, hakamaat",
  "Energiakasvit",
  "Öljyhamppu",
  "Öljypellava"
)

t <-
  filter(Viljavuus_mineraali,(Viljavuus_mineraali$Tuotantosuunta %in% x))

Viljavuus_mineraali <-
  filter(Viljavuus_mineraali,!(Viljavuus_mineraali$Tuotantosuunta %in% x))
colnames(Viljavuus_mineraali)[5]<-"Mineraalimaata"

Viljavuus_mineraali <- rbind(Viljavuus_mineraali, Yhdistaminen)

rm(Yhdistaminen)

#Turvemaa ####

#Modifioidaan tuotantosuunnat siten, että vastaavat ETOL-luokkia 1:1. Muutoin tuloksena on tuplalaskentaa, jos samaan etoliin kuuluu monia peltodatan luokkia-.

#Yhdistetään "Viljat pl ohra" ja "Rehuohra". Vastaa silloin etolia "Kauran, ohran, rukiin ja vehnän viljely".

library(tidyverse)

#Summattavat tuotantosuunnat irti
Yhdistettavat_viljat <-
  filter(Viljavuus_turve,
         Viljavuus_turve$Tuotantosuunta %in% c("Viljat pl. ohra"))
Yhdistettavat_rehuohra <-
  filter(Viljavuus_turve,
         Viljavuus_turve$Tuotantosuunta %in% c("Rehuohra"))

#Mergellä yhteen tuotekoodin mukaisesti, NA:t nolliksi. Mineraalimaata_x on tuotteen ala viljatuotantosuunnalla, y rehuohralla
Yhdistettavat_vilja_rehuohra <-
  merge(Yhdistettavat_viljat,
        Yhdistettavat_rehuohra,
        by = "Kasvikoodi",
        all = T)
Yhdistettavat_vilja_rehuohra$`ala ha.y`[is.na(Yhdistettavat_vilja_rehuohra$`ala ha.y`)] <-
  0
Yhdistettavat_vilja_rehuohra$`ala ha.x`[is.na(Yhdistettavat_vilja_rehuohra$`ala ha.x`)] <-
  0

#ALojen summaus tuotteittain.

Yhdistettavat_vilja_rehuohra$Turvemaata <-
  Yhdistettavat_vilja_rehuohra$`ala ha.x` + Yhdistettavat_vilja_rehuohra$`ala ha.y`


#Uusi sarake yhdistetylle tuotantosuunnalle ja kasvinimille.

Yhdistettavat_vilja_rehuohra$Tuotantosuunta <-
  "Kauran, ohran, rukiin ja vehnän viljely" #Uusi nimi vastaa ettl nimeä suoraan

#Jompi kumpi yhdistettävistä, vilja tai rehuohra, viljelee aina tuotetta ja omistaa kasvinimen. Jos a on tyhjä, otetaan nimi b:ltä. Ovat samoja joka tapauksessa.
Yhdistettavat_vilja_rehuohra$Kasvinimi <-
  Yhdistettavat_vilja_rehuohra$Kasvinimi.x
Yhdistettavat_vilja_rehuohra$Kasvinimi[is.na(Yhdistettavat_vilja_rehuohra$Kasvinimi)] <-
  Yhdistettavat_vilja_rehuohra$Kasvinimi.y[is.na(Yhdistettavat_vilja_rehuohra$Kasvinimi)]

colnames(Yhdistettavat_vilja_rehuohra)[8]<-c("Maalaji multavuuden perusteella")
Yhdistettavat_vilja_rehuohra$`Maalaji multavuuden perusteella`<-"Turve"
Yhdistettavat_vilja_rehuohra <-
  select(Yhdistettavat_vilja_rehuohra, 1,10:12,8)

Vilja_rehuohra<-sum(Yhdistettavat_vilja_rehuohra$Turvemaata) 

rm(Yhdistettavat_viljat, Yhdistettavat_rehuohra)


#Yhdistetään "Nurmet, laitumet, hakamaat" sekä "Energiakasvit".
#Vanha luokka Nurmet jne sisältää myös rehukasvit, koristekasvit ja taimitarhat.

Yhdistettavat_nurmet <-
  filter(Viljavuus_turve,
         Viljavuus_turve$Tuotantosuunta %in% c("Nurmet, laitumet, hakamaat"))
Yhdistettavat_energia <-
  filter(Viljavuus_turve,
         Viljavuus_turve$Tuotantosuunta %in% c("Energiakasvit"))

Yhdistettavat_nurmet_energia <-
  merge(Yhdistettavat_nurmet,
        Yhdistettavat_energia,
        by = "Kasvikoodi",
        all = T)

Yhdistettavat_nurmet_energia$`ala ha.y`[is.na(Yhdistettavat_nurmet_energia$`ala ha.y`)] <-
  0
Yhdistettavat_nurmet_energia$`ala ha.x`[is.na(Yhdistettavat_nurmet_energia$`ala ha.x`)] <-
  0

Yhdistettavat_nurmet_energia$Turvemaata <-
  Yhdistettavat_nurmet_energia$`ala ha.x` + Yhdistettavat_nurmet_energia$`ala ha.y`


Yhdistettavat_nurmet_energia$Tuotantosuunta <-
  "Nurmet_laitumet_hakamaat_rehukasvit_koristekasvit_energiakasvit_taimet"#Uusi nimi vastaa ettl nimeä suoraan


Yhdistettavat_nurmet_energia$Kasvinimi <-
  Yhdistettavat_nurmet_energia$Kasvinimi.x
Yhdistettavat_nurmet_energia$Kasvinimi[is.na(Yhdistettavat_nurmet_energia$Kasvinimi)] <-
  Yhdistettavat_nurmet_energia$Kasvinimi.y[is.na(Yhdistettavat_nurmet_energia$Kasvinimi)]

colnames(Yhdistettavat_nurmet_energia)[8]<-"Maalaji multavuuden perusteella"
Yhdistettavat_nurmet_energia$`Maalaji multavuuden perusteella`<-"Turve"
Yhdistettavat_nurmet_energia <-
  select(Yhdistettavat_nurmet_energia, 1,10:12,8) #68223.66 ha

Nurmet_energia<-sum(Yhdistettavat_nurmet_energia$Turvemaata)

rm(Yhdistettavat_nurmet, Yhdistettavat_energia)

#Yhdistetään "Öljyhamppu" ja "Öljyellava". Vastaa ettl:ää "Öljyhampun ja pellavan viljely.

Yhdistettavat_öljyhamppu <-
  filter(Viljavuus_turve,
         Viljavuus_turve$Tuotantosuunta %in% c("Öljyhamppu"))
Yhdistettavat_pellava <-
  filter(Viljavuus_turve,
         Viljavuus_turve$Tuotantosuunta %in% c("Öljypellava"))

Yhdistettavat_oljyhamppu_pellava <-
  merge(Yhdistettavat_öljyhamppu,
        Yhdistettavat_pellava,
        by = "Kasvikoodi",
        all = T)


Yhdistettavat_oljyhamppu_pellava$`ala ha.y`[is.na(Yhdistettavat_oljyhamppu_pellava$`ala ha.y`)] <-
  0
Yhdistettavat_oljyhamppu_pellava$`ala ha.x`[is.na(Yhdistettavat_oljyhamppu_pellava$`ala ha.x`)] <-
  0

Yhdistettavat_oljyhamppu_pellava$Turvemaata <-
  Yhdistettavat_oljyhamppu_pellava$`ala ha.x` + Yhdistettavat_oljyhamppu_pellava$`ala ha.y`

Yhdistettavat_oljyhamppu_pellava$Tuotantosuunta <-
  "Öljyhampun ja pellavan viljely" #Uusi nimi vastaa ettl nimeä suoraan

Yhdistettavat_oljyhamppu_pellava$Kasvinimi <-
  Yhdistettavat_oljyhamppu_pellava$Kasvinimi.x
Yhdistettavat_oljyhamppu_pellava$Kasvinimi[is.na(Yhdistettavat_oljyhamppu_pellava$Kasvinimi)] <-
  Yhdistettavat_oljyhamppu_pellava$Kasvinimi.y[is.na(Yhdistettavat_oljyhamppu_pellava$Kasvinimi)]

colnames(Yhdistettavat_oljyhamppu_pellava)[8]<-"Maalaji multavuuden perusteella"
Yhdistettavat_oljyhamppu_pellava$`Maalaji multavuuden perusteella`<-"Turve"
Yhdistettavat_oljyhamppu_pellava <-
  select(Yhdistettavat_oljyhamppu_pellava, 1,10:12,8)

Oljyhamppu_pellava<-sum(Yhdistettavat_oljyhamppu_pellava$Turvemaata)

rm(Yhdistettavat_pellava, Yhdistettavat_öljyhamppu) 



#Yhdistetyt luokat samaan frameen.

Yhdistaminen <-
  rbind(Yhdistettavat_vilja_rehuohra,
        Yhdistettavat_oljyhamppu_pellava)


Yhdistaminen <- rbind(Yhdistaminen, Yhdistettavat_nurmet_energia)

rm(
  Yhdistettavat_nurmet_energia,
  Yhdistettavat_oljyhamppu_pellava,
  Yhdistettavat_vilja_rehuohra
)


sum(Yhdistaminen$Turvemaata) == Nurmet_energia+Oljyhamppu_pellava+Vilja_rehuohra



#Poistetaan uusien summaluokkien komponentit aggregaatista, ja liitetään niiden tilalle aggregoidut luokat. Pinta-ala säilyy samana.

x <- c(
  "Viljat pl. ohra",
  "Rehuohra",
  "Nurmet, laitumet, hakamaat",
  "Energiakasvit",
  "Öljyhamppu",
  "Öljypellava"
)

t <-
  filter(Viljavuus_turve,(Viljavuus_turve$Tuotantosuunta %in% x)) #testataan että sama hehtaarimäärä irtoaa

Viljavuus_turve <-
  filter(Viljavuus_turve,!(Viljavuus_turve$Tuotantosuunta %in% x))
colnames(Viljavuus_turve)[5]<-"Turvemaata"
Viljavuus_turve <- rbind(Viljavuus_turve, Yhdistaminen)

rm(Yhdistaminen)

#ETTL ja ETOL Muunnot ####


#Otetaan sisään muuntoavain peltodatan kasvikoodien ja ettl-tuotteiden välille
#ETTL liittäminen kasvikoodeille
#Muuntoavaimessa yhdistetään jokainen kasvi envimatin ettl-tuotteeseen.

library(readxl)
Tuotemuunto <-
  read_excel(
    "Perunatuotteiden disaggregointi/Ruokavirasto-ENVIMATFOOD-muuntoavain_tuotteet_tuotantosuunnat.xlsx",
    sheet = "Ruokavirasto-ETTL tuotemuunto",
    col_types = c("text", "text", "numeric",
                  "text", "skip")
  )
colnames(Tuotemuunto) <-
  c("ETTL_koodi", "ETTL_nimi", "Kasvikoodi", "Kasvinimi")

Viljavuus_mineraali_ETTL <-
  merge(Viljavuus_mineraali, Tuotemuunto, by = "Kasvikoodi") #sama hehtaarimäärä 2085909

Viljavuus_mineraali_ETTL<-select(Viljavuus_mineraali_ETTL,1:7)
colnames(Viljavuus_mineraali_ETTL)[3]<-"Kasvinimi" 

sum(Viljavuus_mineraali$Mineraalimaata) == sum(Viljavuus_mineraali_ETTL$Mineraalimaata) 

rm(Viljavuus_mineraali)

Viljavuus_turve_ETTL <-
  merge(Viljavuus_turve, Tuotemuunto, by = "Kasvikoodi") #Sama hehtaarimäärä 243572.9
Viljavuus_turve_ETTL<-select(Viljavuus_turve_ETTL,1:7)
colnames(Viljavuus_turve_ETTL)[3]<-"Kasvinimi"

sum(Viljavuus_turve$Turvemaata) == sum(Viljavuus_turve_ETTL$Turvemaata)

rm(Viljavuus_turve)

#Toimialamuunto

TSMuunto <-
  read_excel(
    "D:/Paastolaskenta_peltodata/Perunatuotteiden disaggregointi/Ruokavirasto-ENVIMATFOOD-muuntoavain_tuotteet_tuotantosuunnat.xlsx",sheet = "TSMuunto")
colnames(TSMuunto)<-c("ETOL","ETOL_koodi","Tuotantosuunta")

filter(Viljavuus_mineraali_ETTL, !(Tuotantosuunta %in% TSMuunto$Tuotantosuunta))

Viljavuus_mineraali_ETTL_ETOL<-merge(Viljavuus_mineraali_ETTL, TSMuunto, by="Tuotantosuunta")

Viljavuus_turve_ETTL_ETOL<-merge(Viljavuus_turve_ETTL, TSMuunto, by="Tuotantosuunta")

rm(Viljavuus_mineraali_ETTL, Viljavuus_turve_ETTL)

Mineraali_aggre <-
  aggregate(
    Viljavuus_mineraali_ETTL_ETOL$Mineraalimaata,
    by = list(
      Viljavuus_mineraali_ETTL_ETOL$ETOL_koodi,
      Viljavuus_mineraali_ETTL_ETOL$ETOL,
      Viljavuus_mineraali_ETTL_ETOL$ETTL_koodi,
      Viljavuus_mineraali_ETTL_ETOL$ETTL_nimi
    ), sum
  )
colnames(Mineraali_aggre)<-c("ETOL-toimialakoodi","ETOL-toimiala","ETTL-tuotekoodi","ETTL-tuotenimi","Mineraalimaata ha")

Mineraali_aggre<-arrange(Mineraali_aggre, Mineraali_aggre$`ETTL-tuotekoodi`)



Turve_aggre <-
  aggregate(
    Viljavuus_turve_ETTL_ETOL$Turvemaata,
    by = list(
      Viljavuus_turve_ETTL_ETOL$ETOL_koodi,
      Viljavuus_turve_ETTL_ETOL$ETOL,
      Viljavuus_turve_ETTL_ETOL$ETTL_koodi,
      Viljavuus_turve_ETTL_ETOL$ETTL_nimi
    ), sum
  )
colnames(Turve_aggre)<-c("ETOL-toimialakoodi","ETOL-toimiala","ETTL-tuotekoodi","ETTL-tuotenimi","Turvemaata ha")








#Viljavuusdatan aggregaatin tulostus ####
library(openxlsx)
PeltodataEnvimatAggregointi<-createWorkbook()
addWorksheet(PeltodataEnvimatAggregointi, "Viljavuus_mineral")
writeData(PeltodataEnvimatAggregointi, "Viljavuus_mineral", Mineraali_aggre)
addWorksheet(PeltodataEnvimatAggregointi, "Viljavuus_elop")
writeData(PeltodataEnvimatAggregointi, "Viljavuus_elop", Turve_aggre)
saveWorkbook(PeltodataEnvimatAggregointi, file = "D:/Paastolaskenta_peltodata/Perunatuotteiden disaggregointi/Peltodata_envimatfood_aggre_Viljavuus.xlsx", overwrite = T)

rm(list=ls())
gc()



