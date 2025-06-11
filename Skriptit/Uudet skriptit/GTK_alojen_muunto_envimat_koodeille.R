

#GTK-viljelyaladatan muuntaminen envimat-koodeille (ETTL ja ETOL)

library(tidyverse);library(here);library(stringr);library(varhandle)

source(here("Skriptit/Uudet skriptit/GTK_datan_aggregointi.R"))

rm.all.but(c("GTK_aggregointi_elop",
           "GTK_aggregointi_elop_raiviot",
           "GTK_aggregointi_mineral",
           "GTK_aggregointi_mineral_raiviot"))
           

#Tuotantosuuntien kirjoitusasujen yhdenmukaistus. Koskee nii joissa on välilyöntejä. #####

GTK_aggregointi_elop<-GTK_aggregointi_elop %>% mutate(Tuotantosuunta = case_when(Tuotantosuunta == "Lammas- ja vuohitilat" ~ "Lammas_ja_vuohitilat",
                                              Tuotantosuunta == "Muut nautakarjatilat" ~ "Muut_nautakarjatilat",
                                              Tuotantosuunta == "Nurmet, laitumet, hakamaat" ~ "Nurmet_laitumet_hakamaat",
                                              Tuotantosuunta == "Palkokasvit pl. tarhaherne" ~ "Palkokasvit_pl_tarhaherne",
                                              Tuotantosuunta == "Rypsi ja rapsi" ~ "Rypsi_rapsi",
                                              Tuotantosuunta == "Tattari ja kinoa" ~ "Tattari_kinoa",
                                              Tuotantosuunta == "Vihannekset ja juurekset" ~ "Vihannekset_juurekset",
                                              Tuotantosuunta == "Viljat pl. ohra" ~ "Viljat_pl_ohra",
                                              .default = Tuotantosuunta))

GTK_aggregointi_elop_raiviot <- GTK_aggregointi_elop_raiviot %>% mutate(Tuotantosuunta = case_when(Tuotantosuunta == "Lammas- ja vuohitilat" ~ "Lammas_ja_vuohitilat",
                                                                                                   Tuotantosuunta == "Muut nautakarjatilat" ~ "Muut_nautakarjatilat",
                                                                                                   Tuotantosuunta == "Nurmet, laitumet, hakamaat" ~ "Nurmet_laitumet_hakamaat",
                                                                                                   Tuotantosuunta == "Palkokasvit pl. tarhaherne" ~ "Palkokasvit_pl_tarhaherne",
                                                                                                   Tuotantosuunta == "Rypsi ja rapsi" ~ "Rypsi_rapsi",
                                                                                                   Tuotantosuunta == "Tattari ja kinoa" ~ "Tattari_kinoa",
                                                                                                   Tuotantosuunta == "Vihannekset ja juurekset" ~ "Vihannekset_juurekset",
                                                                                                   Tuotantosuunta == "Viljat pl. ohra" ~ "Viljat_pl_ohra",
                                                                                                   .default = Tuotantosuunta))

GTK_aggregointi_mineral <- GTK_aggregointi_mineral %>% mutate(Tuotantosuunta = case_when(Tuotantosuunta == "Lammas- ja vuohitilat" ~ "Lammas_ja_vuohitilat",
                                                                                             Tuotantosuunta == "Muut nautakarjatilat" ~ "Muut_nautakarjatilat",
                                                                                             Tuotantosuunta == "Nurmet, laitumet, hakamaat" ~ "Nurmet_laitumet_hakamaat",
                                                                                             Tuotantosuunta == "Palkokasvit pl. tarhaherne" ~ "Palkokasvit_pl_tarhaherne",
                                                                                             Tuotantosuunta == "Rypsi ja rapsi" ~ "Rypsi_rapsi",
                                                                                             Tuotantosuunta == "Tattari ja kinoa" ~ "Tattari_kinoa",
                                                                                             Tuotantosuunta == "Vihannekset ja juurekset" ~ "Vihannekset_juurekset",
                                                                                             Tuotantosuunta == "Viljat pl. ohra" ~ "Viljat_pl_ohra",
                                                                                             .default = Tuotantosuunta))

GTK_aggregointi_mineral_raiviot <- GTK_aggregointi_mineral_raiviot %>% mutate(Tuotantosuunta = case_when(Tuotantosuunta == "Lammas- ja vuohitilat" ~ "Lammas_ja_vuohitilat",
                                                                                                         Tuotantosuunta == "Muut nautakarjatilat" ~ "Muut_nautakarjatilat",
                                                                                                         Tuotantosuunta == "Nurmet, laitumet, hakamaat" ~ "Nurmet_laitumet_hakamaat",
                                                                                                         Tuotantosuunta == "Palkokasvit pl. tarhaherne" ~ "Palkokasvit_pl_tarhaherne",
                                                                                                         Tuotantosuunta == "Rypsi ja rapsi" ~ "Rypsi_rapsi",
                                                                                                         Tuotantosuunta == "Tattari ja kinoa" ~ "Tattari_kinoa",
                                                                                                         Tuotantosuunta == "Vihannekset ja juurekset" ~ "Vihannekset_juurekset",
                                                                                                         Tuotantosuunta == "Viljat pl. ohra" ~ "Viljat_pl_ohra",
                                                                                                         .default = Tuotantosuunta))  
                         
#Tuotantosuuntaryhmien tasolle muunto


library(readxl)
Tuotantosuuntaryhmat <-
  read_excel(
    here("Data","Muuntoavain_tuotantosuunnat_tuotteet_ETOL.xlsx"),
    sheet = "Tuotantosuunnat ryhmittäin",
    col_types = c("text",
                  "text",
                  "text"
    )
  )
colnames(Tuotantosuuntaryhmat) <-
  c("Tuotantosuunta", "Tuotantosuuntaryhmä","ETOL")


#Yhdistetään tuotantosuuntaryhmän nimi tuotantosuuntanimen perusteella


GTK_aggregointi_elop<-inner_join(GTK_aggregointi_elop, Tuotantosuuntaryhmat, by="Tuotantosuunta")

GTK_aggregointi_elop_raiviot<-inner_join(GTK_aggregointi_elop_raiviot, Tuotantosuuntaryhmat, by="Tuotantosuunta")
  
GTK_aggregointi_mineral <- inner_join(GTK_aggregointi_mineral, Tuotantosuuntaryhmat, by="Tuotantosuunta")

GTK_aggregointi_mineral_raiviot<- inner_join(GTK_aggregointi_mineral_raiviot, Tuotantosuuntaryhmat, by="Tuotantosuunta")

rm.all.but(c("GTK_aggregointi_elop",
             "GTK_aggregointi_elop_raiviot",
             "GTK_aggregointi_mineral",
             "GTK_aggregointi_mineral_raiviot"))



#Seuraavaksi yhdistetään tuotekoodi ja sitä vastaava ETTL-tuotenimi
#Jokainen kasvikoodi vastaa jotain ettl tuotetta

library(readxl)
Tuoteryhmat <-
  read_excel(
    here("Data","Muuntoavain_tuotantosuunnat_tuotteet_ETOL.xlsx"),
    sheet = "Kasvit_ETTL_koodeittain",
    col_types = c("text",
                  "text",
                  "numeric",
                  "text"
    )
  )
colnames(Tuoteryhmat)[3:4]<-c("Kasvikoodi", "Kasvinimi")

#Kasvinimi, joka koodia vastaa, tulee nyt kahdesta framesta. Toinen niistä poistetaan. 

nrow(inner_join(GTK_aggregointi_elop, Tuoteryhmat, by=c("Kasvikoodi")))
GTK_aggregointi_elop<-inner_join(GTK_aggregointi_elop, Tuoteryhmat, by="Kasvikoodi")
GTK_aggregointi_elop$Kasvinimi.y<-NULL
colnames(GTK_aggregointi_elop)[colnames(GTK_aggregointi_elop)=="Kasvinimi.x"]<-"Kasvinimi"


nrow(inner_join(GTK_aggregointi_elop_raiviot, Tuoteryhmat, by=c("Kasvikoodi")))
GTK_aggregointi_elop_raiviot<-inner_join(GTK_aggregointi_elop_raiviot, Tuoteryhmat, by="Kasvikoodi")
GTK_aggregointi_elop_raiviot$Kasvinimi.y<-NULL
colnames(GTK_aggregointi_elop_raiviot)[colnames(GTK_aggregointi_elop_raiviot)=="Kasvinimi.x"]<-"Kasvinimi"


nrow(inner_join(GTK_aggregointi_mineral, Tuoteryhmat, by=c("Kasvikoodi")))
GTK_aggregointi_mineral<-inner_join(GTK_aggregointi_mineral, Tuoteryhmat, by="Kasvikoodi")
GTK_aggregointi_mineral$Kasvinimi.y<-NULL
colnames(GTK_aggregointi_mineral)[colnames(GTK_aggregointi_mineral)=="Kasvinimi.x"]<-"Kasvinimi"


nrow(inner_join(GTK_aggregointi_mineral_raiviot, Tuoteryhmat, by=c("Kasvikoodi")))
GTK_aggregointi_mineral_raiviot<-inner_join(GTK_aggregointi_mineral_raiviot, Tuoteryhmat, by="Kasvikoodi")
GTK_aggregointi_mineral_raiviot$Kasvinimi.y<-NULL
colnames(GTK_aggregointi_mineral_raiviot)[colnames(GTK_aggregointi_mineral_raiviot)=="Kasvinimi.x"]<-"Kasvinimi"

#Lisätään yksi- monivuotisuustieto

library(readxl)
Kasvikategoriat_avain <- read_excel("Data/Kasvikategoriat_avain.xlsx")


GTK_aggregointi_elop<-inner_join(GTK_aggregointi_elop, Kasvikategoriat_avain, by="Kasvikoodi")
GTK_aggregointi_elop$Kasvi<-NULL


GTK_aggregointi_elop_raiviot<-inner_join(GTK_aggregointi_elop_raiviot, Kasvikategoriat_avain, by="Kasvikoodi")
GTK_aggregointi_elop_raiviot$Kasvi<-NULL

GTK_aggregointi_mineral<-inner_join(GTK_aggregointi_mineral, Kasvikategoriat_avain, by="Kasvikoodi")
GTK_aggregointi_mineral$Kasvi<-NULL

GTK_aggregointi_mineral_raiviot<-inner_join(GTK_aggregointi_mineral_raiviot, Kasvikategoriat_avain, by="Kasvikoodi")
GTK_aggregointi_mineral_raiviot$Kasvi<-NULL


#Aggregoidaan ettl ja etol luokkien mukaisesti

GTK_aggregointi_elop<-GTK_aggregointi_elop %>% group_by(ETOL,Tuotantosuuntaryhmä,ETTL,`ETTL Nimike`,`Yksi/monivuotinen`,`Cropland/grassland`) %>% summarise(Eloperaista_maata=sum(EloperäistäMaata))


GTK_aggregointi_elop_raiviot<-GTK_aggregointi_elop_raiviot %>% group_by(ETOL,Tuotantosuuntaryhmä,ETTL,`ETTL Nimike`,`Yksi/monivuotinen`,`Cropland/grassland`) %>% summarise(Eloperaista_maata=sum(EloperäistäMaata))


GTK_aggregointi_mineral<-GTK_aggregointi_mineral %>% group_by(ETOL,Tuotantosuuntaryhmä,ETTL,`ETTL Nimike`,`Yksi/monivuotinen`,`Cropland/grassland`) %>% summarise(Mineraalimaata=sum(Mineraalimaata))


GTK_aggregointi_mineral_raiviot<-GTK_aggregointi_mineral_raiviot %>% group_by(ETOL,Tuotantosuuntaryhmä,ETTL,`ETTL Nimike`,`Yksi/monivuotinen`,`Cropland/grassland`) %>% summarise(Mineraalimaata=sum(Mineraalimaata))


#Tarkista, että ala stemmaa. 
sum(GTK_aggregointi_elop$Eloperaista_maata)+sum(GTK_aggregointi_elop_raiviot$Eloperaista_maata)+sum(GTK_aggregointi_mineral$Mineraalimaata)+sum(GTK_aggregointi_mineral_raiviot$Mineraalimaata)


#Exportataan 

library(openxlsx)

Alat<-createWorkbook()
addWorksheet(Alat, "Eloperainen")
writeData(Alat, "Eloperainen", GTK_aggregointi_elop)
addWorksheet(Alat, "Mineraali")
writeData(Alat, "Mineraali", GTK_aggregointi_mineral)
addWorksheet(Alat, "Eloperainen_raivattu")
writeData(Alat, "Eloperainen_raivattu", GTK_aggregointi_elop_raiviot)
addWorksheet(Alat, "Mineraali_raivattu")
writeData(Alat, "Mineraali_raivattu", GTK_aggregointi_mineral_raiviot)

saveWorkbook(Alat, file=here("Output/AreaAggregates/Viljelyalat_gtk_ettl_etol_040625.xlsx"),overwrite = T)
