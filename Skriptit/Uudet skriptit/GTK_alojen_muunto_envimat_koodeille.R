

#GTK-viljelyaladatan muuntaminen envimat-koodeille (ETTL ja ETOL)

library(tidyverse);library(here);library(stringr)

#Ajetaan pinta-ala-aggregaatit
source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}

source_lines(here("Skriptit/Uudet skriptit/GTK_datan_tasokorjaus_ohitus.R"),1:35)

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


#Tarkistetaan tuotantosuuntien kirjoitusasu

#library(usefun)

#test<-length(outersect(GTKdata$Tuotantosuunta, Tuotantosuuntaryhmat$Tuotantosuunta))

#if(test!=0)
  #stop("Tuotantosuuntanimet eivät samat, kuin avaimessa")

#Yhdistetään tuotantosuuntaryhmän nimi tuotantosuuntanimen perusteella


GTK_aggregointi_elop<-inner_join(GTK_aggregointi_elop, Tuotantosuuntaryhmat, by="Tuotantosuunta")

GTK_aggregointi_elop_raiviot<-inner_join(GTK_aggregointi_elop_raiviot, Tuotantosuuntaryhmat, by="Tuotantosuunta")
  
GTK_aggregointi_mineral <- inner_join(GTK_aggregointi_mineral, Tuotantosuuntaryhmat, by="Tuotantosuunta")

GTK_aggregointi_mineral_raiviot<- inner_join(GTK_aggregointi_mineral_raiviot, Tuotantosuuntaryhmat, by="Tuotantosuunta")


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

#Aggregoidaan ettl ja etol luokkien mukaisesti

GTK_aggregointi_elop<-GTK_aggregointi_elop %>% group_by(ETOL,Tuotantosuuntaryhmä,ETTL,`ETTL Nimike`,`Yksi/monivuotinen`,`Cropland/grassland`) %>% summarise(Eloperaista_maata=sum(EloperäistäMaata))
sum(GTK_aggregointi_elop$Eloperaista_maata)

GTK_aggregointi_elop_raiviot<-GTK_aggregointi_elop_raiviot %>% group_by(ETOL,Tuotantosuuntaryhmä,ETTL,`ETTL Nimike`,`Yksi/monivuotinen`,`Cropland/grassland`) %>% summarise(Eloperaista_maata=sum(EloperäistäMaata))
sum(GTK_aggregointi_elop_raiviot$Eloperaista_maata)

GTK_aggregointi_mineral<-GTK_aggregointi_mineral %>% group_by(ETOL,Tuotantosuuntaryhmä,ETTL,`ETTL Nimike`,`Yksi/monivuotinen`,`Cropland/grassland`) %>% summarise(Mineraalimaata=sum(Mineraalimaata))
sum(GTK_aggregointi_mineral$Mineraalimaata)

GTK_aggregointi_mineral_raiviot<-GTK_aggregointi_mineral_raiviot %>% group_by(ETOL,Tuotantosuuntaryhmä,ETTL,`ETTL Nimike`,`Yksi/monivuotinen`,`Cropland/grassland`) %>% summarise(Mineraalimaata=sum(Mineraalimaata))
sum(GTK_aggregointi_mineral_raiviot$Mineraalimaata)

#Tarkista, että ala stemmaa. 
sum(GTK_aggregointi_elop$Eloperaista_maata)+sum(GTK_aggregointi_elop_raiviot$Eloperaista_maata)+sum(GTK_aggregointi_mineral$Mineraalimaata)+sum(GTK_aggregointi_mineral_raiviot$Mineraalimaata)

#Data matriisimaisen muotoiseksi, NA:n muutos nolliksi
#Tämä edellyttää koodien poistoa, jotta leveä muoto kääntyy oikein 
GTK_aggregointi_elop <- ungroup(GTK_aggregointi_elop)
GTK_aggregointi_elop <-
  GTK_aggregointi_elop %>% select(
    Tuotantosuuntaryhmä,
    `ETTL Nimike`,
    `Yksi/monivuotinen`,
    `Cropland/grassland`,
    Eloperaista_maata
  )

GTK_aggregointi_elop_raiviot <- ungroup(GTK_aggregointi_elop_raiviot)
GTK_aggregointi_elop_raiviot <-
  GTK_aggregointi_elop_raiviot %>% select(
    Tuotantosuuntaryhmä,
    `ETTL Nimike`,
    `Yksi/monivuotinen`,
    `Cropland/grassland`,
    Eloperaista_maata
  )

GTK_aggregointi_mineral <- ungroup(GTK_aggregointi_mineral)
GTK_aggregointi_mineral <-
  GTK_aggregointi_mineral %>% select(
    Tuotantosuuntaryhmä,
    `ETTL Nimike`,
    `Yksi/monivuotinen`,
    `Cropland/grassland`,
    Mineraalimaata
  )

GTK_aggregointi_mineral_raiviot <-
  ungroup(GTK_aggregointi_mineral_raiviot)
GTK_aggregointi_mineral_raiviot <-
  GTK_aggregointi_mineral_raiviot %>% select(
    Tuotantosuuntaryhmä,
    `ETTL Nimike`,
    `Yksi/monivuotinen`,
    `Cropland/grassland`,
    Mineraalimaata
  )

#Leveään muotoon

GTK_aggregointi_elop <-
  GTK_aggregointi_elop %>% pivot_wider(names_from = Tuotantosuuntaryhmä,
                                       values_from = Eloperaista_maata,
                                       values_fill = 0)
GTK_aggregointi_elop_raiviot<-
GTK_aggregointi_elop_raiviot %>% pivot_wider(names_from = Tuotantosuuntaryhmä,
                                             values_from = Eloperaista_maata,
                                             values_fill = 0)
GTK_aggregointi_mineral<-
GTK_aggregointi_mineral %>% pivot_wider(names_from = Tuotantosuuntaryhmä,
                                        values_from = Mineraalimaata,
                                        values_fill = 0)
GTK_aggregointi_mineral_raiviot<-
GTK_aggregointi_mineral_raiviot %>% pivot_wider(names_from = Tuotantosuuntaryhmä,
                                                values_from = Mineraalimaata,
                                                values_fill = 0)

#Viljelyalan tarkistud
sum(colSums(GTK_aggregointi_elop[4:length(GTK_aggregointi_elop)]))+
sum(colSums(GTK_aggregointi_elop_raiviot[4:length(GTK_aggregointi_elop_raiviot)]))+
sum(colSums(GTK_aggregointi_mineral[4:length(GTK_aggregointi_mineral)]))+
sum(colSums(GTK_aggregointi_mineral_raiviot[4:length(GTK_aggregointi_mineral_raiviot)]))


#Exportataan välitulokset

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

saveWorkbook(Alat, file=here("Output/AreaAggregates/Viljelyalat_gtk_0924.xlsx"))
