

library(varhandle);library(tidyverse);library(openxlsx)

#Pinta-ala-aggregaatti gtk-aineistosta, tarkkuustasona ETTL, ETOL, biodiversiteettiluokka. 

#Ajetaan pinta-aladata  ETTL ja ETOL tasolle. 
source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}

source_lines(here("Skriptit/Uudet skriptit/GTK_alojen_muunto_envimat_koodeille.R"),1:134)

#Viljelyalan tarkistus
sum(GTK_aggregointi_elop$EloperäistäMaata)+sum(GTK_aggregointi_elop_raiviot$EloperäistäMaata)+sum(GTK_aggregointi_mineral$Mineraalimaata)+sum(GTK_aggregointi_mineral_raiviot$Mineraalimaata)

rm.all.but(c("GTK_aggregointi_elop",
             "GTK_aggregointi_elop_raiviot",
             "GTK_aggregointi_mineral",
             "GTK_aggregointi_mineral_raiviot"))

#Liitetään biodiversiteetin kerroin

library(readxl)
Biodiversiteettiluokat_kasveille <- read_excel("Data/Biodiversiteettiluokat_kasveille.xlsx", 
                                               col_types = c("numeric", "text", "skip", 
                                                             "text"))
Biodiversiteettiluokat_kasveille$Kasvi<-NULL


nrow(inner_join(GTK_aggregointi_elop, Biodiversiteettiluokat_kasveille, by="Kasvikoodi"))
GTK_aggregointi_elop<-inner_join(GTK_aggregointi_elop, Biodiversiteettiluokat_kasveille, by="Kasvikoodi")

nrow(inner_join(GTK_aggregointi_elop_raiviot, Biodiversiteettiluokat_kasveille, by="Kasvikoodi"))
GTK_aggregointi_elop_raiviot<-inner_join(GTK_aggregointi_elop_raiviot, Biodiversiteettiluokat_kasveille, by="Kasvikoodi")


nrow(inner_join(GTK_aggregointi_mineral, Biodiversiteettiluokat_kasveille, by="Kasvikoodi"))
GTK_aggregointi_mineral<-inner_join(GTK_aggregointi_mineral, Biodiversiteettiluokat_kasveille, by="Kasvikoodi")

nrow(inner_join(GTK_aggregointi_mineral_raiviot, Biodiversiteettiluokat_kasveille, by="Kasvikoodi"))
GTK_aggregointi_mineral_raiviot<-inner_join(GTK_aggregointi_mineral_raiviot, Biodiversiteettiluokat_kasveille, by="Kasvikoodi")

sum(GTK_aggregointi_elop$EloperäistäMaata)+sum(GTK_aggregointi_elop_raiviot$EloperäistäMaata)+sum(GTK_aggregointi_mineral$Mineraalimaata)+sum(GTK_aggregointi_mineral_raiviot$Mineraalimaata)


#Aggregoidaan ettl ja etol luokkien mukaisesti

GTK_aggregointi_elop <-
  GTK_aggregointi_elop %>% group_by(
    ETOL,
    Tuotantosuuntaryhmä,
    ETTL,
    `ETTL Nimike`,
    `Yksi/monivuotinen`,
    `Cropland/grassland`,
    Soveltuva_biodiv_kerroin
  ) %>% summarise(Eloperaista_maata = sum(EloperäistäMaata))
sum(GTK_aggregointi_elop$Eloperaista_maata)


GTK_aggregointi_elop_raiviot<-GTK_aggregointi_elop_raiviot %>% group_by(ETOL,Tuotantosuuntaryhmä,ETTL,`ETTL Nimike`,`Yksi/monivuotinen`,`Cropland/grassland`,Soveltuva_biodiv_kerroin) %>% summarise(Eloperaista_maata=sum(EloperäistäMaata))
sum(GTK_aggregointi_elop_raiviot$Eloperaista_maata)

GTK_aggregointi_mineral<-GTK_aggregointi_mineral %>% group_by(ETOL,Tuotantosuuntaryhmä,ETTL,`ETTL Nimike`,`Yksi/monivuotinen`,`Cropland/grassland`,Soveltuva_biodiv_kerroin) %>% summarise(Mineraalimaata=sum(Mineraalimaata))
sum(GTK_aggregointi_mineral$Mineraalimaata)

GTK_aggregointi_mineral_raiviot<-GTK_aggregointi_mineral_raiviot %>% group_by(ETOL,Tuotantosuuntaryhmä,ETTL,`ETTL Nimike`,`Yksi/monivuotinen`,`Cropland/grassland`,Soveltuva_biodiv_kerroin) %>% summarise(Mineraalimaata=sum(Mineraalimaata))
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
    Soveltuva_biodiv_kerroin,
    `Cropland/grassland`,
    Eloperaista_maata
  )

GTK_aggregointi_elop_raiviot <- ungroup(GTK_aggregointi_elop_raiviot)
GTK_aggregointi_elop_raiviot <-
  GTK_aggregointi_elop_raiviot %>% select(
    Tuotantosuuntaryhmä,
    `ETTL Nimike`,
    `Yksi/monivuotinen`,
    Soveltuva_biodiv_kerroin,
    `Cropland/grassland`,
    Eloperaista_maata
  )

GTK_aggregointi_mineral <- ungroup(GTK_aggregointi_mineral)
GTK_aggregointi_mineral <-
  GTK_aggregointi_mineral %>% select(
    Tuotantosuuntaryhmä,
    `ETTL Nimike`,
    `Yksi/monivuotinen`,
    Soveltuva_biodiv_kerroin,
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
    Soveltuva_biodiv_kerroin,
    `Cropland/grassland`,
    Mineraalimaata
  )


sum(GTK_aggregointi_elop$Eloperaista_maata)+sum(GTK_aggregointi_elop_raiviot$Eloperaista_maata)+sum(GTK_aggregointi_mineral$Mineraalimaata)+sum(GTK_aggregointi_mineral_raiviot$Mineraalimaata)


#Leveään muotoon

GTK_aggregointi_elop <-
  GTK_aggregointi_elop %>% pivot_wider(names_from = `ETTL Nimike`,
                                       values_from = Eloperaista_maata,
                                       values_fill = 0)
GTK_aggregointi_elop_raiviot<-
  GTK_aggregointi_elop_raiviot %>% pivot_wider(names_from = `ETTL Nimike`,
                                               values_from = Eloperaista_maata,
                                               values_fill = 0)
GTK_aggregointi_mineral<-
  GTK_aggregointi_mineral %>% pivot_wider(names_from = `ETTL Nimike`,
                                          values_from = Mineraalimaata,
                                          values_fill = 0)
GTK_aggregointi_mineral_raiviot<-
  GTK_aggregointi_mineral_raiviot %>% pivot_wider(names_from = `ETTL Nimike`,
                                                  values_from = Mineraalimaata,
                                                  values_fill = 0)

#Viljelyalan tarkistud
sum(colSums(GTK_aggregointi_elop[5:length(GTK_aggregointi_elop)]))+
  sum(colSums(GTK_aggregointi_elop_raiviot[5:length(GTK_aggregointi_elop_raiviot)]))+
  sum(colSums(GTK_aggregointi_mineral[5:length(GTK_aggregointi_mineral)]))+
  sum(colSums(GTK_aggregointi_mineral_raiviot[5:length(GTK_aggregointi_mineral_raiviot)]))



Tuotevektori<-c(
  "Vehnä",
  "Maissi",
  "Mallasohra",
  "Rehuohra",
  "Ruis",
  "Kaura",
  "Muut viljat",
  "Rypsi ja rapsi",
  "Seesaminsiemenet",
  "Auringonkukansiemenet",
  "Öljyhamppu",
  "Öljypellava",
  "Ruokaperuna",
  "Varhaisperuna",
  "Sokerijuurikas",
  "Pensaspapu",
  "Tarhaherne",
  "Härkäpapu",
  "Ruokaherne",
  "Parsa",
  "Valko-eli keräkaali ja punakaali",
  "Kiinankaali",
  "Savoijinkaali (kurttukaali)",
  "Ruusukaali",
  "Kyssäkaali",
  "Kukkakaali",
  "Parsakaali",
  "Salaatti (Lactuca-suku)",
  "Ruukkusalaatit",
  "Sikurit ja endiivit",
  "Pinaatti",
  "Latva-artisokka",
  "Ruukkuyrtit",
  "Tilli (avomaa)",
  "Persilja (avomaa)",
  "Lehtiselleri",
  "Raparperi",
  "Sokerimaissi",
  "Vesimelonit",
  "Muut melonit",
  "Paprika (kasvihuonetuotanto)",
  "Kasvihuonekurkku",
  "Avomaankurkku",
  "Munakoisot",
  "Tomaatti",
  "Kurpitsa",
  "Kesäkurpitsa",
  "Porkkana",
  "Nauris",
  "Valkosipuli",
  "Kepa-eli ruokasipuli, salotti-, puna- ja jättisipuli sekä istukassipulit",
  "Purjo",
  "Lanttu",
  "Mukulaselleri",
  "Palsternakka",
  "Punajuurikas ja keltajuurikas",
  "Piparjuuri",
  "Juuripersilja",
  "Bataatit",
  "Kassava",
  "Maa-artisokka",
  "Kasvisten siemenet, ei kuitenkaan juurikkaansiemenet",
  "Sokerijuurikkaan siemenet",
  "Viljellyt sienet ja multasienet (tryffelit)",
  "Kasvikset, tuoreet, muualle luokittelemattomat",
  "Omenat",
  "Päärynät",
  "Kvittenit",
  "Aprikoosit",
  "Kirsikat",
  "Persikat",
  "Nektariinit",
  "Luumut",
  "Muut kota- ja kivihedelmät, muualle luokittelemattomat",
  "Kiivit",
  "Vadelmat",
  "Mansikat",
  "Mustaherukat",
  "Punaherukat",
  "Valkoherukat",
  "Karviaiset",
  "Pensasmustikat",
  "Muut marjat",
  "Muut maustekasvit, muut kuin jalostetut",
  "Rehukasvit",
  "Kaikki yhteensä"
)


for (name in Tuotevektori) {
  ifelse(is.na(GTK_aggregointi_elop[[name]]), GTK_aggregointi_elop <- cbind(GTK_aggregointi_elop, name = ""), GTK_aggregointi_elop <- cbind(GTK_aggregointi_elop, name))
}








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

saveWorkbook(Alat, file=here("Output/AreaAggregates/Viljelyalat_gtk_BDLuokka_0924.xlsx"))
