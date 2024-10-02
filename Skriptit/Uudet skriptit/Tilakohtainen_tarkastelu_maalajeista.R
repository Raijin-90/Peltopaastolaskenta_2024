#Lasketaan tiloittain GTK-datan pohjalta maalajisuhde sen arvioimiseksi, mitkä tilat ovat erityisen painottuneita org. maalle ja tarvitsevat sitä maata toimintaansa. 

#Sisältää sellaiset tilat joiden: 
#Peltolohkoille voidaan määrittää maalaji,
#Ja joilla ylipäänsä on 2017 kasvulohkodatasta löytyviä polygoneja, ts. jotka tekivät tuolloin kasvinviljelyä rekisteriin sisältyvällä tavalla. 

# Tiloja yhteensä 51 764
# Originaalilistassa 50 588

51764-50588 #vajetta 1176

#Pellottomia tiloja 1173

51764-50588-1173 #vajetta 3

#Kasvulohkot2017 paikkatietodatassa on lähtökohtaisesti (ennen käsittelyjä) 50 588 tilaa, joka on vähemmän kuin arvioitu kokonaismäärä. Pellottomat tilat eivät näy datassa. 

library(tidyverse);library(here);library(usefun) 

#Ajetaan pinta-ala-aggregaatit
source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}

source_lines(here("Skriptit/Uudet skriptit/GTK_datan_aggregointi.R"), 1:260)

library(varhandle)
rm.all.but("GTKdata")

#Leikataan datasta tarvittavat muuttujat

Tiladata<-GTKdata %>% select(MAATILA_TUNNUS,
                             Tuotantosuunta,
                   KASVIKOODI_lohkodata_reclass,
                   KASVINIMI_reclass,
                   Maannossumma,
                   Eloperaista,
                   Mineraalia)

#Tuotantosuuntaryhmä-taso ####

library(readxl)
Tuotantosuuntaryhmat <-
  read_excel(
    here("Data","Muuntoavain_tuotantosuunnat_tuotteet_ETOL.xlsx"),
    sheet = "Tuotantosuunnat ryhmittäin",
    col_types = c("text",
                  "text",
                  "skip"
    )
  )
colnames(Tuotantosuuntaryhmat) <-
  c("Tuotantosuunta", "Tuotantosuuntaryhmä","ETOL_koodi")

#Tuotantosuunnat samaan kirjoitusasuun
Tiladata<-Tiladata %>% mutate(Tuotantosuunta = case_when(Tuotantosuunta == "Lammas- ja vuohitilat" ~ "Lammas_ja_vuohitilat",
                                  Tuotantosuunta == "Muut nautakarjatilat" ~ "Muut_nautakarjatilat",
                                  Tuotantosuunta == "Nurmet, laitumet, hakamaat" ~ "Nurmet_laitumet_hakamaat",
                                  Tuotantosuunta == "Palkokasvit pl. tarhaherne" ~ "Palkokasvit_pl_tarhaherne",
                                  Tuotantosuunta == "Rypsi ja rapsi" ~ "Rypsi_rapsi",
                                  Tuotantosuunta == "Tattari ja kinoa" ~ "Tattari_kinoa",
                                  Tuotantosuunta == "Vihannekset ja juurekset" ~ "Vihannekset_juurekset",
                                  Tuotantosuunta == "Viljat pl. ohra" ~ "Viljat_pl_ohra",
                                  .default = Tuotantosuunta))

outersect(unique(Tiladata$Tuotantosuunta),unique(Tuotantosuuntaryhmat$Tuotantosuunta))

nrow(inner_join(Tiladata, Tuotantosuuntaryhmat, by="Tuotantosuunta"))
Tiladata<-inner_join(Tiladata, Tuotantosuuntaryhmat, by="Tuotantosuunta")

#Tuoteryhmät

library(readxl)
Kasvikategoriat_avain <- read_excel(here("Data","Kasvikategoriat_avain.xlsx"), 
                                    col_types = c("text", "text", "text", 
                                                  "numeric", "skip"))

colnames(Tiladata)[colnames(Tiladata)=="KASVIKOODI_lohkodata_reclass"]<-"Kasvikoodi"
Tiladata$Kasvikoodi<-as.character(Tiladata$Kasvikoodi)
Kasvikategoriat_avain$Kasvikoodi<-as.character(Kasvikategoriat_avain$Kasvikoodi)

a<-filter(Tiladata, !(Kasvikoodi %in% Kasvikategoriat_avain$Kasvikoodi))

nrow(inner_join(Tiladata, Kasvikategoriat_avain, by="Kasvikoodi"))
Tiladata<-inner_join(Tiladata, Kasvikategoriat_avain, by="Kasvikoodi")

#Aggregoidaan tuotantosuunta- ja tuoteryhmätasolla, maalajit huomioiden



Tiladata<-Tiladata %>% group_by(MAATILA_TUNNUS, Tuotantosuuntaryhmä) %>% summarise(
  Eloperaista = sum(Eloperaista),
  Mineraalia = sum(Mineraalia),
  Yhteensa = Eloperaista + Mineraalia,
  Elop_pros = 100*Eloperaista/Yhteensa,
  Min_pros = 100*Mineraalia/Yhteensa) 

Tiladata$Elop_pros<-round(Tiladata$Elop_pros, 0)
Tiladata$Min_pros<-round(Tiladata$Min_pros, 0)

#Jaetaan tilatasolla desiileihin, perusteena tilan lohkojen eloperäisellä maalla sijaitseva osuus

Tiladata<-Tiladata %>% mutate(Turveprosentti_luokitus = case_when(Elop_pros >= 0 & Elop_pros <= 10 ~ "0-10",
                                                                  Elop_pros  >=  11 & Elop_pros <= 20 ~ ">10-20",
                                                                  Elop_pros  >=  21 & Elop_pros <= 30 ~ ">20-30",
                                                                  Elop_pros  >=  31 & Elop_pros <= 40 ~ ">30-40",
                                                                  Elop_pros  >=  41 & Elop_pros <= 50 ~ ">40-50",
                                                                  Elop_pros  >=  51 & Elop_pros <= 60 ~ ">50-60",
                                                                  Elop_pros  >=  61 & Elop_pros <= 70 ~ ">60-70",
                                                                  Elop_pros  >=  71 & Elop_pros <= 80 ~ ">70-80",
                                                                  Elop_pros  >=  81 & Elop_pros <= 90 ~ ">80-90",
                                                                  Elop_pros  >=  91 & Elop_pros <= 100 ~ ">90-100"))


Tiladata$Turveprosentti_luokitus[Tiladata$MAATILA_TUNNUS == "975032175"]<-">75-100"
                             
#Lisätään tilojen lukumäärälaskuri

Tiladata$Laskuri<-1

#Aggregoidaan data tuotantosuunnan ja turveprosentin mukaan. Mahdollistaa niiden tilatyyppien erottamisen, 

Tiladatan_tallennus<-createWorkbook()
addWorksheet(Tiladatan_tallennus, "Raakadata")
writeData(Tiladatan_tallennus, "Raakadata", Tiladata) #Koska raakadatan luokittaminen vie paljon aikaa per ajo, tallennetaan välituloskin.
saveWorkbook(Tiladatan_tallennus,here("Output/AreaAggregates/Tilatason_tarkastelu/Tiladatan_tallennus.xlsx"))

rm.all.but("Tiladata")

#DESIILITARKASTELU
#TUOTANTOSUUNNITTAIN JA ILMAN

#Desiilit tuotantosuunta huomioituna 

Desiilit_ts <-
  Tiladata %>% group_by(Tuotantosuuntaryhmä, Turveprosentti_luokitus)  %>% summarise(Farm_count = sum(Laskuri))

#Ja ilman sitä

Desiilit_kaikkiaan <-
  Tiladata %>% group_by(Turveprosentti_luokitus)  %>% summarise(Farm_count = sum(Laskuri))


#Grafiikkapohja

gt(Min_Org, groupname_col = NULL) %>%
  cols_label(Turveprosentti_luokitus_gtk = "% of organic soils, geospatial data",
             lohkolukum_min = "Mineral parcels, count",
             Mineraalipros = "%",
             lohkolukum_elop="Organic parcels count",
             Turvepros = "%",
             lohkoja_yht = "Total parcel count") %>%
  grand_summary_rows(columns = c(2,4,6),
                     fns= Total ~ sum(.),
                     fmt = ~ fmt_number(., decimals = 0, sep_mark =" ")) %>%
  fmt_number(sep_mark = " ",
             decimals = 0,
             columns = c(2,4,6)) %>%
  fmt_number(decimals = 1,
             columns = c(3,5)) %>%
  gtsave(filename="Aineistojen_maalajivertailu.docx", path = here("Output/Yksinkertaistettu_intensiteettilaskenta"))
