library(varhandle);library(openxlsx);library(here)

source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}

#Arvio lannoitteiden ravinteista kokonaismassana kasveittain
#Perustuu apetit-aineistosta laskettuihin kasveittaisiin keskiarvoisiin kg ravinnetta/ha intensiteetteihin 

library(readxl)
Apetit_lannoitus_ravinteet_eiluomua <- read_excel("Output/Ravinnedata/Apetit_lannoitus_ravinteet_eiluomua.xlsx")
Lannoitus<-Apetit_lannoitus_ravinteet_eiluomua

#Pinta-aladatan luonti: 
source_lines(here("Skriptit/Uudet skriptit/GTK_datan_aggregointi.R"),1:264)

Alat<-GTKdata %>% select(1:3,9,5,KASVINIMI_reclass,30:32)


#Vastaavatko kasvikoodit aloissa ja lannoitusdatassa toisiaan

kk1<-Alat %>% select(KASVIKOODI_lohkodata_reclass,KASVINIMI_reclass)
kk1<-unique(kk1)

kk2<-Lannoitus %>% select(Sopimuskasvi,Sopimuskasvi_nimi)
kk2<-unique(kk2)
#Vain muutama sama..vaatii taas regexiä...
filter(kk1, kk1$KASVIKOODI_lohkodata_reclass %in% kk2$Sopimuskasvi)

rm.all.but(c("Lannoitus", "Alat"))

Lannoitusdatan_kasvit<-unique(Lannoitus$Sopimuskasvi_nimi)


kk<-unique(Alat$KASVINIMI_reclass)
kk<-as.data.frame(kk)
write.xlsx(kk, file="kasvikoodit.xlsx", overwrite = T)

#Yksitulkintaiset tapaukset: 
#Hernetyyppejä datassa: ruoka-, rehu- ja tarhaherne. 
#Näille lannoituksen ravinteet kertoimesta "Pakasteherne", ainoa herne datassa. 
#Annetaan näille datan ja lannoitusaineiston riveille koodiksi 1
#Lanttu: 1 koodi, samalla nimellä. Koodiksi 2
#keräkaali: 1 koodi nimellä "valko- eli keräkaali". Koodiksi 3

#Muut:
#Punajuuri datassa nimellä "punajuurikas ja keltajuurikas". Koodiksi 4
#Punajuuren lannoitsmäärälle 2 koodia, "PUNAJUURI", BF-PUNAJUURI PYÖREÄ". Lasketan näistä keskiarvo. 

#Perunat: useita luokkia datassa. Koodiksi 5.  
#Tärkkelysperuna
#"Tärkkelysperunan oma siemenlisäys
#Ruokaperuna
#Siemenperuna (sertifioidun siemenen tuotantoon)"
#Lannoitusmäärälle useita kertoimia "BF-PERUNA"            "IP-KESÄPERUNA"        "PERUNA"  

#Samoin porkkana. Koodiksi 6 
#Useita lannoituskoodeja: "IP-PORKKANA KELTAINE" "PORKKANA" "IP-VIIPALEPORKKANA"  "BF-PORKKANA"

#ja palsternakka. Koodiksi 7
#Useita lannoituskoodeja: "PALSTERNAKKA" "BF-PALSTERNAKKA"

#Kaikille muille koodiksi nolla. 

#Tällä koodauksella yhdistetään lannoitekerroin aineistoon. 

Alat <- Alat %>% mutate(
  Lannoitus_ID = case_when(
    KASVINIMI_reclass %in% c("Ruokaherne", 
                             "Rehuherne", 
                             "Tarhaherne") ~ 1,
    KASVINIMI_reclass %in% c("Lanttu") ~ 2,
    KASVINIMI_reclass %in% c("Valko- eli keräkaali") ~ 3,
    KASVINIMI_reclass %in% c("Punajuurikas ja keltajuurikas") ~ 4,
    KASVINIMI_reclass %in% c(
      "Tärkkelysperuna",
      "Tärkkelysperunan oma siemenlisäys",
      "Ruokaperuna",
      "Siemenperuna (sertifioidun siemenen tuotantoon)",
      "Ruokateollisuusperuna"
    ) ~ 5,
    KASVINIMI_reclass %in% c("Porkkana") ~ 6,
    KASVINIMI_reclass %in% c("Palsternakka") ~ 7,
    KASVINIMI_reclass %in% c("Kesäkurpitsa") ~ 8,
    KASVINIMI_reclass %in% c("Kukkakaali") ~ 9,
    KASVINIMI_reclass %in% c("Pinaatti") ~ 10,
    KASVINIMI_reclass %in% c("Mukulaselleri") ~ 11,
    .default = 0
  ))
  
#Lannoitusdataan vastaava koodaus. 

Lannoitus <- Lannoitus %>% 
  mutate(
  Lannoitus_ID = case_when(
    Sopimuskasvi_nimi %in% c("PAKASTEHERNE") ~ 1,
    Sopimuskasvi_nimi %in% c("LANTTU") ~ 2,
    Sopimuskasvi_nimi %in% c("KERÄKAALI") ~ 3,
    Sopimuskasvi_nimi %in% c("PUNAJUURI","BF-PUNAJUURI PYÖREÄ") ~ 4,
    Sopimuskasvi_nimi %in% c(
      "BF-PERUNA","IP-KESÄPERUNA","PERUNA")  ~ 5,
    Sopimuskasvi_nimi %in% c("IP-PORKKANA KELTAINE","PORKKANA","IP-VIIPALEPORKKANA","BF-PORKKANA") ~ 6,
    Sopimuskasvi_nimi %in% c("PALSTERNAKKA","BF-PALSTERNAKKA") ~ 7,
    Sopimuskasvi_nimi %in% c("KESÄKURPITSA", "IP-KESÄKURPITSA KELT") ~ 8,
    Sopimuskasvi_nimi %in% c("KUKKAKAALI") ~ 9,
    Sopimuskasvi_nimi %in% c("PINAATTI") ~ 10,
    Sopimuskasvi_nimi %in% c("MUKULASELLERI") ~ 11,
    .default = 0
  ))

#Ryhmien mukaiset keskiarvoiset ravinnetta lannoitteesta/hehtaari kertoimet näille. 
#Ryhmäksi 0 merkitään Default-kasvit, eli kaikki sellaiset peltodatan asiat joille ei omaa lannoituskerrointa. Keskiarvotus ryhmiä huomioimatta


a<-Lannoitus %>% filter(Lannoitus_ID != 0) %>% group_by(Lannoitus_ID) %>% summarise(Typpea_hehtaarille_keskiarvo = mean(N_kg_ha_keskiarvo),
                                                                                 Fosforia_hehtaarille_keskiarvo = mean(P_kg_ha_keskiarvo),
                                                                                 Kaliumia_hehtaarille_keskiarvo = mean(K_kg_ha_keskiarvo))
#Nollaryhmä: keskiarvotetaan lannoitusdatan kasvit erittelemättä niiden tyyppiä

b <- Lannoitus %>% summarise(
  Typpea_hehtaarille_keskiarvo = mean(N_kg_ha_keskiarvo),
  Fosforia_hehtaarille_keskiarvo = mean(P_kg_ha_keskiarvo),
  Kaliumia_hehtaarille_keskiarvo = mean(K_kg_ha_keskiarvo)
) %>% mutate (Lannoitus_ID = 0)

Lannoiteravinne_kertoimet<-rbind(a,b)
rm(a,b,kk, Lannoitus)

#Yhdistetään kertoimet ja kasvin viljelyala

Data<- inner_join(Alat, Lannoiteravinne_kertoimet, by="Lannoitus_ID")

rm.all.but("Data")

#Lasketaan lohkoille annostellut ravinnemäärät gtk-peltodatasta

Data<-Data %>% mutate(Typpea_kg = Typpea_hehtaarille_keskiarvo*Maannossumma,
                Fosforia_kg = Fosforia_hehtaarille_keskiarvo*Maannossumma,
                Kaliumia_kg = Kaliumia_hehtaarille_keskiarvo*Maannossumma) 

Aggregointi_kasvit<-Data %>% group_by(Tuotantosuunta, KASVIKOODI_lohkodata_reclass,KASVINIMI_reclass) %>% summarise_at(c("Typpea_kg","Fosforia_kg","Kaliumia_kg"),sum) 
colnames(Aggregointi_kasvit)[2]<-"Kasvikoodi"

#Kategoriatiedot lisäaggregointeihin
library(readxl)
Kasvikategoriat_avain <- read_excel("Data/Kasvikategoriat_avain.xlsx")

Aggregointi_kasvit<-inner_join(Aggregointi_kasvit, Kasvikategoriat_avain, by="Kasvikoodi") %>% mutate(Kasvi = NULL)

Kesannot<-filter(Aggregointi_kasvit, Tuoteryhmä == "Kesannot,laitumet,yms.")
Kasvit <-filter(Aggregointi_kasvit, Tuoteryhmä != "Kesannot,laitumet,yms.") 

library(openxlsx)

Output<- createWorkbook()
addWorksheet(Output, "Kasvit_ei_kesantoja")
writeData(Output, "Kasvit_ei_kesantoja", Kasvit)
addWorksheet(Output, "Kesannot")
writeData(Output, "Kesannot", Kesannot)

saveWorkbook(Output,here("Output/Ravinnedata/Lannoiteravinteet_gtkdata.xlsx"))
