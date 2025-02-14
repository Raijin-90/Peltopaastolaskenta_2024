library(varhandle);library(openxlsx);library(here)

source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}

#Arvio lannoitteiden ravinteista kokonaismassana kasveittain
#Perustuu apetit-aineistosta laskettuihin kasveittaisiin keskiarvoisiin kg ravinnetta/ha intensiteetteihin 

library(readxl)
Apetit_lannoitus_ravinteet_eiluomua <- read_excel("Output/Ravinnedata/Apetit_lannoitus_ravinteet_eiluomua.xlsx")
Lannoitus<-Apetit_lannoitus_ravinteet_eiluomua
rm(Apetit_lannoitus_ravinteet_eiluomua)
gc()

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

rm(kk)
# Otetaan sisään kasvien ryhmittelyavain lannoitustarpeen mukaan
# Yhdistetään lannoitustarpeen määrittävä ryhmä-ID kasvikoodin perusteella pinta-aloihin

library(readxl)
Apetit_lannoitustarve_peltodata_avain <- read_excel("Data/Ravinnelaskennan_aineisto/Apetit_lannoitustarve_peltodata_avain.xlsx")
Apetit_lannoitustarve_peltodata_avain[4:length(Apetit_lannoitustarve_peltodata_avain)]<-NULL
Apetit_lannoitustarve_peltodata_avain$KASVINIMI<-NULL
colnames(Alat)[5]<-"KASVIKOODI"

if(nrow(merge(Alat, Apetit_lannoitustarve_peltodata_avain, by="KASVIKOODI")) != 1097531) {
  stop("VÄÄRÄ RIVIMÄÄRÄ, TARKISTA KOHDISTUSAVAIN")}

Alat<-merge(Alat, Apetit_lannoitustarve_peltodata_avain, by="KASVIKOODI")
colnames(Alat)[colnames(Alat)=="ID"]<-"Lannoitus_ID"


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
    Sopimuskasvi_nimi %in% c("MUKULASELLERI", "BF-MUKULASELLERI") ~ 11,
    .default = 0
  ))

#Ryhmien mukaiset keskiarvoiset ravinnetta lannoitteesta/hehtaari kertoimet lasketaan näille, ja kohdistetaan ID-tunnisteella peltolohkodatan kasveille. 


a<-Lannoitus %>% group_by(Lannoitus_ID) %>% summarise(Typpea_hehtaarille_keskiarvo = mean(N_kg_ha_keskiarvo),
                                                                                 Fosforia_hehtaarille_keskiarvo = mean(P_kg_ha_keskiarvo),
                                                                                 Kaliumia_hehtaarille_keskiarvo = mean(K_kg_ha_keskiarvo))

#Erikoistapaukset

#Ryhmä 12: Keskimääräinen vihannes, sisältäen 
#"LANTTU" ID 2
#"KERÄKAALI" ID 3 
#"PUNAJUURI" ID 4             
#"IP-PORKKANA KELTAINE"  #"PORKKANA" #"IP-VIIPALEPORKKANA"  #"BF-PORKKANA"  ID 6
#"PALSTERNAKKA" #"BF-PALSTERNAKKA" ID 7 
#KESÄKURPITSA, #IP-KESÄKURPITSA KELT ID 8 
#"KUKKAKAALI" ID 9
#"PINAATTI" ID 10 
#"MUKULASELLERI" #"BF-MUKULASELLERI" ID 11

averageVeggie<-Lannoitus %>% filter(Lannoitus_ID %in% c(2,3,4,seq(6,11,1))) %>% summarise(Typpea_hehtaarille_keskiarvo = mean(N_kg_ha_keskiarvo),
                                                                           Fosforia_hehtaarille_keskiarvo = mean(P_kg_ha_keskiarvo),
                                                                           Kaliumia_hehtaarille_keskiarvo = mean(K_kg_ha_keskiarvo)) %>% add_column(Lannoitus_ID = 12)

#Käytetään vihanneksille, joille datassa ei suoraa vastinetta

a<-rbind(a,averageVeggie)

#Ryhmä 9999: laitumet ym. joita ei lannoiteta, ravinnekerroin nolla. 

No_fert<-tibble(Typpea_hehtaarille_keskiarvo = 0,
                  Fosforia_hehtaarille_keskiarvo = 0,
                  Kaliumia_hehtaarille_keskiarvo =0,
                  Lannoitus_ID = 9999) 

a<-rbind(a, No_fert)
rm(No_fert)

#ryhmä 0: kasvit joille ei Apetit-datassa vastinetta. Lasketaan lannoitus keskiarvottamalla yli kasvityyppien. 
#Nollaryhmä: keskiarvotetaan lannoitusdatan kasvit erittelemättä niiden tyyppiä

b <- Lannoitus %>% summarise(
  Typpea_hehtaarille_keskiarvo = mean(N_kg_ha_keskiarvo),
  Fosforia_hehtaarille_keskiarvo = mean(P_kg_ha_keskiarvo),
  Kaliumia_hehtaarille_keskiarvo = mean(K_kg_ha_keskiarvo)
) %>% mutate (Lannoitus_ID = 0)

Lannoiteravinne_kertoimet<-rbind(a,b)
rm(a,b, Lannoitus)

#Yhdistetään kertoimet ja kasvin viljelyala

Data<- inner_join(Alat, Lannoiteravinne_kertoimet, by="Lannoitus_ID")

rm.all.but("Data")

#Lasketaan lohkoille annostellut ravinnemäärät gtk-peltodatasta

Data<-Data %>% mutate(Typpea_kg = Typpea_hehtaarille_keskiarvo*Maannossumma,
                Fosforia_kg = Fosforia_hehtaarille_keskiarvo*Maannossumma,
                Kaliumia_kg = Kaliumia_hehtaarille_keskiarvo*Maannossumma) 

Aggregointi_kasvit<-Data %>% group_by(Tuotantosuunta, KASVIKOODI,KASVINIMI_reclass) %>% summarise_at(c("Typpea_kg","Fosforia_kg","Kaliumia_kg"),sum) 
colnames(Aggregointi_kasvit)[2]<-"Kasvikoodi"

#Kategoriatiedot lisäaggregointeihin
library(readxl)
Kasvikategoriat_avain <- read_excel("Data/Kasvikategoriat_avain.xlsx")

Aggregointi_kasvit<-inner_join(Aggregointi_kasvit, Kasvikategoriat_avain, by="Kasvikoodi") %>% mutate(Kasvi = NULL)



#ETOL-ryhmät ja ETTL-ryhmät

library(readxl)
ETOL <- read_excel("Data/Muuntoavain_tuotantosuunnat_tuotteet_ETOL.xlsx", 
                                                        sheet = "Tuotantosuunnat ryhmittäin")
colnames(ETOL)[colnames(ETOL)=="Tuotantosuunta_original"]<-"Tuotantosuunta"
#Välilyönnit pois tuotantosuuntanimistä
Aggregointi_kasvit<-Aggregointi_kasvit %>% mutate(Tuotantosuunta = case_when(Tuotantosuunta == "Lammas- ja vuohitilat" ~ "Lammas_ja_vuohitilat",
                                  Tuotantosuunta == "Muut nautakarjatilat" ~ "Muut_nautakarjatilat",
                                  Tuotantosuunta == "Nurmet, laitumet, hakamaat" ~ "Nurmet_laitumet_hakamaat",
                                  Tuotantosuunta == "Palkokasvit pl. tarhaherne" ~ "Palkokasvit_pl_tarhaherne",
                                  Tuotantosuunta == "Rypsi ja rapsi" ~ "Rypsi_rapsi",
                                  Tuotantosuunta == "Tattari ja kinoa" ~ "Tattari_kinoa",
                                  Tuotantosuunta == "Vihannekset ja juurekset" ~ "Vihannekset_juurekset",
                                  Tuotantosuunta == "Viljat pl. ohra" ~ "Viljat_pl_ohra",
                                  .default = Tuotantosuunta))

Aggregointi_kasvit<-inner_join(Aggregointi_kasvit, ETOL, by = "Tuotantosuunta")


ETTL <- read_excel("Data/Muuntoavain_tuotantosuunnat_tuotteet_ETOL.xlsx", 
                   sheet = "Kasvit_ETTL_koodeittain")
colnames(ETTL)[colnames(ETTL)=="Ruokaviraston koodi"]<-"Kasvikoodi"
ETTL$`Ruokaviraston nimi`<-NULL

Aggregointi_kasvit<-inner_join(Aggregointi_kasvit, ETTL, by="Kasvikoodi")

#ETTL-ETOL aggregointi

Aggregointi_ETOL_ETTL<-Aggregointi_kasvit %>% group_by(ETOL, ETTL, `ETTL Nimike`) %>% summarise_at(c("Typpea_kg", "Fosforia_kg", "Kaliumia_kg"), sum)


library(openxlsx)

Output<- createWorkbook()
addWorksheet(Output, "Kasvit")
addWorksheet(Output, "ETOL_ETTL")
writeData(Output, "Kasvit", Aggregointi_kasvit)
writeData(Output, "ETOL_ETTL", Aggregointi_ETOL_ETTL)


saveWorkbook(Output,here("Output/Ravinnedata/Lannoiteravinteet_tarkennettu_gtkdata_0225.xlsx"), overwrite = T )


