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


Tiladata$Turveprosentti_luokitus[Tiladata$MAATILA_TUNNUS == "975032175"]<-
                             
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

library(tidyverse);library(openxlsx);library(readxl);library(here)
Tiladata <- read_excel("Output/AreaAggregates/Tilatason_tarkastelu/Tiladatan_tallennus.xlsx")


#ETOL nimien englanninnos
Tiladta<a-Tiladata %>% mutate(Tuotantosuuntaryhmä = case_when(Tuotantosuuntaryhmä == "Hedelmien viljely" ~ "Fruits",
                                                                      Tuotantosuuntaryhmä == "Hevostilat" ~ "Horse farms",
                                                                      Tuotantosuuntaryhmä == "Hunajatuotanto, muu eläimen hoito" ~ "Honey production",
                                                                      Tuotantosuuntaryhmä == "Kauran, ohran, rukiin ja vehnän viljely" ~ "Oat, barley, rye and wheat",
                                                                      Tuotantosuuntaryhmä == "Kuminan ja muiden maustekasvien viljely" ~ "Cumin & other spice crops",
                                                                      Tuotantosuuntaryhmä == "Lammas- ja vuohitilat" ~ "Sheep & goat farms",
                                                                      Tuotantosuuntaryhmä == "Maitotilat" ~ "Dairy farms",
                                                                      Tuotantosuuntaryhmä == "Mallasohran viljely" ~ "Malt barley",
                                                                      Tuotantosuuntaryhmä == "Marjojen viljely" ~ "Berries",
                                                                      Tuotantosuuntaryhmä == "Muiden vihannesten ja juuresten viljely" ~ "Other vegetables & root vegetables",
                                                                      Tuotantosuuntaryhmä =="Munatilat" ~ "Egg-laying poultry",
                                                                      Tuotantosuuntaryhmä =="Muut nautakarjatilat" ~ "Other cattle farms",
                                                                      Tuotantosuuntaryhmä == "Peltoherneen ja härkäpavun viljely" ~ "Field pea & faba bean",
                                                                      Tuotantosuuntaryhmä == "Perunan viljely" ~ "Potato",
                                                                      Tuotantosuuntaryhmä == "Rehukasvien viljely" ~ "Fodder crops & pastures",
                                                                      Tuotantosuuntaryhmä == "Rypsin ja rapsin viljely" ~ "Rapeseed & Turnip rape",
                                                                      Tuotantosuuntaryhmä == "Siipikarjatilat" ~ "Poultry",
                                                                      Tuotantosuuntaryhmä == "Sikatilat" ~ "Pig farms",
                                                                      Tuotantosuuntaryhmä == "Sokerijuurikkaan viljely" ~ "Sugar beet",
                                                                      Tuotantosuuntaryhmä == "Tarhaherneen viljely" ~ "Garden pea",
                                                                      Tuotantosuuntaryhmä == "Tattarin ja kinoan viljely" ~ "Buckwheat & Quinoa",
                                                                      Tuotantosuuntaryhmä == "Turkistarhaus" ~ "Fur farms",
                                                                      Tuotantosuuntaryhmä == "Yrttien viljely avomaalla" ~ "Open-ground herbs",
                                                                      Tuotantosuuntaryhmä == "Öljyhampun ja -pellavan viljely" ~ "Oilseed hemp and -flax",
                                                                      Tuotantosuuntaryhmä == "Total" ~ "Total"))




#Desiilit tuotantosuunta huomioituna 

Desiilit_ts <-
  Tiladata %>% group_by(Tuotantosuuntaryhmä, Turveprosentti_luokitus)  %>% summarise(Farm_count = sum(Laskuri))


Desiilit_ts_levea<-Desiilit_ts %>% pivot_wider(
   values_from = Farm_count, names_from =Tuotantosuuntaryhmä , values_fill = 0
)





write.xlsx(Desiilit_ts, file="Desiilit_tuotantosuunnittain.xlsx")
write.xlsx(Desiilit_ts_levea, file="Desiilit_tuotantosuunnittain_levea.xlsx")

library(openxlsx)
#Piirtämistä toisella ohjelmalla varten nämä datat ositetaan ja siirretään exceliksi. 1 taulu per tuotantosuunta
#Looppiin tulee yhtä monta askelta kuin tuotantosuuntia

Tuotantosuunnat<-unique(Desiilit_ts$Tuotantosuuntaryhmä)

for (i in seq_along(Tuotantosuunnat)) {

farm_type<-Tuotantosuunnat[i] #Tästä muodostetaan erittelevä tiedostonimi pian. 

#Filtteröi 
Desiilit_ts %>% filter(Tuotantosuuntaryhmä == Tuotantosuunnat[i]) %>% write.csv(file=paste0(Tuotantosuunnat[i],".csv") )

  } 




#Ja ilman sitä

Desiilit_kaikkiaan <-
  Tiladata %>% group_by(Turveprosentti_luokitus)  %>% summarise(Farm_count = sum(Laskuri))

write.xlsx(Desiilit_ts, file=here("Output/AreaAggregates/Tilatason_tarkastelu/Desiilit_tuotantosuunta.xls"),overwrite = T)
write.xlsx(Desiilit_kaikkiaan, file=here("Output/AreaAggregates/Tilatason_tarkastelu/Desiilit_kaikkiaan.xls"),overwrite = T)


write.xls


 
#Grafiikkapohja: kaikki tilat, desiilijakauma ilman tuotantosuuntaerottelua

Sorttivektori<-c("0-10",
">10-20",
">20-30",
">30-40",
 ">40-50",
">50-60",
">60-70",
">70-80",
">80-90",
">90-100")


library(viridis);library(here)
a<-Desiilit_kaikkiaan %>%  ggplot(aes(x =factor(Turveprosentti_luokitus, Sorttivektori), y =Farm_count, fill = Turveprosentti_luokitus)) +  theme_bw()+
  geom_bar(stat = "identity") +
  scale_fill_grey(name="Organic soils on farms, %") +
  xlab("Classification of farms by % of organic soils")+
  ylab("Farm count")+geom_text(aes(label = Farm_count), vjust = -0.5, hjust=0.5) 
  ggsave(filename = "Tilat_org_pros_desiileittäin.tiff", device = "tiff", dpi=1200, path= here("Output/Grafiikka/"))


#Desiilikuvien luuppaus.

library(here)    
Tilat<-unique(Desiilit_ts$Tuotantosuuntaryhmä)  
  
for (i in seq_along(Tilat)) {

    # Extract the current farm type 
  farm_type <- Tilat[i] 

  # Create the plot
  p <- ggplot(data = filter(Desiilit_ts, Tuotantosuuntaryhmä == Tilat[i]), aes(x = factor(Turveprosentti_luokitus, Sorttivektori), y = Farm_count)) + 
    geom_bar(stat = "identity") + 
    theme_bw() + 
    labs(title = farm_type,
         x="Classification of farms by % of organic soils",
         y="Farm count")

  # Save the plot as a PDF file
  ggsave(filename = paste0("plot_", farm_type, ".tiff"),device = "tiff", dpi=1200, path= here("Output/Grafiikka/"))
  print(p)
  dev.off()
  
}  
  
  
#Kumulatiivinen kertymäkuva

#Kertymäkuva ilman tuotantosuuntia

Kertymä<-Tiladata
Kertymä<-Kertymä %>% select(1,2,3)
Kertymä<-Kertymä %>% arrange(desc(Eloperaista))
Kertymä<-Kertymä %>% mutate(Laskuri =seq(1, nrow(Kertymä), 1))

Kertymä<-Kertymä %>% mutate(Pros=100*Eloperaista/sum(Eloperaista))

b<-Kertymä %>% ggplot(aes(x = Laskuri, y = cumsum(Pros))) +  
  theme_bw() + 
  geom_line()+
  xlab("Farm count")+
  ylab("Organic soil %, cumulative sum")

#Tuotantosuuntajaon kanssa
Kertyma_ts<-Tiladata
Kertyma_ts<-Kertyma_ts %>% 
  group_by(Tuotantosuuntaryhmä) %>%
  arrange(Tuotantosuuntaryhmä,desc(Eloperaista)) %>% 
  mutate(Kumul_pros = 100*Eloperaista/sum(Eloperaista)) %>%
  mutate(Juokseva_numerointi = row_number(Tuotantosuuntaryhmä))
  
  
  


