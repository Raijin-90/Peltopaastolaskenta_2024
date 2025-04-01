library(openxlsx);library(here);library(varhandle);library(stringr)

#' Source specific lines in an R file
#'
#' @param file character string with the path to the file to source.
#' @param lines numeric vector of lines to source in \code{file}.

source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}

#GTK-viljelyaladatan prosessointi
source_lines(here("Skriptit/Uudet skriptit/GTK_datan_aggregointi.R"), 1:260)


#Liikevaihtodata allokoinnin perusteeksi, sekä kuntakoodit.
#Lohkojen kunta on peruslokotunnuksen 3 ensimmäistä nroa. 

library(readxl)
Turkistarhaus_kunnittain_2015 <- read_excel("Data/Ravinnelaskennan_aineisto/Turkistarhaus_kunnittain_2015.xlsx", 
                                            sheet = "2017")
Turkistarhaus_kunnittain_2015<-Turkistarhaus_kunnittain_2015 %>% slice(-77:-79)


rm.all.but(c("GTKdata","Turkistarhaus_kunnittain_2015"))


#Peltolohkokoodeista 3 ekaa nroa eli kunnan koodi irti 

GTKdata$Kuntakoodi<-substr(GTKdata$PLTUNNUS, start = 1, stop = 3)

#Irrotetaan liikevaihtodatasta turkistarhausta sisältävien kuntien koodit. 

Turkiskunnat<-unique(Turkistarhaus_kunnittain_2015$Kuntakoodi)

Turkiskuntien_pellot<- GTKdata %>% filter(Kuntakoodi %in% Turkiskunnat)

Turkispellot_aggre <- Turkiskuntien_pellot %>% group_by(Kuntakoodi) %>% summarise_at(c("Maannossumma", "Eloperaista", "Mineraalia"),sum)

Liikevaihto_osuus <- Turkistarhaus_kunnittain_2015 %>% select(Kuntakoodi,`Osuus liikevaihdosta`)

#Datasta puuttuvat turkiskunnat

Ei_datassa<-filter(Turkistarhaus_kunnittain_2015, !(Kuntakoodi %in% Turkispellot_aggre$Kuntakoodi))

#Liikevaihto-osuus kunnittaiseen peltoalasummaan:

Turkispellot_aggre_liikevaihto<-left_join(Turkispellot_aggre, Liikevaihto_osuus,by="Kuntakoodi")
sum(Turkispellot_aggre_liikevaihto$`Osuus liikevaihdosta`)



Turkisdata<-createWorkbook()
addWorksheet(Turkisdata,"Turkistilojen_pellot_kunta")
writeData(Turkisdata,"Turkistilojen_pellot_kunta",Turkispellot_aggre_liikevaihto)
saveWorkbook(Turkisdata, here("Output/Ravinnedata/Turkistilojen_pellot_kunnat_liikevaihto.xlsx"),overwrite = T)

# Tarkennetaan:

#Yllä on jaettu turkislannan ravinnemassat kunnille liikevaihdon mukaan . 
#Otetaan näistä "keskittymiksi" x:llä merkityt kunnat erityiseen tarkasteluun. 
#Otetaan näiden kuntien peltolohkot irti, ja jaetaan kunkin kunnan ravinnemassa sen kunnan peltolohkojen kesken (lohkotasolla). 

rm.all.but(c("Turkiskuntien_pellot","Turkistarhaus_kunnittain_2015"))

#Keskittymät: poista kommentit seuraavista, jos haluat suodattaa vain turkistalouden keskittymiksi nimetyt kunnat. Muutoin saat kaikki. 

#Turkiskeskittymat<-Turkistarhaus_kunnittain_2015 %>% filter(Keskittymä == "x")
#Turkiskeskittymat<-unique(Turkiskeskittymat$Kuntakoodi)

#Kuntakoodilla peltolohkojen irrotus

#Turkiskeskittyma_pellot <- Turkiskuntien_pellot %>% filter(Kuntakoodi %in% Turkiskeskittymat)

#kunnat<-unique(Turkiskeskittyma_pellot$Kuntakoodi)

Turkiskeskittyma_pellot<-Turkiskuntien_pellot

Turkiskeskittyma_pellot<-Turkiskeskittyma_pellot %>% group_by(Kuntakoodi) %>% mutate(Kunnan_peltoalasumma = sum(Maannossumma))
Turkiskeskittyma_pellot<-Turkiskeskittyma_pellot %>% mutate(Kunnan_peltoalajakauma = Maannossumma/Kunnan_peltoalasumma)

#Nyt tiedetään kunkin kunnan viljelyalan jakauma
#ja alkuperäisestä laskennasta kunnan turkislannan ravinnetotaali
#Yhdistetään kunnalle kuuluva ravinnetonnimäärä dataan. Jaetaan peltolohkoille viljelyalan jakaumalla. 

library(readxl)
jaettavat_ravinteet <- read_excel("Data/Ravinnelaskennan_aineisto/Turkislannan_ravinteet_kunnittain.xlsx")

rm.all.but(c("jaettavat_ravinteet","Turkiskeskittyma_pellot"))
             
Turkiskeskittyma_pellot<-left_join(Turkiskeskittyma_pellot, jaettavat_ravinteet, by="Kuntakoodi")

Turkiskeskittyma_pellot<-Turkiskeskittyma_pellot %>% mutate(Lohkoille_jaettu_fosfori_kg = Kunnan_peltoalajakauma*`Jaetut fosforikilot`,
                                   Lohkoille_jaettu_typpi_kg = Kunnan_peltoalajakauma*`Jaetut typpikilot` )

Turkisravinteet_lohkoille<-createWorkbook()
addWorksheet(Turkisravinteet_lohkoille,"Turkislanta_ravinteet_lohkoille")
writeData(Turkisravinteet_lohkoille,"Turkislanta_ravinteet_lohkoille",Turkiskeskittyma_pellot)
saveWorkbook(Turkisravinteet_lohkoille, here("Output/Ravinnedata/Turkislannan_ravinteet_jaettuna_pelloille_kaikki.xlsx"),overwrite = T)

gc()
