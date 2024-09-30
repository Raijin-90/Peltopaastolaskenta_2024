require(tidyverse)
require(openxlsx)
library(here)

#Lisämoduuli: 
#lAIDUNTILOJEN JA HAKAMAATILOJEN ERITTELY.
#Tilakoodivektori perustuu tiedostoon \\kkb01\gkkenvimat$\Työohjeet ja taustamateriaalit\Alkutuotanto\Viljelykasvit tiloittain_120123.xlsx
#Noudetaan tilat jotka:
#Eivät ole eläintiloja (eläinmäärä 0)
#Laitumia ja hakamaita (kasviluokkanimen perusteella) on pinta-alaltaan enemmän, kuin muita kasviluokkia yhteensä



#Laidunten tilakoodivektori ####


Laitumet_hakamaat<-c("005032377",
                     "014028422",
                     "016788777",
                     "018066046",
                     "043023540",
                     "044031532",
                     "044058107",
                     "044079426",
                     "046018517",
                     "046022355",
                     "049100184",
                     "049114332",
                     "050007136",
                     "060015516",
                     "060031781",
                     "060037542",
                     "062007955",
                     "062008965",
                     "062009571",
                     "062019574",
                     "072019769",
                     "073002503",
                     "073112233",
                     "076005156",
                     "076006873",
                     "076035468",
                     "077036992",
                     "077056695",
                     "077071752",
                     "077163294",
                     "082136768",
                     "085036462",
                     "086046979",
                     "086080426",
                     "089037714",
                     "090049645",
                     "092117664",
                     "098071848",
                     "103059365",
                     "105044330",
                     "106016451",
                     "106021505",
                     "106030696",
                     "106034841",
                     "106074752",
                     "106099711",
                     "106105569",
                     "106105670",
                     "106107084",
                     "106107387",
                     "108022533",
                     "108078915",
                     "108088817",
                     "109050632",
                     "142050335",
                     "142079637",
                     "142170068",
                     "145034602",
                     "145056224",
                     "146118574",
                     "146285292",
                     "148018966",
                     "149020187",
                     "149104558",
                     "153061350",
                     "165041860",
                     "165100262",
                     "167023387",
                     "169008352",
                     "170005634",
                     "171144170",
                     "177029646",
                     "177059756",
                     "178003282",
                     "178024201",
                     "178252755",
                     "180034626",
                     "180104950",
                     "180113741",
                     "181056661",
                     "182009685",
                     "182038785",
                     "182041213",
                     "182102342",
                     "182189743",
                     "182201463",
                     "183009896",
                     "183019903",
                     "202002496",
                     "208168464",
                     "210073910",
                     "211001470",
                     "211011978",
                     "211100086",
                     "211102009",
                     "211120702",
                     "213288953",
                     "214170845",
                     "214208635",
                     "220057735",
                     "223022703",
                     "224024732",
                     "230084101",
                     "232208704",
                     "244055939",
                     "245003004",
                     "245011993",
                     "245012296",
                     "245013815",
                     "246003518",
                     "246141035",
                     "246151846",
                     "249001727",
                     "252027218",
                     "254001469",
                     "255075139",
                     "257141340",
                     "257142552",
                     "259039409",
                     "260061141",
                     "263012062",
                     "263057431",
                     "263089561",
                     "263284773",
                     "272117736",
                     "272119150",
                     "273115624",
                     "274023178",
                     "274028131",
                     "275028645",
                     "277157793",
                     "279005443",
                     "284049140",
                     "287031080",
                     "288194777",
                     "290259867",
                     "297028144",
                     "297223962",
                     "299069386",
                     "300009377",
                     "303005364",
                     "303034767",
                     "303053258",
                     "303117320",
                     "306029845",
                     "310057668",
                     "315018210",
                     "316001344",
                     "316010943",
                     "317066728",
                     "318002776",
                     "319108374",
                     "320062917",
                     "320186387",
                     "398039092",
                     "398039702",
                     "400091957",
                     "400265143",
                     "400806525",
                     "402010840",
                     "405003389",
                     "405020062",
                     "405041482",
                     "405080383",
                     "405200221",
                     "408299369",
                     "410020515",
                     "410034255",
                     "410157527",
                     "410185213",
                     "410186627",
                     "413020542",
                     "414026110",
                     "414028837",
                     "418003211",
                     "418092935",
                     "418233482",
                     "421008995",
                     "423434096",
                     "426044511",
                     "426144844",
                     "428116469",
                     "429012509",
                     "429070002",
                     "430044345",
                     "431135189",
                     "433019013",
                     "433027602",
                     "433137635",
                     "433159661",
                     "434009019",
                     "476112978",
                     "489118557",
                     "492013403",
                     "492036540",
                     "492063216",
                     "492068973",
                     "492082919",
                     "492092922",
                     "492112423",
                     "494146591",
                     "495025857",
                     "498006383",
                     "499273851",
                     "499353572",
                     "505115776",
                     "505183171",
                     "505210554",
                     "507008084",
                     "507240480",
                     "531031651",
                     "532066420",
                     "535075137",
                     "536076358",
                     "537029281",
                     "538042226",
                     "538076881",
                     "540004858",
                     "540055479",
                     "540125403",
                     "543033480",
                     "543065311",
                     "543080667",
                     "543192724",
                     "543198178",
                     "543205050",
                     "544032883",
                     "544122813",
                     "545235683",
                     "560014645",
                     "562118737",
                     "564020947",
                     "573000117",
                     "575043379",
                     "577025112",
                     "578029363",
                     "578051187",
                     "578063719",
                     "585014070",
                     "585026602",
                     "585084701",
                     "586114012",
                     "592015450",
                     "592096282",
                     "593005254",
                     "594015468",
                     "595237971",
                     "598201929",
                     "599036432",
                     "599100187",
                     "601086869",
                     "602005541",
                     "604003337",
                     "604031225",
                     "606019725",
                     "607032060",
                     "609226381",
                     "609245579",
                     "609266090",
                     "613216822",
                     "616048010",
                     "618003063",
                     "618072781",
                     "620008539",
                     "620028747",
                     "620055827",
                     "623041609",
                     "630022676",
                     "632019765",
                     "632048158",
                     "685029659",
                     "685034107",
                     "685074321",
                     "686053011",
                     "689082138",
                     "696006120",
                     "696022183",
                     "696026631",
                     "696153943",
                     "696163037",
                     "696169202",
                     "698002296",
                     "699040402",
                     "700034549",
                     "701026171",
                     "705005090",
                     "705015501",
                     "707007637",
                     "707010566",
                     "707025219",
                     "707065837",
                     "730010074",
                     "730011993",
                     "736038626",
                     "737010848",
                     "738030762",
                     "740030073",
                     "740044726",
                     "740103229",
                     "741005430",
                     "741034934",
                     "749042484",
                     "753028275",
                     "753163570",
                     "753164580",
                     "753165085",
                     "754004238",
                     "754006157",
                     "754008581",
                     "754179848",
                     "755003641",
                     "755009604",
                     "755032034",
                     "755186527",
                     "758061969",
                     "761025826",
                     "761071700",
                     "761221846",
                     "762012293",
                     "766001219",
                     "768055393",
                     "772069476",
                     "790000231",
                     "832001938",
                     "832008103",
                     "832010628",
                     "832043465",
                     "834156752",
                     "835025510",
                     "837039369",
                     "842027694",
                     "842035879",
                     "844001040",
                     "848018456",
                     "848082518",
                     "848193662",
                     "850039389",
                     "851255125",
                     "853083674",
                     "853128033",
                     "853135713",
                     "853139753",
                     "853141268",
                     "856004586",
                     "856022875",
                     "856023784",
                     "858034011",
                     "858128583",
                     "858130708",
                     "858130910",
                     "858137879",
                     "890047041",
                     "892008158",
                     "906040119",
                     "908028316",
                     "908037208",
                     "908039935",
                     "909063485",
                     "909119968",
                     "911013387",
                     "911040467",
                     "911108064",
                     "917048104",
                     "917048205",
                     "917232404",
                     "918032854",
                     "922103824",
                     "925093848",
                     "925129517",
                     "925152149",
                     "927163584",
                     "927182176",
                     "927190260",
                     "927192381",
                     "927195314",
                     "932035311",
                     "933051989",
                     "935029577",
                     "935103844",
                     "936201661",
                     "941013568",
                     "972065894",
                     "973078637",
                     "975196166",
                     "976096044",
                     "976180920",
                     "977039368",
                     "980037981",
                     "980040914",
                     "981092251",
                     "981093665",
                     "988000873",
                     "989033218",
                     "992009502",
                     "992012128",
                     "992067092",
                     "992070833")


#Taimitarhat


Taimi<-c("082017944",
         "099094287",
         "143252832",
         "165155836",
         "171118205",
         "178058048",
         "284055103",
         "297199512",
         "312032125",
         "426006317",
         "439006539",
         "545333289",
         "545546992",
         "586001147",
         "606013762",
         "707134242",
         "778012142",
         "834010444",
         "908052160",
         "927196021")





#GTK-AINEISTO ####

source(here("Skriptit/GTK_datan_aggregointi.R")) #Scriptistä saadaan irti GTKData, jossa kaikki gtk-aineisto
Kat <- read.xlsx(here("Data/Kasvikategoriat_avain.xlsx"))

colnames(Kat)[colnames(Kat) == "Kasvikoodi"] <-
  "KASVIKOODI" #Yhdistetään tieto kasvikategorioista




df <-
  merge(GTKdata, Kat, by = "KASVIKOODI") #Aggregoitava data, originaali jää jos tulee ongelmia. Tämä kategoria-avain sisältää sekä disaggregoidut (rehu/ruokavilja) että ei-disaggregoidut koodit.




#Nimeämiskorjaus niille tuotantosuunnille, joilla raakadatassa on välilyöntejä

uudet_nimet <- c(
  "Viljat pl. ohra"  = "Viljat_pl_ohra",
  "Muut nautakarjatilat"  = "Muut_nautakarjatilat",
  "Vihannekset ja juurekset" = "Vihannekset_juurekset",
  "Palkokasvit pl. tarhaherne" = "Palkokasvit_pl_tarhaherne",
  "Rypsi ja rapsi" = "Rypsi_rapsi",
  "Lammas- ja vuohitilat" = "Lammas_ja_vuohitilat",
  "Nurmet, laitumet, hakamaat" = "Nurmet_laitumet_hakamaat",
  "Tattari ja kinoa" = "Tattari_kinoa",
  "Hunajatuotanto" = "Hunajatuotanto"
)


df$Tuotantosuunta <-
  str_replace_all(df$Tuotantosuunta, uudet_nimet)

#MUUTOS 20/11
#Muutetaan tilakoodivektorin mukaisten tilojen tuotantosuunta "Nurmet laitumet hakamaat" --> "Laitumet, hakamaat
#Kommentoi pois jättääksesi alkuperäiset

#df$Tuotantosuunta[df$MAATILA_TUNNUS %in% Laitumet_hakamaat]<-"Laitumet_hakamaat"
df$Tuotantosuunta[df$MAATILA_TUNNUS %in% Taimi]<-"Taimitarhat"



#Muut "nurmet_laitumet_hakamaat" muutetaan rehukasveiksi, sillä sisältää rehunurmet yms. vaan ei enää laitumia, joilla oma koodi 

#df$Tuotantosuunta[df$Tuotantosuunta == "Nurmet_laitumet_hakamaat"]<-"Rehunurmet_rehukasvit_pl_rehuohra"


#Muutos 16.11.2023.
#Lisätään tuotantosuuntaryhmäluokat aggregointiavaimeksi.

TS_Ryhmat <-
  read.xlsx(here("Data/Muuntoavain_tuotantosuunnat_tuotteet.xlsx"),
            sheet = "Tuotantosuunnat ryhmittäin")
colnames(TS_Ryhmat) <- c("Tuotantosuunta", "Tuotantosuuntaryhma","ETOL")

filter(df,!(Tuotantosuunta %in% TS_Ryhmat$Tuotantosuunta))  #Tavoitteena tyhjä vektori eli samat nimet

df <- merge(df, TS_Ryhmat, by = "Tuotantosuunta")


#Toinen muutos: lisätään ENVIMAT-tuotteiden ja kasvikoodien vastinetieto

ETTL <-
  read_excel(
    here(
      "Data/Ruokavirasto-ENVIMATFOOD-muuntoavain_envimatfood_1.1.xlsx"
    ),
    sheet = "Ruokavirasto-ETTL tuotemuunto"
  )

ETTL <- select(ETTL, 1:3)
colnames(ETTL)[3] <- "KASVIKOODI"

df <- merge(df, ETTL, by = "KASVIKOODI")


#Lisätään biodiversiteettiluokat


library(readxl)
Biodiversiteettiluokat_kasveille <- read_excel(here("Data/Biodiversiteettiluokat_kasveille.xlsx"))
colnames(Biodiversiteettiluokat_kasveille)[1]<-c("KASVIKOODI")

df<-merge(df, Biodiversiteettiluokat_kasveille, by="KASVIKOODI")



## AGGREGOINTI VANHOISTA LOHKOISTA ####

#Ei raivattuja lohkoja mukaan, näille oma frame
df_vanhat_lohkot <- filter(df, is.na(Raivattu))

Aggre_vanhat_gtk <- df_vanhat_lohkot  %>%
  group_by(Tuotantosuuntaryhma,
           ETTL,
           `ETTL Nimike`,
           `Cropland/grassland`,
           `Yksi/monivuotinen`,
           Soveltuva_biodiv_kerroin) %>% summarise(
             Eloperaista_maata =  sum(Eloperaista),
             Mineraalimaata = sum(Mineraalia)
           )

#Jako aliaineistoihin
Annual_cropland <-
  Aggre_vanhat_gtk %>% filter(`Yksi/monivuotinen` == "Yksivuotinen" &
                                `Cropland/grassland` == "Cropland")
Perennial_cropland <-
  Aggre_vanhat_gtk %>% filter(`Yksi/monivuotinen` == "Monivuotinen" &
                                `Cropland/grassland` == "Cropland")

Annual_grassland <-
  Aggre_vanhat_gtk %>% filter(`Yksi/monivuotinen` == "Yksivuotinen" &
                                `Cropland/grassland` == "Grassland")
Perennial_grassland <-
  Aggre_vanhat_gtk %>% filter(`Yksi/monivuotinen` == "Monivuotinen" &
                                `Cropland/grassland` == "Grassland")


#ja Edelleen maalajikohtaisiksi tauluiksi, ts. pudotetaan "väärä" maalaji ulos:

Annual_cropland_mineral <-
  Annual_cropland %>% select(-"Eloperaista_maata")
Annual_cropland_organic <-
  Annual_cropland %>% select(-"Mineraalimaata")

Annual_grassland_mineral <-
  Annual_grassland %>% select(-"Eloperaista_maata")
Annual_grassland_organic <-
  Annual_grassland %>% select(-"Mineraalimaata")


Perennial_cropland_mineral <-
  Perennial_cropland %>% select(-"Eloperaista_maata")
Perennial_cropland_organic <-
  Perennial_cropland %>% select(-"Mineraalimaata")

Perennial_grassland_mineral <-
  Perennial_grassland %>% select(-"Eloperaista_maata")
Perennial_grassland_organic <-
  Perennial_grassland %>% select(-"Mineraalimaata")


#Tarkistussummat 
sum(Aggre_vanhat_gtk$Mineraalimaata)
sum(Annual_cropland_mineral$Mineraalimaata)+sum(Perennial_cropland_mineral$Mineraalimaata)+sum(Annual_grassland_mineral$Mineraalimaata)+sum(Perennial_grassland_mineral$Mineraalimaata)

sum(Aggre_vanhat_gtk$Eloperaista_maata)
sum(Annual_cropland_organic$Eloperaista_maata)+sum(Perennial_cropland_organic$Eloperaista_maata)+sum(Annual_grassland_organic$Eloperaista_maata)+sum(Perennial_grassland_organic$Eloperaista_maata)


## AGGREGOINTI RAIVATUISTA LOHKOISTA #### 

df_raivatut_lohkot <- filter(df, !(is.na(Raivattu)))

Aggre_raivatut_gtk <- df_raivatut_lohkot  %>%
  group_by(Tuotantosuuntaryhma,
           ETTL,
           `ETTL Nimike`,
           `Cropland/grassland`,
           `Yksi/monivuotinen`,
           Soveltuva_biodiv_kerroin) %>% summarise(
             Eloperaista_maata =  sum(Eloperaista),
             Mineraalimaata = sum(Mineraalia)
           )

#Jako aliaineistoihin
Annual_cropland <-
  Aggre_raivatut_gtk %>% filter(`Yksi/monivuotinen` == "Yksivuotinen" &
                                  `Cropland/grassland` == "Cropland") 
Perennial_cropland <-
  Aggre_raivatut_gtk %>% filter(`Yksi/monivuotinen` == "Monivuotinen" &
                                  `Cropland/grassland` == "Cropland")

Annual_grassland <-
  Aggre_raivatut_gtk %>% filter(`Yksi/monivuotinen` == "Yksivuotinen" &
                                  `Cropland/grassland` == "Grassland")
Perennial_grassland <-
  Aggre_raivatut_gtk %>% filter(`Yksi/monivuotinen` == "Monivuotinen" &
                                  `Cropland/grassland` == "Grassland")


#ja Edelleen maalajikohtaisiksi tauluiksi, ts. pudotetaan "väärä" maalaji ulos:

Annual_cropland_mineral_raivattu <-
  Annual_cropland %>% select(-"Eloperaista_maata")
Annual_cropland_organic_raivattu <-
  Annual_cropland %>% select(-"Mineraalimaata")

Annual_grassland_mineral_raivattu <-
  Annual_grassland %>% select(-"Eloperaista_maata")
Annual_grassland_organic_raivattu <-
  Annual_grassland %>% select(-"Mineraalimaata")


Perennial_cropland_mineral_raivattu <-
  Perennial_cropland %>% select(-"Eloperaista_maata")
Perennial_cropland_organic_raivattu <-
  Perennial_cropland %>% select(-"Mineraalimaata")

Perennial_grassland_mineral_raivattu <-
  Perennial_grassland %>% select(-"Eloperaista_maata")
Perennial_grassland_organic_raivattu <-
  Perennial_grassland %>% select(-"Mineraalimaata")

#Siistitään environment. Setdiff katsoo, mitä eroa on annetun listan ja ls() välillä. 

rm(list = rm(list = setdiff(
  ls(),
  c(
    
    "Annual_cropland_mineral",
    "Annual_cropland_organic",
    "Annual_cropland_mineral_raivattu",
    "Annual_cropland_organic_raivattu",
    "Annual_grassland_mineral",
    "Annual_grassland_organic",
    "Annual_grassland_mineral_raivattu",
    "Annual_grassland_organic_raivattu",
    "Perennial_cropland_mineral",
    "Perennial_cropland_organic",
    "Perennial_cropland_mineral_raivattu",
    "Perennial_cropland_organic_raivattu",
    "Perennial_grassland_mineral",
    "Perennial_grassland_organic",
    "Perennial_grassland_mineral_raivattu",
    "Perennial_grassland_organic_raivattu"
  )
)))
gc()


#Raivattujen kokoaminen ####

a<-Annual_cropland_mineral_raivattu %>% 
  ungroup() %>% 
  select(1:3, 6,7) %>% 
   replace(is.na(.), 0)

b<-Annual_cropland_organic_raivattu %>% 
  ungroup() %>% 
  select(1:3, 6,7) %>% 
    replace(is.na(.), 0)

c<-Annual_grassland_mineral_raivattu %>% 
  ungroup() %>% 
  select(1:3, 6,7) %>% 
   replace(is.na(.), 0)

d<-Annual_grassland_organic_raivattu %>% 
  ungroup() %>%  
  select(1:3, 6,7) %>% 
    replace(is.na(.), 0)

e<-Perennial_cropland_mineral_raivattu %>% 
  ungroup() %>% 
  select(1:3, 6,7) %>% 
   replace(is.na(.), 0)

f<-Perennial_cropland_organic_raivattu %>% 
  ungroup() %>%  
  select(1:3, 6,7) %>% 
    replace(is.na(.), 0)


g<-Perennial_grassland_mineral_raivattu %>% 
  ungroup() %>% 
  select(1:3, 6,7) %>% 
   replace(is.na(.), 0)


h<-Perennial_grassland_organic_raivattu %>% 
  ungroup() %>%  
  select(1:3, 6,7) %>% 
    replace(is.na(.), 0)


Raivatut<-createWorkbook()

Raivatut %>% addWorksheet("Annual crops, mineral") 
Raivatut %>% addWorksheet("Annual crops, organic") 
Raivatut %>% addWorksheet("Annual grass, mineral") 
Raivatut %>% addWorksheet("Annual grass, organic")
Raivatut %>% addWorksheet("Perennial crops, mineral") 
Raivatut %>% addWorksheet("Perennial crops, organic") 
Raivatut %>% addWorksheet("Perennial grass, mineral") 
Raivatut %>% addWorksheet("Perennial grass, org")

Raivatut %>% writeData("Annual crops, mineral", a)
Raivatut %>% writeData("Annual crops, organic", b)
Raivatut %>% writeData("Annual grass, mineral", c)
Raivatut %>% writeData("Annual grass, organic", d)
Raivatut %>% writeData("Perennial crops, mineral",e)
Raivatut %>% writeData("Perennial crops, organic",f)
Raivatut %>% writeData("Perennial grass, mineral",g)
Raivatut %>% writeData("Perennial grass, org",h)


saveWorkbook(Raivatut, file=here("Envimatin_paivitys/gtk_raiviodata_envimatiin_BD.xlsx"), overwrite = T)

#Muiden kuin raivattujen kokoaminen ####


a<-Annual_cropland_mineral %>% 
  ungroup() %>% 
  select(1:3, 6,7) %>% 
   replace(is.na(.), 0)

b<-Annual_cropland_organic %>% 
  ungroup() %>% 
  select(1:3, 6,7) %>% 
    replace(is.na(.), 0)

c<-Annual_grassland_mineral %>% 
  ungroup() %>% 
  select(1:3, 6,7) %>% 
   replace(is.na(.), 0)

d<-Annual_grassland_organic %>% 
  ungroup() %>%  
  select(1:3, 6,7) %>% 
    replace(is.na(.), 0)

e<-Perennial_cropland_mineral %>% 
  ungroup() %>% 
  select(1:3, 6,7) %>% 
   replace(is.na(.), 0)

f<-Perennial_cropland_organic %>% 
  ungroup() %>%  
  select(1:3, 6,7) %>% 
    replace(is.na(.), 0)


g<-Perennial_grassland_mineral %>% 
  ungroup() %>% 
  select(1:3, 6,7) %>% 
   replace(is.na(.), 0)


h<-Perennial_grassland_organic %>% 
  ungroup() %>%  
  select(1:3, 6,7) %>% 
    replace(is.na(.), 0)

Vanhat_pellot<-createWorkbook()

Vanhat_pellot %>% addWorksheet("Annual crops, mineral") 
Vanhat_pellot %>% addWorksheet("Annual crops, organic") 
Vanhat_pellot %>% addWorksheet("Annual grass, mineral") 
Vanhat_pellot %>% addWorksheet("Annual grass, organic")
Vanhat_pellot %>% addWorksheet("Perennial crops, mineral") 
Vanhat_pellot %>% addWorksheet("Perennial crops, organic") 
Vanhat_pellot %>% addWorksheet("Perennial grass, mineral") 
Vanhat_pellot %>% addWorksheet("Perennial grass, org")

Vanhat_pellot %>% writeData("Annual crops, mineral", a)
Vanhat_pellot %>% writeData("Annual crops, organic", b)
Vanhat_pellot %>% writeData("Annual grass, mineral", c)
Vanhat_pellot %>% writeData("Annual grass, organic", d)
Vanhat_pellot %>% writeData("Perennial crops, mineral",e)
Vanhat_pellot %>% writeData("Perennial crops, organic",f)
Vanhat_pellot %>% writeData("Perennial grass, mineral",g)
Vanhat_pellot %>% writeData("Perennial grass, org",h)

saveWorkbook(Vanhat_pellot, file=here("Envimatin_paivitys/gtk_vanhat_pellot_envimatiin_BD.xlsx"), overwrite = T)



#Lisäys: aggregointi biodiversiteettiluokan tarkkuudella. 
#Aggregoidaan luotua df-dataframea siten, että saadaan pinta-alat kutakin biodiversiteettiluokkaa 

#BD-laskennassa ei ole tarpeen eritellä maalajia, eikä raivattua alaa. 



#Tuote x tuotantosuunta

df$Tuotantosuuntaryhma<-df$ETOL


Tuote_tuotantosuunta<-df %>% 
  group_by(Tuotantosuuntaryhma, ETTL,`ETTL Nimike`, Soveltuva_biodiv_kerroin) %>% 
  summarise(Viljelyala_ha = sum(Maannossumma))


#Liitetään vielä kertoimen numeroarvo. 


library(readxl)
colnames(Tuote_tuotantosuunta)[4]<-"Elinympäristötyyppi"

Biodiversiteettikertoimet <- read_excel(here("Data/Biodiversiteettikertoimet.xlsx"))


#Pinta-alataulukko

Tuote_tuotantosuunta<-inner_join(Tuote_tuotantosuunta, Biodiversiteettikertoimet, by="Elinympäristötyyppi") #Muista sarakesummia laskiessa että dimensio vaihtuu kun uusi numeromuuttuja tuodaan. Älä summaa saraketta 30...

sum(colSums(Tuote_tuotantosuunta[4:28]))
sum(GTKdata$Maannossumma)


#Aggregoidaan tuotantosuunnittainen totaalisumma kutakin elinympäristötyyppiä

Summat<-Tuote_tuotantosuunta %>% group_by(Tuotantosuuntaryhma, Elinympäristötyyppi) %>% summarise(Elinymparistoa_yhteensa = sum (Viljelyala_ha))

#Yhdistetään aineistoon

Tuote_tuotantosuunta<-inner_join(Tuote_tuotantosuunta, Summat, by=c("Tuotantosuuntaryhma", "Elinympäristötyyppi"))


#Kerrotaan elinympäristö/tuote/tuotantosuuntakohtainen viljelyala skaalatulla BD-kertoimella, ja jaetaan kokonaisalalla ko. elinympäristöä sen tuotantosuunnan tapauksessa


Tuote_tuotantosuunta$BD_arvo <-
  (
    Tuote_tuotantosuunta$Skaalattu_kokonaisdiversiteetti * Tuote_tuotantosuunta$Viljelyala_ha
  ) / Tuote_tuotantosuunta$Elinymparistoa_yhteensa


#Tulosten kokoaminen

BD_luvut<-select(Tuote_tuotantosuunta, 1:4,8) %>% pivot_wider(names_from = Tuotantosuuntaryhma, values_from = BD_arvo)
BD_luvut[is.na(BD_luvut)]<-0

Alat<-select(Tuote_tuotantosuunta, 1:4,5) %>% pivot_wider(names_from = Tuotantosuuntaryhma, values_from = Viljelyala_ha) 
Alat[is.na(Alat)]<-0

Kertoimet_painotus<-select(Tuote_tuotantosuunta, 1:4, 6,7)



#Täydennetään tyhjinä sarakkeina ne alkutuotannon tuotantosuunnat, joita peltodatassa ei ole
#Saadaan sarakesuunnassa miotia vastaava dimensio

BD_luvut$`Muiden kasvien viljely`<-NA
BD_luvut$`Sienimöt`<-NA
BD_luvut$`Syötävien kasvien tuotanto kasvihuoneessa`<-NA
BD_luvut$`Koristekasvituotanto kasvihuoneessa`<-NA
BD_luvut$Poronhoito<-NA

BD_luvut[is.na(BD_luvut)]<-0


Alat$`Muiden kasvien viljely`<-NA
Alat$`Sienimöt`<-NA
Alat$`Syötävien kasvien tuotanto kasvihuoneessa`<-NA
Alat$`Koristekasvituotanto kasvihuoneessa`<-NA
Alat$Poronhoito<-NA

Alat[is.na(Alat)]<-0



#Lajitellaan sarakkeet MIOTin mallin mukaisesti


Sorttivektori <-
  c(
    "ETTL",
    "ETTL Nimike",
    "Elinympäristötyyppi",
    "Kauran, ohran, rukiin ja vehnän viljely",
    "Rehukasvien viljely",
    "Mallasohran viljely",
    "Peltoherneen ja härkäpavun viljely",
    "Rypsin ja rapsin viljely",
    "Tattarin ja kinoan viljely",
    "Öljyhampun ja -pellavan viljely",
    "Kuminan ja muiden maustekasvien viljely",
    "Muiden kasvien viljely",
    "Perunan viljely",
    "Sokerijuurikkaan viljely",
    "Tarhaherneen viljely",
    "Muiden vihannesten ja juuresten viljely",
    "Yrttien viljely avomaalla",
    "Marjojen viljely",
    "Hedelmien viljely",
    "Taimitarhat",
    "Sienimöt",
    "Syötävien kasvien tuotanto kasvihuoneessa",
    "Koristekasvituotanto kasvihuoneessa",
    "Maitotilat",
    "Muut nautakarjatilat",
    "Lammas- ja vuohitilat",
    "Sikatilat",
    "Hevostilat",
    "Munatilat",
    "Siipikarjatilat",
    "Turkistarhaus",
    "Poronhoito",
    "Hunajatuotanto, muu eläimen hoito"
  )

x<-colnames(BD_luvut) #Tarkistetaan sarakkeiden nimet
x[!(x %in% Sorttivektori)]

x<-colnames(Alat) #Tarkistetaan sarakkeiden nimet
x[!(x %in% Sorttivektori)]


Alat<-Alat %>% select(all_of(Sorttivektori)) #Järjestetään sarakkeet
a
BD_luvut<-BD_luvut %>% select(all_of(Sorttivektori)) #Järjestetään sarakkeet


sum(colSums(Alat[4:32]))
sum(df$Maannossumma) #Kaikki ala allokoituu




library(openxlsx)
Peltoala_BD_luokka<-createWorkbook()
addWorksheet(Peltoala_BD_luokka, "Peltoala BD-luokittain")
writeData(Peltoala_BD_luokka, "Peltoala BD-luokittain", Alat)
addWorksheet(Peltoala_BD_luokka, "BD-arvot")
writeData(Peltoala_BD_luokka, "BD-arvot", BD_luvut)
addWorksheet(Peltoala_BD_luokka, "Kertoimet_painotusalat")
writeData(Peltoala_BD_luokka, "Kertoimet_painotusalat", Kertoimet_painotus)



saveWorkbook(Peltoala_BD_luokka, here("Envimatin_paivitys/Peltoala_BD_luokittain_1112.xlsx"),overwrite = T)


rm(list=ls())
gc()


################

# Lisäys: pientarealan jakaminen tuotantosuunnille/tuotteille
# Marjaana Toivosen tiedon mukaan pientareiden arvioitu kokonaismäärä on n. 40 000 ha. 


#Lasketaan tuotekohtainen jakauma per tuotantosuunta. Jaetaan tuotantosuunnalle kuuluva pientarepotti tällä tuoteriveille.  

df$Tuotantosuuntaryhma<-df$ETOL

Pientare<-df %>% group_by(Tuotantosuuntaryhma, ETTL, `ETTL Nimike`) %>% summarise(Ala_yht = sum(Maannossumma))

#Lasketaan tuotantosuunnan kokonaisviljelyala, niiden jakosuhde, ja sen pohjalta allokoidaan kullekin tuotantosuunnalle kuuluva osuus 40 kHa potista pientaretta

TS_jakauma <-
  Pientare %>% group_by(Tuotantosuuntaryhma) %>% summarise(Tuotantosuunnan_kokonaisala =
                                                        sum(Ala_yht)) %>% mutate(Jakauma = Tuotantosuunnan_kokonaisala/sum(Tuotantosuunnan_kokonaisala),
                                                                                 Pientaretta_tuotantosuunnalla = Jakauma * 40000) %>% select(1,2,4)



#Yhdistetään tuotantosuunnan kokonaisala Pientare-frameen jotta saadaan jakauma per tuote tuotantosuunnan sisällä

Pientare<-inner_join(Pientare, TS_jakauma, by="Tuotantosuuntaryhma")

#Lasketaan kunkin tuotantosuunnan sisäinen tuotejakauma. Tämä kerrotaan tuotantosuunnalle kuuluvalla pientare yhteensä-summalla. -> pientare joka tuotantosuunnalla allookoituu tuotteelle

Pientare<-Pientare %>% mutate(Tuotteittainen_jakauma=Ala_yht/Tuotantosuunnan_kokonaisala,
                              Pientaretta_tuotteella = Tuotteittainen_jakauma*Pientaretta_tuotantosuunnalla)



sum(unique(Pientare$Tuotantosuunnan_kokonaisala))
sum(unique(Pientare$Pientaretta_tuotantosuunnalla))
sum(Pientare$Pientaretta_tuotteella)

#Välitulosten poisto ja kääntö pitkäksi

Pientare<-Pientare %>% select(1:3, 8) %>% pivot_wider(names_from=Tuotantosuuntaryhma, values_from=Pientaretta_tuotteella)

#Täydennetään tyhjinä sarakkeina ne alkutuotannon tuotantosuunnat, joita peltodatassa ei ole
#Saadaan sarakesuunnassa miotia vastaava dimensio

Pientare$`Muiden kasvien viljely`<-NA
Pientare$`Sienimöt`<-NA
Pientare$`Syötävien kasvien tuotanto kasvihuoneessa`<-NA
Pientare$`Koristekasvituotanto kasvihuoneessa`<-NA
Pientare$Poronhoito<-NA

Pientare[is.na(Pientare)]<-0

#Sortataan columnit

Sorttivektori <-
  c(
    "ETTL",
    "ETTL Nimike",
    "Kauran, ohran, rukiin ja vehnän viljely",
    "Rehukasvien viljely",
    "Mallasohran viljely",
    "Peltoherneen ja härkäpavun viljely",
    "Rypsin ja rapsin viljely",
    "Tattarin ja kinoan viljely",
    "Öljyhampun ja -pellavan viljely",
    "Kuminan ja muiden maustekasvien viljely",
    "Muiden kasvien viljely",
    "Perunan viljely",
    "Sokerijuurikkaan viljely",
    "Tarhaherneen viljely",
    "Muiden vihannesten ja juuresten viljely",
    "Yrttien viljely avomaalla",
    "Marjojen viljely",
    "Hedelmien viljely",
    "Taimitarhat",
    "Sienimöt",
    "Syötävien kasvien tuotanto kasvihuoneessa",
    "Koristekasvituotanto kasvihuoneessa",
    "Maitotilat",
    "Muut nautakarjatilat",
    "Lammas- ja vuohitilat",
    "Sikatilat",
    "Hevostilat",
    "Munatilat",
    "Siipikarjatilat",
    "Turkistarhaus",
    "Poronhoito",
    "Hunajatuotanto, muu eläimen hoito"
  )
x<-Sorttivektori
y<-colnames(Pientare)
y[!(y %in% x)]

Pientare<-Pientare %>% select(all_of(Sorttivektori))


sum(colSums(Pientare[3:length(Pientare)]))

#Tallennetaan

Pientareala<-createWorkbook()
addWorksheet(Pientareala,"Pientare")
writeData(Pientareala, "Pientare",Pientare)
saveWorkbook(Pientareala, file=here("Envimatin_paivitys/Pientareala.xlsx"))
