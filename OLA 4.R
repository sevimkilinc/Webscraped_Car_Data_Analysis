############################
####Opgave 1 – Webscrape####
############################

############################################
####Opgave 1.1 – Hente data fra Bilbasen####
############################################

library("httr")
library("dplyr")
library("rvest")
library("stringr")
library("quantmod")
library("ggplot2")
library("scales")
library("lubridate")
library("DBI")
library("RMariaDB")
library("jsonlite")
library("maps")
library("ggrepel")
library("leaflet")


# Step 1 Find hjemmesiden hvor der skal webscrapes & Tilføj header(cookies) - test indlæsning af rådata

startlink <- "https://www.bilbasen.dk/brugt/bil/mercedes/ms-eqa-klasse?fuel=3&includeengroscvr=true&includeleasing=false&page="

response <- GET(startlink,
                add_headers(
                  cookie = 'aws-waf-token=64e206b8-54bb-4261-9dea-049898a086a4:CgoAaXSHyyZrAQAA:WV83nxjZrYTh+Ss62ltSB17rt6r2IHmIerfduOdia7rOXilSgVdEVosvVtAWQMK4F+3gYIQdycYyXv5wVQ9p0Kb5EizmekPg6s78/dnDdo32MMBRcHWw06Jc3nG6+InlSI/mJE1nenVkm+QGDVhAOTWeIQxVaM5q7Dt/oXWdT5ZOuqKFJz2mHkCinevd5XsXKIY=;GdprConsent={"version":"IAB TCF 2.0","consentString":"CQH-gkAQH-gkAAGABBENBOFgAAAAAAAAAAZQAAAAAAAA.YAAAAAAAAAAA"};_pulse2data=32ba677d-836d-4eff-a407-eeddf7b82529%2Cv%2C%2C1733062743000%2C%2C%2C0%2Ctrue%2C%2C;_sp_su=false;_pulsesession=%5B%22sdrn%3Aschibsted%3Asession%3Af28be5a4-737e-44a2-80dc-11cc6e86289c%22%2C1732457647472%2C1732457950590%5D;bbsession=id=45729d35-01e2-424e-bece-00cf8f0b67be;bbtracker=id=d6823628-7461-466a-8c0f-3eb9c5059923;consentDate=2024-11-12T03:06:04.992Z;consentUUID=dfb55337-c629-47fa-a4b5-02a008eb06ea_36_37;GdprConsent-Custom=eyJ2ZW5kb3JJZHMiOnt9fQ==',
                  `user-agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36"
                )
)

response$status_code

# Step 2 Find data i udviklerværktøjet som skal ekstraktes fra hjemmesiden
pricetag <- ".Listing_price__6B3kE"
propertiestag <- ".Listing_properties___ptWv"
modeltag <- ".Listing_makeModel__7yqgs"
detail_itemstag <- ".ListingDetails_list__WPBUe"
descriptiontag <- ".Listing_description__sCNNM"
locationtag <- ".Listing_location__nKGQz"

# Step 3 Lav dataframe til opsamling

car_colnames <- c("Price", "Details", "Makemodel", "Properties", "Description", "Location", "Link", "CarID", "Scrapdate")

carframe <- as.data.frame(matrix(data = NA, nrow = 0, ncol = 9))
colnames(carframe) <- car_colnames

# Step 4 Lav et loop som indsamler data vha. wepscraping

for (page_number in 1:20) {
  # Byg URL for den aktuelle side
  url <- paste0(startlink, page_number)
  
  message("Behandler side ", page_number, " af 20")
  
  # Udfør forespørgsel m. fejlhåndtering
  tryCatch({
    
    # Udfør forespørgsel
    response <- GET(url,
                    add_headers(
                      cookie = 'aws-waf-token=64e206b8-54bb-4261-9dea-049898a086a4:CgoAaXSHyyZrAQAA:WV83nxjZrYTh+Ss62ltSB17rt6r2IHmIerfduOdia7rOXilSgVdEVosvVtAWQMK4F+3gYIQdycYyXv5wVQ9p0Kb5EizmekPg6s78/dnDdo32MMBRcHWw06Jc3nG6+InlSI/mJE1nenVkm+QGDVhAOTWeIQxVaM5q7Dt/oXWdT5ZOuqKFJz2mHkCinevd5XsXKIY=;GdprConsent={"version":"IAB TCF 2.0","consentString":"CQH-gkAQH-gkAAGABBENBOFgAAAAAAAAAAZQAAAAAAAA.YAAAAAAAAAAA"};_pulse2data=32ba677d-836d-4eff-a407-eeddf7b82529%2Cv%2C%2C1733062743000%2C%2C%2C0%2Ctrue%2C%2C;_sp_su=false;_pulsesession=%5B%22sdrn%3Aschibsted%3Asession%3Af28be5a4-737e-44a2-80dc-11cc6e86289c%22%2C1732457647472%2C1732457950590%5D;bbsession=id=45729d35-01e2-424e-bece-00cf8f0b67be;bbtracker=id=d6823628-7461-466a-8c0f-3eb9c5059923;consentDate=2024-11-12T03:06:04.992Z;consentUUID=dfb55337-c629-47fa-a4b5-02a008eb06ea_36_37;GdprConsent-Custom=eyJ2ZW5kb3JJZHMiOnt9fQ==',
                      `user-agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36"
                    )
    )
    
    # Tjek statuskode
    if (response$status_code != 200) {
      message("Fejl ved side ", page_number, ": Statuskode ", response$status_code)
      next
    }
    
    # Parse HTML-indhold
    content <- content(response, "text")
    page <- read_html(content)
    carlist <- page %>% html_elements("article")
    
    # Ekstraher data for hver bil
    for (car in carlist) {
      price <- car %>% 
        html_element(pricetag) %>% 
        html_text()
      
      property <- car %>% 
        html_element(propertiestag) %>% 
        html_text()
      
      model <- car %>% 
        html_element(modeltag) %>% 
        html_text()
      
      detail_items <- car %>% 
        html_elements(detail_itemstag) %>% 
        html_text() %>% 
        paste0(collapse = "_")
      
      description <- car %>% 
        html_element(descriptiontag) %>% 
        html_text()
      
      location <- car %>% 
        html_element(locationtag) %>% 
        html_text()
      
      link <- car %>% 
        html_element("a") %>% 
        html_attr("href")
      
      carid <- link %>% 
        str_extract("[0-9]{7}")
      
      # Saml data
      scraped_data <- data.frame(
        Price = price, 
        Details = detail_items, 
        Makemodel = model, 
        Properties = property, 
        Description = description, 
        Location = location, 
        Link = link, 
        CarID = carid, 
        Scrapedate = Sys.time()
      )
      
      # Tilføj til samlet dataframe
      carframe <- rbind(carframe, scraped_data)
    }
    
    # Tilføj en pause på 1 sekund
    Sys.sleep(1)
  }, error = function(e) {
    message("Fejl ved side ", page_number, ": ", e$message)
  })
}

dealerframe <- data.frame(
  CarID = character(),
  DealerCVR = character(),
  DealerAddress = character(),
  DealerID = character()
)

# CSS selectors
cvrtag <- ".bas-MuiSellerInfoComponent-cvr"
addresstag <- ".bas-MuiSellerInfoComponent-address"

# Loop gennem hver bil
for (row in 1:length(carframe$Link)) {
  
  message("Behandler række ", row, " af ", length(carframe$Link))
  
  car_response <- GET(
    url = carframe$Link[row],
    add_headers(
      cookie = 'aws-waf-token=64e206b8-54bb-4261-9dea-049898a086a4:CgoAaXSHyyZrAQAA:WV83nxjZrYTh+Ss62ltSB17rt6r2IHmIerfduOdia7rOXilSgVdEVosvVtAWQMK4F+3gYIQdycYyXv5wVQ9p0Kb5EizmekPg6s78/dnDdo32MMBRcHWw06Jc3nG6+InlSI/mJE1nenVkm+QGDVhAOTWeIQxVaM5q7Dt/oXWdT5ZOuqKFJz2mHkCinevd5XsXKIY=;GdprConsent={"version":"IAB TCF 2.0","consentString":"CQH-gkAQH-gkAAGABBENBOFgAAAAAAAAAAZQAAAAAAAA.YAAAAAAAAAAA"};_pulse2data=32ba677d-836d-4eff-a407-eeddf7b82529%2Cv%2C%2C1733062743000%2C%2C%2C0%2Ctrue%2C%2C;_sp_su=false;_pulsesession=%5B%22sdrn%3Aschibsted%3Asession%3Af28be5a4-737e-44a2-80dc-11cc6e86289c%22%2C1732457647472%2C1732457950590%5D;bbsession=id=45729d35-01e2-424e-bece-00cf8f0b67be;bbtracker=id=d6823628-7461-466a-8c0f-3eb9c5059923;consentDate=2024-11-12T03:06:04.992Z;consentUUID=dfb55337-c629-47fa-a4b5-02a008eb06ea_36_37;GdprConsent-Custom=eyJ2ZW5kb3JJZHMiOnt9fQ==',
      `user-agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36"
    )
  )
  
  # Parse HTML
  car_page <- read_html(content(car_response, "text"))
  
  # Udtræk data
  dealer_cvr <- car_page %>% 
    html_element(cvrtag) %>% 
    html_text() %>% 
    str_extract("[0-9]{8}")
  dealer_cvr <- ifelse(length(dealer_cvr) == 0 || is.na(dealer_cvr), "NA", dealer_cvr)
  
  dealer_address <- car_page %>% 
    html_element(addresstag) %>% 
    html_text()
  dealer_address <- ifelse(length(dealer_address) == 0 || is.na(dealer_address), "NA", dealer_address)
  
  dealer_id <- car_page %>% 
    html_elements("a") %>% 
    html_attr("href") %>%
    str_subset("id\\d+") %>%
    str_extract("(?<=id)\\d+")
  dealer_id <- ifelse(length(dealer_id) == 0 || is.na(dealer_id), "NA", dealer_id)
  
  # Tilføj til dataframe
  dealer_data <- data.frame(
    CarID = carframe$CarID[row],
    DealerCVR = dealer_cvr,
    DealerAddress = dealer_address,
    DealerID = dealer_id
  )
  
  dealerframe <- rbind(dealerframe, dealer_data)
  
  # Opdater carframe
  carframe[row, "DealerCVR"] <- dealer_cvr
  carframe[row, "DealerAddress"] <- dealer_address
  carframe[row, "DealerID"] <- dealer_id
  
  # Kort pause mellem requests
  Sys.sleep(0.2)
}

###############################
####Opgave 1.2 – Rense data####
###############################

Mercedes <- carframe

Mercedes$Brand <- gsub("^(.{1,8}).*", "\\1", Mercedes$Makemodel)

Mercedes$Doors <- gsub(".*(\\d{1}d)$", "\\1", Mercedes$Makemodel)
Mercedes$Doors <- ifelse(Mercedes$Doors %in% c("4d", "5d"), Mercedes$Doors, NA)

Mercedes$Modeltype <- gsub("^.{1,8}\\s(.*)\\s\\d{1}d$", "\\1", Mercedes$Makemodel)
Mercedes <- Mercedes[grepl("EQA250", Mercedes$Modeltype), ] # Fjerner alle de modeller som IKKE er EQA250

Mercedes$Year <- sub("^(\\d+/\\d{4}).*", "\\1", Mercedes$Details)
Mercedes$Year <- ifelse(grepl("^\\d{1,3} km", Mercedes$Year), NA, Mercedes$Year)

Mercedes$Km <- sub(".*?(\\d{1,3}(?:[\\.\\,]\\d{3})*) km.*", "\\1", Mercedes$Details)
Mercedes$Km <- sub("^.*?\\d{1,2}/\\d{4}(\\d{1,3}(?:[\\.\\,]\\d{3})*) km.*", "\\1", Mercedes$Details)
Mercedes$Km <- ifelse(grepl("^0 km\\d+ km rækkeviddeEl$", Mercedes$Details), 0,
                      ifelse(grepl("^100 km\\d+ km rækkeviddeEl$", Mercedes$Details), 100, Mercedes$Km))

Mercedes$Range <- sub(".*?\\d{1,3}(?:[\\.\\,]\\d{3})* km(\\d+).*rækkevidde", "\\1", Mercedes$Details)
Mercedes$Range <- sub("El$", "", Mercedes$Range)
Mercedes$Range <- ifelse(grepl("rækkevidde", Mercedes$Details), Mercedes$Range, NA)

Mercedes$Price <- Mercedes$Price
Mercedes$Price <- gsub("kr.$", "", Mercedes$Price) # Fjerner kr. i slutningen af værdierne
Mercedes$Price <- gsub("kr. \\(Uden afgift\\)", "", Mercedes$Price) # Fjerner kr. (Uden afgift) i slutningen af værdierne
Mercedes$Price <- gsub("kr. \\(Engros/CVR\\)", "", Mercedes$Price) # Fjerner kr. (Engros/CVR) i slutningen af værdierne
Mercedes$Price <- gsub("\\.", "", Mercedes$Price) # Fjerner punktum i form af tusindseparator
Mercedes$Price <- str_trim(Mercedes$Price) # Fjerner overskydende mellemrum
Mercedes$Price <- as.numeric(Mercedes$Price)

Mercedes$CarID <- as.numeric(Mercedes$CarID)

Mercedes <- Mercedes[,-c(2:4)]

Mercedes$Description <- Mercedes$Description %>%
  gsub("\n", ". ", .) %>% # Erstat newline-tegn med ". "
  gsub("\t", " ", .) %>% # Erstat tabulatorer med mellemrum
  gsub("[^\x01-\x7F]", "", .) %>% # Fjern emojis og ikke-alfanumeriske tegn, kun tillad punktum, komma og mellemrum
  gsub("\\s+", " ", .) %>% # Erstat flere mellemrum med ét mellemrum
  trimws() # Fjern eventuelle ledende og afsluttende mellemrum

dealer_cvr <- c(25500938, 20560010, 26988136, 31747945, 25905504, 32478808, 80493215, 48118712, 41042842, 28995288, 
                30175727, 37069434, 37874299, 25935349, 35513043, 35469044, 36953217, 39588781, 28320566, 44896338, 
                34604207, 37646962, 14214178, 73120713, 27211615, 33746504, 27972721, 33150946, 36534850, 33076045, 
                27066860, 38534939, 35530487, 37505757, 16833037, 10109957, 29605858, 58811211, 29838550, 42113883, 
                42504505, 44336383, 44164280, 28859309, 41748206, 38612298, 42534188, 33067607, 65782928, 67044428, 
                28894376, 43160192, 31791588, 38184393, 41763329, 39030152, 26621798, 18036800, 44887029, 20178108, 
                31479622, 39076632, 38772627, 31943140, 33383738, 44883112, 10130115, 38590170, 41340355, 30721853, 
                40593381, 21481483, 44057778, 44895919, 29813841, 10131111, 43327755, 45050912, 20975245, 31748895, 
                32358942, 16957895, 29399190, 26172403, 42664707, 40309306, 25786211, 38494457, 28324537, 33770529, 
                40590692, 33975112, 43848526, 17694707, 39000628, 18930579, 40777822, 35704329, 45006638, 44253283, 
                39041758, 44102552, 42579432, 41373334, 36543604, 38763768, 39542536, 70785412, 24230341, 34087164, 
                14952705, 87554716, 41475145, 28303696, 31857309, 41992786, 41457635, 41696559, 43417126, 14854738, 
                81813418, 21050245, 39795930, 15214678, 27714587, 38320386, 16257303, 44924498, 30484916, 26343003, 
                28572921, 18673339, 40593667, 36974249, 30098315, 44943190, 35037209, 44243350, 16231991, 26003369, 
                32275354, 13724032, 32768040, 37213721, 44010038, 44385171, 44634562, 58893218, 72450728, 29843864, 
                35841350, 44874172, 21797898, 44298252, 18667770, 80144415, 42706922, 33882378, 42854085, 43448269, 
                87626415, 38793012, 25328582, 29798710, 43855999, 39468867, 34466629, 40711864, 33149832, 24220192, 
                19430243, 35860797, 39256282, 28281986, 33249381, 35871128, 40819258, 44435268, 40624791, 42581534, 
                38462857, 28737816)

dealer_names <- c("JØRGEN HANSEN BILER A/S", "Van Mossel Automotive Group Denmark A/S", "MICHAELS AUTOMOBILER ApS", 
                  "REHABILER & BUSSER A/S", "STARMARK I/S", "AUTOC A/S", "P. CHRISTENSEN A/S", "SAND JENSEN AUTOMOBILER A/S", 
                  "Strand Biler A/S", "BEKA AUTO A/S", "RØDKJÆR BILER ApS", "Forza Car Birkerød ApS", "Au2Vest ApS", 
                  "MICA HORSENS ApS", "CarPal ApS", "KOMFORT BILER A/S", "MY GARAGE A/S", "IMBILER ApS", 
                  "C.A. LARSEN AUTOMOBILER A/S", "Fløe Auto ApS", "VIBORG AUTOGÅRD A/S", "Auto Group Nordvest A/S", 
                  "GLOBUS BILER ApS", "GØRLØSE AUTOIMPORT ApS", "ERIK MAIBOM A/S", "JACON BILER A/S", 
                  "MOBILITY SERVICE DANMARK A/S", "SMH BILER ApS", "DANNEVANG AUTO ApS", "BILHUSET LAURSEN A/S", 
                  "Bjørn Caning's Eftf. A/S", "Dansk Bilimport ApS", "BRDR. DAN BILER A/S", "MH Automobiler ApS", 
                  "JK Auto ApS", "LARS HEIN AUTOMOBILER A/S", "C.C. BILER A/S", "EJNER HESSEL A/S", 
                  "LANGER BILER ApS", "Langsø Biler Silkeborg ApS", "Green Garage Nordsjælland ApS", "HAC Biler ApS", 
                  "Brdr. Jensen Sales ApS", "KRAFT BILER A/S", "CL Leasing ApS", "MK Biler ApS", "BILBOEL A/S", 
                  "NIELS JOHANNESEN AUTOMOBILER ApS", "KJÆRSGAARD AUTO. AALBORG A/S", "AUTOCENTRALEN.COM A/S", 
                  "SJÆLLANDS AUTOMÆGLER ApS", "WeLease ApS", "Løvborg Biler (Hvalpsund Færgekiosk)", "Lønquist Auto ApS", 
                  "AM BILHUS A/S", "Autos.dk A/S", "HAMMER-HØYER BILER A/S", "NELLEMANN A/S", "J. T. Biler ApS", 
                  "BØGE'S BILER OG BÅDE ApS", "K.H. AUTOMOBILER A/S", "Octane Automotive ApS", "Import ApS", 
                  "Nielsen Car Group A/S", "Bilgruppen ApS", "VIA BILER A/S", "Østergaard Automobiler ApS", 
                  "Sonne Leasing A/S", "Auto-Centralen Vejle A/S", "LJ BILER A/S", "Schønemann & Brodersen Biler A/S", 
                  "KROGSGAARD-JENSEN A/S", "Aksel.nu ApS", "Car Choice ApS", "AUTOPUNKT.DK A/S", 
                  "LINDING BILER A/S", "Rodi's Autocenter ApS", "Osbech Cars ApS", "BILGÅRDEN HOSTRUP A/S", 
                  "KRAFT BILER HORSENS A/S", "CARCLUB A/S", "PER CHRISTENSEN AUTOMOBILER ApS", "Silkeborg Motor Co ApS",
                  "KARVIL BILER A/S", "Albæk Automobiler ApS", "Morten Hytting Elbiler ApS", "DALSGAARD BILER A/S",
                  "Asbjørn Biler ApS", "RØRBÆK BILER A/S, FREDERIKSHAVN", "AUTOLANDET ApS", "Kring & Nielsen ApS",
                  "KOFOED & THOMSEN A/S", "Arsamanagement - Din bilpartner", "ANDERSEN BILER A/S", "MOTORGAARDEN I SKIVE A/S",
                  "AUTOHUSET VESTERGAARD A/S PERSONVOGNE", "Kloster Biler ApS", "SP Leasing", "Solstar Biler ApS",
                  "Virum Car House", "JS Carline ApS", "Onlinebiler.dk ApS", "Rødbjerg Biler ApS", "AA Bilcenter ApS",
                  "M.P BILER ApS", "Carma Car ApS", "Aaskov.nu ApS", "HENRIK CHRISTENSEN A/S", "Therkildsen Biler ApS",
                  "KARLSEN BILER ApS", "Højbogaard Biler", "BILHUSET. RONALD CARLSEN ODENSE A/S", "Bilhuset CMD ApS",
                  "DIT AUTOCENTER STORVORDE ApS", "BILHUSET KØGE A/S", "Skive Bilhus ApS", "K2 Biler ApS", "Alk Biler ApS",
                  "Signatur Biler ApS", "PEDERSEN & NIELSEN AUTOMOBILFORRETNING A/S", "MAX DUE A/S", "Jensen Bilsalg",
                  "SBN Biler ApS", "BENT PEDERSEN A/S", "Importbilen / Hald&Partners v/Christoffer Hald-Christensen",
                  "Carplace A/S", "ALLAN HANSEN AUTOMOBILER A/S", "Sørby Autocenter ApS", "BILHUSET MBM A/S",
                  "AUTONOVA ApS", "importbiler.dk v/Hans Jørgen Mogensen", "RING BILER ApS", "Wittenkamp Biler & Leasing ApS",
                  "Carmark ApS", "G. BECH HANSEN A/S", "Icar Auto, Silkeborg ApS", "ANDERSEN & MARTINI A/S", "HJA Biler ApS",
                  "TØRRING AUTO A/S", "BILHUSET HJØRRING A/S", "KILDEBILER ApS", "VEJLEBO & LARSEN ApS", "LINDHOLM BILER A/S",
                  "KOBE Leasing A/S", "Hammershøj Biler ApS", "Recars ApS", "ODENSE BILHUS APS", "RÆVHEDE AUTO A/S",
                  "TAGE THOMSEN A/S", "VESTJYSK BILHUS A/S", "BILHUSET BIERSTED A/S", "Auto Find A/S", "IDÈ BILER, SKJERN A/S",
                  "KP Autohandel ApS", "VAMDRUP BILER V/FREDDY CHRISTENSEN", "AUTO-CRAMER A/S", "MP Biler Århus ApS",
                  "LEASE DANMARK ApS", "Holstebro Bilcenter ApS", "CB Biler ApS", "MEHLSEN AUTOMOBILER A/S", "Diluxu ApS",
                  "RAF MOTORS A/S", "HØJLAND BILER A/S", "Alpha Auto Greve ApS", "TA Biler ApS", "BILHUSET LIND ApS",
                  "City Car Leasing ApS", "DK AUTOMOBILER A/S", "LEMVIG AUTOTEKNIK A/S", "OJA Biler ApS", "NORD LEASING ApS",
                  "HolmLy Biler ApS", "AUTOFOKUS DRASTRUP ApS", "AUTOA A/S", "BILCENTRET ROSKILDEVEJ 320-322 TÅSTRUP A/S",
                  "Autoelite ApS", "B&D Biler A/S", "Netto Biler A/S", "NOVOCAR A/S", "Stjernegårdens Biler", "HandiAutos GmbH (DE28737816)")


Mercedes$DealerName <- dealer_names[match(Mercedes$DealerCVR, dealer_cvr)]

Mercedes <- Mercedes[c(
  "CarID", "Brand", "Modeltype", "Doors", "Year", "Km", "Range", 
  "Price", "Description", "Location", "DealerName", 
  "DealerCVR", "DealerID", "DealerAddress", "Link", "Scrapedate"
)]

###############################################
####Opgave 1.3 – Hente nye data - simuleret####
###############################################

Mercedes_simulation <- Mercedes

# 1. Ændre Scrapedate til en dag senere
Mercedes_simulation$Scrapedate <- Mercedes_simulation$Scrapedate + as.difftime(1, units = "days")

# 2. Tilføj 2 nye biler
nye_biler <- data.frame(
  CarID = max(Mercedes_simulation$CarID) + 1:2,
  Brand = sample(Mercedes_simulation$Brand, 2, replace = TRUE),
  Modeltype = sample(Mercedes_simulation$Modeltype, 2, replace = TRUE),
  Doors = sample(Mercedes_simulation$Doors, 2, replace = TRUE),
  Year = sample(Mercedes_simulation$Year, 2, replace = TRUE),
  Km = sample(Mercedes_simulation$Km, 2, replace = TRUE),
  Range = sample(Mercedes_simulation$Range, 2, replace = TRUE),
  Price = sample(Mercedes_simulation$Price, 2, replace = TRUE),
  Description = "Jeg er ikke rigtig hehe",
  Location = sample(Mercedes_simulation$Location, 2, replace = TRUE),
  DealerName = sample(Mercedes_simulation$DealerName[!is.na(Mercedes_simulation$DealerName)], 2, replace = TRUE),
  DealerCVR = sample(Mercedes_simulation$DealerCVR[!is.na(Mercedes_simulation$DealerCVR)], 2, replace = TRUE),
  DealerID = sample(Mercedes_simulation$DealerID, 2, replace = TRUE),
  DealerAddress = sample(Mercedes_simulation$DealerAddress[!is.na(Mercedes_simulation$DealerAddress)], 2, replace = TRUE),
  Link = "https://www.bilbasen.dk/Jeg/er/en/simuleret/bil",
  Scrapedate = as.POSIXct(sample(Mercedes_simulation$Scrapedate, 2, replace = TRUE))
)

Mercedes_simulation <- rbind(Mercedes_simulation, nye_biler)

# 3. Ændr priserne på 3 tilfældige biler
set.seed(123)  # For reproducerbarhed

price_changes <- sample(1:nrow(Mercedes_simulation), 3)  # Vælg 3 tilfældige biler
changed_prices <- Mercedes_simulation$CarID[price_changes]  # CarID på biler med ændret priser
Mercedes_simulation$Price[price_changes] <- Mercedes_simulation$Price[price_changes] * runif(3, 0.9, 1.1)  # ændringer priser med tilfældige faktorer

# 4. Fjern 5 biler for at simulere solgte biler
Mercedes_simulation$solgt <- FALSE # Opret ny kategorivariabel
solgte_biler <- sample(1:nrow(Mercedes_simulation), 5) # Vælg tilfældigt 5 biler som markeres solgt
Mercedes_simulation$solgt[solgte_biler] <- TRUE # Opdater solgt-kolonnen for de "solgte" biler

#####################################
####Opgave 1.4 – Hente tyske data####
#####################################

base_url <- "https://www.autoscout24.de/lst/mercedes-benz/eqa-250?atype=C&cy=D&desc=0&ocs_listing=include&page="
end_url <- "&search_id=140tp5nojo7&sort=standard&source=listpage_pagination&ustate=N%2CU"

categoriesDE <- c("price", "link", "scrapdate")
carframeDE <- as.data.frame(matrix(data = NA, nrow = 0, ncol = 3))
colnames(carframeDE) <- categoriesDE

pricetag <- "[class^='Price']"

for (i in 1:19) {
  page_url <- paste0(base_url, i, end_url)
  response <- GET(page_url,
                  add_headers(
                    cookie = "culture=de-DE; as24Visitor=a4b7942b-b141-47a8-bf73-b644de4b0992; euconsent-v2=CQIiw4AQIiw4AGNAFBDEBQFgAAAAAELgAAAAAAAUIgiAALgAoACoAHAAPAAgABIADIAGgAPAAiABHACZAFIAUwAqgBdADEAG8APwAhABDQCIAIkARwAmgBRgCtAGGAMsAaIA5ABzgDugH4AfsBBwEIAItARwBHQCSgGKAM-AdQA7YB9gD_gIvAR6AkQBMgChwFHgKRAU-AqUBagC2AFyALzAX-AyGBkYGSAMqAZYAy4Bq4DiwHcgPFAhCBOsChAAwSBEAAsACoAHAAPAAggBeAGgAPAAiABMACqAG8APwAhIBDAESAI4ATQAwwBlgDnAHcAPaAfgB-gElASIAocBR4CkQFsALkAZIAzMBq4EIQKEDoFAACwAKgAcABBAC8ANAAeABEACYAFMAKoAXQAxABvAD9AIYAiQBNACjAGGANEAc4A7gB7QD8AP0AiwBHQCSgHUAReAkQBMgChwFHgLYAXIAyQBlQDLAGZgNXAcWQgFAALACqAGIAN4AjgBzgDuAJSAdQBchKAmAAsADgAPAAiABMACqAGKAQwBEgCOAFGAPwA6gCLwEiAKPAWwAyQBlgEISkB0ABYAFQAOAAggBkAGgAPAAiABMACkAFUAMQAfoBDAESAKMAaIA54B-AH6ARYAjoBJQDqAIvASIAocBbAC5AGSAMsAhCWgBgDuAUOAzMAA.YAAAAAAAA4CA; cconsent-v2=%7B%22purpose%22%3A%7B%22legitimateInterests%22%3A%5B25%5D%2C%22consents%22%3A%5B%5D%7D%2C%22vendor%22%3A%7B%22legitimateInterests%22%3A%5B10218%2C10441%2C11006%2C11005%2C11009%5D%2C%22consents%22%3A%5B%5D%7D%7D; addtl_consent=1~; as24-cmp-signature=TRBvR%2FMsZQ8nwa2b6oRuBFSs2L65%2BPpNuygVJO2SmWHb9noD%2BP0cQS3LV%2FIHjWNl%2F7DZWe7s9IASjghKGIMKfDHiN7P%2BGjZqOxTAp%2BXHRm2w75A3p0L92GMqiUFtkze08xNJxuYdP8GZziJ5D5q55EPd6xU8GABVf%2BYjQI7YxX8%3D; ab_test_lp=%7B%7D; user_profile_features=F1%3A0%2CF2%3A0%2CF3%3A0.5%2CF4%3A0.5%2CF5%3A1%2CF6%3A0.25; search-session=true; ab_test_dsp=%7B%7D",
                    `user-agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36"
                  ))
  
  if (response$status_code == 200) {
    responsecontent <- httr::content(response, as = "text")
    page <- read_html(responsecontent)
    carlist <- page %>% html_elements("article")
    
    # Ekstraher information fra hvert bil-element
    for (car in carlist) {
      price <- car %>% html_element(pricetag) %>% html_text()
      link <- car %>% html_element("a") %>% html_attr("href")
      scraped_data <- data.frame(price, link, Sys.time())
      carframeDE <- rbind(carframeDE, scraped_data)
    }
  } else {
    warning(paste("Failed to fetch page", i))
  }
}

carframeDE$link <- paste0("https://www.autoscout24.de", carframeDE$link)

carframeDE <- readRDS("CarDE.rds")

autoframe <- data.frame(
  year = character(),
  km = character(),
  dealername = character(),
  dealeradress = character(),
  milage = character(),
  makemodel = character(),
  modeltype = character(),
  doors = character()
)

for (i in 1:nrow(carframeDE)) {
  car_url <- carframeDE$link[i]
  
  car_page <- tryCatch(
    { read_html(car_url) },
    error = function(e) { NULL }
  )
  
  if (!is.null(car_page)) {
    
    year <- car_page %>%
      html_element(".VehicleOverview_containerMoreThanFourItems__691k2") %>%
      html_text() %>%
      str_extract("\\d{4}")
    
    km <- car_page %>%
      html_elements(".VehicleOverview_itemText__AI4dA") %>%
      html_text() %>%
      .[2] %>%
      str_remove_all("[^0-9]")
    
    dealername <- car_page %>%
      html_element(".TieredPricingRatingsSection_nameContainer__fMSj2") %>%
      html_text()
    
    dealeradress <- car_page %>%
      html_element(".Department_departmentContainer__UZ97C") %>%
      html_text()
    
    milage <- car_page %>%
      html_element(".VehicleOverview_itemText__AI4dA") %>%
      html_text()
    
    makemodel <- car_page %>%
      html_element(".StageTitle_makeModelContainer__RyjBP") %>%
      html_text()
    
    modeltype <- car_page %>%
      html_element(".StageTitle_modelVersion__Yof2Z") %>%
      html_text()
    
    doors <- car_page %>%
      html_element("#basic-details-section dl dt:contains('Türen') + dd") %>%
      html_text()
    
    autoframe <- rbind(
      autoframe, 
      data.frame(
        year = year,
        km = km,
        dealername = dealername,
        dealeradress = dealeradress,
        milage = milage,
        makemodel = makemodel,
        modeltype = modeltype,
        doors = doors
      )
    )
  } else {
    autoframe <- rbind(
      autoframe,
      data.frame(
        year = NA, km = NA,
        dealername = NA, dealeradress = NA,
        milage = NA, makemodel = NA,
        modeltype = NA, doors = NA
      )
    )
  }
}

MercedesDE <- cbind(carframeDE, autoframe)

#################################
####Data cleaning Tyske biler####
#################################

MercedesDE <- readRDS("MercedesDE.rds")

# Tag kun den første pris (før det første komma/første forekomst af €)
MercedesDE$price <- sub("^€\\s*(\\d+[.,]\\d+).*$", "\\1", MercedesDE$price)
MercedesDE$price <- gsub("[.,]", "", MercedesDE$price)
MercedesDE$price <- as.numeric(MercedesDE$price)

MercedesDE$km <- as.numeric(MercedesDE$km)
names(MercedesDE)[names(MercedesDE) == "km"] <- "range"

MercedesDE$year <- as.integer(MercedesDE$year)

MercedesDE$brand <- gsub("^(Mercedes).*", "\\1", MercedesDE$makemodel)

MercedesDE$makemodel <- NULL

# Tilføj 'd' til døre
MercedesDE$doors <- ifelse(!is.na(MercedesDE$doors), 
                                 paste0(MercedesDE$doors, "d"), 
                                 MercedesDE$doors)

MercedesDE$milage <- gsub(" km", "", MercedesDE$milage)  # Fjerner "km" efter tallet
names(MercedesDE)[names(MercedesDE) == "milage"] <- "km"

# grepL modeltype så det matcher modeltype i 'Mercedes'
MercedesDE$modeltype <- case_when(
  grepl("PROG\\+|Progressive|PROGRESSIVE", MercedesDE$modeltype, ignore.case = TRUE) & 
    !grepl("Advanced|AMG|Edition", MercedesDE$modeltype) ~ "EQA250+Progressive",
  grepl("AMG Line|AMG\\+", MercedesDE$modeltype) ~ "EQA250+AMG Line",
  grepl("AMG Edition", MercedesDE$modeltype) ~ "EQA250+AMG Edition",
  grepl("Progressive Advanced", MercedesDE$modeltype) ~ "EQA250Progressive Advanced",
  grepl("Electric Art", MercedesDE$modeltype) ~ "EQA250+Electric Art",
  TRUE ~ MercedesDE$modeltype
)

# Omregning af priser fra EUR til DKK
getSymbols("EURDKK=X", src="yahoo") # Henter valutakurs data på EUR/DKK

eur_to_dkk <- as.numeric(Cl(last(`EURDKK=X`))) # danner vektor med den seneste lukkekurs

MercedesDE$price_dkk <- round(MercedesDE$price * eur_to_dkk) # Konverter priser fra EUR til DKK

MercedesDE <- MercedesDE[, c("brand", "modeltype", "doors", "year", "km", "range", "price", "price_dkk", "dealeradress", "dealername", "link", "Sys.time..")]

car_colnamesDE <- c("Brand", "Modeltype", "Doors", "Year", "Km", "Range", "Price EUR", "Price DKK", "Location", "DealerName", "Link", "Scrapedate")

colnames(MercedesDE) <- car_colnamesDE

##########################################
####Kombinér datasæt for DK & DE biler####
##########################################

MercedesDK <- Mercedes

MercedesDK <-  MercedesDK [,c(2:8,10:11,15:16)]

MercedesDK$CountryCode <- "DK"

MercedesDK$Year <- ifelse(grepl("/", MercedesDK$Year),
                          year(mdy(MercedesDK$Year)),  # Håndterer "MM/YYYY"
                          as.factor(MercedesDK$Year))  # Håndterer "YYYY"

names(MercedesDE)[names(MercedesDE) == "Price DKK"] <- "Price"

MercedesDE$CountryCode <- "DE"

MercedesDE <- MercedesDE[,-7]

MercedesDKvsDE <- rbind(MercedesDK, MercedesDE)

MercedesDKvsDE <- MercedesDKvsDE[!is.na(MercedesDKvsDE$Year), ]

rownames(MercedesDKvsDE) <- NULL

####################################################
####Grafisk illustration af prisforskel DK vs DE####
####################################################

# Beregn gennemsnitsprisen per Year og CountryCode
MercedesAvgPrice <- aggregate(Price ~ Year + CountryCode, data = MercedesDKvsDE, FUN = function(x) round(mean(x)))

MercedesAvgPrice_matrix <- reshape(MercedesAvgPrice, 
                                   idvar = "Year", 
                                   timevar = "CountryCode", 
                                   direction = "wide")

colnames(MercedesAvgPrice_matrix) <- sub("Price.", "", colnames(MercedesAvgPrice_matrix))

ggplot(MercedesAvgPrice, aes(x = factor(Year), y = Price, fill = CountryCode)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +  # Juster søjlernes bredde (0.6 for lidt smallere)
  geom_text(aes(label = paste(format(Price, big.mark = ".", decimal.mark = ","), "kr.")), 
            position = position_dodge(width = 0.6), vjust = -0.5, size = 4) +  # Juster tekstens position
  labs(title = "Gennemsnitspris på Mercedes EQA250 i Tyskland vs. Danmark",
       x = "År",
       y = "Pris (i kr.)",
       fill = "Landekode") +
  theme_minimal() +  # Ren og simpel grafik
  scale_fill_manual(values = c("DK" = "#ff0041", "DE" = "#FFC814")) +  # Farver for landekoder
  scale_y_continuous(labels = label_comma(), 
                     breaks = seq(0, 450000, by = 50000),  # Y-aksens intervaller
                     limits = c(0, 450000))  # Sæt y-aksen op til 500.000

########################################
####Sammenligning af bil fra DK & DE####
########################################

Mercedes2024DKvsDE <- MercedesDKvsDE[c(15, 761),]

Mercedes2024DKvsDE$Range <- as.numeric(Mercedes2024DKvsDE$Range)
Mercedes2024DKvsDE$Km <- as.numeric(gsub("\\.", "", Mercedes2024DKvsDE$Km))

# Graf over 2 tilfældige EQA250 AMG
ggplot(Mercedes2024DKvsDE, aes(x = as.factor(Year), fill = CountryCode)) +
  geom_bar(aes(y = Price), stat = "identity", position = "dodge", width = 0.7) +
  geom_bar(aes(y = Range * 1000), stat = "identity", position = "dodge", width = 0.7, alpha = 0.5) +
  labs(title = "Mercedes EQA250 AMG fås markant billigere i Tyskland!",
       x = "År",
       y = "Pris (i kr.)",
       fill = "Landekode") +
  theme_minimal() +
  scale_fill_manual(values = c("DK" = "#ff0041", "DE" = "#FFC814")) +
  scale_y_continuous(labels = label_comma(big.mark = ".", decimal.mark = ","), 
                     breaks = seq(0, max(500000, na.rm = TRUE), by = 50000)) +
  geom_text(aes(y = Price, label = paste0(format(Price, big.mark = ".", decimal.mark = ","), " kr")), 
            position = position_dodge(width = 0.7), vjust = -0.5) +
  geom_text(aes(y = Range * 1000, label = paste0("Range: ", Range, " km")), 
            position = position_dodge(width = 0.7), vjust = -0.5, color = "black") +
  geom_text(aes(y = Km, label = paste0("Km: ", Km)), 
            position = position_dodge(width = 0.7), vjust = -0.5, color = "black")

##############################################################################
####Opgave 2.1-2.2 – Oprette skemaet for bilbasen/Gemme bilerne i database####
##############################################################################

#1. Indlæs RDS fil med webscraping data
Mercedes <- readRDS(file = "Mercedes.rds")

#2. Opret tabeller til MySQL
car_history <- Mercedes[,c(1,8,16)]
car_history$Solgt <- FALSE
car <- Mercedes[,c(1:7,9,13)]
dealer <- Mercedes[,c(10:15)]

## Fjern duplikater da DealerID kun skal indlæses unikt i databasen i MySQL
dealer <- dealer[!duplicated(dealer$DealerID), ]

#3. Opret forbindelse til MySQL database
connection <- dbConnect(MariaDB(),
                        db = "bilbasen",
                        host = "localhost",
                        port = 3306,
                        user = "root",
                        password = "loubani1045")

#4. Indlæs tabeller i MySQL databasen navngivet "bilbasen"
dbWriteTable(connection,"car_history",car_history)
dbWriteTable(connection,"car",car)
dbWriteTable(connection,"dealer",dealer)

# Hvis forbindelsen skal afbrydes til MySQL - kør nedenstående
#dbDisconnect(connection)

# Behov for at indsætte ny data manuelt fra R i stedet for MySQL - Se eksempel nedenfor
#insert_query <- "INSERT INTO empcar (carid, rentdate, empid) 
#VALUES (5895865, CURRENT_DATE(), 7369)"
#dbExecute(connection, insert_query)

####################################################################
####Opgave 2.3 – Opdatere databasen ud fra den simulerede kørsel####
####################################################################

Mercedes_simulation <- readRDS("Mercedes_simulation.rds")

# Tilføj simuleret data til database i MySQL for at versionere opdatering/ændring i data.
car_history_simulation <- Mercedes_simulation[,c(1,8,16:17)]
car_simulation <- Mercedes_simulation[,c(1:7,9,13)]
dealer_simulation <- Mercedes_simulation[,c(10:15)]

dealer_simulation <- dealer_simulation[!duplicated(dealer_simulation$DealerID), ]

dbWriteTable(connection, "car_history", car_history_simulation, append = TRUE, row.names = FALSE)
dbWriteTable(connection, "car", car_simulation, overwrite = TRUE, row.names = FALSE)
dbWriteTable(connection, "dealer", dealer_simulation, overwrite = TRUE, row.names = FALSE)

saveRDS(car_history, file = "car_history.rds")
saveRDS(car, file = "car.rds")
saveRDS(dealer, file = "dealer.rds")

saveRDS(car_history_simulation, file = "car_history_simulation.rds")
saveRDS(car_simulation, file = "car_simulation.rds")
saveRDS(dealer_simulation, file = "dealer_simulation.rds")

## Join funktion i R

query <- "
  SELECT dealer.DealerID, dealer.DealerName, dealer.Location, COUNT(car_history.CarID) AS AntalBilerSolgt
  FROM dealer
  INNER JOIN car ON dealer.DealerID = car.DealerID
  INNER JOIN car_history ON car.CarID = car_history.CarID
  WHERE car_history.Solgt = 1
  GROUP BY dealer.DealerID, dealer.DealerName, dealer.Location
  ORDER BY AntalBilerSolgt DESC;
"

# Udfør forespørgslen
solgte_biler <- dbGetQuery(connection, query)

## som funktion

# Definer funktion til at hente solgte biler for en given DealerID
get_sold_cars <- function(dealerID) {
  query <- paste0("
    SELECT dealer.DealerID, dealer.DealerName, dealer.Location, COUNT(car_history.CarID) AS AntalBilerSolgt
    FROM dealer
    INNER JOIN car ON dealer.DealerID = car.DealerID
    INNER JOIN car_history ON car.CarID = car_history.CarID
    WHERE car_history.Solgt = 1 AND dealer.DealerID = ", dealerID, "
    GROUP BY dealer.DealerID, dealer.DealerName, dealer.Location;
  ")
  
  # Udfør SQL-forespørgslen
  sold_cars <- dbGetQuery(connection, query)
  
  # Returner resultaterne
  return(sold_cars)
}

dealerID_example <- 10550
sold_cars_for_dealer <- get_sold_cars(dealerID_example)

print(sold_cars_for_dealer)

## med dplyr og join() i R

# Definer funktion til at hente solgte biler for en given DealerID ved hjælp af dplyr
get_sold_cars_dplyr <- function(dealerID) {
  
  # Hent data fra 'car', 'car_history', og 'dealer' tabellerne ved hjælp af tbl()
  car_history_tbl <- tbl(connection, "car_history")
  car_tbl <- tbl(connection, "car")
  dealer_tbl <- tbl(connection, "dealer")
  
  # Udfør join og filtrer efter DealerID
  sold_cars <- car_history_tbl %>%
    inner_join(car_tbl, by = "CarID") %>% 
    inner_join(dealer_tbl, by = "DealerID") %>%
    filter(Solgt == 1, DealerID == dealerID) %>%
    group_by(DealerID, DealerName, Location) %>%
    summarise(AntalBilerSolgt = n()) %>%
    collect()  # Saml resultaterne tilbage til R
  
  # Returner resultaterne
  return(sold_cars)
}

# Eksempel på brug af funktionen
dealerID_example <- 10550  # Indsæt ønsket DealerID her
sold_cars_for_dealer <- get_sold_cars_dplyr(dealerID_example)

# Se resultaterne
print(sold_cars_for_dealer)


###############################################
####Opgave 2.4 – Scrape & SQL med Miljødata####
###############################################

user_agent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36"

baseurl <- "https://envs2.au.dk/Luftdata/Presentation/table/Copenhagen/HCAB"

GET_response <- GET(url = baseurl)

GET_response$status_code

GET_content <- httr::content(GET_response, as = "text")

page <- read_html(GET_content)

token <- read_html(GET_content) %>% 
  html_element("input[name='__RequestVerificationToken']") %>% 
  html_attr("value")

################################################################################
maintable_HCAB <- "https://envs2.au.dk/Luftdata/Presentation/table/MainTable/Copenhagen/HCAB"

POST_HCAB <- POST(
  url = maintable_HCAB,
  add_headers(
    `User-Agent` = user_agent
  ),
  body = list(`__RequestVerificationToken` = token), encode = "form")

html_HCAB <- content(POST_HCAB, as = "text")

HCAB_tabel <- read_html(html_HCAB) %>% 
  html_element("table") %>% 
  html_table()

################################################################################
maintable_HCAB <- "https://envs2.au.dk/Luftdata/Presentation/table/MainTable/Copenhagen/HCAB"

POST_HCAB <- POST(
  url = maintable_HCAB,
  add_headers(
    `User-Agent` = user_agent
  ),
  body = list(`__RequestVerificationToken` = token), encode = "form")

html_HCAB <- content(POST_HCAB, as = "text")

HCAB_tabel <- read_html(html_HCAB) %>% 
  html_element("table") %>% 
  html_table()

################################################################################
maintable_ANHO <- "https://envs2.au.dk/Luftdata/Presentation/table/MainTable/Rural/ANHO"

POST_ANHO <- POST(
  url = maintable_ANHO,
  add_headers(
    `User-Agent` = user_agent
  ),
  body = list(`__RequestVerificationToken` = token), encode = "form")

html_ANHO <- content(POST_ANHO, as = "text")

ANHO_tabel <- read_html(html_ANHO) %>% 
  html_element("table") %>% 
  html_table()

################################################################################
maintable_AARH3 <- "https://envs2.au.dk/Luftdata/Presentation/table/MainTable/Aarhus/AARH3"

POST_AARH3 <- POST(
  url = maintable_AARH3,
  add_headers(
    `User-Agent` = user_agent
  ),
  body = list(`__RequestVerificationToken` = token), encode = "form")

html_AARH3 <- content(POST_AARH3, as = "text")

AARH3_tabel <- read_html(html_AARH3) %>% 
  html_element("table") %>% 
  html_table()

################################################################################
maintable_RISOE <- "https://envs2.au.dk/Luftdata/Presentation/table/MainTable/Rural/RISOE"

POST_RISOE <- POST(
  url = maintable_RISOE,
  add_headers(
    `User-Agent` = user_agent
  ),
  body = list(`__RequestVerificationToken` = token), encode = "form")

html_RISOE <- content(POST_RISOE, as = "text")

RISOE_tabel <- read_html(html_RISOE) %>% 
  html_element("table") %>% 
  html_table()

################################################################################

connection <- dbConnect(MariaDB(),
                        db = "luftdata",
                        host = "localhost",
                        port = 3306,
                        user = "root",
                        password = "loubani1045")

luftdata_Anholt_27_11_24$`Målt (starttid)` <- format(strptime(luftdata_Anholt_27_11_24$`Målt (starttid)`, "%d-%m-%Y %H:%M"), "%Y-%m-%d %H:%M")
luftdata_Anholt_28_11_24$`Målt (starttid)` <- format(strptime(luftdata_Anholt_28_11_24$`Målt (starttid)`, "%d-%m-%Y %H:%M"), "%Y-%m-%d %H:%M")

luftdata_KBH_27_11_24$`Målt (starttid)` <- format(strptime(luftdata_KBH_27_11_24$`Målt (starttid)`, "%d-%m-%Y %H:%M"), "%Y-%m-%d %H:%M")
luftdata_KBH_28_11_24$`Målt (starttid)` <- format(strptime(luftdata_KBH_28_11_24$`Målt (starttid)`, "%d-%m-%Y %H:%M"), "%Y-%m-%d %H:%M")

luftdata_Risø_27_11_24$`Målt (starttid)` <- format(strptime(luftdata_Risø_27_11_24$`Målt (starttid)`, "%d-%m-%Y %H:%M"), "%Y-%m-%d %H:%M")
luftdata_Risø_28_11_24$`Målt (starttid)` <- format(strptime(luftdata_Risø_28_11_24$`Målt (starttid)`, "%d-%m-%Y %H:%M"), "%Y-%m-%d %H:%M")

luftdata_Århus_27_11_24$`Målt (starttid)` <- format(strptime(luftdata_Århus_27_11_24$`Målt (starttid)`, "%d-%m-%Y %H:%M"), "%Y-%m-%d %H:%M")
luftdata_Århus_28_11_24$`Målt (starttid)` <- format(strptime(luftdata_Århus_28_11_24$`Målt (starttid)`, "%d-%m-%Y %H:%M"), "%Y-%m-%d %H:%M")

dbWriteTable(connection,"Anholt",luftdata_Anholt_27_11_24)
dbWriteTable(connection,"Anholt_ny",luftdata_Anholt_28_11_24)
dbWriteTable(connection,"København",luftdata_KBH_27_11_24)
dbWriteTable(connection,"København_ny",luftdata_KBH_28_11_24)
dbWriteTable(connection,"Risø",luftdata_Risø_27_11_24)
dbWriteTable(connection,"Risø_ny",luftdata_Risø_28_11_24)
dbWriteTable(connection,"Århus",luftdata_Århus_27_11_24)
dbWriteTable(connection,"Århus_ny",luftdata_Århus_28_11_24)

######################################
####Opgave 3 – Analyse af logfiler####
######################################

#############################################
####Opgave 3.1 – Rapport fra en webserver####
#############################################

#setwd("/Users/sevimkilinc/Documents/Dataprojekter/OLA/OLA4/logfiler")
#file.exists("/Users/sevimkilinc/Documents/Dataprojekter/OLA/OLA4/logfiler") # hvis din sti fucker

Logfiles <- list.files(pattern = "access*", path = ".")
log_content <- lapply(Logfiles, readLines)

først_log_df=as.data.frame(log_content[[1]])

# Kombiner indholdet af alle logfiler
all_logs <- do.call(c, log_content)

extract_log_data <- function(raw_log) {
  ip <- str_extract(raw_log, "^\\S+") # Første ord: IP-adresse
  status <- str_extract(raw_log, "\\s\\d{3}\\s") # HTTP-statuskode
  path <- str_extract(raw_log, "\\\"(GET|POST|HEAD)\\s(.*?)\\sHTTP") # Path fra forespørgsel
  time <- str_extract(raw_log, "\\[(.*?)\\]") # Tidsstempel
  return(c(ip, time, path, status))
}

# Parse loglinjer og opret en data frame
parsed_logs <- t(sapply(all_logs, extract_log_data))
colnames(parsed_logs) <- c("IP", "Time", "Path", "Status")
logs_prep <- as.data.frame(parsed_logs, stringsAsFactors = FALSE)

logs_prep$Status <- as.numeric(logs_prep$Status)


# Fjern eventuelle rownames i logs_prep
rownames(logs_prep) <- NULL

# Ekstrahér dato fra Time-kolonnen
logs_prep$Date <- sub(":(.*)", "", logs_prep$Time) # Fjern tid fra datoen
logs_prep$Date <- gsub("\\[", "", logs_prep$Date) # Fjern venstre parentes
logs_prep$Date <- as.Date(logs_prep$Date, format = "%d/%b/%Y") # Konverter til Date-type

# Ekstrahér alene tid (hh:mm:ss) fra Time-kolonnen
logs_prep$Exacttime <- sub("^.*:(\\d{2}:\\d{2}:\\d{2}).*", "\\1", logs_prep$Time)

structured_logs <- logs_prep

#############################################
# Aktive IP-adresser og deres forekomster
#############################################

# Optælling af unikke IP-adresser pr. dato
active_ips_per_day <- aggregate(IP ~ Date, data = structured_logs, FUN = function(x) length(unique(x)))
colnames(active_ips_per_day) <- c("Date", "UniqueIPs")

# Optælling af forekomster for hver IP-adresse
IP_antal <- table(structured_logs$IP)
sorted_IP_antal <- sort(IP_antal, decreasing = TRUE)
Antal_forekomster_IP <- as.data.frame(sorted_IP_antal)
colnames(Antal_forekomster_IP) <- c("IP", "Antal")

# Visualisering af de mest aktive IP-adresser
Top_aktive_IPs <- head(Antal_forekomster_IP, 15)
ggplot(Top_aktive_IPs, aes(x = reorder(IP, -Antal), y = Antal, fill = IP)) +
  geom_bar(stat = "identity") +
  labs(title = "Hypigst IP-aktivitet: Få adresser genererer størstedelen af trafikken",
       x = "IP-adresse",
       y = "Antal forekomster") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold"),
        legend.position = "none")


######################################################
## Mistænksomme HTTP-forespørgsler (statuscode 404) ##
######################################################

logs_404 <- structured_logs[structured_logs$Status == 404, ]
Path_404_summary <- aggregate(Status ~ Path + IP, data = logs_404, FUN = length)
colnames(Path_404_summary) <- c("Path", "IP", "Count404")

# Sortér stier efter antal fejl (mest til mindst)
sorted_path_404 <- Path_404_summary[order(-Path_404_summary$Count404), ]
rownames(sorted_path_404) <- NULL

Top_10_suspicious <- head(sorted_path_404, 10)
suspicious_ips <- unique(Top_10_suspicious$IP)

###########################################################
## Hent IP-data fra ipinfo.io og generer et Leaflet-kort ##
###########################################################

# Definer liste over de 10 mistænksomme IP'er
top_10_suspicious_IPs <- c(
  "93.162.98.150", "5.179.80.204", "83.97.73.87", 
  "170.64.220.120", "157.90.209.77", "159.100.22.187", 
  "162.240.239.98", "5.179.80.205"
)

# Funktion til at hente IP-data
fetch_ip_details <- function(ip) {
  response <- GET(paste0("https://ipinfo.io/", ip, "/json"))
  if (status_code(response) == 200) {
    data <- fromJSON(rawToChar(response$content))
    loc <- strsplit(data$loc, ",")[[1]]
    return(data.frame(
      IP = ip,
      City = data$city,
      Region = data$region,
      Country = data$country,
      Latitude = as.numeric(loc[1]),
      Longitude = as.numeric(loc[2]),
      stringsAsFactors = FALSE
    ))
  }
  return(NULL)
}

# Hent data for de  mistænksomme IP'er
top_suspicious_IP_details <- do.call(rbind, lapply(top_10_suspicious_IPs, fetch_ip_details))

# Fjern dubletter baseret på Latitude og Longitude
top_suspicious_IP_details <- top_suspicious_IP_details[!duplicated(top_suspicious_IP_details[, c("Latitude", "Longitude")]), ]

#####################################
## Opret Leaflet-kort med IP-data ##
#####################################

leaflet(data = top_suspicious_IP_details) %>%
  addProviderTiles("Esri.WorldImagery") %>%  # Esri kort med jordfarver
  addCircleMarkers(
    lng = ~Longitude, lat = ~Latitude,
    label = ~paste("IP:", IP),
    labelOptions = labelOptions(
      noHide = TRUE,
      direction = "top",
      textsize = "12px",
      style = list("color" = "black", "font-weight" = "bold")
    ),
    color = "red", radius = 6, fillOpacity = 0.8
  ) %>%
  setView(lng = 20, lat = 55, zoom = 4) %>%
  addControl(
    html = "<h3 style='color:blue; text-align:center;'>Globale hotspots for potentielt skadelig IP-aktivitet</h3>",
    position = "topright"
  )



################################################################
### ekstra observation: inddrages i mistænksomme request   #####
################################################################

# ud kolonnen statuskode, er det 200 observationer som er lig 12.289 af hele 15.000 observationer dvs. 81.9%
round(sum(structured_logs$Status == 200) / nrow(structured_logs) * 100, 1)

Top_1_active_IP <- "192.0.102.40" 

# Hent oplysninger for IP-adressen
response <- GET(paste0("https://ipinfo.io/", Top_1_active_IP, "/json"))
data <- fromJSON(rawToChar(response$content))
cat(paste("IP:", data$ip, "\nCITY:", data$city, "\nREGION:", 
          data$region, "\nCOUNTRY:", data$country, "\nLOCATION:", data$loc, "\n"))

###################################
# Geografisk placering af IP
###################################

# Ekstraher koordinater
coords <- strsplit(data$loc, ",")[[1]]
latitude <- as.numeric(coords[1])
longitude <- as.numeric(coords[2])

# Hent kortdata for USA
state_map <- map_data("state")

# Data for punktet (IP-adressen)
point_data <- data.frame(longitude = longitude, latitude = latitude)

# Tilføj fiktive værdier til gradientfyld (ens for hele kortet)
state_map$value <- runif(nrow(state_map), 1, 100)

# Visualiser IP-adressens placering med gradientfyld
ggplot() +
  geom_polygon(data = state_map, aes(x = long, y = lat, group = group, fill = value),
               color = "white", size = 0.2) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +  # Gradient uden forklaring
  geom_point(data = point_data, aes(x = longitude, y = latitude),
             color = "red", size = 3) +
  geom_label(data = point_data, aes(x = longitude, y = latitude, label = "192.0.102.40"),
             color = "white", size = 4, vjust = -1, fontface = "bold", fill = "black", label.size = 0) +
  coord_fixed(xlim = c(-130, -60), ylim = c(20, 55)) +  # Fokus på USA
  theme_minimal() +
  labs(
    title = "USA topper listen med den mest aktive IP-adresse:"
  ) +
  annotate("text", x = -125, y = 25, label = paste(
    "IP: 192.0.102.40\n",
    "CITY: Ashburn\n",
    "REGION: Virginia\n",
    "COUNTRY: US\n",
    "LOCATION: 39.0437,-77.4875"
  ),
  hjust = 0, color = "black", size = 4, fontface = "bold") +
  theme(
    legend.position = "none",  
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
  )