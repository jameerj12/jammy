
library(sf)
library(leaflet)
library(ggplot2)
library(tidyverse)
library(fuzzyjoin)
library(readr)
library(tidyverse)

Pac_data <- read_delim("C:/Users/lordo/Documents/R Notes and Info/New/webk22.txt", col_names =FALSE, delim="|")

Pac_to_indiv <- read_delim("C:/Users/lordo/Documents/R Notes and Info/Afternoon data sets/Campaign finance open secrets/pacs22.txt", col_names =FALSE, delim="|")

pac_to_pac <- read_delim("C:/Users/lordo/Documents/R Notes and Info/Afternoon data sets/Campaign finance open secrets/pac_other22.txt", col_names =FALSE, delim="|")


cds118 <- st_read("C:/Users/lordo/Documents/R Notes and Info/Afternoon data sets/Congressional distrcits")
cds118 <- st_transform(cds118, 4326)


#Find a data set that groups PACs by types
Candidate_open_secret<- read_delim("C:/Users/lordo/Documents/R Notes and Info/Afternoon data sets/Campaign finance open secrets/cands22.txt", col_names=FALSE, delim="|")




#Find a census of districts by Race
race <- cds118 %>% select(STATEFP20,CD118FP,NAMELSAD20)

racial_majority_districts <- race %>% filter(STATEFP20 %in% c("01","04","06","12","13","15", "17", "22", "24",
                                                              "26", "28", "34", "35", "36", "39",
                                                              "42", "45", "47", "48")) 

#Create a dataset for Black Majority districts

Black_majority <- racial_majority_districts %>% 
  filter(
    (STATEFP20 %in% c("01", "12", "13", "17", "22", "24", "26", "28", "34", "36", "39", "42", "45", "47")) &  
      (CD118FP %in% c("01", "02","03", "04", "05","06", "07","08","09", "10","11", "13", "20", "24"))
  )


Black_majority <- Black_majority %>%
  mutate(
    STATEFP20 = case_when(
      STATEFP20 == "01" ~ "Alabama",
      STATEFP20 == "12" ~ "Florida",
      STATEFP20 == "13" ~ "Georgia",
      STATEFP20 == "17" ~ "Illinois",
      STATEFP20 == "22" ~ "Louisiana",
      STATEFP20 == "24" ~ "Maryland",
      STATEFP20 == "26" ~ "Michigan",
      STATEFP20 == "28" ~ "Mississippi",
      STATEFP20 == "34" ~ "New Jersey",
      STATEFP20 == "36" ~ "New York",
      STATEFP20 == "39" ~ "Ohio",
      STATEFP20 == "42" ~ "Pennsylvania",
      STATEFP20 == "45" ~ "South Carolina",
      STATEFP20 == "47" ~ "Tennessee",
      TRUE ~ STATEFP20  
    )
  )
black_majority_selected <- Black_majority[c(7, 20, 21,23, 25, 26,32,34,35,47,55,58,71,73,85,94,95,111,115,130, 140),]




#Create a data set for Asian Majority


Asian_majority <- racial_majority_districts %>% 
  filter(
    (STATEFP20 %in% c("06", "15", "36")) &  
      (CD118FP %in% c("01", "17", "06"))
  )




Asian_majority <- Asian_majority %>%
  mutate(
    STATEFP20 = case_when(
      STATEFP20 == "06" ~ "California",
      STATEFP20 == "15" ~ "Hawaii",
      STATEFP20 == "36" ~ "New York",
      TRUE ~ STATEFP20  
    ))

Asian_majority_selected <- Asian_majority[c(3, 4, 6),]  



cds118<- cds118 %>%
  mutate(
    STATEFP20 = case_when(
      STATEFP20 == "01" ~ "Alabama",
      STATEFP20 == "12" ~ "Florida",
      STATEFP20 == "13" ~ "Georgia",
      STATEFP20 == "17" ~ "Illinois",
      STATEFP20 == "22" ~ "Louisiana",
      STATEFP20 == "24" ~ "Maryland",
      STATEFP20 == "26" ~ "Michigan",
      STATEFP20 == "28" ~ "Mississippi",
      STATEFP20 == "34" ~ "New Jersey",
      STATEFP20 == "39" ~ "Ohio",
      STATEFP20 == "42" ~ "Pennsylvania",
      STATEFP20 == "45" ~ "South Carolina",
      STATEFP20 == "47" ~ "Tennessee",
      STATEFP20 == "04" ~ "Arizona",
      STATEFP20 == "06" ~ "California",
      STATEFP20 == "12" ~ "Florida",
      STATEFP20 == "17" ~ "Illinois",
      STATEFP20 == "34" ~ "New Jersey",  
      STATEFP20 == "35" ~ "New Mexico",
      STATEFP20 == "36" ~ "New York",
      STATEFP20 == "48" ~ "Texas",
      STATEFP20 == "15" ~ "Hawaii",
      TRUE ~ STATEFP20  ))











# 01= Alabama 12= Florida, 13= Georgia, 17= Illinois 22= Louisiana 24= Maryland 26= Michigan 28= Mississippi 34= New Jersey 36= New York 39= Ohio 42= Pennsylvania 45= South Carolina 47=Tennesse
#06= California 15 Hawaii, 35=New Mexico 48= Texas Arizona=4

#Hispanic/Latino Majority
Latino_majority <- racial_majority_districts %>% 
  filter(
    (STATEFP20 %in% c("03", "04", "06", "12", "17", "34", "35", "36", "48")) & 
      (CD118FP %in% c("03", "07", "40", "34", "38", "35", "44", "51", "29", "31", "46", "32","16", 
                      "41","20", "21", "25","27","26","04","08","02","31","32","34","35","36","41", "15", "16", "28", "29", "27", "20", "33", "23", "35" )))

c("2", "3", "4", "7", "8", "15", "16", "20", "21", "23", "25", "26", "27", "28", "29", "31", "32", "33", "34", "35", "36", "38", "40", "41", "44", "46", "51")




Latino_majority <- Latino_majority %>%
  mutate(
    STATEFP20 = case_when(  STATEFP20 == 
                              "04" ~ "Arizona",
                            STATEFP20 == "06" ~ "California",
                            STATEFP20 == "12" ~ "Florida",
                            STATEFP20 == "17" ~ "Illinois",
                            STATEFP20 == "34" ~ "New Jersey",  
                            STATEFP20 == "35" ~ "New Mexico",
                            STATEFP20 == "36" ~ "New York",
                            STATEFP20 == "48" ~ "Texas",
                            TRUE ~ STATEFP20  
    )
  )

Latino_majority_selected <- Latino_majority[c(2, 4, 7, 8, 26, 12,13,16,28, 15, 29, 30, 19, 20, 31, 32, 42, 43,44, 49, 58, 59,
                                              67, 77,91, 92, 78,81, 82, 83, 85, 87, 88 ),]    











#Change Names in Pac_to_indiv

Pac_to_indiv_name <- Pac_to_indiv %>% 
  mutate(X16 = case_when(
    X16 == 'H2NY10092' ~ 'Hakeem Jeffries',
    X16 == 'H6TN09068' ~ 'Steve Cohen',
    X16 == 'H0AL07086' ~ 'Terri Sewell',
    X16 == 'H4MS02068' ~ 'Bennie Thompson',
    X16 == 'H2LA02149' ~ 'Troy Carter',
    X16 == 'H6GA04129' ~ 'Hank Johnson',
    X16 == 'H0GA05301' ~ 'Nikema Williams',
    X16 == 'H2GA13012' ~ 'David Scott',
    X16 == 'H2SC02042' ~ 'James Clyburn',
    X16 == 'H2IL02172' ~ 'Robin Kelly',
    X16 == 'H8MI13250' ~ 'Rashida Tlaib',
    X16 == 'H6PA02171' ~ 'Dwight Evans',
    X16 == 'H6MD07020' ~ 'Kweisi Mfume',
    X16 == 'H8FL20032' ~ 'Sheila Cherfilus-Mccormick',
    X16 == 'H6MD04209' ~ 'Anthony Brown',
    X16 == 'H2OH11169' ~ 'Shontel Brown',
    X16 == 'H2GA02031' ~ 'Sanford Bishop',
    X16 == 'H2NJ10154' ~ 'Donald Jr Payne',
    X16 == 'H2IL01042' ~ 'Bobby L Rush',
    X16 == 'H0FL17068' ~ 'Frederica Wilson',
    X16 == 'H4NY11138' ~ 'Yvette D Clarke',
    X16 == 'H2CA33048' ~ 'Lucille Roybal-Allard',
    X16 == 'H6TX15162' ~ 'Vicente Gonzalez',
    X16 == 'H8TX16109' ~ 'Veronica Escobar',
    X16 == 'H2TX23082' ~ 'Henry Cuellar',
    X16 == 'H8CA34266' ~ 'Jimmy Gomez',
    X16 == 'H8TX29052' ~ 'Sylvia Garcia',
    X16 == 'H2CA39078' ~ 'Linda Sanchez',
    X16 == 'H8IL04134' ~ 'Jesus Garcia',
    X16 == 'H2FL25018' ~ 'Mario Diaz-Balart',
    X16 == 'H2TX35011' ~ 'Joaquin Castro',
    X16 == 'H4CA35031' ~ 'Norma Torres',
    X16 == 'H6CA44103' ~ 'Nanette Barragan',
    X16 == 'H8FL27185' ~ 'Maria Salazar',
    X16 == 'H2CA50026' ~ 'Juan Vargas',
    X16 == 'H2CA28113' ~ 'Tony Cardenas',
    X16 == 'H2CA31125' ~ 'Pete Aguilar',
    X16 == 'H0FL26036' ~ 'Carlos Gimenez',
    X16 == 'H2TX33073' ~ 'Marc Veasey',
    X16 == 'H0TX35015' ~ 'Tony Gonzales',
    X16 == 'H0NY15160' ~ 'Ritchie Torres',
    X16 == 'H6CA46116' ~ 'Lou Correa',
    X16 == 'H8CA34068' ~ 'Grace Napolitano',
    X16 == 'H2AZ07070' ~ 'Raul M Grijalva',
    X16 == 'H4TX10028' ~ 'Lloyd Doggett',
    X16 == 'H4CA20082' ~ 'Jim Costa',
    X16 == 'H2CA43245' ~ 'Mark Takano',
    X16 == 'H4AZ07043' ~ 'Ruben Gallego',
    X16 == 'H6NJ13191' ~ 'Albio Sires',
    X16 == 'H6CA20152' ~ 'Jimmy Panetta',
    X16 == 'H8NM02156' ~ 'Yvette Herrell',
    X16 == 'H2CA20094' ~ 'David Valadao',
    X16 == 'H2TX34113' ~ 'Mayra Flores',
    X16 == 'H2HI02128' ~ 'Ed Case',
    X16 == 'H4CA12055' ~ 'Ro Khanna',
    X16 == 'H2NY06116' ~ 'Grace Meng',
    TRUE ~ X16
  ))


names_to_keep <- c(
  'Hakeem Jeffries', 'Steve Cohen', 'Terri Sewell', 'Bennie Thompson', 'Troy Carter',
  'Hank Johnson', 'Nikema Williams', 'David Scott', 'James Clyburn', 'Robin Kelly',
  'Rashida Tlaib', 'Dwight Evans', 'Kweisi Mfume', 'Sheila Cherfilus-Mccormick', 'Anthony Brown',
  'Shontel Brown', 'Sanford Bishop', 'Donald Jr Payne', 'Bobby L Rush', 'Frederica Wilson',
  'Yvette D Clarke', 'Lucille Roybal-Allard', 'Vicente Gonzalez', 'Veronica Escobar', 'Henry Cuellar',
  'Jimmy Gomez', 'Sylvia Garcia', 'Linda Sanchez', 'Jesus Garcia', 'Mario Diaz-Balart',
  'Joaquin Castro', 'Norma Torres', 'Nanette Barragan', 'Maria Salazar', 'Juan Vargas',
  'Tony Cardenas', 'Pete Aguilar', 'Carlos Gimenez', 'Marc Veasey', 'Tony Gonzales',
  'Ritchie Torres', 'Lou Correa', 'Grace Napolitano', 'Raul M Grijalva', 'Lloyd Doggett',
  'Jim Costa', 'Mark Takano', 'Ruben Gallego', 'Albio Sires', 'Jimmy Panetta', 'Yvette Herrell',
  'David Valadao', 'Mayra Flores', 'Ed Case', 'Ro Khanna', 'Grace Meng')


Important_candidate <- Pac_to_indiv_name %>% filter(X16 %in% names_to_keep)



#Mutate important_candidate for pac type

pacs_industry_candidate <- Important_candidate %>% 
  mutate(X10 = case_when(
    X10 == 'A0000' ~ 'Misc Agriculture',
    X10 == 'A1000' ~ 'Crop Production & Basic Processing',
    X10 == 'A1100' ~ 'Crop Production & Basic Processing',
    X10 == 'A1200' ~ 'Crop Production & Basic Processing',
    X10 == 'A1300' ~ 'Tobacco',
    X10 == 'A1400' ~ 'Crop Production & Basic Processing',
    X10 == 'A1500' ~ 'Crop Production & Basic Processing',
    X10 == 'A1600' ~ 'Crop Production & Basic Processing',
    X10 == 'A2000' ~ 'Dairy',
    X10 == 'A2300' ~ 'Poultry & Eggs',
    X10 == 'A3000' ~ 'Livestock',
    X10 == 'A3100' ~ 'Agricultural Services/Products',
    X10 == 'A3200' ~ 'Livestock',
    X10 == 'A3300' ~ 'Livestock',
    X10 == 'A3500' ~ 'Livestock',
    X10 == 'A4000' ~ 'Agricultural Services/Products',
    X10 == 'A4100' ~ 'Agricultural Services/Products',
    X10 == 'A4200' ~ 'Agricultural Services/Products',
    X10 == 'A4300' ~ 'Agricultural Services/Products',
    X10 == 'A4500' ~ 'Agricultural Services/Products',
    X10 == 'A5000' ~ 'Forestry & Forest Products',
    X10 == 'A5200' ~ 'Forestry & Forest Products',
    X10 == 'A6000' ~ 'Agricultural Services/Products',
    X10 == 'A6500' ~ 'Agricultural Services/Products',
    X10 == 'A8000' ~ 'Agricultural Services/Products',
    X10 == 'B0000' ~ 'General Contractors',
    X10 == 'B0500' ~ 'General Contractors',
    X10 == 'B1000' ~ 'General Contractors',
    X10 == 'B1200' ~ 'General Contractors',
    X10 == 'B1500' ~ 'General Contractors',
    X10 == 'B2000' ~ 'Home Builders',
    X10 == 'B2400' ~ 'Home Builders',
    X10 == 'B3000' ~ 'Special Trade Contractors',
    X10 == 'B3200' ~ 'Special Trade Contractors',
    X10 == 'B3400' ~ 'Special Trade Contractors',
    X10 == 'B3600' ~ 'Special Trade Contractors',
    X10 == 'B4000' ~ 'Construction Services',
    X10 == 'B4200' ~ 'Construction Services',
    X10 == 'B4300' ~ 'Construction Services',
    X10 == 'B4400' ~ 'Construction Services',
    X10 == 'B5000' ~ 'Building Materials & Equipment',
    X10 == 'B5100' ~ 'Building Materials & Equipment',
    X10 == 'B5200' ~ 'Building Materials & Equipment',
    X10 == 'B5300' ~ 'Building Materials & Equipment',
    X10 == 'B5400' ~ 'Building Materials & Equipment',
    X10 == 'B5500' ~ 'Building Materials & Equipment',
    X10 == 'B6000' ~ 'Building Materials & Equipment',
    X10 == 'C0000' ~ 'Misc Communications Electronics',
    X10 == 'C1000' ~ 'Printing & Publishing',
    X10 == 'C1100' ~ 'Printing & Publishing',
    X10 == 'C1300' ~ 'Printing & Publishing',
    X10 == 'C1400' ~ 'Printing & Publishing',
    X10 == 'C2000' ~ 'TV/Movies Music',
    X10 == 'C2100' ~ 'TV/Movies Music',
    X10 == 'C2200' ~ 'TV/Movies Music',
    X10 == 'C2300' ~ 'TV/Movies Music',
    X10 == 'C2400' ~ 'TV/Movies Music',
    X10 == 'C2600' ~ 'TV/Movies/Music',
    X10 == 'C2700' ~ 'TV/Movies/Music',
    X10 == 'C2800' ~ 'TV/Movies/Music',
    X10 == 'C2900' ~ 'TV/Movies/Music',
    X10 == 'C4000' ~ 'Telecom Services',
    X10 == 'C4100' ~ 'Telephone Utilities',
    X10 == 'C4200' ~ 'Telephone Utilities',
    X10 == 'C4300' ~ 'Telecom Services',
    X10 == 'C4400' ~ 'Telecom Services',
    X10 == 'C4500' ~ 'Telecom Services',
    X10 == 'C4600' ~ 'Telecom Services',
    X10 == 'C5000' ~ 'Electronics Mfg & Equip',
    X10 == 'C5100' ~ 'Electronics Mfg & Equip',
    X10 == 'C5110' ~ 'Electronics Mfg & Equip',
    X10 == 'C5120' ~ 'Electronics Mfg & Equip',
    X10 == 'C5130' ~ 'Electronics Mfg & Equip',
    X10 == 'C5200' ~ 'Electronics Mfg & Equip',
    X10 == 'C5300' ~ 'Electronics Mfg & Equip',
    X10 == 'C5400' ~ 'Electronics Mfg & Equip',
    X10 == 'C6000' ~ 'Internet',
    X10 == 'C6100' ~ 'Internet',
    X10 == 'C6200' ~ 'Internet',
    X10 == 'C6300' ~ 'Internet',
    X10 == 'C6400' ~ 'Internet',
    X10 == 'C6500' ~ 'Internet',
    X10 == 'D0000' ~ 'Misc Defense',
    X10 == 'D2000' ~ 'Defense Aerospace',
    X10 == 'D3000' ~ 'Defense Electronics',
    X10 == 'D4000' ~ 'Misc Defense',
    X10 == 'D5000' ~ 'Misc Defense',
    X10 == 'D6000' ~ 'Misc Defense',
    X10 == 'D8000' ~ 'Misc Defense',
    X10 == 'D9000' ~ 'Misc Defense',
    X10 == 'E0000' ~ 'Misc Energy',
    X10 == 'E1000' ~ 'Misc Energy',
    X10 == 'E1100' ~ 'Oil & Gas',
    X10 == 'E1110' ~ 'Oil & Gas',
    X10 == 'E1120' ~ 'Oil & Gas',
    X10 == 'E1140' ~ 'Oil & Gas',
    X10 == 'E1150' ~ 'Oil & Gas',
    X10 == 'E1160' ~ 'Oil & Gas',
    X10 == 'E1170' ~ 'Oil & Gas',
    X10 == 'E1180' ~ 'Oil & Gas',
    X10 == 'E1190' ~ 'Oil & Gas',
    X10 == 'E1200' ~ 'Mining',
    X10 == 'E1210' ~ 'Mining',
    X10 == 'E1220' ~ 'Mining',
    X10 == 'E1230' ~ 'Mining',
    X10 == 'E1240' ~ 'Mining',
    X10 == 'E1300' ~ 'Electric Utilities',
    X10 == 'E1320' ~ 'Electric Utilities',
    X10 == 'E1500' ~ 'Misc Energy',
    X10 == 'E1600' ~ 'Electric Utilities',
    X10 == 'E1610' ~ 'Electric Utilities',
    X10 == 'E1620' ~ 'Electric Utilities',
    X10 == 'E1630' ~ 'Electric Utilities',
    X10 == 'E1700' ~ 'Misc Energy',
    X10 == 'E2000' ~ 'Environmental Svcs/Equipment',
    X10 == 'E3000' ~ 'Waste Management',
    X10 == 'E4000' ~ 'Fisheries & Wildlife',
    X10 == 'E4100' ~ 'Fisheries & Wildlife',
    X10 == 'E4200' ~ 'Fisheries & Wildlife',
    X10 == 'E5000' ~ 'Misc Energy',
    X10 == 'F0000' ~ 'Misc Finance',
    X10 == 'F1000' ~ 'Commercial Banks',
    X10 == 'F1100' ~ 'Commercial Banks',
    X10 == 'F1200' ~ 'Savings & Loans',
    X10 == 'F1300' ~ 'Credit Unions',
    X10 == 'F1400' ~ 'Finance/Credit Companies',
    X10 == 'F1410' ~ 'Finance/Credit Companies',
    X10 == 'F1420' ~ 'Finance/Credit Companies',
    X10 == 'F2000' ~ 'Securities & Investment',
    X10 == 'F2100' ~ 'Securities & Investment',
    X10 == 'F2110' ~ 'Securities & Investment',
    X10 == 'F2200' ~ 'Securities & Investment',
    X10 == 'F2300' ~ 'Securities & Investment',
    X10 == 'F2400' ~ 'Securities & Investment',
    X10 == 'F2500' ~ 'Securities & Investment',
    X10 == 'F2600' ~ 'Securities & Investment',
    X10 == 'F2700' ~ 'Securities & Investment',
    X10 == 'F3000' ~ 'Insurance',
    X10 == 'F3100' ~ 'Insurance',
    X10 == 'F3200' ~ 'Insurance',
    X10 == 'F3300' ~ 'Insurance',
    X10 == 'F3400' ~ 'Insurance',
    X10 == 'F4000' ~ 'Real Estate',
    X10 == 'F4100' ~ 'Real Estate',
    X10 == 'F4200' ~ 'Real Estate',
    X10 == 'F4300' ~ 'Real Estate',
    X10 == 'F4400' ~ 'Real Estate',
    X10 == 'F4500' ~ 'Real Estate',
    X10 == 'F4600' ~ 'Real Estate',
    X10 == 'F4700' ~ 'Real Estate',
    X10 == 'F5000' ~ 'Misc Finance',
    X10 == 'F5100' ~ 'Accountants',
    X10 == 'F5200' ~ 'Misc Finance',
    X10 == 'F5300' ~ 'Misc Finance',
    X10 == 'F5500' ~ 'Misc Finance',
    X10 == 'F7000' ~ 'Misc Finance',
    X10 == 'G0000' ~ 'Misc Business',
    X10 == 'G1000' ~ 'Business Associations',
    X10 == 'G1100' ~ 'Business Associations',
    X10 == 'G1200' ~ 'Business Associations',
    X10 == 'G1300' ~ 'Business Associations',
    X10 == 'G1310' ~ 'Business Associations',
    X10 == 'G1400' ~ 'Business Associations',
    X10 == 'G2000' ~ 'Food Processing & Sales',
    X10 == 'G2100' ~ 'Food Processing & Sales',
    X10 == 'G2110' ~ 'Food & Beverage',
    X10 == 'G2200' ~ 'Food & Beverage',
    X10 == 'G2300' ~ 'Food Processing & Sales',
    X10 == 'G2350' ~ 'Food & Beverage',
    X10 == 'G2400' ~ 'Food Processing & Sales',
    X10 == 'G2500' ~ 'Food Processing & Sales',
    X10 == 'G2600' ~ 'Food & Beverage',
    X10 == 'G2700' ~ 'Food & Beverage',
    X10 == 'G2800' ~ 'Beer, Wine & Liquor',
    X10 == 'G2810' ~ 'Beer, Wine & Liquor',
    X10 == 'G2820' ~ 'Beer, Wine & Liquor',
    X10 == 'G2840' ~ 'Beer, Wine & Liquor',
    X10 == 'G2850' ~ 'Beer, Wine & Liquor',
    X10 == 'G2860' ~ 'Marijuana',
    X10 == 'G2900' ~ 'Food & Beverage',
    X10 == 'G2910' ~ 'Food & Beverage',
    X10 == 'G3000' ~ 'Misc Business',
    X10 == 'G3500' ~ 'Misc Business',
    X10 == 'G4000' ~ 'Retail Sales',
    X10 == 'G4100' ~ 'Retail Sales',
    X10 == 'G4200' ~ 'Retail Sales',
    X10 == 'G4300' ~ 'Retail Sales',
    X10 == 'G4400' ~ 'Retail Sales',
    X10 == 'G4500' ~ 'Retail Sales',
    X10 == 'G4600' ~ 'Retail Sales',
    X10 == 'G4700' ~ 'Retail Sales',
    X10 == 'G4800' ~ 'Retail Sales',
    X10 == 'G4850' ~ 'Retail Sales',
    X10 == 'G4900' ~ 'Retail Sales',
    X10 == 'G5000' ~ 'Misc Services',
    X10 == 'G5100' ~ 'Misc Services',
    X10 == 'G5200' ~ 'Business Services',
    X10 == 'G5210' ~ 'Business Services',
    X10 == 'G5220' ~ 'Business Services',
    X10 == 'G5230' ~ 'Business Services',
    X10 == 'G5240' ~ 'Business Services',
    X10 == 'G5250' ~ 'Business Services',
    X10 == 'G5260' ~ 'Business Services',
    X10 == 'G5270' ~ 'Business Services',
    X10 == 'G5280' ~ 'Business Services',
    X10 == 'G5290' ~ 'Business Services',
    X10 == 'G5300' ~ 'Misc Services',
    X10 == 'G5400' ~ 'Misc Services',
    X10 == 'G5500' ~ 'Misc Services',
    X10 == 'G5600' ~ 'Misc Services',
    X10 == 'G5700' ~ 'Misc Services',
    X10 == 'G5800' ~ 'Misc Services',
    X10 == 'G6000' ~ 'Recreation/Live Entertainment',
    X10 == 'G6100' ~ 'Recreation/Live Entertainment',
    X10 == 'G6400' ~ 'Recreation/Live Entertainment',
    X10 == 'G6500' ~ 'Casinos/Gambling',
    X10 == 'G6550' ~ 'Casinos/Gambling',
    X10 == 'G6700' ~ 'Recreation/Live Entertainment',
    X10 == 'G6800' ~ 'Misc Services',
    X10 == 'G7000' ~ 'Misc Business',
    X10 == 'H1000' ~ 'Health Professionals',
    X10 == 'H1100' ~ 'Health Professionals',
    X10 == 'H1110' ~ 'Health Professionals',
    X10 == 'H1120' ~ 'Health Professionals',
    X10 == 'H1130' ~ 'Health Professionals',
    X10 == 'H1400' ~ 'Health Professionals',
    X10 == 'H1500' ~ 'Health Professionals',
    X10 == 'H1700' ~ 'Health Professionals',
    X10 == 'H1710' ~ 'Health Professionals',
    X10 == 'H1750' ~ 'Health Professionals',
    X10 == 'H2000' ~ 'Hospitals/Nursing Homes',
    X10 == 'H2100' ~ 'Hospitals/Nursing Homes',
    X10 == 'H2200' ~ 'Hospitals/Nursing Homes',
    X10 == 'H2300' ~ 'Hospitals/Nursing Homes',
    X10 == 'H3000' ~ 'Health Services/HMOs',
    X10 == 'H3100' ~ 'Health Services/HMOs',
    X10 == 'H3200' ~ 'Health Services/HMOs',
    X10 == 'H3300' ~ 'Health Services/HMOs',
    X10 == 'H3400' ~ 'Health Services/HMOs',
    X10 == 'H3500' ~ 'Health Services/HMOs',
    X10 == 'H3700' ~ 'Health Services/HMOs',
    X10 == 'H3800' ~ 'Health Services/HMOs',
    X10 == 'H3900' ~ 'Health Services/HMOs',
    X10 == 'H4000' ~ 'Pharmaceuticals/Health Products',
    X10 == 'H4100' ~ 'Pharmaceuticals/Health Products',
    X10 == 'H4200' ~ 'Pharmaceuticals/Health Products',
    X10 == 'H4300' ~ 'Pharmaceuticals/Health Products',
    X10 == 'H4400' ~ 'Pharmaceuticals/Health Products',
    X10 == 'H4500' ~ 'Pharmaceuticals/Health Products',
    X10 == 'H4600' ~ 'Pharmaceuticals/Health Products',
    X10 == 'H4700' ~ 'Pharmaceuticals/Health Products',
    X10 == 'H5000' ~ 'Education',
    X10 == 'H5100' ~ 'Education',
    X10 == 'H5150' ~ 'Education',
    X10 == 'H5170' ~ 'Education',
    X10 == 'H5200' ~ 'Education',
    X10 == 'H5300' ~ 'Education',
    X10 == 'H6000' ~ 'Other',
    X10 == 'J0000' ~ 'Misc Issues',
    X10 == 'J1000' ~ 'Misc Issues',
    X10 == 'J1100' ~ 'Republican/Conservative',
    X10 == 'J1200' ~ 'Democratic/Liberal',
    X10 == 'J1300' ~ 'Misc Issues',
    X10 == 'J2000' ~ 'Leadership PACs',
    X10 == 'J2100' ~ 'Leadership PACs',
    X10 == 'J2200' ~ 'Leadership PACs',
    X10 == 'J2300' ~ 'Leadership PACs',
    X10 == 'J2400' ~ 'Leadership PACs',
    X10 == 'J2500' ~ 'Leadership PACs',
    X10 == 'J3000' ~ 'Misc Issues',
    X10 == 'J4000' ~ 'Misc Issues',
    X10 == 'J5000' ~ 'Foreign & Defense Policy',
    X10 == 'J5100' ~ 'Pro-Israel',
    X10 == 'J5200' ~ 'Foreign & Defense Policy',
    X10 == 'J5300' ~ 'Foreign & Defense Policy',
    X10 == 'J5400' ~ 'Foreign & Defense Policy',
    X10 == 'J6100' ~ 'Gun Control',
    X10 == 'J6200' ~ 'Gun Rights',
    X10 == 'J6500' ~ 'Misc Issues',
    X10 == 'J7000' ~ 'Human Rights',
    X10 == 'J7120' ~ 'Abortion Policy/Anti-Abortion',
    X10 == 'J7150' ~ 'Abortion Policy/Pro-Abortion Rights',
    X10 == 'J7200' ~ 'Misc Issues',
    X10 == 'J7210' ~ 'Misc Issues',
    X10 == 'J7300' ~ 'Human Rights',
    X10 == 'J7400' ~ "Women's Issues",
    X10 == 'J7500' ~ 'Human Rights',
    X10 == 'J7510' ~ 'Human Rights',
    X10 == 'J7600' ~ 'Misc Issues',
    X10 == 'J7700' ~ 'Human Rights',
    X10 == 'J8000' ~ 'Misc Issues',
    X10 == 'J9000' ~ 'Misc Issues',
    X10 == 'J9100' ~ 'Misc Issues',
    X10 == 'JD100' ~ 'Foreign & Defense Policy',
    X10 == 'JD200' ~ 'Foreign & Defense Policy',
    X10 == 'JE300' ~ 'Environment',
    X10 == 'JH100' ~ 'Human Rights',
    X10 == 'JW100' ~ 'Misc Issues',
    X10 == 'K0000' ~ 'Lawyers/Law Firms',
    X10 == 'K1000' ~ 'Lawyers/Law Firms',
    X10 == 'K1100' ~ 'Lawyers/Law Firms',
    X10 == 'K1200' ~ 'Lawyers/Law Firms',
    X10 == 'K2000' ~ 'Lawyers/Law Firms',
    X10 == 'K2100' ~ 'Lawyers/Law Firms',
    X10 == 'L0000' ~ 'Misc Unions',
    X10 == 'L1000' ~ 'Public Sector Unions',
    X10 == 'L1100' ~ 'Public Sector Unions',
    X10 == 'L1200' ~ 'Public Sector Unions',
    X10 == 'L1300' ~ 'Public Sector Unions',
    X10 == 'L1400' ~ 'Public Sector Unions',
    X10 == 'L1500' ~ 'Public Sector Unions',
    X10 == 'L5000' ~ 'Misc Unions',
    X10 == 'LA100' ~ 'Misc Unions',
    X10 == 'LB100' ~ 'Building Trade Unions',
    X10 == 'LC100' ~ 'Industrial Unions',
    X10 == 'LC150' ~ 'Industrial Unions',
    X10 == 'LD100' ~ 'Misc Unions',
    X10 == 'LE100' ~ 'Industrial Unions',
    X10 == 'LE200' ~ 'Industrial Unions',
    X10 == 'LG000' ~ 'Misc Unions',
    X10 == 'LG100' ~ 'Misc Unions',
    X10 == 'LG200' ~ 'Misc Unions',
    X10 == 'LG300' ~ 'Misc Unions',
    X10 == 'LG400' ~ 'Misc Unions',
    X10 == 'LG500' ~ 'Misc Unions',
    X10 == 'LH100' ~ 'Misc Unions',
    X10 == 'LM100' ~ 'Industrial Unions',
    X10 == 'LM150' ~ 'Industrial Unions',
    X10 == 'LT000' ~ 'Transportation Unions',
    X10 == 'LT100' ~ 'Transportation Unions',
    X10 == 'LT300' ~ 'Transportation Unions',
    X10 == 'LT400' ~ 'Transportation Unions',
    X10 == 'LT500' ~ 'Transportation Unions',
    X10 == 'LT600' ~ 'Transportation Unions',
    X10 == 'M0000' ~ 'Misc Manufacturing & Distributing', 
    X10 == 'M1000' ~ 'Chemical & Related Manufacturing',
    X10 == 'M1100' ~ 'Chemical & Related Manufacturing',
    X10 == 'M1300' ~ 'Chemical & Related Manufacturing',
    X10 == 'M1400' ~ 'Misc Manufacturing & Distributing',
    X10 == 'M1500' ~ 'Chemical & Related Manufacturing',
    X10 == 'M1600' ~ 'Chemical & Related Manufacturing',
    X10 == 'M1700' ~ 'Chemical & Related Manufacturing',
    X10 == 'M2000' ~ 'Misc Manufacturing & Distributing',
    X10 == 'M2100' ~ 'Steel Production',
    X10 == 'M2200' ~ 'Misc Manufacturing & Distributing',
    X10 == 'M2250' ~ 'Misc Manufacturing & Distributing',
    X10 == 'M2300' ~ 'Misc Manufacturing & Distributing',
    X10 == 'M2400' ~ 'Misc Manufacturing & Distributing',
    X10 == 'M3000' ~ 'Misc Manufacturing & Distributing',
    X10 == 'M3100' ~ 'Misc Manufacturing & Distributing',
    X10 == 'M3200' ~ 'Misc Manufacturing & Distributing',
    X10 == 'M3300' ~ 'Misc Manufacturing & Distributing',
    X10 == 'M3400' ~ 'Misc Manufacturing & Distributing',
    X10 == 'M3500' ~ 'Misc Manufacturing & Distributing',
    X10 == 'M3600' ~ 'Misc Manufacturing & Distributing',
    X10 == 'M4000' ~ 'Misc Manufacturing & Distributing',
    X10 == 'M4100' ~ 'Misc Manufacturing & Distributing',
    X10 == 'M4200' ~ 'Misc Manufacturing & Distributing',
    X10 == 'M4300' ~ 'Misc Manufacturing & Distributing',
    X10 == 'M5000' ~ 'Misc Manufacturing & Distributing',
    X10 == 'M5100' ~ 'Misc Manufacturing & Distributing',
    X10 == 'M5200' ~ 'Misc Manufacturing & Distributing',
    X10 == 'M5300' ~ 'Misc Manufacturing & Distributing',
    X10 == 'M6000' ~ 'Misc Manufacturing & Distributing',
    X10 == 'M7000' ~ 'Misc Manufacturing & Distributing',
    X10 == 'M7100' ~ 'Misc Manufacturing & Distributing',
    X10 == 'M7200' ~ 'Misc Manufacturing & Distributing',
    X10 == 'M7300' ~ 'Misc Manufacturing & Distributing',
    X10 == 'M8000' ~ 'Textiles',
    X10 == 'M9000' ~ 'Misc Manufacturing & Distributing',
    X10 == 'M9100' ~ 'Misc Manufacturing & Distributing',
    X10 == 'M9200' ~ 'Misc Manufacturing & Distributing',
    X10 == 'M9300' ~ 'Misc Manufacturing & Distributing',
    X10 == 'T0000' ~ 'Misc Transport',
    X10 == 'T1000' ~ 'Air Transport',
    X10 == 'T1100' ~ 'Air Transport',
    X10 == 'T1200' ~ 'Air Transport',
    X10 == 'T1300' ~ 'Air Transport',
    X10 == 'T1400' ~ 'Air Transport',
    X10 == 'T1500' ~ 'Air Transport',
    X10 == 'T1600' ~ 'Air Transport',
    X10 == 'T1700' ~ 'Air Transport',
    X10 == 'T2000' ~ 'Automotive',
    X10 == 'T2100' ~ 'Automotive',
    X10 == 'T2200' ~ 'Automotive',
    X10 == 'T2300' ~ 'Automotive',
    X10 == 'T2310' ~ 'Automotive',
    X10 == 'T2400' ~ 'Automotive',
    X10 == 'T2500' ~ 'Automotive',
    X10 == 'T3000' ~ 'Trucking',
    X10 == 'T3100' ~ 'Trucking',
    X10 == 'T3200' ~ 'Trucking',
    X10 == 'T4000' ~ 'Misc Transport',
    X10 == 'T4100' ~ 'Misc Transport',
    X10 == 'T4200' ~ 'Misc Transport',
    X10 == 'T5000' ~ 'Railroads',
    X10 == 'T5100' ~ 'Railroads',
    X10 == 'T5200' ~ 'Railroads',
    X10 == 'T5300' ~ 'Railroads',
    X10 == 'T6000' ~ 'Sea Transport',
    X10 == 'T6100' ~ 'Sea Transport',
    X10 == 'T6200' ~ 'Sea Transport',
    X10 == 'T6250' ~ 'Sea Transport',
    X10 == 'T7000' ~ 'Misc Transport',
    X10 == 'T7100' ~ 'Air Transport',
    X10 == 'T7200' ~ 'Misc Business',
    X10 == 'T8000' ~ 'Misc Transport',
    X10 == 'T8100' ~ 'Misc Transport',
    X10 == 'T8200' ~ 'Misc Transport',
    X10 == 'T8300' ~ 'Misc Transport',
    X10 == 'T8400' ~ 'Misc Transport',
    X10 == 'T9000' ~ 'Lodging/Tourism',
    X10 == 'T9100' ~ 'Lodging/Tourism',
    X10 == 'T9300' ~ 'Lodging/Tourism',
    X10 == 'T9400' ~ 'Lodging/Tourism',
    X10 == 'X0000' ~ 'Other',
    X10 == 'X1200' ~ 'Retired',
    X10 == 'X3000' ~ 'Civil Servants/Public Officials',
    X10 == 'X3100' ~ 'Civil Servants/Public Officials',
    X10 == 'X3200' ~ 'Civil Servants/Public Officials',
    X10 == 'X3300' ~ 'Civil Servants/Public Officials',
    X10 == 'X3500' ~ 'Education',
    X10 == 'X3700' ~ 'Civil Servants/Public Officials',
    X10 == 'X4000' ~ 'Non-Profit Institutions',
    X10 == 'X4100' ~ 'Non-Profit Institutions',
    X10 == 'X4110' ~ 'Non-Profit Institutions',
    X10 == 'X4200' ~ 'Non-Profit Institutions',
    X10 == 'X5000' ~ 'Other',
    X10 == 'X7000' ~ 'Clergy & Religious Organizations',
    X10 == 'X8000' ~ 'Other',
    X10 == 'X9000' ~ 'Other',
    X10 == 'Y0000' ~ 'Unknown',
    X10 == 'Y1000' ~ 'Homemakers/Non-Income Earners',
    X10 == 'Y2000' ~ 'No Employer Listed or Found',
    X10 == 'Y3000' ~ 'Generic Occupation/Category Unknown',
    X10 == 'Y4000' ~ 'Employer Listed/Category Unknown',
    X10 == 'Z1000' ~ 'Ideology/Single-Issue',
    X10 == 'Z1100' ~ 'Ideology/Single-Issue',
    X10 == 'Z1200' ~ 'Ideology/Single-Issue',
    X10 == 'Z1300' ~ 'Ideology/Single-Issue',
    X10 == 'Z1400' ~ 'Ideology/Single-Issue',
    X10 == 'Z4100' ~ 'Joint Candidate Cmtes',
    X10 == 'Z4200' ~ 'Joint Candidate Cmtes',
    X10 == 'Z4300' ~ 'Joint Candidate Cmtes',
    X10 == 'Z4400' ~ 'Joint Candidate Cmtes',
    X10 == 'Z4500' ~ 'Joint Candidate Cmtes',
    X10 == 'Z5000' ~ 'Party Cmte',
    X10 == 'Z5100' ~ 'Party Cmte',
    X10 == 'Z5200' ~ 'Party Cmte',
    X10 == 'Z5300' ~ 'Party Cmte',
    X10 == 'Z9000' ~ 'Candidate',
    X10 == 'Z9100' ~ 'Non-contribution',
    X10 == 'Z9500' ~ 'Non-contribution',
    X10 == 'z9600' ~ 'Non-contribution',
    X10 == 'Z9700' ~ 'Non-contribution',
    X10 == 'Z9800' ~ 'Non-contribution',
    X10 == 'Z9999' ~ 'Non-contribution', 
    TRUE ~ X10 ))


#Combine the racial districts
combined_racial_districts <- st_join(Asian_majority_selected, black_majority_selected) 

combined_racial_districts <- st_join(combined_racial_districts, Latino_majority_selected)

combined_racial_districts <- bind_rows(Asian_majority_selected, black_majority_selected, Latino_majority_selected)



#Congress District selected

cds118_selected <- cds118[c(3, 7, 11, 15, 29, 61, 
                            62,31, 37, 38, 41, 65, 66, 67, 45, 50, 69,73, 106, 109, 110,
                            111, 112,119, 121, 122,129, 132, 136, 137, 139, 178, 188, 191, 214, 224, 254, 256, 260, 268,270, 271, 277, 313, 331, 353, 364, 376, 399, 400, 380, 384, 385, 386, 390,392, 394 ),]    




cds118_selected <- select(cds118_selected, -source, -ALAND20, -AWATER20)







cds118_selected$INTPTLAT20 <- as.numeric(cds118_selected$INTPTLAT20)
cds118_selected$INTPTLON20 <- as.numeric(cds118_selected$INTPTLON20)


#COmbined
#racial_cds_joined<- left_join(cds118, combined_racial_districts)

#Candidate_with_district <- bind_cols(pacs_industry_candidate, combined_racial_districts)







# Assuming you have a data frame called candidate_open_secret
# Replace the X16 == conditions with X8== and update the names after the tilde



#important!


C_open_secret_fil <- Candidate_open_secret %>%
  filter(
    X8 %in% c(
      'Hakeem Jeffries (D)', 'Steve Cohen(D)', 'Terri Sewell (D)', 'Bennie G Thompson (D)', 'Troy Carter (D)',
      'Hank Johnson (D)', 'Nikema Williams (D)', 'David Scott (D)', 'James E Clyburn (D)', 'Robin Kelly (D)',
      'Rashida Tlaib (D)', 'Dwight Evans (D)', 'Kweisi Mfume (D)', 'Sheila Cherfilus-McCormick (D)', 'Anthony Brown (D)',
      'Shontel Brown (D)', 'Sanford Bishop (D)', 'Donald L Payne Jr. (D)', 'Bobby Rush (D)', 'Frederica Wilson (D)',
      'Yvette Clarke (D)', 'Lucille Roybal-Allard (D)', 'Vicente Gonzalez (D)', 'Veronica Escobar (D)', 'Henry Cuellar (D)',
      'Jimmy Gomez (D)', 'Sylvia Garcia (D)', 'Linda Sanchez (D)', 'Jesus Garcia (D)', 'Mario Diaz-Balart (R)',
      'Joaquin Castro (D)', 'Norma Torres (D)', 'Nanette Barragan (D)', 'Maria Salazar (R)', 'Juan Vargas (D)',
      'Tony Cardenas (D)', 'Pete Aguilar (D)', 'Carlos Gimenez (R)', 'Marc Veasey (D)', 'Tony Gonzales (R)',
      'Ritchie Torres (D)', 'Lou Correa (D)', 'Grace Napolitano (D)', 'Raul M Grijalva (D)', 'Lloyd Doggett (D)',
      'Jim Costa (D)', 'Mark Takano (D)', 'Ruben Gallego (D)', 'Albio Sires (D)', 'Jimmy Panetta (D)', 'Yvette Herrell (R)',
      'David Valadao (R)', 'Mayra Flores (R)', 'Ed Case (D)', 'Ro Khanna (D)', 'Grace Meng (D)', 'Mike D Rogers (R)'
    ))



Pacs_can_joint <- left_join(pacs_industry_candidate, C_open_secret_fil, by = c("X8" = "X6"))






cds118_selected2<- cds118_selected %>%
  mutate(CD118FP= case_when(
    row_number() ==  1 ~ "AL03",  
    row_number() == 2~ "AL07", 
    row_number() == 3~ "AZ03",   
    row_number() == 5~ "CA16",
    row_number() == 7~ "CA20",
    row_number() == 29~ "IL01",
    row_number() == 30~ "IL02",
    row_number() == 31~ "IL04",
    row_number() == 32~ "LA02",
    row_number() == 33~ "MD04",
    row_number() == 34~ "MD07",
    row_number() == 35~ "MI13",
    row_number() == 36~ "MS02",
    row_number() == 37~ "NJ08",
    row_number() == 39~ "NM02",
    row_number() == 40~ "NY06",
    row_number() == 42~ "NY09",
    row_number() == 43~ "NY15",
    row_number() == 50~ "TX20",
    row_number() == 52~ "TX27",
    row_number() == 54~ "TX29",
    row_number() == 55~ "TX34",
    row_number() == 57~ "TX35",
    CD118FP == "07"~"AZ07",
    CD118FP == "17"~"CA17",
    CD118FP == "21"~"CA21",
    CD118FP == "29"~"CA29",
    CD118FP == "31"~"CA31",
    CD118FP == "35"~"CA35",
    CD118FP == "34"~"CA34",
    CD118FP == "38"~"CA38",
    CD118FP == "40"~"CA40",
    CD118FP == "41"~"CA41",
    CD118FP == "44"~"CA44",
    CD118FP == "46"~"CA46",
    CD118FP == "51"~"CA51",
    CD118FP == "20"~"FL20",
    CD118FP == "24"~"FL24",
    CD118FP == "25"~"FL25",
    CD118FP == "26"~"FL26",
    CD118FP == "27"~"FL27",
    CD118FP == "02"~"GA02",
    CD118FP == "04"~"GA04",
    CD118FP == "05"~"GA05",
    CD118FP == "13"~"GA13",
    CD118FP == "01"~"HI01",
    CD118FP == "04"~"IL04",
    CD118FP == "07"~"MD07",
    CD118FP == "10"~"NJ10",
    CD118FP == "08"~"NY08",
    CD118FP == "11"~"OH11",
    CD118FP == "03"~"PA03",
    CD118FP == "06"~"SC06",
    CD118FP == "09"~"TN09",
    CD118FP == "15"~"TX15",
    CD118FP == "16"~"TX16",
    CD118FP == "23"~"TX23",
    CD118FP == "28"~"TX28",
    CD118FP == "33"~"TX33",
    TRUE ~ CD118FP
  ))




Complete_data <- left_join(cds118_selected2, Pacs_can_joint, by = c("CD118FP" = "X14.y"))


# Transform coordinates to the desired projection
Complete_data <- st_transform(Complete_data, 4326)

















#Leaflet map
ui_data_twice <- fluidPage(
  titlePanel("Race Data"),
  leafletOutput("map")
)

race_server <- function(input, output) {Racial_district_map <- function(Complete_data) {
  leaflet() %>%
    addTiles() %>%
    addPolygons(
      data = Complete_data,
      fillColor = "blue",    
      fillOpacity = 0.5,     
      stroke = TRUE,         
      color = "black",       
      weight = 1,            
      popup = ~paste("State Code: ", STATEFP20, "<br>District Name: ", NAMELSAD20, "<br>Candidate Name:", X16.x)
    ) %>%
    addLegend(
      position = "bottomright",
      colors = "red",
      labels = 'Pac Type',
      opacity = 0.5
    )
}
output$map <- renderLeaflet({
  Racial_district_map(Complete_data)
})}

shinyApp(ui_data_twice, race_server)

runGitHub
