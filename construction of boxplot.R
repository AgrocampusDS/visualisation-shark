
requin <- read.csv("attacks.csv") 
dim(requin) #  25723    24

req <- requin %>% 
  select("Year", "Type", "Country", "Area","Activity", "Fatal..Y.N.", "Species") %>%
  filter(Year >= 1930) 
dim(req) # 5054    6


req[req$Country == "columbia"   ,]$Country <- "colombia"
req[req$Country == "curacao"    ,]$Country <- "curaçao"
req[req$Country == "egypt / israel"    ,]$Country <- "egypt" # israel 
req[req$Country == "grand cayman",]$Country <- "cayman islands"
req[req$Country == "iran / iraq",]$Country <- "iran" # "iraq"
req[req$Country == "italy / croatia" ,]$Country <- "iran" # "italy"
req[req$Country == "maldive islands" ,]$Country <- "maldives" 
req[req$Country == "micronesia" ,]$Country <- "federated states of micronesia" 
req[req$Country == "nicaragua "  ,]$Country <- "nicaragua"  
req[req$Country == "palestinian territories"  ,]$Country <- "palestine"  
req[req$Country == "solomon islands / vanuatu"  ,]$Country <- "solomon islands"  # et vanatu 
req[req$Country == "tanzania"  ,]$Country <- "united republic of tanzania"
req[req$Country == "sudan?"  ,]$Country <- "sudan"
req[req$Country == "trinidad & tobago"  ,]$Country <- "trinidad and tobago"
req[req$Country == "turks & caicos"  ,]$Country <- "turks and caicos island"
req[req$Country == "united arab emirates (uae)"   ,]$Country <- "united arab emirates"
req[req$Country == "usa"   ,]$Country <- "united states of america"
req[req$Country ==  "western samoa"   ,]$Country <- "samoa"

req[req$Country ==  "north atlantic ocean " ,]$Country <-"north atlantic ocean"
req[req$Country ==  "pacific ocean " ,]$Country <-"pacific ocean"
req[req$Country ==  "red sea / indian ocean" ,]$Country <-"indian ocean"
req[req$Country ==  "central pacific" ,]$Country <-"pacific ocean"
req[req$Country ==  "southwest pacific ocean" ,]$Country <-"south pacific ocean"
req[req$Country ==  "pacific ocean" ,]$Country <-"mid-pacific ocean"


#### faire le data 

req_bar <- req %>%
  select("Species", "Fatal..Y.N.") %>%
  group_by(Species) %>%
  summarise(Fatal_Yes = sum(Fatal..Y.N.=="Y"), Fatal_Maybe = sum(Fatal..Y.N.=="M"), Fatal_No = sum(Fatal..Y.N.=="N"))
  