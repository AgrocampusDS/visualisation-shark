
###### mise en forme du jeu de donnée pour avoir les groupes d'activités 

requin <- read.csv("attacks.csv") 

## choisir les colonnes d'intérêt
req <- requin %>% 
  select("Year", "Type", "Country", "Area","Activity", "Fatal..Y.N.") %>%
  filter(Year >= 1930) 


## mise en forme du nom des endroits 
req$Country <- tolower(req$Country)

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


## 1e "group by" des activités 

req_pie <- req %>%
  filter(Fatal..Y.N. == "Y") %>%
  group_by(Activity) %>%
  summarise(Effectif = n(), Pourcentage = Effectif/353)


###### regroupement des "activitésé selon leur nature 

# diving 
for (i in (1:nrow(req_pie))) {
  if (grepl("diving", req_pie$Activity[i]) | (grepl("Diving", req_pie$Activity[i])) |
      (grepl("Dived", req_pie$Activity[i]))) {
    req_pie$Activity[i] <- "Diving"
  } 
}

# fishing
for (i in (1:nrow(req_pie))) {
  if (grepl("fishing", req_pie$Activity[i]) | (grepl("Fishing", req_pie$Activity[i])) |
      (grepl("lobsters", req_pie$Activity[i])) |
      grepl("fish", req_pie$Activity[i]) |
      grepl("p\xeacheur", req_pie$Activity[i])
  ) {
    req_pie$Activity[i] <- "fishing"
  } 
}

# air or sea disaster
for (i in (1:nrow(req_pie))) {
  if (grepl("disaster", req_pie$Activity[i]) | grepl("Disaster", req_pie$Activity[i])| 
      (grepl("air", req_pie$Activity[i])) | (grepl("plane", req_pie$Activity[i]))
      | (grepl("757", req_pie$Activity[i]))|
      grepl("hurricane", req_pie$Activity[i])
  ) {
    req_pie$Activity[i] <- "Air/Sea disaster"
  } 
}

# boat/ship accident de bateau 
for (i in (1:nrow(req_pie))) {
  if (grepl("Boat", req_pie$Activity[i]) | (grepl("boat", req_pie$Activity[i]))
      | (grepl("ship", req_pie$Activity[i])) | (grepl("Fell", req_pie$Activity[i]))
      | (grepl("Vessel", req_pie$Activity[i])) |
      grepl("sank", req_pie$Activity[i])|
      grepl("promotional film ", req_pie$Activity[i]) |
      grepl("Ferry", req_pie$Activity[i])|
      grepl("ferry", req_pie$Activity[i])|
      grepl("capsiz", req_pie$Activity[i]) |
      grepl("MV Dona Marilyn", req_pie$Activity[i]) |
      grepl("schooner Elizabeth", req_pie$Activity[i]) |
      grepl("Towing", req_pie$Activity[i]) |
      grepl("Tzenny Chandris", req_pie$Activity[i]) |
      grepl("Wreck", req_pie$Activity[i]) |
      grepl("wreck", req_pie$Activity[i]) |
      grepl("dinghy", req_pie$Activity[i]) |
      grepl("yacht", req_pie$Activity[i]) 
  ) {
    req_pie$Activity[i] <- "boat/ship accident"
  } 
}

# sport 
for (i in (1:nrow(req_pie))) {
  if (grepl("surf", req_pie$Activity[i]) | grepl("Surf", req_pie$Activity[i]) |(grepl("boarding", req_pie$Activity[i]))|
      grepl("Boarding", req_pie$Activity[i]) |
      (grepl("batching", req_pie$Activity[i]))| (grepl("Bathing", req_pie$Activity[i])) |
      (grepl("bathing", req_pie$Activity[i])) |
      (grepl("Swim", req_pie$Activity[i]))| (grepl("swim", req_pie$Activity[i])) |
      (grepl("fell", req_pie$Activity[i])) | (grepl("Paddling", req_pie$Activity[i]))| 
      (grepl("Paddling", req_pie$Activity[i])) | (grepl("water", req_pie$Activity[i]))
      | (grepl("Snorkeling", req_pie$Activity[i]))
      | (grepl("Sitting", req_pie$Activity[i])) | 
      grepl("Happy Jack", req_pie$Activity[i]) |
      grepl("Bending", req_pie$Activity[i]) |
      grepl("Canoe", req_pie$Activity[i]) |
      grepl("paddling", req_pie$Activity[i]) |
      grepl("Kayak", req_pie$Activity[i]) |
      grepl("Lifesaving", req_pie$Activity[i]) |
      grepl("Snorkeling", req_pie$Activity[i]) |
      grepl("Stamding", req_pie$Activity[i])|
      grepl("Wading", req_pie$Activity[i]) |
      grepl("Wash", req_pie$Activity[i]) |
      grepl("Workong", req_pie$Activity[i])| 
      grepl("McIver", req_pie$Activity[i]) |
      grepl("Standing", req_pie$Activity[i]) |
      grepl("Crossing the bay", req_pie$Activity[i])|
      grepl("Walking", req_pie$Activity[i]) |
      grepl("kill a shark", req_pie$Activity[i]) |
      grepl("Removing shark", req_pie$Activity[i]) 
  ) {
    req_pie$Activity[i] <- "sport/ activity" }
}



# adrift
for (i in (1:nrow(req_pie))) {
  if (grepl("Adrift", req_pie$Activity[i]) | grepl("adrift", req_pie$Activity[i]) |
      grepl("Floating", req_pie$Activity[i])) {
    req_pie$Activity[i] <- "Adrift"
  } 
}

# murder /  war 
for (i in (1:nrow(req_pie))) {
  if (grepl("Murder", req_pie$Activity[i]) ||
      grepl("submarine", req_pie$Activity[i]) |
      grepl("American", req_pie$Activity[i]) |
      grepl("torpedoed", req_pie$Activity[i]) |
      grepl("Torpedoed", req_pie$Activity[i]) |
      grepl("soldiers", req_pie$Activity[i]) |
      grepl("Escaping", req_pie$Activity[i]) |
      grepl("Japanese", req_pie$Activity[i]) |
      grepl("Japan", req_pie$Activity[i]) |
      grepl("Sinking", req_pie$Activity[i]) |
      grepl("SS", req_pie$Activity[i]) |
      grepl("foundered", req_pie$Activity[i])  |
      grepl("US", req_pie$Activity[i])
  ) {
    req_pie$Activity[i] <- "murder /  war "
  } 
}

req_pie_final <- req_pie %>%
  group_by(Activity) %>%
  summarise(Effectif = n(), Pourcentage = Effectif/353) 

req_pie_final <- req_pie_final[-1,]


########################
#### faire le pie chart
########################

ggplot(data = req_pie_final, aes(x="", y=Pourcentage, fill= Activity)) +
  geom_bar(stat="identity", width=1, color= "white") +
  coord_polar("y", start=0) +
  theme_void()




