
#### install library 

library(RColorBrewer)
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
require(ggplot2)
library("ggnewscale")  
library(ggrepel)


########################################################
# get right dataset for world map construction 
########################################################

#===================================
## read files
#===================================

requin <- read.csv("attacks.csv") 

req <- requin %>% 
  select("Year", "Type", "Country", "Area","Activity", "Fatal..Y.N.") %>%
  filter(Year >= 1930) 

# select columns necessary for the construction of the world map 
req_map <- req %>%
  group_by(Country) %>%
  summarize(nb_attaque = n())

# read base map 
world <- ne_countries(scale = "medium", returnclass = "sf") 
world <- world %>% select(admin, geometry) # for countries 

fond <- st_read(dsn = "world-administrative-boundaries/world-administrative-boundaries.shp",
                stringsAsFactors = F) # for regions 

#===================================
## modify element 
#===================================

# change countries names and site to lower 
req_map$Country <- tolower(req_map$Country)
world$admin <- tolower(world$admin)
fond$name <- tolower(fond$name)

# standardise site name 
req_map[req_map$Country == "columbia"   ,]$Country <- "colombia"
req_map[req_map$Country == "curacao"    ,]$Country <- "curaçao"
req_map[req_map$Country == "egypt / israel"    ,]$Country <- "egypt" # israel 
req_map[req_map$Country == "grand cayman",]$Country <- "cayman islands"
req_map[req_map$Country == "iran / iraq",]$Country <- "iran" # "iraq"
req_map[req_map$Country == "italy / croatia" ,]$Country <- "iran" # "italy"
req_map[req_map$Country == "maldive islands" ,]$Country <- "maldives" 
req_map[req_map$Country == "micronesia" ,]$Country <- "federated states of micronesia" 
req_map[req_map$Country == "nicaragua "  ,]$Country <- "nicaragua"  
req_map[req_map$Country == "palestinian territories"  ,]$Country <- "palestine"  
req_map[req_map$Country == "solomon islands / vanuatu"  ,]$Country <- "solomon islands"  # et vanatu 
req_map[req_map$Country == "tanzania"  ,]$Country <- "united republic of tanzania"
req_map[req_map$Country == "sudan?"  ,]$Country <- "sudan"
req_map[req_map$Country == "trinidad & tobago"  ,]$Country <- "trinidad and tobago"
req_map[req_map$Country == "turks & caicos"  ,]$Country <- "turks and caicos island"
req_map[req_map$Country == "united arab emirates (uae)"   ,]$Country <- "united arab emirates"
req_map[req_map$Country == "usa"   ,]$Country <- "united states of america"
req_map[req_map$Country ==  "western samoa"   ,]$Country <- "samoa"


# get site present in csv "world"
a <- req_map$Country
b <- world$admin
present_in_world <- a[which(a %in% b  == T)] 

# get site present in csv "fond"
a <- as.vector(factor(req_map$Country))
bbis <- as.vector(factor(fond$name))
present_in_fond <- a[which(a %in% bbis  == T)] 

# site present in both 
present <- union(present_in_world , present_in_fond )
dif <- setdiff(present_in_world ,present_in_fond) # site present in fond and not in world 

#===================================
## get the right dataset for countries and regiop
#===================================

# find geometry necessary for some site in the base fond 
geometry_fond <- fond %>%
  filter(fond$name %in% dif) %>%
  select("admin"= name, "geometry")

geom_init <- rbind(world, geometry_fond)

# for every site, calculate totale attacks 
attack_by_site <- req_map %>%
  group_by(Country) %>%
  summarise(nb_att_tot = sum(nb_attaque)) %>%
  select("admin"= Country, "attacks"= nb_att_tot)

# get dataset with geometry and total attacks of every site
geom <- merge(geom_init , attack_by_site, by="admin", all.x=T)


########################################################
## make dataset for oceans and seas 
########################################################

# standardisation 
req_map[req_map$Country ==  "north atlantic ocean " ,]$Country <-"north atlantic ocean"
req_map[req_map$Country ==  "pacific ocean " ,]$Country <-"pacific ocean"
req_map[req_map$Country ==  "red sea / indian ocean" ,]$Country <-"indian ocean"
req_map[req_map$Country ==  "central pacific" ,]$Country <-"pacific ocean"
req_map[req_map$Country ==  "southwest pacific ocean" ,]$Country <-"south pacific ocean"
req_map[req_map$Country ==  "pacific ocean" ,]$Country <-"mid-pacific ocean"


# locate ocean/sea/gulf site present in attacks.csv
oce_mer <- c("caribbean sea" , "gulf of aden", "indian ocean",
             "mid atlantic ocean" ,"mid-pacifc ocean",
             "north atlantic ocean","north pacific ocean","north sea", 
             "northern arabian sea", "mid-pacific ocean",
             "persian gulf", "red sea", "south atlantic ocean", "south china sea",
             "south pacific ocean" )

#=========================================================================================
## get element necessary for every site: latitude, longitude, nb tot of attacks 
#=========================================================================================

lat <- c(14.939588401460744, 12.579150350346652, -31.65256869194269, 
         0.0012874607478224015, 0.5168641856999522,
         37.55840704949698,26.740062627514437, 56.89226369685983, 
         13.52913765091261, 6.647930428748582,
         27.39827299362649, 21.182319274300287, -32.004663545742, 17.010418251252442, 
         -34.74671641096358)


long <- c(-74.7792643588162, 47.96353180808757,81.67236879838734, 
          -19.9999143509654, -150.4849986612673,
          -39.08714830907774, -169.80601017384916, 3.676283675031177,
          64.31588270602367, -163.13809314592606,
          51.41987343745529, 38.424038461264196, -15.840314716043286, 113.41267801349872,
          -139.57163512902778)



attacks_oce_mer <- req_map %>%
  filter(Country %in% oce_mer) %>%
  group_by(Country) %>% 
  summarise(nb_att_tot = sum(nb_attaque)) %>%
  select("oce_mer"= Country, "attacks"= nb_att_tot)

#========================================
# make dataframe
#========================================

omg_init <- data.frame(oce_mer, lat, long)
omg <- merge(omg_init, attacks_oce_mer, by="oce_mer")

geom_test <- data.frame("admin"= c(geom$admin, omg$oce_mer),
                        "lat" = c(rep(NA, 241), omg$lat), 
                        "long" = c(rep(NA, 241), omg$long),
                        "attacks" = c(geom$attacks, omg$attacks))


########################################################
## still have some site without geometry or localisation
########################################################

all.countries <- req_map$Country
all.countries[which( ( all.countries  %in% oce_mer| all.countries%in% present  )== F)]


########################################################
## world map
########################################################

map <- geom %>% 
  ggplot() + 
  geom_sf(aes(fill = attacks)) +
  scale_fill_distiller(palette = "YlOrRd", trans= "log", direction = 1, 
                       name="Nombre d'attaque",
                       labels =  function(x) sprintf("%.f", x), 
                       breaks = c(1, 7, 55, 403, 1954),
                       na.value="#edeef2") 
  # by default, scale_fill_distiller direction is 1

map

# ==================
# Formatted map
# =================

map2 <- map +
  theme_void() +
  ggtitle("Nombre d'attaque par les requins entre 1930 et 2018") +
  #guides(fill = guide_legend(reverse = T))+
  theme(text = element_text(family = 'Gill Sans', color = '#EEEEEE')   # mettre le texte en blanc et type de texte
        ,plot.title = element_text(size = 26.5)
        ,panel.background = element_rect(fill = '#b3e2f5') # ajouter couleur du fond de carte  
        ,plot.background = element_rect(fill = '#333333') # ajouter couleur de fond 
        ,legend.position="bottom")

map2



# ============================
# add oceans /sea/ gulf point 
# ===========================

map3 <- map2 +
  new_scale_color() +
  geom_point(data = geom_test, 
             aes(x=long, y=lat, col = attacks, fill=attacks),
             colour = "black", pch=21, size=5,
             show.legend = F) +
  geom_text_repel(data = geom_test, aes(x=long, y=lat, label=admin),
                   point.padding = 0.5,
                   segment.color = 'white',
                   colour =  "#394b85",
                   size=3)+
  scale_color_distiller(palette = "YlOrRd", trans= "log", direction = 1 ) 
  
map3



