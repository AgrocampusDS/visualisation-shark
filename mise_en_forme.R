
## mise en forme du jeu de donnée 

require(dplyr)

requin <- read.csv("attacks.csv") 
dim(requin) #  25723    24

req <- requin %>% 
  select("Year", "Type", "Country", "Area","Activity", "Fatal..Y.N.") %>%
  filter(Year >= 1930) 
dim(req) # 5054    6

sum(is.na(req)) # 0 donc pas de na 

# faire le graphe 
# map avec les attapes totaux de requins sur chaque pays, et en motif le nombre 
# d’attaque fatal (en gros, mort)

req_map <- req %>%
  group_by(Country) %>%
  summarize(nb_attaque = n())

summary(req_map$nb_attaque)

# map of world 
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
require(ggplot2)

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
world <- world %>% select(admin, geometry) %>% print(n=5)
