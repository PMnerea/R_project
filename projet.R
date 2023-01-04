library(tidyverse)
library(ggplot2)
library(readxl)
library('geojsonio')
library(broom)
library(dplyr)

# importation du fichier excel sans prendre en compte la première ligne
# suppression de cette ligne correspondant aux noms
# renomer les colonnes avec la première ligne qui correspond aux noms de colonnes
crimeData <- read_excel("5A/R/Projet/data_cts_violent_and_sexual_crime.xlsx", skip = 1)
colnames(crimeData) <- crimeData[1,]
crimeData <- crimeData[-1,]

# type de crimes
typeCrimes <- c("Acts intended to induce fear or emotional distress", "Acts intended to induce fear or emotional distress: Cyber-related", "Child pornography", "Child pornography: Cyber-related", "Kidnapping", "Robbery", "Serious assault", "Sexual Exploitation", "Sexual violence", "Sexual violence: Other acts of sexual violence", "Sexual violence: Rape", "Sexual violence: Sexual assault")

# Séparation en 2 dataframes, un pour le nombre de crime et l'autre pour le taux de crimes pour 100 000 personnes
crimeDataCounts <- crimeData %>% filter(`Unit of measurement` == "Counts") 
crimeDataRate <- crimeData %>% filter(`Unit of measurement` == "Rate per 100,000 population")

# Nombre de pays par régions et sous régions
countriesByRegion <- crimeData %>% group_by(Region, Subregion) %>% summarise(nbCountries = n_distinct(Country))

# Définition de la table contenant le nombre de crimes par continent pour 2019 (année la plus récente avec le plus de données)
crimeByContinent2019 = crimeDataCounts %>% filter(Year == 2019) %>% filter(Category %in% typeCrimes) %>% group_by(Region) %>% summarise(nbCrime=sum(type.convert(VALUE, dec=".", as.is = TRUE)))
crimeByContinent2019 %>% ggplot(aes(x = Region, y = nbCrime, color = Region, fill = Region)) + geom_histogram(stat="identity") + ggtitle("Nombre de crimes par continent en 2019")

# Evolution du nombre de crimes par pays si des données sont disponibles sur plus de 10 ans
evolCrimeCountry <- function(country) {
  return (crimeDataCounts %>% filter(Country == country) %>% group_by(Year, Country) %>% summarise(nbCrime=sum(type.convert(VALUE, dec=".", as.is = TRUE))) %>% group_by(Country) %>% filter(n_distinct(Year) >= 10))
}
ggplot(evolCrimeCountry('France'), aes(x=Year, y=nbCrime, colour=Country, group = Country)) + geom_line() + geom_point() 

# Evolution du nombre de crimes + pays par continent
evolCrimePerYear <- crimeDataCounts %>% group_by(Year, Region) %>% summarise(nbCrime=sum(type.convert(VALUE, dec=".", as.is = TRUE)))
ggplot(evolCrimePerYear, aes(x=Year, y=nbCrime, colour=Region, group = Region)) + geom_line() + geom_point() + facet_wrap(vars(Region)) + ggtitle("Evolution du nombre de crimes par continent")

evolCountryByContinent <- crimeDataCounts %>% group_by(Year, Region) %>% summarise(nbCountries = n_distinct(Country))
ggplot(evolCountryByContinent, aes(x=Year, y=nbCountries, colour=Region, group = Region)) + geom_line() + geom_point() + facet_wrap(vars(Region)) + ggtitle("Evolution du nombre de pays par continent")


# Part de crimes par catégorie par subregions
typeCrimes <- c("Acts intended to induce fear or emotional distress", "Acts intended to induce fear or emotional distress: Cyber-related", "Child pornography", "Child pornography: Cyber-related", "Kidnapping", "Robbery", "Serious assault", "Sexual Exploitation", "Sexual violence", "Sexual violence: Other acts of sexual violence", "Sexual violence: Rape", "Sexual violence: Sexual assault")
crimesBySubregion <- crimeDataCounts %>% group_by(Subregion, Category) %>% summarise(nbCrimes = sum(type.convert(VALUE, dec=".", as.is = TRUE))) %>% filter(Category %in% typeCrimes)
piechartRateCrime <- function(subregionData) {
  pie(subregionData$nbCrimes, subregionData$Category, main=(paste("Part des crimes en fonction de leur catégorie pour ", subregionData$Subregion[1])))
}
piechartRateCrime(filter(crimesBySubregion, Subregion == "Western Europe"))

# Pays ayant aucun crime dans certaines catégories de crime sexuel et l'année
sexualCrimes <- c("Sexual Exploitation", "Sexual violence", "Sexual violence: Rape", "Sexual violence: Sexual assault")
zeroSexualCrime <- crimeDataCounts %>% filter(Category %in% sexualCrimes) %>% filter(type.convert(VALUE, dec=".", as.is = TRUE) == 0) 
ggplot(zeroSexualCrime, aes(x=Year, y=Country, colour=Category)) + geom_point() + ggtitle("Pays ayant 0 crime sexuel déclaré en fonction de l'année")

# Evolution taux d'agression sexuelle par 100 000 habitants par subregion
sexualCrimes <- c("Sexual Exploitation", "Sexual violence", "Sexual violence: Rape", "Sexual violence: Sexual assault", "Sexual violence: Other acts of sexual violence")
rateSexualCrimeSubregion <- crimeDataRate %>% filter(Category %in% sexualCrimes) %>% group_by(Subregion, Year) %>% summarise(rate = mean(type.convert(VALUE, dec=".", as.is = TRUE)))
ggplot(rateSexualCrimeSubregion, aes(x=Year, y=rate, colour=Subregion, group = Subregion)) + geom_line() + geom_point() + facet_wrap(vars(Subregion)) + ggtitle("Evolution du taux de crimes sexuels par subregion")

# Relation a l'agresseur en cas de violence sexuelle par Region 
victimRelationship <- crimeDataCounts %>% filter(Dimension == "by relationship to perpetrator") %>% filter(Indicator == "Victims of sexual violence") %>% group_by(Subregion, Country, Category)  %>% summarise(nbCrime=sum(type.convert(VALUE, dec=".", as.is = TRUE))) 
ggplot(victimRelationship, aes(x=Subregion, y = nbCrime, fill=Category)) + geom_bar(stat="identity", position="dodge") + ggtitle("Nombre de crimes en fonction de la relation à l'agresseur par Subregion")

# World map des taux de criminalité 
spdf <- geojson_read('countries.geojson',what='sp')
spdf_fortified <- tidy(spdf,region="code")
str(spdf_fortified)
head(spdf_fortified)

ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill="white", color="grey") +
  theme_void() +
  coord_map()

crimeRateCountry <- crimeDataRate %>% filter(Category %in% sexualCrimes) %>% group_by(Iso3_code, Year) %>% summarise(crimeRate=mean(type.convert(VALUE, dec=".", as.is = TRUE))) %>% group_by(Iso3_code) %>% filter(n_distinct(Year) >= 10) %>% summarise(crimeRate=mean(crimeRate))

spdf_fortified <- spdf_fortified %>% left_join(. , crimeRateCountry, by=c("ISO_A3"="Iso3_code"))

jpeg("heat_map_crime_rate.jpg",width=1920,height=1080)
ggplot() +
  geom_polygon(data = spdf_fortified, aes(fill = n, x = long, y = lat, group = group)) +
  scale_fill_gradient(low='#eeebc5',high='#bb0600') +
  theme_void() +
  coord_map() 
dev.off()

