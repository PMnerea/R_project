library(tidyverse)
library(ggplot2)
library(readxl)
library(sf)
library(dplyr)
library("giscoR")

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


# Part de crimes par sous-catégorie par subregions
emotionalCrimes <- crimeDataCounts %>% filter(Category %in% c("Acts intended to induce fear or emotional distress", "Acts intended to induce fear or emotional distress: Cyber-related")) %>% group_by(Subregion) %>% summarise(nbCrimes = sum(type.convert(VALUE, dec=".", as.is = TRUE)))
emotionalCrimes$crime = "Emotional crime"
pedoCrimes <- crimeDataCounts %>% filter(Category %in% c("Child pornography", "Child pornography: Cyber-related")) %>% group_by(Subregion) %>% summarise(nbCrimes = sum(type.convert(VALUE, dec=".", as.is = TRUE)))
pedoCrimes$crime = "Pedophilia crime"
violenceCrimes <- crimeDataCounts %>% filter(Category %in% c("Kidnapping", "Robbery", "Serious assault")) %>% group_by(Subregion) %>% summarise(nbCrimes = sum(type.convert(VALUE, dec=".", as.is = TRUE)))
violenceCrimes$crime = "Violence crime"
sexualCrimes <- crimeDataCounts %>% filter(Category %in% c("Sexual violence", "Sexual violence: Other acts of sexual violence", "Sexual violence: Rape", "Sexual violence: Sexual assault")) %>% group_by(Subregion) %>% summarise(nbCrimes = sum(type.convert(VALUE, dec=".", as.is = TRUE)))
sexualCrimes$crime = "Sexual crime"

crimesBySubregion <- rbind(sexualCrimes, pedoCrimes, emotionalCrimes, violenceCrimes) %>% group_by(Subregion, crime) %>% summarise(nbCrimes = sum(nbCrimes))
ggplot(crimesBySubregion, aes(x=Subregion, y = nbCrimes, fill=crime)) + geom_bar(stat="identity", position="dodge") + ggtitle("Part de crimes par sous-catégorie par subregions")

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
cntries <- gisco_get_countries()

library(ggplot2)
ggplot(cntries) +
  geom_sf()

crimeRateCountry <- crimeDataRate %>% filter(Category %in% sexualCrimes) %>% group_by(Iso3_code, Year) %>% summarise(crimeRate=mean(type.convert(VALUE, dec=".", as.is = TRUE))) %>% group_by(Iso3_code) %>% filter(n_distinct(Year) >= 10) %>% summarise(crimeRate=mean(crimeRate))
head(crimeRateCountry)
head(cntries)

world_data <- cntries %>%
  left_join(crimeRateCountry, by = c("ISO3_CODE" = "Iso3_code"))

plot(world_data[, "crimeRate"],
     breaks = "jenks",
     main = "Choropleth map")
