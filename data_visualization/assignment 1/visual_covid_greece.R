library("readxl")
library("dplyr")
require(ggplot2)
require(directlabels)
require(maps)
require(viridis)

data_path <- "C:/Users/thomas/ownCloud/data science/Data Visualization & Communication/assignment/COVID-19-geographic-disbtribution-worldwide.xlsx"
data <- read_excel(data_path)
print(data)

greek_data = data[data$geoId == "EL", ]
summary(greek_data)

#obtain top 5 countries in Europe
d <- data[data$continentExp == "Europe", ] #extract europe data
n <- d %>% group_by(countriesAndTerritories) %>% 
 summarise(cases = sum(cases)) %>%
 top_n(5, cases) %>%
 arrange(desc(cases))
#plot top 5
pdf(file = 'recent_cases_top_5.pdf', width = 9, height = 6)
ggplot(filter(d, countriesAndTerritories %in% n$countriesAndTerritories, d$dateRep > "2020-09-29 00:00:00"),
 aes(dateRep, cases, color=countriesAndTerritories)) +
 geom_line() +
 geom_dl(aes(label = countryterritoryCode), method = list(dl.combine("first.points", "last.points")), cex = 0.8) +
 theme_minimal(base_size=14) +
 theme(legend.position = "none") +
 labs(title="Recent cases in top 5 European Countries", x="Date", y = "Cases")
dev.off()

n <- d %>% group_by(countriesAndTerritories) %>% 
 summarise(deaths = sum(deaths)) %>%
 top_n(5, deaths) %>%
 arrange(desc(deaths))
#plot top 5
pdf(file = 'recent_deaths_top_5.pdf', width = 9, height = 6)
ggplot(filter(d, countriesAndTerritories %in% n$countriesAndTerritories, d$dateRep > "2020-09-29 00:00:00"),
 aes(dateRep, deaths, color=countriesAndTerritories)) +
 geom_line() +
 geom_dl(aes(label = countryterritoryCode), method = list(dl.combine("first.points", "last.points")), cex = 0.8) +
 theme_minimal(base_size=14) +
 theme(legend.position = "none") +
 labs(title="Recent deaths in top 5 European Countries", x="Date", y = "Cases")
dev.off()

# greek data line chart
pdf(file = 'cases_greece.pdf', width = 9, height = 6)
ggplot(greek_data,
 aes(dateRep, cases, color="steelblue")) +
 geom_line() +
 theme_minimal(base_size=14) +
 theme(legend.position = "none") +
 labs(title="Cases over time in Greece (31/12/2019 - 21/11/2020)", x="Time", y = "Number of Cases")
dev.off()

pdf(file = 'deaths_greece.pdf', width = 9, height = 6)
ggplot(greek_data,
 aes(dateRep, deaths, color="steelblue")) +
 geom_line() +
 theme_minimal(base_size=14) +
 theme(legend.position = "none") +
 labs(title="Deaths from COVID-19 over time in Greece (31/12/2019 - 21/11/2020)", x="Time", y = "Deaths")
dev.off()

# greek data by month
greek_cases_by_month = greek_data %>% group_by(month) %>% summarise(cases = sum(cases))
greek_cases_by_month = greek_cases_by_month[c(12,1,2,3,4,5,6,7,8,9,10,11),]
greek_cases_by_month["mon"] <- c('12/2019', '01/2020' , '02/2020', '03/2020', '04/2020', '05/2020', '06/2020', '07/2020', '08/2020', '09/2020', '10/2020', '11/2020')
greek_cases_by_month$mon <- factor(greek_cases_by_month$mon, levels = greek_cases_by_month$mon)

#pdf(file = 'cases_by_month_greece.pdf', width = 9, height = 6)
#ggplot(data=greek_cases_by_month,
# aes(x=mon, y=cases)) +
# geom_bar(stat="identity", fill="steelblue") +
# geom_text(aes(label=cases), vjust=-0.3, size=3.5)+
# #labs(title="Cases by month in Greece", x="Month", y = "Cases")
# theme_minimal()
#dev.off()

# find countries with similar population
s_g = data %>% filter(popData2019 > 8000000 & popData2019 < 12000000) %>% group_by(countriesAndTerritories, popData2019) %>% summarise(cases = sum(cases)) %>% arrange(desc(popData2019))
print(s_g, n=25)

belgium_data = data[data$countriesAndTerritories == "Belgium", ]
czechia_data = data[data$countriesAndTerritories == "Czechia", ]
austria_data = data[data$countriesAndTerritories == "Austria", ]
simiral_pop <- rbind(greek_data, czechia_data, austria_data, belgium_data)

#plot case of countries with similar plot
pdf(file = 'cases_others.pdf', width = 9, height = 6)
ggplot(filter(d, countriesAndTerritories %in% simiral_pop$countriesAndTerritories, d$dateRep > "2020-09-29 00:00:00"),
 aes(dateRep, cases, color=countriesAndTerritories)) +
 geom_line() +
 geom_dl(aes(label = countryterritoryCode), method = list(dl.combine("first.points", "last.points")), cex = 0.8) +
 theme_minimal(base_size=14) +
 theme(legend.position = "none") +
 labs(title="Cases in other countries with simiral population compared to Greece (29/09/2020 - 21/11/2020)", x="Date", y = "Cases")
dev.off()

#plot by month similar data
#require(reshape2)
greek_cases_by_month = greek_data %>% group_by(month, countriesAndTerritories) %>% summarise(cases = sum(cases))
greek_cases_by_month = greek_cases_by_month[c(12,1,2,3,4,5,6,7,8,9,10,11),]
greek_cases_by_month["mon"] <- c('12/2019', '01/2020' , '02/2020', '03/2020', '04/2020', '05/2020', '06/2020', '07/2020', '08/2020', '09/2020', '10/2020', '11/2020')
greek_cases_by_month$mon <- factor(greek_cases_by_month$mon, levels = greek_cases_by_month$mon)
#greek_cases_by_month$month <- factor(greek_cases_by_month$month, levels = greek_cases_by_month$month)
austria_cases_by_month = austria_data %>% group_by(month, countriesAndTerritories) %>% summarise(cases = sum(cases))
austria_cases_by_month = austria_cases_by_month[c(12,1,2,3,4,5,6,7,8,9,10,11),]
austria_cases_by_month["mon"] <- c('12/2019', '01/2020' , '02/2020', '03/2020', '04/2020', '05/2020', '06/2020', '07/2020', '08/2020', '09/2020', '10/2020', '11/2020')
austria_cases_by_month$mon <- factor(austria_cases_by_month$mon, levels = austria_cases_by_month$mon)
#austria_cases_by_month$month <- factor(austria_cases_by_month$month, levels = austria_cases_by_month$month)

simiral_data <- rbind(greek_cases_by_month, austria_cases_by_month)

pdf(file = 'cases_greece_austria.pdf', width = 9, height = 6)
ggplot(data=simiral_data,
 aes(x=mon, y=cases, fill=countriesAndTerritories)) +
 geom_bar(stat="identity", position='dodge') +
 scale_fill_manual(values = c("Greece" = "#0D5EAF", "Austria" = "#ED2936")) +
 labs(title="Cases per month in Greece & Austria (31/12/2019 - 21/11/2020)", x="Month", y = "Cases") +
 theme_minimal()
dev.off()

#plot deaths per month in Greece and Austria
greek_deaths_by_month = greek_data %>% group_by(month, countriesAndTerritories) %>% summarise(deaths = sum(deaths))
greek_deaths_by_month = greek_deaths_by_month[c(12,1,2,3,4,5,6,7,8,9,10,11),]
greek_deaths_by_month["mon"] <- c('12/2019', '01/2020' , '02/2020', '03/2020', '04/2020', '05/2020', '06/2020', '07/2020', '08/2020', '09/2020', '10/2020', '11/2020')
greek_deaths_by_month$mon <- factor(greek_deaths_by_month$mon, levels = greek_deaths_by_month$mon)
austria_deaths_by_month = austria_data %>% group_by(month, countriesAndTerritories) %>% summarise(deaths = sum(deaths))
austria_deaths_by_month = austria_deaths_by_month[c(12,1,2,3,4,5,6,7,8,9,10,11),]
austria_deaths_by_month["mon"] <- c('12/2019', '01/2020' , '02/2020', '03/2020', '04/2020', '05/2020', '06/2020', '07/2020', '08/2020', '09/2020', '10/2020', '11/2020')
austria_deaths_by_month$mon <- factor(austria_deaths_by_month$mon, levels = austria_deaths_by_month$mon)

simiral_deaths_data <- rbind(greek_deaths_by_month, austria_deaths_by_month)
pdf(file = 'deaths_greece_austria.pdf', width = 9, height = 6)
ggplot(data=simiral_deaths_data,
 aes(x=mon, y=deaths, fill=countriesAndTerritories)) +
 geom_bar(stat="identity", position='dodge') +
 scale_fill_manual(values = c("Greece" = "#0D5EAF", "Austria" = "#ED2936")) +
 labs(title="Deaths per month in Greece & Austria (31/12/2019 - 21/11/2020)", x="Month", y = "Cases") +
 theme_minimal()
dev.off()

# plot top 10 cases bar plot in europe
n <- d %>% group_by(countriesAndTerritories) %>% 
 summarise(cases = sum(cases)) %>% 
 top_n(10, cases) %>%
 arrange(desc(cases))
#plot top 5
pdf(file = 'top10_cases_europe.pdf', width = 9, height = 6)
ggplot(data=n,
 aes(x=countriesAndTerritories, y=cases, fill=countriesAndTerritories)) +
 geom_bar(stat="identity", position='dodge') +
 scale_fill_manual(values = c("Belgium" = "#000000", 
					"France" = "#0020A5",
					"Germany" = "#DD0000",
					"Italy" = "#009249",
					"Czechia" = "#AE1C28",
					"Poland" = "#DC143C",
					"Russia" = "#DA291C",
					"Spain" = "#FFC400",
					"Ukraine" = "#005BBB",
					"United_Kingdom" = "#00247D")) +
 labs(title="Most affected countries in Eupope (cases 21/11/2020))", x="Countries", y = "Cases") +
 theme_minimal() +
 theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

# plot top 10 deaths bar plot in europe
n <- d %>% group_by(countriesAndTerritories) %>% 
 summarise(deaths = sum(deaths)) %>% 
 top_n(10, deaths) %>%
 arrange(desc(deaths))
pdf(file = 'top10_deaths_europe.pdf', width = 9, height = 6)
ggplot(data=n,
 aes(x=countriesAndTerritories, y=deaths, fill=countriesAndTerritories)) +
 geom_bar(stat="identity", position='dodge') +
 scale_fill_manual(values = c("Belgium" = "#000000", 
					"France" = "#0020A5",
					"Germany" = "#DD0000",
					"Italy" = "#009249",
					"Poland" = "#AE1C28",
					"Turkey" = "#DC143C",
					"Russia" = "#DA291C",
					"Spain" = "#FFC400",
					"Ukraine" = "#005BBB",
					"United_Kingdom" = "#00247D")) +
 labs(title="Most affected countries in Eupope (deaths 21/11/2020))", x="Countries", y = "Cases") +
 theme_minimal() +
 theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

# plot map in europe with cases
theme_set(
  theme_void()
  )
eu_countries <- unique(d$countriesAndTerritories)
eu_countries <- eu_countries[!eu_countries %in% "Russia"]
eu_countries[8] <- "Bosnia and Herzegovina"
eu_countries[40] <- "Macedonia"
eu_countries[53] <- "UK"
eu_countries[12] <- "Czech Republic"

# Retrieve the map data
eu_map <- map_data("world", region = eu_countries)
d_eu <- d %>% rename(region = countriesAndTerritories)

n <- d_eu %>% group_by(region) %>% summarise(cases = sum(cases))
n[n == "United_Kingdom"] <- "UK"
n[n == "Czechia"] <- "Czech Republic"
n[n == "Bosnia_and_Herzegovina"] <- "Bosnia and Herzegovina"
n[n == "North_Macedonia"] <- "Macedonia"

cases_map <- left_join(eu_map, n, by = "region")

# Create the map
pdf(file = 'eu_map_cases.pdf', width = 9, height = 6)
ggplot(cases_map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = cases), color = "white")+
  scale_fill_viridis_c(option = "C")+
  labs(title="Total cases in Eupope (21/11/2020)")
dev.off()
