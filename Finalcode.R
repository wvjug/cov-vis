#Final Project
#QAC 251
#Wisly Juganda

require(rgdal)
require(leaflet)
require(geojsonio)
require(readr)
require(data.table)
require(dplyr)
require(RColorBrewer)
require(shiny)
require(plotly)
require(lubridate)
require(ggplot2)

#Read Time-series data
time <- read.csv("Timeseries.csv")

#Convert Date column into date format
time$Date <- ymd(time$Date)

#Plot growth rate
plot_ly(time,
        x=~Date,
        y=~New_case_per_day,
        name="New Confirmed cases",
        type="scatter",
        mode="lines+markers")%>%
  add_trace(y=~Death_cases_perDay, name="New Death cases", mode="lines+markers")%>%
  layout(title="Daily Growth rate of COVID-19 in Indonesia",
         xaxis=list(title="Date"),
         yaxis=list(title="Number of New Cases"))

#Plot cumulative cases
plot_ly(time,
        x=~Date,
        y=~Patient_under_treatment,
        name="Cases under treatment",
        type="scatter",
        mode="none",
        stackgroup='one',
        fillcolor="steel blue",
        hoverinfo="text",
        text=~paste(Date, " Percentage: ", as.character(round(((Patient_under_treatment/Cumulative_cases)*100), digits=2)),'%'))%>%
  add_trace(y = ~Recovered_cases, name = 'Recovered cases', fillcolor = 'chartreuse',hoverinfo="text",
            text=~paste(Date, " Percentage: ", as.character(round(((Recovered_cases/Cumulative_cases)*100), digits=2)),'%'))%>%
  add_trace(y=~Total_death, name= 'Death cases', fillcolor='orange', hoverinfo="text",
            text=~paste(Date, " Percentage: ", as.character(round(((Total_death/Cumulative_cases)*100), digits=2)),'%'))%>%
  layout(title="Cumulative cases of COVID-19 in Indonesia",
         xaxis=list(title="Date"),
         yaxis=list(title="Number of Cases"))

#Read Provincial COVID-19 distribution data
prov <- read.csv("CasebyProvince.csv")

#Read provincial map of Indonesia
province <- geojson_read("IDN_adm_1_province.json", what='sp')

#Prepare data frames for merging the two loaded datasets
slotNames(province)

province@data$rn <- row.names(province)

temp.province<-data.table(province@data)

#Fix names of provinces according to those in the map data
prov %>%
  mutate(NAME_1=as.character(Province_name)) -> prov
prov%>%
  mutate(NAME_1=ifelse(NAME_1=="Daerah Istimewa Yogyakarta", 'Yogyakarta', NAME_1),
         NAME_1=ifelse(NAME_1=="DKI Jakarta", 'Jakarta Raya', NAME_1),
         NAME_1=ifelse(NAME_1=="Papua Barat", 'Irian Jaya Barat', NAME_1),
         NAME_1=ifelse(NAME_1=="Kepulauan Bangka Belitung", 'Bangka-Belitung', NAME_1)) -> prov
prov <- prov %>% filter(NAME_1 != 'Indonesia'&NAME_1 !='Kepulauan Riau')

#Merge two datasets
out.prov<-merge(temp.province, prov, by="NAME_1", all.x=TRUE)

out.prov<-data.table(out.prov)

setkey(out.prov, rn)
province@data <- out.prov[row.names(province)]

#Define coloring function and color palette
pal <- colorNumeric("YlOrRd", domain = NULL)

#Plot confirmed cases choropleth
leaflet(data = province)%>%
  addProviderTiles("CartoDB.Positron")%>%
  addPolygons(fillColor = ~pal(log(Confirmed_cases)),
              fillOpacity = 0.8,
              color = "black",
              weight = 1,
              popup=~paste(NAME_1, ", No. of Confirmed cases:", as.character(Confirmed_cases)))%>%
  addMarkers(lng=106.816666,
             lat=-6.200000,
             popup=paste("Jakarta", ", No. of Confirmed cases: 5688"))%>%
  addLegend("bottomleft",
            colors=brewer.pal(5,"YlOrRd"),
            labels=c("low","","","","high"),
            title="Number of Confirmed cases")

#Plot case fatality rate choropleth
leaflet(data = province)%>%
  addProviderTiles("CartoDB.Positron")%>%
  addPolygons(fillColor = ~pal((Death_cases/Confirmed_cases)*100),
              fillOpacity = 0.8,
              color = "black",
              weight = 1,
              popup=~paste(NAME_1, ", Case Fatality Rate:", as.character(round((Death_cases/Confirmed_cases)*100, digits=2)), '%'))%>%
  addMarkers(lng=106.816666,
             lat=-6.200000,
             popup=paste("Jakarta", ", Case Fatality Rate: 7.95%"))%>%
  addLegend("bottomleft",
            colors=brewer.pal(5,"YlOrRd"),
            labels=c("low","","","","high"),
            title="Case Fatality rate")

#Read and fix structure of columns in the global death rate dataset
w_death <- read.csv("deathrate.csv")
w_death$Entity <- as.character(w_death$Entity)
w_death$Date <- mdy(w_death$Date)

#Filter to regions and dates of interest
w_death <- w_death %>% filter(Entity %in% c("World", "United States", "Indonesia", "Asia", "Italy")&
                                Date>=ymd("2020-03-16")&Date<=ymd("2020-05-14"))

#Plot death rate data of different regions
ggplot(w_death)+
  geom_line(aes(x=Date, y=Case.fatality.rate.of.COVID.19......Only.observations.with.â..100.cases....., color=Entity))+
  ggtitle("Case Fatality rate in different countries as of May 14th 2020")+
  ylab("Case Fatality rate (%)") 

#Data sources:

#https://data.humdata.org/dataset/indonesia-covid-19-cases-recoveries-and-deaths-per-province
#https://github.com/pararawendy/border-desa-indonesia-geojson
#https://ourworldindata.org/covid-deaths

#Literary sources:

#Indonesia Age structure. (n.d.). Retrieved from 
#https://www.indexmundi.com/indonesia/age_structure.html

#Jakarta Post. (n.d.). Indonesia's latest official COVID-19 figures. Retrieved from 
#https://www.thejakartapost.com/news/2020/03/23/indonesias-latest-covid-19-figures.html

#Policy Responses to COVID19. (n.d.). Retrieved from 
#https://www.imf.org/en/Topics/imf-and-covid19/Policy-Responses-to-COVID-19


