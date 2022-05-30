## @knitr Setup
library(tidyverse)
library(janitor)
library(readr)

RecovPeriod=14
MinimumTotalCount=10
MinimumTargetLine=-0.1
StartSocDist="2020-03-19"
StartPlotReporting="2020-03-01"
source("ConfigCounties.R")
StateData=read_csv(file="../DataWithScript/StateData.csv") %>% 
            filter(State==CurrentState)

NRecState=nrow(StateData)

if (nrow(StateData[!complete.cases(StateData),])!=0){
  print("NAs in data")
}else{
  print("No NAsin data")
}


for (i in c(1:RecovPeriod)) {
  StateData[i,"EstActCases"]=StateData[i,"ConfirmedCases"]
}

for (i in c((RecovPeriod+1):NRecState)) {
  StateData[i,"EstActCases"]=StateData[i-1,"EstActCases"]+StateData[i,"NewConfirmedCases"]-StateData[i-RecovPeriod,"NewConfirmedCases"]
}

StateData=StateData %>%
  mutate(GrowthRateEstActCases=log(EstActCases)-log(lag(EstActCases)))

ObservationAtSocDistStartState=NewCasesStateAtSocDist=StateData %>% 
           filter(Date==as.Date(StartSocDist))

StateData = StateData %>% 
                filter(Date>=as.Date(StartPlotReporting))

PlotTotalCasesState=ggplot(data=StateData,aes(x=Date,y=ConfirmedCases))+
  geom_smooth(span = 0.5)+
  geom_line()+
  ylab("Accumulated Cases")+
  labs(title="Accumulated Cases (including recovered cases and deaths)")+
  scale_x_date(date_breaks = "3 week", date_labels = "%m/%d")


PlotNewCasesState=ggplot(data=StateData,aes(x=Date,y=NewConfirmedCases))+
  geom_line()+
  geom_smooth(span = 0.5)+
  geom_vline(xintercept = as.numeric(max(StateData$Date)-RecovPeriod), linetype="dashed")+
  annotate("text", x = max(StateData$Date)-RecovPeriod,
           y = max(StateData$NewConfirmedCases, na.rm = TRUE)*0.85,
           angle = 90, label = "14 days ago")+
  geom_vline(xintercept = as.numeric(as.Date(StartSocDist)), linetype="dashed", color="Red")+
  annotate("text", x = as.Date(StartSocDist),
           y = max(StateData$NewConfirmedCases, na.rm = TRUE)*0.85,
           angle = 90, label = "Start social dist.")+
  geom_hline(yintercept = ObservationAtSocDistStartState[["NewConfirmedCases"]], linetype="dashed", color="Red")+
      ylab("New Cases")+
  labs(title="New Cases by Day")+
  scale_x_date(date_breaks = "3 week", date_labels = "%m/%d")


PlotEstActCasesState= ggplot(data=StateData,aes(x=Date,y=EstActCases))+
  geom_line()+
  geom_smooth(span = 0.5)+
  geom_vline(xintercept = as.numeric(max(StateData$Date)-RecovPeriod), linetype="dashed")+
  annotate("text", x = max(StateData$Date)-RecovPeriod,
           y = max(StateData$EstActCases, na.rm = TRUE)*0.85,
           angle = 90, label = "14 days ago")+
  geom_vline(xintercept = as.numeric(as.Date(StartSocDist)), linetype="dashed", color="Red")+
  annotate("text", x = as.Date(StartSocDist),
           y = max(StateData$EstActCases, na.rm = TRUE)*0.85,
           angle = 90, label = "Start social dist.")+
  geom_hline(yintercept = ObservationAtSocDistStartState[["EstActCases"]], linetype="dashed", color="Red")+
  ylab("Active Cases")+
  labs(title="Predicted Active Cases")+
  scale_x_date(date_breaks = "3 week", date_labels = "%m/%d")


PlotGrowthRateEstActCasesState=ggplot(data=StateData,aes(x=Date,y=GrowthRateEstActCases))+
  geom_smooth(span = 0.5)+
  geom_line()+
  scale_y_continuous(labels = scales::percent_format())+
  ylab("Daily % Change")+
  labs(title="%-Change of Predicted Active Cases")+
  scale_x_date(date_breaks = "3 week", date_labels = "%m/%d")+ 
  geom_hline(yintercept=MinimumTargetLine, color="red")+
  annotate("text", x = mean(range(StateData$Date)), y=MinimumTargetLine-0.03, color="red",
           label = "Target Line")

####### County Section ###########
PlotTotalCases=list()
PlotNewCases=list()
PlotEstActCases=list()
PlotGrowthRateEstActCases=list()

CountyData=read_csv(file = "../DataWithScript/CountyData.csv")%>% 
            filter(State==CurrentState)
          
for (CountyName in CurrentCounties) {
TempCountyData=CountyData %>% filter(County==CountyName,
                                       Date>=as.Date(StartPlotReporting))
NRecTempCounty=nrow(TempCountyData)

if (nrow(TempCountyData[!complete.cases(TempCountyData),])!=0){
  print (CountyName)
  print("Nas in data")
}


for (i in c(1:RecovPeriod)) {
  TempCountyData[i,"EstActCases"]=TempCountyData[i,"ConfirmedCases"]
}

for (i in c((RecovPeriod+1):NRecTempCounty)) {
  TempCountyData[i,"EstActCases"]=TempCountyData[i-1,"EstActCases"]+TempCountyData[i,"NewConfirmedCases"]-TempCountyData[i-RecovPeriod,"NewConfirmedCases"]
}

TempCountyData=TempCountyData %>%
  mutate(GrowthRateEstActCases=log(EstActCases)-log(lag(EstActCases)))

TempCountyData=TempCountyData %>% filter(ConfirmedCases>=MinimumTotalCount)

ObservationAtSocDistStartTempCountyData=TempCountyData %>% 
  filter(Date==as.Date(StartSocDist))

PlotTotalCases[[CountyName]]=ggplot(data=TempCountyData,aes(x=Date,y=ConfirmedCases))+
  geom_smooth(span = 0.5)+
  geom_line()+
  ylab("Accumulated Cases")+
  labs(title="Accumulated Cases (including recovered cases and deaths)")+
  scale_x_date(date_breaks = "3 week", date_labels = "%m/%d")


PlotNewCases[[CountyName]]=ggplot(data=TempCountyData,aes(x=Date,y=NewConfirmedCases))+
  geom_line()+
  geom_smooth(span = 0.5)+
  geom_vline(xintercept = as.numeric(max(TempCountyData$Date)-RecovPeriod), linetype="dashed")+
  annotate("text", x = max(TempCountyData$Date)-RecovPeriod, 
           y = max(TempCountyData$NewConfirmedCases, na.rm = TRUE)*0.85, 
           angle = 90, label = "14 days ago")+
  geom_vline(xintercept = as.numeric(as.Date(StartSocDist)), linetype="dashed", color="Red")+
  annotate("text", x = as.Date(StartSocDist),
           y = max(TempCountyData$NewConfirmedCases, na.rm = TRUE)*0.85,
           angle = 90, label = "Start social dist.")+
  ylab("New Cases")+
  labs(title="New Cases by Day")+
  scale_x_date(date_breaks = "3 week", date_labels = "%m/%d")

if (nrow(ObservationAtSocDistStartTempCountyData)>0){
  PlotNewCases[[CountyName]]=PlotNewCases[[CountyName]]+
  geom_hline(yintercept = ObservationAtSocDistStartTempCountyData[["NewConfirmedCases"]], linetype="dashed", color="Red")
}  


PlotEstActCases[[CountyName]]= ggplot(data=TempCountyData,aes(x=Date,y=EstActCases))+
  geom_line()+
  geom_smooth(span = 0.5)+
  geom_vline(xintercept = as.numeric(max(TempCountyData$Date)-RecovPeriod), linetype="dashed")+
  annotate("text", x = max(TempCountyData$Date)-RecovPeriod, 
           y = max(TempCountyData$EstActCases, na.rm = TRUE)*0.85, 
           angle = 90, label = "14 days ago")+
  geom_vline(xintercept = as.numeric(as.Date(StartSocDist)), linetype="dashed", color="Red")+
  annotate("text", x = as.Date(StartSocDist),
           y = max(TempCountyData$EstActCases, na.rm = TRUE)*0.85,
           angle = 90, label = "Start social dist.")+
  ylab("Est. Act. Cases")+
  labs(title="Predicted Active Cases")+
  scale_x_date(date_breaks = "3 week", date_labels = "%m/%d")

if (nrow(ObservationAtSocDistStartTempCountyData)>0){
  PlotEstActCases[[CountyName]]=PlotEstActCases[[CountyName]]+
    geom_hline(yintercept = ObservationAtSocDistStartTempCountyData[["EstActCases"]], linetype="dashed", color="Red")
}
PlotGrowthRateEstActCases[[CountyName]]=ggplot(data=TempCountyData,aes(x=Date,y=GrowthRateEstActCases))+
  geom_smooth(span = 0.5)+
  geom_line()+
  scale_y_continuous(labels = scales::percent_format())+
  ylab("Daily % Change")+
  labs(title="%-Change of Predicted Active Cases")+
  scale_x_date(date_breaks = "3 week", date_labels = "%m/%d")+ 
  geom_hline(yintercept=MinimumTargetLine, color="red")+
  annotate("text", x = mean(range(TempCountyData$Date)), y=MinimumTargetLine-0.03, color="red",
           label = "Target Line")
}# end loop CountyName

