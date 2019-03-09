#Aman Bhayana - amanbhay
#Pratik Agarwal - pagarwal

#---------------------------------------------------TASK 4------------------------------------------------------

influenza_national_summary <- function( filedata) {
  library(plotly)
  national_summary <- filedata
  week<- sprintf("%d %02d",national_summary$YEAR,national_summary$WEEK)
  week<- gsub(" ", "", week, fixed = TRUE)
  totalA <- national_summary$TOTAL.A
  totalB <- national_summary$TOTAL.B
  percentA <- national_summary$PERCENT.A
  percentB <- national_summary$PERCENT.B
  percentPositive <- national_summary$PERCENT.POSITIVE
  
    second_yaxis <- list(
    tickfont = list(color = "black"),
    overlaying = "y",
    side = "right",
    title = "Percent Positive"
  )
  
  t <- list(
    size = 8)
  

  data <- data.frame(week,totalA, totalB, percentA,percentB,percentPositive)
  
  p <- plot_ly(data) %>%
    add_trace(x=~week, y = ~totalB, type="bar", name = "Total B", color = I("dark green")) %>%
    add_trace(x=~week, y=~totalA, type = 'bar', name = 'Total A', color = I("yellow")) %>%
    add_trace(x=~week,y = ~percentA, type="scatter", mode="lines", name="Percentage A", color = I("orange"), yaxis='y2',line=list(dash="dash")) %>%
    add_trace(x=~week,y = ~percentB, type="scatter", mode="lines",name="Percentage B", color = I("green"), yaxis='y2', line=list(dash="dash")) %>%
    add_trace(x=~week,y = ~percentPositive, type="scatter", mode="lines", name="Total Percentage", color = I("black"), yaxis='y2') %>%
    layout( title = 'Influenza Positive Tests Reported to CDC by US-Cln-Lab, National Summary 2017-2018',yaxis = list(title = 'Total Positive Specimens '),  yaxis2=second_yaxis, barmode = "stack",font=t)
  p
}

influenza_positive_tested <-function(publicHealthfiledata, isNY = FALSE){
  if (isNY)
  {
    library(plotly)
    positive_test <- publicHealthfiledata
    noOfPositiveSpecimen <- positive_test$TOTAL.SPECIMENS
    aH1N1<-positive_test$A..2009.H1N1.
    aH3<-positive_test$A..H3.
    aSubtypyingNotPerformed<-positive_test$A..Subtyping.not.Performed.
    b<-positive_test$B
    bvc<-positive_test$BVic
    byam<-positive_test$BYam
    h3n2v<-positive_test$H3N2v
    season <-positive_test$SEASON_DESCRIPTION
    data<- data.frame(aSubtypyingNotPerformed,aH1N1,aH3,h3n2v,b,bvc,byam)
    pNY <- plot_ly(data)%>%
      add_trace(x=~week,y=~b,type="bar",name="B(lineage not performend)", color =I("blue"))%>%
      add_trace(x=~week,y=~bvc,type="bar",name="B(Victoria Lineage)", color =I("green"))%>%
      add_trace(x=~week,y=~byam,type="bar",name="B(Yamagata Lineage)", color =I("dark green"))%>%
      add_trace(x=~week,y=~h3n2v,type="bar",name="H3N2V", color =I("purple"))%>%
      add_trace(x=~week,y=~aH3,type="bar",name="A(H3N2)", color =I("red"))%>%
      add_trace(x=~week,y=~aH1N1,type="bar",name="A(H1N1)pdm09", color =I("orange"))%>%
      add_trace(x=~week,y=~aSubtypyingNotPerformed,type="bar",name="A(subtyping not performed)", color =I("yellow"))%>% 
      layout( title = 'Influenza Positive Tests Reported to CDC by US-Public Health Lab, National Summary 2017-2018-NY',xaxis= list(title = 'Season'),yaxis = list(title = 'Number of Positive Specimens '), barmode = "stack",font=t)
    pNY
  }
  else
  {
  t <- list(size =7)
  library(plotly)
  positive_test <- publicHealthfiledata
  week<- sprintf("%d %02d",positive_test$YEAR,positive_test$WEEK)
  week<- gsub(" ", "", week, fixed = TRUE)
  noOfPositiveSpecimen <- positive_test$TOTAL.SPECIMENS
  aH1N1<-positive_test$A..2009.H1N1.
  aH3<-positive_test$A..H3.
  aSubtypyingNotPerformed<-positive_test$A..Subtyping.not.Performed.
  b<-positive_test$B
  bvc<-positive_test$BVic
  byam<-positive_test$BYam
  h3n2v<-positive_test$H3N2v
  
  data<- data.frame(aSubtypyingNotPerformed,aH1N1,aH3,h3n2v,b,bvc,byam)
  p2 <- plot_ly(data)%>%
    add_trace(x=~week,y=~b,type="bar",name="B(lineage not performend)", color =I("blue"))%>%
    add_trace(x=~week,y=~bvc,type="bar",name="B(Victoria Lineage)", color =I("green"))%>%
    add_trace(x=~week,y=~byam,type="bar",name="B(Yamagata Lineage)", color =I("dark green"))%>%
    add_trace(x=~week,y=~h3n2v,type="bar",name="H3N2V", color =I("purple"))%>%
    add_trace(x=~week,y=~aH3,type="bar",name="A(H3N2)", color =I("red"))%>%
    add_trace(x=~week,y=~aH1N1,type="bar",name="A(H1N1)pdm09", color =I("orange"))%>%
    add_trace(x=~week,y=~aSubtypyingNotPerformed,type="bar",name="A(subtyping not performed)", color =I("yellow"))%>% 
    
    layout( title = 'Influenza Positive Tests Reported to CDC by US-Public Health Lab, National Summary 2017-2018',xaxis= list(title = 'Weeks'),yaxis = list(title = 'Number of Positive Specimens '), barmode = "stack",font=t)
 p2
  }
}

#-1) Influenza national summary (green and yellow chart)
influenza_national_summary(read.csv(file.choose(),skip = 1))


#-2) Positive tested
influenza_positive_tested(read.csv(file.choose(),skip =1))

#-3) Mortality
library(plotly)
mortality_csv <- read.csv(file.choose())
percentPI <-  mortality_csv$PERCENT.P.I 
threshold <- mortality_csv$THRESHOLD
baseline <- mortality_csv$BASELINE
week <- sprintf("%02d",mortality_csv$WEEK)
displayWeek <- week
displayWeek
week <- paste(mortality_csv$SEASON, week)
f <- list( font = 4)

dfMortality <- data.frame(displayWeek,week, percentPI, threshold,baseline)

p <- plot_ly(dfMortality, x = ~week, y = ~percentPI, name = 'Percent Death due to Pneumonia and Influenza', type = 'scatter', mode = 'lines',
             line = list(color = 'red')) %>%
  add_trace(y = ~baseline, name = 'Seasonal Baseline', line = list(color = 'black')) %>%
  add_trace(y = ~threshold, name = 'Epidemic Threshold', line = list(color = 'black')) %>%
  layout(title = "Pneumonia and Influenza Mortality",
         xaxis = list(title = "MMWR Week",categoryorder = "array",categoryarray = week ), # https://stackoverflow.com/questions/40701491/plot-ly-in-r-unwanted-alphabetical-sorting-of-x-axis   - TO avoid unwanted sorting of a column using plotly 
         yaxis = list (title = "% of All Deaths due to P & I") ,font = f
         )
p



#-4) Pediatric Death 

library(plotly)
pdeath <- read.csv(file.choose(), skip =1)
week<- pdeath$WEEK.NUMBER
noOfDeath <- pdeath$NO..OF.DEATHS
previousDeath <- pdeath$PREVIOUS.WEEK.DEATHS
currentDeath<- pdeath$CURRENT.WEEK.DEATHS

data <- data.frame(week, noOfDeath, previousDeath, currentDeath)

p1<- plot_ly(data, x= ~week , y = ~previousDeath,name ='Previous Deaths', type = 'bar',color = I('dark green') )%>%
    add_trace( y =~currentDeath, color = I( 'pink') , name ="Current Deaths")%>%
  layout(title = "Number of Influenza-Associated Pediatric Deaths",
         xaxis = list(title = "Week" ), 
         yaxis = list (title = "Number of deaths"))
p1




#-5) Influenza Like Ilness
library(plotly)
ili <- read.csv(file.choose(), skip =1)
#week <- sprintf("%02d", ili$WEEK)
#week<- paste( ili$YEAR, week)
#season2009 <-ili[ili$YEAR == '2009',]
week <- sprintf("%04d%02d",ili$YEAR,ili$WEEK)

# Reference -http://www.cookbook-r.com/Basics/Getting_a_subset_of_a_data_structure/


season2009 <- subset(ili, (YEAR == 2009 & WEEK >=40) | (YEAR == 2010 & WEEK < 40))
season2011 <- subset(ili, (YEAR==2011 & WEEK>=40) | (YEAR == 2012 & WEEK < 40))
season2014 <- subset(ili, (YEAR==2014 & WEEK>=40 & WEEK <= 52) | (YEAR == 2015 & WEEK < 40 ))
season2015 <- subset(ili, (YEAR==2015 & WEEK>=40) | (YEAR == 2016 & WEEK < 40))
season2016 <- subset(ili, (YEAR==2016 & WEEK>=40) | (YEAR == 2017 & WEEK <40))
season2017 <- subset(ili, (YEAR==2017 & WEEK>=40) | (YEAR == 2018 & WEEK<40))
season2018 <- subset(ili, (YEAR==2018 & WEEK>=40) | (YEAR == 2018 & WEEK<40))


percentVisit09 <- season2009$X..WEIGHTED.ILI
percentVisit11 <- season2011$X..WEIGHTED.ILI
percentVisit14 <- season2014$X..WEIGHTED.ILI
percentVisit15 <- season2015$X..WEIGHTED.ILI
percentVisit16 <- season2016$X..WEIGHTED.ILI
percentVisit17 <- season2017$X..WEIGHTED.ILI
percentVisit18 <- season2018$X..WEIGHTED.ILI

week<- season2018$WEEK
week
data <- data.frame(week,percentVisit09,percentVisit11,percentVisit14,percentVisit15,percentVisit16,percentVisit17,percentVisit18)
data
p<- plot_ly(data, x=~week, y =~percentVisit09, type = 'scatter',name = "2009-2010 Season", mode = 'lines')%>%
  add_trace(y=~percentVisit11 ,name = "2011-2012 Season" , color = I('pink') )%>%
  add_trace(y=~percentVisit14 ,name = "2014-2015 Season" , color = I('orange') )%>%
  add_trace(y=~percentVisit15 ,name = "2015-2016 Season" , color = I('blue') )%>%
  add_trace(y=~percentVisit16 ,name = "2016-2017 Season" , color = I('cyan') )%>%
  add_trace(y=~percentVisit17 ,name = "2017-2018 Season" , color = I('black') )%>%
  add_trace(y=~percentVisit18 ,name = "2018-2019 Season" , color = I('red') )%>%
  layout(xaxis = list(title = "Week", tickmode = 'linear'), yaxis = list(title = "% of Visist for ILI"), font = list(f=4))
p


#-6)Heat Map

# Reference -https://stackoverflow.com/questions/29614972/ggplot-us-state-map-colors-are-fine-polygons-jagged-r

library(ggplot2)
library(maptools)
library(RColorBrewer)
library(usmap)

heatMap_csv <- read.csv(file.choose())
region <- tolower(heatMap_csv$STATENAME)
heatMap_csv$STATENAME<-tolower( heatMap_csv$STATENAME)
integerActivityLevel <-as.numeric( substr( heatMap_csv$ACTIVITY.LEVEL, 6, 8 ))
total <- data.frame(heatMap_csv,integerActivityLevel)
write.csv(total,"heatMap_edited.csv")
editedHeatmapCsv <- read.csv("heatMap_edited.csv")
editedHeatmapCsv$STATENAME <- tolower(editedHeatmapCsv$STATENAME)
uniqueStates<-unique(editedHeatmapCsv$STATENAME)
statesAverageActivity<- aggregate(.~STATENAME, data =editedHeatmapCsv, mean)
data<- statesAverageActivity[,c("STATENAME","ACTIVITY.LEVEL")]
#total<- c(uniqueStates,statesAverageActivity)
names(data ) <-c("state","activity")
dfTotal <-data.frame(data)
dfTotal

plot_usmap(data = dfTotal, values = "activity", lines = "black", regions = "states", labels = TRUE) +
  scale_fill_continuous(
    low = "white", high = "red", name = "ILI Activity Level", label = scales::comma
  ) + theme(legend.position = "right") + labs(title = "2018-19 Influenza Season Week 8 ending Feb 23, 2019")
#+scale_fill_gradientn(colours = rev(brewer.pal(11, name="RdYlGn")))

#---------------------------------------------------------------------------------------------------------------
#Influenza Virus Characterization
#Partitioning the screen in 5 divisions

par(mar=c(5,4,6,2)+0.1)
layout(matrix(c(1,2,3,1,4,5), 2, 3, byrow = TRUE))

datap<-read.csv(file.choose(), skip=1)
datap1<-read.csv(file.choose())
#Left graph
slices<-datap[,c("A..2009.H1N1.","A..H3.","A..Subtyping.not.Performed.","B","BVic","BYam","H3N2v")]
combo<-colSums(slices)
colr<-c("orange","red","yellow","lightgreen","green","purple","black")
pie(combo,labels = combo,radius = 1.0, col=colr)


legend(
  "bottom", c("A..2009.H1N1.","A..H3.","A..Subtyping.not.Performed.","B","BVic","BYam","H3N2v" ),
  #col = c("orange","red","yellow","lightgreen","green","purple","black"),
  fill = colr
  #bty = "n"
)
#Partioning the area for 2 rows and 2 colums
#par(mfrow=c(2,2))
#cl=colors (distinct = TRUE)


#Graph 1
b<-datap1[which(datap1$X.Sub.type=='H3') & (datap1$B.Lineage=='Yamagata') &(datap1$X.Sub.type!='B') & (datap1$X.Sub.type!='H1pdm09'),]

#(datap1$Sequence.Genetic.Group=='3C.2a') && (datap1$Sequence.Genetic.Group=='3C.2a1') && (datap1$Sequence.Genetic.Group=='3C.3a'),]
displayper2<- b$X..of.Total.Distinct.count.of.Cdc.Id..
displayrow2<- b$Distinct.count.of.Cdc.Id..
name1=c("3C2a","3C 2a1", "3C 3a")
lab1=paste(displayper2,"\n", name1,"\n", displayrow2)

pie(displayrow2, labels=lab1, main = "Influenza H3", col = 4:7)


#Graph 2
c<-datap1[which(datap1$X.Sub.type=='H1pdm09') & (datap1$B.Lineage=='Yamagata') & (datap1$X.Sub.type!='B') & (datap1$X.Sub.type !='H3'), ]
displayrow3<- c$Distinct.count.of.Cdc.Id..
displayper3<- c$X..of.Total.Distinct.count.of.Cdc.Id..
df3<-data.frame(displayper3)
df3<-as.numeric(df3)
lab2=paste(displayrow3,"\n", name1,"\n", displayper3)
pie(df3, labels = lab2, main = "Influenza H1pdm09", col="gold")

#Graph 3
a<-datap1[which(datap1$X.Sub.type=='B') & (datap1$B.Lineage=='Victoria'), ]
displayper1<- a$X..of.Total.Distinct.count.of.Cdc.Id..
displayrow1<- a$Distinct.count.of.Cdc.Id..
name3<-c("V1A","V1A.1","V1A-3DEL")
lab3=paste(name3,"\n", displayrow1,"\n", displayper1)
pie(displayrow1, labels=lab3, main="Influenza B Victoria", col = 4:7)

#Graph 4
d<-datap1[which(datap1$X.Sub.type=='B') & (datap1$B.Lineage=='Yamagata') & (datap1$X.Sub.type!='H1pdm09') & (datap1$X.Sub.type !='H3'), ]
displayrow4<- d$Distinct.count.of.Cdc.Id..
displayper4<- d$X..of.Total.Distinct.count.of.Cdc.Id..
name4<-c("Y3")
lab4<-paste(name4,"\n",displayrow4,"\n",displayper4)
pie(displayrow4, labels=lab4, main="Influenza B Yamagata", col = 4:7)

mtext("Sequence Results, by Genetic HA Clade/Subclade, of Specimens \n Submitted to CDC by U.S. Public Health Laborataries,Cumulative,2018-2019 Season", outer = TRUE , side = 3 , line = 4)
  

#-------------------------------------------------TASK 5--------------------------------------------------------

#-1) Influenza national summary (green and yellow chart)
influenza_national_summary(read.csv(file.choose(),skip=1))


#-2) Positive tested
influenza_positive_tested(read.csv(file.choose(),skip=1))



#-------------------------------------------------TASK 6--------------------------------------------------------

#-NY State Influenza Positive Tested
influenza_positive_tested(read.csv(file.choose(),skip=1),TRUE)


#-NY State Summary
influenza_national_summary(read.csv(file.choose(),skip=1))
