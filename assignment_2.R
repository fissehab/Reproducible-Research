
  # Reproducible Research/Assignment2
   
   rm(list=ls())    # clear workspace
   
   # Load required libraries
   
   library(plyr)
   
   library(ggplot2)
   
   
   
   storm <- tempfile()
   
   download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", storm)
   
   data <- data.table::data.table(read.table(storm, sep = ",", header = T, stringsAsFactors = F))
   
   unlink(storm)
   
   
   names(data)
   
   # Extract the variables of interest for further analysis
   
   data2<-data[,c("EVTYPE", "FATALITIES", "INJURIES",
            "PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")]
   
   summary(data2)
   
   str(data2)
   
   fatalities<-aggregate(FATALITIES~EVTYPE,data2,sum)
   injuries<-aggregate(INJURIES~EVTYPE,data2,sum)
   
   # let see the top 7 stroms with the highest INJURIES and FATALITIES
   
   top_7_fatality<-arrange(fatalities, desc(fatalities$FATALITIES))[1:7,]
   top_7_injury<-arrange(injuries, desc(injuries$INJURIES))[1:7,]
   
   

   
   ggplot(top_7_fatality, aes(x=reorder(EVTYPE, FATALITIES), y=FATALITIES,fill=FATALITIES))+ 
       geom_bar(stat='identity',colour='white')+
       ggtitle('Top 7 Storm Events by Fatality')+
       xlab('Type of Event')+
       ylab('Total number of Deaths')
   
   
   
   ggplot(top_7_injury, aes(x=reorder(EVTYPE, INJURIES), y=INJURIES,fill=INJURIES))+ 
       geom_bar(stat='identity',colour='white')+
       ggtitle('Top 7 Storm Events by Injuries')+
       xlab('Type of Event')+
       ylab('Total number of Injuries')
      
   
   
   
  
       
   system.time({ 
       for (i in 1:length(data2$CROPDMGEXP)){
       
       if (data2$PROPDMGEXP[i]=='k' | data2$PROPDMGEXP[i]=="K")
       {data2$PROPDMG[i]=data2$PROPDMG[i]*10^3}
       
       else if (data2$PROPDMGEXP[i]=='B')
       {data2$PROPDMG[i]=data2$PROPDMG[i]*10^9}
       
       else if (data2$PROPDMGEXP[i]=='m' | data2$PROPDMGEXP[i]=='M')
       {data2$PROPDMG[i]=data2$PROPDMG[i]*10^6}
       
       else if (data2$PROPDMGEXP[i]=='h' | data2$PROPDMGEXP[i]=='H')
       {data2$PROPDMG[i]=data2$PROPDMG[i]*10^2}
       
       else if (is.numeric(data2$PROPDMGEXP[i]))
       {data2$PROPDMG[i]=data2$PROPDMG[i]*10^data2$PROPDMGEXP[i]}
       
       
       else if (data2$CROPDMGEXP[i]=='k' | data2$CROPDMGEXP[i]=="K")
       {data2$CROPDMG[i]=data2$CROPDMG[i]*10^3}
       
       else if (data2$CROPDMGEXP[i]=='B')
       {data2$CROPDMG[i]=data2$CROPDMG[i]*10^9}
       
       else if (data2$CROPDMGEXP[i]=='m' |data2$CROPDMGEXP[i]=='M')
       {data2$CROPDMG[i]=data2$CROPDMG[i]*10^6}
       
       else if (data2$CROPDMGEXP[i]=='h' | data2$CROPDMGEXP[i]=='H')
       {data2$CROPDMG[i]=data2$CROPDMG[i]*10^2}
       
       else if (is.numeric(data2$CROPDMGEXP[i]))
       {data2$CROPDMG[i]=data2$CROPDMG[i]*10^data2$CROPDMGEXP[i]}   
   }
   
   }) 
   
   
   