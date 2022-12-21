setwd("~/Downloads/project")
dcon <- dbConnect(SQLite(), dbname = 'data.db')


### Clean data
res <- dbSendQuery(conn = dcon, "
SELECT *
FROM US_Accidents;
")
accidents <- dbFetch(res, -1)
dbClearResult(res)


############
num_cols <- unlist(lapply(accidents, is.numeric))
data_num <- accidents[, num_cols]
data_num <- data_num[,-c(7,14)]
data_num<- na.omit(data_num)

############
dcon <- dbConnect(SQLite(), dbname = 'data.db')
res <- dbSendQuery(conn = dcon, "
SELECT *
FROM DelayTime1
")
DelayTime <- dbFetch(res, -1)
dbClearResult(res)
DelayTime$Time <- as.numeric(gsub("([0-9]+).*$", "\\1", DelayTime$T))
DelayTime$Time[27:75] <- c(rep(8,4),rep(5,4),rep(4,4),rep(9,4),2,rep(1,4),rep(7,4),rep(6,4),rep(10,2),rep(3,4),rep(2,4),c(10,10,15,15,20,30,30,45,5,60))
DelayTime <- subset(DelayTime, select = -T)
colnames(DelayTime)[2] <- 'Freq'

############
convert_hrs <- function(dt){
  return(hours(dt))
  
}

accidents_new <- accidents %>%
  mutate(startHr=hour(Start_Time))


accidents_count <- accidents_new %>%
  count(startHr)

accidents_severity <- accidents_new %>%
  group_by(startHr) %>%
  summarise(mean(Severity))


accident_summary <- merge(accidents_count, accidents_severity)
accident_summary <- accident_summary %>% rename(sev_avg = "mean(Severity)")



### Ploting 
#Plot 1 - Correlation Plot
corrplot(cor(data_num), method = "circle", title = 'Correlation Plot', 
           tl.cex = 0.5, mar=c(0,0,1,0))

  
  
#Plot 2 - Severity Delay Time
DelayTime %>% arrange(Severity, Time) %>%
  group_by(Severity,Time) -> df_density

df <- accidents
ggplot(data = df, aes(Severity)) + geom_bar(fill= 'darkgreen') + 
  ggtitle('Count Plot for Severity') + 
  theme(plot.title=element_text(face='bold', hjust=0.5))

plot(df_density$Time[1:10],df_density$Freq[1:10], type = 'l',
     ylim = c(1,4600000), xlim = c(0,15),ylab = 'Freq', xlab = 'Time', main = 'Delay Time and Severity')
lines(df_density$Time[11:20],df_density$Freq[11:20], col = 'red')
lines(df_density$Time[22:46],df_density$Freq[22:46],col = 'blue')

#Plot 3 - Amount of car accidents by hour

ggplot(data = accident_summary) +
  annotate(geom="rect", xmin=-Inf, xmax=6, ymin=-Inf, ymax=Inf, fill="blue",
           alpha=0.5) +
  annotate(geom="rect", xmin=6, xmax=19, ymin=-Inf, ymax=Inf, fill="yellow",
           alpha=0.5) +
  annotate(geom="rect", xmin=19, xmax=Inf, ymin=-Inf, ymax=Inf, fill="blue",
           alpha=0.5) +
  geom_col(mapping=aes(x=startHr, y=n, fill=sev_avg)) +
  scale_fill_distiller(palette="Reds", trans= "reverse") +
  labs(
    title = "Amount of car accidents by hour",
    x = "Hour",
    y = "Number of accidents",
    caption = "A Countrywide Traffic Accident Dataset, 2016-2020.",
    fill = "Average Severity") +
  scale_y_continuous(expand = expansion(mult = c(0, .1)))



#Plot 4 - Severity vs. Traffic Distance , Visibility vs. Severity of Accidents

mean_data <- function(x){
  return(mean(x, na.rm=TRUE))
}
par(mfrow=c(1,2))
barplot(tapply(df$`Distance(mi)`, df$Severity, FUN=mean_data), col='orange', 
        main='Severity vs.Traffic Distance',
        ylab='Average Distance of Traffic', xlab='Severity')

barplot(tapply(df$`Distance(mi)`, df$Severity, FUN=mean_data), col='skyblue3',
        main='Visibility vs. Severity of Accidents',
        xlab='Severity', ylab='Visibility') 


#Plot 5 - States with the Most, Least Accidents

# 5 States with most accidents
df_subset_max = df[df$State == 'CA' | df$State == 'FL'| df$State == 'OR'| 
                     df$State == 'TX'| df$State == 'NY', ]
# 5 States with least accidents
df_subset_min = df[df$State == 'SD' | df$State == 'WY'| df$State == 'VT'| 
                     df$State == 'ND'| df$State == 'NM', ]


plot1 <- ggplot(df_subset_max, aes(State, `Temperature(F)`)) + 
  geom_boxplot(fill='brown') +
  ggtitle('States with the Most Accidents') +
  theme(plot.title=element_text(face='bold', hjust=0.5),
        plot.background = element_rect(fill='skyblue')) 

plot2 <- ggplot(df_subset_min, aes(State, `Temperature(F)`)) + 
  geom_boxplot(fill='brown') +
  ggtitle('States with the Least Accidents') +
  theme(plot.title=element_text(face='bold', hjust=0.5),
        plot.background = element_rect(fill='skyblue')) 

grid.arrange(plot1, plot2, ncol=2)



#Plot 6 - Traffic Distance

barplot(tapply(df$`Distance(mi)`, df$Severity, FUN=mean_data), col='orange', 
        main='Traffic Distance',
        ylab='Average Distance of Traffic', xlab='Severity')








