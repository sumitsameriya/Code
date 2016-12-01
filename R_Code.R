# Different libraries used for various functions in the study
library(plyr)
library(Hmisc) 
library(data.table)
library(ggmap)

#Uploading a csv file from the local machine
Data <- read.csv(file = "Crash_Clean_all.csv", header = TRUE)
View(Data)
names(Data)


# Selecting Variables i.e. columns from the table
Var_data <- Data[c(1, 5, 6, 8)]
View(Var_data)


#Cleaning Variable 
Var_data[Var_data == ""] <- NA

Var_clean <- na.omit(Var_data)
View(Var_clean)

----------------------------------------------------------------------------
  
  # Segmenting different times into five group Mid Night, Morning, Afternoon, Evening, Night.   
  
  Data$Time_new  <- strptime(x = as.character(Data$TIME), format = "%H:%M")
View()

as.string(Data$Group)
Data$Group[] <- lapply(Data$Group, as.character)

Data$Group[Data$Time_new >= '2016-10-17 00:00:00' &
             Data$Time_new < '2016-10-17 04:59:00'] <- "Mid_Night"
Data$Group[Data$Time_new >= '2016-10-17 05:00:00' &
             Data$Time_new < '2016-10-17 11:59:00'] <- "Morning"
Data$Group[Data$Time_new >= '2016-10-17 12:00:00' &
             Data$Time_new < '2016-10-17 16:59:00'] <- "Afternoon"
Data$Group[Data$Time_new >= '2016-10-17 17:00:00' &
             Data$Time_new < ' 2016-10-17 19:59:00'] <- "Evening"
Data$Group[Data$Time_new >= '2016-10-17 20:00:00' &
             Data$Time_new < '2016-10-17 23:59:00'] <- "Night"

View(Data)

------------------------------------------------------------------------------------
  
  # Saving newly created Data table as CSV file
  
  write.csv(file = "Data.csv", x = Data)

-------------------------------------------------------------------------------------
  
  #Calculating Frequency of streets and changing column names
  
  Data <- read.csv(file = "Data.csv", header = TRUE)

Streets <- data.frame(table(Data$ON.STREET.NAME))
View(Streets)


setDT(Streets)[, z := (Freq / sum(Freq))]
View(Streets)

# Dividing Frequency into 10 segments
# cut2
Streets$Type <- as.numeric(cut2(Streets$Freq, g = 10))

#Renaming column names


rename(Streets,
       c(
         "Var1" = "Street Name",
         "z" = "Relative_Frequency",
         "Type" = "Road Type"
       ))

View(Streets)

write.csv(file = "Crash_rate.csv", x = Streets)

------------------------------------------------------------------------
  
  # for calculating Frequency of time and plotting "Road Type" and "Time of the Day"
  
  Crash_time <- data.frame(table(Data$Group))

library(data.table)
setDT(Crash_time)[, Time_z := (Freq / sum(Freq))]
View(Crash_time)

plot(Streets$Freq, Streets$Type, xlab = "Frequency", ylab = "Road Type")

plot(
  Crash_time$Var1,
  Crash_time$Time_z,
  xlab = "Time of the day",
  ylab = "Frequency of Accidents",
  col2rgb(136, alpha = FALSE)
)
----------------------------------------------------------
  
  #For checking Results  
  sum(Streets$z)
mean(Streets$z)


-----------------------------------------------------------
  
  
  # Selecting Variables i.e. columns from the table
  Var_data_1 <- Data[c(18:25)]
View(Var_data_1)


#Cleaning Variable 
Var_data_1[Var_data_1 == ""] <- NA

Var_clean_1 <- na.omit(Var_data_1)
View(Var_clean_1)

#Sum for different factors included in the study
Total_factors <- as.data.frame(colSums(Var_clean_1))
Total_factors['Category'] <- row.names(Total_factors)
names(Total_factors)[1] <-"Total"
View(Total_factors)

#Segmenting total number of people got killed and injured 
Injured <- Total_factors[c(3,5,7),]
Killed <- Total_factors[c(4,6,8),]

#Weighting index of killed peoples in different category 
setDT(Killed)[, z := (Total / sum(Total))]
View(Killed)

#Weighting index of injured peoples in different category 
setDT(Injured)[, z := (Total / sum(Total))]
View(Injured)


-------------------------------------------------------------------------------
  #Map Visuailization  
  
  
  map <- get_map(location = "New York", zoom = "auto" , source = "google", scale="auto")
ggmap(map)


mapPoints <- ggmap(map) +  geom_point(data = Crash_Clean_all.csv, aes(x = as.numeric(Longitude), y = as.numeric(Latitude), colour = "red" ), alpha =0.8, size = 1, shape = 21)
mapPoints

-------------------------------------------------------------------------------------
  
  # For routing between points  
  
  from <- 'Foley Square, New york, USA'
to <- 'VESEY STREET'
route <- route(from, to, structure = 'route', mode = 'Driving')
qmap('New york, USA', zoom = 16) +
  geom_path(
    aes(x = lon, y = lat),  colour = 'red', size = 1.5,
    data = route_df, lineend = 'round')
------------------------------------------------------------------------------------
  
  #Density plot
  
  #plot the roads Google Maps basemap
  map <- qmap('New york, USA', zoom = 12, maptype = 'roadmap')

#plot the density map
map + stat_density2d( aes(x = Longitude, y = Latitude ), size = 2, bins = 5, data = Crash_Clean_all.csv, geom = "polygon", fill = ..level.., alpha = ..level..*2) +
  scale_fill_gradient(low = "black", high = "red")
