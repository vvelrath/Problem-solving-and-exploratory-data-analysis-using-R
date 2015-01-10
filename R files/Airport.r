actualData <- read.csv("C:\\Users\\Jerry\\Desktop\\DIC_dataset\\2007.csv")
library(ggmap, rworldmap)
head(actualData)
destination <- as.vector(actualData$Dest)
origin <- as.vector(actualData$Origin)
lonlat <- geocode(unique(origin))
departures <- as.data.frame(table(origin))
arrivals <- as.data.frame(table(destination))
map <- get_map(location="USA", zoom=4)
mapPoints <- ggmap(map)+ geom_point(aes(x=lonlat$lon, y=lonlat$lat, size= origin), data=departures, alpha= .5)
mapPoints



diff <- arrivals$Freq-departures$Freq
origin <- data.frame(table(origin))


destination <- arrivals$destination
destination <- as.data.frame(destination)
destination$depArivDiff <- as.data.frame(diff)

#Arrival vs Departure in all airports
ggplot(destination, aes(arrivals$destination, diff)) + geom_bar(stat="identity",position="stack",aes(color=arrivals$destination))
destination[order(destination[,2]), ]


#Airline usage pattern
ggplot(actualData,aes(x=DayOfWeek,fill=UniqueCarrier))+geom_histogram(binwidth=1)



x <- data.frame(actualData$DepDelay, actualData$ArrDelay)
mat = as.matrix(x)
mat[!is.finite(mat)] <- 0
kclus <- kmeans(mat,5)
plot(mat,col = kclus$cluster)