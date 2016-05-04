#读取csv
brazil <- read.csv("brazil_daily.csv")
china <- read.csv("China_daily.csv")
india <- read.csv("India_daily.csv")
rusia <- read.csv("Rusia_daily.csv")
#抽取数据
brazil1 <- data.frame(brazil$Date,brazil$Adj.Close)
china1 <- data.frame(china$Date,china$Adj.Close)
india1 <- data.frame(india$Date,india$Adj.Close)
rusia1 <- data.frame(rusia$Date,rusia$Adj.Close)
#重命名数据
names(brazil1) <- c("date","brazil")
names(china1) <- c("date","china")
names(india1) <- c("date","india")
names(rusia1) <- c("date","rusia")
#时间数据格式不同，都转为Date才能匹配
china1$date <- as.Date(china1$date)
brazil1$date <- as.Date(brazil1$date)
india1$date <- as.Date(india1$date)
rusia1$date <- as.Date(rusia1$date)
#按照日期匹配合并
CB <- merge(china1,brazil1,by="date")
CIB <- merge(CB,india1,by="date")
CIBR <- merge(CIB,rusia1,by="date")
length(CIBR$date)

#用plyr join
Aid90s00sJoin <- join(x = Aid_90s, y = Aid_00s, by = c("Country.Name", "Program.Name")) 
