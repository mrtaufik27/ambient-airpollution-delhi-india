setwd("D:/project/nagato/data and command/2000-2015/delhi")
#setwd("D:/Ibu/PROJECT/nagato/delhi")

##########################################
# DATA MANAGEMENT AND DATA CLEANSING
##########################################

##########################################
#2000
##########################################

d <- read.csv("cpcb_dly_aq_delhi-2000.csv", header=T)
str(d)
d$st <- ifelse(d$Stn.Code==56,	"Ashok Vihar",
          ifelse(d$Stn.Code==59,	"Janakpuri",
                 ifelse(d$Stn.Code==55,	"Nizamuddin",
                        ifelse(d$Stn.Code==58,	"Shahadra",
                               ifelse(d$Stn.Code==57,	"Shahzada Bagh",
                                      ifelse(d$Stn.Code==60,	"Siri Fort",
                                             ifelse(d$Stn.Code==345,	"Mayapuri",
                                                    ifelse(d$Stn.Code==145,	"Mayapuri",
                                                           ifelse(d$Stn.Code==144,	"N.Y. School",
                                                           ifelse(d$Stn.Code==146,	"Town Hall",
                                                                  ifelse(d$Stn.Code==531,	"Pritampura","undefined"
    )))))))))))

d$st <- factor(d$st)
d$yr <- "2000"
d$yr <- factor(d$yr)

d$mn <- ifelse(d$Sampling.Date=="January - M012000","Jan",
               ifelse(d$Sampling.Date=="February - M022000","Feb",
                      ifelse(d$Sampling.Date=="March - M032000","Mar",
                             ifelse(d$Sampling.Date=="April - M042000", "Apr",
                                    ifelse(d$Sampling.Date=="May - M052000","May",
                                           ifelse(d$Sampling.Date=="June - M062000","Jun",
                                                  ifelse(d$Sampling.Date=="July - M072000","Jul",
                                                         ifelse(d$Sampling.Date=="August - M082000","Aug",
                                                                ifelse(d$Sampling.Date=="September - M092000","Sep",
                                                                       ifelse(d$Sampling.Date=="October - M102000","Oct",
                                                                              ifelse(d$Sampling.Date=="November - M112000","Nov",
                                                                                     ifelse(d$Sampling.Date=="December - M122000","Dec","Undefined"))))))))))))



d$mn <- factor(d$mn)
str(d)

d$typ <- ifelse(d$Type.of.Location=="Industrial Area", "ind",
                ifelse(d$Type.of.Location=="Residential, Rural and other Areas", "rsd","undefined"))
d$typ <- as.factor(d$typ)
d$PM10 <- d$RSPM.PM10

names(d)
dat <- as.data.frame(d[,c(13,12,11,15,7,8,14,10)])
write.csv(dat,"2000.csv")


##########################################
#2001
##########################################
d <- read.csv("cpcb_dly_aq_delhi-2001.csv", header=T)
str(d)

d$st <- ifelse(d$Stn.Code==56,	"Ashok Vihar",
               ifelse(d$Stn.Code==59,	"Janakpuri",
                      ifelse(d$Stn.Code==55,	"Nizamuddin",
                             ifelse(d$Stn.Code==58,	"Shahadra",
                                    ifelse(d$Stn.Code==57,	"Shahzada Bagh",
                                           ifelse(d$Stn.Code==60,	"Siri Fort",
                                                  ifelse(d$Stn.Code==345,	"Mayapuri",
                                                         ifelse(d$Stn.Code==145,	"Mayapuri",
                                                                ifelse(d$Stn.Code==144,	"N.Y. School",
                                                                ifelse(d$Stn.Code==146,	"Town Hall",
                                                                       ifelse(d$Stn.Code==531,	"Pritampura","undefined"
                                                                       )))))))))))

d$st <- factor(d$st)
d$yr <- "2001"
d$yr <- factor(d$yr)

d$mn <- ifelse(d$Sampling.Date=="January - M012001","Jan",
               ifelse(d$Sampling.Date=="February - M022001","Feb",
                      ifelse(d$Sampling.Date=="March - M032001","Mar",
                             ifelse(d$Sampling.Date=="April - M042001", "Apr",
                                    ifelse(d$Sampling.Date=="May - M052001","May",
                                           ifelse(d$Sampling.Date=="June - M062001","Jun",
                                                  ifelse(d$Sampling.Date=="July - M072001","Jul",
                                                         ifelse(d$Sampling.Date=="August - M082001","Aug",
                                                                ifelse(d$Sampling.Date=="September - M092001","Sep",
                                                                       ifelse(d$Sampling.Date=="October - M102001","Oct",
                                                                              ifelse(d$Sampling.Date=="November - M112001","Nov",
                                                                                     ifelse(d$Sampling.Date=="December - M122001","Dec","Undefined"))))))))))))



d$mn <- factor(d$mn)
d$typ <- ifelse(d$Type.of.Location=="Industrial Area", "ind",
                ifelse(d$Type.of.Location=="Residential, Rural and other Areas", "rsd","undefined"))
d$typ <- as.factor(d$typ)
d$PM10 <- d$RSPM.PM10

names(d)
dat <- as.data.frame(d[,c(13,12,11,14,7,8,15,10)])
str(dat)
names(dat)
write.csv(dat,"2001.csv")

#

##########################################
#2002
##########################################
d <- read.csv("cpcb_dly_aq_delhi-2002.csv", header=T)
str(d)

d$mn <- ifelse(d$Sampling.Date=="January - M012002","Jan",
               ifelse(d$Sampling.Date=="February - M022002","Feb",
                      ifelse(d$Sampling.Date=="March - M032002","Mar",
                             ifelse(d$Sampling.Date=="April - M042002", "Apr",
                                    ifelse(d$Sampling.Date=="May - M052002","May",
                                           ifelse(d$Sampling.Date=="June - M062002","Jun",
                                                  ifelse(d$Sampling.Date=="July - M072002","Jul",
                                                         ifelse(d$Sampling.Date=="August - M082002","Aug",
                                                                ifelse(d$Sampling.Date=="September - M092002","Sep",
                                                                       ifelse(d$Sampling.Date=="October - M102002","Oct",
                                                                              ifelse(d$Sampling.Date=="November - M112002","Nov",
                                                                                     ifelse(d$Sampling.Date=="December - M122002","Dec",
                                                                                            ifelse(d$Sampling.Date=="Annual - M132002","Annual" ,"Undefined")))))))))))))




d$mn <- factor(d$mn)

d$yr <- "2002"
d$yr <- factor(d$yr)

d$st <- d$Location.of.Monitoring.Station
d$st <- ifelse(d$Stn.Code==56,	"Ashok Vihar",
               ifelse(d$Stn.Code==59,	"Janakpuri",
                      ifelse(d$Stn.Code==55,	"Nizamuddin",
                             ifelse(d$Stn.Code==58,	"Shahadra",
                                    ifelse(d$Stn.Code==57,	"Shahzada Bagh",
                                           ifelse(d$Stn.Code==60,	"Siri Fort",
                                                  ifelse(d$Stn.Code==345,	"Mayapuri",
                                                         ifelse(d$Stn.Code==144,	"N.Y. School",
                                                                ifelse(d$Stn.Code==146,	"Town Hall",
                                                                       ifelse(d$Stn.Code==531,	"Pritampura","undefined"
                                                                       ))))))))))

d$st <- factor(d$st)

d$typ <- ifelse(d$Type.of.Location=="Industrial Area", "ind",
                ifelse(d$Type.of.Location=="Residential, Rural and other Areas", "rsd","undefined"))
d$typ <- as.factor(d$typ)
d$PM10 <- d$RSPM.PM10
str(d)
names(d)
dat <- as.data.frame(d[,c(11:14,7,8,15,10)])
str(dat)
names(dat)
write.csv(dat,"2002.csv")


##########################################
#2003
##########################################
d <- read.csv("cpcb_dly_aq_delhi-2003.csv", header=T)
str(d)

d$mn <- pmax(
  as.POSIXct(d$Sampling.Date, format="%d/%m/%Y"),
  as.POSIXct(d$Sampling.Date, format="%d-%m-%y"),
  na.rm=TRUE
)

d$mn <- strftime(d$mn, "%m")
d$mn <- factor(d$mn)
d$mn <- ifelse(d$mn=="01","Jan",
               ifelse(d$mn=="02","Feb",
                      ifelse(d$mn=="03","Mar",
                             ifelse(d$mn=="04", "Apr",
                                    ifelse(d$mn=="05","May",
                                           ifelse(d$mn=="06","Jun",
                                                  ifelse(d$mn=="07","Jul",
                                                         ifelse(d$mn=="08","Aug",
                                                                ifelse(d$mn=="09","Sep",
                                                                       ifelse(d$mn=="10","Oct",
                                                                              ifelse(d$mn=="11","Nov",
                                                                                     ifelse(d$mn=="12","Dec",
                                                                                            ifelse(d$mn=="13","Annual" ,"Undefined")))))))))))))




d$mn <- factor(d$mn)

d$yr <- "2003"
d$yr <- factor(d$yr)

d$st <- ifelse(d$Stn.Code==56,	"Ashok Vihar",
               ifelse(d$Stn.Code==59,	"Janakpuri",
                      ifelse(d$Stn.Code==55,	"Nizamuddin",
                             ifelse(d$Stn.Code==58,	"Shahadra",
                                    ifelse(d$Stn.Code==57,	"Shahzada Bagh",
                                           ifelse(d$Stn.Code==60,	"Siri Fort",
                                                  ifelse(d$Stn.Code==345,	"Mayapuri",
                                                         ifelse(d$Stn.Code==145,	"Mayapuri",
                                                                ifelse(d$Stn.Code==144,	"N.Y. School",
                                                                ifelse(d$Stn.Code==146,	"Town Hall",
                                                                       ifelse(d$Stn.Code==531,	"Pritampura","undefined"
                                                                       )))))))))))

d$st <- factor(d$st)

d$typ <- ifelse(d$Type.of.Location=="Industrial Area", "ind",
                ifelse(d$Type.of.Location=="Residential, Rural and other Areas", "rsd","undefined"))
d$typ <- as.factor(d$typ)
d$PM10 <- d$RSPM.PM10
str(d)
names(d)
dat <- as.data.frame(d[,c(11:14,7,8,15,10)])
str(dat)
names(dat)
write.csv(dat,"2003.csv")

##########################################
#2004
##########################################
d <- read.csv("cpcb_dly_aq_delhi-2004.csv", header=T)
str(d)

d$mn <- pmax(
  as.POSIXct(d$Sampling.Date, format="%d/%m/%Y"),
  as.POSIXct(d$Sampling.Date, format="%d-%m-%y"),
  na.rm=TRUE
)

d$mn <- strftime(d$mn, "%m")
d$mn <- factor(d$mn)
d$mn <- ifelse(d$mn=="01","Jan",
               ifelse(d$mn=="02","Feb",
                      ifelse(d$mn=="03","Mar",
                             ifelse(d$mn=="04", "Apr",
                                    ifelse(d$mn=="05","May",
                                           ifelse(d$mn=="06","Jun",
                                                  ifelse(d$mn=="07","Jul",
                                                         ifelse(d$mn=="08","Aug",
                                                                ifelse(d$mn=="09","Sep",
                                                                       ifelse(d$mn=="10","Oct",
                                                                              ifelse(d$mn=="11","Nov",
                                                                                     ifelse(d$mn=="12","Dec",
                                                                                            ifelse(d$mn=="13","Annual" ,"Undefined")))))))))))))




d$mn <- factor(d$mn)

d$yr <- "2004"
d$yr <- factor(d$yr)

d$st <- ifelse(d$Stn.Code==56,	"Ashok Vihar",
               ifelse(d$Stn.Code==59,	"Janakpuri",
                      ifelse(d$Stn.Code==55,	"Nizamuddin",
                             ifelse(d$Stn.Code==58,	"Shahadra",
                                    ifelse(d$Stn.Code==57,	"Shahzada Bagh",
                                           ifelse(d$Stn.Code==60,	"Siri Fort",
                                                  ifelse(d$Stn.Code==345,	"Mayapuri",
                                                         ifelse(d$Stn.Code==144,	"N.Y. School",
                                                                ifelse(d$Stn.Code==146,	"Town Hall",
                                                                       ifelse(d$Stn.Code==531,	"Pritampura","undefined"
                                                                       ))))))))))

d$st <- factor(d$st)

d$typ <- ifelse(d$Type.of.Location=="Industrial Area", "ind",
                ifelse(d$Type.of.Location=="Residential, Rural and other Areas", "rsd","undefined"))
d$typ <- as.factor(d$typ)
d$PM10 <- d$RSPM.PM10
str(d)
names(d)
dat <- as.data.frame(d[,c(12:15,8,9,16,11)])
str(dat)
write.csv(dat,"2004.csv")

##########################################
#2005
##########################################
d <- read.csv("cpcb_dly_aq_delhi-2005.csv", header=T)
str(d)

d$mn <- pmax(
  as.POSIXct(d$Sampling.Date, format="%d/%m/%Y"),
  as.POSIXct(d$Sampling.Date, format="%d-%m-%y"),
  na.rm=TRUE
)

d$mn <- strftime(d$mn, "%m")
d$mn <- factor(d$mn)
d$mn <- ifelse(d$mn=="01","Jan",
               ifelse(d$mn=="02","Feb",
                      ifelse(d$mn=="03","Mar",
                             ifelse(d$mn=="04", "Apr",
                                    ifelse(d$mn=="05","May",
                                           ifelse(d$mn=="06","Jun",
                                                  ifelse(d$mn=="07","Jul",
                                                         ifelse(d$mn=="08","Aug",
                                                                ifelse(d$mn=="09","Sep",
                                                                       ifelse(d$mn=="10","Oct",
                                                                              ifelse(d$mn=="11","Nov",
                                                                                     ifelse(d$mn=="12","Dec",
                                                                                            ifelse(d$mn=="13","Annual" ,"Undefined")))))))))))))




d$mn <- factor(d$mn)

d$yr <- "2005"
d$yr <- factor(d$yr)

d$st <- d$Location.of.Monitoring.Station
unique(d$st)

d$typ <- ifelse(d$Type.of.Location=="Industrial Areas", "ind",
                ifelse(d$Type.of.Location=="Residential and others", "rsd","undefined"))
d$typ <- as.factor(d$typ)
d$PM10 <- d$RSPM.PM10
str(d)
names(d)
dat <- as.data.frame(d[,c(10:13,6,7,14,9)])
str(dat)
write.csv(dat,"2005.csv")

##########################################
#2006
##########################################
d <- read.csv("cpcb_dly_aq_delhi-2006.csv", header=T)
str(d)

d$mn <- pmax(
  as.POSIXct(d$Sampling.Date, format="%d/%m/%Y"),
  as.POSIXct(d$Sampling.Date, format="%d-%m-%y"),
  na.rm=TRUE
)

d$mn <- strftime(d$mn, "%m")
d$mn <- factor(d$mn)
d$mn <- ifelse(d$mn=="01","Jan",
               ifelse(d$mn=="02","Feb",
                      ifelse(d$mn=="03","Mar",
                             ifelse(d$mn=="04", "Apr",
                                    ifelse(d$mn=="05","May",
                                           ifelse(d$mn=="06","Jun",
                                                  ifelse(d$mn=="07","Jul",
                                                         ifelse(d$mn=="08","Aug",
                                                                ifelse(d$mn=="09","Sep",
                                                                       ifelse(d$mn=="10","Oct",
                                                                              ifelse(d$mn=="11","Nov",
                                                                                     ifelse(d$mn=="12","Dec",
                                                                                            ifelse(d$mn=="13","Annual" ,"Undefined")))))))))))))




d$mn <- factor(d$mn)

d$yr <- "2006"
d$yr <- factor(d$yr)

d$st <- d$Location.of.Monitoring.Station

d$typ <- ifelse(d$Type.of.Location=="Industrial Areas", "ind",
                ifelse(d$Type.of.Location=="Residential and others", "rsd","undefined"))
d$typ <- as.factor(d$typ)
d$PM10 <- d$RSPM.PM10
str(d)
names(d)
dat <- as.data.frame(d[,c(10:13,6,7,14,9)])
str(dat)
write.csv(dat,"2006.csv")

##########################################
#2007
##########################################
d <- read.csv("cpcb_dly_aq_delhi-2007.csv", header=T)
str(d)

d$mn <- pmax(
  as.POSIXct(d$Sampling.Date, format="%d/%m/%Y"),
  as.POSIXct(d$Sampling.Date, format="%d-%m-%y"),
  na.rm=TRUE
)

d$mn <- strftime(d$mn, "%m")
d$mn <- factor(d$mn)
d$mn <- ifelse(d$mn=="01","Jan",
               ifelse(d$mn=="02","Feb",
                      ifelse(d$mn=="03","Mar",
                             ifelse(d$mn=="04", "Apr",
                                    ifelse(d$mn=="05","May",
                                           ifelse(d$mn=="06","Jun",
                                                  ifelse(d$mn=="07","Jul",
                                                         ifelse(d$mn=="08","Aug",
                                                                ifelse(d$mn=="09","Sep",
                                                                       ifelse(d$mn=="10","Oct",
                                                                              ifelse(d$mn=="11","Nov",
                                                                                     ifelse(d$mn=="12","Dec",
                                                                                            ifelse(d$mn=="13","Annual" ,"Undefined")))))))))))))




d$mn <- factor(d$mn)

d$yr <- "2007"
d$yr <- factor(d$yr)

d$st <- d$Location.of.Monitoring.Station

d$typ <- ifelse(d$Type.of.Location=="Industrial Areas", "ind",
                ifelse(d$Type.of.Location=="Residential and others", "rsd","undefined"))
d$typ <- as.factor(d$typ)
d$PM10 <- d$RSPM.PM10
str(d)
names(d)
dat <- as.data.frame(d[,c(10:13,6,7,14,9)])
str(dat)
write.csv(dat,"2007.csv")

##########################################
#2008
##########################################
d <- read.csv("cpcb_dly_aq_delhi-2008.csv", header=T)
str(d)

d$mn <- pmax(
  as.POSIXct(d$Sampling.Date, format="%d/%m/%Y"),
  as.POSIXct(d$Sampling.Date, format="%d-%m-%y"),
  na.rm=TRUE
)

d$mn <- strftime(d$mn, "%m")
d$mn <- factor(d$mn)
d$mn <- ifelse(d$mn=="01","Jan",
               ifelse(d$mn=="02","Feb",
                      ifelse(d$mn=="03","Mar",
                             ifelse(d$mn=="04", "Apr",
                                    ifelse(d$mn=="05","May",
                                           ifelse(d$mn=="06","Jun",
                                                  ifelse(d$mn=="07","Jul",
                                                         ifelse(d$mn=="08","Aug",
                                                                ifelse(d$mn=="09","Sep",
                                                                       ifelse(d$mn=="10","Oct",
                                                                              ifelse(d$mn=="11","Nov",
                                                                                     ifelse(d$mn=="12","Dec",
                                                                                            ifelse(d$mn=="13","Annual" ,"Undefined")))))))))))))




d$mn <- factor(d$mn)

d$yr <- "2008"
d$yr <- factor(d$yr)

d$st <- d$Location.of.Monitoring.Station

d$typ <- ifelse(d$Type.of.Location=="Industrial Areas", "ind",
                ifelse(d$Type.of.Location=="Residential and others", "rsd","undefined"))
d$typ <- as.factor(d$typ)
d$PM10 <- d$RSPM.PM10
str(d)
names(d)
dat <- as.data.frame(d[,c(10:13,6,7,14,9)])
str(dat)
write.csv(dat,"2008.csv")

##########################################
#2009
##########################################
d <- read.csv("cpcb_dly_aq_delhi-2009.csv", header=T)
str(d)

d$mn <- pmax(
  as.POSIXct(d$Sampling.Date, format="%d/%m/%Y"),
  as.POSIXct(d$Sampling.Date, format="%d-%m-%y"),
  na.rm=TRUE
)

d$mn <- strftime(d$mn, "%m")
d$mn <- factor(d$mn)
d$mn <- ifelse(d$mn=="01","Jan",
               ifelse(d$mn=="02","Feb",
                      ifelse(d$mn=="03","Mar",
                             ifelse(d$mn=="04", "Apr",
                                    ifelse(d$mn=="05","May",
                                           ifelse(d$mn=="06","Jun",
                                                  ifelse(d$mn=="07","Jul",
                                                         ifelse(d$mn=="08","Aug",
                                                                ifelse(d$mn=="09","Sep",
                                                                       ifelse(d$mn=="10","Oct",
                                                                              ifelse(d$mn=="11","Nov",
                                                                                     ifelse(d$mn=="12","Dec",
                                                                                            ifelse(d$mn=="13","Annual" ,"Undefined")))))))))))))




d$mn <- factor(d$mn)

d$yr <- "2009"
d$yr <- factor(d$yr)

d$st <- d$Location.of.Monitoring.Station

d$typ <- ifelse(d$Type.of.Location=="Industrial Areas", "ind",
                ifelse(d$Type.of.Location=="Residential and others", "rsd","undefined"))
d$typ <- as.factor(d$typ)
d$PM10 <- d$RSPM.PM10
str(d)
names(d)
dat <- as.data.frame(d[,c(10:13,6,7,14,9)])
str(dat)
write.csv(dat,"2009.csv")

##########################################
#2011
##########################################
d <- read.csv("cpcb_dly_aq_delhi-2011.csv", header=T)
str(d)
d$mn <- pmax(
  as.POSIXct(d$Sampling.Date, format="%d/%m/%Y"),
  as.POSIXct(d$Sampling.Date, format="%d-%m-%y"),
  na.rm=TRUE
)

d$mn <- strftime(d$mn, "%m")
d$mn <- factor(d$mn)
d$mn <- ifelse(d$mn=="01","Jan",
               ifelse(d$mn=="02","Feb",
                      ifelse(d$mn=="03","Mar",
                             ifelse(d$mn=="04", "Apr",
                                    ifelse(d$mn=="05","May",
                                           ifelse(d$mn=="06","Jun",
                                                  ifelse(d$mn=="07","Jul",
                                                         ifelse(d$mn=="08","Aug",
                                                                ifelse(d$mn=="09","Sep",
                                                                       ifelse(d$mn=="10","Oct",
                                                                              ifelse(d$mn=="11","Nov",
                                                                                     ifelse(d$mn=="12","Dec",
                                                                                            ifelse(d$mn=="13","Annual" ,"Undefined")))))))))))))




d$mn <- as.factor(d$mn)
d$yr <- "2011"
d$yr <- factor(d$yr)

d$st <- ifelse(d$Stn.Code==56,	"Ashok Vihar",
               ifelse(d$Stn.Code==59,	"Janakpuri",
                      ifelse(d$Stn.Code==55,	"Nizamuddin",
                             ifelse(d$Stn.Code==58,	"Shahadra",
                                    ifelse(d$Stn.Code==57,	"Shahzada Bagh",
                                           ifelse(d$Stn.Code==60,	"Siri Fort",
                                                  ifelse(d$Stn.Code==345,	"Mayapuri",
                                                         ifelse(d$Stn.Code==144,	"N.Y. School",
                                                                ifelse(d$Stn.Code==146,	"Town Hall",
                                                                       ifelse(d$Stn.Code==531,	"Pritampura","undefined"
                                                                       ))))))))))

d$st <- factor(d$st)

d$typ <- ifelse(d$Type.of.Location=="Industrial Area", "ind",
                ifelse(d$Type.of.Location=="Residential, Rural and other Areas", "rsd","undefined"))
d$typ <- as.factor(d$typ)
d$PM10 <- d$RSPM.PM10
str(d)
names(d)
dat <- as.data.frame(d[,c(12:15,8,9,16,11)])
str(dat)
write.csv(dat,"2011.csv")

##########################################
#2012
##########################################
d <- read.csv("cpcb_dly_aq_delhi-2012.csv", header=T)
str(d)
d$mn <- pmax(
  as.POSIXct(d$Sampling.Date, format="%d/%m/%Y"),
  as.POSIXct(d$Sampling.Date, format="%d-%m-%y"),
  na.rm=TRUE
)

d$mn <- strftime(d$mn, "%m")
d$mn <- factor(d$mn)
d$mn <- ifelse(d$mn=="01","Jan",
               ifelse(d$mn=="02","Feb",
                      ifelse(d$mn=="03","Mar",
                             ifelse(d$mn=="04", "Apr",
                                    ifelse(d$mn=="05","May",
                                           ifelse(d$mn=="06","Jun",
                                                  ifelse(d$mn=="07","Jul",
                                                         ifelse(d$mn=="08","Aug",
                                                                ifelse(d$mn=="09","Sep",
                                                                       ifelse(d$mn=="10","Oct",
                                                                              ifelse(d$mn=="11","Nov",
                                                                                     ifelse(d$mn=="12","Dec",
                                                                                            ifelse(d$mn=="13","Annual" ,"Undefined")))))))))))))




d$mn <- as.factor(d$mn)
d$yr <- "2012"
d$yr <- factor(d$yr)

d$st <- ifelse(d$Stn.Code==56,	"Ashok Vihar",
               ifelse(d$Stn.Code==59,	"Janakpuri",
                      ifelse(d$Stn.Code==55,	"Nizamuddin",
                             ifelse(d$Stn.Code==58,	"Shahadra",
                                    ifelse(d$Stn.Code==57,	"Shahzada Bagh",
                                           ifelse(d$Stn.Code==60,	"Siri Fort",
                                                  ifelse(d$Stn.Code==345,	"Mayapuri",
                                                         ifelse(d$Stn.Code==144,	"N.Y. School",
                                                                ifelse(d$Stn.Code==146,	"Town Hall",
                                                                       ifelse(d$Stn.Code==531,	"Pritampura","undefined"
                                                                       ))))))))))

d$st <- factor(d$st)

d$typ <- ifelse(d$Type.of.Location=="Industrial Area", "ind",
                ifelse(d$Type.of.Location=="Residential, Rural and other Areas", "rsd","undefined"))
d$typ <- as.factor(d$typ)
d$PM10 <- d$RSPM.PM10
str(d)
names(d)
dat <- as.data.frame(d[,c(12:15,8,9,16,11)])
str(dat)
write.csv(dat,"2012.csv")

##########################################
#2013
##########################################
d <- read.csv("cpcb_dly_aq_delhi-2013.csv", header=T)
str(d)
d$mn <- pmax(
  as.POSIXct(d$Sampling.Date, format="%d/%m/%Y"),
  as.POSIXct(d$Sampling.Date, format="%d-%m-%y"),
  na.rm=TRUE
)

d$mn <- strftime(d$mn, "%m")
d$mn <- factor(d$mn)
d$mn <- ifelse(d$mn=="01","Jan",
               ifelse(d$mn=="02","Feb",
                      ifelse(d$mn=="03","Mar",
                             ifelse(d$mn=="04", "Apr",
                                    ifelse(d$mn=="05","May",
                                           ifelse(d$mn=="06","Jun",
                                                  ifelse(d$mn=="07","Jul",
                                                         ifelse(d$mn=="08","Aug",
                                                                ifelse(d$mn=="09","Sep",
                                                                       ifelse(d$mn=="10","Oct",
                                                                              ifelse(d$mn=="11","Nov",
                                                                                     ifelse(d$mn=="12","Dec",
                                                                                            ifelse(d$mn=="13","Annual" ,"Undefined")))))))))))))




d$mn <- as.factor(d$mn)
d$yr <- "2013"
d$yr <- factor(d$yr)

d$st <- ifelse(d$Stn.Code==56,	"Ashok Vihar",
               ifelse(d$Stn.Code==59,	"Janakpuri",
                      ifelse(d$Stn.Code==55,	"Nizamuddin",
                             ifelse(d$Stn.Code==58,	"Shahadra",
                                    ifelse(d$Stn.Code==57,	"Shahzada Bagh",
                                           ifelse(d$Stn.Code==60,	"Siri Fort",
                                                  ifelse(d$Stn.Code==345,	"Mayapuri",
                                                         ifelse(d$Stn.Code==144,	"N.Y. School",
                                                                ifelse(d$Stn.Code==146,	"Town Hall",
                                                                       ifelse(d$Stn.Code==531,	"Pritampura","undefined"
                                                                       ))))))))))

d$st <- factor(d$st)

d$typ <- ifelse(d$Type.of.Location=="Industrial Area", "ind",
                ifelse(d$Type.of.Location=="Residential, Rural and other Areas", "rsd","undefined"))
d$typ <- as.factor(d$typ)
d$PM10 <- d$RSPM.PM10
str(d)
names(d)
dat <- as.data.frame(d[,c(12:15,8,9,16,11)])
str(dat)
write.csv(dat,"2013.csv")

##########################################
#2014
##########################################
d <- read.csv("cpcb_dly_aq_delhi-2014.csv", header=T)
str(d)
d$mn <- pmax(
  as.POSIXct(d$Sampling.Date, format="%d/%m/%Y"),
  as.POSIXct(d$Sampling.Date, format="%d-%m-%y"),
  na.rm=TRUE
)

d$mn <- strftime(d$mn, "%m")
d$mn <- factor(d$mn)
d$mn <- ifelse(d$mn=="01","Jan",
               ifelse(d$mn=="02","Feb",
                      ifelse(d$mn=="03","Mar",
                             ifelse(d$mn=="04", "Apr",
                                    ifelse(d$mn=="05","May",
                                           ifelse(d$mn=="06","Jun",
                                                  ifelse(d$mn=="07","Jul",
                                                         ifelse(d$mn=="08","Aug",
                                                                ifelse(d$mn=="09","Sep",
                                                                       ifelse(d$mn=="10","Oct",
                                                                              ifelse(d$mn=="11","Nov",
                                                                                     ifelse(d$mn=="12","Dec",
                                                                                            ifelse(d$mn=="13","Annual" ,"Undefined")))))))))))))




d$mn <- as.factor(d$mn)
d$yr <- "2014"
d$yr <- factor(d$yr)

d$st <- ifelse(d$Stn.Code==56,	"Ashok Vihar",
               ifelse(d$Stn.Code==59,	"Janakpuri",
                      ifelse(d$Stn.Code==55,	"Nizamuddin",
                             ifelse(d$Stn.Code==58,	"Shahadra",
                                    ifelse(d$Stn.Code==57,	"Shahzada Bagh",
                                           ifelse(d$Stn.Code==60,	"Siri Fort",
                                                  ifelse(d$Stn.Code==345,	"Mayapuri",
                                                         ifelse(d$Stn.Code==144,	"N.Y. School",
                                                                ifelse(d$Stn.Code==146,	"Town Hall",
                                                                       ifelse(d$Stn.Code==531,	"Pritampura","undefined"
                                                                       ))))))))))

d$st <- factor(d$st)

d$typ <- ifelse(d$Type.of.Location=="Industrial Area", "ind",
                ifelse(d$Type.of.Location=="Residential, Rural and other Areas", "rsd","undefined"))
d$typ <- as.factor(d$typ)
d$PM10 <- d$RSPM.PM10
d$SPM <- d$PM.2.5
str(d)
names(d)
dat <- as.data.frame(d[,c(12:15,8,9,16,17)])
str(dat)
write.csv(dat,"2014.csv")

##########################################
#2015
##########################################
d <- read.csv("cpcb_dly_aq_delhi-2015.csv", header=T)
str(d)
d$mn <- pmax(
  as.POSIXct(d$Sampling.Date, format="%d/%m/%Y"),
  as.POSIXct(d$Sampling.Date, format="%d-%m-%y"),
  na.rm=TRUE
)

d$mn <- strftime(d$mn, "%m")
d$mn <- factor(d$mn)
d$mn <- ifelse(d$mn=="01","Jan",
               ifelse(d$mn=="02","Feb",
                      ifelse(d$mn=="03","Mar",
                             ifelse(d$mn=="04", "Apr",
                                    ifelse(d$mn=="05","May",
                                           ifelse(d$mn=="06","Jun",
                                                  ifelse(d$mn=="07","Jul",
                                                         ifelse(d$mn=="08","Aug",
                                                                ifelse(d$mn=="09","Sep",
                                                                       ifelse(d$mn=="10","Oct",
                                                                              ifelse(d$mn=="11","Nov",
                                                                                     ifelse(d$mn=="12","Dec",
                                                                                            ifelse(d$mn=="13","Annual" ,"Undefined")))))))))))))




d$mn <- as.factor(d$mn)
d$yr <- "2015"
d$yr <- factor(d$yr)

d$st <- ifelse(d$Stn.Code==56,	"Ashok Vihar",
               ifelse(d$Stn.Code==59,	"Janakpuri",
                      ifelse(d$Stn.Code==55,	"Nizamuddin",
                             ifelse(d$Stn.Code==58,	"Shahadra",
                                    ifelse(d$Stn.Code==57,	"Shahzada Bagh",
                                           ifelse(d$Stn.Code==60,	"Siri Fort",
                                                  ifelse(d$Stn.Code==345,	"Mayapuri",
                                                         ifelse(d$Stn.Code==144,	"N.Y. School",
                                                                ifelse(d$Stn.Code==146,	"Town Hall",
                                                                       ifelse(d$Stn.Code==531,	"Pritampura","undefined"
                                                                       ))))))))))

d$st <- factor(d$st)

d$typ <- ifelse(d$Type.of.Location=="Industrial Area", "ind",
                ifelse(d$Type.of.Location=="Residential, Rural and other Areas", "rsd","undefined"))
d$typ <- as.factor(d$typ)
d$PM10 <- d$RSPM.PM10
d$SPM <- d$PM.2.5
str(d)
names(d)
dat <- as.data.frame(d[,c(12:15,8,9,16,17)])
str(dat)
write.csv(dat,"2015.csv")



#______________________________________________________ Pause 1

#######################################
# COMPILING DATA
#######################################
y00 <- read.csv("2000.csv", header=T)
y01 <- read.csv("2001.csv", header=T)
y02 <- read.csv("2002.csv", header=T)
y03 <- read.csv("2003.csv", header=T)
y04 <- read.csv("2004.csv", header=T)
y05 <- read.csv("2005.csv", header=T)
y06 <- read.csv("2006.csv", header=T)
y07 <- read.csv("2007.csv", header=T)
y08 <- read.csv("2008.csv", header=T)
y09 <- read.csv("2009.csv", header=T)
#y10 <- read.csv("2010.csv", header=T)
y11 <- read.csv("2011.csv", header=T)
y12 <- read.csv("2012.csv", header=T)
y13 <- read.csv("2013.csv", header=T)
y14 <- read.csv("2014.csv", header=T)
y15 <- read.csv("2015.csv", header=T)

df <- rbind(y00,y01,y02,y03,y04,y05,y06,y07,y08,y09,y11,y12,y13,y14,y15)
str(df)
summary(df)
df$yr <- as.factor(df$yr)

#data management
unique(df$st)
subset(df,st=="Mayapuri Indl. Area" )
View(subset(df,st=="Shahdara" ))
subset(df,st=="undefined" )
# Rename by name: change "Mayapuri Indl. Area","Mayapuri"
levels(df$st)[levels(df$st)=="Mayapuri Indl. Area"] <- "Mayapuri"
levels(df$st)[levels(df$st)=="Shahadra"] <- "Shahdara"
unique(df$st)

str(df)
summary(df)

subset(df,is.na(df$mn))
levels(df$mn)
## To reorder the levels:
df$mn = factor(df$mn,levels(df$mn)[c(5,4,8,1,9,7,6,2,12,11,10,3,13)])
levels(df$typ) <- list("Industrial"="ind","Residential"="rsd")

#write.csv(df, "fulldata.csv")



#______________________________________________________ Pause 2

setwd("D:/project/nagato/data and command/2000-2015/delhi")
#setwd("C:/Users/USER/Downloads")
df <- read.csv("fulldata.csv", header=T)
df <- df[,c(3:10)]
str(df)
summary(df)
df$yr <- as.factor(df$yr)
################
# DESCRIPTIVE ANALYSIS
################
tab <- table(df$typ,df$st,df$mn)

#library(epiDisplay)
#tableStack(vars=c("mn", "yr"), dataFrame=df)

#month
df$mn <- as.factor(df$mn)
levels(df$mn)
## To reorder the levels:
df$mn<- factor(df$mn,levels=levels(df$mn)[c(6,5,9,2,10,8,7,3,13,12,11,4,1)])

windows(30,18)
#par(mfrow=c(2,2))
barplot(table(df$mn), ylim=c(0,800),xlab = "",  main="MONTH",ylab="", cex.axis =1.4 , cex.names =1.4 , las=1, family="serif")

#year
windows(30,18)
barplot(table(df$yr), ylim=c(0,800), xlab= "",  main="YEAR", ylab="", cex.axis =1.4 , cex.names =1.4 , las=1, family="serif")

#monitoring station
windows(30,18)
b <- barplot(table(df$st), ylim=c(0,2000),xlab= "",  main="STATION", ylab="",  cex.axis =1.4 , cex.names =1.4 , las=1, family="serif", xaxt="n")
lab1 <- c("1","2","3","4","5","6","7","8","9","10","11","12")
axis(side=1,at=b,lab=lab1,padj=0,cex.axis=1.4,tcl=0,family="serif")
lg <- c("1: Ashok Vihar",   "2: BSZ Marg","3: DCE","4: Janakpuri","5: Mayapuri","6: N.Y. School","7: Nizamuddin","8: Pritampura","9: Shahdara","10: Shahzada Bagh", "11: Siri Fort", "12: Town Hall")
#eTtl <- "Meters above sea level"
legend("topleft",leg=lg,inset=c(0.01,0.01),bg="ivory",cex=1.4, x.intersp=0.2,y.intersp=0.9,ncol=2)

#type of location
windows(30,18)
barplot(table(df$typ), ylim=c(0,5000),xlab= "",  main="LOCATION",ylab="",  cex.axis =1.4 , cex.names =1.4 , las=1, family="serif")

#______________________________________________________ Pause 3

#plot SO2
windows(30,18)
plot(df$mn,df$SO2, type="p", xlab="MONTH",ylab="Sulfur dioxide", cex.lab =1.4 ,cex.axis =1.4 , cex.names =1.4 ,las=1, family="serif")
windows(35,18)
plot(df$yr,df$SO2, type="l", xlab="YEAR", last=1,ylab="Sulfur dioxide", cex.lab =1.4 ,cex.axis =1.4 , cex.names =1.4 ,las=1, family="serif")
windows(30,18)
b <- plot(df$st,df$SO2, type="b", xlab="STATION", last=1,ylab="Sulfur dioxide", cex.lab =1.4 ,cex.axis =1.4 , cex.names =1.4 ,las=1, family="serif", xaxt="n")
lab1 <- c("1","2","3","4","5","6","7","8","9","10","11","12")
axis(side=1,at=c(1:12),lab=lab1,padj=0,cex.axis=1.4,tcl=0,family="serif")
lg <- c("1: Ashok Vihar",   "2: BSZ Marg","3: DCE","4: Janakpuri","5: Mayapuri","6: N.Y. School","7: Nizamuddin","8: Pritampura","9: Shahdara","10: Shahzada Bagh", "11: Siri Fort", "12: Town Hall")
legend("topleft",leg=lg,inset=c(0.01,0.01),bg="ivory",cex=1.2, x.intersp=0.2,y.intersp=0.9,ncol=2)
windows(18,18)
plot(df$typ,df$SO2, type="c", xlab="LOCATION", las=1,ylab="Sulfur dioxide", cex.lab =1.4 ,cex.axis =1.4 , cex.names =1.4 ,las=1, family="serif")

###################
# linear model
###################
names(df)

#SO2
#mod <- lm(data=df,SO2~mn+yr+st+typ+NO2+PM10+SPM)
#2004 dan 2010 data tidak tersedia, sehingga total ada 14 tahun
#options(contrasts=c("contr.sum","contr.poly"))	# sum contrasts
mod <- lm(data=df,SO2~mn+yr+st+typ)
summary(mod) -> rez
rez
windows()
par(mfrow=c(2,2))
plot(mod)

windows(4,4)
par(mar=c(2.5,0.5,0,0.6),mgp=c(1.1,0.2,0),oma=c(0,1.8,2.5,0),las=1,tcl=-0.2)
qqnorm(resid(mod),las=1,pch=20,cex=1.2,main="")
qqline(resid(mod),col=2)
mtext(side=3,"Sample Quantiles",adj=-0.14,line=0.1)
mtext(side=3,"Log-Linear Model",adj=1,line=0.1)

drop1(mod,test="F")
rez1 <- drop1(mod,test="F")
pval <- rez1$"Pr(>F)"[2:3]
pval1 <- ifelse(pval[1]<0.0001,"<0.0001",round(pval[1],3))
pval2 <- ifelse(pval[2]<0.0001,"<0.0001",round(pval[2],3))
text(-3,0.2,paste("p-value: ",pval1,sep=""),adj=0)
mtext(side=3,adj=1,line=1.4,"Sulfur Dioxide")
box()

# plot results
source("dcis.Rcm")
names(df)
d <- na.omit(df[,c(1:3,5,7)])
d$yr <- droplevels(d$yr, exclude = "2004")
names(d)
lm.dci(d,yID=4,xIDs=c(1:3,5)) -> rez
xlab1 <- "Month"
xlab2 <- "Year"
xlab3 <- "Station"
xlab4 <- "Area"
ylab <- expression(paste("Sulfur Dioxide",sep="")) 

windows(25,10)				# Figure 5

par(oma=c(0,0,0,0),mar=c(2.5,2,2.5,1),las=1,mgp=c(1.1,0.1,0),tcl=0.1)

n1 <- length(unique(d[,1]))
n2 <- length(unique(d[,2]))
n3 <- length(unique(d[,3]))
n4 <- length(unique(d[,5]))

xCoord <- c(1:n1,(n1+2):(n1+n2+1),(n1+n2+3):(n1+n2+n3+2), (n1+n2+n3+4):(n1+n2+n3+n4+3))
mAdj1 <- rez[c(1:n1),3]
mAdj2 <- rez[(n1+1):(n1+n2),3]
mAdj3 <- rez[(n1+n2+1):(n1+n2+n3),3]
mAdj4 <- rez[(n1+n2+n3+1):(n1+n2+n3+n4),3]

meanTrue <- mean(d$SO2)
yCoord <- c(mAdj1,mAdj2,mAdj3,mAdj4)

cilb1 <- rez[1:n1,4]
cilb2 <- rez[(n1+1):(n1+n2),4]
cilb3 <- rez[(n1+n2+1):(n1+n2+n3),4]
cilb4 <- rez[(n1+n2+n3+1):(n1+n2+n3+n4),4]
ciub1 <- rez[1:n1,5]
ciub2 <- rez[(n1+1):(n1+n2),5]
ciub3 <- rez[(n1+n2+1):(n1+n2+n3),5]
ciub4 <- rez[(n1+n2+n3+1):(n1+n2+n3+n4),5]

mCr1 <- tapply(d$SO2,d$mn,mean)	# crude means
mCr2 <- tapply(d$SO2,d$yr,mean)
mCr3 <- tapply(d$SO2,d$st,mean)
mCr4 <- tapply(d$SO2,d$typ,mean)

ymin <- min(cilb1,cilb2,cilb3,cilb4,mCr1,mCr2,mCr3,mCr4)-0.05
ymax <- max(ciub1,ciub2,ciub3,ciub4,mCr1,mCr2,mCr3,mCr4)+0.15

plot(1,type="n",xlim=c(1,max(xCoord)+0.001),ylim=c(ymin,ymax),ylab="",xlab="",xaxt="n",cex.axis=1)
abline(v=c(1:max(xCoord)),col="grey")
abline(h=meanTrue,col=2)
abline(v=c(n1+1,n1+n2+2,n1+n2+n3+3),col="dimgrey")
dx <- 0.1
points(xCoord[1:n1]+dx,yCoord[1:n1],type="l")

for (i in c(1:n1)) {
  points(xCoord[i]+c(0,0)+dx,c(cilb1[i],ciub1[i]),type="l",lwd=1)
}
for (i in c(1:n2)) {
  points(xCoord[n1+i]+c(0,0)+dx,c(cilb2[i],ciub2[i]),type="l",lwd=1)
}
for (i in c(1:n3)) {
  points(xCoord[n1+n2+i]+c(0,0)+dx,c(cilb3[i],ciub3[i]),type="l",lwd=1)
}
for (i in c(1:n4)) {
  points(xCoord[n1+n2+n3+i]+c(0,0)+dx,c(cilb4[i],ciub4[i]),type="l",lwd=1)
}
points(xCoord+dx+0.03,yCoord+0.005,pch="_")

points(xCoord[1:n1]-dx,mCr1,pch=21,bg=3,cex=0.9)
points(xCoord[(n1+1):(n1+n2)]-dx,mCr2,pch=21,bg=3,cex=0.9)
points(xCoord[(n1+n2+1):(n1+n2+n3)]-dx,mCr3,pch=21,bg=3,cex=0.9)
points(xCoord[(n1+n2+n3+1):(n1+n2+n3+n4)]-dx,mCr4,pch=21,bg=3,cex=0.9)

lb1 <- c("j","f","m","a","m","j","j","a","s","o","n","d","A")
lb2 <- c("00","01","02","03","05","06","07","08","09","11","12","13","14","15")
lb3 <- c("1","2","3","4","5","6","7","8","9","10","11","12")
lb4 <- c("I","R")
dy <- (-0.3)
axis(side=1,at=c(1:n1),lab=lb1,padj=dy,cex.axis=1)
axis(side=1,at=n1+1+c(1:n2),lab=lb2,padj=dy,cex.axis=1)
axis(side=1,at=n1+n2+2+c(1:n3),lab=lb3,padj=dy,cex.axis=1)
axis(side=1,at=n1+n2+n3+3+c(1:n4),lab=lb4,padj=dy,cex.axis=1)

at1 <- (1+n1)/2
axis(side=1,at=at1,lab=xlab1,tcl=0,padj=1.4,cex.axis=1)
at2 <- (n1+1+n1+n2+2)/2
axis(side=1,at=at2,lab=xlab2,tcl=0,padj=1.4,cex.axis=1)
at3 <- (n1+n2+2+n1+n2+n3+3)/2
axis(side=1,at=at3,lab=xlab3,tcl=0,padj=1.4,cex.axis=1)
at4 <- (n1+n2+n3+3+n1+n2+n3+n4+4)/2
axis(side=1,at=at4,lab=xlab4,tcl=0,padj=1.4,cex.axis=1)

mtext(side=3,line=0,adj=0.01,ylab,cex=1)
lab="Linear Model"
mtext(side=3,line=0.2,adj=1,lab,cex=1)

lg1 <- c("j:  January","f:  February","m: March","a:  April","m: May","j:  June","j:  July","a:  August","s:  September","o:  October","n:  November","d:  Desember", "A:  Annual")
legend("topleft",leg=lg1,inset=c(0.05,0.025),bg="ivory",cex=0.9,title="Month",x.intersp=0.2,y.intersp=0.9,ncol=3)

lg2 <- c("00: 2000","01: 2001","02: 2002","03: 2003","05: 2005","06: 2006","07: 2007" ,"08: 2008","09: 2009","11: 2011","12: 2012","13: 2013","14: 2014","15: 2015")
legend("topleft",leg=lg2,inset=c(0.4,0.025),bg="ivory",cex=0.9,title="Year",x.intersp=0.2,y.intersp=0.9,ncol=3)

lg3 <- c("1: Ashok Vihar", "2: BSZ Marg", "3: DCE",          
"4: Janakpuri", "5: Mayapuri","6: N.Y. School",
"7: Nizamuddin", "8: Pritampura", "9: Shahdara" ,    
"10: Shahzada Bagh","11: Siri Fort", "12: Town Hall")
legend("topright",leg=lg3,inset=c(0.115,0.025),bg="ivory",cex=0.9,title="Station",x.intersp=0.2,y.intersp=0.9,ncol=2)

lg4 <- c("I: Industrial","R: Residential")
legend("bottomright",leg=lg4,inset=c(0.01,0.025),bg="ivory",cex=0.9,title="Year",x.intersp=0.2,y.intersp=0.9,ncol=1)
#____________________________________________________

#plot NO2
library("dplyr")
windows(30,18)
plot(df$mn,df$NO2, type="l", xlab="month",ylab="Nitrogen dioxide", cex.lab =1.4 ,cex.axis =1.4 , cex.names =1.4 ,las=1, family="serif", col="pink")
d <- subset(df, !(df$mn=="Annual"))
dat <- d %>%
  group_by(mn) %>%
  summarise(mean = mean(NO2, na.rm=T), n = n(), sd=sd(NO2, na.rm=T), se=sqrt(sd)/n,
            lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
            upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)
windows(10,7)
par(mar = c(5, 6, 4, 5) + 0.1)
plotTop <- max(dat$mean) + 20
dat$clr <- ifelse(dat$mean<53,"darkolivegreen3",ifelse(dat$mean<100,"yellow",ifelse(dat$mean<150,"orange",
                                                                                      ifelse(dat$mean<200,"firebrick1",ifelse(dat$mean<300,"firebrick","grey")))))
barCenters <- barplot(height = dat$mean,
                      names.arg = dat$mn,
                      beside = true, las = 2,
                      ylim = c(0, plotTop),
                      cex.names = 0.75, xaxt = "n",
                      main = "NO2 by month",col=dat$clr,
                      ylab = "microgram per cubic meter",
                      border = "black", axes = TRUE,yaxt="n")
axis(side=2, at=seq(0, plotTop, by=5), las=1)
grid()
# Specify the groupings. We use srt = 45 for a
# 45 degree string rotation
text(x = barCenters, y = par("usr")[3] - 1, srt = 45,
     adj = 1, labels = dat$mn, xpd = TRUE)
segments(barCenters, dat$mean - dat$se * 2, barCenters,
         dat$mean + dat$se * 2, lwd = 1.5)
arrows(barCenters, dat$mean - dat$se * 2, barCenters,
       dat$mean + dat$se * 2, lwd = 1.5, angle = 90,
       code = 3, length = 0.05)
labels <- paste(round(dat$mean, digit=2))
text(barCenters, dat$mean+1, labels, cex=1, pos=3) 
abline(h=53, col="red")

windows(30,18)
plot(df$yr,df$NO2, type="l", xlab="year", ylab="Nitrogen dioxide", cex.lab =1.4 ,cex.axis =1.4 , cex.names =1.4 ,las=1, family="serif")
dat <- df %>%
  group_by(yr) %>%
  summarise(mean = mean(NO2, na.rm=T), n = n(), sd=sd(NO2, na.rm=T), se=sqrt(sd)/n,
            lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
            upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)
windows(10,7)
par(mar = c(5, 6, 4, 5) + 0.1)
plotTop <- max(dat$mean, na.rm=T) + 20
dat$clr <- ifelse(dat$mean<53,"darkolivegreen3",ifelse(dat$mean<100,"yellow",ifelse(dat$mean<150,"orange",
                                                                                    ifelse(dat$mean<200,"firebrick1",ifelse(dat$mean<300,"firebrick","grey")))))
barCenters <- barplot(height = dat$mean,
                      names.arg = dat$yr,
                      beside = true, las = 2,
                      ylim = c(0, plotTop),
                      cex.names = 0.75, xaxt = "n",
                      main = "NO2 by year",
                      ylab = "microgram per cubic meter",
                      border = "black", axes = TRUE, col=dat$clr,yaxt="n")
axis(side=2, at=seq(0, plotTop, by=5), las=1)
grid()
# Specify the groupings. We use srt = 45 for a
# 45 degree string rotation
text(x = barCenters, y = par("usr")[3] - 1, srt = 45,
     adj = 1, labels = dat$yr, xpd = TRUE)
segments(barCenters, dat$mean - dat$se * 2, barCenters,
         dat$mean + dat$se * 2, lwd = 1.5)
arrows(barCenters, dat$mean - dat$se * 2, barCenters,
       dat$mean + dat$se * 2, lwd = 1.5, angle = 90,
       code = 3, length = 0.05)
labels <- paste(round(dat$mean, digit=2))
text(barCenters, dat$mean+1, labels, cex=1, pos=3) 
abline(h=53, col="red")


windows(30,18)
b <- plot(df$st,df$NO2, type="b", xlab="monitoring station",ylab="Nitrogen dioxide", cex.lab =1.4 ,cex.axis =1.4 , cex.names =1.4 ,las=1, family="serif", xaxt="n")
lab1 <- c("1","2","3","4","5","6","7","8","9","10","11","12")
axis(side=1,at=c(1:12),lab=lab1,padj=0,cex.axis=1.4,tcl=0,family="serif")
lg <- c("1: Ashok Vihar",   "2: BSZ Marg","3: DCE","4: Janakpuri","5: Mayapuri","6: N.Y. School","7: Nizamuddin","8: Pritampura","9: Shahdara","10: Shahzada Bagh", "11: Siri Fort", "12: Town Hall")
legend("topleft",leg=lg,inset=c(0.01,0.01),bg="ivory",cex=1.2, x.intersp=0.2,y.intersp=0.9,ncol=2)

dat <- df %>%
  group_by(st) %>%
  summarise(mean = mean(NO2, na.rm=T), n = n(), sd=sd(NO2, na.rm=T), se=sqrt(sd)/n,
            lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
            upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)
windows(10,7)
par(mar = c(5, 6, 4, 5) + 0.1)
plotTop <- max(dat$mean, na.rm=T) + 20
dat$clr <- ifelse(dat$mean<53,"darkolivegreen3",ifelse(dat$mean<100,"yellow",ifelse(dat$mean<150,"orange",
                                                                                    ifelse(dat$mean<200,"firebrick1",ifelse(dat$mean<300,"firebrick","grey")))))
barCenters <- barplot(height = dat$mean,
                      names.arg = dat$st,
                      beside = true, las = 2,
                      ylim = c(0, plotTop+1),
                      cex.names = 0.75, xaxt = "n",
                      main = "NO2 by station",
                      ylab = "microgram per cubic meter",
                      border = "black", axes = TRUE, col=dat$clr,yaxt="n")
axis(side=2, at=seq(0, plotTop, by=5), las=1)
grid()
# Specify the groupings. We use srt = 45 for a
# 45 degree string rotation
text(x = barCenters, y = par("usr")[3] - 1, srt = 45,
     adj = 1, labels = dat$st, xpd = TRUE)
segments(barCenters, dat$mean - dat$se * 2, barCenters,
         dat$mean + dat$se * 2, lwd = 1.5)
arrows(barCenters, dat$mean - dat$se * 2, barCenters,
       dat$mean + dat$se * 2, lwd = 1.5, angle = 90,
       code = 3, length = 0.05)
labels <- paste(round(dat$mean, digit=2))
text(barCenters, dat$mean+1, labels, cex=1, pos=3) 
abline(h=53, col="red")

windows(18,18)
plot(df$typ,df$NO2, type="c",xlab="type of location",ylab="Nitrogen dioxide", cex.lab =1.4 ,cex.axis =1.4 , cex.names =1.4 ,las=1, family="serif")
dat <- df %>%
  group_by(typ) %>%
  summarise(mean = mean(NO2, na.rm=T), n = n(), sd=sd(NO2, na.rm=T), se=sqrt(sd)/n,
            lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
            upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)
windows(10,7)
par(mar = c(5, 6, 4, 5) + 0.1)
plotTop <- max(dat$mean, na.rm=T) + 5
dat$clr <- ifelse(dat$mean<53,"darkolivegreen3",ifelse(dat$mean<100,"yellow",ifelse(dat$mean<150,"orange",
                                                                                    ifelse(dat$mean<200,"firebrick1",ifelse(dat$mean<300,"firebrick","grey")))))
barCenters <- barplot(height = dat$mean,
                      names.arg = dat$typ,
                      beside = true, las = 2,
                      cex.names = 0.75, xaxt = "n",
                      ylim = c(0, plotTop+1),
                      main = "NO2 by area",
                      ylab = "microgram per cubic meter",
                      border = "black", axes = TRUE, col=dat$clr,yaxt="n")
axis(side=2, at=seq(0, plotTop, by=5), las=1)
# Specify the groupings. We use srt = 45 for a
# 45 degree string rotation
#text(x = barCenters, y = par("usr")[3] - 1, srt = 45,adj = 1, labels = dat$typ, xpd = TRUE)
#segments(barCenters, dat$mean - dat$se * 2, barCenters, dat$mean + dat$se * 2, lwd = 1.5)
#arrows(barCenters, dat$mean - dat$se * 2, barCenters,dat$mean + dat$se * 2, lwd = 1.5, angle = 90,code = 3, length = 0.05)
labels <- paste(round(dat$mean, digit=2))
text(barCenters, dat$mean+1, labels, cex=1, pos=3) 
abline(h=53, col="red")

library(ggplot2)
windows()
ggplot(df, aes(fill=yr, y=NO2, x=st)) +
  geom_bar( position="dodge", stat="identity",na.rm=T, alpha=0.7)+
  geom_hline(yintercept=c(50,100,150,200,300), color=c("darkolivegreen3", "yellow","orange","firebrick1","firebrick"),size=1)


windows()
clr <- ifelse(df$mn=="Jan","cadelblue1",
       ifelse(df$mn=="Feb","chartreuse",
       ifelse(df$mn=="Mar","chocolate4",
       ifelse(df$mn=="Apr","cornflowerblue",
       ifelse(df$mn=="May","blue4",
       ifelse(df$mn=="Jun","darkorange",
       ifelse(df$mn=="Jul","gray0",
       ifelse(df$mn=="Aug","green",
       ifelse(df$mn=="Sep","honeydew4",
       ifelse(df$mn=="Oct","ivory2",
       ifelse(df$mn=="Nov","lightskyblue",
       ifelse(df$mn=="Dec","midnightblue","yellow"
              ))))))))))))
#clr <- unique(clr)
d<- subset(df, !(df$mn=="Annual"))
windows()
ggplot(d, aes(fill=mn, y=NO2, x=yr)) +
  geom_bar( position="dodge", stat="identity",na.rm=T, alpha=0.7)+
  scale_fill_manual("legend", values = clr)+
  geom_hline(yintercept=c(50,100,150,200,300), color=c("darkolivegreen3", "yellow","orange","firebrick1","firebrick"),size=1)


scale_color_manual(labels = levels(d$mn), values = clr) +
  theme_bw() +
  guides(color=guide_legend("my title"))+  # add guide properties by aesthetic

# Scatter plot comparing gdpPercap and lifeExp, with color representing continent
# and size representing population, faceted by year

#d$tlv <- ifelse(d$NO2<50,"less than 50",ifelse(d$NO2<100,"more than 50",ifelse(d$NO2<150,"more than 100","more than 150")))
d$y <- factor(d$yr, levels=c(levels(d$yr),"2010"))
windows(120,60)
ggplot(data=d,aes(x=mn,y=NO2,color=st, na.rm=T)) + #,size=tlv  
geom_point(na.rm=T) + 
geom_hline(yintercept=c(53), color=c("darkolivegreen3"),size=1)+
facet_wrap(~y) + 
  scale_color_manual(values=c(
  "yellow","chartreuse","chocolate4","seagreen3","blueviolet",
  "darkorange","gray0","green","honeydew4","ivory2","lightskyblue",
  "midnightblue"))+
  labs(title="", y="Nitrogen Dioxide", x="Month", color="Station")

                                                                                             
        
#mod2 <- lm(data=df,NO2~mn+yr+st+typ+SO2+PM10+SPM)
mod <- lm(data=df,(NO2)^(1/2)~mn+yr+st+typ)
summary(mod)
windows()
par(mfrow=c(2,2))
plot(mod)

windows(4,4)
par(mar=c(2.5,0.5,0,0.6),mgp=c(1.1,0.2,0),oma=c(0,1.8,2.5,0),las=1,tcl=-0.2)
qqnorm(resid(mod),las=1,pch=20,cex=1.2,main="")
qqline(resid(mod),col=2)
mtext(side=3,"Sample Quantiles",adj=-0.14,line=0.1)
mtext(side=3,"Linear Model",adj=1,line=0.1)

rsq <- rez$adj.r.sq
drop1(mod,test="F")
rez1 <- drop1(mod,test="F")
pval <- rez1$"Pr(>F)"[2:3]
pval1 <- ifelse(pval[1]<0.0001,"<0.0001",round(pval[1],3))
pval2 <- ifelse(pval[2]<0.0001,"<0.0001",round(pval[2],3))
text(-3,0.2,paste("p-value: ",pval1,sep=""),adj=0)
mtext(side=3,adj=1,line=1.4,"Nitrogen Dioxide")
box()

# plot results
source("dcis.Rcm")
names(df)
d <- na.omit(df[,c(1:3,6,7)])
d$yr <- droplevels(d$yr, exclude = "2004")
names(d)
lm.dci(d,yID=4,xIDs=c(1:3,5)) -> rez
xlab1 <- "Month"
xlab2 <- "Year"
xlab3 <- "Station"
xlab4 <- "Area"
ylab <- expression(paste("Nitrogen Dioxide",sep="")) 

windows(25,10)				# Figure 5

par(oma=c(0,0,0,0),mar=c(2.5,2,2.5,1),las=1,mgp=c(1.1,0.1,0),tcl=0.1)

n1 <- length(unique(d[,1]))
n2 <- length(unique(d[,2]))
n3 <- length(unique(d[,3]))
n4 <- length(unique(d[,5]))

xCoord <- c(1:n1,(n1+2):(n1+n2+1),(n1+n2+3):(n1+n2+n3+2), (n1+n2+n3+4):(n1+n2+n3+n4+3))
mAdj1 <- rez[c(1:n1),3]
mAdj2 <- rez[(n1+1):(n1+n2),3]
mAdj3 <- rez[(n1+n2+1):(n1+n2+n3),3]
mAdj4 <- rez[(n1+n2+n3+1):(n1+n2+n3+n4),3]

meanTrue <- mean(d$NO2)
yCoord <- c(mAdj1,mAdj2,mAdj3,mAdj4)

cilb1 <- rez[1:n1,4]
cilb2 <- rez[(n1+1):(n1+n2),4]
cilb3 <- rez[(n1+n2+1):(n1+n2+n3),4]
cilb4 <- rez[(n1+n2+n3+1):(n1+n2+n3+n4),4]
ciub1 <- rez[1:n1,5]
ciub2 <- rez[(n1+1):(n1+n2),5]
ciub3 <- rez[(n1+n2+1):(n1+n2+n3),5]
ciub4 <- rez[(n1+n2+n3+1):(n1+n2+n3+n4),5]

mCr1 <- tapply(d$NO2,d$mn,mean)	# crude means
mCr2 <- tapply(d$NO2,d$yr,mean)
mCr3 <- tapply(d$NO2,d$st,mean)
mCr4 <- tapply(d$NO2,d$typ,mean)

ymin <- min(cilb1,cilb2,cilb3,cilb4,mCr1,mCr2,mCr3,mCr4)-0.05
ymax <- max(ciub1,ciub2,ciub3,ciub4,mCr1,mCr2,mCr3,mCr4)+0.15

plot(1,type="n",xlim=c(1,max(xCoord)+0.001),ylim=c(ymin,ymax),ylab="",xlab="",xaxt="n",cex.axis=1)
abline(v=c(1:max(xCoord)),col="grey")
abline(h=meanTrue,col=2)
abline(v=c(n1+1,n1+n2+2,n1+n2+n3+3),col="dimgrey")
dx <- 0.1
points(xCoord[1:n1]+dx,yCoord[1:n1],type="l")

for (i in c(1:n1)) {
  points(xCoord[i]+c(0,0)+dx,c(cilb1[i],ciub1[i]),type="l",lwd=1)
}
for (i in c(1:n2)) {
  points(xCoord[n1+i]+c(0,0)+dx,c(cilb2[i],ciub2[i]),type="l",lwd=1)
}
for (i in c(1:n3)) {
  points(xCoord[n1+n2+i]+c(0,0)+dx,c(cilb3[i],ciub3[i]),type="l",lwd=1)
}
for (i in c(1:n4)) {
  points(xCoord[n1+n2+n3+i]+c(0,0)+dx,c(cilb4[i],ciub4[i]),type="l",lwd=1)
}
points(xCoord+dx+0.03,yCoord+0.005,pch="_")

points(xCoord[1:n1]-dx,mCr1,pch=21,bg=3,cex=0.9)
points(xCoord[(n1+1):(n1+n2)]-dx,mCr2,pch=21,bg=3,cex=0.9)
points(xCoord[(n1+n2+1):(n1+n2+n3)]-dx,mCr3,pch=21,bg=3,cex=0.9)
points(xCoord[(n1+n2+n3+1):(n1+n2+n3+n4)]-dx,mCr4,pch=21,bg=3,cex=0.9)

lb1 <- c("j","f","m","a","m","j","j","a","s","o","n","d","A")
lb2 <- c("00","01","02","03","05","06","07","08","09","11","12","13","14","15")
lb3 <- c("1","2","3","4","5","6","7","8","9","10","11","12")
lb4 <- c("I","R")
dy <- (-0.3)
axis(side=1,at=c(1:n1),lab=lb1,padj=dy,cex.axis=1)
axis(side=1,at=n1+1+c(1:n2),lab=lb2,padj=dy,cex.axis=1)
axis(side=1,at=n1+n2+2+c(1:n3),lab=lb3,padj=dy,cex.axis=1)
axis(side=1,at=n1+n2+n3+3+c(1:n4),lab=lb4,padj=dy,cex.axis=1)

at1 <- (1+n1)/2
axis(side=1,at=at1,lab=xlab1,tcl=0,padj=1.4,cex.axis=1)
at2 <- (n1+1+n1+n2+2)/2
axis(side=1,at=at2,lab=xlab2,tcl=0,padj=1.4,cex.axis=1)
at3 <- (n1+n2+2+n1+n2+n3+3)/2
axis(side=1,at=at3,lab=xlab3,tcl=0,padj=1.4,cex.axis=1)
at4 <- (n1+n2+n3+3+n1+n2+n3+n4+4)/2
axis(side=1,at=at4,lab=xlab4,tcl=0,padj=1.4,cex.axis=1)

mtext(side=3,line=0,adj=0.01,ylab,cex=1)
lab="Linear Model"
mtext(side=3,line=0.2,adj=1,lab,cex=1)

lg1 <- c("j:  January","f:  February","m: March","a:  April","m: May","j:  June","j:  July","a:  August","s:  September","o:  October","n:  November","d:  Desember", "A:  Annual")
legend("topleft",leg=lg1,inset=c(0.017,0.025),bg="ivory",cex=0.9,title="Month",x.intersp=0.2,y.intersp=0.9,ncol=3)

lg2 <- c("00: 2000","01: 2001","02: 2002","03: 2003","05: 2005","06: 2006","07: 2007" ,"08: 2008","09: 2009","11: 2011","12: 2012","13: 2013","14: 2014","15: 2015")
legend("topleft",leg=lg2,inset=c(0.34,0.025),bg="ivory",cex=0.9,title="Year",x.intersp=0.2,y.intersp=0.9,ncol=3)

lg3 <- c("1: Ashok Vihar", "2: BSZ Marg", "3: DCE",          
         "4: Janakpuri", "5: Mayapuri","6: N.Y. School",
         "7: Nizamuddin", "8: Pritampura", "9: Shahdara" ,    
         "10: Shahzada Bagh","11: Siri Fort", "12: Town Hall")
legend("bottomright",leg=lg3,inset=c(0.37,0.025),bg="ivory",cex=0.9,title="Station",x.intersp=0.2,y.intersp=0.9,ncol=2)

lg4 <- c("I: Industrial","R: Residential")
legend("bottomright",leg=lg4,inset=c(0.01,0.025),bg="ivory",cex=0.9,title="Year",x.intersp=0.2,y.intersp=0.9,ncol=1)


#___________________________________________________
#PM10
#plot descriptive PM10
windows(30,18)
plot(df$mn,df$PM10, type="p", xlab="month", ylab="particulate matter 10 ", cex.lab =1.4 ,cex.axis =1.4 , cex.names =1.4 ,las=1, family="serif")
windows(30,18)
plot(df$yr,df$PM10, type="l", xlab="year", ylab="particulate matter 10 ", cex.lab =1.4 ,cex.axis =1.4 , cex.names =1.4 ,las=1, family="serif")
windows(30,18)
b <- plot(df$st,df$PM10, type="b", xlab="monitoring station", ylab="particulate matter 10 ", cex.lab =1.4 ,cex.axis =1.4 , cex.names =1.4 ,las=1, family="serif",xaxt="n")
lab1 <- c("1","2","3","4","5","6","7","8","9","10","11","12")
axis(side=1,at=c(1:12),lab=lab1,padj=0,cex.axis=1.4,tcl=0,family="serif")
lg <- c("1: Ashok Vihar",   "2: BSZ Marg","3: DCE","4: Janakpuri","5: Mayapuri","6: N.Y. School","7: Nizamuddin","8: Pritampura","9: Shahdara","10: Shahzada Bagh", "11: Siri Fort", "12: Town Hall")
legend("topleft",leg=lg,inset=c(0.01,0.01),bg="ivory",cex=1.2, x.intersp=0.2,y.intersp=0.9,ncol=2)
windows(18,18)
plot(df$typ,df$PM10, type="c", xlab="type of location", ylab="particulate matter 10 ", cex.lab =1.4 ,cex.axis =1.4 , cex.names =1.4 ,las=1, family="serif")

#mod3 <- lm(data=df,PM10~mn+yr+st+typ+NO2+SO2+SPM)
mod <- lm(data=df,log(PM10)~mn+yr+st+typ)
summary(mod)
windows()
par(mfrow=c(2,2))
plot(mod)

windows(4,4)
par(mar=c(2.5,0.5,0,0.6),mgp=c(1.1,0.2,0),oma=c(0,1.8,2.5,0),las=1,tcl=-0.2)
qqnorm(resid(mod),las=1,pch=20,cex=1.2,main="")
qqline(resid(mod),col=2)
mtext(side=3,"Sample Quantiles",adj=-0.14,line=0.1)
mtext(side=3,"Log-Linear Model",adj=1,line=0.1)

rsq <- rez$adj.r.sq
drop1(mod,test="F")
rez1 <- drop1(mod,test="F")
pval <- rez1$"Pr(>F)"[2:3]
pval1 <- ifelse(pval[1]<0.0001,"<0.0001",round(pval[1],3))
pval2 <- ifelse(pval[2]<0.0001,"<0.0001",round(pval[2],3))
text(-3,0.2,paste("p-value: ",pval1,sep=""),adj=0)
mtext(side=3,adj=1,line=1.4,"PM10")
box()

# plot results
source("dcis.Rcm")
names(df)
d <- na.omit(df[,c(1:3,4,7)])
d$mn <- droplevels(d$mn, exclude = "Annual")
d$yr <- droplevels(d$yr, exclude = c("2000","2001","2002"))
d$st <- droplevels(d$st, exclude = "3")
names(d)
lm.dci(d,yID=4,xIDs=c(1:3,5)) -> rez
xlab1 <- "Month"
xlab2 <- "Year"
xlab3 <- "Station"
xlab4 <- "Area"
ylab <- expression(paste("PM10",sep="")) 

windows(25,10)				# Figure 5

par(oma=c(0,0,0,0),mar=c(2.5,2,2.5,1),las=1,mgp=c(1.1,0.1,0),tcl=0.1)

n1 <- length(unique(d[,1]))
n2 <- length(unique(d[,2]))
n3 <- length(unique(d[,3]))
n4 <- length(unique(d[,5]))

xCoord <- c(1:n1,(n1+2):(n1+n2+1),(n1+n2+3):(n1+n2+n3+2), (n1+n2+n3+4):(n1+n2+n3+n4+3))
mAdj1 <- rez[c(1:n1),3]
mAdj2 <- rez[(n1+1):(n1+n2),3]
mAdj3 <- rez[(n1+n2+1):(n1+n2+n3),3]
mAdj4 <- rez[(n1+n2+n3+1):(n1+n2+n3+n4),3]

meanTrue <- mean(d$PM10)
yCoord <- c(mAdj1,mAdj2,mAdj3,mAdj4)

cilb1 <- rez[1:n1,4]
cilb2 <- rez[(n1+1):(n1+n2),4]
cilb3 <- rez[(n1+n2+1):(n1+n2+n3),4]
cilb4 <- rez[(n1+n2+n3+1):(n1+n2+n3+n4),4]
ciub1 <- rez[1:n1,5]
ciub2 <- rez[(n1+1):(n1+n2),5]
ciub3 <- rez[(n1+n2+1):(n1+n2+n3),5]
ciub4 <- rez[(n1+n2+n3+1):(n1+n2+n3+n4),5]

mCr1 <- tapply(d$PM10,d$mn,mean)	# crude means
mCr2 <- tapply(d$PM10,d$yr,mean)
mCr3 <- tapply(d$PM10,d$st,mean)
mCr4 <- tapply(d$PM10,d$typ,mean)

ymin <- min(cilb1,cilb2,cilb3,cilb4,mCr1,mCr2,mCr3,mCr4)-0.05
ymax <- max(ciub1,ciub2,ciub3,ciub4,mCr1,mCr2,mCr3,mCr4)+0.15

plot(1,type="n",xlim=c(1,max(xCoord)+0.001),ylim=c(ymin,ymax),ylab="",xlab="",xaxt="n",cex.axis=1)
abline(v=c(1:max(xCoord)),col="grey")
abline(h=meanTrue,col=2)
abline(v=c(n1+1,n1+n2+2,n1+n2+n3+3),col="dimgrey")
dx <- 0.1
points(xCoord[1:n1]+dx,yCoord[1:n1],type="l")

for (i in c(1:n1)) {
  points(xCoord[i]+c(0,0)+dx,c(cilb1[i],ciub1[i]),type="l",lwd=1)
}
for (i in c(1:n2)) {
  points(xCoord[n1+i]+c(0,0)+dx,c(cilb2[i],ciub2[i]),type="l",lwd=1)
}
for (i in c(1:n3)) {
  points(xCoord[n1+n2+i]+c(0,0)+dx,c(cilb3[i],ciub3[i]),type="l",lwd=1)
}
for (i in c(1:n4)) {
  points(xCoord[n1+n2+n3+i]+c(0,0)+dx,c(cilb4[i],ciub4[i]),type="l",lwd=1)
}
points(xCoord+dx+0.03,yCoord+0.005,pch="_")

points(xCoord[1:n1]-dx,mCr1,pch=21,bg=3,cex=0.9)
points(xCoord[(n1+1):(n1+n2)]-dx,mCr2,pch=21,bg=3,cex=0.9)
points(xCoord[(n1+n2+1):(n1+n2+n3)]-dx,mCr3,pch=21,bg=3,cex=0.9)
points(xCoord[(n1+n2+n3+1):(n1+n2+n3+n4)]-dx,mCr4,pch=21,bg=3,cex=0.9)

lb1 <- c("j","f","m","a","m","j","j","a","s","o","n","d")
lb2 <- c("03","04", "05","06","07","08","09","11","12","13","14","15")
lb3 <- c("1","2","4","5","6","7","8","9","10","11","12")
lb4 <- c("I","R")
dy <- (-0.3)
axis(side=1,at=c(1:n1),lab=lb1,padj=dy,cex.axis=1)
axis(side=1,at=n1+1+c(1:n2),lab=lb2,padj=dy,cex.axis=1)
axis(side=1,at=n1+n2+2+c(1:n3),lab=lb3,padj=dy,cex.axis=1)
axis(side=1,at=n1+n2+n3+3+c(1:n4),lab=lb4,padj=dy,cex.axis=1)

at1 <- (1+n1)/2
axis(side=1,at=at1,lab=xlab1,tcl=0,padj=1.4,cex.axis=1)
at2 <- (n1+1+n1+n2+2)/2
axis(side=1,at=at2,lab=xlab2,tcl=0,padj=1.4,cex.axis=1)
at3 <- (n1+n2+2+n1+n2+n3+3)/2
axis(side=1,at=at3,lab=xlab3,tcl=0,padj=1.4,cex.axis=1)
at4 <- (n1+n2+n3+3+n1+n2+n3+n4+4)/2
axis(side=1,at=at4,lab=xlab4,tcl=0,padj=1.4,cex.axis=1)

mtext(side=3,line=0,adj=0.01,ylab,cex=1)
lab="Linear Model"
mtext(side=3,line=0.2,adj=1,lab,cex=1)

lg1 <- c("j:  January","f:  February","m: March","a:  April","m: May","j:  June","j:  July","a:  August","s:  September","o:  October","n:  November","d:  Desember")
legend("topleft",leg=lg1,inset=c(0.017,0.025),bg="ivory",cex=0.9,title="Month",x.intersp=0.2,y.intersp=0.9,ncol=3)

lg2 <- c("03: 2003","04: 2004", "05: 2005","06: 2006","07: 2007" ,"08: 2008","09: 2009","11: 2011","12: 2012","13: 2013","14: 2014","15: 2015")
legend("topleft",leg=lg2,inset=c(0.34,0.025),bg="ivory",cex=0.9,title="Year",x.intersp=0.2,y.intersp=0.9,ncol=3)

lg3 <- c("1: Ashok Vihar", "2: BSZ Marg",           
         "4: Janakpuri", "5: Mayapuri","6: N.Y. School",
         "7: Nizamuddin", "8: Pritampura", "9: Shahdara" ,    
         "10: Shahzada Bagh","11: Siri Fort", "12: Town Hall")
legend("bottomright",leg=lg3,inset=c(0.37,0.025),bg="ivory",cex=0.9,title="Station",x.intersp=0.2,y.intersp=0.9,ncol=2)

lg4 <- c("I: Industrial","R: Residential")
legend("bottomright",leg=lg4,inset=c(0.01,0.025),bg="ivory",cex=0.9,title="Year",x.intersp=0.2,y.intersp=0.9,ncol=1)




#__________________________________________________
#plot SPM
windows(30,18)
plot(df$mn,df$SPM, type="p", xlab="month", ylab="", cex.lab =1.4 ,cex.axis =1.4 , cex.names =1.4 ,las=1)
windows(30,18)
plot(df$yr,df$SPM, type="l", xlab="year", ylab="", cex.lab =1.4 ,cex.axis =1.4 , cex.names =1.4 ,las=1, family="serif")
windows(30,18)
b <- plot(df$st,df$SPM, type="b", xlab="monitoring station", ylab="", cex.lab =1.4 ,cex.axis =1.4 , cex.names =1.4 ,las=1, family="serif", xaxt="n")
lab1 <- c("1","2","3","4","5","6","7","8","9","10","11","12")
axis(side=1,at=c(1:12),lab=lab1,padj=0,cex.axis=1.4,tcl=0,family="serif")
lg <- c("1: Ashok Vihar",   "2: BSZ Marg","3: DCE","4: Janakpuri","5: Mayapuri","6: N.Y. School","7: Nizamuddin","8: Pritampura","9: Shahdara","10: Shahzada Bagh", "11: Siri Fort", "12: Town Hall")
legend("topleft",leg=lg,inset=c(0.01,0.01),bg="ivory",cex=1.2, x.intersp=0.2,y.intersp=0.9,ncol=2)
windows(18,18)
plot(df$typ,df$SPM, type="c", xlab="type of location",ylab="", cex.lab =1.4 ,cex.axis =1.4 , cex.names =1.4 ,las=1, family="serif")

#SPM
#mod4 <- lm(data=df,SPM~mn+yr+st+typ+NO2+PM10+SO2)
mod <- lm(data=df,log(SPM)~mn+yr+st+typ)
summary(mod)
windows()
par(mfrow=c(2,2))
plot(mod)

windows(4,4)
par(mar=c(2.5,0.5,0,0.6),mgp=c(1.1,0.2,0),oma=c(0,1.8,2.5,0),las=1,tcl=-0.2)
qqnorm(resid(mod),las=1,pch=20,cex=1.2,main="")
qqline(resid(mod),col=2)
mtext(side=3,"Sample Quantiles",adj=-0.14,line=0.1)
mtext(side=3,"Log-Linear Model",adj=1,line=0.1)

rsq <- rez$adj.r.sq
drop1(mod,test="F")
rez1 <- drop1(mod,test="F")
pval <- rez1$"Pr(>F)"[2:3]
pval1 <- ifelse(pval[1]<0.0001,"<0.0001",round(pval[1],3))
pval2 <- ifelse(pval[2]<0.0001,"<0.0001",round(pval[2],3))
text(-3,0.2,paste("p-value: ",pval1,sep=""),adj=0)
mtext(side=3,adj=1,line=1.4,"PM10")
box()

# plot results
source("dcis.Rcm")
names(df)
d <- na.omit(df[,c(1:3,8,7)])
d$yr <- droplevels(d$yr, exclude = c("2011","2012","2013","2014"))
d$st <- droplevels(d$st, exclude = "3")
names(d)
lm.dci(d,yID=4,xIDs=c(1:3,5)) -> rez
xlab1 <- "Month"
xlab2 <- "Year"
xlab3 <- "Station"
xlab4 <- "Area"
ylab <- expression(paste("SPM",sep="")) 

windows(25,10)				# Figure 5

par(oma=c(0,0,0,0),mar=c(2.5,2,2.5,1),las=1,mgp=c(1.1,0.1,0),tcl=0.1)

n1 <- length(unique(d[,1]))
n2 <- length(unique(d[,2]))
n3 <- length(unique(d[,3]))
n4 <- length(unique(d[,5]))

xCoord <- c(1:n1,(n1+2):(n1+n2+1),(n1+n2+3):(n1+n2+n3+2), (n1+n2+n3+4):(n1+n2+n3+n4+3))
mAdj1 <- rez[c(1:n1),3]
mAdj2 <- rez[(n1+1):(n1+n2),3]
mAdj3 <- rez[(n1+n2+1):(n1+n2+n3),3]
mAdj4 <- rez[(n1+n2+n3+1):(n1+n2+n3+n4),3]

meanTrue <- mean(d$SPM)
yCoord <- c(mAdj1,mAdj2,mAdj3,mAdj4)

cilb1 <- rez[1:n1,4]
cilb2 <- rez[(n1+1):(n1+n2),4]
cilb3 <- rez[(n1+n2+1):(n1+n2+n3),4]
cilb4 <- rez[(n1+n2+n3+1):(n1+n2+n3+n4),4]
ciub1 <- rez[1:n1,5]
ciub2 <- rez[(n1+1):(n1+n2),5]
ciub3 <- rez[(n1+n2+1):(n1+n2+n3),5]
ciub4 <- rez[(n1+n2+n3+1):(n1+n2+n3+n4),5]

mCr1 <- tapply(d$SPM,d$mn,mean)	# crude means
mCr2 <- tapply(d$SPM,d$yr,mean)
mCr3 <- tapply(d$SPM,d$st,mean)
mCr4 <- tapply(d$SPM,d$typ,mean)

ymin <- min(cilb1,cilb2,cilb3,cilb4,mCr1,mCr2,mCr3,mCr4)-0.05
ymax <- max(ciub1,ciub2,ciub3,ciub4,mCr1,mCr2,mCr3,mCr4)+0.15

plot(1,type="n",xlim=c(1,max(xCoord)+0.001),ylim=c(ymin,ymax),ylab="",xlab="",xaxt="n",cex.axis=1)
abline(v=c(1:max(xCoord)),col="grey")
abline(h=meanTrue,col=2)
abline(v=c(n1+1,n1+n2+2,n1+n2+n3+3),col="dimgrey")
dx <- 0.1
points(xCoord[1:n1]+dx,yCoord[1:n1],type="l")

for (i in c(1:n1)) {
  points(xCoord[i]+c(0,0)+dx,c(cilb1[i],ciub1[i]),type="l",lwd=1)
}
for (i in c(1:n2)) {
  points(xCoord[n1+i]+c(0,0)+dx,c(cilb2[i],ciub2[i]),type="l",lwd=1)
}
for (i in c(1:n3)) {
  points(xCoord[n1+n2+i]+c(0,0)+dx,c(cilb3[i],ciub3[i]),type="l",lwd=1)
}
for (i in c(1:n4)) {
  points(xCoord[n1+n2+n3+i]+c(0,0)+dx,c(cilb4[i],ciub4[i]),type="l",lwd=1)
}
points(xCoord+dx+0.03,yCoord+0.005,pch="_")

points(xCoord[1:n1]-dx,mCr1,pch=21,bg=3,cex=0.9)
points(xCoord[(n1+1):(n1+n2)]-dx,mCr2,pch=21,bg=3,cex=0.9)
points(xCoord[(n1+n2+1):(n1+n2+n3)]-dx,mCr3,pch=21,bg=3,cex=0.9)
points(xCoord[(n1+n2+n3+1):(n1+n2+n3+n4)]-dx,mCr4,pch=21,bg=3,cex=0.9)

lb1 <- c("j","f","m","a","m","j","j","a","s","o","n","d","A")
lb2 <- c("00","01","02", "03","04", "05","06","07","08","09","15")
lb3 <- c("1","2","4","5","6","7","8","9","10","11","12")
lb4 <- c("I","R")
dy <- (-0.3)
axis(side=1,at=c(1:n1),lab=lb1,padj=dy,cex.axis=1)
axis(side=1,at=n1+1+c(1:n2),lab=lb2,padj=dy,cex.axis=1)
axis(side=1,at=n1+n2+2+c(1:n3),lab=lb3,padj=dy,cex.axis=1)
axis(side=1,at=n1+n2+n3+3+c(1:n4),lab=lb4,padj=dy,cex.axis=1)

at1 <- (1+n1)/2
axis(side=1,at=at1,lab=xlab1,tcl=0,padj=1.4,cex.axis=1)
at2 <- (n1+1+n1+n2+2)/2
axis(side=1,at=at2,lab=xlab2,tcl=0,padj=1.4,cex.axis=1)
at3 <- (n1+n2+2+n1+n2+n3+3)/2
axis(side=1,at=at3,lab=xlab3,tcl=0,padj=1.4,cex.axis=1)
at4 <- (n1+n2+n3+3+n1+n2+n3+n4+4)/2
axis(side=1,at=at4,lab=xlab4,tcl=0,padj=1.4,cex.axis=1)

mtext(side=3,line=0,adj=0.01,ylab,cex=1)
lab="Linear Model"
mtext(side=3,line=0.2,adj=1,lab,cex=1)

lg1 <- c("j:  January","f:  February","m: March","a:  April","m: May","j:  June","j:  July","a:  August","s:  September","o:  October","n:  November","d:  Desember")
legend("bottomleft",leg=lg1,inset=c(0.04,0.025),bg="ivory",cex=0.9,title="Month",x.intersp=0.2,y.intersp=0.9,ncol=3)

lg2 <- c("00: 2000", "01: 2001","02:2002", "03: 2003","04: 2004", "05: 2005","06: 2006","07: 2007" ,"08: 2008","09: 2009","15: 2015")
legend("bottomleft",leg=lg2,inset=c(0.4,0.025),bg="ivory",cex=0.9,title="Year",x.intersp=0.2,y.intersp=0.9,ncol=3)

lg3 <- c("1: Ashok Vihar", "2: BSZ Marg",           
         "4: Janakpuri", "5: Mayapuri","6: N.Y. School",
         "7: Nizamuddin", "8: Pritampura", "9: Shahdara" ,    
         "10: Shahzada Bagh","11: Siri Fort", "12: Town Hall")
legend("bottomright",leg=lg3,inset=c(0.125,0.025),bg="ivory",cex=0.9,title="Station",x.intersp=0.2,y.intersp=0.9,ncol=2)

lg4 <- c("I: Industrial","R: Residential")
legend("bottomright",leg=lg4,inset=c(0.01,0.025),bg="ivory",cex=0.9,title="Year",x.intersp=0.2,y.intersp=0.9,ncol=1)




#____________________________________ pause 4