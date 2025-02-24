#Upload and clean age data
MPO_a<-read.csv("C:/Users/ahoward1/Documents/Disc golf analytics/Aging curves/Disc-Golf-Aging-Curves/Ages.csv",header=T,sep=",")
MPO_a$Age<-numeric(nrow(MPO_a))
for (i in 1:nrow(MPO_a)){
  MPO_a$Age[i]<-substr(as.character(MPO_a$Date[i]),(nchar(as.character(MPO_a$Date[i]))-3),nchar(as.character(MPO_a$Date[i])))
}
MPO_a$Age<-as.numeric(MPO_a$Age)

#read in outFd for PDGA numbers
outFd<-read.csv("C:/Users/ahoward1/Documents/Disc golf analytics/outFd.csv", sep=",", header=T)
outFd$Last <- sub('.* ','',outFd$Name)
outFd$First <- sub(' .*','',outFd$Name)

MPO_a$PDGA<-outFd$PDGA[match(MPO_a$Last,outFd$Last)]

MPO_a$PDGA[MPO_a$Last=="Ulibarri" & MPO_a$Age=="1979"]<-41750
MPO_a$PDGA[MPO_a$Last=="Johansen" & MPO_a$Age=="1986"]<-42493
MPO_a$PDGA[MPO_a$Last=="Johansen" & MPO_a$Age=="1979"]<-20300
MPO_a$PDGA[MPO_a$Last=="Smith" & MPO_a$Age=="1971"]<-4034
MPO_a$PDGA[MPO_a$Last=="Smith" & MPO_a$Age=="1976"]<-16831
MPO_a$PDGA[MPO_a$Last=="Jones" & MPO_a$Age=="1996"]<-41760
MPO_a$PDGA[MPO_a$Last=="Russell" & MPO_a$Age=="1994"]<-66362
MPO_a$PDGA[MPO_a$Last=="Smith" & MPO_a$Age=="1987"]<-128378
MPO_a$PDGA[MPO_a$Last=="Smith" & MPO_a$Age=="1976"]<-16831
MPO_a$PDGA[MPO_a$Last=="Smith" & MPO_a$Age=="1971"]<-4034
MPO_a$PDGA[MPO_a$Last=="Smith" & MPO_a$Age=="2004"]<-101574
MPO_a$PDGA[MPO_a$Last=="Henson"]<-60440
MPO_a$PDGA[MPO_a$Last=="Budge"]<-90487
MPO_a$PDGA[MPO_a$Last=="Kester IV"]<-36777
MPO_a$PDGA[MPO_a$Last=="McClellan"]<-3566
MPO_a$PDGA[MPO_a$Last=="McRee"]<-7883
MPO_a$PDGA[MPO_a$Last=="M?ller"]<-4498
MPO_a$PDGA[MPO_a$Last=="O'Reilly"]<-99648
MPO_a$PDGA[MPO_a$Last=="Palmeri"]<-23
MPO_a$PDGA[MPO_a$Last=="Rainey"]<-41947
MPO_a$PDGA[MPO_a$Last=="Sheehan Jr."]<-62461
MPO_a$PDGA[MPO_a$Last=="Conrad"]<-17295
MPO_a$PDGA[MPO_a$Last=="Rico"]<-4666
MPO_a$PDGA[MPO_a$Last=="Schultz"]<-6840

MPO_a<-MPO_a[complete.cases(MPO_a[,c(1,2,5,6)]),c(1,2,5,6)]
MPO_a<-MPO_a[1:which(MPO_a$Last=="Wysocki"),]


which(table(MPO_a$Last)>=2)


#scrape ratings data
require(rvest)
MPO_a$avail<-numeric(nrow(MPO_a));PDGA<-numeric()
event <- read_html(paste("https://www.pdga.com/player/",MPO_a$PDGA[1],"/history",sep=""))
ratd<-event %>%
  html_nodes(".date") %>%
  html_text()
ratr<-event %>%
  html_nodes(".player-rating") %>%
  html_text()
ratrd<-event %>%
  html_nodes(".round") %>%
  html_text()
if (length(ratd)!=0) {MPO_a$avail[1]<-"y"} else {MPO_a$avail[1]<-"n"}
if (length(ratd)!=0) {PDGA<-rep(MPO_a$PDGA[1],length(ratd))}
for (i in 2:nrow(MPO_a)) {
event <- read_html(paste("https://www.pdga.com/player/",MPO_a$PDGA[i],"/history",sep=""))
ratd1<-event %>%
  html_nodes(".date") %>%
  html_text()
ratd<-c(ratd,ratd1)
ratr1<-event %>%
  html_nodes(".player-rating") %>%
  html_text()
ratr<-c(ratr,ratr1)
ratrd1<-event %>%
  html_nodes(".round") %>%
  html_text()
ratrd<-c(ratrd,ratrd1)
if (length(ratd1)!=0) {MPO_a$avail[i]<-"y"} else {MPO_a$avail[i]<-"n"}
if (length(ratd1)!=0) {PDGA<-c(PDGA,rep(MPO_a$PDGA[i],length(ratd1)))}
print(i)
}

MPO_a<-MPO_a[MPO_a$avail=="y",]

ratd<-ratd[ratd!="Effective Date"]
ratr<-ratr[ratr!="Rating"]
ratrd<-ratrd[ratrd!="Rounds Used"]

MPO_ar<-data.frame(ratd,ratr,ratrd)

#formatting data
MPO_a<-MPO_a[order(unique(PDGA)),]
MPO_ar<-MPO_ar[order(PDGA),]
MPO_ar$Name<-rep(MPO_a$Last,table(PDGA))
MPO_ar$Name_f<-rep(MPO_a$First,table(PDGA))
MPO_ar$Byear<-rep(MPO_a$Age,table(PDGA))
MPO_ar$PDGA<-rep(MPO_a$PDGA,table(PDGA))
MPO_ar<-MPO_ar[MPO_ar$ratd!="Effective Date",]

#reduce MPO_ar$ratd to just year
MPO_ar$year<-numeric(nrow(MPO_ar))
for (i in 1:nrow(MPO_ar)){
  MPO_ar$year[i]<-substr(as.character(MPO_ar$ratd[i]),(nchar(as.character(MPO_ar$ratd[i]))-3),nchar(as.character(MPO_ar$ratd[i])))
}
MPO_ar$year<-as.numeric(MPO_ar$year)

#Calculate age
MPO_ar$Age<-MPO_ar$year-MPO_ar$Byear

MPO_ar$ratr<-as.numeric(as.character(MPO_ar$ratr))
MPO_ar$ratrd<-as.numeric(as.character(MPO_ar$ratrd))

write.csv(MPO_ar,"MPO_ar.csv")

#########################################################
########Run through here to create MPO_ar dataset########
#########################################################

#controlling for rating inflation
m0<-lm(MPO_ar$ratr~MPO_ar$year+MPO_ar$Age+I(MPO_ar$Age^2))
color<-adjustcolor(col="gray",alpha.f=0.5)
plot(MPO_ar$ratr~MPO_ar$year,ylab="Rating",xlab="Year",pch=20,col=color)
abline(lm(MPO_ar$ratr~MPO_ar$year),lwd=2,col="firebrick3")

MPO_ar$infy<-MPO_ar$year-1997
MPO_ar$ratr<-MPO_ar$ratr-(MPO_ar$infy*coef(m0)[2])

#Calculate age curve
require(matrixStats)
ry<-tapply(MPO_ar$ratr,MPO_ar[c("Name","Age")],mean,na.rm=T)[,18:52]
ryp<-apply(ry,1,diff)
w<-t(tapply(MPO_ar$ratrd,MPO_ar[c("Name","Age")],sum,na.rm=T))[18:52,]
aging1<-numeric(nrow(ryp))
for (i in 1:nrow(ryp)){
aging1[i]<-weighted.mean(ryp[i,],w[i,],na.rm=T)}
aging<-cumsum(c(0,aging1))
year<-c(16,as.numeric(names(cumsum(c(0,rowMeans(ryp,na.rm=T))))[-1]))

m2<-lm(aging~year+I(year^2)+I(year^3))
color<-adjustcolor(col="gray",alpha.f=0.5)
plot(aging~year,pch=20,col=color,cex=1.5,ylab="Change in Rating",xlab="Age")#not controlling for inflation
lines(year,predict(m2),lwd=2,col="cadetblue4")
points(aging~year,pch=20,col=color,cex=1.5)
lines(year,predict(m2),lwd=2,col="firebrick3")

#prediction for Eagle McMahon
#control for inflation, prediction 2018
age<-20
ddd2<-cumsum(c(1039,diff(predict(m2))[which(year==age):(length(year)-1)]))
plot(ddd2[-1]~year[6:35],type="l",lwd=2,ylim=c(1035,1085),
     ylab="PDGA Rating",xlab="Age",col="cadetblue")
points(ddd2~year[5:35],type="l",lwd=2,lty=2,col="cadetblue")
text(36,1065,"2018 Projection",col="cadetblue")
points(year[17],1074.280,pch=20,cex=2,col="cadetblue")
text(year[17],1076.7,"1074",col="cadetblue")

#control for inflation, prediction 2019
ddd3<-cumsum(c(1053,diff(predict(m2))[6:34]))
points(ddd3[-1]~year[7:35],type="l",lwd=2,
     ylab="PDGA Rating",xlab="Age",col="firebrick3")
points(ddd3~year[6:35],type="l",lwd=2,lty=2,col="firebrick3")
text(46.5,1070,"2019 Projection",col="firebrick3")
points(year[17],1081.783,pch=20,cex=2,col="firebrick3")
text(year[17],1084.3,"1082",col="firebrick3")

#Paige Pierce
plot(cumsum(c(978,diff(predict(m2))[13:34]))~year[13:35])
