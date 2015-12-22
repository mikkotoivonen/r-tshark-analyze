require(lattice)
require(plyr)

#require(stringdist)
manuf <- read.csv("mactomanufacture.txt", header = FALSE, sep="\t")
colnames(manuf) <- c("MAC","Company")

getManufName <- function(mac,manuf){
if(substr(mac,0,8) %in% manuf$MAC){
return(paste(manuf$Company[manuf$MAC == substr(mac,0,8)],""))
}else{
return("")
}
}

getManufWithMac <- function(mac, manuf){
if(substr(mac,0,8) %in% manuf$MAC){
return(paste(mac,manuf$Company[manuf$MAC == substr(mac,0,8)]))
}else{
return(mac)
}
}

mac1 <- "ee:ee:ee:ee:ee:ee"

knownmacs <- data.frame(MAC = c("ee:ee:ee:ee:ee:ee"), NAME = c("Name of mac") )

url <- "url or file path"
dump <- read.csv(url, header=FALSE, col.names = c("Time","MAC","SSID","dbm"), fill = TRUE)
#colnames(dump) <- c("Time","MAC","SSID","dbm")
lct <- Sys.getlocale() # save current locale
Sys.setlocale("LC_TIME", "C") # set locale to C, so that English month names are accepted
dump$Time <- strptime(dump$Time, "%b %d, %Y %H:%M:%OS")
dump$Hour <- as.numeric(format(dump$Time, format="%H"))
dump$Minute <- as.numeric(format(dump$Time, format="%H"))*60 + as.numeric(format(dump$Time, format="%M"))

#Filter time. Set the number of days you want to go back
daysBack <- 2
dump <- subset(dump, Time > (Sys.time() - (daysBack*60*60*24)))

#Display 10 most active MAC addresses
sort(table(dump$MAC), decreasing = TRUE)[1:10]
topmac <- sort(table(dump$MAC), decreasing = TRUE)

#Get the first (or any) MAC address from the sorted list
firstmac <- names(topmac)[[1]]
secondmac <- names(topmac)[[2]]

#Display 10 most probed SSIDs
sort(table(dump$SSID), decreasing = TRUE)[1:10]

#Data for a specific SSID
ssid <- "default"
b <- subset(dump, SSID == ssid)

#Get SSID probes for a particular MAC
getSSIDforMAC <- function(mac, dump){
print(paste("MAC:",mac))
ssidmac <- sort(table(subset(dump, MAC == mac)$SSID), decreasing = TRUE)
#Filter out with zero rows (with zero value)
ssidmac_sub <- apply(ssidmac, 1, function(row) all(row !=0 ))
ssidmac <- ssidmac[ssidmac_sub]
print(names(ssidmac))
}

countSSIDforMAC <- function(mac, dump){
ssidmac <- sort(table(subset(dump, MAC == mac)$SSID), decreasing = TRUE)
#Filter out with zero rows (with zero value)
ssidmac_sub <- apply(ssidmac, 1, function(row) all(row !=0 ))
count <- sum(count(ssidmac[ssidmac_sub])$freq)
return(count)
}

#Get SSID list for specific mac
getSSIDforMAC(mac1,dump)
q <- apply(array(names(topmac[1:10])),1,getSSIDforMAC, dump = dump)

#Get MAC for a particular SSID
macssid <- sort(table(subset(dump, SSID == ssid)$MAC), decreasing = TRUE)
macssid_sub <- apply(macssid, 1, function(row) all(row !=0 ))
macssid <- macssid[macssid_sub]

#Data for a specific MAC addresses
mac <- mac1
a <- subset(dump, MAC == mac)

#Plot occurence of given mac
par(mar = c(5, 4, 4, 4) + 0.3)
plot(a$Time,rep(1,length(a$Time)), xlab="Time", ylab="Presence", pch="|")

#Plot time difference curve for a given mac
a <- subset(dump, MAC == mac)
timediff <- diff(a$Time)
par(new = TRUE)
plot(a$Time[1:length(a$Time)-1], log(as.numeric(timediff)), col="Blue",type="s", axes = FALSE, xlab="", ylab="")
axis(side=4, at = pretty(range(timediff)))
mtext("Log Time Difference", side=4, line=3)
tdf <- filter(timediff, filter=rep(1/5,5))
lines(a$Time[1:length(a$Time)-1], log(tdf), col="Red",type="s", lwd=2)
r <- as.POSIXct(seq(from = round(range(dump$Time)[1],"days"),to = round(range(dump$Time)[2],"days"),by=24*60*60)) # round by day so we can draw vertical lines every day
abline(v=r,col="red") # draw vertical linesa

#Plot histogram of time difference of probes
dev.new()
hist(log(as.numeric(timediff)), prob=TRUE, label=TRUE)

dev.new()
#Plot the presence of the top n mac addresses
n <- 20
i <- n
mac <- names(sort(table(dump$MAC), decreasing = TRUE)[1])
a <- subset(dump, MAC == mac)
plot(a$Time,rep(i,length(a$Time)), xlab="Time", ylab="Presence", ylim = c(0,n+1), xlim = as.POSIXct(range(dump$Time)), col=i, pch="|" , yaxt="n")
if(mac %in% knownmacs$MAC){
text(a$Time[round(length(a$Time)/2)], i-0.5, knownmacs$NAME[knownmacs$MAC == mac], col="green")
}else{
text(a$Time[round(length(a$Time)/2)], i-0.5, getManufWithMac(mac,manuf), col="red")
}
for(mac in array(names(sort(table(dump$MAC), decreasing = TRUE)[2:n]))){
i <- i-1
#par(new = TRUE)
a <- subset(dump, MAC == mac)
points(a$Time,rep(i,length(a$Time)), col=i, pch="|")
if(mac %in% knownmacs$MAC){
text(a$Time[round(length(a$Time)/2)], i-0.5, knownmacs$NAME[knownmacs$MAC == mac], col="green")
}else{
text(a$Time[round(length(a$Time)/2)], i-0.5, getManufWithMac(mac,manuf), col="red")
}
}
r <- as.POSIXct(seq(from = round(range(dump$Time)[1],"days"),to = round(range(dump$Time)[2],"days"),by=24*60*60)) # round by day so we can draw vertical lines every day
abline(v=r,col="red") # draw vertical lines

dev.new()
#Plot the radio power of the top n mac addresses
n <- 10
i <- n
mac <- names(sort(table(dump$MAC), decreasing = TRUE)[1])
a <- subset(dump, MAC == mac)
plot(a$Time,a$dbm, xlab="Time", ylab="dbm", ylim = c(-100,0), xlim = as.POSIXct(range(dump$Time)), col=i, pch="." , type = "l")
if(mac %in% knownmacs$MAC){
text(a$Time[round(length(a$Time)/2)], max(a$dbm), knownmacs$NAME[knownmacs$MAC == mac], col="green")
}else{
text(a$Time[round(length(a$Time)/2)], max(a$dbm), getManufWithMac(mac,manuf), col="red")
}
for(mac in array(names(sort(table(dump$MAC), decreasing = TRUE)[2:n]))){
i <- i-1
#par(new = TRUE)
a <- subset(dump, MAC == mac)
points(a$Time,a$dbm, col=i, pch=".", type = "l")
if(mac %in% knownmacs$MAC){
text(a$Time[round(length(a$Time)/2)], max(a$dbm), knownmacs$NAME[knownmacs$MAC == mac], col="green")
}else{
text(a$Time[round(length(a$Time)/2)], max(a$dbm), getManufWithMac(mac,manuf), col="red")
}
}
r <- as.POSIXct(seq(from = round(range(dump$Time)[1],"days"),to = round(range(dump$Time)[2],"days"),by=24*60*60)) # round by day so we can draw vertical lines every day
abline(v=r,col="red") # draw vertical lines

palette("default")

#Fold data by hour and plot by mac
dev.new()
dumpHourCount <- count(dump, vars = c("MAC", "Hour"))
n <- 5
i <- n
mac <- names(topmac[1])
a <- subset(dumpHourCount, MAC == mac)

plot(a$Hour,a$freq, xlab="Hour (folded)", ylab="Presence",col=i, type="s",ylim=c(0,max(dumpHourCount$freq)),xlim=c(0,24), pch=".", xaxt="n")
abline(v=0:23, col=rgb(0.9,0.9,0.9,0.9))
lines(a$Hour,a$freq,col=i, type="s")
if(mac %in% knownmacs$MAC){
text(subset(a,a$freq == max(a$freq))$Hour, max(a$freq), knownmacs$NAME[knownmacs$MAC == mac], col="green")
}else{
text(subset(a,a$freq == max(a$freq))$Hour, max(a$freq), getManufWithMac(mac,manuf), col="red")
}
for(mac in names(topmac[2:n])){
i <- i-1
#par(new = TRUE)
a <- subset(dumpHourCount, MAC == mac)
lines(a$Hour,a$freq,col=i, type="s")
if(mac %in% knownmacs$MAC){
text(subset(a,a$freq == max(a$freq))$Hour, max(a$freq), knownmacs$NAME[knownmacs$MAC == mac], col="green")
}else{
text(subset(a,a$freq == max(a$freq))$Hour, max(a$freq), getManufWithMac(mac,manuf), col="red")
}
}
axis(1, 0:23)

#Fold data by minute and plot by mac
#dev.new()
#dumpMinuteCount <- count(dump, vars = c("MAC", "Minute"))
#n <- 5
#i <- n
#mac <- names(topmac[1])
#a <- subset(dumpMinuteCount, MAC == mac)

#plot(a$Minute,a$freq, xlab="Minute (folded)", ylab="Presence",col=i, type="s",ylim=c(0,max(a$freq)), pch=".", xaxt="n")
#abline(v=seq(0,24*60,60), col=rgb(0.9,0.9,0.9,0.9))
#lines(a$Minute,a$freq,col=i, type="s")
#text(subset(a,a$freq == max(a$freq))$Minute, max(a$freq), mac)
#for(mac in names(topmac[2:n])){
# i <- i-1
# #par(new = TRUE)
# a <- subset(dumpMinuteCount, MAC == mac)
# lines(a$Minute,a$freq,col=i, type="s")
# text(subset(a,a$freq == max(a$freq))$Minute, max(a$freq), mac)
#}
#axis(1, seq(0,24*60,60))

#Calculate some basic stats based on MAC address
macstats <- data.frame(MAC = names(topmac), count = as.numeric(topmac))
macstats$nofssid <- apply(array(names(topmac)),1,countSSIDforMAC, dump = dump)

avgInterval <- c()
sdInterval <- c()
avgdbm <- c()
sddbm <- c()

i <- 1
for(mac in macstats$MAC){
avgInterval[i] <- mean(as.numeric(diff(subset(dump, MAC == mac)$Time)), na.rm=TRUE)
sdInterval[i] <- sd(as.numeric(diff(subset(dump, MAC == mac)$Time)), na.rm=TRUE)
avgdbm[i] <- mean(subset(dump, MAC == mac)$dbm, na.rm=TRUE)
sddbm[i] <- sd(subset(dump, MAC == mac)$dbm, na.rm=TRUE)
i <- i+1
}

macstats$intavg <- avgInterval
macstats$intsd <- sdInterval
macstats$avgdbm <- avgdbm
macstats$sddbm <- sddbm

dev.new()
plot(subset(macstats, select = c("count", "intavg", "intsd", "nofssid")))
dev.new()
plot(subset(macstats, select = c("count", "avgdbm", "sddbm")))

#q <- apply(array(macstats$MAC[macstats$nofssid > 6]),1,getSSIDforMAC, dump = dump)

#Do we know the closest MACs?
macstats$MAC[macstats$avgdbm > -40] %in% knownmacs$MAC
#What are the friendly names?
subset(macstats, avgdbm > -40)$MAC

#Draw presence by average radio power
dev.new()

n <- 20
i <- n
mac <- names(sort(table(dump$MAC), decreasing = TRUE)[1])
a <- subset(dump, MAC == mac)
plot(a$Time,rep(macstats$avgdbm[macstats$MAC == mac],length(a$Time)), xlab="Time", ylab="Presence", ylim = c(-100,0), xlim = as.POSIXct(range(dump$Time)), col=abs(macstats$avgdbm[macstats$MAC == mac]), pch="|" , yaxt="n")
if(mac %in% knownmacs$MAC){
text(a$Time[round(length(a$Time)/2)], macstats$avgdbm[macstats$MAC == mac], knownmacs$NAME[knownmacs$MAC == mac], col="green")
}else{
text(a$Time[round(length(a$Time)/2)], macstats$avgdbm[macstats$MAC == mac], getManufWithMac(mac,manuf), col="red")
}
for(mac in array(names(sort(table(dump$MAC), decreasing = TRUE)[2:n]))){
i <- i-1
#par(new = TRUE)
a <- subset(dump, MAC == mac)
points(a$Time,rep(macstats$avgdbm[macstats$MAC == mac],length(a$Time)), col=abs(macstats$avgdbm[macstats$MAC == mac]), pch="|")
if(mac %in% knownmacs$MAC){
text(a$Time[round(length(a$Time)/2)], macstats$avgdbm[macstats$MAC == mac], knownmacs$NAME[knownmacs$MAC == mac], col="green")
}else{
text(a$Time[round(length(a$Time)/2)], macstats$avgdbm[macstats$MAC == mac], getManufWithMac(mac,manuf), col="red")
}
}
r <- as.POSIXct(seq(from = round(range(dump$Time)[1],"days"),to = round(range(dump$Time)[2],"days"),by=24*60*60)) # round by day so we can draw vertical lines every day
abline(v=r,col="red") # draw vertical lines
