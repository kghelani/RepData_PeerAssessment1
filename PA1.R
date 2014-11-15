library(dplyr)
library(scales)
library(ggplot2)
library(lattice)
getsteps <- function(x)
{
  if (is.na(x["steps"]))
  {
    x["steps"] <- isteps[isteps$tinterval==x["tinterval"],]$median
  }
  x
}

require(plyr)
function plot_daily_acitivity (df, tinterval, colors)
{
  #Daily Activity Pattern
  arglist <- list(data = data, tinterval = as.quoted(tinterval))
  isteps <- substitute(ddply(.data = df, .variables=tinterval, function(x) c(mean=mean(x$steps, na.rm=TRUE),median=median(x$steps, na.rm=TRUE))), arglist)
  #isteps <-ddply(.data = df, .variables=as.quoted(tinterval), function(x) c(mean=mean(x$steps, na.rm=TRUE),median=median(x$steps, na.rm=TRUE)))
  
  plot(factor(isteps$tinterval),isteps$mean, type="n", main="Mean/Median steps taken per day", ylab="Steps per day")
  lines(factor(isteps$tinterval),isteps$mean,col=colors[1], pch = 19)
  lines(factor(isteps$tinterval),isteps$median,col=colors[2],lwd=1.5, pch=5)
  legend("topleft", c("Mean steps", "Median steps"), lty=c(1,1), col=colors, bty="n")
 # dates <- paste(substr(df$tinterval,1,2),substr(df$tinterval, 3, 4),sep=":")
 # xtime <- strptime(dates, format="%H:%M")
  #r <- as.POSIXct(round(range(xtime), "hours"))
  #axis.POSIXct(1, at = seq(r[1], r[2], by = "hour"), format = "%H")
}

function plot_hist_steps(df, color)
{

  
  stepseachday <- ddply(.data = df, .variables=as.quoted("date"), function(x) c(tsteps=sum(x$steps, na.rm=TRUE)))
  hist(stepseachday$tsteps,col=color, main="Histogrram of steps taken each day", xlab="Steps taken each day")
  
}


dataset <- tbl_df(read.csv("activity.csv"))
dataset$tinterval <- sapply(dataset$interval, FUN <- function(x) ifelse (nchar(x)<4, paste(c(rep("0", 4-nchar(x)),x), collapse="", sep=""),x))

#plot histogram of steps taken each day
#plot_hist_steps(dataset, "red")
stepseachday <- ddply(.data = dataset, 'date', function(x) c(tsteps=sum(x$steps, na.rm=TRUE)))
hist(stepseachday$tsteps,col=rgb(1,0,1,1/4), main="Histogrram of steps taken each day", xlab="Steps taken each day")


#Daily Activity pattern
#plot_daily_activity(dataset, c("red", "blue"))
par(lwd=2)
isteps <-ddply(dataset, 'tinterval', function(x) c(mean=mean(x$steps, na.rm=TRUE),median=median(x$steps, na.rm=TRUE)))
plot(factor(isteps$tinterval),isteps$mean, xlab ="Time of day", xaxt="n", type="n", main="Mean/Median steps taken each day by Time of day", ylab="Steps each day")
lines(factor(isteps$tinterval),isteps$mean,col="red", pch = 19)
lines(factor(isteps$tinterval),isteps$median,col="blue",lwd=1.5, pch=5)
legend("topleft", c("Mean steps", "Median steps"), lty=c(1,1), col=c("red","blue"), bty="n")
# dates <- paste(substr(df$tinterval,1,2),substr(df$tinterval, 3, 4),sep=":")
#axis(1, at=seq(r[1],r[2],by="hour"), labels=format(seq(r[1],r[2],"hours"), format="%H"))
axis(1, at=seq(1, length(levels(factor(isteps$tinterval))),12), labels=format(seq(0,23,1), format="%2s"))




#Max steps
msteps <- isteps[(isteps$mean == max(isteps$mean)),]
msteps[,"interval"]

nasteps <- is.na(dataset$steps)
naset <- dataset[nasteps,]
nrow(naset)

#Imputing data
dd<- apply(dataset, 1, getsteps)
dataset2 <- data.frame(t(dd))

#plot_hist_steps(dataset2, "blue")
#plot_daily_activity(daaset2, c("green", "orange"))
par(mfrow=c(1,2))
stepseachday2 <- ddply(.data = dataset, 'date', function(x) c(tsteps=sum(x$steps, na.rm=TRUE)))
hist(stepseachday$tsteps,col=rgb(1,0,1,1/4), main="Histogrram of steps taken each day", xlab="Steps taken each day")
hist(stepseachday2$tsteps,col=rgb(0,0,1,1/4), main="Histogrram of steps taken each day (Imputed dataset)", xlab="Steps taken each day")

par(mfrow=c(1,1))
isteps2 <-ddply(dataset2, 'tinterval', function(x) c(mean=mean(as.numeric(x$steps)),median=median(as.numeric(x$steps))))
plot(factor(isteps2$tinterval),xaxt="n", isteps2$mean, type="n", main="Mean/Median steps taken each day by Time of day", ylab="Steps each day")
lines(factor(isteps2$tinterval),isteps2$mean,col="green", lwd=1)
lines(factor(isteps2$tinterval),isteps2$median,col="brown",lwd=1)
lines(factor(isteps$tinterval),isteps$mean,col="red", lwd=1, type ="s")
lines(factor(isteps$tinterval),isteps$median,col="blue", lwd=1)
legend("topright", c("Mean steps(imputed)", "Median steps(imputed)", "Mean (original)", "Median(original)"), lty=c(1,1,1,1), col=c("green", "brown", "red", "blue"), bty="n")
axis(1, at=seq(1, length(levels(factor(isteps$tinterval))),12), labels=format(seq(0,23,1), format="%2s"))


#Add weekday or weekend
dataset2$day <- as.factor(ifelse((wday(strptime(dataset2$date, format="%Y-%m-%d")) %in% 2:6), "weekday", "weekend"))
isteps3 <- ddply(dataset2, .(tinterval, day), summarize, mean=mean(as.numeric(steps)))
xyplot(mean ~ tinterval | day, data = isteps3, layout = c(1, 2), 
       scales = list(
         x=list(
           at=seq(1,length(factor(isteps$tinterval)),12)
           , 
         labels=sprintf(as.character(seq(0,23,1)),"%S")
         )), 
         xlab = "Time of day")

