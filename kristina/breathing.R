ecg_breathing <- read.csv("C:/Users/Administrator/Desktop/Zephyr/Meritev_14_7/breathing_zephyr.csv", stringsAsFactors =FALSE)

options(digits.secs = 3)

# pravilna pretvorba
ecg_breathing$breathing <- as.numeric(ecg_breathing$breathing)
ecg_breathing$milis <- as.POSIXct(as.numeric(ecg_breathing$milis)/1000,origin = "1970-01-01", tz = "Europe/Ljubljana")

ecg_breath <- ecg_breathing$breathing[400:1500]
plot(ecg_breath, main="Neobdelan signal", xlab="s", ylab="dihanje")

# interpolacija  
if (ecg_breath[1]>900) {
  ecg_breath[1] <- ecg_breath[2]
}
if (ecg_breath[length(ecg_breath)]>900) {
  ecg_breath[length(ecg_breath)] <- ecg_breath[length(ecg_breath)-1]
}
for (i in 2:length(ecg_breath)-1){
  if (ecg_breath[i]>900) {
    ecg_breath[i] <- (ecg_breath[i-1]+ecg_breath[i+1])/2
  }
}
plot(ecg_breath, main="Preciscen signal", xlab="s", ylab="dihanje")

###########
h<-length(ecg_breath)
z<-numeric(h)
r<-numeric(h)
m<-numeric(h)

for (i in 1:h){
   if (diff(ecg_breath)[i]>0){
     z[i]<-1
   } 
  if (diff(ecg_breath)[i]<(0)){
    r[i]<-1
  } 
  if (diff(ecg_breath)[i]==0){
    m[i]<-1
  } 
  print(c(z[i], r[i], m[i]))
}

col<-numeric()
for (i in c(1,2,3,h-2,h-1,h)){
  col[i] <- which.max(c(sum(z[i]),sum(r[i]),sum(m[i])))
}

for (i in 4:(h-3)){
  col[i] <- which.max(c(sum(z[(i-2):(i+2)]),sum(r[(i-2):(i+2)]),sum(m[(i-2):(i+2)])))
}
cbind(z,r,m,col)

#primerjava
plot(which(diff(ecg_breath)>0),ecg_breath[which(diff(ecg_breath)>0)], col='green')
points(which(diff(ecg_breath)==0),ecg_breath[which(diff(ecg_breath)==0)], col='blue')
points(which(diff(ecg_breath)<0),ecg_breath[which(diff(ecg_breath)<0)], col='red')

#predelano
plot(which(col==1),ecg_breath[which(col==1)], xlim=c(1,h),col='green',main="Analiziran signal", xlab="s", ylab="dihanje")
points(which(col==3),ecg_breath[which(col==3)], col='blue')
points(which(col==2),ecg_breath[which(col==2)], col='red')
legend("topright", ncol=3, fill=c("red","green","blue"), c("vdih","izdih","idle"))
