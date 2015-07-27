#prebere vse podatke
ecg_zephyr <- read.csv("C:/Users/Administrator/Desktop/Zephyr/Meritev_14_7/ecg_zephyr.csv", stringsAsFactors =FALSE)

options(digits.secs = 3)

# pravilna pretvorba
ecg_zephyr$ecg <- as.numeric(ecg_zephyr$ecg)
ecg_zephyr$milis <- as.POSIXct(as.numeric(ecg_zephyr$milis)/1000,origin = "1970-01-01", tz = "Europe/Ljubljana")

######################################################
remove(ecg_inter, ecg_a)
#total <- length(ecg_zephyr$ecg)
total <- 1250
premik <- 250
a <-1
b <-750

tekoci_min <- a
tekoci_max <- a
prvi_zvoki <- .POSIXct(character())
drugi_zvoki <- .POSIXct(character())
shrani1 <- numeric()
shrani2 <- numeric()

while (a<total) {
  ecg_inter <- ecg_zephyr$ecg[a:b]
  
  # obravnava NA
  ecg_inter[which(is.na(ecg_inter))] <- mean(ecg_inter, na.rm=TRUE)
  
  # interpolacija  
  if (ecg_inter[1]>700) {
    ecg_inter[1] <- ecg_inter[2]
  }
  if (ecg_inter[length(ecg_inter)]>700) {
    ecg_inter[length(ecg_inter)] <- ecg_inter[length(ecg_inter)-1]
  }
  for (i in 2:length(ecg_inter)-1){
    if (ecg_inter[i]>700) {
      ecg_inter[i] <- (ecg_inter[i-1]+ecg_inter[i+1])/2
    }
  }
  
  ##################################################
  #iskanje nicel=prvih zvokov
  
  #odvod
  ecg_a <- diff(ecg_inter)

  #aproksimacija
  fun_apro <- approxfun(ecg_a)
  
  #odrezemo pragove (zg.in sp.)
  vrhovi <- which(ecg_a >= 10)
  doline <- which(ecg_a <= -10)
  
  # zanka
  nicle <- shrani1
  st_nicel <- 1+sum(diff(vrhovi)>1)
  # diff(vrhovi) izracuna razlike/odvod vrhov, ki so stopnicasta funkcija,
  # vsaka stopnica pa je nov peak plato. Kjer je ta odvod>1, ravno
  # skocimo na nov plato, torej na nov peak v ecg signalu. Prestejemo
  # te preskoke in povecamo za 1, da stejemo tudi prvega.
  
  {if (st_nicel >=1) {
    for (i in 1:st_nicel){
      tekoci_max <- vrhovi[which.max(vrhovi>tekoci_min)]
      tekoci_min <- doline[which.max(doline>tekoci_max)]
      nicle <- c(nicle, round(uniroot(fun_apro, c(tekoci_max, tekoci_min))$root))
    }
  }
  else  
    print("Check vital signs.")}
  
  shrani1 <- nicle[which.max(nicle)]-(b-a)
  
  # predpostavljam ekvidistancnost, gledam razliko indeksov in zaokrozim
  calc <- shrani2
  for (j in 1:length(nicle)-1){
    calc <- c(calc, round((nicle[j+1]-nicle[j])*0.416+nicle[j+1])-1)
  }
  
  shrani2<-numeric()
  
  #ce je izracunani drugi zvok preko konca intervala
  if (max(calc, na.rm=TRUE)>(b-a)){ 
    shrani2 <- calc[which.max(calc)]-(b-a)
  }
  if (a==1){
    calc <- c(round((nicle[2]-nicle[1])*0.416+nicle[1])-1, calc)
  }
  
  plot(ecg_inter, xlim=c(1,(b-a)),xlab="s", ylab="ekg",main="Preciscen signal z dolocenimi zvoki",type='l')
  legend("top", c("prvi zvok","drugi zvok"), pch = c(1,1), , col=c("red","green"))
  points(nicle, rep(550,length(nicle)),col='red')
  points(calc, rep(550,length(calc)), col='green')
  
  print(a)
  print(b)
  readline(prompt="Press [enter] to continue")
  a <- b+1
  b <- min(b+premik,total)
}