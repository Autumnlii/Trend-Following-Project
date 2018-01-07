 require(parallel)
require(data.table)

stock.df <-read.table("JY-5min.txt",sep=",",header=T,row.names = NULL, stringsAsFactors = FALSE)
open.price<-stock.df$Open
close.price<-stock.df$Close
high.price<-stock.df$High
low.price<-stock.df$Low
date<-stock.df$Date
time<-stock.df$Time
four_year <- 40000

# compute.price <- function(i, low.price, high.price, tau){
#   data.frame(HH = max(high.price[(i-tau):(i-1)]),
#              LL = min(low.price[(i-tau):(i-1)]))
# }
# segment.parallel <- function(cl, index, low.price, high.price, tau){
#   cl2 <- parallel::makeCluster(8)
#   segment.price <- parallel::parLapply(cl2, index,
#                              compute.price,
#                              low.price = low.price,
#                              high.price = high.price,
#                              tau = tau)
#   return (rbindlist(segment.price))
# }


compute.price <- function(i, low.price, high.price, tau){
  data.frame(HH = max(high.price[(i-tau):(i-1)]),
             LL = min(low.price[(i-tau):(i-1)]))
}
segment.parallel <- function(index, low.price, high.price, tau){
  segment.price <- lapply(index,
                          compute.price,
                          low.price = low.price,
                          high.price = high.price,
                          tau = tau)
  return (rbindlist(segment.price))
}


tradestrategy <-function(id, high.price, low.price, open.price, close.price,
                         date, time, param){
  start <- param$start[id]
  end <- param$end[id]
  # end <- 40000
  tau <- param$tau[id]
  stp <- param$stp[id]
  peak<-0
  trough<-0
  equity<-100000
  #holding.money<-equity
  Gain <- 0
  Underwater <- 0
  maxDD<-0
  PV<-1250
  PV.multi<-100
  position<-0
  entry.price<-0
  price<-0
  slippage<- 53
  buy_num <- 0
  sell_num <- 0
  #start <- 0
  #end <- 0
  numHH <- 0
  equityvec <- c()
  numLL <- 0
  pos.vec <- 0
  enter.date<-c()
  enter.time<-c()
  exit.time<-c()
  exit.date<-c()

  index <- (start+tau):(end)
  # segment.price <- segment.parallel(cl,index, low.price, high.price, tau = tau)
  segment.price <- segment.parallel(index, low.price, high.price, tau = tau)
  HH.price <- c()
  LL.price <- c()
  HH.price[index] <- segment.price$HH
  LL.price[index] <- segment.price$LL

  for(i in index){
    if(i <= length(close.price)){
      #print(LL)

      if(position==0){
        LL <- LL.price[i]
        HH <- HH.price[i]
        if(open.price[i]>=HH){
          entry.price<-open.price[i]
          position <- 1
          peak<-max(close.price[i],HH)
          equity<- equity+(close.price[i]-entry.price)*PV*PV.multi-slippage/2
        }

        else if(high.price[i]>=HH){
          entry.price<-HH
          position <- 1
          peak<-max(close.price[i],HH)
          equity<- equity+(close.price[i]-entry.price)*PV*PV.multi-slippage/2

        }
        else if(open.price[i]<LL){
          entry.price<-open.price[i]
          position <- -1
          trough <- min(close.price[i],LL)
          equity<-equity-(close.price[i]-entry.price)*PV*PV.multi-slippage/2

        }
        else if(low.price[i]<LL){
          entry.price<-LL
          position <- -1
          trough <- min(close.price[i],LL)
          equity<-equity-(close.price[i]-entry.price)*PV*PV.multi-slippage/2
        }
        else{
          position<-0
        }
        if(position!=0){
          enter.date <-c(enter.date, date[i])
          enter.time<-c(enter.time, time[i])
        }
        pos.vec<-c(pos.vec,position)

      }

      else if(position==1){
        if(open.price[i] <= peak*(1-stp)){ #check
          price<-open.price[i]
          position <- 0
          equity<-equity+(price-close.price[i-1])*PV*PV.multi-slippage/2
          buy_num=buy_num+1
        }
        else if(low.price[i] <= peak*(1-stp)){
          price<-peak*(1-stp)
          position <- 0
          equity<-equity+(price-close.price[i-1])*PV*PV.multi-slippage/2
          buy_num=buy_num+1
        }else{
          position<-1
          peak<-max(peak, close.price[i])
          equity<-equity+(close.price[i]-close.price[i-1])*PV*PV.multi
        }
        if(position==0){
          exit.date <-c(exit.date, date[i])
          exit.time <-c(exit.time, time[i])
        }
        #break;
      }

      else {
        #sell_num=sell_num+1

        #print(equity)
        if(open.price[i] >= trough*(1+stp)){
          sell_num=sell_num+1
          price<-open.price[i]
          position<-0
          equity<-equity+(close.price[i-1]-price)*PV*PV.multi-slippage/2
        }
        else if(high.price[i] >= trough*(1+stp)){
          sell_num=sell_num+1
          price<-trough*(1+stp)
          position<-0
          equity<-equity+(close.price[i-1]-price)*PV*PV.multi-slippage/2
        }
        else{
          position=-1
          trough<-min(trough, close.price[i])
          equity<-equity+(close.price[i-1]-close.price[i])*PV*PV.multi
        }
        if(position==0){
          exit.date <-c(exit.date, date[i])
          exit.time <-c(exit.time, time[i])
        }
      }
      equityvec <- c(equityvec,equity)
    }

    Gain <- max(Gain,equity-100000)
    Underwater <- equity - Gain - 100000
    maxDD <- min(maxDD, Underwater)
  }


  if(maxDD==0){
    NPWD<-0
  }else{NPWD<-(-1)*(equity-100000)/maxDD}
  print(buy_num)
  print(sell_num)

  return(list(NPWD=NPWD,equity=equity,stp=stp,enter.date=enter.date,
              enter.time=enter.time,exit.date=exit.date,exit.time=exit.time))
}

#optimization
insample<-40000
outsample <- 2500 #3 months
start <- 0
length_data <- length(low.price)
counter <- 0
equity<-100000

#optimization
start = seq(0, length_data, outsample)
stp = seq(.005,.1,.05)
tau = seq(500,10000,5000)

# test param
#start = 0
#stp = 0.016
#tau = 10600


param <- CJ(start = start,
            tau =  tau,
            stp = stp)
param$end <- param$start + insample
param <- rbind(param, param)

################ demo test ###############
id <- 1
system.time({
  r <- tradestrategy(id, high.price, low.price, open.price,
                     close.price, date, time,param)
})
################ optimal parallel #################

optim.parallel <- function(cl, index, high.price, low.price, open.price, close.price, date, time, param){
  compute.price <- function(i, low.price, high.price, tau){
    data.frame(HH = max(high.price[(i-tau):(i-1)]),
               LL = min(low.price[(i-tau):(i-1)]))
  }
  segment.parallel <- function(cl, index, low.price, high.price, tau){
    segment.price <- parLapply(cl, index,
                               compute.price,
                               low.price = low.price,
                               high.price = high.price,
                               tau = tau)
    return (rbindlist(segment.price))
  }
  optimal.result <- parLapply(cl,index,
                              tradestrategy,
                              high.price = high.price,
                              low.price = low.price,
                              open.price = open.price,
                              close.price = close.price,
                              date = date,
                              time = time,
                              param = param
  )
  return (optimal.result)
}
cl <- makeCluster(8)
index <- 1:nrow(param)
clusterExport(cl, "high.price")
clusterExport(cl, "low.price")
clusterExport(cl, "open.price")
clusterExport(cl, "close.price")
clusterExport(cl, "date")
clusterExport(cl, "time")
clusterExport(cl, "compute.price")
clusterExport(cl, "segment.parallel")
clusterExport(cl, "rbindlist")

system.time({
  result <- optim.parallel(cl,index, high.price, low.price, open.price, close.price, date, time, param)
})


best <- -100000
best_tau <- 0
best_stppct <- 0
for(i in length(result)){
  optim.r <- result[i]
  NPWD <- optim.r[[1]]$NPWD
  if (best < NPWD) {
    best <- NPWD
    best_stppct = param$stp[i]
    best_tau = param$tau[i]
  }
  print(best_stppct)
  print(best_tau)
 
}
end <- max(param$end)
if(end+2500<length_data){
  best.trade <- tradestrategy(1, high.price, low.price, open.price,
                close.price, date, time,
                data.frame(start = end, tau = best_tau, stp = stp,end = end+2500))
  NPWD <- best.trade$NPWD
  cat("NPWD: ", NPWD,"\n")
  equity <- best.trade$equity
  cat("equity: ", equity,"\n")
} else {
  cat("trade stoped\n")
  break
}
