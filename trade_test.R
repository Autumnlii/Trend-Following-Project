HO<-read.table("HO-5min.asc",sep=",",header=T,row.names = NULL)
colnames(HO)<-c("Date","Time","Open","High","Low","Close","Volume")
open.price<-HO$Open
close.price<-HO$Close
high.price<-HO$High
low.price<-HO$Low
four_year <- 40000 #???
High<-function(price,start,end){
  high<-max(price[start:end])
  return(high)
}
Low<-function(price,start,end){
  low<-min(price[start:end])
  return(low)
}

which(HO$Date=="04/14/14"&HO$Time=="14:30")
which(HO$Date=="04/14/10"&HO$Time=="10:00")

#nrow(HO)
#tradestrategy(100000,500,0.02,0,40000)
#3month strategy
tradestrategy<-function(equity,tau,stp,start,end){
  peak<-0
  trough<-0
  equity<-100000
  holding.money<-equity 
  Gain <- 0
  Underwater <- 0
  maxDD<-0
  PV<-420
  PV.multi<-100
  position<-0
  entry.price<-0
  price<-0
  slippage<- 47 
  buy_num <- 0
  sell_num <- 0
  tau <- 10600
  stp <- .016
  start <- 392702
  end <- 460520
  numHH <- 0
  equityvec <- c()
  numLL <- 0
  pos.vec <- 0
  
  for(i in (start+tau):(end)){
    if(i <= length(close.price)){
      #print(LL)
      
      if(position==0){
        LL<-Low(low.price,i-tau,i-1)
        HH<-High(high.price,i-tau,i-1)
        
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
 
  return(list(NPWD=NPWD,equity=equity))
}

tradestrategy(100000,10600,0.016,392702+17000,460520)
