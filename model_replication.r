cal <- function(y) {
  run<-function(x_fp, dun, Ts, Td, Rs, Rd, cb, bw, pr, TTo,TTa){
    #detect time of single-failure
    MTTDs = Ts
    #detect time of two-failure
    MTTDd = Td
    
    #repair time 
    MTTRs = Rs*(1/bw)
    MTTRd = Rd*(1/bw)
    
    rne = length(x_fp)
    
    x_f = vector(length = rne)
    
    #ratio of concurrent failures
    if(pr > 0){
      a=0
      h=0
      u1 = runif(rne)
      u2 = runif(rne)
      x_f[1] = x[1]
      for(i in 2:rne){
        if(u1[i] > (1-pr/(200))) {
          #generate correlated failures
          a = 1200*u2[i]
          h = h + 1
        }
        else{
          h = 0
          a = 0
        }
        x_f[i] = min(x_fp[i-h] + a, x_fp[i])
      }
    }else{
      x_f<-sort(x_fp)
    }
    
    
    #sort
    x_f<-sort(x_f)
    x_dso<- ceiling((x_f+MTTDs)/TTo)*TTo
    
    x_ddo <- ceiling((x_f+MTTDd)/TTo)*TTo
    x_dda <- ceiling((x_f+MTTDd)/TTa)*TTa
    
    x_ds<-x_dso
    x_dd<-x_ddo
    
    x_end <- (x_f + dun)
    
    #repair end time
    x_reg <- x_end
    
    x_neg<-x_ds
    x_nsg<-x_ds
    x_redg<-x_dd
    
    #global recovery end
    x_repairg = 0
    total_ng = 0
    
    #data unavailability time
    data_ut = 0
    
    #data loss count
    data_lc = 0
    
    comblock = 0 
    
    tmpv = 0
    
    #repair single failure
    for(i in 1:rne)
    {
      if(dun[i] > 0){
        if(x_end[i] > x_ds[i]) {
          tmpv = max(x_repairg, x_ds[i])
          x_reg[i] = min(MTTRs + tmpv, x_end[i])
          if(tmpv < x_end[i]) {
            total_ng = total_ng + max(x_reg[i] - tmpv, 0)
            x_repairg = max(x_repairg, x_reg[i]) 
            x_nsg[i] = tmpv
            x_neg[i] = x_repairg
          }
        }
      }
      else
      {
        tmpv = max(x_repairg, x_ds[i])
        x_reg[i] = MTTRs + tmpv
        x_repairg = x_reg[i]
        total_ng = total_ng + MTTRs
        x_nsg[i] = tmpv
        x_neg[i] = x_repairg
      }
      
      j = i+1
      #adaptive check intervals
      while(j <= rne){
        if(x_dda[j] <= x_reg[i]){
          x_dd[j] = x_dda[j] 
          x_ds[j] = x_dsa[j]
          j = j+1
        }else{
          break
        }
      }
    }
    
    #number of checks
    numtc = ceiling(x_reg[rne]/TTa)
    numc = ceiling(x_reg[rne]/TTo) 
    x_regc <- ceiling((x_reg)/TTa)*TTa
        
        i = 1
        j = 1
        if(1){
            while(j<=rne && i<= numtc){
                if(x_dd[j] <= i*TTa){
                    if((i*TTa) <= x_ds[j] && (i*TTa) <= x_regc[j]){
                        if((i*TTa/TTo)*TTo != i*TTa){
                            numc = numc + 1
                        }
                        i = i + 1
                    }else{
                        j = j + 1
                    }
                } else if (x_dd[j] > (i*TTa)){
                    i=i+1
                } 
                
            }
        }
    
    #computational cost
    cost_com = 0
    for(i in 1:rne) {
      if(x_reg[i] >= x_dd[i])
        cost_com = cost_com + 1
    }
    
    #exra repair cost
    costrs = 0
    costc = 0
    if(1) {
    for(i in 1:(rne-1)) {
      if(x_reg[i] < x_dd[i])
        next
      
      for(j in (i+1):rne) {
        if(x[j] >= x_reg[i])
          break
        
        if(x_reg[j] < x_dd[j])
          next
        
        #been detected & concurrent
        if(x_reg[j] >= x_dd[i]) {
          costc = costc + 1
          if((x_end[i] <= x_ds[i] && dun[i] != 0) || (x_end[j] <= x_ds[j] && dun[j] !=0) )
            costrs = costrs + MTTRd
        }
      }
    }
    }
    
    #data loss 
    if(1){
      for(i in 1:(rne - 2)) {
        if(dun[i]!=0)
          next
        
        for(j in (i+1):(rne-1)){
          #beyond end of x_f[i]
          if(x_f[j] > x_reg[i])
            break
          if(dun[j]!=0)
            next
          
          
          for(q in (j+1):rne){
            #beyond end of x_f[i]
            if(x_f[q] > x_reg[i])
              break
            
            if(x_f[q] > x_reg[j])
              break
            
            if(dun[q]!=0)
              next
            
            #triple happens
            if(x_f[q] <= x_reg[i] && x_f[q] <= x_reg[j]) {
              if(x_f[q] <= x_dd[j] + MTTRd){
                tmpv = min(x_reg[i], x_reg[j]) - x_f[q]
                if(x_f[q] <= x_nsg[i])
                  comblock = 1
                else
                  comblock = max(1-(x_f[q] - x_nsg[i])/MTTRs, 0)
                data_ut = data_ut + tmpv*comblock
                if(dun[i]==0 && dun[j]==0 && dun[q]==0 ){
                  data_lc = data_lc + comblock
                }
              }
            }
          }
        }
      } 
    }
    
    #output
    result<-c(data_lc*cb, total_ng*bw ,costrs*bw*0.25, cost_com, numc)
    return (result)
  }
  
  runall<-function(x_f, dun, Ts, Rs, Rd, cb, slow){
    result1<-run(x_f, dun, Ts, Ts, Rs, Rd, cb, 1,0, 3000, 3000)
    
    result2<-run(x_f, dun, Ts, 1*30*10, Rs, Rd, cb, 1,0, 300, 300)
    
    result3<-run(x_f, dun, Ts, 1*30*10, Rs, Rd, cb, 1,0, 600, 600)
    
    result4<-run(x_f, dun, Ts, 1*30*10, Rs, Rd, cb, 1,0, 1200, 1200)
    
    result5<-run(x_f, dun, Ts, 1*30*10, Rs, Rd, cb, 1,0, 2400, 2400)
    
    result6<-run(x_f, dun, Ts, 1*30*10, Rs, Rd, cb, 1,0, 3000, 300)
    
    result7<-run(x_f, dun, Ts, 1*30*10, Rs, Rd, cb, 1,0, 3000, 600)
    
    result8<-run(x_f, dun, Ts, 1*30*10, Rs, Rd, cb, 1,0, 3000, 1200)
    
    result9<-run(x_f, dun, Ts, 1*30*10, Rs, Rd, cb, 1,0, 3000, 2400)
    
    result10<-run(x_f, dun, Ts, 1*30*10, Rs, Rd, cb, 1,0, 3000, 3000)
    result11<-run(x_f, dun, Ts, 2*30*10, Rs, Rd, cb, 1,0, 3000, 3000)
    result12<-run(x_f, dun, Ts, 4*30*10, Rs, Rd, cb, 1,0, 3000, 3000)
    result13<-run(x_f, dun, Ts, 8*30*10, Rs, Rd, cb, 1,0, 3000, 3000)
    
    result<-c(result1, result2, result3, result4, result5, result6, result7, result8, result9, result10, result11, result12, result13)
    return(result)
  }
  
  
  #number of nodes
  m = 1000
  
  #max failure time
  MFT = 5*365*24*3600*10
  #mean time to failure
  MTTF = 4.3*30*24*3600*10
  #mean duration of unavailability time
  MTUA = 50*600
  
  #number of events
  ne = ceiling(m*(MFT/MTTF)*(1/0.0548)*1.1)
  #real number of events
  rne = 0
  
  #time between failure
  tf = vector(length = ne)
  #time to failure
  x = vector(length = ne)
  
  #generate time between failure
  tf = rexp(ne,m*(1/0.0548)/MTTF)
  
  #generate time to failure
  x[1] = tf[1]
  tft = x[1]
  rne = 1
  for(rne in 2:ne) {
    if(tft < MFT){
      x[rne] = x[rne-1] + tf[rne]
      tft = x[rne]
    }
    else{
      rne = rne -1
      break
    }
  }
  
  #time to failure
  x_fp = vector(length = rne)
  #duration of unavailability
  dun = vector(length = rne)
  
  for(i in 1:rne){
    x_fp[i] = x[i]
  }
  
  #weibull
  #50% < 1min, 94%<15min
  tdun = rweibull(rne,0.540169,112.9594)*10
  u2 = runif(rne)
  for(f in 1:rne) {
    if(u2[f] > (1-0.0548*MTTF/MFT))
      dun[f] = 0
    else 
      dun[f]=tdun[f]
  } 
  
  #total data in a node
  totaldata = 16*1024*1024
  #network per node,10MB/s
  bandwidth = 10
  #repair time of single failure
  MTTRS = 10*totaldata/(bandwidth*m)
  MTTRD = 10*128/bandwidth
  MTTRT = 10*128/bandwidth
  
  #output
  
  #result1<-run(x_fp, dun, 50*60*10+300, 30*10, 30*10, MTTRS*(34/9), MTTRD*(46/9), MTTRT*6, 0.042253, 1, 10, 1)
  #result2<-run(x_fp, dun, 50*60*10+300, 30*10, 30*10, MTTRS*(34/9), MTTRD*(46/9), MTTRT*6, 0.042253, 1, 20, 1)
  #result3<-run(x_fp, dun, 50*60*10+300, 30*10, 30*10, MTTRS*(34/9), MTTRD*(46/9), MTTRT*6, 0.042253, 1, 25, 1)
  
  #6+3
  result1<-runall(x_fp, dun, 15*60*10+300, MTTRS, MTTRD, 0.25075, 1)
  #zigzag
  #result2<-runall(x_fp, dun, 15*60*10+300, MTTRS*(34/9), MTTRD*(46/9), MTTRT*6, 0.042253, 1)
  #lrc 6,2,2
  #result3<-runall(x_f, dun, 30*60*10+300, MTTRS*3.6, MTTRD*4.4, MTTRT*6, 0.06338*0.14, 1)
  #lrc 12,2,2
  #result4<-runall(x_fp, dun, 15*60*10+300, MTTRS*6.75, MTTRD*7.65*1.86, MTTRT*12, 0.3433*0.138, 1)
  #9+3
  #result2<-runall(x_fp, dun, 15*60*10+300, MTTRS*9, MTTRD*9*1.376, MTTRT*9, 0.1244956, 1)
  result<-c(result1)
  return (result)
}

m = 1000
#chunk 16TB/per node
#128MB/per chunk
block = 125000

#for rs
datanum = 3
paritynum = 0
allnum = datanum + paritynum 
common11brs = choose(m-1, allnum-1)*m*block/(choose(m,allnum)*allnum)
common22brs = choose(m-2, allnum-2)*m*block/(choose(m,allnum)*allnum)
common33brs = choose(m-3, allnum-3)*m*block/(choose(m,allnum)*allnum)

#test <- c(0,25,50,75,100,125,150,175)
MTTF = 4.6*30*24*3600*10
#max failure time
MFT = 5*365*24*3600*10

#number of tests
N = 160
NC = 50


#code number
NT = 1

#count of our method plus origin method
NL = 13

#number of output
NR = 5

#result for graph
res_a <- vector(length = NC*NL*NT)
res_b <- vector(length = NC*NL*NT)
res_c <- vector(length = NC*NL*NT)
res_d <- vector(length = NC*NL*NT)


ptm <- proc.time()
proc.time() - ptm
print(ptm)

print("start")


cl.cores <- detectCores()
print(cl.cores)
cl <- makeCluster(cl.cores)

for(j in 1:NC) {
  ptm <- proc.time()
  proc.time() - ptm
  print(ptm)
  print(j)
  print("begin")
  #parax = list()
  #for(i in 1:N) 
  #  parax[[i]] <- c(test[j])
  
  res <- parLapply(cl,1:N,cal)
  
  #print(tt)
  for(i in 1:N) {
    for(k in 1:NT) {
      for (l in 1:NL) {
        res_a[(j-1)*NT*NL + (k-1)*NL + l] = res_a[(j-1)*NT*NL + (k-1)*NL + l] + res[[i]][(k-1)*NL*NR + (l-1)*NR + 1]
        res_b[(j-1)*NT*NL + (k-1)*NL + l] = res_b[(j-1)*NT*NL + (k-1)*NL + l] + res[[i]][(k-1)*NL*NR + (l-1)*NR +2]
        res_c[(j-1)*NT*NL + (k-1)*NL + l] = res_c[(j-1)*NT*NL + (k-1)*NL + l] + res[[i]][(k-1)*NL*NR + (l-1)*NR +3]
        res_d[(j-1)*NT*NL + (k-1)*NL + l] = res_d[(j-1)*NT*NL + (k-1)*NL + l] + res[[i]][(k-1)*NL*NR + (l-1)*NR + 4]
        res_e[(j-1)*NT*NL + (k-1)*NL + l] = res_e[(j-1)*NT*NL + (k-1)*NL + l] + res[[i]][(k-1)*NL*NR + (l-1)*NR + 5]
      }
    }
  }
  
  if(j < NC) {
    for(k in 1:NT) { 
      for (l in 1:NL) {
        res_a[j*NT*NL + (k-1)*NL + l] = res_a[(j-1)*NT*NL + (k-1)*NL + l] 
        res_b[j*NT*NL + (k-1)*NL + l] = res_b[(j-1)*NT*NL + (k-1)*NL + l] 
        res_c[j*NT*NL + (k-1)*NL + l] = res_c[(j-1)*NT*NL + (k-1)*NL + l] 
        res_d[j*NT*NL + (k-1)*NL + l] = res_d[(j-1)*NT*NL + (k-1)*NL + l] 
        res_e[j*NT*NL + (k-1)*NL + l] = res_d[(j-1)*NT*NL + (k-1)*NL + l]
      }
    }
  }
  
  print("reliability")
  for(k in 1:NT) {
    for (l in 1:NL) {
      if(1 >0){
        print(res_a[(j-1)*NT*NL + (k-1)*NL + l])
      }
    }
  }
  
  print("ERNT")
  for(k in 1:NT) {
    for (l in 1:NL) {
      if(1 >0){
        print((res_c[(j-1)*NT*NL + (k-1)*NL + l]/res_b[(j-1)*NT*NL + (k-1)*NL + l]))
      }
    }
  }
  
  print("NOCA")
  for(k in 1:NT) {
    for (l in 1:NL) {
      if(1 >0){
        print(res_d[(j-1)*NT*NL + (k-1)*NL + l])
      }
    }
  }
  
  print("NOCH")
  for(k in 1:NT) {
    for (l in 1:NL) {
      if(1 >0){
        print(res_e[(j-1)*NT*NL + (k-1)*NL + l])
      }
    }
  }
}

ptm <- proc.time()
proc.time() - ptm
print(ptm)

stopCluster(cl)

print("end")
