cal <- function(y) {
  run<-function(x_f, dun, Tt, Sg, Dg, Tg, Ra, p){
    #detect time of baseline
    MTTDg = Tt
    
    #repair time 
    MTTRSg = Sg
    MTTRDg = Dg
    MTTRTg = Tg
    
    #detect time of single-failure
    MTTDs = 50*60*10
    #detect time of two-failure
    MTTDd = 1*600
    #detect time of three-failure
    MTTDt = 1*600
    
    #repair time of failed node
    MTTRSa = MTTRSg 
    MTTRSb = MTTRSg*2 
    MTTRSc = MTTRSg*5
    
    #repair time of rs block,128MB/per block
    MTTRDa = MTTRDg
    MTTRDb = MTTRDg*2
    MTTRDc = MTTRDg*5
    
    MTTRTa = MTTRTg
    MTTRTb = MTTRTg*2
    MTTRTc = MTTRTg*5
    
    #sort
    x_d <- ceiling(x_f/3000)*3000
    
    x_dg <- ceiling((x_f+MTTDg)/3000)*3000
    
    x_ds<- ceiling((x_f+MTTDs)/3000)*3000
    x_dd <- ceiling((x_f+MTTDd)/3000)*3000
    x_dt <- ceiling((x_f+MTTDt)/3000)*3000
    
    x_end <- (x_f + dun)
    
    
    #repair end time
    x_reg <- x_end
    x_rea <- x_end
    x_reb <- x_end
    x_rec <- x_end
    
    x_neg<-x_dg
    x_nea<-x_ds
    x_neb<-x_ds
    x_nec<-x_ds
    
    x_nsg<-x_dg
    x_nsa<-x_ds
    x_nsb<-x_ds
    x_nsc<-x_ds
    
    x_redg <- x_dg
    x_reda <- x_dd
    x_redb <- x_dd 
    x_redc <- x_dd 
    
    #global recovery end
    x_repairg = 0
    x_repaira = 0
    x_repairb = 0
    x_repairc = 0
    
    #total network
    total_ng = 0
    total_na = 0
    total_nb = 0
    total_nc = 0
    
    tmpv = 0
    for(i in 1:rne)
    {
      if(dun[i] > 0){
        if(x_end[i] > x_dg[i]) {
          tmpv = max(x_repairg, x_dg[i])
          x_reg[i] = min(MTTRSg + tmpv, x_end[i])
          if(tmpv < x_end[i]) {
            total_ng = total_ng + max(x_reg[i] - tmpv, 0)
            x_repairg = max(x_repairg, x_reg[i]) 
            x_nsg[i] = tmpv
            x_neg[i] = x_repairg
          }
        }
        
        if(x_end[i] > x_ds[i]) {
          tmpv = max(x_repaira, x_ds[i])
          x_rea[i] = min(MTTRSa + tmpv, x_end[i])
          if(tmpv < x_end[i]) {
            total_na = total_na + max(x_rea[i] - tmpv, 0)
            x_repaira = max(x_repaira, x_rea[i]) 
            x_nsa[i] = tmpv
            x_nea[i] = x_repaira
          }
          
          tmpv = max(x_repairb, x_ds[i])
          x_reb[i] = min(MTTRSb + tmpv, x_end[i])
          if(tmpv < x_end[i]) {
            total_nb = total_nb + max(x_reb[i] - tmpv, 0)
            x_repairb = max(x_repairb, x_reb[i]) 
            x_nsb[i] = tmpv
            x_neb[i] = x_repairb
          }
          
          tmpv = max(x_repairc, x_ds[i])
          x_rec[i] = min(MTTRSc + tmpv, x_end[i])
          if(tmpv < x_end[i]) {
            total_nc = total_nc + max(x_rec[i] - tmpv, 0)
            x_repairc = max(x_repairc, x_rec[i]) 
            x_nsc[i] = tmpv
            x_nec[i] = x_repairc
          }
        }
      }
      else
      {
        tmpv = max(x_repairg, x_dg[i])
        x_reg[i] = MTTRSg + tmpv
        x_repairg = x_reg[i]
        total_ng = total_ng + MTTRS
        x_nsg[i] = tmpv
        x_neg[i] = x_repairg
        
        tmpv = max(x_repaira, x_ds[i])
        x_rea[i] = MTTRSa + tmpv
        x_repaira = x_rea[i]
        total_na = total_na + MTTRSa
        x_nsa[i] = tmpv
        x_nea[i] = x_repaira
        
        tmpv = max(x_repairb, x_ds[i]) 
        x_reb[i] = MTTRSb + tmpv
        x_repairb = x_reb[i]
        total_nb = total_nb + MTTRSb
        x_nsb[i] = tmpv
        x_neb[i] = x_repairb
        
        tmpv = max(x_repairc, x_ds[i])
        x_rec[i] = MTTRSc + tmpv 
        x_repairc = x_rec[i]
        total_nc = total_nc + MTTRSc
        x_nsc[i] = tmpv
        x_nec[i] = x_repairc
      }
    }
    
    if(p){
      k = 2
      for(i in 1:(rne - 1)) {
        for(j in (i+1):rne){
          if(x_dg[j] < x_reg[i]) {
            x_redg[i] = x_redg[i] + MTTRDg
            x_redg[j] = x_redg[j] + MTTRDg
            total_ng = total_ng + MTTRDg
            while(k < rne && x_nsg[k] < x_redg[j]) {
              k = k + 1
            }
            if(x_nsg[k-1] != x_neg[k-1] && x_neg[k-1] > x_dg[j] && x_nsg[k-1] < x_redg[j] ) {
              l = k
              p = k-1
              while(l <= rne) {
                if(x_nsg[l] == x_neg[l]||p == l){
                  l = l + 1
                  next
                }
                
                if(x_nsg[l] < x_neg[p] + MTTRDg) {
                  if(dun[p] == 0)
                    x_reg[p] = x_reg[p] + MTTRDg
                  else
                    x_reg[p] = min(x_reg[p] + MTTRDg, x_end[p])
                  
                  x_nsg[l] = x_reg[p] + MTTRDg
                  p = l
                } else {
                  break
                }
              }
              
            }
          }
          else
            break
        }
      }
      
      k = 2
      for(i in 1:(rne - 1)) {
        for(j in (i+1):rne){
          if(x_dd[j] < x_rea[i]) {
            x_reda[i] = x_reda[i] + MTTRDa
            x_reda[j] = x_reda[j] + MTTRDa
            total_na = total_na + MTTRDa
            while(k < rne && x_nsa[k] < x_reda[j]) {
              k = k + 1
            }
            if(x_nsa[k-1] != x_nea[k-1] && x_nea[k-1] > x_dd[j] && x_nsa[k-1] < x_reda[j]) {
              l = k
              p = k-1
              while(l <= rne) {
                if(x_nsa[l] == x_nea[l]){
                  l = l + 1
                  next
                }
                if(x_nsa[l] < x_nea[p] + MTTRDa) {
                  if(dun[p] == 0||p == l)
                    x_rea[p] = x_rea[p] + MTTRDa
                  else
                    x_rea[p] = min(x_rea[p] + MTTRDa, x_end[p])
                  
                  x_nsa[l] = x_rea[p] + MTTRDa
                  p = l
                } else {
                  break
                }
              }
            }
          }
          else
            break
        }
      }
      
      k = 2
      for(i in 1:(rne - 1)) {
        for(j in (i+1):rne){
          if(x_dd[j] < x_reb[i]) {
            x_redb[i] = x_redb[i] + MTTRDb
            x_redb[j] = x_redb[j] + MTTRDb
            total_nb = total_nb + MTTRDb
            while(k < rne && x_nsb[k] < x_redb[j]) {
              k = k + 1
            }
            if(x_nsb[k-1] != x_neb[k-1] && x_neb[k-1] > x_dd[j] && x_nsb[k-1] < x_redb[j]) {
              l = k
              p = k-1
              while(l <= rne) {
                if(x_nsb[l] == x_neb[l]||p == l){
                  l = l + 1
                  next
                }
                
                if(x_nsb[l] < x_neb[p] + MTTRDb) {
                  
                  if(dun[p] == 0)
                    x_reb[p] = x_reb[p] + MTTRDb
                  else
                    x_reb[p] = min(x_reb[p] + MTTRDb, x_end[p])
                  
                  x_nsb[l] = x_reb[p] + MTTRDb
                  p = l
                } else {
                  break
                }
              }
            }
          }
          else
            break
        }
      }
      
      k = 2
      for(i in 1:(rne - 1)) {
        for(j in (i+1):rne){
          if(x_d[j] < x_rec[i]) {
            x_redc[i] = x_redc[i] + MTTRDc
            x_redc[j] = x_redc[j] + MTTRDc
            total_nc = total_nc + MTTRDc
            while(k < rne && x_nsc[k] < x_redc[j]) {
              k = k + 1
            }
            if(x_nsc[k-1] != x_nec[k-1] && x_nec[k-1] > x_dd[j] && x_nsc[k-1] < x_redc[j]) {
              l = k
              p = k-1
              while(l <= rne) {
                if(x_nsc[l] == x_nec[l] || p == l){
                  l = l + 1
                  next
                }
                if(x_nsc[l] < x_nec[p] + MTTRDc) {
                  if(dun[p] == 0)
                    x_rec[p] = x_rec[p] + MTTRDc
                  else
                    x_rec[p] = min(x_rec[p] + MTTRDc, x_end[p])
                  
                  x_nsc[l] = x_rec[p] + MTTRDc
                  p = l
                } else {
                  break
                }
              }
            }
          }
          else
            break
        }
      }
    }
    
    #exra cost
    costrs = 0
    for(i in 1:(rne-1)) {
      if(x_end[i] <= x_dd[i])
        next
      
      for(j in (i+1):rne) {
        if(x[j] >= x_end[i])
          break
        if(x_end[j] <= x_dd[j])
          next
        #been detected & concurrent
        #if(x[j] >= x_end[i] &&x_end[j] <= x_dd[j] && x_end[i] >= x_dd[j]) {
        if(x_end[i] >= x_dd[j]) {
          if((dun[i] <= MTTDg && dun[i] != 0)|| (dun[j] <= MTTDg  && dun[j] !=0) )
            costrs = costrs + MTTRTa
        }
      }
    }
    
    #data loss count
    data_la_g = 0
    data_la_a = 0
    data_la_b = 0
    data_la_c = 0
    
    #data loss count
    data_lm_g = 0
    data_lm_a = 0
    data_lm_b = 0
    data_lm_c = 0
    
    #google
    for(i in 1:(rne-3)) {
      
      for (j in (i+1):(rne-2)) { 
        if(x[j] >= x_reg[i])
          break
        
        for(l in (j+1):(rne-1)) {
          if(x[l] >= x_reg[i])
            break
          if(x[l] >= x_reg[j])
            break
          
          for(p in (l+1):rne) {
            if (x[p] >= x_reg[i])
              break
            if (x[p] >= x_reg[j])
              break
            if (x[p] >= x_reg[l])
              break
            
            #four nodes are unavailable
            comblock = min((x_reg[i] - x[p])/MTTRSg,(x_reg[j] - x[p])/MTTRSg,(x_reg[l] - x[p])/MTTRSg,1)
            if(x[l] -x_dg[j] <= MTTRDg && x[p] - x_dg[l] <= MTTRTg) {
              if(dun[i] == 0 && dun[j] == 0 && dun[l] == 0 && dun[p] == 0)
                data_lm_g = data_lm_g + comblock
              else
                data_la_g = data_la_g + comblock
            }
          }
        }
      }
    }
    
    #our method a: only lengthen the detection time
    for(i in 1:(rne-3)) {
      
      for (j in (i+1):(rne-2)) { 
        if(x[j] >= x_rea[i])
          break
        
        for(l in (j+1):(rne-1)) {
          if(x[l] >= x_rea[i])
            break
          if(x[l] >= x_rea[j])
            break
          
          for(p in (l+1):rne) {
            if (x[p] >= x_rea[i])
              break
            if (x[p] >= x_rea[j])
              break
            if (x[p] >= x_rea[l])
              break
            
            #four nodes are unavailable
            comblock = min((x_rea[i] - x[p])/MTTRSa,(x_rea[j] - x[p])/MTTRSa,(x_rea[l] - x[p])/MTTRSa,1)
            if(x[l] -x_dd[j] <= MTTRDa && x[p] - x_dt[l] <= MTTRTa) {
              if(dun[i] == 0 && dun[j] == 0 && dun[l] == 0 && dun[p] == 0)
                data_lm_a = data_lm_a + comblock
              else
                data_la_a = data_la_a + comblock
            }
          }
        }
      }
    }
    
    #our method b: lengthen the detection time and 0.5 network
    for(i in 1:(rne-3)) {
      
      for (j in (i+1):(rne-2)) { 
        if(x[j] >= x_reb[i])
          break
        
        for(l in (j+1):(rne-1)) {
          if(x[l] >= x_reb[i])
            break
          if(x[l] >= x_reb[j])
            break
          
          for(p in (l+1):rne) {
            if (x[p] >= x_reb[i])
              break
            if (x[p] >= x_reb[j])
              break
            if (x[p] >= x_reb[l])
              break
            
            #four nodes are unavailable
            comblock = min((x_reb[i] - x[p])/MTTRSb,(x_reb[j] - x[p])/MTTRSb,(x_reb[l] - x[p])/MTTRSb,1)
            if(x[l] -x_dd[j] <= MTTRDb && x[p] - x_dt[l] <= MTTRTb) {
              if(dun[i] == 0 && dun[j] == 0 && dun[l] == 0 && dun[p] == 0)
                data_lm_b = data_la_b + comblock
              else
                data_la_b = data_la_b + comblock
            }
          }
        }
      }
    }
    
    #our method c: lengthen the detection time and 0.2 network
    for(i in 1:(rne-3)) {
      
      for (j in (i+1):(rne-2)) { 
        if(x[j] >= x_rec[i])
          break
        
        for(l in (j+1):(rne-1)) {
          if(x[l] >= x_rec[i])
            break
          if(x[l] >= x_rec[j])
            break
          
          for(p in (l+1):rne) {
            if (x[p] >= x_rec[i])
              break
            if (x[p] >= x_rec[j])
              break
            if (x[p] >= x_rec[l])
              break
            
            #four nodes are unavailable
            comblock = min((x_rec[i] - x[p])/MTTRSc,(x_rec[j] - x[p])/MTTRSc,(x_rec[l] - x[p])/MTTRSc,1)
            if(x[l] -x_dd[j] <= MTTRDc && x[p] - x_dt[l] <= MTTRTc) {
              if(dun[i] == 0 && dun[j] == 0 && dun[l] == 0 && dun[p] == 0)
                data_lm_c = data_lm_c + comblock
              else
                data_la_c = data_la_c + comblock
            }
          }
        }
      }
    }
    #output
    result<-c(data_la_g,data_la_a,data_la_b,data_la_c,data_lm_g,data_lm_a,data_lm_b,data_lm_c,total_ng, total_na,total_nb,total_nc,costrs)
    return (result)
  }
  #number of nodes
  m = 10
  
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
    else
      break
  }
  
  #time to failure
  x_f = vector(length = rne)
  #duration of unavailability
  dun = vector(length = rne)
  
  h=1
  a=0
  u1 = runif(rne)
  u2 = runif(rne)
  for(i in 1:rne){
    x_f[i] = x[h] + a
    if(u1[i] < 0.99) {
      h = h+1
      a = 0
    }
    else{
      #generate correlated failures
      a = 1200*u2[i]
    }
  }
  
  #weibull
  #50% < 1min, 94%<15min
  tdun = rweibull(rne,0.500169,119.9594)*10
  
  for(f in 1:rne) {
    if(u2[f] > 0.993334)
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
  #6+3
  result1<-run(x_f, dun, 15*60*10, MTTRS*6, MTTRD*6, MTTRT*6, 0)
  #zigzag
  #result2<-run(x_f, dun, 15*60*10, MTTRS*(8/3), MTTRD*(14/3), MTTRT*6, 0)
  #lrc 6,2,2
  #result3<-run(x_f, dun, 30*60*10, MTTRS*3.6, MTTRD*4.4, MTTRT*6, 0)
  #lrc 12,2,2
  #result4<-run(x_f, dun, 30*60*10, MTTRS*6.75, MTTRD*7.65*2, MTTRT*12, 0)
  #12+3
  #result5<-run(x_f, dun, 15*60*10, MTTRS*12, MTTRD*12*2, MTTRT*12, 0)
  result<-c(result1)
  return (result)
}

m = 1000
#chunk 16TB/per node
#128MB/per chunk
block = 125000
#for replication
#re3
replica = 3
common22b = choose(m-2, replica-2)*m*block/(choose(m,replica)*replica)
common33b = choose(m-3, replica-3)*m*block/(choose(m,replica)*replica)
common23b = choose(3,2)*choose(m-3, replica-2)*m*block/(choose(m,replica)*replica)


#re4
replica = 4
common22b = choose(m-2, replica-2)*m*block/(choose(m,replica)*replica)
common33b = choose(m-3, replica-3)*m*block/(choose(m,replica)*replica)
common44b = choose(m-4, replica-4)*m*block/(choose(m,replica)*replica)
common24b = choose(4,2)*choose(m-4, replica-2)*m*block/(choose(m,replica)*replica)


#for rs
datanum = 6
paritynum = 3
allnum = datanum + paritynum 
common22brs = choose(m-2, allnum-2)*m*block/(choose(m,allnum)*allnum)
common33brs = choose(m-3, allnum-3)*m*block/(choose(m,allnum)*allnum)
common44brs = choose(m-4, allnum-4)*m*block/(choose(m,allnum)*allnum)
common24brs = choose(4,2)*choose(m-4, allnum-2)*m*block/(choose(m,allnum)*allnum)

#test <- c(0,25,50,75,100,125,150,175)
MTTF = 4.6*30*24*3600*10
#max failure time
MFT = 5*365*24*3600*10

#number of tests
N = 1
NC = 1

#result for graph
res_una_g <- vector(length = NC)
res_una_a <- vector(length = NC)
res_una_b <- vector(length = NC)
res_una_c <- vector(length = NC)

res_mttdl_g <- vector(length = NC)
res_mttdl_a <- vector(length = NC)
res_mttdl_b <- vector(length = NC)
res_mttdl_c <- vector(length = NC)

res_total_ng <- vector(length = NC)
res_total_na <- vector(length = NC)
res_total_nb <- vector(length = NC)
res_total_nc <- vector(length = NC)

res_extra_cost<- vector(length = NC)

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
    res_una_g[j] = res_una_g[j] + res[[i]][1]
    res_una_a[j] = res_una_a[j] + res[[i]][2]
    res_una_b[j] = res_una_b[j] + res[[i]][3]
    res_una_c[j] = res_una_c[j] + res[[i]][4]
    
    res_mttdl_g[j] = res_mttdl_g[j] + res[[i]][5]
    res_mttdl_a[j] = res_mttdl_a[j] + res[[i]][6]
    res_mttdl_b[j] = res_mttdl_b[j] + res[[i]][7]
    res_mttdl_c[j] = res_mttdl_c[j] + res[[i]][8]
    
    res_total_ng[j] = res_total_ng[j] + res[[i]][9]
    res_total_na[j] = res_total_na[j] + res[[i]][10]
    res_total_nb[j] = res_total_nb[j] + res[[i]][11]
    res_total_nc[j] = res_total_nc[j] + res[[i]][12]
    
    res_extra_cost[j] = res_extra_cost[j] + res[[i]][13]

  }
  
  if(j < NC) {
    res_una_g[j+1] = res_una_g[j] 
    res_una_a[j+1] = res_una_a[j] 
    res_una_b[j+1] = res_una_b[j]
    res_una_c[j+1] = res_una_c[j] 
    
    res_mttdl_g[j+1] = res_mttdl_g[j] 
    res_mttdl_a[j+1] = res_mttdl_a[j] 
    res_mttdl_b[j+1] = res_mttdl_b[j] 
    res_mttdl_c[j+1] = res_mttdl_c[j] 
    
    res_total_ng[j+1] = res_total_ng[j] 
    res_total_na[j+1] = res_total_na[j] 
    res_total_nb[j+1] = res_total_nb[j] 
    res_total_nc[j+1] = res_total_nc[j] 
    
    res_extra_cost[j+1] = res_extra_cost[j]
  }
  print("availability")
  print(res_una_g[j])
  print(res_una_a[j])
  print(res_una_b[j])
  print(res_una_c[j])
  print("reliability")
  ttime=j*N*MFT/common44brs
  if(res_mttdl_g[j] > 0)
    print(ttime/res_mttdl_g[j])
  else
    print("Not Loss")
  
  if(res_mttdl_a[j] > 0)
    print(ttime/res_mttdl_a[j])
  else
    print("Not Loss")
  
  if(res_mttdl_b[j] > 0)
    print(ttime/res_mttdl_b[j])
  else
    print("Not Loss")
  
  if(res_mttdl_c[j] > 0)
    print(ttime/res_mttdl_c[j])
  else
    print("Not Loss")
}

ptm <- proc.time()
proc.time() - ptm
print(ptm)

stopCluster(cl)

print("end")
