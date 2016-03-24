cal <- function(y) {

  #number of nodes
  m = 1000
  #mean time to failure
  MTTF = 4.3*30*24*3600*10
  #repair time of single failure
  #MTTRS = 0.9*3600*10
  MTTRS = 3600*10
  #detect time of azure
  MTTDAZURE = 30*60*10
  #detect time of google
  MTTDGOO = 15*60*10
  #repair time of multiple failures
  MTTROUR = 12.8*10
  #our aggresive methods
  MTTROURAGG = 1.28*10
  #mean duration of unavailability time
  MTUA = 65*600
  #detect time of critical-failure
  MTTDC = y[1]*600


  #block
  block = 125000
  replica = 3
  commonb = block*(replica-1)/((m-1)*(m-2))

  #time to failure
  x = vector(length = m)
  #duration of unavailability
  dun = vector(length = m)
  #end time of failure
  x_end = vector(length = m)
  #repair end time
  x_rep = vector(length = m)

  #total time
  ttime = 0
  #total time of unavailability
  tdun = 0
  #total unavailability time using google detection time
  ttungoo = 0
  #total unavailability time using our detection time
  ttunour = 0
  #total unavailability time using aggresive detection time
  ttunagg = 0

  #number of 
  data_loss_goo = 0
  data_loss_our = 0
  data_loss_agg = 0

  #data loss count
  data_lc_goo = 0
  data_lc_our = 0
  data_lc_agg = 0
  
  #data unavailability count 
  countgoo = 0
  countour = 0
  countagg = 0
  
  #generate time to failure
  x = rexp(m,1/MTTF)
    
  #generate correlated failures
  h=1
  a=1
  u1 = runif(m)
  u2 = runif(m)
  for(f in min(m-y[2]+1,m):m) {
    if(u1[a] > 0.6) {
      x[f] = x[h] + 1200*u2[f]
      f = f+1;
      #triple failues or more
      #h = h-1;
    }
    h = h+1
    a = a+1
  }

  #generate duration of failures
    
  #均匀分布
  #dun = 15*600 + runif(m,0,100*600)
    
  #指数分布
  #dun = 15*600 + rexp(m,1/MTUA)
    
  #正太分布
  dun = rnorm(m,65*600,50*200)

  #permanent failure
  u3 = runif(m)
  for(f in 1:m)
  {
    if(u3[f] > 0.95)
        dun[f] = 0
  }
    
  #sort
  x <- sort(x)
  x_d <- ceiling(x/3000)*3000
  x_c <- ceiling((x+MTTDC)/3000)*3000
  x_end <- (x + dun)
    
  #repair end time
  x_re <- x_end
  x_repair = 0
  for(i in 1:m)
  {
    if(x_end[i] -x_d[i] > MTTDGOO) {   
      x_re[i] = min(MTTRS + max(x_repair,x_d[i] + MTTDGOO), x_end[i])
      x_repair = max(x_repair, x_re[i])    
    }
    else if(dun[i] == 0)
    {
      x_re[i] = MTTRS + max(x_repair, x_d[i] + MTTDGOO) 
      x_repair = x_re[i]
    }
  }
      
  for(i in 1:(m-2)) {
    for(j in (i+1):min(i+5,m-1)) {         
      for(l in (j+1):min(j+5,m)) { 
        #three nodes are unavailable
        if((x[l] < x_re[i] && x[l] < x_re[j])) {        
          if(x[l] - x_d[j] <= MTTDGOO + MTTROUR) {
            #num of lost blocks under three permanent failures
            if(dun[i] + dun[j] + dun[l] == 0) {
              data_loss_goo = data_loss_goo + min((x_re[i] - x[l])/(x_re[i] - x_d[i] - MTTDGOO),1)*commonb 
              data_lc_goo = data_lc_goo + 1
            }
              
              #num of unavailabile blocks
              #some blocks have been repaired
            if(x_re[i] - x_d[i] - MTTDGOO > 0)
              comblock = min((x_re[i] - x[l])/(x_re[i] - x_d[i] - MTTDGOO),1)*commonb
            else
              comblock = commonb
          
            #unavailability time
            ttungoo = ttungoo + min(x_re[i]-x[l],x_re[j]-x[l],x_re[l]-x[l])*comblock
            countgoo = countgoo + 1
          }
             
          #our method
          if(x[l] - x_c[j] <= MTTROUR) {
            if(dun[i] + dun[j] + dun[l] == 0) {
              data_loss_our = data_loss_our + min((x_re[i] - x[l])/(x_re[i] - x_d[i] - MTTDGOO),1)*commonb 
              data_lc_our = data_lc_our + 1
            } 
              
            if(x_re[i] - x_d[i] - MTTDGOO > 0)
              comblock = min((x_re[i] - x[l])/(x_re[i] - x_d[i] - MTTDGOO),1)*commonb
            else
              comblock = commonb
              
            ttunour = ttunour + min(x_re[i]-x[l],x_re[j]-x[l],x_re[l]-x[l])*comblock
            countour = countour + 1
          }
            
            
   
          #limit，ideal case
          if(x[l] - x[j] <= MTTROUR) {
            if(dun[i] + dun[j] + dun[l] == 0) {
              data_loss_agg = data_loss_agg + min((x_re[i] - x[l])/(x_re[i] - x_d[i] - MTTDGOO),1)*commonb  
              data_lc_agg = data_lc_agg + 1
            } 
              
            if(x_re[i] - x_d[i] - MTTDGOO > 0)
              comblock = min((x_re[i] - x[l])/(x_re[i] - x_d[i] - MTTDGOO),1)*commonb
            else
              comblock = commonb
              
            ttunagg = ttunagg + min(x_re[i]-x[l],x_re[j]-x[l],x_re[l]-x[l])*comblock
            countagg = countagg + 1
          }
            
        }#endif
          
      }
    }
  }
  #output
  result<-c(ttungoo,ttunour,ttunagg,data_loss_goo,data_loss_our,data_loss_agg,data_lc_goo,data_lc_our,data_lc_agg,countgoo,countour,countagg)
  return (result)
}


#result for graph
res_ava_goo <- vector(length = 8*5)
res_ava_our <- vector(length = 8*5)
res_ava_agg <- vector(length = 8*5)
res_mttdl_goo <- vector(length = 8*5)
res_mttdl_our <- vector(length = 8*5)
res_mttdl_agg <- vector(length = 8*5)
res_count_goo <- vector(length = 8*5)
res_count_our <- vector(length = 8*5)
res_count_agg <- vector(length = 8*5)
countgoo <- vector(length = 8*5)
countour <- vector(length = 8*5)
countagg <- vector(length = 8*5)

#number of tests
N = 100000
m = 1000
test <- c(0,25,50,75,100,125,150,175)
MTTF = 4.6*30*24*3600*10

for(j in 1:40) {
  print("begin")
  mttdc = ceiling(j/8)
  corf = j - (mttdc-1)*8
  parax = list()
  for(i in 1:N) 
    parax[[i]] <- c(mttdc,test[corf])
    
  cl.cores <- detectCores()
  #print(cl.cores)
  cl <- makeCluster(cl.cores)
  tt=system.time({
    res <- parLapply(cl,parax,cal)
  })
  #print(tt)
  for(i in 1:N) {
    res_ava_goo[j] = res_ava_goo[j] + res[[i]][1]
    res_ava_our[j] = res_ava_our[j] + res[[i]][2]
    res_ava_agg[j] = res_ava_agg[j] + res[[i]][3]
    res_mttdl_goo[j] = res_mttdl_goo[j] + res[[i]][4]
    res_mttdl_our[j] = res_mttdl_our[j] + res[[i]][5]
    res_mttdl_agg[j] = res_mttdl_agg[j] + res[[i]][6]
    res_count_goo[j] = res_count_goo[j] + res[[i]][7]
    res_count_our[j] = res_count_our[j] + res[[i]][8]
    res_count_agg[j] = res_count_agg[j] + res[[i]][9]
    countgoo[j] = countgoo[j] + res[[i]][10]
    countour[j] = countour[j] + res[[i]][11]
    countagg[j] = countagg[j] + res[[i]][12]
  }
    
  res_ava_goo[j] = res_ava_goo[j]/N
  res_ava_our[j] = res_ava_our[j]/N
  res_ava_agg[j] = res_ava_agg[j]/N
    
  ttime = MTTF*m*100/129
  if(res_mttdl_goo[j] > 0) {
    res_mttdl_goo[j] = (ttime*N)/(res_mttdl_goo[j]*10*3600*24)
  }
  
  if(res_mttdl_our[j] > 0) {
    res_mttdl_our[j] = (ttime*N)/(res_mttdl_our[j]*10*3600*24)
  }
       
  stopCluster(cl)
}