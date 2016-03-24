cal <- function(y) {
  #number of nodes
  m = 1000
  #mean time to failure
  MTTF = 4.3*30*24*3600
  #repair time of single failure
  MTTRS = 0.9*3600*10
  #detect time of azure
  MTTDAZURE = 30*60*10
  #detect time of google
  MTTDGOO = 15*60*10
  #repair time of multi-failure
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

  #extra cost
  costt = 0
  costp = 0
  costrs = 0
  
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
  m=1000
  if(y[3] == 1) {
    #uniform
    dun = runif(m,1,2*600)
    #15min-115min
    tdun = runif(m,15*600,115*600)
    #0-15min
    ttdun = runif(m,0,15*600)
  }
  else if (y[3] == 2) {
    #exponential
    dun = runif(m,0,2*600)
    tdun = 15*600 + rexp(m,1/MTUA)
    ttdun = rexp(m,log(10)/9000)
  }
  else if(y[3] == 3) {
    #norm
    dun = rnorm(m,1*600,600/3)
    tdun = rnorm(m,65*600,50*600/3)
    ttdun = rnorm(m,7.5*600,7.5*600/3)
  }
  
  u1 = runif(m)
  for(f in 1:m) {
    if(u1[f] >= 0.9) {
      #10% > 15min
      if(tdun[f] < 15*600)
        dun[f] = runif(1,15*600,115*600)
      else
        dun[f] = tdun[f]
    }
    else{
      if(ttdun[f] <= 0||ttdun[f] > 15*600)
        dun[f] = runif(1,1,15*600)
      else
        dun[f] = ttdun[f]
    }
  } 
  
  x <- sort(x)
  x_d <- ceiling(x/3000)*3000
  x_t <- ceiling((x+MTTDC)/3000)*3000
  x_end <- (x + dun)
  
  for(i in 1:(m-1)) {
    for(j in (i+1):min(i+10,m)) {
      #been detected
      if(x_end[i] >= x_t[i] && x_end[j] >= x_t[j]) {
        #concurrent
        if(x_end[i] >= x_t[j]) {
          if(dun[i] < MTTDGOO && dun[j] < MTTDGOO)
            costp = costp + 1
          if(dun[i] < MTTDGOO || dun[j] < MTTDGOO)
            costrs = costrs + 1 
        }
      }
    }
  }
  
  return (c(costt,costp,costrs))
}


#result for graph
resultt <- vector(length = 8*5*3)
resultp <- vector(length = 8*5*3)
resultrs <- vector(length = 8*5*3)


#number of tests
N = 1000000
m = 10000
test <- c(0,25,50,75,100,125,150,175)
MTTF = 4.6*30*24*3600*10

for(j in 1:120) {
  print("begin")
  prob = ceiling(j/40)
  mttdc = ceiling((j-(prob-1)*40)/8)
  corf = j - (prob-1)*40 - (mttdc-1)*8
  parax = list()
  for(i in 1:N) 
    parax[[i]] <- c(mttdc,test[corf],prob)
  
  print("start")
  cl.cores <- detectCores()
  #print(cl.cores)
  cl <- makeCluster(cl.cores)

  res <- parLapply(cl,parax,cal)

  #print(tt)
  for(i in 1:N) {
    resultt[j] = resultt[j] + res[[i]][1]
    resultp[j] = resultp[j] + res[[i]][2]
    resultrs[j] = resultrs[j] + res[[i]][3]
  }
    
  resultp[j] = resultp[j]*2/(resultt[j]*m)
  resultrs[j] = resultrs[j]*2/(resultt[j]*m)
       
  stopCluster(cl)
}