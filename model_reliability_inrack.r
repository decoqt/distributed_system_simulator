cal <- function(y) {
  #number of racks
  nr = 50
  nn = 20
  #number of nodes
  m = nr*nn
  #mean time to failure
  MTTF = 4.3*30*24*3600*10
  #MTTF = 5*365*24*3600*10
  #repair time of single failure
  #MTTRS = 0.9*3600*10
  #repair time of single block
  MTTRSB = 12.8*10

  #detect time of azure
  MTTDAZURE = 30*60*10
  #detect time of google
  MTTDGOO = 15*60*10
  MTTDW = 45*60*10
  
  #our aggresive methods
  MTTROURAGG = 1.28*10
  #mean duration of unavailability time
  MTUA = 50*600

  #block
  block = 125000
  replica = 3
  common_rack = 2*block/(3*(nn-1))
  common_dr = 2*block/(3*nn*(nr-1))
 
  #recovery time
  MTTRS = MTTRSB*block/m
  MTTRSa = MTTRSB*block/(3*m)
  MTTRSb = 2*MTTRSB*block/(3*m)
  
  MTTRr = MTTRSB*common_rack/m
  MTTRdr = MTTRSB*common_dr/m
  

  commonb = block*2/(3*(nr-1)*nn*(nn-1))

  #time to failure
  x_s = vector(length = m)
  x_ss = vector(length = m)
  #duration of unavailability
  dun = vector(length = m)
  #end time of failure
  x_end = vector(length = m)


  #number of data loss
  data_loss_goo = 0
  data_loss_goo_a = 0
  data_loss_goo_b = 0
  data_loss_goo_c = 0
  data_loss_ourc = 0
  data_loss_our_a = 0
  data_loss_our_b = 0
  data_loss_our_c = 0
  data_loss_our = 0
  
  data_loss_in_rack = 0
  data_loss_rack = 0
  
  data_lc_a = 0
  data_lc_b = 0
  data_lc_c = 0

  #generate time to failure
  x_s = rexp(m,1/MTTF)
  tdun = 15*600 + rexp(m,1/MTUA)
  
  h=1
  a=1
  f=1
  u1 = runif(m)
  u2 = runif(m)
  u3 = runif(m)
  
  for(f in 1:m) {
    if(u2[f]>0.95)
      tdun[f] = 0
  } 
  
  f=1
  #generate correlated failures
  for(s1 in 1:nn) {
    for(s2 in 1:nr) {
      x_ss[(s2-1)*nn + s1] = x_s[(s2-1)*nn + s1]
      
      if(f > m-y) {      
        h = max(ceiling(u3[a]*s1)-1,1)
        x_ss[(s2-1)*nn + s1] = x_ss[(s2-1)*nn + h] + 1200*u2[f]
        if(u1[a] > 0.5) {
          tdun[(s2-1)*nn + s1] = tdun[(s2-1)*nn + h]
        }
        a = a+1
      }
      
      f = f+1
    }  
  }
  
  
  
  #sort
  x <- sort(x_ss)
  x_rank <- order(x_ss)
  x_d <- ceiling(x/3000)*3000
  x_br <- ceiling((x+MTTDGOO)/3000)*3000
  
  #repair end time
  x_re_b <- x_br
  x_re_e <- x_br
  x_re_fb <- x_br
  x_re_sb <- x_br
  x_re_fe <- x_br
  x_re_se <- x_br
  
  x_rew_fb <- x_br
  x_rew_sb <- x_br
  x_rew_fe <- x_br
  x_rew_se <- x_br
  
  x_repair = 0
  x_repair_f = 0
  x_repair_d = 0
  for(i in 1:m)
  {
    dun[i] = tdun[x_rank[i]]
    x_end[i] = x[i] + dun[i]
    
    if(x_end[i] -x_d[i] > MTTDGOO) { 
      x_re_b[i] = max(x_repair, x_re_b[i])
      x_re_e[i] = max(min(MTTRS + x_re_b[i], x_end[i]),x_re_b[i])
      x_repair = max(x_repair, x_re_e[i])
    }
    else if(dun[i] == 0)
    {
      x_re_b[i] = max(x_repair, x_br[i])
      x_re_e[i] = MTTRS + x_re_b[i]
      x_repair = max(x_repair, x_re_e[i])
    }
    
    if(x_end[i] -x_d[i] > MTTDGOO) {  
      x_re_fb[i] = max(x_repair_f, x_re_fb[i])
      x_re_fe[i] = max(min(MTTRSa + x_re_fb[i], x_end[i]),x_re_fb[i])
      x_repair_f = max(x_repair_f, x_re_fe[i])   
      if(x_end[i] > x_re_fe[i]) {
        x_re_sb[i] = x_re_fe[i] 
        x_re_se[i] = max(min(MTTRSb + x_re_sb[i], x_end[i]),x_re_sb[i])
        x_repair_f = max(x_repair_f, x_re_se[i])  
      }
      else{
        x_re_se[i] = x_re_sb[i] = x_repair_f
      }
    }
    else if(dun[i] == 0) {
      x_re_fb[i] = max(x_repair_f, x_re_fb[i])
      x_re_fe[i] = MTTRSa + x_re_fb[i]
      x_re_sb[i] = x_re_fe[i]
      x_re_se[i] = MTTRSb + x_re_sb[i]
      x_repair_f = max(x_repair_f, x_re_se[i])  
    }
    
    if(x_end[i] -x_d[i] > MTTDGOO) {  
      x_rew_fb[i] = max(x_repair_d, x_rew_fb[i])
      x_rew_fe[i] = max(min(MTTRSa + x_rew_fb[i], x_end[i]), x_rew_fb[i])
      x_repair_d = max(x_repair_d, x_rew_fe[i])   
      if(x_end[i] > x_rew_fe[i] + MTTDW) {
        x_rew_sb[i] = x_rew_fe[i] + MTTDW 
        x_rew_se[i] = max(min(MTTRSb + x_rew_sb[i], x_end[i]),x_rew_sb[i])
        x_repair_d = max(x_repair_d, x_rew_se[i])  
      }
      else{
        x_rew_se[i] = x_rew_sb[i] = x_repair_d
      }
    }
    else if(dun[i] == 0) {
      x_rew_fb[i] = max(x_repair_d, x_rew_fb[i])
      x_rew_fe[i] = MTTRSa + x_rew_fb[i]
      x_rew_sb[i] = x_rew_fe[i] + MTTDW
      x_rew_se[i] = MTTRSb + x_rew_sb[i]
      x_repair_d = max(x_repair_d, x_rew_se[i])  
    }
  }
    
  for(i in 1:(m-2)) {
    for(j in (i+1):min(i+5,m-1)) {         
      for(l in (j+1):min(j+5,m)) { 
        #three nodes are unavailable
        nn_i = ceiling(x_rank[i]/nn) 
        nn_j = ceiling(x_rank[j]/nn)
        nn_l = ceiling(x_rank[l]/nn)
        if(nn_i == nn_j && nn_i == nn_l) {
          data_loss_goo = data_loss_goo + 0
          data_loss_ourc = data_loss_ourc + 0
          data_loss_our = data_loss_our + 0
          if(dun[i]==0 && dun[j]==0 && dun[l]==0)
            data_loss_in_rack = data_loss_in_rack +1 
        }
        else if(nn_i != nn_j && nn_i != nn_l && nn_j != nn_l) {
          data_loss_goo = data_loss_goo + 0
          data_loss_ourc = data_loss_ourc + 0
          data_loss_our = data_loss_our + 0
          if(dun[i]==0 && dun[j]==0 && dun[l]==0)
            data_loss_rack = data_loss_rack +1 
        }
        else if(nn_i == nn_j) {
          if((x[l] < x_re_e[i] && x[l] < x_re_e[j]) && dun[i]==0 && dun[j]==0 && dun[l]==0) { 
            if(x_re_e[i] - x_re_b[i] > 0)
              comblock = min((x_re_e[i] - x[l])/(x_re_e[i] - x_re_b[i]),1)*commonb
            else 
              comblock = 0
            
            if(x[l] - x_d[j] <= MTTDGOO + MTTRr) {
              #unavailability time
              data_loss_goo = data_loss_goo + comblock
              data_loss_goo_a = data_loss_goo_a + 1
            } 
          }#endif 
          
          if((x[l] < x_re_se[i] && x[l] < x_re_se[j]) && dun[i]==0 && dun[j]==0 && dun[l]==0) { 
            if(x_re_se[i] - x_re_sb[i] > 0)
              comblock = min((x_re_se[i] - x[l])/(x_re_se[i] - x_re_sb[i]),1)*commonb
            else
              comblock = 0
            
            if(x[l] - x_d[j] <= MTTDGOO + MTTRr) {
              #unavailability time
              data_loss_ourc = data_loss_ourc + comblock
            } 
          }#endif 
          
          if(x[l] < x_rew_se[i] && dun[i]==0 && dun[j]==0 && dun[l]==0) { 
            if(x_rew_se[i] - x_rew_sb[i] > 0)
              comblock = min((x_rew_se[i] - x[l])/(x_rew_se[i] - x_rew_sb[i]),1)*commonb
            else {
              comblock = 0
            }
            
              
            
            if(x[l] - x_d[j] <= MTTDGOO + MTTRr) {
              #unavailability time
              data_loss_our = data_loss_our + comblock
              data_loss_our_a = data_loss_our_a + 1
            } 
          }#endif
        }
        else if (nn_j == nn_l) {
          if((x[l] < x_re_e[i] && x[l] < x_re_e[j]) && dun[i]==0 && dun[j]==0 && dun[l]==0) { 
            if(x_re_e[i] - x_re_b[i] > 0)
              comblock = min((x_re_e[i] - x[l])/(x_re_e[i] - x_re_b[i]),1)*commonb
            else
              comblock = 0
            
            if(x[l] - x_d[j] <= MTTDGOO + MTTRdr) {
              #unavailability time
              data_loss_goo = data_loss_goo + comblock
              data_loss_goo_b = data_loss_goo_b + 1
            } 
          }#endif 
          
          if((x[l] < x_re_fe[i] && x[l] < x_re_fe[j]) && dun[i]==0 && dun[j]==0 && dun[l]==0) { 
            if(x_re_fe[i] - x_re_fb[i] > 0)
              comblock = min((x_re_fe[i] - x[l])/(x_re_fe[i] - x_re_fb[i]),1)*commonb
            else
              comblock = 0
            
            if(x[l] - x_d[j] <= MTTDGOO + MTTRdr) {
              #unavailability time
              data_loss_ourc = data_loss_ourc + comblock
            } 
          }#endif 
          
          if(x[l] < x_rew_fe[i] && dun[i]==0 && dun[j]==0 && dun[l]==0) { 
            if(x_rew_fe[i] - x_rew_fb[i] > 0)
              comblock = min((x_rew_fe[i] - x[l])/(x_rew_fe[i] - x_rew_fb[i]),1)*commonb
            else
              comblock = 0
            
            if(x[l] - x_d[j] <= MTTDGOO + MTTRdr) {
              #unavailability time
              data_loss_our = data_loss_our + comblock
              data_loss_our_b = data_loss_our_b + 1
            } 
          }#endif
          
        }
        else if (nn_i == nn_l){
          if((x[l] < x_re_e[i] && x[l] < x_re_e[j]) && dun[i]==0 && dun[j]==0 && dun[l]==0) { 
            if(x_re_e[i] - x_re_b[i] > 0)
              comblock = min((x_re_e[i] - x[l])/(x_re_e[i] - x_re_b[i]),1)*commonb
            else
              comblock = 0
            
            if(x[l] - x_d[j] <= MTTDGOO + MTTRdr) {
              #unavailability time
              data_loss_goo = data_loss_goo + comblock
              data_loss_goo_c = data_loss_goo_c + 1
            } 
          }#endif 
          
          if((x[l] < x_re_se[i] && x[l] < x_re_se[j]) && dun[i]==0 && dun[j]==0 && dun[l]==0) { 
            if(x_re_se[i] - x_re_sb[i] > 0)
              comblock = min((x_re_se[i] - x[l])/(x_re_se[i] - x_re_sb[i]),1)*commonb
            else
              comblock = 0
            
            if(x[l] - x_d[j] <= MTTDGOO + MTTRdr) {
              #unavailability time
              data_loss_ourc = data_loss_ourc + comblock
            } 
          }#endif 
          
          if(x[l] < x_rew_se[i]  && dun[i]==0 && dun[j]==0 && dun[l]==0) { 
            if(x_rew_se[i] - x_rew_sb[i] > 0)
              comblock = min((x_rew_se[i] - x[l])/(x_rew_se[i] - x_rew_sb[i]),1)*commonb
            else
              comblock = 0
            
            if(x[l] - x_d[j] <= MTTDGOO + MTTRdr) {
              #unavailability time
              data_loss_our = data_loss_our + comblock
              data_loss_our_c = data_loss_our_c + 1
            } 
          }#endif 
        }        
      }
    }
  }
  
  tr_goo = sum(x_re_e-x_re_b)
  tr_ourc = sum(x_re_fe-x_re_fb) + sum(x_re_se-x_re_sb)
  tr_our = sum(x_rew_fe-x_rew_fb) + sum(x_rew_se-x_rew_sb)
  
  #output
  result<-c(data_loss_goo,data_loss_ourc,data_loss_our,data_loss_goo_a,data_loss_goo_b,data_loss_goo_c,data_loss_our_a,data_loss_our_b,data_loss_our_c,tr_goo,tr_ourc,tr_our,data_loss_in_rack,data_loss_rack)
  return (result)
}


#result for graph
res_mttdl_goo <- vector(length = 8)
res_mttdl_ourc <- vector(length = 8)
res_mttdl_our <- vector(length = 8)
res_mttdl_goo_a <- vector(length = 8)
res_mttdl_goo_b <- vector(length = 8)
res_mttdl_goo_c <- vector(length = 8)
res_mttdl_our_a <- vector(length = 8)
res_mttdl_our_b <- vector(length = 8)
res_mttdl_our_c <- vector(length = 8)
tr_goo<- vector(length = 8)
tr_our<- vector(length = 8)
tr_ourc<- vector(length = 8)
dl_in_rack<- vector(length = 8)
dl_rack<- vector(length = 8)
#number of tests
N = 1000000

#number of racks
nr = 50
nn = 20
#total number of nodes
m = nr*nn

test <- c(0,25,50,75,100,125,150,175)
#test <- c(0,5,10,15,20,25,30,35)
MTTF = 4.6*30*24*3600*10
#MTTF = 5*365*24*3600*10  

for(j in 1:8) {
  print(j)
  print("begin")
  parax = list()
  for(i in 1:N) 
    parax[i] <- test[j]
  
  print("start")
  cl.cores <- detectCores()
  #print(cl.cores)
  cl <- makeCluster(cl.cores)
  
  res <- parLapply(cl,parax,cal)
  
  #print(tt)
  for(i in 1:N) {
    res_mttdl_goo[j] = res_mttdl_goo[j] + res[[i]][1]
    res_mttdl_ourc[j] = res_mttdl_ourc[j] + res[[i]][2]
    res_mttdl_our[j] = res_mttdl_our[j] + res[[i]][3]
    res_mttdl_goo_a[j] = res_mttdl_goo_a[j] + res[[i]][4]
    res_mttdl_goo_b[j] = res_mttdl_goo_b[j] + res[[i]][5]
    res_mttdl_goo_c[j] = res_mttdl_goo_c[j] + res[[i]][6]
    res_mttdl_our_a[j] = res_mttdl_our_a[j] + res[[i]][7]
    res_mttdl_our_b[j] = res_mttdl_our_b[j] + res[[i]][8]
    res_mttdl_our_c[j] = res_mttdl_our_c[j] + res[[i]][9]
    tr_goo[j] = tr_goo[j] + res[[i]][10]
    tr_ourc[j] = tr_ourc[j] + res[[i]][11]
    tr_our[j] = tr_our[j] + res[[i]][12]
    dl_in_rack[j] = dl_in_rack[j] + res[[i]][13]
    dl_rack[j] = dl_rack[j] + res[[i]][14]
  }
  tr_goo[j] = tr_goo[j]/N
  tr_ourc[j] = tr_ourc[j]/N
  tr_our[j] = tr_our[j]/N
  stopCluster(cl)
}



