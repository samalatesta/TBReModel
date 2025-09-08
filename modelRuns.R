library(dplyr)
library(kableExtra)
library(ggplot2)
library(ggpubr)
library(readxl)

#read in parms for sweep
parms = read_xlsx("parm_sweep_TBARI.xlsx")
parms=parms[1:6, 3:6]/100

#read in times to start treatment
time = readRDS("time.rds")

#source model
source("model_dev.R")

#fixed inputs 

#population size (constant)
pop <- 1000000

#mortality (constant)
mortality = 0.005

#steps to run model (each 1 yr)
start=1
steps = 50

#aris 
aris = c(0.56, 1.12, 1.68, 2.24, 2.80, 3.36, 3.92, 4.49, 5.05, 5.61)


#a place to save all steps for each ari
runs <- vector(mode = "list", length = length(aris)*ncol(parms))

#protection = 0 
inc=0
for(i in 1:length(aris)){
  for(j in 1:4){
    inc=inc+1
    
    #Probability of progression to disease, depends on infection duration
    dis.prob <- parms[,j]
    
    progs <- c("0.03", "0.04", "0.05", "0.06")
    #ari
    ari = aris[i]/100
    
    #trt cure
    tx.cure.rate = 0
  

  #protection from progression for reinfections
  protect=0
  
  #start.trt (could make dynamic if needed), this is informed form the initial analysis establishing target prevalences
  start.trt.step = time %>% filter(ari_par==ari & protect==0 & tot.dis.prog==progs[j])
  
  #trt cure
  tx.cure.rate = 0
  
  #run model and save each run in list
  res <- TBmodel(dis.prob, pop, mortality, start, steps, ari, tx.cure.rate, stable=T, start.trt=start.trt.step$time, protect)
  
  runs[[inc]] <- res
  }
  
}


#combine runs
all1 <- do.call("rbind", runs) 

#save runs to RDS file 
saveRDS(all1, paste0("TB_model_protect0_", Sys.Date(), ".rds"))

#protection = .5

#a place to save all steps for each ari
runs <- vector(mode = "list", length = length(aris)*ncol(parms))

#protection = 0 
inc=0
for(i in 1:length(aris)){
  for(j in 1:4){
    inc=inc+1
    
    #Probability of progression to disease, depends on infection duration
    dis.prob <- parms[,j]
    
    progs <- c("0.03", "0.04", "0.05", "0.06")
    #ari
    ari = aris[i]/100
    
    #trt cure
    tx.cure.rate = 0
    
    
    #protection from progression for reinfections
    protect=0.5
    
    #start.trt (could make dynamic if needed), this is informed form the initial analysis establishing target prevalences
    start.trt.step = time %>% filter(ari_par==ari & protect==0 & tot.dis.prog==progs[j])
    
    #trt cure
    tx.cure.rate = 0
    
    #run model and save each run in list
    res <- TBmodel(dis.prob, pop, mortality, start, steps, ari, tx.cure.rate, stable=T, start.trt=start.trt.step$time, protect)
    
    runs[[inc]] <- res
  }
  
}

#combine runs
all2 <- do.call("rbind", runs) 

#save runs to RDS file 
saveRDS(all2, paste0("TB_model_protect.5_", Sys.Date(), ".rds"))


#protection = .8 
#a place to save all steps for each ari
runs <- vector(mode = "list", length = length(aris)*ncol(parms))

inc=0
for(i in 1:length(aris)){
  for(j in 1:4){
    inc=inc+1
    
    #Probability of progression to disease, depends on infection duration
    dis.prob <- parms[,j]
    
    progs <- c("0.03", "0.04", "0.05", "0.06")
    #ari
    ari = aris[i]/100
    
    #trt cure
    tx.cure.rate = 0
    
    
    #protection from progression for reinfections
    protect=0.8
    
    #start.trt (could make dynamic if needed), this is informed form the initial analysis establishing target prevalences
    start.trt.step = time %>% filter(ari_par==ari & protect==0 & tot.dis.prog==progs[j])
    
    #trt cure
    tx.cure.rate = 0
    
    #run model and save each run in list
    res <- TBmodel(dis.prob, pop, mortality, start, steps, ari, tx.cure.rate, stable=T, start.trt=start.trt.step$time, protect)
    
    runs[[inc]] <- res
  }
  
}

#combine runs
all3 <- do.call("rbind", runs) 

#save runs to RDS file 
saveRDS(all3, paste0("TB_model_protect.8_", Sys.Date()))

#combine runs
all <- rbind(all1, all2, all3)

#save runs to RDS file 
saveRDS(all, paste0("TB_model_", Sys.Date(), ".rds"))
