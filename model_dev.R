library(dplyr)
library(kableExtra)
library(ggplot2)
library(ggpubr)


TBmodel <- function(disease_progression, pop, mortality, start, steps, ari, tx.cure.rate, stable=T, start.trt, protect=0){
  
  #data frame that will be updated each year, requires age specific population proportions, death rates
  sim.pop <- data.frame(mortality=mortality, pop=pop, ari=ari)
  
  sim.pop$tx.cure.rate=tx.cure.rate
  sim.pop$tb.dth.rate= (1-tx.cure.rate)/2/5/2
  sim.pop$self.cure.rate = (1-tx.cure.rate)/2/5/2
  
  #loop across all years 
  for(i in start:steps){
    
    #year 1 (do this separately from all other years)
    if(i==1){
      
      #save time step 
      sim.pop$time <- 1
      
      #calculate new primary infections
      sim.pop$new.prim.inf <- sim.pop$pop*ari
      
      #calculate new reinfections
      sim.pop$new.reinf.inf <- 0
      
      
      #calculate old primary infections 
      sim.pop$old.prim.inf <- 0
      
      #calculate old reinfections 
      sim.pop$old.reinf.inf <- 0
      
      #calculate 1-5 yr primary infections 
      sim.pop$inf.prim.1yr <- 0
      sim.pop$inf.prim.2yr <- 0
      sim.pop$inf.prim.3yr <- 0
      sim.pop$inf.prim.4yr <- 0
      
      #reinfections
      sim.pop$inf.reinf.1yr <- 0
      sim.pop$inf.reinf.2yr <- 0
      sim.pop$inf.reinf.3yr <- 0
      sim.pop$inf.reinf.4yr <- 0
      
      
      #calculate total primary infections
      sim.pop$tot.prim.inf <- sim.pop$new.prim.inf 
      
      #calculate total reinfections 
      sim.pop$tot.reinf.inf <- 0
      
      #calculate uninfected 
      sim.pop$uninf <- sim.pop$pop - sim.pop$tot.prim.inf
      
      #calculate deaths for newly infected
      sim.pop$dths.new.prim.inf <-sim.pop$mortality*sim.pop$new.prim.inf
      
      #calculate deaths for newly reinfected
      sim.pop$dths.new.reinf.inf <-sim.pop$mortality*sim.pop$new.reinf.inf
      
      #calculate deaths for 1-5 yr infections
      #primary
      sim.pop$dths.prim.inf.1yr <-0
      sim.pop$dths.prim.inf.2yr <-0
      sim.pop$dths.prim.inf.3yr <-0
      sim.pop$dths.prim.inf.4yr <-0
      #reinf
      sim.pop$dths.reinf.inf.1yr <-0
      sim.pop$dths.reinf.inf.2yr <-0
      sim.pop$dths.reinf.inf.3yr <-0
      sim.pop$dths.reinf.inf.4yr <-0
      
      #calculate deaths for old primary infections
      sim.pop$dths.prim.old <- 0
      #calculate deaths for old reinfections
      sim.pop$dths.reinf.old <- 0
      
      #calculate deaths for uninfected 
      sim.pop$dths.uninf <-sim.pop$mortality*sim.pop$uninf
      
      #calculate total deaths (no one dies from TB or while on TB treatment since disease prevalence at t0 is 0)
      sim.pop <- sim.pop  %>% mutate(tot.dths=rowSums( 
        select(., "dths.uninf", "dths.new.prim.inf", "dths.prim.inf.1yr", "dths.prim.inf.2yr", "dths.prim.inf.3yr", "dths.prim.inf.4yr", "dths.prim.old", "dths.new.reinf.inf", "dths.reinf.inf.1yr", "dths.reinf.inf.2yr", "dths.reinf.inf.3yr", "dths.reinf.inf.4yr", "dths.reinf.old"))) 
      
      #check total pop
      sim.pop <- sim.pop  %>% mutate(tot.pop=rowSums( 
        select(., "uninf", "new.prim.inf", "inf.prim.1yr", "inf.prim.2yr", "inf.prim.3yr", "inf.prim.4yr", "old.prim.inf", "new.reinf.inf", "inf.reinf.1yr", "inf.reinf.2yr", "inf.reinf.3yr", "inf.reinf.4yr", "old.reinf.inf"))) 
      
      
      #initialize other variables we will need for later years 
      add <- data.frame(protect=rep(protect, dim(sim.pop)[1]), tot.dis.prog=rep(sum(disease_progression[1:5]), dim(sim.pop)[1]), tb.dis = rep(0, dim(sim.pop)[1]),  progress2tbprim = rep(0, dim(sim.pop)[1]),progress2tbreinf = rep(0, dim(sim.pop)[1]) ,tb.dths=rep(0, dim(sim.pop)[1]), tb.dths_allcause=rep(0, dim(sim.pop)[1]), cure.self=rep(0, dim(sim.pop)[1]), cure.trt = rep(0, dim(sim.pop)[1]), new.inf.previous = rep(0, dim(sim.pop)[1]),  ari_par=rep(0, dim(sim.pop)[1]) , sus.inf=rep(0, dim(sim.pop)[1]), reinf=rep(0, dim(sim.pop)[1]), new.reinf.inf.previous=rep(0, dim(sim.pop)[1]), new.prim.inf.previous=rep(NA, dim(sim.pop)[1]), rem=rep(0, dim(sim.pop)[1]), rem_act =rep(0, dim(sim.pop)[1]), progress2tb = rep(0, dim(sim.pop)[1]), rem_need = rep(0, dim(sim.pop)[1]) )
      
      sim.pop <- cbind(sim.pop, add)
      
      #save data frame to output after looping through all years 
      res <- sim.pop
      
      #print("check 1")
    }
    
    if(i>1){
      
      #save time step 
      sim.pop$time <- i
      
      
      #move TB self cures and treatment cures 
      sim.pop$uninf <- sim.pop$uninf + sim.pop$cure.trt
      sim.pop$new.prim.inf <- sim.pop$new.prim.inf + sim.pop$cure.self
      
      #remove deaths 
      sim.pop$new.prim.inf <- sim.pop$new.prim.inf  - sim.pop$dths.new.prim.inf
      sim.pop$inf.prim.1yr <- sim.pop$inf.prim.1yr - sim.pop$dths.prim.inf.1yr
      sim.pop$inf.prim.2yr <- sim.pop$inf.prim.2yr - sim.pop$dths.prim.inf.2yr
      sim.pop$inf.prim.3yr <- sim.pop$inf.prim.3yr - sim.pop$dths.prim.inf.3yr
      sim.pop$inf.prim.4yr <- sim.pop$inf.prim.4yr - sim.pop$dths.prim.inf.4yr
      sim.pop$old.prim.inf <- sim.pop$old.prim.inf - sim.pop$dths.prim.old
      sim.pop$new.reinf.inf <- sim.pop$new.reinf.inf  - sim.pop$dths.new.reinf.inf
      sim.pop$inf.reinf.1yr <- sim.pop$inf.reinf.1yr - sim.pop$dths.reinf.inf.1yr
      sim.pop$inf.reinf.2yr <- sim.pop$inf.reinf.2yr - sim.pop$dths.reinf.inf.2yr
      sim.pop$inf.reinf.3yr <- sim.pop$inf.reinf.3yr - sim.pop$dths.reinf.inf.3yr
      sim.pop$inf.reinf.4yr <- sim.pop$inf.reinf.4yr - sim.pop$dths.reinf.inf.4yr
      sim.pop$old.reinf.inf <- sim.pop$old.reinf.inf - sim.pop$dths.reinf.old
      sim.pop$uninf <- sim.pop$uninf - sim.pop$dths.uninf
      #print("check2")
      #update tb disease group
      sim.pop$rem = (sim.pop$progress2tb-sim.pop$tb.dths_allcause)/sim.pop$tb.dis
      
      #sim.pop$tb.dis.old = sim.pop$tb.dis
      
      sim.pop$tb.dis <- sim.pop$tb.dis - sim.pop$cure.self - sim.pop$cure.trt - sim.pop$tb.dths - sim.pop$tb.dths_allcause + sim.pop$progress2tbprim + sim.pop$progress2tbreinf
      
      sim.pop$rem_act = sim.pop$cure.self+sim.pop$tb.dths+sim.pop$cure.trt
      
      
      #add births to uninfected
      births <- sim.pop$tot.dths
      sim.pop$uninf <- sim.pop$uninf + births
      
      
      #save new infections from previous year 
      sim.pop$new.prim.inf.previous <- sim.pop$new.prim.inf
      sim.pop$new.reinf.inf.previous <- sim.pop$new.reinf.inf
      
      #apply ARI to uninfected and infected groups
      sim.pop$new.prim.inf = ari*sim.pop$uninf 
      
      sim.pop$new.reinf.inf = (sim.pop$new.reinf.inf.previous*(ari)*(1-dis.prob[1])  + ari*sim.pop$inf.reinf.1yr*(1-dis.prob[2]) +  ari*sim.pop$inf.reinf.2yr*(1-dis.prob[3]) + ari*sim.pop$inf.reinf.3yr*(1-dis.prob[4]) + ari*sim.pop$inf.reinf.4yr*(1-dis.prob[5]))*(1-protect)  + ari*sim.pop$old.reinf.inf*(1-dis.prob[6])+ sim.pop$new.prim.inf.previous*(ari)*(1-dis.prob[1])  + ari*sim.pop$inf.prim.1yr*(1-dis.prob[2]) +  ari*sim.pop$inf.prim.2yr*(1-dis.prob[3]) + ari*sim.pop$inf.prim.3yr*(1-dis.prob[4]) + ari*sim.pop$inf.prim.4yr*(1-dis.prob[5])  + ari*sim.pop$old.prim.inf*(1-dis.prob[6])
      
      
      #print("check3")
      
      #age infections (deducting those who progressed to disease or were reinfected)
      sim.pop$old.prim.inf <- sim.pop$old.prim.inf*(1-dis.prob[6])*(1-ari)  + sim.pop$inf.prim.4yr*(1-dis.prob[5])*(1-ari)
      sim.pop$inf.prim.4yr <- sim.pop$inf.prim.3yr*(1-dis.prob[4])*(1-ari)
      sim.pop$inf.prim.3yr <- sim.pop$inf.prim.2yr*(1-dis.prob[3])*(1-ari)
      sim.pop$inf.prim.2yr <- sim.pop$inf.prim.1yr*(1-dis.prob[2])*(1-ari)
      sim.pop$inf.prim.1yr <- sim.pop$new.prim.inf.previous*(1-dis.prob[1])*(1-ari) 
      
      sim.pop$old.reinf.inf <- sim.pop$old.reinf.inf*(1-dis.prob[6])*(1-ari)  + sim.pop$inf.reinf.4yr*(1-dis.prob[5])*(1-ari)*(1-protect)
      sim.pop$inf.reinf.4yr <- sim.pop$inf.reinf.3yr*(1-dis.prob[4])*(1-ari)*(1-protect)
      sim.pop$inf.reinf.3yr <- sim.pop$inf.reinf.2yr*(1-dis.prob[3])*(1-ari)*(1-protect)
      sim.pop$inf.reinf.2yr <- sim.pop$inf.reinf.1yr*(1-dis.prob[2])*(1-ari)*(1-protect)
      sim.pop$inf.reinf.1yr <- sim.pop$new.reinf.inf.previous*(1-dis.prob[1])*(1-ari)*(1-protect)
      
      #apply disease progression 
      sim.pop$progress2tbprim <- sim.pop$new.prim.inf*dis.prob[1] + sim.pop$inf.prim.1yr*dis.prob[2] + sim.pop$inf.prim.2yr*dis.prob[3] + sim.pop$inf.prim.3yr*dis.prob[4] + sim.pop$inf.prim.4yr*dis.prob[5]  + sim.pop$old.prim.inf*dis.prob[6]
      
      sim.pop$progress2tbreinf <- sim.pop$new.reinf.inf*dis.prob[1]*(1-protect) + sim.pop$inf.reinf.1yr*dis.prob[2]*(1-protect) + sim.pop$inf.reinf.2yr*dis.prob[3]*(1-protect) + sim.pop$inf.reinf.3yr*dis.prob[4]*(1-protect) + sim.pop$inf.reinf.4yr*dis.prob[5]*(1-protect)  + sim.pop$old.reinf.inf*dis.prob[6]
      
      sim.pop$progress2tb <- sim.pop$progress2tbprim + sim.pop$progress2tbreinf
      
      sim.pop$rem_need = sim.pop$progress2tb 
      
      #calculate deaths for infected and uninfected groups 
      #calculate deaths for 1-5 yr infections
      sim.pop$dths.prim.inf.1yr <-sim.pop$mortality*sim.pop$inf.prim.1yr
      sim.pop$dths.prim.inf.2yr <-sim.pop$mortality*sim.pop$inf.prim.2yr
      sim.pop$dths.prim.inf.3yr <-sim.pop$mortality*sim.pop$inf.prim.3yr
      sim.pop$dths.prim.inf.4yr <-sim.pop$mortality*sim.pop$inf.prim.4yr
      sim.pop$dths.prim.old <-sim.pop$mortality*sim.pop$old.prim.inf
      
      sim.pop$dths.reinf.inf.1yr <-sim.pop$mortality*sim.pop$inf.reinf.1yr
      sim.pop$dths.reinf.inf.2yr <-sim.pop$mortality*sim.pop$inf.reinf.2yr
      sim.pop$dths.reinf.inf.3yr <-sim.pop$mortality*sim.pop$inf.reinf.3yr
      sim.pop$dths.reinf.inf.4yr <-sim.pop$mortality*sim.pop$inf.reinf.4yr
      sim.pop$dths.reinf.old <-sim.pop$mortality*sim.pop$old.reinf.inf
      #print("check4")
      #calculate deaths for newly infected
      sim.pop$dths.new.prim.inf <-sim.pop$mortality*sim.pop$new.prim.inf
      sim.pop$dths.new.reinf.inf <-sim.pop$mortality*sim.pop$new.reinf.inf
      
      #calculate TB deaths, treatment cures, self cures
      sim.pop$cure.trt = sim.pop$tb.dis*sim.pop$tx.cure.rate
      sim.pop$self.cure.rate =((1- sim.pop$tx.cure.rate)/2)/5/2
      sim.pop$tb.dth.rate=((1- sim.pop$tx.cure.rate)/2)/5/2
      sim.pop$cure.self = sim.pop$tb.dis*sim.pop$self.cure.rate
      sim.pop$tb.dths = sim.pop$tb.dis*sim.pop$tb.dth.rate
      
      sim.pop$tb.dths_allcause <- (sim.pop$tb.dis)*sim.pop$mortality
      
      sim.pop$rem=((sim.pop$progress2tb-sim.pop$tb.dths_allcause)/sim.pop$tb.dis)
      
      if(stable==T & i >=start.trt){sim.pop$tx.cure.rate=((10*sim.pop$rem)-1)/9}
      sim.pop$cure.trt = sim.pop$tb.dis*sim.pop$tx.cure.rate
      
      sim.pop$self.cure.rate =((1- sim.pop$tx.cure.rate)/2)/5/2
      sim.pop$tb.dth.rate=((1- sim.pop$tx.cure.rate)/2)/5/2
      sim.pop$cure.self = sim.pop$tb.dis*sim.pop$self.cure.rate
      sim.pop$tb.dths = sim.pop$tb.dis*sim.pop$tb.dth.rate
      
      #calculate total deaths for each age group 
      sim.pop <- sim.pop  %>% mutate(tot.reinf.inf=rowSums( 
        select(.,  "new.reinf.inf", "inf.reinf.1yr", "inf.reinf.2yr", "inf.reinf.3yr", "inf.reinf.4yr", "old.reinf.inf"))) 
      
      sim.pop <- sim.pop  %>% mutate(tot.prim.inf=rowSums( 
        select(., "new.prim.inf", "inf.prim.1yr", "inf.prim.2yr", "inf.prim.3yr", "inf.prim.4yr", "old.prim.inf"))) 
      
      sim.pop$uninf <- sim.pop$tot.pop - sim.pop$tot.prim.inf - sim.pop$tot.reinf.inf - sim.pop$tb.dis
      
      #calculate deaths for uninfected 
      sim.pop$dths.uninf <-sim.pop$mortality*sim.pop$uninf
      
      #calculate total deaths
      sim.pop <- sim.pop  %>% mutate(tot.dths=rowSums( 
        select(.,  "dths.uninf", "dths.new.prim.inf", "dths.prim.inf.1yr", "dths.prim.inf.2yr", "dths.prim.inf.3yr", "dths.prim.inf.4yr", "dths.prim.old", "dths.new.reinf.inf", "dths.reinf.inf.1yr", "dths.reinf.inf.2yr", "dths.reinf.inf.3yr", "dths.reinf.inf.4yr", "dths.reinf.old", "tb.dths", "tb.dths_allcause"))) 
      
      sim.pop <- sim.pop  %>% mutate(tot.pop=rowSums( 
        select(., "uninf", "new.prim.inf", "inf.prim.1yr", "inf.prim.2yr", "inf.prim.3yr", "inf.prim.4yr", "old.prim.inf", "new.reinf.inf", "inf.reinf.1yr", "inf.reinf.2yr", "inf.reinf.3yr", "inf.reinf.4yr", "old.reinf.inf", "tb.dis")))
      
      res$tot.dis.prog = sum(dis.prob[1:5])
      res$protect=protect
      res$ari_par = ari
      res <- rbind(res, sim.pop)
      
    }
    
  } 
  
  return(res)
}
