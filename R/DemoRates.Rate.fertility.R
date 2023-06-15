
##main function for subset method
run.fertility.rates <- function(data, param, code, plot, method, mfp) {
  
  #### Parameters loading and data preparing ####
  
  nlb <- as.numeric(param$nlb)
  nhb <- as.numeric(param$nhb)
  nr2 <- as.numeric(param$nr2)
  fertM <- as.numeric(param$fertM)
  cohabit <- as.numeric(param$cohabit)
  marital <- ifelse(fertM==0, 0, ifelse(fertM==1&cohabit==0, 4, 7))
  t1Month <- as.numeric(param$t1Month)
  t2Month <- as.numeric(param$t2Month)
  nrj <- as.numeric(param$nrj)
  nRegion <- as.numeric(param$nRegion)
  edu <- as.numeric(param$edu)
  nRace <- as.numeric(param$nRace)
  nWeight <- as.numeric(param$nWeight)
  nrate <- as.numeric(param$nRate)
  

  #region list
  if (nRegion>1){
    region_tmp<- code %>% select(`Region Code`, `Region Name`)
    names(region_tmp) <- c("code", "name")
    region_tmp <- region_tmp %>% filter(!is.na(name))
    region_list <- as.list(region_tmp)
  }
  
  #race list
  if (nRace>1){
    race_tmp<- code %>% select(`Race Code`, `Race Name`)
    names(race_tmp) <- c("code", "name")
    race_tmp <- race_tmp %>% filter(!is.na(name))
    race_list <- as.list(race_tmp)
  }
  
  #Set output table and graph name surfix
  if(nRegion == 1 & nRace == 1 & nrj == 1 & edu == 0){
    combined.name <- ""
  } else {
    combined.name <- ", all"
  }
  
  
  #prepare oe & freq data
  #change wide data into long data (1 person month/row)
  data_oe <- data_freq <- NA
  #set variables indicating whether to calculate oe or frequency
  cal_freq <- cal_oe <- F
  
  if (nrate==1){
    cal_freq <- cal_oe <- T
    data_oe <- data_freq <- data.prepare.birth(data, marital, nr2, t1Month, t2Month, nlb, nhb)
  } else if (nrate==2){          #only oe
    cal_oe <- T
    data_oe <- data.prepare.birth(data, marital, nr2, t1Month, t2Month, nlb, nhb)
  } else if (nrate==3){          #only freq
    cal_freq <- T
    data_freq <- data.prepare.birth(data, marital, nr2, t1Month, t2Month, nlb, nhb)
  }
  
  #estimate by what variable? according to parameters
  #Remark: for subset method, always consider sex
  #overall rates, not by any factor
  if (nRegion == 1 & nrj == 1 & nRace == 1 & edu == 0) {subset <- "overall"}
  #by rural/urban
  if (nRegion == 1 & nrj == 2 & nRace == 1 & edu == 0) {subset <- "ru"}
  #by region
  if (nRegion > 1 & nRegion <= 100 & nrj == 1 & nRace == 1 & edu == 0) {subset <- "region"}
  #by race
  if (nRegion == 1 & nrj == 1 & nRace > 1 & edu == 0) {subset <- "race"}
  #by education
  if (nRegion == 1 & nrj == 1 & nRace == 1 & edu == 1) {subset <- "edu"}
  #special case: by region & rural/urban
  if (nrj==2 & nRegion > 1 & nRegion <= 100 & nRace == 1 & edu == 0) {subset <- "reg.ru"}
  
  if (fertM==0){marsuff <- ""} else if (fertM==1){marsuff <- paste0(" ", marital, " marital status")}
  
  #define code list
  if (subset=="region"){
    i.list <- region_list
  } else if (subset=="ru") {
    i.list <- list(code=c(1,2), name=c("rural", "urban"))
  } else if (subset=="edu"){
    i.list <- list(code=c(1:5),
                   name=c("No education", "Primary school",
                          "Middle school", "High school", "College or higher"))
  } else if (subset=="race"){
    i.list <- race_list
  }
  
  
  if(fertM == 0){
    
    #### estimate not by marital status ####
    
    #generate pop table: # of risk population and # of events
    #used to check direct calculation results
    if (cal_oe==T){
      pop <- pop.count.fert(data_oe, nr2, nlb, nhb)
      write.pop.fert(pop, param, name = combined.name)
    }
    
    if (cal_freq==T){
      #if have not generated pop table in the previous step (oe), generate it.
      #if have already generated, omit.
      if (cal_oe==F){
        pop <- pop.count.fert(data_freq, nr2, nlb, nhb)
        write.pop.fert(pop, param, name =  combined.name)
      }
    }
    
    #estimate overall rates
    combined <- compute.fert(data_oe, data_freq, param, plot, plot.name=combined.name, method, mfp)
    #output overall rates
    write.rates.fert(combined$oe.rates, combined$frequency, param, combined.name, paste0(method, " estimate", sep=""))
    write.rates.fert(combined$raw.oe.rates, combined$raw.frequency, param, combined.name, "Direct calculate")

    
    # if do not estimate by any variable,
    # generate and output overall total rates and mean ages
    if (subset=="overall"){
      total.rates <- combined$total.rates
      mean.age <- combined$mean.age
      write.total.fert.subset(total.rates, mean.age, param, combined.name, marsuff)
    }
    

    # if by rural/urban, region, race, edu
    if (subset %in% c("ru", "region", "race", "edu")){
      
      #overall total rates
      total.rates <- combined$total.rates
      #add an empty row to indicate variable name when output total rates file
      total.rates <- rbind(NA, total.rates)
      #overall mean ages
      mean.age <- combined$mean.age
      #add an empty row to indicate variable name when output total rates file
      mean.age <- rbind(NA, mean.age)
      
      #define general row names
      rowname <- c("Total rates - All combined",
                   "Direct calculate",  paste0(method, " estimate", sep=""), "Difference%")
      total.rates <- cbind(rowname, total.rates)
      rowname <- c("Mean age - All combined",
                   "Direct calculate",  paste0(method, " estimate", sep=""), "Difference%")
      mean.age <- cbind(rowname, mean.age)
      
      #run for each list code
      for(k in 1:length(i.list[[1]])){
        
        #code
        i <- i.list[[1]][k]
        #name
        i.name <- i.list[[2]][k]
        #code for output surfix
        i.code <- paste0(", ", i.name, sep="")
        
        ## Set rates as NA first, change later if cal_oe/cal_freq==T
        data_oe.i <- data_freq.i <- NA
        
        if (cal_oe==T){
          #Subset oe data according to the variable
          data_oe.i <- data_oe[which(eval(parse(text=paste("data_oe$",subset,sep="")))==i),]
          
          #generate pop table: # of risk population and # of events
          pop <- pop.count.fert(data_oe.i, nr2, nlb, nhb)
          write.pop.fert(pop, param, i.code)
        }
        
        if (cal_freq==T){
          
          #Subset freq data according to the variable
          data_freq.i <- data_freq[which(eval(parse(text=paste("data_freq$",subset,sep="")))==i),]
          
          if (cal_oe==F){
            # if do not estimate by any variable,
            # generate and output overall total rates and mean ages
            pop <- pop.count.fert(data_freq.i, nr2, nlb, nhb)
            write.pop.fert(pop, param, i.code)
          }
          
        }
        
        #estimate oe/freq rates based on subset data
        i <- compute.fert(data_oe.i, data_freq.i, plot=plot, param=param, plot.name=i.code, method, mfp)
        #output by variable results
        write.rates.fert(i$oe.rates, i$frequency, param, i.code, paste0(method, " estimate", sep=""))
        write.rates.fert(i$raw.oe.rates, i$raw.frequency, param, i.code, "Direct calculate")
        
        #define subset row names
        rowname <- c(i.name, "Direct calculate",  paste0(method, " estimate", sep=""), "Difference%")
        #add an empty row for variable name
        i$total.rates <- rbind(NA, i$total.rates)
        #add row names to total rates
        i$total.rates <- cbind(rowname, i$total.rates)
        #add subset total rates to general total rates
        total.rates <- rbind(total.rates, i$total.rates)
        #add an empty row for variable name
        i$mean.age <- rbind(NA, i$mean.age)
        #add row names to mean ages
        i$mean.age <- cbind(rowname, i$mean.age)
        #add subset mean age to general mean age
        mean.age <- rbind(mean.age, i$mean.age)
        
      }
      
      #output total rates file
      write.total.fert.subset(total.rates, mean.age, param, paste0("-all ", subset, sep=""), marsuff)
      
    }
    
  } else{
    
    #### estimate by marital status ####
    
    #generate pop table: # of risk population and # of events
    #used to check direct calculation results
    if (cal_oe==T){
      pop <- pop.count.fertM(data_oe, param, nr2, nlb, nhb)
      write.pop.fertM(pop, param, name = combined.name)
    }
    
    if (cal_freq==T){
      #if have not generated pop table in the previous step (oe), generate it.
      #if have already generated, omit.
      if (cal_oe==F){
        pop <- pop.count.fertM(data_freq, param, nr2, nlb, nhb)
        write.pop.fertM(pop, param, name =  combined.name)
      }
    }
    
    #estimate overall rates
    combined <- compute.fertM(data_oe, data_freq, param, plot, plot.name=combined.name, method, mfp)
    #output overall rates
    write.rates.fertM(combined$oe.rates, combined$frequency, param, combined.name, paste0(method, " estimate", sep=""))
    write.rates.fertM(combined$raw.oe.rates, combined$raw.frequency, param, combined.name, "Direct calculate")
    
    
    # if do not estimate by any variable,
    # generate and output overall total rates and mean ages
    if (subset=="overall"){
      total.rates <- combined$total.rates
      mean.age <- combined$mean.age
      write.total.fert.subset(total.rates, mean.age, param, combined.name, marsuff)
    }
    
    
    # if by rural/urban, region, race, edu
    if (subset %in% c("ru", "region", "race", "edu")){
      
      #overall total rates
      total.rates <- combined$total.rates
      #add an empty row to indicate variable name when output total rates file
      total.rates <- rbind(NA, total.rates)
      #overall mean ages
      mean.age <- combined$mean.age
      #add an empty row to indicate variable name when output total rates file
      mean.age <- rbind(NA, mean.age)
      
      #define general row names
      rowname <- c("Total rates - All combined",
                   "Direct calculate",  paste0(method, " estimate", sep=""), "Difference%")
      total.rates <- cbind(rowname, total.rates)
      rowname <- c("Mean age - All combined",
                   "Direct calculate",  paste0(method, " estimate", sep=""), "Difference%")
      mean.age <- cbind(rowname, mean.age)
      
      #run for each list code
      for(k in 1:length(i.list[[1]])){
        
        #code
        i <- i.list[[1]][k]
        #name
        i.name <- i.list[[2]][k]
        #code for output surfix
        i.code <- paste0(", ", i.name, sep="")
        
        ## Set rates as NA first, change later if cal_oe/cal_freq==T
        data_oe.i <- data_freq.i <- NA
        
        if (cal_oe==T){
          #Subset oe data according to the variable
          data_oe.i <- data_oe[which(eval(parse(text=paste("data_oe$",subset,sep="")))==i),]
          
          #generate pop table: # of risk population and # of events
          pop <- pop.count.fertM(data_oe.i, param, nr2, nlb, nhb)
          write.pop.fertM(pop, param, i.code)
        }
        
        if (cal_freq==T){
          
          #Subset freq data according to the variable
          data_freq.i <- data_freq[which(eval(parse(text=paste("data_freq$",subset,sep="")))==i),]
          
          if (cal_oe==F){
            # if do not estimate by any variable,
            # generate and output overall total rates and mean ages
            pop <- pop.count.fertM(data_freq.i, param, nr2, nlb, nhb)
            write.pop.fertM(pop, param, i.code)
          }
          
        }
        
        #estimate oe/freq rates based on subset data
        i <- compute.fertM(data_oe.i, data_freq.i, plot=plot, param=param, plot.name=i.code, method, mfp)
        #output by variable results
        write.rates.fertM(i$oe.rates, i$frequency, param, i.code, paste0(method, " estimate", sep=""))
        write.rates.fertM(i$raw.oe.rates, i$raw.frequency, param, i.code, "Direct calculate")
        
        #define subset row names
        rowname <- c(i.name, "Direct calculate",  paste0(method, " estimate", sep=""), "Difference%")
        #add an empty row for variable name
        i$total.rates <- rbind(NA, i$total.rates)
        #add row names to total rates
        i$total.rates <- cbind(rowname, i$total.rates)
        #add subset total rates to general total rates
        total.rates <- rbind(total.rates, i$total.rates)
        #add an empty row for variable name
        i$mean.age <- rbind(NA, i$mean.age)
        #add row names to mean ages
        i$mean.age <- cbind(rowname, i$mean.age)
        #add subset mean age to general mean age
        mean.age <- rbind(mean.age, i$mean.age)
        
      }
      
      #output total rates file
      write.total.fert.subset(total.rates, mean.age, param, paste0("-all ", subset, sep=""), marsuff)
      
    }
    
    
  }
}

##main function for covariate method
run.fertility.rates.covar <- function(data, param, code, plot, mfp) {
  
  #### Parameters loading and data preparing ####
  
  nlb <- as.numeric(param$nlb)
  nhb <- as.numeric(param$nhb)
  nr2 <- as.numeric(param$nr2)
  fertM <- as.numeric(param$fertM)
  cohabit <- as.numeric(param$cohabit)
  marital <- ifelse(fertM==0, 0, ifelse(fertM==1&cohabit==0, 4, 7))
  t1Month <- as.numeric(param$t1Month)
  t2Month <- as.numeric(param$t2Month)
  nrj <- as.numeric(param$nrj)
  nRegion <- as.numeric(param$nRegion)
  edu <- as.numeric(param$edu)
  nRace <- as.numeric(param$nRace)
  nWeight <- as.numeric(param$nWeight)
  nrate <- as.numeric(param$nRate)
  
  
  #region list
  if (nRegion>1){
    region_tmp<- code %>% select(`Region Code`, `Region Name`)
    names(region_tmp) <- c("code", "name")
    region_tmp <- region_tmp %>% filter(!is.na(name))
    region_list <- as.list(region_tmp)
  }
  
  #race list
  if (nRace>1){
    race_tmp<- code %>% select(`Race Code`, `Race Name`)
    names(race_tmp) <- c("code", "name")
    race_tmp <- race_tmp %>% filter(!is.na(name))
    race_list <- as.list(race_tmp)
  }
  
  #Set output table and graph name surfix
  if(nRegion == 1 & nRace == 1 & nrj == 1 & edu == 0){
    combined.name <- ""
  } else {
    combined.name <- ", all"
  }
  
  
  #prepare oe & freq data
  #change wide data into long data (1 person month/row)
  data_oe <- data_freq <- NA
  #set variables indicating whether to calculate oe or frequency
  cal_freq <- cal_oe <- F
  
  if (nrate==1){
    cal_freq <- cal_oe <- T
    data_oe <- data_freq <- data.prepare.birth(data, marital, nr2, t1Month, t2Month, nlb, nhb)
  } else if (nrate==2){          #only oe
    cal_oe <- T
    data_oe <- data.prepare.birth(data, marital, nr2, t1Month, t2Month, nlb, nhb)
  } else if (nrate==3){          #only freq
    cal_freq <- T
    data_freq <- data.prepare.birth(data, marital, nr2, t1Month, t2Month, nlb, nhb)
  }
  

  ## Common settings for all cases ##
  
  
  ##estimate by what variable? define "byvar" according to parameters
  #sex.nosex -- general estimation, not by any factor
  if (nRegion == 1 & nrj == 1 & nRace == 1 & edu == 0) {byvar<- "sex.nosex"}
  #region.nosex -- only by region
  if (nRegion > 1 & nRegion <= 100 & nrj == 1 & nRace == 1 & edu == 0) {byvar<- "region.nosex"}
  #ru.nosex -- only by rural/urban
  if (nRegion == 1 & nrj == 2 & nRace == 1 & edu == 0) {byvar<- "ru.nosex"}
  #race.nosex -- only by race
  if (nRegion == 1 & nrj == 1 & nRace > 1 & edu == 0) {byvar<- "race.nosex"}
  #reg.ru.nosex -- by region & rural/urban
  if (nrj==2 & nRegion > 1 & nRegion <= 100 & nRace == 1 & edu == 0) {byvar<- "reg.ru.nosex"}

  #set row names for final total rates output: same for all cases
  rowname <- c("Direct calculate", "Poisson estimate", "Difference%")
  
  if (fertM==0){marsuff <- ""} else if (fertM==1){marsuff <- paste0(" ", marital, " marital status")}
  
  
  if(fertM == 0){
    
    #### estimate not by marital status ####
    
    #estimated oe, raw oe, estimated freq, raw freq:
    #The result is for all cases
    est.covar <- compute.fert.covar(data_oe, data_freq, param, code, plot, plot.name=combined.name, byvar, mfp)
    
    ## Set rates as NA first, change later if cal_oe/cal_freq==T
    if (byvar=="reg.ru.nosex"){
      # for reg.ru & reg.ru.nosex only
      oe.rates.ru <- raw.oe.rates.ru <- frequency.ru <- raw.frequency.ru <- NA
      oe.rates.ub <- raw.oe.rates.ub <- frequency.ub <- raw.frequency.ub <- NA
    } else {
      # for all cases other than reg.ru & reg.ru.nosex
      oe.rates <- raw.oe.rates <- frequency <- raw.frequency <- NA
    }
    
    ## generate empty data frames to store total rates and mean ages
    if (byvar=="reg.ru.nosex"){
      # for reg.ru & reg.ru.nosex only
      total.rates.ru <- data.frame(t(rep(NA, 10)))[F,]
      total.rates.ub <- data.frame(t(rep(NA, 10)))[F,]
      mean.age.ru <- data.frame(t(rep(NA, 10)))[F,]
      mean.age.ub <- data.frame(t(rep(NA, 10)))[F,]
    } else {
      # for all cases other than reg.ru & reg.ru.nosex
      total.rates <- data.frame(t(rep(NA, 8)))[F,]
      mean.age <- data.frame(t(rep(NA, 8)))[F,]
    }
    
    
    #Case sex.nosex
    if (byvar=="sex.nosex") {
      
      if (cal_oe==T){
        
        #generate pop table: # of risk population and # of events
        pop <- pop.count.fert(data_oe, nr2, nlb, nhb)
        write.pop.fert(pop, param, name = combined.name)
        
        #extract estimated oe and raw oe
        oe.rates <- est.covar$oe.rates %>% arrange(age)
        raw.oe.rates <- est.covar$raw.oe.rates %>% arrange(age)
        
      }
      
      if (cal_freq==T){
        
        #if have not generated pop table in the previous step (oe), generate it.
        #if have already generated, omit.
        if(cal_oe==F){
          pop <- pop.count.fert(data_freq, nr2, nlb, nhb)
          write.pop.fert(pop, param, name = combined.name)
        }
        
        #extract estimated freq and raw freq
        frequency <- est.covar$frequency %>% arrange(age)
        raw.frequency <- est.covar$raw.frequency %>% arrange(age)
        
        #Calculate estimated total rates and mean age based on estimated freq.
        frequency$All <- tmp.rowSum.w.na(frequency[2:ncol(frequency)])
        frequency <- frequency %>% relocate(All, .after = age)
        
        est.total.rates <- total.freq(frequency)
        est.mean.age <- mean.age.cal(frequency)
        
        freq.total <- data.frame(age = "Total", total.freq(frequency))
        frequency <- rbind(frequency, freq.total)
        rm(freq.total)
        
        #Calculate raw total rates and mean age based on raw freq.
        raw.frequency$All <- tmp.rowSum.w.na(raw.frequency[2:ncol(raw.frequency)])
        raw.frequency <- raw.frequency %>% relocate(All, .after = age)
        
        raw.total.rates <- total.freq(raw.frequency)
        raw.mean.age <- mean.age.cal(raw.frequency)
        
        freq.total <- data.frame(age = "Total", total.freq(raw.frequency))
        raw.frequency <- rbind(raw.frequency, freq.total)
        rm(freq.total)
        
        #combine estimated and raw total rates and frequencies, and calculate difference
        total.rates <- rbind(raw.total.rates, est.total.rates, (est.total.rates-raw.total.rates)/raw.total.rates)
        mean.age <- rbind(raw.mean.age, est.mean.age, (est.mean.age-raw.mean.age)/raw.mean.age)
        total.rates <- do.call(data.frame, lapply(total.rates, function(x) replace(x, is.infinite(x), NA)))
        
        write.total.fert.covar(total.rates, mean.age, param, combined.name, marsuff)
        
      }
      
      #output estimated rates
      write.rates.fert(oe.rates, frequency, param, combined.name, "Poisson estimate")
      #output raw rates
      write.rates.fert(raw.oe.rates, raw.frequency, param, combined.name, "Direct calculate")
      
    }
    
    
    # if by rural/urban, region, race, edu
    if (byvar %in% c("ru.nosex", "region.nosex", "race.nosex")){
      
      #define code list & varable name
      if (byvar=="region.nosex"){
        i.list <- region_list
        var <- "region"
      } else if (byvar=="ru.nosex") {
        i.list <- list(code=c(1,2), name=c("rural", "urban"))
        var <- "ru"
      } else if (byvar=="race.nosex"){
        i.list <- race_list
        var <- "race"
      }
      
      #run for each list code
      for(k in 1:length(i.list[[1]])){
        
        #code
        i <- i.list[[1]][k]
        #name
        i.name <- i.list[[2]][k]
        #code for rowname
        i.rowname <- rep(i.name, 3)
        #code for output surfix
        i.code <- paste0(", ", i.name, sep="")
        
        if (cal_oe==T){
          
          #generate pop table: # of risk population and # of events
          pop <- pop.count.fert(data_oe[which(eval(parse(text=paste("data_oe$",var,sep="")))==i),], nr2, nlb, nhb)
          write.pop.fert(pop, param, i.code)
          
          #extract estimated & direct calculated oe rates from estimation result
          oe.rates <- est.covar$oe.rates
          raw.oe.rates <- est.covar$raw.oe.rates
          #keep oe rates by variable
          oe.rates <- oe.rates[which(eval(parse(text = paste("oe.rates$",var,sep=""))) == i), -which(names(oe.rates)==var)] %>% arrange(age)
          raw.oe.rates <- raw.oe.rates[which(eval(parse(text = paste("raw.oe.rates$",var,sep=""))) == i), -which(names(raw.oe.rates)==var)] %>% arrange(age)
          
        }
        
        if (cal_freq==T){
          
          if (cal_oe==F){
            
            #if have not generated pop table in the previous step (oe), generate it.
            #if have already generated, omit.
            pop <- pop.count.fert(data_freq[which(eval(parse(text=paste("data_freq$",var,sep="")))==i),], nr2, nlb, nhb)
            write.pop.fert(pop, param, i.code)
            
          }
          
          #extract estimated & direct calculated freq rates from estimation result
          frequency <- est.covar$frequency
          raw.frequency <- est.covar$raw.frequency
          #keep freq rates by variable
          frequency <- frequency[which(eval(parse(text = paste("frequency$",var,sep=""))) == i), -which(names(frequency)==var)] %>% arrange(age)
          raw.frequency <- raw.frequency[which(eval(parse(text = paste("raw.frequency$",var,sep=""))) == i), -which(names(raw.frequency)==var)] %>% arrange(age)
          
          #Calculate estimated total rates and mean age based on estimated freq.
          frequency$All <- tmp.rowSum.w.na(frequency[2:ncol(frequency)])
          frequency <- frequency %>% relocate(All, .after = age)
          
          est.total.rates <- total.freq(frequency)
          est.mean.age <- mean.age.cal(frequency)
          
          freq.total <- data.frame(age = "Total", total.freq(frequency))
          frequency <- rbind(frequency, freq.total)
          rm(freq.total)
          
          #Calculate raw total rates and mean age based on raw freq.
          raw.frequency$All <- tmp.rowSum.w.na(raw.frequency[2:ncol(raw.frequency)])
          raw.frequency <- raw.frequency %>% relocate(All, .after = age)
          
          raw.total.rates <- total.freq(raw.frequency)
          raw.mean.age <- mean.age.cal(raw.frequency)
          
          freq.total <- data.frame(age = "Total", total.freq(raw.frequency))
          raw.frequency <- rbind(raw.frequency, freq.total)
          rm(freq.total)
          
          i.total.rates <- rbind(raw.total.rates, est.total.rates, (est.total.rates-raw.total.rates)/raw.total.rates)
          i.mean.age <- rbind(raw.mean.age, est.mean.age, (est.mean.age-raw.mean.age)/raw.mean.age)
          i.total.rates <- do.call(data.frame, lapply(i.total.rates, function(x) replace(x, is.infinite(x), NA)))
          
          #adjust format for ease of output
          i.total.rates <- cbind(i.rowname, rowname, i.total.rates)
          total.rates <- rbind(total.rates, i.total.rates)
          i.mean.age <- cbind(i.rowname, rowname, i.mean.age)
          mean.age <- rbind(mean.age, i.mean.age)
          
        }
        
        #output rates
        write.rates.fert(oe.rates, frequency, param, i.code, "Poisson estimate")
        write.rates.fert(raw.oe.rates, raw.frequency, param, i.code, "Direct calculate")
        
      }
      
      #output total rates
      write.total.fert.covar(total.rates, mean.age, param, paste0("-by ", var, sep=""), marsuff)
      
    }
    
  } else{
    
    #### estimate by marital status ####
    
    #estimated oe, raw oe, estimated freq, raw freq:
    #The result is for all cases
    est.covar <- compute.fertM.covar(data_oe, data_freq, param, code, plot, plot.name=combined.name, byvar, mfp)
    
    ## Set rates as NA first, change later if cal_oe/cal_freq==T
    if (byvar=="reg.ru.nosex"){
      # for reg.ru & reg.ru.nosex only
      oe.rates.ru <- raw.oe.rates.ru <- frequency.ru <- raw.frequency.ru <- NA
      oe.rates.ub <- raw.oe.rates.ub <- frequency.ub <- raw.frequency.ub <- NA
    } else {
      # for all cases other than reg.ru & reg.ru.nosex
      oe.rates <- raw.oe.rates <- frequency <- raw.frequency <- NA
    }
    
    ## generate empty data frames to store total rates and mean ages
    if (byvar=="reg.ru.nosex"){
      # for reg.ru & reg.ru.nosex only
      total.rates.ru <- data.frame(t(rep(NA, 10)))[F,]
      total.rates.ub <- data.frame(t(rep(NA, 10)))[F,]
      mean.age.ru <- data.frame(t(rep(NA, 10)))[F,]
      mean.age.ub <- data.frame(t(rep(NA, 10)))[F,]
    } else {
      # for all cases other than reg.ru & reg.ru.nosex
      total.rates <- data.frame(t(rep(NA, 8)))[F,]
      mean.age <- data.frame(t(rep(NA, 8)))[F,]
    }
    
    
    if (cohabit==0){
      
      #Case sex.nosex
      if (byvar=="sex.nosex") {
        
        if (cal_oe==T){
          
          #generate pop table: # of risk population and # of events
          pop <- pop.count.fertM(data_oe, param, nr2, nlb, nhb)
          write.pop.fertM(pop, param, name = combined.name)
          
          #extract estimated oe and raw oe
          oe.rates <- est.covar$oe.rates %>% arrange(age)
          raw.oe.rates <- est.covar$raw.oe.rates %>% arrange(age)
          
        }
        
        if (cal_freq==T){
          
          #if have not generated pop table in the previous step (oe), generate it.
          #if have already generated, omit.
          if(cal_oe==F){
            pop <- pop.count.fertM(data_freq, param, nr2, nlb, nhb)
            write.pop.fertM(pop, param, name = combined.name)
          }
          
          #extract estimated freq and raw freq
          frequency <- est.covar$frequency %>% arrange(age)
          raw.frequency <- est.covar$raw.frequency %>% arrange(age)
          
          #Calculate estimated total rates and mean age based on estimated freq.
          frequency$All <- tmp.rowSum.w.na(frequency[2:ncol(frequency)])
          frequency <- frequency %>% relocate(All, .after = age)
          
          #non marital frequency
          nonmar.freq <- data.frame(age = frequency$age, All = frequency$All)
          for (i in 1:nr2) {
            nonmar.freq <- data.frame(nonmar.freq, tmp.rowSum.w.na(frequency[seq(2+i,4*nr2+2, by=nr2)]))
            names(nonmar.freq)[i+2] <- paste0("p", i, ".freq")
          }
          
          #add non marital and total for each marital status
          frequency <- data.frame(nonmar.freq, frequency[3:ncol(frequency)])
          frequency$all.nm <- tmp.rowSum.w.na(frequency[(3+nr2):(2+2*nr2)])
          frequency$all.m <- tmp.rowSum.w.na(frequency[(3+2*nr2):(2+3*nr2)])
          frequency$all.d <- tmp.rowSum.w.na(frequency[(3+3*nr2):(2+4*nr2)])
          frequency$all.w <- tmp.rowSum.w.na(frequency[(3+4*nr2):(2+5*nr2)])
          
          frequency <- frequency %>% relocate(all.nm, .before = pnm1.freq)
          frequency <- frequency %>% relocate(all.m, .before = pm1.freq)
          frequency <- frequency %>% relocate(all.d, .before = pd1.freq)
          frequency <- frequency %>% relocate(all.w, .before = pw1.freq)
          
          freq.total <- data.frame(age = "Total",  total.freq(frequency))
          frequency <- rbind(frequency, freq.total)
          rm(freq.total)
          
          est.total.rates <- total.freq(nonmar.freq)
          est.mean.age <- mean.age.cal(nonmar.freq)
          
          #Calculate raw total rates and mean age based on raw freq.
          raw.frequency$All <- tmp.rowSum.w.na(raw.frequency[2:ncol(raw.frequency)])
          raw.frequency <- raw.frequency %>% relocate(All, .after = age)
          
          #non marital frequency
          nonmar.freq <- data.frame(age = raw.frequency$age, All = raw.frequency$All)
          for (i in 1:nr2) {
            nonmar.freq <- data.frame(nonmar.freq, tmp.rowSum.w.na(raw.frequency[seq(2+i,4*nr2+2, by=nr2)]))
            names(nonmar.freq)[i+2] <- paste0("p", i, ".freq")
          }
          
          #add non marital and total for each marital status
          raw.frequency <- data.frame(nonmar.freq, raw.frequency[3:ncol(raw.frequency)])
          raw.frequency$all.nm <- tmp.rowSum.w.na(raw.frequency[(3+nr2):(2+2*nr2)])
          raw.frequency$all.m <- tmp.rowSum.w.na(raw.frequency[(3+2*nr2):(2+3*nr2)])
          raw.frequency$all.d <- tmp.rowSum.w.na(raw.frequency[(3+3*nr2):(2+4*nr2)])
          raw.frequency$all.w <- tmp.rowSum.w.na(raw.frequency[(3+4*nr2):(2+5*nr2)])
          
          raw.frequency <- raw.frequency %>% relocate(all.nm, .before = pnm1.freq)
          raw.frequency <- raw.frequency %>% relocate(all.m, .before = pm1.freq)
          raw.frequency <- raw.frequency %>% relocate(all.d, .before = pd1.freq)
          raw.frequency <- raw.frequency %>% relocate(all.w, .before = pw1.freq)
          
          raw.total.rates <- total.freq(nonmar.freq)
          raw.mean.age <- mean.age.cal(nonmar.freq)
          
          freq.total <- data.frame(age = "Total", total.freq(raw.frequency))
          raw.frequency <- rbind(raw.frequency, freq.total)
          rm(freq.total)
          
          #combine estimated and raw total rates and frequencies, and calculate difference
          total.rates <- rbind(raw.total.rates, est.total.rates, (est.total.rates-raw.total.rates)/raw.total.rates)
          mean.age <- rbind(raw.mean.age, est.mean.age, (est.mean.age-raw.mean.age)/raw.mean.age)
          total.rates <- do.call(data.frame, lapply(total.rates, function(x) replace(x, is.infinite(x), NA)))
          
          write.total.fert.covar(total.rates, mean.age, param, combined.name, marsuff)
          
        }
        
        #output estimated rates
        write.rates.fertM(oe.rates, frequency, param, combined.name, "Poisson estimate")
        #output raw rates
        write.rates.fertM(raw.oe.rates, raw.frequency, param, combined.name, "Direct calculate")
        
      }
      
      
      # if by rural/urban, region, race, edu
      if (byvar %in% c("ru.nosex", "region.nosex", "race.nosex")){
        
        #define code list & varable name
        if (byvar=="region.nosex"){
          i.list <- region_list
          var <- "region"
        } else if (byvar=="ru.nosex") {
          i.list <- list(code=c(1,2), name=c("rural", "urban"))
          var <- "ru"
        } else if (byvar=="race.nosex"){
          i.list <- race_list
          var <- "race"
        }
        
        #run for each list code
        for(k in 1:length(i.list[[1]])){
          
          #code
          i <- i.list[[1]][k]
          #name
          i.name <- i.list[[2]][k]
          #code for rowname
          i.rowname <- rep(i.name, 3)
          #code for output surfix
          i.code <- paste0(", ", i.name, sep="")
          
          if (cal_oe==T){
            
            #generate pop table: # of risk population and # of events
            pop <- pop.count.fertM(data_oe[which(eval(parse(text=paste("data_oe$",var,sep="")))==i),], param, nr2, nlb, nhb)
            write.pop.fertM(pop, param, i.code)
            
            #extract estimated & direct calculated oe rates from estimation result
            oe.rates <- est.covar$oe.rates
            raw.oe.rates <- est.covar$raw.oe.rates
            #keep oe rates by variable
            oe.rates <- oe.rates[which(eval(parse(text = paste("oe.rates$",var,sep=""))) == i), -which(names(oe.rates)==var)] %>% arrange(age)
            raw.oe.rates <- raw.oe.rates[which(eval(parse(text = paste("raw.oe.rates$",var,sep=""))) == i), -which(names(raw.oe.rates)==var)] %>% arrange(age)
            
          }
          
          if (cal_freq==T){
            
            if (cal_oe==F){
              
              #if have not generated pop table in the previous step (oe), generate it.
              #if have already generated, omit.
              pop <- pop.count.fertM(data_freq[which(eval(parse(text=paste("data_freq$",var,sep="")))==i),], param, nr2, nlb, nhb)
              write.pop.fertM(pop, param, i.code)
              
            }
            
            #extract estimated & direct calculated freq rates from estimation result
            frequency <- est.covar$frequency %>% arrange(age)
            raw.frequency <- est.covar$raw.frequency %>% arrange(age)
            #keep freq rates by variable
            frequency <- frequency[which(eval(parse(text = paste("frequency$",var,sep=""))) == i), -which(names(frequency)==var)] %>% arrange(age)
            raw.frequency <- raw.frequency[which(eval(parse(text = paste("raw.frequency$",var,sep=""))) == i), -which(names(raw.frequency)==var)] %>% arrange(age)
            
            #Calculate estimated total rates and mean age based on estimated freq.
            frequency$All <- tmp.rowSum.w.na(frequency[2:ncol(frequency)])
            frequency <- frequency %>% relocate(All, .after = age)
            
            #non marital frequency
            nonmar.freq <- data.frame(age = frequency$age, All = frequency$All)
            for (i in 1:nr2) {
              nonmar.freq <- data.frame(nonmar.freq, tmp.rowSum.w.na(frequency[seq(2+i,4*nr2+2, by=nr2)]))
              names(nonmar.freq)[i+2] <- paste0("p", i, ".freq")
            }
            
            #add non marital and total for each marital status
            frequency <- data.frame(nonmar.freq, frequency[3:ncol(frequency)])
            frequency$all.nm <- tmp.rowSum.w.na(frequency[(3+nr2):(2+2*nr2)])
            frequency$all.m <- tmp.rowSum.w.na(frequency[(3+2*nr2):(2+3*nr2)])
            frequency$all.d <- tmp.rowSum.w.na(frequency[(3+3*nr2):(2+4*nr2)])
            frequency$all.w <- tmp.rowSum.w.na(frequency[(3+4*nr2):(2+5*nr2)])
            
            frequency <- frequency %>% relocate(all.nm, .before = pnm1.freq)
            frequency <- frequency %>% relocate(all.m, .before = pm1.freq)
            frequency <- frequency %>% relocate(all.d, .before = pd1.freq)
            frequency <- frequency %>% relocate(all.w, .before = pw1.freq)
            
            freq.total <- data.frame(age = "Total",  total.freq(frequency))
            frequency <- rbind(frequency, freq.total)
            rm(freq.total)
            
            est.total.rates <- total.freq(nonmar.freq)
            est.mean.age <- mean.age.cal(nonmar.freq)
            
            #Calculate raw total rates and mean age based on raw freq.
            raw.frequency$All <- tmp.rowSum.w.na(raw.frequency[2:ncol(raw.frequency)])
            raw.frequency <- raw.frequency %>% relocate(All, .after = age)
            
            #non marital frequency
            nonmar.freq <- data.frame(age = raw.frequency$age, All = raw.frequency$All)
            for (i in 1:nr2) {
              nonmar.freq <- data.frame(nonmar.freq, tmp.rowSum.w.na(raw.frequency[seq(2+i,4*nr2+2, by=nr2)]))
              names(nonmar.freq)[i+2] <- paste0("p", i, ".freq")
            }
            
            #add non marital and total for each marital status
            raw.frequency <- data.frame(nonmar.freq, raw.frequency[3:ncol(raw.frequency)])
            raw.frequency$all.nm <- tmp.rowSum.w.na(raw.frequency[(3+nr2):(2+2*nr2)])
            raw.frequency$all.m <- tmp.rowSum.w.na(raw.frequency[(3+2*nr2):(2+3*nr2)])
            raw.frequency$all.d <- tmp.rowSum.w.na(raw.frequency[(3+3*nr2):(2+4*nr2)])
            raw.frequency$all.w <- tmp.rowSum.w.na(raw.frequency[(3+4*nr2):(2+5*nr2)])
            
            raw.frequency <- raw.frequency %>% relocate(all.nm, .before = pnm1.freq)
            raw.frequency <- raw.frequency %>% relocate(all.m, .before = pm1.freq)
            raw.frequency <- raw.frequency %>% relocate(all.d, .before = pd1.freq)
            raw.frequency <- raw.frequency %>% relocate(all.w, .before = pw1.freq)
            
            raw.total.rates <- total.freq(nonmar.freq)
            raw.mean.age <- mean.age.cal(nonmar.freq)
            
            freq.total <- data.frame(age = "Total", total.freq(raw.frequency))
            raw.frequency <- rbind(raw.frequency, freq.total)
            rm(freq.total)
            
            #combine estimated and raw total rates and frequencies, and calculate difference
            i.total.rates <- rbind(raw.total.rates, est.total.rates, (est.total.rates-raw.total.rates)/raw.total.rates)
            i.mean.age <- rbind(raw.mean.age, est.mean.age, (est.mean.age-raw.mean.age)/raw.mean.age)
            i.total.rates <- do.call(data.frame, lapply(i.total.rates, function(x) replace(x, is.infinite(x), NA)))
            
            #adjust format for ease of output
            i.total.rates <- cbind(i.rowname, rowname, i.total.rates)
            total.rates <- rbind(total.rates, i.total.rates)
            i.mean.age <- cbind(i.rowname, rowname, i.mean.age)
            mean.age <- rbind(mean.age, i.mean.age)
            
          }
          
          #output rates
          write.rates.fertM(oe.rates, frequency, param, i.code, "Poisson estimate")
          write.rates.fertM(raw.oe.rates, raw.frequency, param, i.code, "Direct calculate")
          
        }
        
        #output total rates
        write.total.fert.covar(total.rates, mean.age, param, paste0("-by ", var, sep=""), marsuff)
        
      }
      
    } else if (cohabit ==1){
      
      #Case sex.nosex
      if (byvar=="sex.nosex") {
        
        if (cal_oe==T){
          
          #generate pop table: # of risk population and # of events
          pop <- pop.count.fertM(data_oe, param, nr2, nlb, nhb)
          write.pop.fertM(pop, param, name = combined.name)
          
          #extract estimated oe and raw oe
          oe.rates <- est.covar$oe.rates %>% arrange(age)
          raw.oe.rates <- est.covar$raw.oe.rates %>% arrange(age)
          
        }
        
        if (cal_freq==T){
          
          #if have not generated pop table in the previous step (oe), generate it.
          #if have already generated, omit.
          if(cal_oe==F){
            pop <- pop.count.fertM(data_freq, param, nr2, nlb, nhb)
            write.pop.fertM(pop, param, name = combined.name)
          }
          
          #extract estimated freq and raw freq
          frequency <- est.covar$frequency %>% arrange(age)
          raw.frequency <- est.covar$raw.frequency %>% arrange(age)
          
          #Calculate estimated total rates and mean age based on estimated freq.
          frequency$All <- tmp.rowSum.w.na(frequency[2:ncol(frequency)])
          frequency <- frequency %>% relocate(All, .after = age)
          
          #non marital frequency
          nonmar.freq <- data.frame(age = frequency$age, All = frequency$All)
          for (i in 1:nr2) {
            nonmar.freq <- data.frame(nonmar.freq, tmp.rowSum.w.na(frequency[seq(2+i,7*nr2+2, by=nr2)]))
            names(nonmar.freq)[i+2] <- paste0("p", i, ".freq")
          }
          
          #add non marital and total for each marital status
          frequency <- data.frame(nonmar.freq, frequency[3:ncol(frequency)])
          frequency$all.nmn <- tmp.rowSum.w.na(frequency[(3+nr2):(2+2*nr2)])
          frequency$all.m <- tmp.rowSum.w.na(frequency[(3+2*nr2):(2+3*nr2)])
          frequency$all.wnc <- tmp.rowSum.w.na(frequency[(3+3*nr2):(2+4*nr2)])
          frequency$all.dnc <- tmp.rowSum.w.na(frequency[(3+4*nr2):(2+5*nr2)])
          frequency$all.nmc <- tmp.rowSum.w.na(frequency[(3+5*nr2):(2+6*nr2)])
          frequency$all.wc <- tmp.rowSum.w.na(frequency[(3+6*nr2):(2+7*nr2)])
          frequency$all.dc <- tmp.rowSum.w.na(frequency[(3+7*nr2):(2+8*nr2)])
          
          frequency <- frequency %>% relocate(all.nmn, .before = pnmn1.freq)
          frequency <- frequency %>% relocate(all.m, .before = pm1.freq)
          frequency <- frequency %>% relocate(all.wnc, .before = pwnc1.freq)
          frequency <- frequency %>% relocate(all.dnc, .before = pdnc1.freq)
          frequency <- frequency %>% relocate(all.nmc, .before = pnmc1.freq)
          frequency <- frequency %>% relocate(all.wc, .before = pwc1.freq)
          frequency <- frequency %>% relocate(all.dc, .before = pdc1.freq)
          
          freq.total <- data.frame(age = "Total",  total.freq(frequency))
          frequency <- rbind(frequency, freq.total)
          rm(freq.total)
          
          est.total.rates <- total.freq(nonmar.freq)
          est.mean.age <- mean.age.cal(nonmar.freq)
          
          #Calculate raw total rates and mean age based on raw freq.
          raw.frequency$All <- tmp.rowSum.w.na(raw.frequency[2:ncol(raw.frequency)])
          raw.frequency <- raw.frequency %>% relocate(All, .after = age)
          
          #non marital frequency
          nonmar.freq <- data.frame(age = raw.frequency$age, All = raw.frequency$All)
          for (i in 1:nr2) {
            nonmar.freq <- data.frame(nonmar.freq, tmp.rowSum.w.na(raw.frequency[seq(2+i,7*nr2+2, by=nr2)]))
            names(nonmar.freq)[i+2] <- paste0("p", i, ".freq")
          }
          
          #add non marital and total for each marital status
          raw.frequency <- data.frame(nonmar.freq, raw.frequency[3:ncol(raw.frequency)])
          raw.frequency$all.nmn <- tmp.rowSum.w.na(raw.frequency[(3+nr2):(2+2*nr2)])
          raw.frequency$all.m <- tmp.rowSum.w.na(raw.frequency[(3+2*nr2):(2+3*nr2)])
          raw.frequency$all.wnc <- tmp.rowSum.w.na(raw.frequency[(3+3*nr2):(2+4*nr2)])
          raw.frequency$all.dnc <- tmp.rowSum.w.na(raw.frequency[(3+4*nr2):(2+5*nr2)])
          raw.frequency$all.nmc <- tmp.rowSum.w.na(raw.frequency[(3+5*nr2):(2+6*nr2)])
          raw.frequency$all.wc <- tmp.rowSum.w.na(raw.frequency[(3+6*nr2):(2+7*nr2)])
          raw.frequency$all.dc <- tmp.rowSum.w.na(raw.frequency[(3+7*nr2):(2+8*nr2)])
          
          raw.frequency <- raw.frequency %>% relocate(all.nmn, .before = pnmn1.freq)
          raw.frequency <- raw.frequency %>% relocate(all.m, .before = pm1.freq)
          raw.frequency <- raw.frequency %>% relocate(all.wnc, .before = pwnc1.freq)
          raw.frequency <- raw.frequency %>% relocate(all.dnc, .before = pdnc1.freq)
          raw.frequency <- raw.frequency %>% relocate(all.nmc, .before = pnmc1.freq)
          raw.frequency <- raw.frequency %>% relocate(all.wc, .before = pwc1.freq)
          raw.frequency <- raw.frequency %>% relocate(all.dc, .before = pdc1.freq)
          
          raw.total.rates <- total.freq(nonmar.freq)
          raw.mean.age <- mean.age.cal(nonmar.freq)
          
          freq.total <- data.frame(age = "Total", total.freq(raw.frequency))
          raw.frequency <- rbind(raw.frequency, freq.total)
          rm(freq.total)
          
          #combine estimated and raw total rates and frequencies, and calculate difference
          total.rates <- rbind(raw.total.rates, est.total.rates, (est.total.rates-raw.total.rates)/raw.total.rates)
          mean.age <- rbind(raw.mean.age, est.mean.age, (est.mean.age-raw.mean.age)/raw.mean.age)
          total.rates <- do.call(data.frame, lapply(total.rates, function(x) replace(x, is.infinite(x), NA)))
          
          write.total.fert.covar(total.rates, mean.age, param, combined.name, marsuff)
          
        }
        
        #output estimated rates
        write.rates.fertM(oe.rates, frequency, param, combined.name, "Poisson estimate")
        #output raw rates
        write.rates.fertM(raw.oe.rates, raw.frequency, param, combined.name, "Direct calculate")
        
      }
      
      
      # if by rural/urban, region, race, edu
      if (byvar %in% c("ru.nosex", "region.nosex", "race.nosex")){
        
        #define code list & varable name
        if (byvar=="region.nosex"){
          i.list <- region_list
          var <- "region"
        } else if (byvar=="ru.nosex") {
          i.list <- list(code=c(1,2), name=c("rural", "urban"))
          var <- "ru"
        } else if (byvar=="race.nosex"){
          i.list <- race_list
          var <- "race"
        }
        
        #run for each list code
        for(k in 1:length(i.list[[1]])){
          
          #code
          i <- i.list[[1]][k]
          #name
          i.name <- i.list[[2]][k]
          #code for rowname
          i.rowname <- rep(i.name, 3)
          #code for output surfix
          i.code <- paste0(", ", i.name, sep="")
          
          if (cal_oe==T){
            
            #generate pop table: # of risk population and # of events
            pop <- pop.count.fertM(data_oe[which(eval(parse(text=paste("data_oe$",var,sep="")))==i),], param, nr2, nlb, nhb)
            write.pop.fertM(pop, param, i.code)
            
            #extract estimated & direct calculated oe rates from estimation result
            oe.rates <- est.covar$oe.rates
            raw.oe.rates <- est.covar$raw.oe.rates
            #keep oe rates by variable
            oe.rates <- oe.rates[which(eval(parse(text = paste("oe.rates$",var,sep=""))) == i), -which(names(oe.rates)==var)] %>% arrange(age)
            raw.oe.rates <- raw.oe.rates[which(eval(parse(text = paste("raw.oe.rates$",var,sep=""))) == i), -which(names(raw.oe.rates)==var)] %>% arrange(age)
            
          }
          
          if (cal_freq==T){
            
            if (cal_oe==F){
              
              #if have not generated pop table in the previous step (oe), generate it.
              #if have already generated, omit.
              pop <- pop.count.fertM(data_freq[which(eval(parse(text=paste("data_freq$",var,sep="")))==i),], param, nr2, nlb, nhb)
              write.pop.fertM(pop, param, i.code)
              
            }
            
            #extract estimated & direct calculated freq rates from estimation result
            frequency <- est.covar$frequency
            raw.frequency <- est.covar$raw.frequency
            #keep freq rates by variable
            frequency <- frequency[which(eval(parse(text = paste("frequency$",var,sep=""))) == i), -which(names(frequency)==var)] %>% arrange(age)
            raw.frequency <- raw.frequency[which(eval(parse(text = paste("raw.frequency$",var,sep=""))) == i), -which(names(raw.frequency)==var)] %>% arrange(age)
            
            #Calculate estimated total rates and mean age based on estimated freq.
            frequency$All <- tmp.rowSum.w.na(frequency[2:ncol(frequency)])
            frequency <- frequency %>% relocate(All, .after = age)
            
            #non marital frequency
            nonmar.freq <- data.frame(age = frequency$age, All = frequency$All)
            for (i in 1:nr2) {
              nonmar.freq <- data.frame(nonmar.freq, tmp.rowSum.w.na(frequency[seq(2+i,7*nr2+2, by=nr2)]))
              names(nonmar.freq)[i+2] <- paste0("p", i, ".freq")
            }
            
            #add non marital and total for each marital status
            frequency <- data.frame(nonmar.freq, frequency[3:ncol(frequency)])
            frequency$all.nmn <- tmp.rowSum.w.na(frequency[(3+nr2):(2+2*nr2)])
            frequency$all.m <- tmp.rowSum.w.na(frequency[(3+2*nr2):(2+3*nr2)])
            frequency$all.wnc <- tmp.rowSum.w.na(frequency[(3+3*nr2):(2+4*nr2)])
            frequency$all.dnc <- tmp.rowSum.w.na(frequency[(3+4*nr2):(2+5*nr2)])
            frequency$all.nmc <- tmp.rowSum.w.na(frequency[(3+5*nr2):(2+6*nr2)])
            frequency$all.wc <- tmp.rowSum.w.na(frequency[(3+6*nr2):(2+7*nr2)])
            frequency$all.dc <- tmp.rowSum.w.na(frequency[(3+7*nr2):(2+8*nr2)])
            
            frequency <- frequency %>% relocate(all.nmn, .before = pnmn1.freq)
            frequency <- frequency %>% relocate(all.m, .before = pm1.freq)
            frequency <- frequency %>% relocate(all.wnc, .before = pwnc1.freq)
            frequency <- frequency %>% relocate(all.dnc, .before = pdnc1.freq)
            frequency <- frequency %>% relocate(all.nmc, .before = pnmc1.freq)
            frequency <- frequency %>% relocate(all.wc, .before = pwc1.freq)
            frequency <- frequency %>% relocate(all.dc, .before = pdc1.freq)
            
            freq.total <- data.frame(age = "Total",  total.freq(frequency))
            frequency <- rbind(frequency, freq.total)
            rm(freq.total)
            
            est.total.rates <- total.freq(nonmar.freq)
            est.mean.age <- mean.age.cal(nonmar.freq)
            
            #Calculate raw total rates and mean age based on raw freq.
            raw.frequency$All <- tmp.rowSum.w.na(raw.frequency[2:ncol(raw.frequency)])
            raw.frequency <- raw.frequency %>% relocate(All, .after = age)
            
            #non marital frequency
            nonmar.freq <- data.frame(age = raw.frequency$age, All = raw.frequency$All)
            for (i in 1:nr2) {
              nonmar.freq <- data.frame(nonmar.freq, tmp.rowSum.w.na(raw.frequency[seq(2+i,7*nr2+2, by=nr2)]))
              names(nonmar.freq)[i+2] <- paste0("p", i, ".freq")
            }
            
            #add non marital and total for each marital status
            raw.frequency <- data.frame(nonmar.freq, raw.frequency[3:ncol(raw.frequency)])
            raw.frequency$all.nmn <- tmp.rowSum.w.na(raw.frequency[(3+nr2):(2+2*nr2)])
            raw.frequency$all.m <- tmp.rowSum.w.na(raw.frequency[(3+2*nr2):(2+3*nr2)])
            raw.frequency$all.wnc <- tmp.rowSum.w.na(raw.frequency[(3+3*nr2):(2+4*nr2)])
            raw.frequency$all.dnc <- tmp.rowSum.w.na(raw.frequency[(3+4*nr2):(2+5*nr2)])
            raw.frequency$all.nmc <- tmp.rowSum.w.na(raw.frequency[(3+5*nr2):(2+6*nr2)])
            raw.frequency$all.wc <- tmp.rowSum.w.na(raw.frequency[(3+6*nr2):(2+7*nr2)])
            raw.frequency$all.dc <- tmp.rowSum.w.na(raw.frequency[(3+7*nr2):(2+8*nr2)])
            
            raw.frequency <- raw.frequency %>% relocate(all.nmn, .before = pnmn1.freq)
            raw.frequency <- raw.frequency %>% relocate(all.m, .before = pm1.freq)
            raw.frequency <- raw.frequency %>% relocate(all.wnc, .before = pwnc1.freq)
            raw.frequency <- raw.frequency %>% relocate(all.dnc, .before = pdnc1.freq)
            raw.frequency <- raw.frequency %>% relocate(all.nmc, .before = pnmc1.freq)
            raw.frequency <- raw.frequency %>% relocate(all.wc, .before = pwc1.freq)
            raw.frequency <- raw.frequency %>% relocate(all.dc, .before = pdc1.freq)
            
            raw.total.rates <- total.freq(nonmar.freq)
            raw.mean.age <- mean.age.cal(nonmar.freq)
            
            freq.total <- data.frame(age = "Total", total.freq(raw.frequency))
            raw.frequency <- rbind(raw.frequency, freq.total)
            rm(freq.total)
            
            i.total.rates <- rbind(raw.total.rates, est.total.rates, (est.total.rates-raw.total.rates)/raw.total.rates)
            i.mean.age <- rbind(raw.mean.age, est.mean.age, (est.mean.age-raw.mean.age)/raw.mean.age)
            i.total.rates <- do.call(data.frame, lapply(i.total.rates, function(x) replace(x, is.infinite(x), NA)))
            
            #adjust format for ease of output
            i.total.rates <- cbind(i.rowname, rowname, i.total.rates)
            total.rates <- rbind(total.rates, i.total.rates)
            i.mean.age <- cbind(i.rowname, rowname, i.mean.age)
            mean.age <- rbind(mean.age, i.mean.age)
            
          }
          
          #output rates
          write.rates.fertM(oe.rates, frequency, param, i.code, "Poisson estimate")
          write.rates.fertM(raw.oe.rates, raw.frequency, param, i.code, "Direct calculate")
          
        }
        
        #output total rates
        write.total.fert.covar(total.rates, mean.age, param, paste0("-by ", var, sep=""), marsuff)
        
      }
      
    }
    
  }
}


#### Estimation main function ####

compute.fert <- function(data_oe, data_freq, param, plot, plot.name, method, mfp){
  
  #parameters
  nlb <- as.numeric(param$nlb)
  nhb <- as.numeric(param$nhb)
  nr2 <- as.numeric(param$nr2)
  title <- as.character(param$title)
  t1Month <- as.numeric(param$t1Month)
  t2Month <- as.numeric(param$t2Month)
  nWeight <- as.numeric(param$nWeight)
  nrate <- as.numeric(param$nRate)
  
  period <- paste0((t1Month-1)%/%12+1900, " - ",(t2Month-1)%/%12+1900, sep="")
  
  #age range
  agelist <- data.frame(age=seq(nlb, nhb, 1))
  
  #options indicating whether calculate oe or freq
  cal_oe <- (nrate==1 | nrate==2)
  cal_freq <- (nrate==1 | nrate==3)

  if(cal_oe == TRUE){
    
    #separate by gender
    female_oe <- subset(data_oe, sex == 2)
    
    #direct calculate rates
    for (i in 1:nr2){
      
      #the lowest fertility rates increase 1 year per parity
      female_ <- female_oe[which(female_oe$age >= nlb + i -1),]
      p.oe <- merge(agelist, oe.raw(female_, nlb, nhb, i-1, i, nWeight), all.x=TRUE)
      assign(paste0("p", i, ".oe", sep=""), p.oe)
    }
    
    #if require estimation by any method: e.g. Poisson
    if (!is.na(method)){
      
      if (method=="Poisson") {
        for (i in 1:nr2){
          female_ <- female_oe[which(female_oe$age >= nlb + i -1),]
          p.oe.est <- merge(agelist, oe.poi(female_, nlb, nhb, i-1, i, nWeight, mfp), all.x=TRUE)
          assign(paste0("p", i, ".oe.est", sep=""), p.oe.est)
        }
      }
      
      ###### Bayesian to be added ######
      
      
      ## merge direct calculate and estimated results together
      for (i in 1:nr2){
        assign(paste0("p", i, ".oe", sep=""), 
               merge(eval(parse(text=paste0("p", i, ".oe", sep=""))), 
                     eval(parse(text=paste0("p", i, ".oe.est", sep=""))), 
                     all.x=TRUE))
        
      }
      
    }
    
  }
  
  if(cal_freq == TRUE){
    
    #separate by gender
    female_freq <- subset(data_freq, sex == 2)
    
    for (i in 1:nr2){
      
      #the lowest fertility rates increase 1 year per parity
      female_ <- female_freq[which(female_freq$age >= nlb + i -1),]
      p.freq <- merge(agelist, freq.raw(female_, nlb, nhb, i, nWeight), all.x=TRUE)
      assign(paste0("p", i, ".freq", sep=""), p.freq)
    }
    
    #if require estimation by any method: e.g. Poisson
    if (!is.na(method)){
      
      if (method=="Poisson"){
        for (i in 1:nr2){
          female_ <- female_freq[which(female_freq$age >= nlb + i -1),]
          p.freq.est <- merge(agelist, freq.poi(female_, nlb, nhb, i, nWeight, mfp), all.x=TRUE)
          assign(paste0("p", i, ".freq.est", sep=""), p.freq.est)
        }
      }
      
      
      ###### Bayesian to be added ######
      
      
      ## merge direct calculate and estimated results together
      for (i in 1:nr2){
        assign(paste0("p", i, ".freq", sep=""), 
               merge(eval(parse(text=paste0("p", i, ".freq", sep=""))), 
                     eval(parse(text=paste0("p", i, ".freq.est", sep=""))), 
                     all.x=TRUE))
        
      }
      
    }
    
  }
  
  
  if (plot==TRUE) {
    
    wb <- createWorkbook()
    # define worksheet number
    k<-1
    
    if(cal_oe == TRUE){
      # oe
      addWorksheet(wb, "oe")
      
      writeData(wb, sheet=k, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
      writeData(wb, sheet=k, paste0("Period: ", period, sep=""), startRow=2, startCol=1)
      k<-k+1    #if cal_freq==T, store in sheet k=2.
      
      row.index <- 3
      
      for (i in 1:nr2){
        
        ## oe does not consider nr2+, due to risk population
        
        p <- rates.plot(eval(parse(text=paste0("p", i, ".oe", sep=""))), nlb, nhb, "o/e rate", paste0("Figure 4.",i," Age specific fertility o/e rates, parity ",i, plot.name, sep=""))
        print(p)
        insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
        row.index <- row.index + 22
        
      }
      
    }
    
    
    if(cal_freq == TRUE){
      # fre
      addWorksheet(wb, "freq")
      
      writeData(wb, sheet=k, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
      writeData(wb, sheet=k, paste0("Period: ", period, sep=""), startRow=2, startCol=1)
      
      row.index <- 3
      
      for (i in 1:nr2){
        
        ##only frequency consider nr2+
        
        if (i<nr2){
          p <- rates.plot(eval(parse(text=paste0("p", i, ".freq", sep=""))), nlb, nhb, "frequency", paste0("Figure 5.",i," Age specific fertility frequencies, parity ",i, plot.name, sep=""))
          print(p)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
          row.index <- row.index + 22
        } else {
          
          #if last event: name as nr2+
          p <- rates.plot(eval(parse(text=paste0("p", i, ".freq", sep=""))), nlb, nhb, "frequency", paste0("Figure 5.",i," Age specific fertility frequencies, parity ",i, "+", plot.name, sep=""))
          print(p)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
        }
        
      }
      
    }
    
    
    output.dir <- getwd()
    plot.name <- gsub(", ", "-", plot.name)
    saveWorkbook(wb, paste0(output.dir, "/", title, " non-mar Fertility plot", plot.name, ".xlsx", sep=""), overwrite=TRUE)
    
  }
  
  
  if(cal_oe == TRUE){
    
    #if having est.rates, i.e. have estimated rates
    if (!is.na(method)){
      
      oe.rates <- data.frame(age=agelist$age)
      for (i in 1:nr2){
        poi.oe_ <- eval(parse(text=paste0("p", i, ".oe")))$est.rates
        poi.oe_[poi.oe_ >= 2] <- NA
        oe.rates <- data.frame(oe.rates, poi.oe_)
        names(oe.rates)[i+1] <- paste0("p", i, ".oe")
      }
      
    } else {
      oe.rates <- NA
    }
    
    #direct calculated rates
    raw.oe.rates <- data.frame(age=agelist$age)
    for (i in 1:nr2){
      raw.oe_ <- eval(parse(text=paste0("p", i, ".oe")))$raw.rates
      raw.oe_ [raw.oe_ >= 2] <- NA
      raw.oe.rates <- data.frame(raw.oe.rates, raw.oe_)
      names(raw.oe.rates)[i+1] <- paste0("p", i, ".oe")
    }
    
  }else{
    oe.rates <- NA
    raw.oe.rates <- NA
  }
  
  
  if(cal_freq == TRUE){
    
    #direct calculated rates
    raw.frequency <- data.frame(age=agelist$age)
    for (i in 1:nr2){
      raw.frequency <- data.frame(raw.frequency, eval(parse(text=paste0("p", i, ".freq")))$raw.rates)
      names(raw.frequency)[i+1] <- paste0("p", i, ".freq")
    }
    
    raw.frequency$All <- tmp.rowSum.w.na(raw.frequency[2:ncol(raw.frequency)])
    raw.frequency <- raw.frequency %>% relocate(All, .after = age)
    
    raw.total.rates <- total.freq(raw.frequency)
    raw.mean.age <- mean.age.cal(raw.frequency)
    
    freq.total <- data.frame(age = "Total", total.freq(raw.frequency))
    raw.frequency <- rbind(raw.frequency, freq.total)
    rm(freq.total)
    
    #if having est.rates, i.e. have estimated rates
    if (!is.na(method)){
      
      frequency <- data.frame(age=agelist$age)
      for (i in 1:nr2){
        frequency <- data.frame(frequency, eval(parse(text=paste0("p", i, ".freq")))$est.rates)
        names(frequency)[i+1] <- paste0("p", i, ".freq")
      }
      
      frequency$All <- tmp.rowSum.w.na(frequency[2:ncol(frequency)])
      frequency <- frequency %>% relocate(All, .after = age)
      
      est.total.rates <- total.freq(frequency)
      est.mean.age <- mean.age.cal(frequency)
      
      freq.total <- data.frame(age = "Total", total.freq(frequency))
      frequency <- rbind(frequency, freq.total)
      rm(freq.total)
      
      total.rates <- rbind(raw.total.rates, est.total.rates, (est.total.rates-raw.total.rates)/raw.total.rates)
      mean.age <- rbind(raw.mean.age, est.mean.age, (est.mean.age-raw.mean.age)/raw.mean.age)
      total.rates <- do.call(data.frame, lapply(total.rates, function(x) replace(x, is.infinite(x), NA)))
      
    } else {
      frequency <- NA
      total.rates <- NA
      mean.age <- NA
    }
    
  }else {
    frequency <- NA
    raw.frequency <- NA
    total.rates <- NA
    mean.age<- NA
  }
  
  
  return(list(oe.rates=oe.rates, frequency=frequency,
              raw.oe.rates = raw.oe.rates, raw.frequency = raw.frequency,
              total.rates = total.rates, mean.age = mean.age))
}


##---------this function is originally edited by Mo Yan------------
compute.fertM <- function(data_oe, data_freq, param, plot, plot.name, method, mfp){
  
  #parameters
  nlb <- as.numeric(param$nlb)
  nhb <- as.numeric(param$nhb)
  nr2 <- as.numeric(param$nr2)
  cohabit <- as.numeric(param$cohabit)
  title <- as.character(param$title)
  t1Month <- as.numeric(param$t1Month)
  t2Month <- as.numeric(param$t2Month)
  nWeight <- as.numeric(param$nWeight)
  nrate <- as.numeric(param$nRate)
  
  period <- paste0((t1Month-1)%/%12+1900, " - ",(t2Month-1)%/%12+1900, sep="")
  
  #age range
  agelist <- data.frame(age=seq(nlb, nhb, 1))
  
  #options indicating whether calculate oe or freq
  cal_oe <- (nrate==1 | nrate==2)
  cal_freq <- (nrate==1 | nrate==3)
  
  if (cohabit==0){
    
    ## 4 marital status
    
    #general setting: marital status and plot column index
    config <- rbind(c(1, "never married", 1, "pnm"),
                    c(2, "married", 7, "pm"),
                    c(4, "divorced", 13, "pd"),
                    c(3, "widowed", 19, "pw"))
    
    #if output graph, create workbook
    if (plot==T){
      wb <- createWorkbook()
      # define worksheet number
      k<-1
    }
    
    if(cal_oe == TRUE){
      
      #if output graph, set worksheet 1
      if (plot==T){
        addWorksheet(wb, "oe")
        
        writeData(wb, sheet=k, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
        writeData(wb, sheet=k, paste0("Period: ", period, sep=""), startRow=2, startCol=1)
        k<-k+1    #if cal_freq==T, store in sheet k=2.
      }
      
      #separate by gender
      female_oe <- subset(data_oe, sex == 2)
      
      #for each marital status, estimate oe
      for (row in 1:nrow(config)) {
        mar.bf_ <- as.numeric(config[row, 1])
        type_ <- config[row, 2]
        col_ <- as.numeric(config[row, 3])
        title_ <- config[row, 4]
        
        cl<- makeCluster(max(1, min(nr2, detectCores())))
        registerDoParallel(cl)
        
        
        ## direct calculate
        p.oes <- foreach (i=1:nr2,.combine='list',.export="oe.raw",.packages=c("dplyr","mfp"),.multicombine=TRUE) %dopar% {
          female_ <- female_oe[which(female_oe$age >= nlb + i -1),]
          p.oe <- merge(agelist, oe.raw(subset(female_, mar.bf==mar.bf_), nlb, nhb, i-1, i, nWeight), all.x=TRUE)
        }
        
        
        #if require estimation by any method: e.g. Poisson
        if (!is.na(method)){
          
          if (method=="Poisson"){
            p.oes.est <- foreach (i=1:nr2,.combine='list',.export="oe.poi",.packages=c("dplyr","mfp"),.multicombine=TRUE) %dopar% {
              female_ <- female_oe[which(female_oe$age >= nlb + i -1),]
              p.oe.est <- merge(agelist, oe.poi(subset(female_, mar.bf==mar.bf_), nlb, nhb, i-1, i, nWeight, mfp), all.x=TRUE)
            }
          }
          
          ###### Bayesian to be added ######
          
          
          stopCluster(cl)
          
          
          ## merge direct calculate and estimated results together
          for (i in 1:nr2){
            p.oe <- data.frame(p.oes[i])
            p.oe.est <- data.frame(p.oes.est[i])
            
            assign(paste0("p", i, ".oe", sep=""), 
                   merge(p.oe, p.oe.est, all.x=TRUE))
            
            #save result list to each marital list
            assign(paste0(title_, i, ".oe", sep=""), eval(parse(text=paste0("p", i, ".oe", sep=""))))
            
          }

        } else {
          ## only direct calculate results, save to each marital list
          for (i in 1:nr2){
            p.oe <- data.frame(p.oes[i])
            assign(paste0(title_, i, ".oe", sep=""), p.oe)
          }
        }
        
        
        #if output graph, insert plots (in different rows) for each marital status (in different columns)
        if (plot==T){
          
          row.index <- 3
          
          for (i in 1:nr2){
            p <- rates.plot(eval(parse(text=paste0("p", i, ".oe", sep=""))), nlb, nhb, "o/e rate", paste0("Figure 4.",i," Age specific fertility o/e rates for ", type_, " women, parity ",i, plot.name, sep=""))
            print(p)
            insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=col_, width=12.55, height=10.4, units="cm")
            row.index <- row.index + 22
          }
          
        }
        
      }

    }
    
    if(cal_freq == TRUE){
      
      #if output graph, set worksheet 2
      if (plot==T){
        addWorksheet(wb, "freq")
        
        writeData(wb, sheet=k, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
        writeData(wb, sheet=k, paste0("Period: ", period, sep=""), startRow=2, startCol=1)
      }
      
      #separate by gender
      female_freq <- subset(data_freq, sex == 2)
      
      #for each marital status, estimate freq
      for (row in 1:nrow(config)) {
        mar.bf_ <- as.numeric(config[row, 1])
        type_ <- config[row, 2]
        col_ <- as.numeric(config[row, 3])
        title_ <- config[row, 4]
        
        cl<- makeCluster(max(1, min(nr2, detectCores())))
        registerDoParallel(cl)
        
        
        #direct calculate
        p.freqs <- foreach (i=1:nr2,.combine='list',.export="freqM.raw",.packages=c("dplyr","mfp"),.multicombine=TRUE) %dopar% {
          female_ <- female_freq[which(female_freq$age >= nlb + i -1),]
          p.freq <- merge(agelist, freqM.raw(female_, nlb, nhb, mar.bf_, i, nWeight), all.x=TRUE)
        }
        
        
        #if require estimation by any method: e.g. Poisson
        if (!is.na(method)){
          
          if (method=="Poisson"){
            p.freqs.est <- foreach (i=1:nr2,.combine='list',.export="freqM.poi",.packages=c("dplyr","mfp"),.multicombine=TRUE) %dopar% {
              female_ <- female_freq[which(female_freq$age >= nlb + i -1),]
              p.freq.est <- merge(agelist, freqM.poi(female_, nlb, nhb, mar.bf_, i, nWeight, mfp), all.x=TRUE)
            }
          }
          
          ###### Bayesian to be added ######
          
          
          stopCluster(cl)
          
          
          ## merge direct calculate and estimated results together
          for (i in 1:nr2){
            p.freq <- data.frame(p.freqs[i])
            p.freq.est <- data.frame(p.freqs.est[i])
            
            assign(paste0("p", i, ".freq", sep=""), 
                   merge(p.freq, p.freq.est, all.x=TRUE))
            
            #save result list to each marital list
            assign(paste0(title_, i, ".freq", sep=""), eval(parse(text=paste0("p", i, ".freq", sep=""))))
            
          }
          
        } else {
          ## only direct calculate results, save to each marital list
          for (i in 1:nr2){
            p.freq <- data.frame(p.freqs[i])
            assign(paste0(title_, i, ".freq", sep=""), p.freq)
          }
        }
        
        #if output graph, insert plots (in different rows) for each marital status (in different columns)
        if (plot==T){
          
          row.index <- 3
          
          for (i in 1:nr2){
            
            if (i<nr2){
              p <- rates.plot(eval(parse(text=paste0("p", i, ".freq", sep=""))), nlb, nhb, "frequency", paste0("Figure 5.",i," Age specific fertility frequencies for ", type_, " women, parity ",i, plot.name, sep=""))
              print(p)
              insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=col_, width=12.55, height=10.4, units="cm")
              row.index <- row.index + 22
            } else {
              
              #if last event: name as nr2+
              p <- rates.plot(eval(parse(text=paste0("p", i, ".freq", sep=""))), nlb, nhb, "frequency", paste0("Figure 5.",i," Age specific fertility frequencies for ", type_, " women, parity ",i, "+", plot.name, sep=""))
              print(p)
              insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=col_, width=12.55, height=10.4, units="cm")
            }
            
          }
          
        }
        
      }
      
    }
    
    #if output graph, write excel file
    if (plot==T){
      output.dir <- getwd()
      plot.name <- gsub(", ", "-", plot.name)
      saveWorkbook(wb, paste0(output.dir, "/", title, " 4 marital Fertility plot", plot.name, ".xlsx", sep=""), overwrite=TRUE)
    }
    
    
    #### the following part is to summarize all results into oe.rates or frequency dataframe
    
    config <- rbind("pnm", "pm", "pd", "pw")
    
    if(cal_oe == TRUE){
      
      ###direct calculate results: raw.oe.rates
      raw.oe.rates <- data.frame(age=agelist$age)
      
      for (row in 1:nrow(config)) {
        title_ <- config[row]
        
        for (i in 1:nr2){
          raw.oe_ <- eval(parse(text=paste0(title_, i, ".oe")))$raw.rates
          raw.oe_ [raw.oe_ >= 2] <- NA
          raw.oe.rates <- data.frame(raw.oe.rates, raw.oe_)
          names(raw.oe.rates)[i+1+(row-1)*nr2] <- paste0(title_, i, ".oe")
        }
      }
      
      ##if method is not NA: estimated rates -- oe.rates
      if (!is.na(method)){
        
        oe.rates <- data.frame(age=agelist$age)
        
        for (row in 1:nrow(config)) {
          title_ <- config[row]
          
          for (i in 1:nr2){
            oe.rates_ <- eval(parse(text=paste0(title_, i, ".oe")))$est.rates
            oe.rates_[oe.rates_ >= 2] <- NA
            oe.rates <- data.frame(oe.rates, oe.rates_)
            names(oe.rates)[i+1+(row-1)*nr2] <- paste0(title_, i, ".oe")
          }
          
        }
        
      }
      
      
    }else{
      oe.rates <- NA
      raw.oe.rates <- NA
    }
    
    if(cal_freq == TRUE){
      
      ###direct calculate
      raw.frequency <- data.frame(age=agelist$age)
      
      for (row in 1:nrow(config)) {
        title_ <- config[row]
        
        for (i in 1:nr2){
          raw.frequency <- data.frame(raw.frequency, eval(parse(text=paste0(title_, i, ".freq")))$raw.rates)
          names(raw.frequency)[i+1+(row-1)*nr2] <- paste0(title_, i, ".freq")
        }
      }
      
      raw.frequency$All <- tmp.rowSum.w.na(raw.frequency[2:ncol(raw.frequency)])
      raw.frequency <- raw.frequency %>% relocate(All, .after = age)
      
      #non marital frequency
      nonmar.freq <- data.frame(age = raw.frequency$age, All = raw.frequency$All)
      for (i in 1:nr2) {
        nonmar.freq <- data.frame(nonmar.freq, tmp.rowSum.w.na(raw.frequency[seq(2+i,4*nr2+2, by=nr2)]))
        names(nonmar.freq)[i+2] <- paste0("p", i, ".freq")
      }
      
      #add non marital and total for each marital status
      raw.frequency <- data.frame(nonmar.freq, raw.frequency[3:ncol(raw.frequency)])
      raw.frequency$all.nm <- tmp.rowSum.w.na(raw.frequency[(3+nr2):(2+2*nr2)])
      raw.frequency$all.m <- tmp.rowSum.w.na(raw.frequency[(3+2*nr2):(2+3*nr2)])
      raw.frequency$all.d <- tmp.rowSum.w.na(raw.frequency[(3+3*nr2):(2+4*nr2)])
      raw.frequency$all.w <- tmp.rowSum.w.na(raw.frequency[(3+4*nr2):(2+5*nr2)])
      
      raw.frequency <- raw.frequency %>% relocate(all.nm, .before = pnm1.freq)
      raw.frequency <- raw.frequency %>% relocate(all.m, .before = pm1.freq)
      raw.frequency <- raw.frequency %>% relocate(all.d, .before = pd1.freq)
      raw.frequency <- raw.frequency %>% relocate(all.w, .before = pw1.freq)
      
      raw.total.rates <- total.freq(nonmar.freq)
      raw.mean.age <- mean.age.cal(nonmar.freq)
      
      freq.total <- data.frame(age = "Total", total.freq(raw.frequency))
      raw.frequency <- rbind(raw.frequency, freq.total)
      rm(freq.total)
      
      #add estimated frequency
      if (!is.na(method)){
        
        frequency <- data.frame(age=agelist$age)
        
        for (row in 1:nrow(config)) {
          title_ <- config[row]
          
          for (i in 1:nr2){
            frequency <- data.frame(frequency, eval(parse(text=paste0(title_, i, ".freq")))$est.rates)
            names(frequency)[i+1+(row-1)*nr2] <- paste0(title_, i, ".freq")
          }
        }
        
        frequency$All <- tmp.rowSum.w.na(frequency[2:ncol(frequency)])
        frequency <- frequency %>% relocate(All, .after = age)
        
        #non marital frequency
        nonmar.freq <- data.frame(age = frequency$age, All = frequency$All)
        for (i in 1:nr2) {
          nonmar.freq <- data.frame(nonmar.freq, tmp.rowSum.w.na(frequency[seq(2+i,4*nr2+2, by=nr2)]))
          names(nonmar.freq)[i+2] <- paste0("p", i, ".freq")
        }
        
        #add non marital and total for each marital status
        frequency <- data.frame(nonmar.freq, frequency[3:ncol(frequency)])
        frequency$all.nm <- tmp.rowSum.w.na(frequency[(3+nr2):(2+2*nr2)])
        frequency$all.m <- tmp.rowSum.w.na(frequency[(3+2*nr2):(2+3*nr2)])
        frequency$all.d <- tmp.rowSum.w.na(frequency[(3+3*nr2):(2+4*nr2)])
        frequency$all.w <- tmp.rowSum.w.na(frequency[(3+4*nr2):(2+5*nr2)])
        
        frequency <- frequency %>% relocate(all.nm, .before = pnm1.freq)
        frequency <- frequency %>% relocate(all.m, .before = pm1.freq)
        frequency <- frequency %>% relocate(all.d, .before = pd1.freq)
        frequency <- frequency %>% relocate(all.w, .before = pw1.freq)
        
        freq.total <- data.frame(age = "Total",  total.freq(frequency))
        frequency <- rbind(frequency, freq.total)
        rm(freq.total)
        
        est.total.rates <- total.freq(nonmar.freq)
        est.mean.age <- mean.age.cal(nonmar.freq)
        
        total.rates <- rbind(raw.total.rates, est.total.rates, (est.total.rates-raw.total.rates)/raw.total.rates)
        mean.age <- rbind(raw.mean.age, est.mean.age, (est.mean.age-raw.mean.age)/raw.mean.age)
        total.rates <- do.call(data.frame, lapply(total.rates, function(x) replace(x, is.infinite(x), NA)))
        
      } else {
        frequency <- NA
        total.rates <- NA
        mean.age <- NA
      }
      
    }else {
      frequency <- NA
      raw.frequency <- NA
      total.rates <- NA
      mean.age<- NA
    }
    
    
  } else if (cohabit==1) {
    
    #general setting: marital status and plot column index
    config <- rbind(c(1, "never married and not cohabiting", 1, "pnmnc"),
                    c(2, "married", 8, "pm"),
                    c(3, "widowed and not cohabiting", 15, "pwnc"),
                    c(4, "divorced and not cohabiting", 22, "pdnc"),
                    c(5, "never married and cohabiting", 29, "pnmc"),
                    c(7, "widowed and cohabiting", 36, "pwc"),
                    c(8, "divorced and cohabiting", 43, "pdc"))
    
    #if output graph, create workbook
    if (plot==T){
      wb <- createWorkbook()
      # define worksheet number
      k<-1
    }
    
    if(cal_oe == TRUE){
      
      #if output graph, set worksheet 1
      if (plot==T){
        addWorksheet(wb, "oe")
        
        writeData(wb, sheet=k, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
        writeData(wb, sheet=k, paste0("Period: ", period, sep=""), startRow=2, startCol=1)
        k<-k+1    #if cal_freq==T, store in sheet k=2.
      }
      
      #separate by gender
      female_oe <- subset(data_oe, sex == 2)
      
      #for each marital status, estimate oe
      for (row in 1:nrow(config)) {
        mar.bf_ <- as.numeric(config[row, 1])
        type_ <- config[row, 2]
        col_ <- as.numeric(config[row, 3])
        title_ <- config[row, 4]
        
        cl<- makeCluster(max(1, min(nr2, detectCores())))
        registerDoParallel(cl)
        
        
        ## direct calculate
        p.oes <- foreach (i=1:nr2,.combine='list',.export="oe.raw",.packages=c("dplyr","mfp"),.multicombine=TRUE) %dopar% {
          female_ <- female_oe[which(female_oe$age >= nlb + i -1),]
          p.oe <- merge(agelist, oe.raw(subset(female_, mar.bf==mar.bf_), nlb, nhb, i-1, i, nWeight), all.x=TRUE)
        }
        
        
        #if require estimation by any method: e.g. Poisson
        if (!is.na(method)){
          
          if (method=="Poisson"){
            p.oes.est <- foreach (i=1:nr2,.combine='list',.export="oe.poi",.packages=c("dplyr","mfp"),.multicombine=TRUE) %dopar% {
              female_ <- female_oe[which(female_oe$age >= nlb + i -1),]
              p.oe.est <- merge(agelist, oe.poi(subset(female_, mar.bf==mar.bf_), nlb, nhb, i-1, i, nWeight, mfp), all.x=TRUE)
            }
          }
          
          ###### Bayesian to be added ######
          
          
          stopCluster(cl)
          
          
          ## merge direct calculate and estimated results together
          for (i in 1:nr2){
            p.oe <- data.frame(p.oes[i])
            p.oe.est <- data.frame(p.oes.est[i])
            
            assign(paste0("p", i, ".oe", sep=""), 
                   merge(p.oe, p.oe.est, all.x=TRUE))
            
            #save result list to each marital list
            assign(paste0(title_, i, ".oe", sep=""), eval(parse(text=paste0("p", i, ".oe", sep=""))))
            
          }
          
        } else {
          ## only direct calculate results, save to each marital list
          for (i in 1:nr2){
            p.oe <- data.frame(p.oes[i])
            assign(paste0(title_, i, ".oe", sep=""), p.oe)
          }
        }
        
        
        #if output graph, insert plots (in different rows) for each marital status (in different columns)
        if (plot==T){
          
          row.index <- 3
          
          for (i in 1:nr2){
            p <- rates.plot(eval(parse(text=paste0("p", i, ".oe", sep=""))), nlb, nhb, "o/e rate", paste0("Figure 4.",i," Age specific fertility o/e rates for ", type_, " women, parity ",i, plot.name, sep=""))
            print(p)
            insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=col_, width=12.55, height=10.4, units="cm")
            row.index <- row.index + 22
          }
          
        }
        
      }
      
    }
    
    if(cal_freq == TRUE){
      
      #if output graph, set worksheet 2
      if (plot==T){
        addWorksheet(wb, "freq")
        
        writeData(wb, sheet=k, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
        writeData(wb, sheet=k, paste0("Period: ", period, sep=""), startRow=2, startCol=1)
      }
      
      #separate by gender
      female_freq <- subset(data_freq, sex == 2)
      
      #for each marital status, estimate freq
      for (row in 1:nrow(config)) {
        mar.bf_ <- as.numeric(config[row, 1])
        type_ <- config[row, 2]
        col_ <- as.numeric(config[row, 3])
        title_ <- config[row, 4]
        
        cl<- makeCluster(max(1, min(nr2, detectCores())))
        registerDoParallel(cl)
        
        
        #direct calculate
        p.freqs <- foreach (i=1:nr2,.combine='list',.export="freqM.raw",.packages=c("dplyr","mfp"),.multicombine=TRUE) %dopar% {
          female_ <- female_freq[which(female_freq$age >= nlb + i -1),]
          p.freq <- merge(agelist, freqM.raw(female_, nlb, nhb, mar.bf_, i, nWeight), all.x=TRUE)
        }
        
        
        #if require estimation by any method: e.g. Poisson
        if (!is.na(method)){
          
          if (method=="Poisson"){
            p.freqs.est <- foreach (i=1:nr2,.combine='list',.export="freqM.poi",.packages=c("dplyr","mfp"),.multicombine=TRUE) %dopar% {
              female_ <- female_freq[which(female_freq$age >= nlb + i -1),]
              p.freq.est <- merge(agelist, freqM.poi(female_, nlb, nhb, mar.bf_, i, nWeight, mfp), all.x=TRUE)
            }
          }
          
          
          ###### Bayesian to be added ######
          
          
          stopCluster(cl)
          
          
          ## merge direct calculate and estimated results together
          for (i in 1:nr2){
            p.freq <- data.frame(p.freqs[i])
            p.freq.est <- data.frame(p.freqs.est[i])
            
            assign(paste0("p", i, ".freq", sep=""), 
                   merge(p.freq, p.freq.est, all.x=TRUE))
            
            #save result list to each marital list
            assign(paste0(title_, i, ".freq", sep=""), eval(parse(text=paste0("p", i, ".freq", sep=""))))
            
          }
          
        } else {
          ## only direct calculate results, save to each marital list
          for (i in 1:nr2){
            p.freq <- data.frame(p.freqs[i])
            assign(paste0(title_, i, ".freq", sep=""), p.freq)
          }
        }
        
        #if output graph, insert plots (in different rows) for each marital status (in different columns)
        if (plot==T){
          
          row.index <- 3
          
          for (i in 1:nr2){
            
            if (i<nr2){
              p <- rates.plot(eval(parse(text=paste0("p", i, ".freq", sep=""))), nlb, nhb, "frequency", paste0("Figure 5.",i," Age specific fertility frequencies for ", type_, " women, parity ",i, plot.name, sep=""))
              print(p)
              insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=col_, width=12.55, height=10.4, units="cm")
              row.index <- row.index + 22
            } else {
              
              #if last event: name as nr2+
              p <- rates.plot(eval(parse(text=paste0("p", i, ".freq", sep=""))), nlb, nhb, "frequency", paste0("Figure 5.",i," Age specific fertility frequencies for ", type_, " women, parity ",i, "+", plot.name, sep=""))
              print(p)
              insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=col_, width=12.55, height=10.4, units="cm")
            }
            
          }
          
        }
        
      }
      
    }
    
    #if output graph, write excel file
    if (plot==T){
      output.dir <- getwd()
      plot.name <- gsub(", ", "-", plot.name)
      saveWorkbook(wb, paste0(output.dir, "/", title, " 7 marital Fertility plot", plot.name, ".xlsx", sep=""), overwrite=TRUE)
    }
    
    
    #### the following part is to summarize all results into oe.rates or frequency dataframe
    
    config <- rbind("pnmnc", "pm", "pwnc", "pdnc", "pnmc", "pwc", "pdc")
    
    if(cal_oe == TRUE){
      
      ###direct calculate results: raw.oe.rates
      raw.oe.rates <- data.frame(age=agelist$age)
      
      for (row in 1:nrow(config)) {
        title_ <- config[row]
        
        for (i in 1:nr2){
          raw.oe_ <- eval(parse(text=paste0(title_, i, ".oe")))$raw.rates
          raw.oe_ [raw.oe_ >= 2] <- NA
          raw.oe.rates <- data.frame(raw.oe.rates, raw.oe_)
          names(raw.oe.rates)[i+1+(row-1)*nr2] <- paste0(title_, i, ".oe")
        }
      }
      
      ##if method is not NA: estimated rates -- oe.rates
      if (!is.na(method)){
        
        oe.rates <- data.frame(age=agelist$age)
        
        for (row in 1:nrow(config)) {
          title_ <- config[row]
          
          for (i in 1:nr2){
            oe.rates_ <- eval(parse(text=paste0(title_, i, ".oe")))$est.rates
            oe.rates_[oe.rates_ >= 2] <- NA
            oe.rates <- data.frame(oe.rates, oe.rates_)
            names(oe.rates)[i+1+(row-1)*nr2] <- paste0(title_, i, ".oe")
          }
          
        }
        
      }
      
      
    }else{
      oe.rates <- NA
      raw.oe.rates <- NA
    }
    
    if(cal_freq == TRUE){
      
      ###direct calculate
      raw.frequency <- data.frame(age=agelist$age)
      
      for (row in 1:nrow(config)) {
        title_ <- config[row]
        
        for (i in 1:nr2){
          raw.frequency <- data.frame(raw.frequency, eval(parse(text=paste0(title_, i, ".freq")))$raw.rates)
          names(raw.frequency)[i+1+(row-1)*nr2] <- paste0(title_, i, ".freq")
        }
      }
      
      raw.frequency$All <- tmp.rowSum.w.na(raw.frequency[2:ncol(raw.frequency)])
      raw.frequency <- raw.frequency %>% relocate(All, .after = age)
      
      #non marital frequency
      nonmar.freq <- data.frame(age = raw.frequency$age, All = raw.frequency$All)
      for (i in 1:nr2) {
        nonmar.freq <- data.frame(nonmar.freq, tmp.rowSum.w.na(raw.frequency[seq(2+i,7*nr2+2, by=nr2)]))
        names(nonmar.freq)[i+2] <- paste0("p", i, ".freq")
      }
      
      #add non marital and total for each marital status
      raw.frequency <- data.frame(nonmar.freq, raw.frequency[3:ncol(raw.frequency)])
      raw.frequency$all.nmn <- tmp.rowSum.w.na(raw.frequency[(3+nr2):(2+2*nr2)])
      raw.frequency$all.m <- tmp.rowSum.w.na(raw.frequency[(3+2*nr2):(2+3*nr2)])
      raw.frequency$all.wnc <- tmp.rowSum.w.na(raw.frequency[(3+3*nr2):(2+4*nr2)])
      raw.frequency$all.dnc <- tmp.rowSum.w.na(raw.frequency[(3+4*nr2):(2+5*nr2)])
      raw.frequency$all.nmc <- tmp.rowSum.w.na(raw.frequency[(3+5*nr2):(2+6*nr2)])
      raw.frequency$all.wc <- tmp.rowSum.w.na(raw.frequency[(3+6*nr2):(2+7*nr2)])
      raw.frequency$all.dc <- tmp.rowSum.w.na(raw.frequency[(3+7*nr2):(2+8*nr2)])
      
      raw.frequency <- raw.frequency %>% relocate(all.nmn, .before = pnmn1.freq)
      raw.frequency <- raw.frequency %>% relocate(all.m, .before = pm1.freq)
      raw.frequency <- raw.frequency %>% relocate(all.wnc, .before = pwnc1.freq)
      raw.frequency <- raw.frequency %>% relocate(all.dnc, .before = pdnc1.freq)
      raw.frequency <- raw.frequency %>% relocate(all.nmc, .before = pnmc1.freq)
      raw.frequency <- raw.frequency %>% relocate(all.wc, .before = pwc1.freq)
      raw.frequency <- raw.frequency %>% relocate(all.dc, .before = pdc1.freq)
      
      raw.total.rates <- total.freq(nonmar.freq)
      raw.mean.age <- mean.age.cal(nonmar.freq)
      
      freq.total <- data.frame(age = "Total", total.freq(raw.frequency))
      raw.frequency <- rbind(raw.frequency, freq.total)
      rm(freq.total)
      
      #add estimated frequency
      if (!is.na(method)){
        
        frequency <- data.frame(age=agelist$age)
        
        for (row in 1:nrow(config)) {
          title_ <- config[row]
          
          for (i in 1:nr2){
            frequency <- data.frame(frequency, eval(parse(text=paste0(title_, i, ".freq")))$est.rates)
            names(frequency)[i+1+(row-1)*nr2] <- paste0(title_, i, ".freq")
          }
        }
        
        frequency$All <- tmp.rowSum.w.na(frequency[2:ncol(frequency)])
        frequency <- frequency %>% relocate(All, .after = age)
        
        #non marital frequency
        nonmar.freq <- data.frame(age = frequency$age, All = frequency$All)
        for (i in 1:nr2) {
          nonmar.freq <- data.frame(nonmar.freq, tmp.rowSum.w.na(frequency[seq(2+i,7*nr2+2, by=nr2)]))
          names(nonmar.freq)[i+2] <- paste0("p", i, ".freq")
        }
        
        #add non marital and total for each marital status
        frequency <- data.frame(nonmar.freq, frequency[3:ncol(frequency)])
        frequency$all.nmn <- tmp.rowSum.w.na(frequency[(3+nr2):(2+2*nr2)])
        frequency$all.m <- tmp.rowSum.w.na(frequency[(3+2*nr2):(2+3*nr2)])
        frequency$all.wnc <- tmp.rowSum.w.na(frequency[(3+3*nr2):(2+4*nr2)])
        frequency$all.dnc <- tmp.rowSum.w.na(frequency[(3+4*nr2):(2+5*nr2)])
        frequency$all.nmc <- tmp.rowSum.w.na(frequency[(3+5*nr2):(2+6*nr2)])
        frequency$all.wc <- tmp.rowSum.w.na(frequency[(3+6*nr2):(2+7*nr2)])
        frequency$all.dc <- tmp.rowSum.w.na(frequency[(3+7*nr2):(2+8*nr2)])
        
        frequency <- frequency %>% relocate(all.nmn, .before = pnmn1.freq)
        frequency <- frequency %>% relocate(all.m, .before = pm1.freq)
        frequency <- frequency %>% relocate(all.wnc, .before = pwnc1.freq)
        frequency <- frequency %>% relocate(all.dnc, .before = pdnc1.freq)
        frequency <- frequency %>% relocate(all.nmc, .before = pnmc1.freq)
        frequency <- frequency %>% relocate(all.wc, .before = pwc1.freq)
        frequency <- frequency %>% relocate(all.dc, .before = pdc1.freq)
        
        freq.total <- data.frame(age = "Total",  total.freq(frequency))
        frequency <- rbind(frequency, freq.total)
        rm(freq.total)
        
        est.total.rates <- total.freq(nonmar.freq)
        est.mean.age <- mean.age.cal(nonmar.freq)
        
        total.rates <- rbind(raw.total.rates, est.total.rates, (est.total.rates-raw.total.rates)/raw.total.rates)
        mean.age <- rbind(raw.mean.age, est.mean.age, (est.mean.age-raw.mean.age)/raw.mean.age)
        total.rates <- do.call(data.frame, lapply(total.rates, function(x) replace(x, is.infinite(x), NA)))
        
      } else {
        frequency <- NA
        total.rates <- NA
        mean.age <- NA
      }
      
    }else {
      frequency <- NA
      raw.frequency <- NA
      total.rates <- NA
      mean.age<- NA
    }
    
  }
  
  return(list(oe.rates=oe.rates, frequency=frequency,
              raw.oe.rates = raw.oe.rates, raw.frequency = raw.frequency,
              total.rates = total.rates, mean.age = mean.age))
}

## Newly Added Covar function ##
compute.fert.covar <- function(data_oe, data_freq, param, code, plot, plot.name, byvar, mfp){
  
  #parameters
  nlb <- as.numeric(param$nlb)
  nhb <- as.numeric(param$nhb)
  nr2 <- as.numeric(param$nr2)
  title <- as.character(param$title)
  t1Month <- as.numeric(param$t1Month)
  t2Month <- as.numeric(param$t2Month)
  nWeight <- as.numeric(param$nWeight)
  nrate <- as.numeric(param$nRate)
  nRegion <- as.numeric(param$nRegion)
  nRace <- as.numeric(param$nRace)
  
  period <- paste0((t1Month-1)%/%12+1900, " - ",(t2Month-1)%/%12+1900, sep="")
  

  #options indicating whether calculate oe or freq
  cal_oe <- (nrate==1 | nrate==2)
  cal_freq <- (nrate==1 | nrate==3)
  
  #region list
  if (nRegion>1){
    region_tmp<- code %>% select(`Region Code`, `Region Name`)
    names(region_tmp) <- c("code", "name")
    region_tmp <- region_tmp %>% filter(!is.na(name))
    region_list <- as.list(region_tmp)
  }
  
  #race list
  if (nRace>1){
    race_tmp<- code %>% select(`Race Code`, `Race Name`)
    names(race_tmp) <- c("code", "name")
    race_tmp <- race_tmp %>% filter(!is.na(name))
    race_list <- as.list(race_tmp)
  }
  
  
  #set rates as NA first, change later if cal_oe/cal_freq is TRUE.
  oe.rates <- raw.oe.rates <- frequency <- raw.frequency <- NA
  
  if(cal_oe == TRUE){
    
    #keep only female sample
    female_oe <- subset(data_oe, sex == 2)
    
    agelist <- null.rates(female_oe, nlb, nhb, byvar)
    
    #estimate rates for each parity
    for (i in 1:nr2){
      
      #the lowest fertility rates increase 1 year per parity
      female_ <- female_oe[which(female_oe$age >= nlb + i -1),]
      p.oe <- merge(agelist, oe.est.byvar(female_, nlb, nhb, i-1, i, byvar, nWeight,mfp), all.x=TRUE)
      assign(paste0("p", i, ".oe", sep=""), p.oe)
    }
    
  }
  
  if(cal_freq == TRUE){
    
    #keep only female sample
    female_freq <- subset(data_freq, sex == 2)
    
    agelist <- null.rates(female_freq, nlb, nhb, byvar)
    
    for (i in 1:nr2){
      
      #the lowest fertility rates increase 1 year per parity
      female_ <- female_freq[which(female_freq$age >= nlb + i -1),]
      p.freq <- merge(agelist, freq.est.byvar(female_, nlb, nhb, i, byvar, nWeight, mfp), all.x=TRUE)
      assign(paste0("p", i, ".freq", sep=""), p.freq)
    }
    
  }
  
  
  if (plot==TRUE) {
    
    wb <- createWorkbook()
    # define worksheet number
    k<-1
    
    if(cal_oe == TRUE){
      # oe
      addWorksheet(wb, "oe")
      
      writeData(wb, sheet=k, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
      writeData(wb, sheet=k, paste0("Period: ", period, sep=""), startRow=2, startCol=1)
      k<-k+1    #if cal_freq==T, store in sheet k=2.
      
      row.index <- 3
      
      for (i in 1:nr2){
        
        ## oe does not consider nr2+, due to risk population
        
        p <- rates.plot(eval(parse(text=paste0("p", i, ".oe", sep=""))), nlb, nhb, "o/e rate", paste0("Figure 4.",i," Age specific fertility o/e rates, parity ",i, plot.name, sep=""))
        print(p)
        insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
        row.index <- row.index + 22
        
      }
      
    }
    
    
    if(cal_freq == TRUE){
      # fre
      addWorksheet(wb, "freq")
      
      writeData(wb, sheet=k, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
      writeData(wb, sheet=k, paste0("Period: ", period, sep=""), startRow=2, startCol=1)
      
      row.index <- 3
      
      for (i in 1:nr2){
        
        ##only frequency consider nr2+
        
        if (i<nr2){
          p <- rates.plot(eval(parse(text=paste0("p", i, ".freq", sep=""))), nlb, nhb, "frequency", paste0("Figure 5.",i," Age specific fertility frequencies, parity ",i, plot.name, sep=""))
          print(p)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
          row.index <- row.index + 22
        } else {
          
          #if last event: name as nr2+
          p <- rates.plot(eval(parse(text=paste0("p", i, ".freq", sep=""))), nlb, nhb, "frequency", paste0("Figure 5.",i," Age specific fertility frequencies, parity ",i, "+", plot.name, sep=""))
          print(p)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
        }
        
      }
      
    }
    
    
    output.dir <- getwd()
    plot.name <- gsub(", ", "-", plot.name)
    saveWorkbook(wb, paste0(output.dir, "/", title, " non-mar Fertility plot", plot.name, ".xlsx", sep=""), overwrite=TRUE)
    
  }
  
  
  if(cal_oe == TRUE){
    
    agelist <- arrange.covar(agelist, byvar) %>% distinct()
    oe.rates <- raw.oe.rates <- agelist
    
    #estimated rates
    for (i in 1:nr2){
      poi.oe_ <- eval(parse(text=paste0("p", i, ".oe")))$est.rates
      poi.oe_[poi.oe_ >= 2] <- NA
      oe.rates <- data.frame(oe.rates, poi.oe_)
      names(oe.rates)[i+ncol(agelist)] <- paste0("p", i, ".oe")
    }
    
    #direct calculated rates
    for (i in 1:nr2){
      raw.oe_ <- eval(parse(text=paste0("p", i, ".oe")))$raw.rates
      raw.oe_ [raw.oe_ >= 2] <- NA
      raw.oe.rates <- data.frame(raw.oe.rates, raw.oe_)
      names(raw.oe.rates)[i+ncol(agelist)] <- paste0("p", i, ".oe")
    }
    
  }
  
  
  if(cal_freq == TRUE){
    
    agelist <- arrange.covar(agelist, byvar) %>% distinct()
    frequency <- raw.frequency <- agelist
    
    #estimated rates
    for (i in 1:nr2){
      frequency <- data.frame(frequency, eval(parse(text=paste0("p", i, ".freq")))$est.rates)
      names(frequency)[i+ncol(agelist)] <- paste0("p", i, ".freq")
    }
    
    #direct calculated rates
    for (i in 1:nr2){
      raw.frequency <- data.frame(raw.frequency, eval(parse(text=paste0("p", i, ".freq")))$raw.rates)
      names(raw.frequency)[i+ncol(agelist)] <- paste0("p", i, ".freq")
    }
    
  }
  
  
  return(list(oe.rates=oe.rates, frequency=frequency,
              raw.oe.rates = raw.oe.rates, raw.frequency = raw.frequency))
}

compute.fertM.covar <- function(data_oe, data_freq, param, code, plot, plot.name, byvar, mfp){
  
  #parameters
  nlb <- as.numeric(param$nlb)
  nhb <- as.numeric(param$nhb)
  nr2 <- as.numeric(param$nr2)
  cohabit <- as.numeric(param$cohabit)
  title <- as.character(param$title)
  t1Month <- as.numeric(param$t1Month)
  t2Month <- as.numeric(param$t2Month)
  nWeight <- as.numeric(param$nWeight)
  nrate <- as.numeric(param$nRate)
  nRegion <- as.numeric(param$nRegion)
  nRace <- as.numeric(param$nRace)
  
  period <- paste0((t1Month-1)%/%12+1900, " - ",(t2Month-1)%/%12+1900, sep="")
  
  #options indicating whether calculate oe or freq
  cal_oe <- (nrate==1 | nrate==2)
  cal_freq <- (nrate==1 | nrate==3)
  
  
  #region list
  if (nRegion>1){
    region_tmp<- code %>% select(`Region Code`, `Region Name`)
    names(region_tmp) <- c("code", "name")
    region_tmp <- region_tmp %>% filter(!is.na(name))
    region_list <- as.list(region_tmp)
  }
  
  #race list
  if (nRace>1){
    race_tmp<- code %>% select(`Race Code`, `Race Name`)
    names(race_tmp) <- c("code", "name")
    race_tmp <- race_tmp %>% filter(!is.na(name))
    race_list <- as.list(race_tmp)
  }
  
  #set rates as NA first, change later if cal_oe/cal_freq is TRUE.
  oe.rates <- raw.oe.rates <- frequency <- raw.frequency <- NA
  
  if (cohabit==0){
    
    ## 4 marital status
    
    #general setting: marital status and plot column index
    config <- rbind(c(1, "never married", 1, "pnm"),
                    c(2, "married", 7, "pm"),
                    c(4, "divorced", 13, "pd"),
                    c(3, "widowed", 19, "pw"))
    
    #if output graph, create workbook
    if (plot==T){
      wb <- createWorkbook()
      # define worksheet number
      k<-1
    }
    
    if(cal_oe == TRUE){
      
      #keep only female sample
      female_oe <- subset(data_oe, sex == 2)
      
      agelist <- null.rates(female_oe, nlb, nhb, byvar)
      
      #if output graph, set worksheet 1
      if (plot==T){
        addWorksheet(wb, "oe")
        
        writeData(wb, sheet=k, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
        writeData(wb, sheet=k, paste0("Period: ", period, sep=""), startRow=2, startCol=1)
        k<-k+1    #if cal_freq==T, store in sheet k=2.
      }
      

      #for each marital status, estimate oe
      p.oes <- list()
      for (row in 1:nrow(config)) {
        mar.bf_ <- as.numeric(config[row, 1])
        type_ <- config[row, 2]
        col_ <- as.numeric(config[row, 3])
        title_ <- config[row, 4]
        
        cl<- makeCluster(max(1, min(nr2, detectCores())))
        registerDoParallel(cl)
        
        ### foreach function is orginally edited by Mo Yan.
        ### oe.est.byvar is changed (by Siyao) due to prediction.data function cannot be found when using oe.est.byvar function.
        for (i in 1:nr2){
          female_ <- female_oe[which(female_oe$age >= nlb + i -1),]
          p.oe <- merge(agelist, oe.est.byvar(subset(female_, mar.bf==mar.bf_), nlb, nhb, i-1, i, byvar, nWeight, mfp), all.x=TRUE)
          p.oes[[i]] <- p.oe
        }
        
        # p.oes <- foreach (i=1:nr2,.combine='list',.export="oe.est.byvar",.packages=c("dplyr","mfp"),.multicombine=TRUE) %dopar% {
        #   female_ <- female_oe[which(female_oe$age >= nlb + i -1),]
        #   p.oe <- merge(agelist, oe.est.byvar(subset(female_, mar.bf==mar.bf_), nlb, nhb, i-1, i, byvar, nWeight, mfp), all.x=TRUE)
        # }
        
        for (i in 1:nr2){
          p.oe <- data.frame(p.oes[i])
          assign(paste0("p", i, ".oe", sep=""), p.oe)  ## to draw the graphs
          assign(paste0(title_, i, ".oe", sep=""), p.oe)
        }
        
        
        #if output graph, insert plots (in different rows) for each marital status (in different columns)
        if (plot==T){
          
          row.index <- 3
          
          for (i in 1:nr2){
            p <- rates.plot(eval(parse(text=paste0("p", i, ".oe", sep=""))), nlb, nhb, "o/e rate", paste0("Figure 4.",i," Age specific fertility o/e rates for ", type_, " women, parity ",i, plot.name, sep=""))
            print(p)
            insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=col_, width=12.55, height=10.4, units="cm")
            row.index <- row.index + 22
          }
          
        }
        
      }
      
    }
    
    if(cal_freq == TRUE){
      
      #keep only female sample
      female_freq <- subset(data_freq, sex == 2)
      
      agelist <- null.rates(female_freq, nlb, nhb, byvar)
      
      #if output graph, set worksheet 2
      if (plot==T){
        addWorksheet(wb, "freq")
        
        writeData(wb, sheet=k, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
        writeData(wb, sheet=k, paste0("Period: ", period, sep=""), startRow=2, startCol=1)
      }
      
      
      #for each marital status, estimate freq
      p.freqs <- list()
      for (row in 1:nrow(config)) {
        mar.bf_ <- as.numeric(config[row, 1])
        type_ <- config[row, 2]
        col_ <- as.numeric(config[row, 3])
        title_ <- config[row, 4]
        
        cl<- makeCluster(max(1, min(nr2, detectCores())))
        registerDoParallel(cl)
        
        
        ### foreach function is orginally edited by Mo Yan.
        ### freqM.est.byvar is changed (by Siyao) due to null.rates function cannot be found when using freqM.est.byvar function.
        for (i in 1:nr2) {
          female_ <- female_freq[which(female_freq$age >= nlb + i -1),]
          p.freq <- merge(agelist, freqM.est.byvar(female_, nlb, nhb, mar.bf_, i, byvar, nWeight, mfp), all.x=TRUE)
          p.freqs[[i]] <- p.freq
        }
        
        # p.freqs <- foreach (i=1:nr2,.combine='list',.export="freqM.est.byvar",.packages=c("dplyr","mfp"),.multicombine=TRUE) %dopar% {
        #   female_ <- female_freq[which(female_freq$age >= nlb + i -1),]
        #   p.freq <- merge(agelist, freqM.est.byvar(female_, nlb, nhb, mar.bf_, i, byvar, nWeight, mfp), all.x=TRUE)
        # }
        
        for (i in 1:nr2){
          p.freq <- data.frame(p.freqs[i])
          assign(paste0("p", i, ".freq", sep=""), p.freq)   ## to draw the graph
          assign(paste0(title_, i, ".freq", sep=""), p.freq)
        }
        
        
        #if output graph, insert plots (in different rows) for each marital status (in different columns)
        if (plot==T){
          
          row.index <- 3
          
          for (i in 1:nr2){
            
            if (i<nr2){
              p <- rates.plot(eval(parse(text=paste0("p", i, ".freq", sep=""))), nlb, nhb, "frequency", paste0("Figure 5.",i," Age specific fertility frequencies for ", type_, " women, parity ",i, plot.name, sep=""))
              print(p)
              insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=col_, width=12.55, height=10.4, units="cm")
              row.index <- row.index + 22
            } else {
              
              #if last event: name as nr2+
              p <- rates.plot(eval(parse(text=paste0("p", i, ".freq", sep=""))), nlb, nhb, "frequency", paste0("Figure 5.",i," Age specific fertility frequencies for ", type_, " women, parity ",i, "+", plot.name, sep=""))
              print(p)
              insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=col_, width=12.55, height=10.4, units="cm")
            }
            
          }
          
        }
        
      }
      
    }
    
    #if output graph, write excel file
    if (plot==T){
      output.dir <- getwd()
      plot.name <- gsub(", ", "-", plot.name)
      saveWorkbook(wb, paste0(output.dir, "/", title, " 4 marital Fertility plot", plot.name, ".xlsx", sep=""), overwrite=TRUE)
    }
    
    
    #### the following part is to summarize all results into oe.rates or frequency dataframe
    
    config <- rbind("pnm", "pm", "pd", "pw")
    
    if(cal_oe == TRUE){
      
      agelist <- arrange.covar(agelist, byvar) %>% distinct()
      oe.rates <- raw.oe.rates <- agelist
      
      for (row in 1:nrow(config)) {
        title_ <- config[row]
        
        for (i in 1:nr2){

          oe.rates_ <- eval(parse(text=paste0(title_, i, ".oe")))$est.rates
          oe.rates_[oe.rates_ >= 2] <- NA
          oe.rates <- data.frame(oe.rates, oe.rates_)
          names(oe.rates)[i+ncol(agelist)+(row-1)*nr2] <- paste0(title_, i, ".oe")
          
          raw.oe_ <- eval(parse(text=paste0(title_, i, ".oe")))$raw.rates
          raw.oe_ [raw.oe_ >= 2] <- NA
          raw.oe.rates <- data.frame(raw.oe.rates, raw.oe_)
          names(raw.oe.rates)[i+ncol(agelist)+(row-1)*nr2] <- paste0(title_, i, ".oe")
          
        }
      }
      
      
    }
    
    if(cal_freq == TRUE){
      
      agelist <- arrange.covar(agelist, byvar) %>% distinct()
      frequency <- raw.frequency <- agelist
      
      for (row in 1:nrow(config)) {
        title_ <- config[row]
        
        for (i in 1:nr2){
          
          frequency <- data.frame(frequency, eval(parse(text=paste0(title_, i, ".freq")))$est.rates)
          names(frequency)[i+ncol(agelist)+(row-1)*nr2] <- paste0(title_, i, ".freq")
          
          raw.frequency <- data.frame(raw.frequency, eval(parse(text=paste0(title_, i, ".freq")))$raw.rates)
          names(raw.frequency)[i+ncol(agelist)+(row-1)*nr2] <- paste0(title_, i, ".freq")
          
        }
      }

    }
    
    
  } else if (cohabit==1) {
    
    #general setting: marital status and plot column index
    config <- rbind(c(1, "never married and not cohabiting", 1, "pnmnc"),
                    c(2, "married", 8, "pm"),
                    c(3, "widowed and not cohabiting", 15, "pwnc"),
                    c(4, "divorced and not cohabiting", 22, "pdnc"),
                    c(5, "never married and cohabiting", 29, "pnmc"),
                    c(7, "widowed and cohabiting", 36, "pwc"),
                    c(8, "divorced and cohabiting", 43, "pdc"))
    
    #if output graph, create workbook
    if (plot==T){
      wb <- createWorkbook()
      # define worksheet number
      k<-1
    }
    
    if(cal_oe == TRUE){
      
      #if output graph, set worksheet 1
      if (plot==T){
        addWorksheet(wb, "oe")
        
        writeData(wb, sheet=k, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
        writeData(wb, sheet=k, paste0("Period: ", period, sep=""), startRow=2, startCol=1)
        k<-k+1    #if cal_freq==T, store in sheet k=2.
      }
      
      #separate by gender
      female_oe <- subset(data_oe, sex == 2)
      
      #for each marital status, estimate oe
      for (row in 1:nrow(config)) {
        mar.bf_ <- as.numeric(config[row, 1])
        type_ <- config[row, 2]
        col_ <- as.numeric(config[row, 3])
        title_ <- config[row, 4]
        
        cl<- makeCluster(max(1, min(nr2, detectCores())))
        registerDoParallel(cl)
        
        p.oes <- foreach (i=1:nr2,.combine='list',.export="oe.est.byvar",.packages=c("dplyr","mfp"),.multicombine=TRUE) %dopar% {
          female_ <- female_oe[which(female_oe$age >= nlb + i -1),]
          p.oe <- merge(agelist, oe.est.byvar(subset(female_, mar.bf==mar.bf_), nlb, nhb, i-1, i, byvar, nWeight, mfp), all.x=TRUE)
        }
        
        for (i in 1:nr2){
          p.oe <- data.frame(p.oes[i])
          assign(paste0("p", i, ".oe", sep=""), p.oe)
          assign(paste0(title_, i, ".oe", sep=""), p.oe)
        }
        

        #if output graph, insert plots (in different rows) for each marital status (in different columns)
        if (plot==T){
          
          row.index <- 3
          
          for (i in 1:nr2){
            p <- rates.plot(eval(parse(text=paste0("p", i, ".oe", sep=""))), nlb, nhb, "o/e rate", paste0("Figure 4.",i," Age specific fertility o/e rates for ", type_, " women, parity ",i, plot.name, sep=""))
            print(p)
            insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=col_, width=12.55, height=10.4, units="cm")
            row.index <- row.index + 22
          }
          
        }
        
      }
      
    }
    
    if(cal_freq == TRUE){
      
      #if output graph, set worksheet 2
      if (plot==T){
        addWorksheet(wb, "freq")
        
        writeData(wb, sheet=k, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
        writeData(wb, sheet=k, paste0("Period: ", period, sep=""), startRow=2, startCol=1)
      }
      
      #separate by gender
      female_freq <- subset(data_freq, sex == 2)
      
      #for each marital status, estimate freq
      for (row in 1:nrow(config)) {
        mar.bf_ <- as.numeric(config[row, 1])
        type_ <- config[row, 2]
        col_ <- as.numeric(config[row, 3])
        title_ <- config[row, 4]
        
        cl<- makeCluster(max(1, min(nr2, detectCores())))
        registerDoParallel(cl)
        
        
        p.freqs <- foreach (i=1:nr2,.combine='list',.export="freqM.est.byvar",.packages=c("dplyr","mfp"),.multicombine=TRUE) %dopar% {
          female_ <- female_freq[which(female_freq$age >= nlb + i -1),]
          p.freq <- merge(agelist, freqM.est.byvar(female_, nlb, nhb, mar.bf_, i, byvar, nWeight, mfp), all.x=TRUE)
        }
        
        for (i in 1:nr2){
          p.freq <- data.frame(p.freqs[i])
          assign(paste0("p", i, ".freq", sep=""), p.freq)
          assign(paste0(title_, i, ".freq", sep=""), p.freq)
        }
        

        #if output graph, insert plots (in different rows) for each marital status (in different columns)
        if (plot==T){
          
          row.index <- 3
          
          for (i in 1:nr2){
            
            if (i<nr2){
              p <- rates.plot(eval(parse(text=paste0("p", i, ".freq", sep=""))), nlb, nhb, "frequency", paste0("Figure 5.",i," Age specific fertility frequencies for ", type_, " women, parity ",i, plot.name, sep=""))
              print(p)
              insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=col_, width=12.55, height=10.4, units="cm")
              row.index <- row.index + 22
            } else {
              
              #if last event: name as nr2+
              p <- rates.plot(eval(parse(text=paste0("p", i, ".freq", sep=""))), nlb, nhb, "frequency", paste0("Figure 5.",i," Age specific fertility frequencies for ", type_, " women, parity ",i, "+", plot.name, sep=""))
              print(p)
              insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=col_, width=12.55, height=10.4, units="cm")
            }
            
          }
          
        }
        
      }
      
    }
    
    #if output graph, write excel file
    if (plot==T){
      output.dir <- getwd()
      plot.name <- gsub(", ", "-", plot.name)
      saveWorkbook(wb, paste0(output.dir, "/", title, " 7 marital Fertility plot", plot.name, ".xlsx", sep=""), overwrite=TRUE)
    }
    
    
    #### the following part is to summarize all results into oe.rates or frequency dataframe
    
    config <- rbind("pnmnc", "pm", "pwnc", "pdnc", "pnmc", "pwc", "pdc")
    
    if(cal_oe == TRUE){
      
      agelist <- arrange.covar(agelist, byvar) %>% distinct()
      oe.rates <- raw.oe.rates <- agelist
      
      for (row in 1:nrow(config)) {
        title_ <- config[row]
        
        for (i in 1:nr2){
          
          oe.rates_ <- eval(parse(text=paste0(title_, i, ".oe")))$est.rates
          oe.rates_[oe.rates_ >= 2] <- NA
          oe.rates <- data.frame(oe.rates, oe.rates_)
          names(oe.rates)[i+ncol(agelist)+(row-1)*nr2] <- paste0(title_, i, ".oe")
          
          raw.oe_ <- eval(parse(text=paste0(title_, i, ".oe")))$raw.rates
          raw.oe_ [raw.oe_ >= 2] <- NA
          raw.oe.rates <- data.frame(raw.oe.rates, raw.oe_)
          names(raw.oe.rates)[i+ncol(agelist)+(row-1)*nr2] <- paste0(title_, i, ".oe")
          
        }
      }
      
    }
    
    if(cal_freq == TRUE){
      
      agelist <- arrange.covar(agelist, byvar) %>% distinct()
      frequency <- raw.frequency <- agelist
      
      for (row in 1:nrow(config)) {
        title_ <- config[row]
        
        for (i in 1:nr2){
          
          frequency <- data.frame(frequency, eval(parse(text=paste0(title_, i, ".freq")))$est.rates)
          names(frequency)[i+ncol(agelist)+(row-1)*nr2] <- paste0(title_, i, ".freq")
          
          raw.frequency <- data.frame(raw.frequency, eval(parse(text=paste0(title_, i, ".freq")))$raw.rates)
          names(raw.frequency)[i+ncol(agelist)+(row-1)*nr2] <- paste0(title_, i, ".freq")
          
        }
      }
      
    }
    
  }
  
  return(list(oe.rates=oe.rates, frequency=frequency,
              raw.oe.rates = raw.oe.rates, raw.frequency = raw.frequency))
}


##------Prepare Data-------
data.prepare.birth <- function(data, marital, nr2, t1Month, t2Month, nlb, nhb){
  info <- data %>% select(region:events)
  evt <- data %>% select(ID, m1:ncol(data))
  long <- reshape(evt, idvar = "ID",
                  varying = names(evt)[2:ncol(evt)],
                  v.names = c("event", "m", "mar.bf"),
                  timevar = "event_index",
                  times = 1:((ncol(evt)-1)/3),
                  new.row.names = 1:10000000,
                  direction = "long") %>% arrange(ID, event_index)
  names(long)[3:4] <- names(long)[4:3]
  
  #keep only birth events
  birth <- long %>%
    mutate(m = replace(m, which(event > 100), NA)) %>%
    mutate(event = replace(event, which(event > 100), NA)) %>%
    arrange(ID, m, event) %>%
    group_by(ID) %>%
    mutate(new_index = row_number()) %>%
    filter(rowSums(is.na(cbind(event, m)))<2 | new_index==1)
  
  #generate parity at t2
  birth <- birth %>% left_join(info[,c("ID", "events", "parity")], by = "ID")
  birth <- parity.t2(birth, nr2, t2Month)
  
  #keep only marital events
  if (marital==4|marital==7){
    mar <- long %>%
      mutate(m = replace(m, which(event < 100), NA)) %>%
      mutate(event = replace(event, which(event < 100), NA)) %>%
      arrange(ID, m, event) %>%
      group_by(ID) %>%
      mutate(new_index = row_number()) %>%
      filter(rowSums(is.na(cbind(event, m)))<2 | new_index==1)
    mar <- mar %>% left_join(info[,c("ID", "mar")], by = "ID")
    rm(long)
    rm(evt)
    rm(info)
  }
  
  #generate marital status at t2
  if(marital==0){
    data <- merge(data, birth) %>% relocate(parity2, .after = parity)
    data$mar2 <- NA_real_
    data <- data %>% relocate(mar2, .after = mar)
    rm(birth)
  } else if (marital==4){
    mar <- mar.bf4.t2(mar, t2Month)
    data <- merge(data, birth) %>% relocate(parity2, .after = parity)
    data <- merge(data, mar) %>% relocate(mar2, .after = mar)
    rm(birth)
    rm(mar)
  } else if (marital==7){
    mar <- mar.bf7.t2(mar, t2Month)
    data <- merge(data, birth) %>% relocate(parity2, .after = parity)
    data <- merge(data, mar) %>% relocate(mar2, .after = mar)
    rm(birth)
    rm(mar)
  }
  
  
  ## Convert each record into 1 person month/row
  
  #define start time t0 and end time t
  data[, "t"] <-  round(t2Month - data$bMonth + 1, 0)
  data[, "t0"] <-  round(t1Month - data$bMonth, 0)
  data[,"fake.event"] <- 1
  data <- survSplit(Surv(data$t0, data$t, data$fake.event)~., data, cut=seq(min(data$t0),max(data$t),1),
                    start="t0", end="t")
  
  #add age month for each month
  data[,"period"] <- data$bMonth + data$t0
  #add year info for each month
  data[,"year"] <- (data$period-1)%/%12+1900
  
  #add age for each month
  data$age <- floor(data$t0/12)
  #delete out of range ages
  data <- data[which(data$age >= nlb & data$age <= nhb),]
  
  #number of person year per row: 0.08333333
  data$py <- (data$t - data$t0)/12
  
  #set event and marital status as NA first, and fill in the information
  data$event <- NA_real_
  data$mar.bf <- NA_real_
  
  for(i in 1:max(data$events)){
    event_m <- paste0("m", i, sep="")
    event_type <- paste0("event", i, sep="")
    event_mar <- paste0("mar.bf", i, sep="")
    temp <- cbind(data[, event_m], data$period)
    data[which(temp[,1] == temp[,2]), "event"] <- data[which(temp[,1] == temp[,2]), event_type]
    data[which(temp[,1] == temp[,2]), "mar.bf"] <- data[which(temp[,1] == temp[,2]), event_mar]
  }
  
  #marital status at end month
  data[which(t2Month == (data$period)), "mar.bf"] <- data[which(t2Month == (data$period)), "mar2"]
  data[which(data$t0==(nhb*12+11)&is.na(data$mar.bf)), "mar.bf"] <- data[which(data$t0==(nhb*12+11)&is.na(data$mar.bf)), "mar2"]
  data <- data %>% group_by(ID) %>% arrange(ID, period) %>% fill(mar.bf, .direction = "up")
  
  #delete marital events
  data[which(is.na(data$event)|data$event>100), "event"] <- 0
  
  # allow multiple births at a month
  if (max(data$events)>1){
    for (i in 1:(max(data$events)-1)){
      temp <- data[which(data[,paste0("m",i)]==data[,paste0("m",i+1)]&data[,paste0("event",i)]<100&data[,paste0("event",i+1)]<100&data[,paste0("m",i)]==data$period),]
      temp$event <- temp$event-1
      data <- rbind(data, temp)
    }
  }
  
  data <- data %>% distinct()
  data <- data %>% arrange(ID, period, event)
  
  #status
  data$prev <- data$post <- NA_real_
  data[which(t2Month == (data$period)), "prev"] <- data[which(t2Month == (data$period)), "parity2"]
  data[which(data$t0==(nhb*12+11)&is.na(data$prev)), "prev"] <- data[which(data$t0==(nhb*12+11)&is.na(data$prev)), "parity2"]
  
  data[which(data$event!=0), "post"] <- data[which(data$event!=0), "event"]
  data[which(data$event!=0), "prev"] <- data[which(data$event!=0), "event"]-1
  
  data[which(data$prev > nr2), "prev"] <- nr2
  data[which(data$post > nr2), "post"] <- nr2
  data[which(data$event > nr2), "event"] <- nr2
  
  data <- as.data.frame(data %>% group_by(ID) %>% fill(post, .direction="down") %>%
                          fill(prev, .direction="up"))
  
  data[which(is.na(data$post)), "post"] <- data[which(is.na(data$post)), "prev"]
  
  data <- data[which(data$period <= data$tMonth),]
  
  data <- data %>% select(region, ID, weight, race, ru, sex, edu, age, event, mar.bf, prev, post, py, year)
  
  return(data)
}

parity.t2 <- function(data, nr2, t2Month){
  
  parity2 <- data %>%
    mutate(event.cp = event) %>%
    mutate(event = replace(event, which(m>t2Month), NA)) %>%
    mutate(m = replace(m, which(m>t2Month), NA)) %>%
    filter(rowSums(is.na(cbind(event, m)))<2 | new_index==1)
  
  parity2 <- parity2 %>% group_by(ID) %>%
    arrange(ID, m) %>%
    mutate(n = n()) %>%
    mutate(row = row_number()) 
  
  parity2$parity2 <- dplyr::case_when(
    # no birth
    parity2$events==0 ~ parity2$parity,
    # all birth before t2: parity
    parity2$events==parity2$n & parity2$row==parity2$n & !is.na(parity2$event) ~ parity2$parity,
    # all birth after t2: first event after t2 - 1
    parity2$events>=parity2$n & parity2$row==parity2$n & is.na(parity2$event) ~ parity2$event.cp-1,
    # some birth before t2: event
    parity2$events>parity2$n & parity2$row==parity2$n & !is.na(parity2$event) ~ parity2$event)
  
  parity2 <- parity2 %>% fill(parity2, .direction = "up") %>% select(-n, -row)
  #if mar2 cannot be figured out by logic, set as the same as mar
  parity2$parity2 <- ifelse(is.na(parity2$parity2), parity2$parity, parity2$parity2)
  
  parity2 <- parity2 %>% select(ID, parity2)
  data <- unique(parity2)
  return(data)
}

mar.bf4.t2 <- function(data, t2Month) {
  mar2 <- data %>% group_by(ID) %>%
    mutate(events = n()) %>%
    mutate(events = ifelse(events==1&is.na(event), 0, events)) %>%
    mutate(mar.cp = mar.bf) %>%
    mutate(event = replace(event, which(m>t2Month), NA)) %>%
    mutate(m = replace(m, which(m>t2Month), NA)) %>%
    filter(rowSums(is.na(cbind(event, m)))<2 | new_index==1)
  
  mar2 <- mar2 %>% group_by(ID) %>%
    arrange(ID, m) %>%
    mutate(n = n()) %>%
    mutate(row = row_number())
  
  mar2$mar2 <- dplyr::case_when(
    #no marriage events
    mar2$events==0 ~ mar2$mar,
    #all marriage before t2: last event
    mar2$events>=mar2$n & mar2$row==mar2$n & mar2$event %in% c(101,102) ~ 2,
    mar2$events>=mar2$n & mar2$row==mar2$n & mar2$event==103 ~ 3,
    mar2$events>=mar2$n & mar2$row==mar2$n & mar2$event==104 ~ 4,
    #all marriage after t2: first mar.bf after t2
    mar2$events>=mar2$n & mar2$row==mar2$n & is.na(mar2$event) ~ mar2$mar.cp)
  
  mar2 <- mar2 %>% fill(mar2, .direction = "up") %>% select(-n, -row)
  #if mar2 cannot be figured out by logic, set as the same as mar
  mar2$mar2 <- ifelse(is.na(mar2$mar2), mar2$mar, mar2$mar2)
  
  mar2 <- mar2 %>% select(ID, mar2)
  data <- unique(mar2)
  return(data)
}

mar.bf7.t2 <- function(data, t2Month) {
  
  mar2 <- data %>% group_by(ID) %>%
    mutate(events = n()) %>%
    mutate(events = ifelse(events==1&is.na(event), 0, events)) %>%
    mutate(mar.cp = mar.bf) %>%
    mutate(new_event = ifelse(event %in% c(101:104), event, NA)) %>%
    mutate(lag.event = lag(new_event)) %>%
    mutate(next.event = lead(new_event)) %>%
    mutate(next.mar.cp = lead(mar.cp)) %>%
    mutate(event = replace(event, which(m>t2Month), NA)) %>%
    mutate(m = replace(m, which(m>t2Month), NA)) %>%
    filter(rowSums(is.na(cbind(event, m)))<2 | new_index==1)
  
  mar2 <- mar2 %>% group_by(ID) %>%
    arrange(ID, m) %>%
    mutate(n = n()) %>%
    mutate(row = row_number())
  
  mar2$mar2 <- dplyr::case_when(
    #no marriage event
    mar2$events==0 ~ mar2$mar,
    #all marriage event before t2
    mar2$events>=mar2$n & mar2$row==mar2$n & mar2$event %in% c(101,102) ~ 2,
    mar2$events>=mar2$n & mar2$row==mar2$n & mar2$event==103 ~ 3,
    mar2$events>=mar2$n & mar2$row==mar2$n & mar2$event==104 ~ 4,
    mar2$events>=mar2$n & mar2$row==mar2$n & mar2$event==105 & mar2$lag.event==103 ~ 7,
    mar2$events>=mar2$n & mar2$row==mar2$n & mar2$event==105 & mar2$lag.event==104 ~ 8,
    mar2$events>=mar2$n & mar2$row==mar2$n & mar2$event==106 & mar2$lag.event==103 ~ 3,
    mar2$events>=mar2$n & mar2$row==mar2$n & mar2$event==106 & mar2$lag.event==104 ~ 4,
    #all events are cohabiting and before t2
    mar2$events==mar2$n & mar2$row==mar2$n & is.na(mar2$lag.event) & mar2$event %in% c(105, 106) ~ mar2$mar,
    #only cohabiting events before t2
    mar2$events>mar2$n & mar2$row==mar2$n & mar2$event==105 & is.na(mar2$lag.event) & mar2$next.event==101 ~ 5,
    mar2$events>mar2$n & mar2$row==mar2$n & mar2$event==106 & is.na(mar2$lag.event) & mar2$next.event==101 ~ 1,
    mar2$events>mar2$n & mar2$row==mar2$n & mar2$event==106 & is.na(mar2$lag.event) & mar2$next.event==102 ~ mar2$next.mar.cp,
    mar2$events>mar2$n & mar2$row==mar2$n & mar2$event==105 & is.na(mar2$lag.event) & mar2$next.event==102 ~ mar2$next.mar.cp,
    #all events after t2
    mar2$events>=mar2$n & mar2$row==mar2$n & is.na(mar2$event) ~ mar2$mar.cp)
  
  mar2 <- mar2 %>% fill(mar2, .direction = "up") %>% select(-n, -row, -new_event, -lag.event, -next.event, -next.mar.cp)
  #if mar2 cannot be figured out by logic, set as the same as mar
  mar2$mar2 <- ifelse(is.na(mar2$mar2), mar2$mar, mar2$mar2)
  
  mar2 <- mar2 %>% select(ID, mar2)
  data <- unique(mar2)
  return(data)
}


#### non marital fertility ####

pop.count.fert <- function(data, nr2, nlb, nhb){
  age <- data.frame(age=seq(nlb, nhb, 1))
  d.001 <- subset(data, sex == 2, select=c(age, post, event, py))
  
  #status
  if(nrow(d.001)==0){
    temp <- data.frame(age=NA,status=NA, count=NA)[F,]
  } else {
    temp <- aggregate(cbind(d.001[0], count=d.001$py),
                      list(age=d.001$age, status=d.001$post), sum)
  }
  
  female <- as.data.frame(merge(age, subset(temp, status == 0, select=c(age, count)), all.x=TRUE))
  for (i in 1:(nr2-1)) {
    female_add <- as.data.frame(merge(age, subset(temp, status == i, select=c(age, count)), all.x=TRUE))
    female <- cbind(female, female_add[,2])
  }
  names(female) <- c("age", paste0("Parity ", 0:(nr2-1), sep = ""))
  
  if(nrow(d.001)==0){
    all.female <- data.frame(age=NA,x=NA)[F,]
  } else {
    all.female <- aggregate(cbind(d.001[0], count=d.001$py),
                     list(age=d.001$age), sum)
  }
  all.female <- merge(age, all.female, all.x = T)
  names(all.female) <- c("age", "count")
  
  status <- data.frame(age=age$age, all=all.female$count, female[,2:(nr2+1)])
  status[, 2:ncol(status)] <- round(status[, 2:ncol(status)], 1)
  risk.total <- data.frame(age = "Total", t(colSums(status[2:ncol(status)], na.rm = T)))
  status <- rbind(status, risk.total)
  rm(risk.total)
  
  # event
  if(nrow(d.001)==0){
    temp <- data.frame(age=NA,event=NA, count=NA)[F,]
  } else {
    temp <- aggregate(cbind(d.001[0], count=1),
                      list(age=d.001$age, event=d.001$event), sum)
  }
  

  female.par <- as.data.frame(merge(age, subset(temp, event == 1, select=c(age, count)), all.x=TRUE))
  for (i in 2:nr2) {
    par_add <- as.data.frame(merge(age, subset(temp, event == i, select=c(age, count)), all.x=TRUE))
    female.par <- cbind(female.par, par_add[,2])
  }
  names(female.par) <- c("age", paste0("Parity ", 1:(nr2-1), sep = ""), paste0("Parity ", nr2, "+", sep = ""))
  
  t <- temp[-which(temp$event == 0),]
  if(nrow(t)==0){
    all.female <- data.frame(age=NA,x=NA)[F,]
  } else {
    all.female <- aggregate(t[,3], list(age=t$age), sum)
  }
  all.female <- merge(age, all.female, all.x = T)
  names(all.female) <- c("age", "count")
  
  event <- data.frame(age=age$age, all=all.female$count, female.par[, 2:(nr2+1)])
  
  event.total <- data.frame(age = "Total", t(colSums(event[2:ncol(event)], na.rm = T)))
  event <- rbind(event, event.total)
  rm(event.total)
  
  return(list(status=status, event=event))
}

write.pop.fert <- function(pop, param, name){
  forDatstl <- createStyle(halign="center", border="TopBottomLeftRight", wrapText = TRUE)
  forDatstl2 <- createStyle(halign="right", border="TopBottomLeftRight", numFmt = "#,##0.0")
  wb <- createWorkbook()
  addWorksheet(wb, "status")
  addWorksheet(wb, "event")
  title <- as.character(param$title)
  t1Month <- as.numeric(param$t1Month)
  t2Month <- as.numeric(param$t2Month)
  nr2 <- as.numeric(param$nr2)
  
  period <- paste0((t1Month-1)%/%12+1900, " - ",(t2Month-1)%/%12+1900, sep="")
  writeData(wb, sheet=1,
            paste0("Table F1. Person-years by age and parity, ", title, ", ", period, name, sep=""),
            startRow=4, startCol=2)
  mergeCells(wb, sheet=1, cols=2:9, rows=4)
  colnames(pop$status) <-
    c("Age", "All", paste0("Parity ", 0:(nr2-1), sep = ""))
  writeData(wb, sheet=1, pop$status, startRow=5, startCol=2, borders="all")
  addStyle(wb, sheet=1, style=forDatstl, rows=5, cols=2:(nr2+3))
  addStyle(wb, sheet=1, style=forDatstl2, rows=6:(5+nrow(pop$status)), cols=3:(nr2+3), gridExpand = TRUE)
  setColWidths(wb, sheet=1, cols=2:(nr2+3), widths=13)
  setColWidths(wb, sheet=1, cols=2, widths=7)
  
  writeData(wb, sheet=2, paste0("Table F2. Number of women who given birth by age and parity, ", title, ", ", period, name, sep=""),
            startRow=4, startCol=2)
  mergeCells(wb, sheet=2, cols=2:9, rows=4)
  colnames(pop$event) <-
    c("Age", "All", paste0("Parity ", 1:(nr2-1), sep = ""), paste0("Parity ", nr2, "+", sep = ""))
  writeData(wb, sheet=2, pop$event, startRow=5, startCol=2, borders="all")
  addStyle(wb, sheet=2, style=forDatstl, rows=5, cols=2:(nr2+3))
  setColWidths(wb, sheet=2, cols=3:(nr2+3), widths=12)
  setColWidths(wb, sheet=2, cols=2, widths=6)
  
  output.dir <- getwd()
  name <- gsub(", ", "-", name)
  saveWorkbook(wb, paste0(output.dir, "/", title, " non-mar Fertility popTable", name, ".xlsx", sep=""), overwrite=T)
}

write.rates.fert <- function(oe.rates, frequency, param, name, method){
  forDatstl <- createStyle(halign="center", border="TopBottomLeftRight")
  forDatstl2 <- createStyle(halign="right", border="TopBottomLeftRight", numFmt = "#,##0.0000")
  wb <- createWorkbook()
  addWorksheet(wb, "oe.rates")
  addWorksheet(wb, "frequency")
  title <- as.character(param$title)
  nr2 <- as.numeric(param$nr2)
  
  writeData(wb, sheet=1, paste0("Table F2. Age-specific fertility o/e rates by parity, ", title, name, sep=""),
            startRow=4, startCol=2)
  mergeCells(wb, sheet=1, cols=2:(nr2+2), rows=4)
  colnames(oe.rates) <-
    c("Age", paste0("Parity ", 1:nr2, sep = ""))
  writeData(wb, sheet=1, oe.rates, startRow=5, startCol=2, borders="all")
  addStyle(wb, sheet=1, style=forDatstl, rows=5, cols=2:(nr2+2))
  addStyle(wb, sheet=1, style=forDatstl2, rows=6:(5+nrow(oe.rates)), cols=3:(nr2+2), gridExpand = TRUE)
  setColWidths(wb, sheet=1, cols=2:(nr2+2), widths=10)
  setColWidths(wb, sheet=1, cols=2, widths=6)
  
  writeData(wb, sheet=2, paste0("Table F1. Age-specific fertility frequencies by parity, ", title, name, sep=""),
            startRow=4, startCol=2)
  mergeCells(wb, sheet=2, cols=2:(nr2+3), rows=4)
  colnames(frequency) <-
    c("Age", "All", paste0("Parity ", 1:(nr2-1), sep = ""), paste0("Parity ", nr2, "+", sep = ""))
  writeData(wb, sheet=2, frequency, startRow=5, startCol=2, borders="all")
  addStyle(wb, sheet=2, style=forDatstl, rows=5, cols=2:(nr2+3))
  addStyle(wb, sheet=2, style=forDatstl2, rows=6:(5+nrow(frequency)), cols=3:(nr2+3), gridExpand = TRUE)
  setColWidths(wb, sheet=2, cols=2:(nr2+3), widths=10)
  setColWidths(wb, sheet=2, cols=2, widths=6)
  
  output.dir <- getwd()
  name <- gsub(", ", "-", name)
  saveWorkbook(wb, paste0(output.dir, "/", title, " non-mar Fertility rates, ", method, name, ".xlsx", sep=""), overwrite=TRUE)
}

#### marital fertility ####
pop.count.fertM <- function(data, param, nr2, nlb, nhb){
  
  cohabit <- as.numeric(param$cohabit)
  
  age <- data.frame(age=seq(nlb, nhb, 1))
  d.001 <- subset(data, sex==2, select=c(age, post, event, mar.bf, py))
  
  if (cohabit==0){
    
    #status
    if(nrow(d.001)==0){
      temp <- data.frame(age=NA,status=NA,mar=NA,count=NA)[F,]
    } else {
      temp <- aggregate(cbind(d.001[0], count=d.001$py),
                        list(age=d.001$age, status=d.001$post, mar=d.001$mar.bf), sum)
    }
    
    female.nm <- as.data.frame(merge(age, subset(temp, status == 0 & mar == 1, select=c(age, count)), all.x=TRUE))
    for (i in 1:(nr2-1)) {
      female_add <- as.data.frame(merge(age, subset(temp, mar == 1 & status == i, select=c(age, count)), all.x=TRUE))
      female.nm <- cbind(female.nm, female_add[,2])
    }
    names(female.nm) <- c("age", paste0("Parity ", 0:(nr2-1), sep = ""))
    
    female.m <- as.data.frame(merge(age, subset(temp, status == 0 & mar == 2, select=c(age, count)), all.x=TRUE))
    for (i in 1:(nr2-1)) {
      female_add <- as.data.frame(merge(age, subset(temp, mar == 2 & status == i, select=c(age, count)), all.x=TRUE))
      female.m <- cbind(female.m, female_add[,2])
    }
    names(female.m) <- c("age", paste0("Parity ", 0:(nr2-1), sep = ""))
    
    female.d <- as.data.frame(merge(age, subset(temp, status == 0 & mar == 4, select=c(age, count)), all.x=TRUE))
    for (i in 1:(nr2-1)) {
      female_add <- as.data.frame(merge(age, subset(temp, mar == 4 & status == i, select=c(age, count)), all.x=TRUE))
      female.d <- cbind(female.d, female_add[,2])
    }
    names(female.d) <- c("age", paste0("Parity ", 0:(nr2-1), sep = ""))
    
    female.w <- as.data.frame(merge(age, subset(temp, status == 0 & mar == 3, select=c(age, count)), all.x=TRUE))
    for (i in 1:(nr2-1)) {
      female_add <- as.data.frame(merge(age, subset(temp, mar == 3 & status == i, select=c(age, count)), all.x=TRUE))
      female.w <- cbind(female.w, female_add[,2])
    }
    names(female.w) <- c("age", paste0("Parity ", 0:(nr2-1), sep = ""))
    
    female <- cbind(female.nm, female.m[,2:(1+nr2)], female.d[,2:(1+nr2)], female.w[,2:(1+nr2)])
    
    if(nrow(d.001)==0){
      all.female <- data.frame(age=NA,x=NA)[F,]
      all.female <- merge(age, all.female, all.x = T)
    } else {
      all <- aggregate(temp[,4], list(age=temp$age), sum)
      all.female <- merge(age, subset(all, select=c(age, x)), all.x=TRUE)
    }
    names(all.female) <- c("age", "count")
    
    status <- data.frame(age=age$age, all=all.female$count, female[,2:ncol(female)])
    status[, 2:ncol(status)] <- round(status[, 2:ncol(status)], 1)
    risk.total <- data.frame(age = "Total", t(colSums(status[2:ncol(status)], na.rm = T)))
    status <- rbind(status, risk.total)
    rm(risk.total)
    
    # event
    if(nrow(d.001)==0){
      temp <- data.frame(age=NA,event=NA,mar=NA,count=NA)[F,]
    } else {
      temp <- aggregate(cbind(d.001[0], count=1),
                        list(age=d.001$age, event=d.001$event, mar=d.001$mar.bf), sum)
    }
    
    female.par.nm <- as.data.frame(merge(age, subset(temp, event == 1 & mar==1, select=c(age, count)), all.x=TRUE))
    for (i in 2:nr2) {
      par_add <- as.data.frame(merge(age, subset(temp, event == i & mar==1, select=c(age, count)), all.x=TRUE))
      female.par.nm <- cbind(female.par.nm, par_add[,2])
    }
    names(female.par.nm) <- c("age", paste0("Parity ", 1:(nr2-1), sep = ""), paste0("Parity ", nr2, "+", sep = ""))
    female.par.m <- as.data.frame(merge(age, subset(temp, event == 1 & mar==2, select=c(age, count)), all.x=TRUE))
    for (i in 2:nr2) {
      par_add <- as.data.frame(merge(age, subset(temp, event == i & mar==2, select=c(age, count)), all.x=TRUE))
      female.par.m <- cbind(female.par.m, par_add[,2])
    }
    names(female.par.m) <- c("age", paste0("Parity ", 1:(nr2-1), sep = ""), paste0("Parity ", nr2, "+", sep = ""))
    female.par.d <- as.data.frame(merge(age, subset(temp, event == 1 & mar==4, select=c(age, count)), all.x=TRUE))
    for (i in 2:nr2) {
      par_add <- as.data.frame(merge(age, subset(temp, event == i & mar==4, select=c(age, count)), all.x=TRUE))
      female.par.d <- cbind(female.par.d, par_add[,2])
    }
    names(female.par.d) <- c("age", paste0("Parity ", 1:(nr2-1), sep = ""), paste0("Parity ", nr2, "+", sep = ""))
    female.par.w <- as.data.frame(merge(age, subset(temp, event == 1 & mar==3, select=c(age, count)), all.x=TRUE))
    for (i in 2:nr2) {
      par_add <- as.data.frame(merge(age, subset(temp, event == i & mar==3, select=c(age, count)), all.x=TRUE))
      female.par.w <- cbind(female.par.w, par_add[,2])
    }
    names(female.par.w) <- c("age", paste0("Parity ", 1:(nr2-1), sep = ""), paste0("Parity ", nr2, "+", sep = ""))
    
    female.par <- cbind(female.par.nm, female.par.m[,2:(1+nr2)], female.par.d[,2:(1+nr2)], female.par.w[,2:(1+nr2)])
    t <- temp[-which(temp$event == 0),]
    
    if(nrow(t)==0){
      all.female <- data.frame(age=NA,x=NA)[F,]
      all.female <- merge(age, all.female, all.x = T)
    } else {
      all.female <- aggregate(t[,4], list(age=t$age), sum)
      all.female <-  merge(age, subset(all, select=c(age, x)), all.x=TRUE)
    }
    names(all.female) <- c("age", "count")
    
  } else if (cohabit==1){
    
    #status
    if(nrow(d.001)==0){
      temp <- data.frame(age=NA,status=NA,mar=NA,count=NA)[F,]
    } else {
      temp <- aggregate(cbind(d.001[0], count=d.001$py),
                        list(age=d.001$age, status=d.001$post, mar=d.001$mar.bf), sum)
    }
    
    female.nmnc <- as.data.frame(merge(age, subset(temp, status == 0 & mar == 1, select=c(age, count)), all.x=TRUE))
    for (i in 1:(nr2-1)) {
      female_add <- as.data.frame(merge(age, subset(temp, mar == 1 & status == i, select=c(age, count)), all.x=TRUE))
      female.nmnc <- cbind(female.nmnc, female_add[,2])
    }
    names(female.nmnc) <- c("age", paste0("Parity ", 0:(nr2-1), sep = ""))
    
    female.m <- as.data.frame(merge(age, subset(temp, status == 0 & mar == 2, select=c(age, count)), all.x=TRUE))
    for (i in 1:(nr2-1)) {
      female_add <- as.data.frame(merge(age, subset(temp, mar == 2 & status == i, select=c(age, count)), all.x=TRUE))
      female.m <- cbind(female.m, female_add[,2])
    }
    names(female.m) <- c("age", paste0("Parity ", 0:(nr2-1), sep = ""))
    
    female.dnc <- as.data.frame(merge(age, subset(temp, status == 0 & mar == 4, select=c(age, count)), all.x=TRUE))
    for (i in 1:(nr2-1)) {
      female_add <- as.data.frame(merge(age, subset(temp, mar == 4 & status == i, select=c(age, count)), all.x=TRUE))
      female.dnc <- cbind(female.dnc, female_add[,2])
    }
    names(female.dnc) <- c("age", paste0("Parity ", 0:(nr2-1), sep = ""))
    
    female.wnc <- as.data.frame(merge(age, subset(temp, status == 0 & mar == 3, select=c(age, count)), all.x=TRUE))
    for (i in 1:(nr2-1)) {
      female_add <- as.data.frame(merge(age, subset(temp, mar == 3 & status == i, select=c(age, count)), all.x=TRUE))
      female.wnc <- cbind(female.wnc, female_add[,2])
    }
    names(female.wnc) <- c("age", paste0("Parity ", 0:(nr2-1), sep = ""))

    female.nmc <- as.data.frame(merge(age, subset(temp, status == 0 & mar == 5, select=c(age, count)), all.x=TRUE))
    for (i in 1:(nr2-1)) {
      female_add <- as.data.frame(merge(age, subset(temp, mar == 5 & status == i, select=c(age, count)), all.x=TRUE))
      female.nmc <- cbind(female.nmc, female_add[,2])
    }
    names(female.nmc) <- c("age", paste0("Parity ", 0:(nr2-1), sep = ""))

    female.wc <- as.data.frame(merge(age, subset(temp, status == 0 & mar == 7, select=c(age, count)), all.x=TRUE))
    for (i in 1:(nr2-1)) {
      female_add <- as.data.frame(merge(age, subset(temp, mar == 7 & status == i, select=c(age, count)), all.x=TRUE))
      female.wc <- cbind(female.wc, female_add[,2])
    }
    names(female.wc) <- c("age", paste0("Parity ", 0:(nr2-1), sep = ""))

    female.dc <- as.data.frame(merge(age, subset(temp, status == 0 & mar == 8, select=c(age, count)), all.x=TRUE))
    for (i in 1:(nr2-1)) {
      female_add <- as.data.frame(merge(age, subset(temp, mar == 8 & status == i, select=c(age, count)), all.x=TRUE))
      female.dc <- cbind(female.dc, female_add[,2])
    }
    names(female.dc) <- c("age", paste0("Parity ", 0:(nr2-1), sep = ""))
    
    female <- cbind(female.nmnc, female.m[,2:(1+nr2)], female.dnc[,2:(1+nr2)], female.wnc[,2:(1+nr2)],
                    female.nmc[,2:(1+nr2)], female.dc[,2:(1+nr2)], female.wc[,2:(1+nr2)])
    
    if(nrow(d.001)==0){
      all.female <- data.frame(age=NA,x=NA)[F,]
      all.female <- merge(age, all.female, all.x = T)
    } else {
      all <- aggregate(temp[,4], list(age=temp$age), sum)
      all.female <- merge(age, subset(all, select=c(age, x)), all.x=TRUE)
    }
    names(all.female) <- c("age", "count")
    
    status <- data.frame(age=age$age, all=all.female$count, female[,2:ncol(female)])
    status[, 2:ncol(status)] <- round(status[, 2:ncol(status)], 1)
    risk.total <- data.frame(age = "Total", t(colSums(status[2:ncol(status)], na.rm = T)))
    status <- rbind(status, risk.total)
    rm(risk.total)
    
    # event
    if(nrow(d.001)==0){
      temp <- data.frame(age=NA,event=NA,mar=NA,count=NA)[F,]
    } else {
      temp <- aggregate(cbind(d.001[0], count=1),
                        list(age=d.001$age, event=d.001$event, mar=d.001$mar.bf), sum)
    }

    female.par.nm <- as.data.frame(merge(age, subset(temp, event == 1 & mar==1, select=c(age, count)), all.x=TRUE))
    for (i in 2:nr2) {
      par_add <- as.data.frame(merge(age, subset(temp, event == i & mar==1, select=c(age, count)), all.x=TRUE))
      female.par.nm <- cbind(female.par.nm, par_add[,2])
    }
    names(female.par.nm) <- c("age", paste0("Parity ", 1:(nr2-1), sep = ""), paste0("Parity ", nr2, "+", sep = ""))
    female.par.m <- as.data.frame(merge(age, subset(temp, event == 1 & mar==2, select=c(age, count)), all.x=TRUE))
    for (i in 2:nr2) {
      par_add <- as.data.frame(merge(age, subset(temp, event == i & mar==2, select=c(age, count)), all.x=TRUE))
      female.par.m <- cbind(female.par.m, par_add[,2])
    }
    names(female.par.m) <- c("age", paste0("Parity ", 1:(nr2-1), sep = ""), paste0("Parity ", nr2, "+", sep = ""))
    female.par.d <- as.data.frame(merge(age, subset(temp, event == 1 & mar==4, select=c(age, count)), all.x=TRUE))
    for (i in 2:nr2) {
      par_add <- as.data.frame(merge(age, subset(temp, event == i & mar==4, select=c(age, count)), all.x=TRUE))
      female.par.d <- cbind(female.par.d, par_add[,2])
    }
    names(female.par.d) <- c("age", paste0("Parity ", 1:(nr2-1), sep = ""), paste0("Parity ", nr2, "+", sep = ""))
    female.par.w <- as.data.frame(merge(age, subset(temp, event == 1 & mar==3, select=c(age, count)), all.x=TRUE))
    for (i in 2:nr2) {
      par_add <- as.data.frame(merge(age, subset(temp, event == i & mar==3, select=c(age, count)), all.x=TRUE))
      female.par.w <- cbind(female.par.w, par_add[,2])
    }
    names(female.par.w) <- c("age", paste0("Parity ", 1:(nr2-1), sep = ""), paste0("Parity ", nr2, "+", sep = ""))
    
    female.par <- cbind(female.par.nm, female.par.m[,2:(1+nr2)], female.par.d[,2:(1+nr2)], female.par.w[,2:(1+nr2)])
    t <- temp[-which(temp$event == 0),]
    if(nrow(t)==0){
      all.female <- data.frame(age=NA,x=NA)[F,]
      all.female <- merge(age, all.female, all.x = T)
    } else {
      all.female <- aggregate(t[,4], list(age=t$age), sum)
      all.female <-  merge(age, subset(all, select=c(age, x)), all.x=TRUE)
    }
    names(all.female) <- c("age", "count")

  }
  
  event <- data.frame(age=age$age, all=all.female$count, female.par[, 2:ncol(female.par)])
  
  event.total <- data.frame(age = "Total", t(colSums(event[2:ncol(event)], na.rm = T)))
  event <- rbind(event, event.total)
  rm(event.total)
  
  return(list(status=status, event=event))
}

# 7 marital to be added #
write.pop.fertM <- function(pop, param, name){
  forDatstl <- createStyle(halign="center", border="TopBottomLeftRight", wrapText = TRUE)
  forDatstl2 <- createStyle(halign="right", border="TopBottomLeftRight", numFmt = "#,##0.0")
  wb <- createWorkbook()
  addWorksheet(wb, "status")
  addWorksheet(wb, "event")
  title <- as.character(param$title)
  t1Month <- as.numeric(param$t1Month)
  t2Month <- as.numeric(param$t2Month)
  nr2 <- as.numeric(param$nr2)
  cohabit <- as.numeric(param$cohabit)
  
  period <- paste0((t1Month-1)%/%12+1900, " - ",(t2Month-1)%/%12+1900, sep="")
  
  if (cohabit==0){
    writeData(wb, sheet=1,
              paste0("Table F1. Person-years by age and parity, ", title, ", ", period, name, sep=""),
              startRow=4, startCol=2)
    mergeCells(wb, sheet=1, cols=2:(nr2*4+3), rows=4)
    writeData(wb, sheet=1, "Never Married", startRow=5, startCol=4, borders="all")
    mergeCells(wb, sheet=1, cols=4:(nr2+3), rows=5)
    writeData(wb, sheet=1, "Married", startRow=5, startCol=(4+nr2), borders="all")
    mergeCells(wb, sheet=1, cols=(4+nr2):(nr2*2+3), rows=5)
    writeData(wb, sheet=1, "Divorced", startRow=5, startCol=(4+nr2*2), borders="all")
    mergeCells(wb, sheet=1, cols=(4+nr2*2):(nr2*3+3), rows=5)
    writeData(wb, sheet=1, "Widowed", startRow=5, startCol=(4+nr2*3), borders="all")
    mergeCells(wb, sheet=1, cols=(4+nr2*3):(nr2*4+3), rows=5)
    colnames(pop$status) <-
      c("Age", "All", rep(paste0("Parity ", 0:(nr2-1), sep = ""), 4))
    writeData(wb, sheet=1, pop$status, startRow=6, startCol=2, borders="all")
    addStyle(wb, sheet=1, style=forDatstl, rows=5:6, cols=2:(nr2*4+3), gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl2, rows=7:(6+nrow(pop$status)), cols=3:(nr2*4+3), gridExpand = TRUE)
    setColWidths(wb, sheet=1, cols=2:(nr2*4+3), widths=10)
    setColWidths(wb, sheet=1, cols=2, widths=6)
    
    writeData(wb, sheet=2, paste0("Table F2. Number of women who given birth by age and parity, ", title, ", ", period, name, sep=""),
              startRow=4, startCol=2)
    mergeCells(wb, sheet=2, cols=2:(nr2*4+3), rows=4)
    writeData(wb, sheet=2, "Never Married", startRow=5, startCol=4, borders="all")
    mergeCells(wb, sheet=2, cols=4:(nr2+3), rows=5)
    writeData(wb, sheet=2, "Married", startRow=5, startCol=(4+nr2), borders="all")
    mergeCells(wb, sheet=2, cols=(4+nr2):(nr2*2+3), rows=5)
    writeData(wb, sheet=2, "Divorced", startRow=5, startCol=(4+nr2*2), borders="all")
    mergeCells(wb, sheet=2, cols=(4+nr2*2):(nr2*3+3), rows=5)
    writeData(wb, sheet=2, "Widowed", startRow=5, startCol=(4+nr2*3), borders="all")
    mergeCells(wb, sheet=2, cols=(4+nr2*3):(nr2*4+3), rows=5)
    colnames(pop$event) <-
      c("Age", "All", rep(c(paste0("Parity ", 1:(nr2-1), sep = ""), paste0("Parity ", nr2, "+", sep = "")), 4))
    writeData(wb, sheet=2, pop$event, startRow=6, startCol=2, borders="all")
    addStyle(wb, sheet=2, style=forDatstl, rows=5:6, cols=2:(nr2*4+3), gridExpand = TRUE)
    addStyle(wb, sheet=2, style=forDatstl2, rows=7:(6+nrow(pop$event)), cols=3:(nr2*4+3), gridExpand = TRUE)
    setColWidths(wb, sheet=2, cols=2:(nr2*4+3), widths=10)
    setColWidths(wb, sheet=2, cols=2, widths=6)
    
  } else if (cohabit==1){
    
    writeData(wb, sheet=1,
              paste0("Table F1. Person-years by age and parity, ", title, ", ", period, name, sep=""),
              startRow=4, startCol=2)
    mergeCells(wb, sheet=1, cols=2:(nr2*7+3), rows=4)
    writeData(wb, sheet=1, "Never Married & Not Cohabiting", startRow=5, startCol=4, borders="all")
    mergeCells(wb, sheet=1, cols=4:(nr2+3), rows=5)
    writeData(wb, sheet=1, "Married", startRow=5, startCol=(4+nr2), borders="all")
    mergeCells(wb, sheet=1, cols=(4+nr2):(nr2*2+3), rows=5)
    writeData(wb, sheet=1, "Divorced & Not Cohabiting", startRow=5, startCol=(4+nr2*2), borders="all")
    mergeCells(wb, sheet=1, cols=(4+nr2*2):(nr2*3+3), rows=5)
    writeData(wb, sheet=1, "Widowed & Not Cohabiting", startRow=5, startCol=(4+nr2*3), borders="all")
    mergeCells(wb, sheet=1, cols=(4+nr2*3):(nr2*4+3), rows=5)
    writeData(wb, sheet=1, "Never Married & Cohabiting", startRow=5, startCol=(4+nr2*4), borders="all")
    mergeCells(wb, sheet=1, cols=(4+nr2*4):(nr2*5+3), rows=5)
    writeData(wb, sheet=1, "Divorced & Cohabiting", startRow=5, startCol=(4+nr2*5), borders="all")
    mergeCells(wb, sheet=1, cols=(4+nr2*5):(nr2*6+3), rows=5)
    writeData(wb, sheet=1, "Widowed & Cohabiting", startRow=5, startCol=(4+nr2*6), borders="all")
    mergeCells(wb, sheet=1, cols=(4+nr2*6):(nr2*7+3), rows=5)
    colnames(pop$status) <-
      c("Age", "All", rep(paste0("Parity ", 0:(nr2-1), sep = ""), 7))
    writeData(wb, sheet=1, pop$status, startRow=6, startCol=2, borders="all")
    addStyle(wb, sheet=1, style=forDatstl, rows=5:6, cols=2:(nr2*7+3), gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl2, rows=7:(6+nrow(pop$status)), cols=3:(nr2*7+3), gridExpand = TRUE)
    setColWidths(wb, sheet=1, cols=2:(nr2*7+3), widths=10)
    setColWidths(wb, sheet=1, cols=2, widths=6)
    
    writeData(wb, sheet=2, paste0("Table F2. Number of women who given birth by age and parity, ", title, ", ", period, name, sep=""),
              startRow=4, startCol=2)
    mergeCells(wb, sheet=2, cols=2:(nr2*7+3), rows=4)
    writeData(wb, sheet=2, "Never Married & Not Cohabiting", startRow=5, startCol=4, borders="all")
    mergeCells(wb, sheet=2, cols=4:(nr2+3), rows=5)
    writeData(wb, sheet=2, "Married", startRow=5, startCol=(4+nr2), borders="all")
    mergeCells(wb, sheet=2, cols=(4+nr2):(nr2*2+3), rows=5)
    writeData(wb, sheet=2, "Divorced & Not Cohabiting", startRow=5, startCol=(4+nr2*2), borders="all")
    mergeCells(wb, sheet=2, cols=(4+nr2*2):(nr2*3+3), rows=5)
    writeData(wb, sheet=2, "Widowed & Not Cohabiting", startRow=5, startCol=(4+nr2*3), borders="all")
    mergeCells(wb, sheet=2, cols=(4+nr2*3):(nr2*4+3), rows=5)
    writeData(wb, sheet=2, "Never Married & Cohabiting", startRow=5, startCol=(4+nr2*4), borders="all")
    mergeCells(wb, sheet=2, cols=(4+nr2*4):(nr2*5+3), rows=5)
    writeData(wb, sheet=2, "Divorced & Cohabiting", startRow=5, startCol=(4+nr2*5), borders="all")
    mergeCells(wb, sheet=2, cols=(4+nr2*5):(nr2*6+3), rows=5)
    writeData(wb, sheet=2, "Widowed & Cohabiting", startRow=5, startCol=(4+nr2*6), borders="all")
    mergeCells(wb, sheet=2, cols=(4+nr2*6):(nr2*7+3), rows=5)
    colnames(pop$event) <-
      c("Age", "All", rep(c(paste0("Parity ", 1:(nr2-1), sep = ""), paste0("Parity ", nr2, "+", sep = "")), 7))
    writeData(wb, sheet=2, pop$event, startRow=6, startCol=2, borders="all")
    addStyle(wb, sheet=2, style=forDatstl, rows=5:6, cols=2:(nr2*7+3), gridExpand = TRUE)
    addStyle(wb, sheet=2, style=forDatstl2, rows=7:(6+nrow(pop$event)), cols=3:(nr2*7+3), gridExpand = TRUE)
    setColWidths(wb, sheet=2, cols=2:(nr2*7+3), widths=10)
    setColWidths(wb, sheet=2, cols=2, widths=6)
    
  }
  
  output.dir <- getwd()
  name <- gsub(", ", "-", name)
  saveWorkbook(wb, paste0(output.dir, "/", title, " mar Fertility popTable", name, ".xlsx", sep=""), overwrite=T)
}

write.rates.fertM <- function(oe.rates, frequency, param, name, method){
  forDatstl <- createStyle(halign="center", border="TopBottomLeftRight")
  forDatstl2 <- createStyle(halign="right", border="TopBottomLeftRight", numFmt = "#,##0.0000")
  wb <- createWorkbook()
  addWorksheet(wb, "oe.rates")
  addWorksheet(wb, "frequency")
  title <- as.character(param$title)
  nr2 <- as.numeric(param$nr2)
  cohabit <- as.numeric(param$cohabit)
  
  if (cohabit==0){
    writeData(wb, sheet=1, paste0("Table F2. Age-specific fertility o/e rates by parity and marital status, ", title, name, sep=""),
              startRow=4, startCol=2)
    mergeCells(wb, sheet=1, cols=2:(nr2*4+2), rows=4)
    writeData(wb, sheet=1, "Never Married", startRow=5, startCol=3, borders="all")
    mergeCells(wb, sheet=1, cols=3:(nr2+2), rows=5)
    writeData(wb, sheet=1, "Married", startRow=5, startCol=(3+nr2), borders="all")
    mergeCells(wb, sheet=1, cols=(3+nr2):(nr2*2+2), rows=5)
    writeData(wb, sheet=1, "Divorced", startRow=5, startCol=(3+nr2*2), borders="all")
    mergeCells(wb, sheet=1, cols=(3+nr2*2):(nr2*3+2), rows=5)
    writeData(wb, sheet=1, "Widowed", startRow=5, startCol=(3+nr2*3), borders="all")
    mergeCells(wb, sheet=1, cols=(3+nr2*3):(nr2*4+2), rows=5)
    colnames(oe.rates) <-
      c("Age", rep(c(paste0("Parity ", 1:(nr2-1), sep = ""), paste0("Parity ", nr2, "+", sep = "")), 4))
    writeData(wb, sheet=1, oe.rates, startRow=6, startCol=2, borders="all")
    addStyle(wb, sheet=1, style=forDatstl, rows=5:6, cols=2:(nr2*4+2), gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl2, rows=7:(6+nrow(oe.rates)), cols=3:(nr2*4+2), gridExpand = TRUE)
    setColWidths(wb, sheet=1, cols=2:(nr2*4+2), widths=10)
    setColWidths(wb, sheet=1, cols=2, widths=6)
    
    writeData(wb, sheet=2, paste0("Table F1. Age-specific fertility frequencies by parity and marital status, ", title, name, sep=""),
              startRow=4, startCol=2)
    mergeCells(wb, sheet=2, cols=2:(nr2*4+3), rows=4)
    writeData(wb, sheet=2, "All marital status combined", startRow=5, startCol=3, borders="all")
    mergeCells(wb, sheet=2, cols=3:(nr2+3), rows=5)
    writeData(wb, sheet=2, "Never Married", startRow=5, startCol=(4+nr2), borders="all")
    mergeCells(wb, sheet=2, cols=(4+nr2):(2+(nr2+1)*2), rows=5)
    writeData(wb, sheet=2, "Married", startRow=5, startCol=(3+(nr2+1)*2), borders="all")
    mergeCells(wb, sheet=2, cols=(3+(nr2+1)*2):(2+(nr2+1)*3), rows=5)
    writeData(wb, sheet=2, "Divorced", startRow=5, startCol=(3+(nr2+1)*3), borders="all")
    mergeCells(wb, sheet=2, cols=(3+(nr2+1)*3):(2+(nr2+1)*4), rows=5)
    writeData(wb, sheet=2, "Widowed", startRow=5, startCol=(3+(nr2+1)*4), borders="all")
    mergeCells(wb, sheet=2, cols=(3+(nr2+1)*4):(2+(nr2+1)*5), rows=5)
    colnames(frequency) <-
      c("Age", rep(c("All", paste0("Parity ", 1:(nr2-1), sep = ""), paste0("Parity ", nr2, "+", sep = "")), 5))
    writeData(wb, sheet=2, frequency, startRow=6, startCol=2, borders="all")
    addStyle(wb, sheet=2, style=forDatstl, rows=5:6, cols=2:(nr2*5+7), gridExpand = TRUE)
    addStyle(wb, sheet=2, style=forDatstl2, rows=7:(6+nrow(frequency)), cols=3:(nr2*5+7), gridExpand = TRUE)
    setColWidths(wb, sheet=2, cols=2:(nr2*5+7), widths=10)
    setColWidths(wb, sheet=2, cols=2, widths=6)
    
    output.dir <- getwd()
    name <- gsub(", ", "-", name)
    saveWorkbook(wb, paste0(output.dir, "/", title, " 4 marital Fertility rates, ", method, name, ".xlsx", sep=""), overwrite=TRUE)
  
  } else if (cohabit==1){
    
    writeData(wb, sheet=1, paste0("Table F2. Age-specific fertility o/e rates by parity and marital status, ", title, name, sep=""),
              startRow=4, startCol=2)
    mergeCells(wb, sheet=1, cols=2:(nr2*7+2), rows=4)
    
    writeData(wb, sheet=1, "Never married and not cohabiting", startRow=5, startCol=3, borders="all")
    mergeCells(wb, sheet=1, cols=3:(nr2+2), rows=5)
    
    writeData(wb, sheet=1, "Married", startRow=5, startCol=(3+nr2), borders="all")
    mergeCells(wb, sheet=1, cols=(3+nr2):(nr2*2+2), rows=5)
    
    writeData(wb, sheet=1, "Widowed and not cohabiting", startRow=5, startCol=(3+nr2*2), borders="all")
    mergeCells(wb, sheet=1, cols=(3+nr2*2):(nr2*3+2), rows=5)
    
    writeData(wb, sheet=1, "Divorced and not cohabiting", startRow=5, startCol=(3+nr2*3), borders="all")
    mergeCells(wb, sheet=1, cols=(3+nr2*3):(nr2*4+2), rows=5)
    
    writeData(wb, sheet=1, "Never married and cohabiting", startRow=5, startCol=(3+nr2*4), borders="all")
    mergeCells(wb, sheet=1, cols=(3+nr2*4):(nr2*5+2), rows=5)
    
    writeData(wb, sheet=1, "Widowed and cohabiting", startRow=5, startCol=(3+nr2*5), borders="all")
    mergeCells(wb, sheet=1, cols=(3+nr2*5):(nr2*6+2), rows=5)
    
    writeData(wb, sheet=1, "Divorced and cohabiting", startRow=5, startCol=(3+nr2*6), borders="all")
    mergeCells(wb, sheet=1, cols=(3+nr2*6):(nr2*7+2), rows=5)
    
    colnames(oe.rates) <-
      c("Age", rep(c(paste0("Parity ", 1:(nr2-1), sep = ""), paste0("Parity ", nr2, "+", sep = "")), 7))
    writeData(wb, sheet=1, oe.rates, startRow=6, startCol=2, borders="all")
    addStyle(wb, sheet=1, style=forDatstl, rows=5:6, cols=2:(nr2*7+2), gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl2, rows=7:(6+nrow(oe.rates)), cols=3:(nr2*7+2), gridExpand = TRUE)
    setColWidths(wb, sheet=1, cols=2:(nr2*7+2), widths=10)
    setColWidths(wb, sheet=1, cols=2, widths=6)
    
    
    writeData(wb, sheet=2, paste0("Table F1. Age-specific fertility frequencies by parity and marital status, ", title, name, sep=""),
              startRow=4, startCol=2)
    mergeCells(wb, sheet=2, cols=2:(nr2*7+3), rows=4)
    
    writeData(wb, sheet=2, "All marital status combined", startRow=5, startCol=3, borders="all")
    mergeCells(wb, sheet=2, cols=3:(nr2+3), rows=5)
    
    writeData(wb, sheet=2, "Never married and not cohabiting", startRow=5, startCol=(4+nr2), borders="all")
    mergeCells(wb, sheet=2, cols=(4+nr2):(2+(nr2+1)*2), rows=5)
    
    writeData(wb, sheet=2, "Married", startRow=5, startCol=(3+(nr2+1)*2), borders="all")
    mergeCells(wb, sheet=2, cols=(3+(nr2+1)*2):(2+(nr2+1)*3), rows=5)
    
    writeData(wb, sheet=2, "Widowed and not cohabiting", startRow=5, startCol=(3+(nr2+1)*3), borders="all")
    mergeCells(wb, sheet=2, cols=(3+(nr2+1)*3):(2+(nr2+1)*4), rows=5)
    
    writeData(wb, sheet=2, "Divorced and not cohabiting", startRow=5, startCol=(3+(nr2+1)*4), borders="all")
    mergeCells(wb, sheet=2, cols=(3+(nr2+1)*4):(2+(nr2+1)*5), rows=5)
    
    writeData(wb, sheet=2, "Never married and cohabiting", startRow=5, startCol=(3+(nr2+1)*5), borders="all")
    mergeCells(wb, sheet=2, cols=(3+(nr2+1)*5):(2+(nr2+1)*6), rows=5)
    
    writeData(wb, sheet=2, "Widowed and cohabiting", startRow=5, startCol=(3+(nr2+1)*6), borders="all")
    mergeCells(wb, sheet=2, cols=(3+(nr2+1)*6):(2+(nr2+1)*7), rows=5)
    
    writeData(wb, sheet=2, "Divorced and cohabiting", startRow=5, startCol=(3+(nr2+1)*7), borders="all")
    mergeCells(wb, sheet=2, cols=(3+(nr2+1)*7):(2+(nr2+1)*8), rows=5)
    
    
    colnames(frequency) <-
      c("Age", rep(c("All", paste0("Parity ", 1:(nr2-1), sep = ""), paste0("Parity ", nr2, "+", sep = "")), 8))
    writeData(wb, sheet=2, frequency, startRow=6, startCol=2, borders="all")
    addStyle(wb, sheet=2, style=forDatstl, rows=5:6, cols=2:(nr2*8+10), gridExpand = TRUE)
    addStyle(wb, sheet=2, style=forDatstl2, rows=7:(6+nrow(frequency)), cols=3:(nr2*8+10), gridExpand = TRUE)
    setColWidths(wb, sheet=2, cols=2:(nr2*8+10), widths=10)
    setColWidths(wb, sheet=2, cols=2, widths=6)
    
    output.dir <- getwd()
    name <- gsub(", ", "-", name)
    saveWorkbook(wb, paste0(output.dir, "/", title, " 7 marital Fertility rates, ", method, name, ".xlsx", sep=""), overwrite=TRUE)
  }
}

#### total rates output ####
write.total.fert.subset <- function(total.rates, mean.age, param, name, mar){
  forDatstl <- createStyle(halign="center", border="TopBottomLeftRight", wrapText = TRUE)
  forDatstl2 <- createStyle(halign="left", border="TopBottomLeftRight", wrapText = TRUE)
  forDatstl3 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0000")
  forDatstl4 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0")
  forDatstl5 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0%")
  wb <- createWorkbook()
  addWorksheet(wb, "total rates")
  title <- as.character(param$title)
  nr2 <- as.numeric(param$nr2)
  
  if (nrow(total.rates)==3){
    writeData(wb, sheet=1, paste0("Table F3. Total fertility rates and mean ages of fertility by parity, ", title, sep=""),
              startRow=4, startCol=2)
    mergeCells(wb, sheet=1, cols=2:(3+nr2), rows=4)
    rownames(total.rates) <- c("Direct calculate", "Poisson estimate", "Difference%")
    rownames(mean.age) <- c("Direct calculate", "Poisson estimate", "Difference%")
    cname <- c("All", paste0("Parity ", 1:(nr2-1), sep = ""), paste0("Parity ", nr2, "+", sep = ""))
    cname <- as.data.frame(t(cname))
    writeData(wb, sheet=1, cname, startRow=5, startCol=3, colNames = FALSE, borders="all")
    writeData(wb, sheet=1, "Total fertility rates", startRow=6, startCol=2, borders="all")
    writeData(wb, sheet=1, total.rates, startRow=7, startCol=2, colNames = FALSE, rowNames = TRUE, borders="all")
    writeData(wb, sheet=1, "Mean age", startRow=10, startCol=2, borders="all")
    writeData(wb, sheet=1, mean.age, startRow=11, startCol=2, colNames = FALSE, rowNames = TRUE, borders="all")
    addStyle(wb, sheet=1, style=forDatstl, rows=5, cols=2:(3+nr2))
    addStyle(wb, sheet=1, style=forDatstl3, rows=6:8, cols=3:(3+nr2), gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl5, rows=9, cols=3:(3+nr2))
    addStyle(wb, sheet=1, style=forDatstl4, rows=10:12, cols=3:(3+nr2), gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl5, rows=13, cols=3:(3+nr2))
    setColWidths(wb, sheet=1, cols=3:(3+nr2), widths=10)
    setColWidths(wb, sheet=1, cols=2, widths=17)
  } else {
    writeData(wb, sheet=1, paste0("Table F3. Total fertility rates and mean ages of fertility by parity, ", title, sep=""),
              startRow=4, startCol=2)
    mergeCells(wb, sheet=1, cols=2:(3+nr2), rows=4)
    colnames(total.rates) <- c("", "All", paste0("Parity ", 1:(nr2-1), sep = ""), paste0("Parity ", nr2, "+", sep = ""))
    writeData(wb, sheet=1, total.rates, startRow=5, startCol=2, borders="all")
    writeData(wb, sheet=1, mean.age, startRow=6+nrow(total.rates), startCol=2, colNames = FALSE, borders="all")
    addStyle(wb, sheet=1, style=forDatstl, rows=5, cols=2:(3+nr2))
    addStyle(wb, sheet=1, style=forDatstl3, rows=6:(nrow(total.rates)+5), cols=3:(3+nr2), gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl4, rows=(nrow(total.rates)+6):(nrow(total.rates)+nrow(mean.age)+5), cols=3:(3+nr2), gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl5, rows=seq(1, nrow(total.rates)+nrow(mean.age), by=4)+8, cols=3:(3+nr2), gridExpand = TRUE)
    setColWidths(wb, sheet=1, cols=3:(3+nr2), widths=10)
    setColWidths(wb, sheet=1, cols=2, widths=17)
  }
  
  output.dir <- getwd()
  saveWorkbook(wb, paste0(output.dir, "/", title, mar, " Fertility total rates", name, ".xlsx", sep=""), overwrite=T)
  
}

write.total.fert.covar <- function(total.rates, mean.age, param, name, mar){
  forDatstl <- createStyle(halign="center", border="TopBottomLeftRight", wrapText = TRUE)
  forDatstl2 <- createStyle(halign="left", border="TopBottomLeftRight", wrapText = TRUE)
  forDatstl3 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0000")
  forDatstl4 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0")
  forDatstl5 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0%")
  wb <- createWorkbook()
  addWorksheet(wb, "total rates")
  title <- as.character(param$title)
  nr2 <- as.numeric(param$nr2)
  
  if (nrow(total.rates)==3){
    writeData(wb, sheet=1, paste0("Table F3. Total fertility rates and mean ages of fertility by parity, ", title, sep=""),
              startRow=4, startCol=2)
    mergeCells(wb, sheet=1, cols=2:(3+nr2), rows=4)
    
    rownames(total.rates) <- c("Direct calculate", "Poisson estimate", "Difference%")
    rownames(mean.age) <- c("Direct calculate", "Poisson estimate", "Difference%")
    cname <- c("All", paste0("Parity ", 1:(nr2-1), sep = ""), paste0("Parity ", nr2, "+", sep = ""))
    cname <- as.data.frame(t(cname))
    
    writeData(wb, sheet=1, cname, startRow=5, startCol=3, colNames = FALSE, borders="all")
    writeData(wb, sheet=1, "Total fertility rates", startRow=6, startCol=2, borders="all")
    writeData(wb, sheet=1, total.rates, startRow=7, startCol=2, colNames = FALSE, rowNames = TRUE, borders="all")
    writeData(wb, sheet=1, "Mean age", startRow=10, startCol=2, borders="all")
    writeData(wb, sheet=1, mean.age, startRow=11, startCol=2, colNames = FALSE, rowNames = TRUE, borders="all")
    
    addStyle(wb, sheet=1, style=forDatstl, rows=5, cols=2:(3+nr2))
    addStyle(wb, sheet=1, style=forDatstl3, rows=6:8, cols=3:(3+nr2), gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl5, rows=9, cols=3:(3+nr2))
    addStyle(wb, sheet=1, style=forDatstl4, rows=10:12, cols=3:(3+nr2), gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl5, rows=13, cols=3:(3+nr2))
    setColWidths(wb, sheet=1, cols=3:(3+nr2), widths=10)
    setColWidths(wb, sheet=1, cols=2, widths=17)
    
  } else {
    
    writeData(wb, sheet=1, paste0("Table F3. Total fertility rates of fertility by parity, ", title, sep=""),
              startRow=4, startCol=2)
    mergeCells(wb, sheet=1, cols=2:(4+nr2), rows=4)
    
    colnames(total.rates) <- c("Name", "Direct calculate and Poisson estimate", 
                               "All", paste0("Parity ", 1:(nr2-1), sep = ""), paste0("Parity ", nr2, "+", sep = ""))
    writeData(wb, sheet=1, total.rates, startRow=5, startCol=2, borders="all")
    addStyle(wb, sheet=1, style=forDatstl, rows=5, cols=2:(4+nr2), gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl3, rows=6:(nrow(total.rates)+5), cols=4:(4+nr2), gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl5, rows=seq(1, nrow(total.rates), by=3)+7, cols=4:(4+nr2), gridExpand = TRUE)
    setColWidths(wb, sheet=1, cols=4:(4+nr2), widths=10)
    setColWidths(wb, sheet=1, cols=2, widths=13)
    setColWidths(wb, sheet=1, cols=3, widths=17)
    
    addWorksheet(wb, "mean ages")
    writeData(wb, sheet=2, paste0("Table F3. Mean ages of fertility by parity, ", title, sep=""),
              startRow=4, startCol=2)
    mergeCells(wb, sheet=2, cols=2:(4+nr2), rows=4)
    
    colnames(mean.age) <- c("Name", "Direct calculate and Poisson estimate", 
                               "All", paste0("Parity ", 1:(nr2-1), sep = ""), paste0("Parity ", nr2, "+", sep = ""))
    writeData(wb, sheet=2, mean.age, startRow=5, startCol=2, borders="all")
    addStyle(wb, sheet=2, style=forDatstl, rows=5, cols=2:(4+nr2), gridExpand = TRUE)
    addStyle(wb, sheet=2, style=forDatstl3, rows=6:(nrow(mean.age)+5), cols=4:(4+nr2), gridExpand = TRUE)
    addStyle(wb, sheet=2, style=forDatstl5, rows=seq(1, nrow(mean.age), by=3)+7, cols=4:(4+nr2), gridExpand = TRUE)
    setColWidths(wb, sheet=2, cols=4:(4+nr2), widths=10)
    setColWidths(wb, sheet=2, cols=2, widths=13)
    setColWidths(wb, sheet=2, cols=3, widths=17)
    
  }
  
  output.dir <- getwd()
  saveWorkbook(wb, paste0(output.dir, "/", title, mar, " Fertility total rates", name, ".xlsx", sep=""), overwrite=T)
  
}
