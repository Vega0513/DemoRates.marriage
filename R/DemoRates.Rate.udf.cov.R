#This script saves all the function needed for DemoRates rates estimation.

#source("/Users/cfpr/Desktop/R/DemoRates.Rate.utility.R")

#### The main function for user-defined estimation with co-variant method ####
run.defined.rates.covar <- function(data, param, code, plot, sex, mfp) {
  
  #### Parameters loading and data preparing ####
  
  #Load parameters from param
  nl <- as.numeric(param$nl)
  nh <- as.numeric(param$nh)
  t1Month <- as.numeric(param$t1Month)
  t2Month <- as.numeric(param$t2Month)
  nrj <- as.numeric(param$nrj)
  nRegion <- as.numeric(param$nRegion)
  edu <- as.numeric(param$edu)
  nRace <- as.numeric(param$nRace)
  Marital <- as.numeric(param$Marital)
  Parent <- as.numeric(param$Parent)
  Children <- as.numeric(param$Children)
  nrate <- as.numeric(param$nRate)
  nWeight <- as.numeric(param$nWeight)
  
  ##这里加status list and event list

  name <- as.data.frame(code)
  event_tmp<- name %>% select(`Event code`, `Event name`) ##注意名字
  names(event_tmp) <- c("code", "name")
  event_tmp <- event_tmp %>% filter(!is.na(name))
  event_list <- as.list(event_tmp)

  status_tmp<- name %>% select(`Status code`, `Status name`)
  names(status_tmp) <- c("code", "name")
  status_tmp <- status_tmp %>% filter(!is.na(name))
  status_list <- as.list(status_tmp)


  #找出事件数和status数
  eve_num <- length(event_list[[1]])
  status_num <- length(status_list[[1]])
  ###

  #添加region list
  if (nRegion>1) {
    region_tmp<- name %>% select(`Region Code`, `Region Name`)
    names(region_tmp) <- c("code", "name")
    region_tmp <- region_tmp %>% filter(!is.na(name))
    region_list <- as.list(region_tmp)
  }
  
  ##添加race list
  if (nRace>1){
    race_tmp<- name %>% select(`Race Code`, `Race Name`)
    names(race_tmp) <- c("code", "name")
    race_tmp <- race_tmp %>% filter(!is.na(name))
    race_list <- as.list(race_tmp)
  }
  
  ##添加marital的list
  if (Marital==1){
    marital_tmp<- name %>% select(`Marital Code`, `Marital Name`)
    names(marital_tmp) <- c("code", "name")
    marital_tmp <- marital_tmp %>% filter(!is.na(name))
    marital_list <- as.list(marital_tmp)
  }


  #Set output table and graph name surfix
  if(nRegion == 1 & nRace == 1 & nrj == 1 & edu == 0 & Marital == 0 & Children == 0 & Parent == 0){
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
    data_oe <- data_freq <- data.prepare.def(data, event_list, t1Month, t2Month, nl, nh)
  } else if (nrate==2){          #only oe
    cal_oe <- T
    data_oe <- data.prepare.def(data, event_list, t1Month, t2Month, nl, nh)
  } else if (nrate==3){          #only freq
    cal_freq <- T
    data_freq <- data.prepare.def(data, event_list, t1Month, t2Month, nl, nh)
  }
  
  
  ## Common settings for all cases ##
  
  ##estimate by what variable? define "byvar" according to parameters
  #Remark: for co-variant method, can choose whether consider sex or not
  #sex.nosex -- general estimation, not by any factor
  if (nRegion == 1 & nRace == 1 & nrj == 1 & edu == 0 & Marital == 0 & Children == 0 & Parent == 0 & sex==F) {byvar<- "sex.nosex"}
  #sex, only by gender
  if (nRegion == 1 & nRace == 1 & nrj == 1 & edu == 0 & Marital == 0 & Children == 0 & Parent == 0 & sex==T) {byvar<- "sex"}
  #region.nosex -- only by region
  if (nRegion > 1 & nRegion <= 100 & sex==F & nrj == 1 & nRace == 1 & edu == 0 & Marital == 0 & Children == 0 & Parent == 0) {byvar<- "region.nosex"}
  #ru.nosex -- only by rural/urban
  if (nRegion == 1 & nrj == 2 & sex==F & nRace == 1 & edu == 0 & Marital == 0 & Children == 0 & Parent == 0) {byvar<- "ru.nosex"}
  #race.nosex -- only by rural/urban
  if (nRegion == 1 & nrj == 1 & sex==F & nRace > 1 & edu == 0 & Marital == 0 & Children == 0 & Parent == 0) {byvar<- "race.nosex"}
  #region -- by region & sex
  if (nRegion > 1 & nRegion <= 100 & sex==T & nrj == 1 & nRace == 1 & edu == 0 & Marital == 0 & Children == 0 & Parent == 0) {byvar<- "region"}
  #ru -- by rural/urban & sex
  if (nRegion == 1 & nrj == 2 & sex==T & nRace == 1 & edu == 0 & Marital == 0 & Children == 0 & Parent == 0) {byvar<- "ru"}
  #race -- by race & sex
  if (nRegion == 1 & nrj == 1 & nRace > 1 & sex == T & edu == 0 & Marital == 0 & Children == 0 & Parent == 0) {byvar<- "race"}
  #edu -- by education & sex
  if (nRegion == 1 & nrj == 1 & nRace == 1 & sex == T & edu == 1 & Marital == 0 & Children == 0 & Parent == 0) {byvar<- "edu"}
  #with_c -- by with children & sex
  if (nRegion == 1 & nrj == 1 & nRace == 1 & sex == T & edu == 0 & Marital == 0 & Children == 1 & Parent == 0) {byvar<- "child"}
  #with_p -- by with parents & sex
  if (nRegion == 1 & nrj == 1 & nRace == 1 & sex == T & edu == 0 & Marital == 0 & Children == 0 & Parent == 1) {byvar<- "parent"}
  #marital -- by & sex
  if (nRegion == 1 & nrj == 1 & nRace == 1 & sex == T & edu == 0 & Marital == 1 & Children == 0 & Parent == 0) {byvar<- "marital"}
  #reg.ru.nosex -- by region & rural/urban
  if (nrj==2 & nRegion > 1 & nRegion <= 100 & sex==F & nRace == 1 & edu == 0 & Marital == 0 & Children == 0 & Parent == 0) {byvar<- "reg.ru.nosex"}
  #reg.ru -- by region, rural/urban & sex
  if (nrj==2 & nRegion > 1 & nRegion <= 100 & sex==T & nRace == 1 & edu == 0 & Marital == 0 & Children == 0 & Parent == 0) {byvar<- "reg.ru"}
  
  
  #set row names for final total rates output: same for all cases
  rowname <- c("Direct calculate", "Poisson estimate", "Difference%")
  
  #### Estimation by cases ####
  
  
  #estimated oe, raw oe, estimated freq, raw freq:
  #The result is for all cases
  est.covar <- compute.def.covar(data_oe, data_freq, param, code, plot, combined.name, byvar, mfp)
  
  
  ## Set rates as NA first, change later if cal_oe/cal_freq==T
  if (byvar=="reg.ru"|byvar=="reg.ru.nosex"){
    # for reg.ru & reg.ru.nosex only
    oe.rates.ru <- raw.oe.rates.ru <- frequency.ru <- raw.frequency.ru <- NA
    oe.rates.ub <- raw.oe.rates.ub <- frequency.ub <- raw.frequency.ub <- NA
  } else {
    # for all cases other than reg.ru & reg.ru.nosex
    oe.rates <- raw.oe.rates <- frequency <- raw.frequency <- NA
  }
  
  ## generate empty data frames to store total rates and mean ages
  if (byvar=="reg.ru"|byvar=="reg.ru.nosex"){
    # for reg.ru & reg.ru.nosex only
    total.rates.ru <- data.frame(t(rep(NA, eve_num*6+1)))[F,]
    total.rates.ub <- data.frame(t(rep(NA, eve_num*6+1)))[F,]
    mean.age.ru <- data.frame(t(rep(NA, eve_num*6+1)))[F,]
    mean.age.ub <- data.frame(t(rep(NA, eve_num*6+1)))[F,]
  } else {
    # for all cases other than reg.ru & reg.ru.nosex
    total.rates <- data.frame(t(rep(NA, eve_num*2)))[F,]
    mean.age <- data.frame(t(rep(NA, eve_num*2)))[F,]
  }
  
  
  #Case sex.nosex & sex
  if (byvar=="sex.nosex"|byvar=="sex") {
    
    if (cal_oe==T){
      
      #generate pop table: # of risk population and # of events
      pop <- pop.count.def(data_oe, nl, nh, event_list, status_list, len_ec, sex)
      write.pop.def(pop, param, name = ", all", eve_num, status_num, sex)
      
      #extract estimated oe and raw oe
      oe.rates <- est.covar$oe.rates %>% arrange(age)
      raw.oe.rates <- est.covar$raw.oe.rates %>% arrange(age)
      
    }
    
    if (cal_freq==T){
      
      #if have not generated pop table in the previous step (oe), generate it.
      #if have already generated, omit.
      if(cal_oe==F){
        pop <- pop.count.def(data_freq, nl, nh, event_list, status_list, len_ec, sex)
        write.pop.def(pop, param, name =  ", all", eve_num, status_num, sex)
      }
      
      #extract estimated freq and raw freq
      frequency <- est.covar$frequency %>% arrange(age)
      raw.frequency <- est.covar$raw.frequency %>% arrange(age)
      
      #Calculate estimated total rates and mean age based on estimated freq.
      poi.freq <- freq.def(frequency)
      frequency <- poi.freq$frequency
      poi.total.rates <- poi.freq$total.rates
      poi.mean.age <- poi.freq$mean.age
      
      #Calculate raw total rates and mean age based on raw freq.
      raw.freq <- freq.def(raw.frequency)
      raw.frequency <- raw.freq$frequency
      raw.total.rates <- raw.freq$total.rates
      raw.mean.age <- raw.freq$mean.age
      
      #combine estimated and raw total rates and frequencies, and calculate difference
      total.rates <- rbind(raw.total.rates, poi.total.rates, (poi.total.rates-raw.total.rates)/raw.total.rates)
      mean.age <- rbind(raw.mean.age, poi.mean.age, (poi.mean.age-raw.mean.age)/raw.mean.age)
      
      #replace Inf or NaN rates into NA.
      total.rates <- do.call(data.frame, lapply(total.rates, function(x) replace(x, is.infinite(x), NA)))
      
      write.total.def(total.rates, mean.age, event_list, param, combined.name, sex)
      
    }
    
    #output estimated rates
    write.rates.def(oe.rates, frequency, event_list, param, combined.name, "Poisson estimate", sex)
    #output raw rates
    write.rates.def(raw.oe.rates, raw.frequency, event_list, param, combined.name, "Direct calculate", sex)
    
  }
  
  #Case one covariant other than sex
  if (byvar %in% c("region.nosex", "ru.nosex", "race.nosex", "region", "ru", "race", "edu", "child", "parent", "marital")){
    
    #define code list & varable name
    if (byvar=="region.nosex"|byvar=="region"){
      i.list <- region_list
      var <- "region"
    } else if (byvar=="ru.nosex"|byvar=="ru") {
      i.list <- list(code=c(1,2), name=c("rural", "urban"))
      var <- "ru"
    } else if (byvar=="edu"){
      var <- "edu"
      i.list <- list(code=c(1:5),
                     name=c("No education", "Primary school",
                            "Middle school", "High school", "College or higher"))
    } else if (byvar=="race"|byvar=="race.nosex"){
      i.list <- race_list
      var <- "race"
    } else if (byvar=="child"){
      var <- "with_c"
      i.list <- list(code=c(0:1), 
                     name=c("without children", "with children"))
    } else if (byvar=="parent"){
      var <- "with_p"
      i.list <- list(code=c(0:2), 
                     name=c("Without parents", "With single parent", "With parents"))
    } else if (byvar=="marital"){
      var <- "mar"
      i.list <- marital_list
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
        pop <- pop.count.def(data_oe[which(eval(parse(text=paste("data_oe$",var,sep="")))==i),], nl, nh, event_list, status_list, len_ec, sex)
        write.pop.def(pop, param, i.code, eve_num, status_num, sex)
        
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
          pop <- pop.count.def(data_freq[which(eval(parse(text=paste("data_freq$",var,sep="")))==i),], nl, nh, event_list, status_list, len_ec, sex)
          write.pop.def(pop, param, i.code, eve_num, status_num, sex)
          
        }
        
        #extract estimated & direct calculated freq rates from estimation result
        frequency <- est.covar$frequency
        raw.frequency <- est.covar$raw.frequency
        #keep freq rates by variable
        frequency <- frequency[which(eval(parse(text = paste("frequency$",var,sep=""))) == i), -which(names(frequency)==var)] %>% arrange(age)
        raw.frequency <- raw.frequency[which(eval(parse(text = paste("raw.frequency$",var,sep=""))) == i), -which(names(raw.frequency)==var)] %>% arrange(age)
        
        #Calculate estimated total rates and mean age based on estimated freq.
        poi.freq <- freq.def(frequency)
        frequency <- poi.freq$frequency
        poi.total.rates <- poi.freq$total.rates
        poi.mean.age <- poi.freq$mean.age
        
        #Calculate raw total rates and mean age based on raw freq.
        raw.freq <- freq.def(raw.frequency)
        raw.frequency <- raw.freq$frequency
        raw.total.rates <- raw.freq$total.rates
        raw.mean.age <- raw.freq$mean.age
        
        i.total.rates <- rbind(raw.total.rates, poi.total.rates, (poi.total.rates-raw.total.rates)/raw.total.rates)
        i.mean.age <- rbind(raw.mean.age, poi.mean.age, (poi.mean.age-raw.mean.age)/raw.mean.age)
        
        i.total.rates <- do.call(data.frame, lapply(i.total.rates, function(x) replace(x, is.infinite(x), NA)))
        
        #adjust format for ease of output
        i.total.rates <- cbind(i.rowname, rowname, i.total.rates)
        total.rates <- rbind(total.rates, i.total.rates)
        i.mean.age <- cbind(i.rowname, rowname, i.mean.age)
        mean.age <- rbind(mean.age, i.mean.age)
        
      }
      
      #output rates
      write.rates.def(oe.rates, frequency, event_list, param, i.code, "Poisson estimate", sex)
      write.rates.def(raw.oe.rates, raw.frequency, event_list, param, i.code, "Direct calculate", sex)
      
    }
    
    #output total rates
    write.total.def(total.rates, mean.age, event_list, param, paste0("-by ", var, sep=""), sex)
    
  }
  
  #Case reg.ru & reg.ru.nosex
  if(byvar=="reg.ru"|byvar=="reg.ru.nosex"){
    
    #for each region
    for(k in 1:length(region_list[[1]])){
      #code
      i <- region_list[[1]][k]
      #name
      i.name <- region_list[[2]][k]
      #surfix
      i.code <- paste0(", ", i.name, sep="")
      
      if (cal_oe==T){
        
        #generate pop table: # of risk population and # of events
        pop <- pop.count.def(data_oe[which(data_oe$region==i&data_oe$ru==1),], nl, nh, event_list, status_list, len_ec, sex)
        write.pop.def(pop, param, paste0(i.code, "-rural"), eve_num, status_num, sex)
        pop <- pop.count.def(data_oe[which(data_oe$region==i&data_oe$ru==2),], nl, nh, event_list, status_list, len_ec, sex)
        write.pop.def(pop, param, paste0(i.code, "-urban"), eve_num, status_num, sex)
        
        #extract estimated & direct calculated oe rates from estimation result
        oe.rates.ru <- est.covar$oe.rates %>% filter(region == i&ru==1) %>% select(-region, -ru) %>% arrange(age)
        oe.rates.ub <- est.covar$oe.rates %>% filter(region == i&ru==2) %>% select(-region, -ru) %>% arrange(age)
        raw.oe.rates.ru <- est.covar$raw.oe.rates %>% filter(region == i&ru==1) %>% select(-region, -ru) %>% arrange(age)
        raw.oe.rates.ub <- est.covar$raw.oe.rates %>% filter(region == i&ru==2) %>% select(-region, -ru) %>% arrange(age)
        
      }
      
      if (cal_freq==T){
        
        if (cal_oe==F){
          
          #if have not generated pop table in the previous step (oe), generate it.
          #if have already generated, omit.
          pop <- pop.count.def(data_freq[which(data_freq$region==i&data_freq$ru==1),], nl, nh, event_list, status_list, len_ec, sex)
          write.pop.def(pop, param, paste0(i.code, "-rural"), eve_num, status_num, sex)
          pop <- pop.count.def(data_freq[which(data_freq$region==i&data_freq$ru==2),], nl, nh, event_list, status_list, len_ec, sex)
          write.pop.def(pop, param, paste0(i.code, "-urban"), eve_num, status_num, sex)
        }
        
        #extract estimated & direct calculated freq rates from estimation result
        frequency.ru <- est.covar$frequency %>% filter(region == i&ru==1) %>% select(-region, -ru) %>% arrange(age)
        frequency.ub <- est.covar$frequency %>% filter(region == i&ru==2) %>% select(-region, -ru) %>% arrange(age)
        raw.frequency.ru <- est.covar$raw.frequency %>% filter(region == i&ru==1) %>% select(-region, -ru) %>% arrange(age)
        raw.frequency.ub <- est.covar$raw.frequency %>% filter(region == i&ru==2) %>% select(-region, -ru) %>% arrange(age)
        
        #Calculate estimated rural total rates and mean age based on estimated freq.
        poi.freq.ru <- freq.def(frequency.ru)
        frequency.ru <- poi.freq.ru$frequency
        poi.total.rates.ru <- poi.freq.ru$total.rates
        poi.mean.age.ru <- poi.freq.ru$mean.age
        
        #Calculate raw rural total rates and mean age based on raw freq.
        raw.freq.ru <- freq.def(raw.frequency.ru)
        raw.frequency.ru <- raw.freq.ru$frequency
        raw.total.rates.ru <- raw.freq.ru$total.rates
        raw.mean.age.ru <- raw.freq.ru$mean.age
        
        #Calculate estimated urban total rates and mean age based on estimated freq.
        poi.freq.ub <- freq.def(frequency.ub)
        frequency.ub <- poi.freq.ub$frequency
        poi.total.rates.ub <- poi.freq.ub$total.rates
        poi.mean.age.ub <- poi.freq.ub$mean.age
        
        #Calculate raw urban total rates and mean age based on raw freq.
        raw.freq.ub <- freq.def(raw.frequency.ub)
        raw.frequency.ub <- raw.freq.ub$frequency
        raw.total.rates.ub <- raw.freq.ub$total.rates
        raw.mean.age.ub <- raw.freq.ub$mean.age
        
        #format total rates and mean ages
        if (sex==T){
          
          for (k in seq(1,eve_num*2,2)) {
            i.total.rates.ru <- data.frame(i.name, raw.total.rates.ru[k], poi.total.rates.ru[k], (poi.total.rates.ru[k]-raw.total.rates.ru[k])/raw.total.rates.ru[k],
                                           raw.total.rates.ru[k+1], poi.total.rates.ru[k+1], (poi.total.rates.ru[k+1]-raw.total.rates.ru[k+1])/raw.total.rates.ru[k+1])
            
            i.mean.age.ru <- data.frame(i.name, raw.mean.age.ru[k], poi.mean.age.ru[k], (poi.mean.age.ru[k]-raw.mean.age.ru[k])/raw.mean.age.ru[k],
                                        raw.mean.age.ru[k+1], poi.mean.age.ru[k+1], (poi.mean.age.ru[k+1]-raw.mean.age.ru[k+1])/raw.mean.age.ru[k+1])
            
            i.total.rates.ub <- data.frame(i.name, raw.total.rates.ub[k], poi.total.rates.ub[k], (poi.total.rates.ub[k]-raw.total.rates.ub[k])/raw.total.rates.ub[k],
                                           raw.total.rates.ub[k+1], poi.total.rates.ub[k+1], (poi.total.rates.ub[k+1]-raw.total.rates.ub[k+1])/raw.total.rates.ub[k+1])
            
            i.mean.age.ub <- data.frame(i.name, raw.mean.age.ub[k], poi.mean.age.ub[k], (poi.mean.age.ub[k]-raw.mean.age.ub[k])/raw.mean.age.ub[k],
                                        raw.mean.age.ub[k+1], poi.mean.age.ub[k+1], (poi.mean.age.ub[k+1]-raw.mean.age.ub[k+1])/raw.mean.age.ub[k+1])
            
          }
          
        } else {
          
          for (k in 1:eve_num) {
            i.total.rates.ru <- data.frame(i.name, raw.total.rates.ru[k], poi.total.rates.ru[k], (poi.total.rates.ru[k]-raw.total.rates.ru[k])/raw.total.rates.ru[k])
            
            i.mean.age.ru <- data.frame(i.name, raw.mean.age.ru[k], poi.mean.age.ru[k], (poi.mean.age.ru[k]-raw.mean.age.ru[k])/raw.mean.age.ru[k])
            
            i.total.rates.ub <- data.frame(i.name, raw.total.rates.ub[k], poi.total.rates.ub[k], (poi.total.rates.ub[k]-raw.total.rates.ub[k])/raw.total.rates.ub[k])
            
            i.mean.age.ub <- data.frame(i.name, raw.mean.age.ub[k], poi.mean.age.ub[k], (poi.mean.age.ub[k]-raw.mean.age.ub[k])/raw.mean.age.ub[k])
            
          }
          
        }
        
        i.total.rates.ru <- do.call(data.frame, lapply(i.total.rates.ru, function(x) replace(x, is.infinite(x), NA)))
        i.total.rates.ub <- do.call(data.frame, lapply(i.total.rates.ub, function(x) replace(x, is.infinite(x), NA)))
        
        total.rates.ru <- rbind(total.rates.ru, i.total.rates.ru)
        total.rates.ub <- rbind(total.rates.ub, i.total.rates.ub)
        mean.age.ru <- rbind(mean.age.ru, i.mean.age.ru)
        mean.age.ub <- rbind(mean.age.ub, i.mean.age.ub)
        
      }
      
      #output rates
      write.rates.def(oe.rates.ru, frequency.ru, event_list, param, paste0(i.code, "-rural"), "Poisson estimate", sex)
      write.rates.def(oe.rates.ub, frequency.ub, event_list, param, paste0(i.code, "-urban"), "Poisson estimate", sex)
      write.rates.def(raw.oe.rates.ru, raw.frequency.ru, event_list, param, paste0(i.code, "-rural"), "Direct calculate", sex)
      write.rates.def(raw.oe.rates.ub, raw.frequency.ub, event_list, param, paste0(i.code, "-urban"), "Direct calculate", sex)
      
    }
    
    #output total rates
    write.total.def.covar(total.rates.ru, total.rates.ub, mean.age.ru, mean.age.ub, event_list, param, "-region by residence", sex)
    
  }
  
}

compute.def.covar <- function(data_oe, data_freq, param, code, plot, plot.name, byvar, mfp){
  
  #parameters
  nrate <- as.numeric(param$nRate)
  nl <- as.numeric(param$nl)
  nh <- as.numeric(param$nh)
  title <- as.character(param$title)
  t1Month <- as.numeric(param$t1Month)
  t2Month <- as.numeric(param$t2Month)
  nRegion <- as.numeric(param$nRegion)
  nRace <- as.numeric(param$nRace)
  nWeight <- as.numeric(param$nWeight)
  Marital <- as.numeric(param$Marital)
  period <- paste0((t1Month-1)%/%12+1900, " - ",(t2Month-1)%/%12+1900, sep="")

  #options indicating whether calculate oe or freq
  cal_oe <- (nrate==1 | nrate==2)
  cal_fre <- (nrate==1 | nrate==3)

  ## 找出编码长度
  name <- as.data.frame(code)
  status_code <- na.omit(unique(name$`Status code`))
  len_ec <- nchar(status_code[1])

  event_tmp<- name %>% select(`Event code`, `Event name`) ##注意名字
  names(event_tmp) <- c("code", "name")
  event_tmp <- event_tmp %>% filter(!is.na(name))
  event_list <- as.list(event_tmp)

  status_tmp<- name %>% select(`Status code`, `Status name`)
  names(status_tmp) <- c("code", "name")
  status_tmp <- status_tmp %>% filter(!is.na(name))
  status_list <- as.list(status_tmp)


  #找出事件数和status数
  eve_num <- length(event_list[[1]])
  status_num <- length(status_list[[1]])
  ###

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
  
  #marital status list
  if (Marital==1){
    marital_tmp<- name %>% select(`Marital Code`, `Marital Name`)
    names(marital_tmp) <- c("code", "name")
    marital_tmp <- marital_tmp %>% filter(!is.na(name))
    marital_list <- as.list(marital_tmp)
  }
  
  
  #set rates as NA first, change later if cal_oe/cal_freq is TRUE.
  def.all.oe <- def.all.freq <- NA
  oe.rates <- raw.oe.rates <- frequency <- raw.frequency <- NA
  
  
  if(cal_oe == TRUE){
    # oe
    def.all.oe  <- list()
    agelist <- null.rates(data_oe, nl, nh, byvar)

    for(i in 1:length(event_list[[1]])){
      cur_event <- event_list[[1]][i]#拿event的code
      cur_name <- event_list[[2]][i]
      cur_prev <- floor(cur_event/(10**len_ec))

      oe <- merge(agelist, oe.est.byvar(data_oe, nl, nh, cur_prev, cur_event, byvar, nWeight, mfp), all.x=TRUE)
      oe <- arrange.covar(oe, byvar)
      def.all.oe <- c(def.all.oe, list(oe))
    }

  }

  if(cal_fre == TRUE){
    #freq
    def.all.freq  <- list()
    agelist <- null.rates(data_freq, nl, nh, byvar)

    for(i in 1:length(event_list[[1]])){
      cur_event <- event_list[[1]][i]#拿event的code
      cur_name <- event_list[[2]][i]
      cur_prev <- floor(cur_event/(10**len_ec))

      freq <- merge(agelist, freq.est.byvar(data_freq, nl, nh, cur_event, byvar, nWeight, mfp), all.x=TRUE)
      freq <- arrange.covar(freq, byvar)
      def.all.freq <- c(def.all.freq, list(freq))
    }

  }

  if(plot == TRUE){
    draw.udf.nRate(def.all.oe, def.all.freq, nl, nh, title, period, plot.name, byvar,
                    cal_oe, cal_fre, event_list, region_list, race_list, marital_list)
  }


  if (byvar!="reg.ru.nosex"&byvar!="region.nosex"&byvar!="sex.nosex"&byvar!="ru.nosex"&byvar!="race.nosex") {
    
    agelist <- arrange.covar(agelist, byvar)
    agelist <- agelist %>% select(-sex) %>% distinct()
    rates.by <- agelist

    if (byvar=="sex"){
      merge.by = c("age")
    } else if (byvar=="reg.ru"){
      merge.by = c("age", "region", "ru")
    } else if (byvar=="ru"){
      merge.by = c("age", "ru")
    } else if (byvar=="region"){
      merge.by = c("age", "region")
    } else if (byvar=="race"){
      merge.by = c("age", "race")
    } else if (byvar=="edu"){
      merge.by = c("age", "edu")
    } else if (byvar=="with_p"){
      merge.by = c("age", "with_p")
    } else if (byvar=="with_c"){
      merge.by = c("age", "with_c")
    } else if (byvar=="marital"){
      merge.by = c("age", "mar")
    } else if (byvar=="mar.ru"){
      merge.by = c("age", "mar", "ru")
    }

    if(cal_oe == TRUE){
      oe.rates1 <- rates.by
      oe.rates2 <- rates.by
      raw.oe.rates1 <- rates.by
      raw.oe.rates2 <- rates.by

      for(i in 1:length(event_list[[1]])) {
        cur_event <- event_list[[1]][i]
        cur_name <- event_list[[2]][i]

        ## oe male
        def.oe.male <- def.all.oe[[i]] %>% filter(sex==1)
        def.raw.oe.male <- def.oe.male[length(merge.by)+2]
        def.poi.oe.male <- def.oe.male[length(merge.by)+3]
        ## oe female
        def.oe.female <- def.all.oe[[i]] %>% filter(sex==2)
        def.raw.oe.female <- def.oe.female[length(merge.by)+2]
        def.poi.oe.female <- def.oe.female[length(merge.by)+3]

        #gender
        #male
        oe.rates1[paste0("def.male.", cur_event)] <- def.poi.oe.male
        raw.oe.rates1[paste0("def.male.", cur_event)] <- def.raw.oe.male

        #female
        oe.rates2[paste0("def.female.", cur_event)] <- def.poi.oe.female
        raw.oe.rates2[paste0("def.female.", cur_event)] <- def.raw.oe.female
      }

      oe.rates <- merge(oe.rates1, oe.rates2, by=merge.by)
      raw.oe.rates <- merge(raw.oe.rates1, raw.oe.rates2, by=merge.by)

      oe.rates <- merge(oe.rates1, oe.rates2, by='age')
      raw.oe.rates <- merge(raw.oe.rates1, raw.oe.rates2, by='age')

    }

    if(cal_fre == TRUE){
      frequency1 <- rates.by
      frequency2 <- rates.by
      raw.frequency1 <- rates.by
      raw.frequency2 <- rates.by

      for(i in 1:length(event_list[[1]])){
        cur_event <- event_list[[1]][i]
        cur_name <- event_list[[2]][i]

        ## freq male
        def.freq.male <- def.all.freq[[i]] %>% filter(sex==1)
        def.raw.freq.male <- def.freq.male[length(merge.by)+2]
        def.poi.freq.male <- def.freq.male[length(merge.by)+3]

        ## freq female
        def.freq.female <- def.all.freq[[i]] %>% filter(sex==2)
        def.raw.freq.female <- def.freq.female[length(merge.by)+2]
        def.poi.freq.female <- def.freq.female[length(merge.by)+3]


        frequency1[paste0("def.male.", cur_event)] <- def.poi.freq.male
        raw.frequency1[paste0("def.male.", cur_event)] <- def.raw.freq.male


        frequency2[paste0("def.female.", cur_event)] <- def.poi.freq.female
        raw.frequency2[paste0("def.female.", cur_event)] <- def.raw.freq.female
      }

      frequency <- merge(frequency1, frequency2, by=merge.by)
      raw.frequency <- merge(raw.frequency1, raw.frequency2, by=merge.by)

    }

  } else if (byvar=="reg.ru.nosex"){
    agelist <- agelist %>% arrange(age, region, ru)
    rates.by <- data.frame(age=agelist$age, region=agelist$region, ru=agelist$ru)

    if(cal_oe == TRUE){

      for(i in 1:length(event_list[[1]])) {
        cur_event <- event_list[[1]][i]
        cur_name <- event_list[[2]][i]

        def.raw.oe <- def.all.oe[[i]][4]
        def.poi.oe <- def.all.oe[[i]][5]

        oe.rates <- rates.by
        raw.oe.rates <- rates.by
        oe.rates[paste0("def.", cur_event)] <- def.poi.oe
        raw.oe.rates[paste0("def.", cur_event)] <- def.raw.oe
      }

    }

    if(cal_fre == TRUE){

      for(i in 1:length(event_list[[1]])){
        cur_event <- event_list[[1]][i]
        cur_name <- event_list[[2]][i]

        def.raw.freq <- def.all.freq[[i]][4]
        def.poi.freq <- def.all.freq[[i]][5]

        frequency <- rates.by
        raw.frequency <- rates.by

        frequency[paste0("def.", cur_event)] <- def.poi.freq
        raw.frequency[paste0("def.", cur_event)] <- def.raw.freq
      }

    }

  } else if (byvar=="region.nosex"){
    agelist <- agelist %>% arrange(age, region)
    rates.by <- data.frame(age=agelist$age, region=agelist$region)

    if(cal_oe == TRUE){

      for(i in 1:length(event_list[[1]])) {
        cur_event <- event_list[[1]][i]
        cur_name <- event_list[[2]][i]

        def.raw.oe <- def.all.oe[[i]][3]
        def.poi.oe <- def.all.oe[[i]][4]

        oe.rates <- rates.by
        raw.oe.rates <- rates.by
        oe.rates[paste0("def.", cur_event)] <- def.poi.oe
        raw.oe.rates[paste0("def.", cur_event)] <- def.raw.oe
      }

    }

    if(cal_fre == TRUE){

      for(i in 1:length(event_list[[1]])){
        cur_event <- event_list[[1]][i]
        cur_name <- event_list[[2]][i]

        def.raw.freq <- def.all.freq[[i]][3]
        def.poi.freq <- def.all.freq[[i]][4]

        frequency <- rates.by
        raw.frequency <- rates.by

        frequency[paste0("def.", cur_event)] <- def.poi.freq
        raw.frequency[paste0("def.", cur_event)] <- def.raw.freq
      }

    }
  }  else if (byvar=="race.nosex"){
    agelist <- agelist %>% arrange(age, race)
    rates.by <- data.frame(age=agelist$age, race=agelist$race)
    
    if(cal_oe == TRUE){
      
      for(i in 1:length(event_list[[1]])) {
        cur_event <- event_list[[1]][i]
        cur_name <- event_list[[2]][i]
        
        def.raw.oe <- def.all.oe[[i]][3]
        def.poi.oe <- def.all.oe[[i]][4]
        
        oe.rates <- rates.by
        raw.oe.rates <- rates.by
        oe.rates[paste0("def.", cur_event)] <- def.poi.oe
        raw.oe.rates[paste0("def.", cur_event)] <- def.raw.oe
      }
      
    }
    
    if(cal_fre == TRUE){
      
      for(i in 1:length(event_list[[1]])){
        cur_event <- event_list[[1]][i]
        cur_name <- event_list[[2]][i]
        
        def.raw.freq <- def.all.freq[[i]][3]
        def.poi.freq <- def.all.freq[[i]][4]
        
        frequency <- rates.by
        raw.frequency <- rates.by
        
        frequency[paste0("def.", cur_event)] <- def.poi.freq
        raw.frequency[paste0("def.", cur_event)] <- def.raw.freq
      }
      
    }
  }  else if (byvar=="ru.nosex"){
    agelist <- agelist %>% arrange(age, ru)
    rates.by <- data.frame(age=agelist$age, ru=agelist$ru)
    
    if(cal_oe == TRUE){
      
      for(i in 1:length(event_list[[1]])) {
        cur_event <- event_list[[1]][i]
        cur_name <- event_list[[2]][i]
        
        def.raw.oe <- def.all.oe[[i]][3]
        def.poi.oe <- def.all.oe[[i]][4]
        
        oe.rates <- rates.by
        raw.oe.rates <- rates.by
        oe.rates[paste0("def.", cur_event)] <- def.poi.oe
        raw.oe.rates[paste0("def.", cur_event)] <- def.raw.oe
      }
      
    }
    
    if(cal_fre == TRUE){
      
      for(i in 1:length(event_list[[1]])){
        cur_event <- event_list[[1]][i]
        cur_name <- event_list[[2]][i]
        
        def.raw.freq <- def.all.freq[[i]][3]
        def.poi.freq <- def.all.freq[[i]][4]
        
        frequency <- rates.by
        raw.frequency <- rates.by
        
        frequency[paste0("def.", cur_event)] <- def.poi.freq
        raw.frequency[paste0("def.", cur_event)] <- def.raw.freq
      }
      
    }
  }  else if (byvar=="sex.nosex"){
    agelist <- agelist %>% arrange(age)
    rates.by <- data.frame(age=agelist$age)
    
    if(cal_oe == TRUE){
      
      for(i in 1:length(event_list[[1]])) {
        cur_event <- event_list[[1]][i]
        cur_name <- event_list[[2]][i]
        
        def.raw.oe <- def.all.oe[[i]][2]
        def.poi.oe <- def.all.oe[[i]][3]
        
        oe.rates <- rates.by
        raw.oe.rates <- rates.by
        oe.rates[paste0("def.", cur_event)] <- def.poi.oe
        raw.oe.rates[paste0("def.", cur_event)] <- def.raw.oe
      }
      
    }
    
    if(cal_fre == TRUE){
      
      for(i in 1:length(event_list[[1]])){
        cur_event <- event_list[[1]][i]
        cur_name <- event_list[[2]][i]
        
        def.raw.freq <- def.all.freq[[i]][2]
        def.poi.freq <- def.all.freq[[i]][3]
        
        frequency <- rates.by
        raw.frequency <- rates.by
        
        frequency[paste0("def.", cur_event)] <- def.poi.freq
        raw.frequency[paste0("def.", cur_event)] <- def.raw.freq
      }
      
    }
  }

  return(list(oe.rates=oe.rates, frequency=frequency,
              raw.oe.rates = raw.oe.rates, raw.frequency = raw.frequency))
}

##------Prepare Data-------
def.t2 <- function(data, t2Month){

  def2 <- data %>%group_by(ID) %>%
    arrange(ID, m, event) %>%
    mutate(events = n()) %>%
    mutate(events = ifelse(events==1&is.na(event), 0, events)) %>%
    mutate(next.statusbf = lead(status_bf)) %>%
    mutate(lag.statusbf = lag(status_bf)) %>%
    mutate(next.statusaf = lead(status_af)) %>%
    mutate(lag.statusaf = lag(status_af)) %>%
    mutate(m = replace(m, which(m>t2Month), NA)) %>%           #event = NA说明不在研究范围内
    mutate(event = replace(event, which(m>t2Month), NA)) %>%
    mutate(status_bf = replace(status_bf, which(m>t2Month), NA)) %>%
    mutate(status_af = replace(status_af, which(m>t2Month), NA)) %>%
    filter(rowSums(is.na(cbind(event, m)))<2 | new_index==1)

  ##找到t2前最后一个发生的事件

  def2 <- def2 %>% group_by(ID) %>%
    arrange(ID, m) %>%
    mutate(n = n()) %>%
    mutate(row = row_number()) %>%
    mutate(event.t2 = case_when(events>=n&row==n&!is.na(event) ~ event)) %>%
    fill(event.t2, .direction = "up") %>% select(-n, -row)

  ##before、after的状态

  def2 <- def2 %>% group_by(ID) %>%
    arrange(ID, m) %>%
    mutate(n = n()) %>%
    mutate(row = row_number()) %>%
    mutate(statusbf.t2 = case_when(events==0 ~ k, ###k为访问时候的状态
                                   events==n&row==n&!is.na(status_bf) ~ status_bf,
                                   events>=n&row==n&is.na(status_bf)&!is.na(lag.statusaf) ~ lag.statusaf)) %>%
    fill(statusbf.t2, .direction = "up") %>% select(-n, -row)
  def2$statusbf.t2 <- ifelse(is.na(def2$statusbf.t2), def2$k, def2$statusbf.t2)


  def2 <- def2 %>% group_by(ID) %>%
    arrange(ID, m) %>%
    mutate(n = n()) %>%
    mutate(row = row_number()) %>%
    mutate(statusaf.t2 = case_when(events==0 ~ k,
                                   events==n&row==n&!is.na(status_af) ~ status_af,
                                   events>=n&row==n&is.na(status_af)&!is.na(next.statusbf) ~ next.statusbf)) %>%
    fill(statusaf.t2, .direction = "up") %>% select(-n, -row)
  def2$statusaf.t2 <- ifelse(is.na(def2$statusaf.t2), def2$k, def2$statusaf.t2)


  def2 <- def2 %>% select(ID, event.t2, statusbf.t2, statusaf.t2)
  data <- unique(def2)
  return(data)
}

data.prepare.def <- function(data, event_list, t1Month, t2Month, nl, nh){

  info <- data %>% select(region:events)
  evt <- data %>% select(ID, m1:ncol(data))
  long <- reshape(evt, idvar = "ID",
                  varying = names(evt)[2:ncol(evt)],
                  v.names = c("event", 'status_bf', 'status_af', "m"),
                  timevar = "event_index",
                  times = 1:((ncol(evt)-1)/4),
                  new.row.names = 1:10000000,
                  direction = "long") %>% arrange(ID, event_index)

  names(long)[3:6] <- names(long)[6:3] ##注意名字是不是对得上


  long <- long %>% relocate(event, .after = m)
  long <- long %>% relocate(status_bf, .after = event)


  def <- long %>% arrange(ID, m, event) %>%
    group_by(ID) %>%
    mutate(new_index = row_number()) %>%
    filter(rowSums(is.na(cbind(event, m)))<2 | new_index==1)

  def <- def %>% left_join(info, by = "ID")
  rm(long)
  rm(evt)

  def2 <- def.t2(def, t2Month)

  data <- merge(data, def2) %>% relocate(event.t2, statusbf.t2, statusaf.t2, .after = events)

  max <- max(data$events)


  data[, "t"] <-  round(t2Month - data$bMonth + 1, 0)
  data[, "t0"] <-  round(t1Month - data$bMonth, 0)

  data[,"fake.event"] <- 1
  data <- survSplit(Surv(data$t0, data$t, data$fake.event)~., data, cut=seq(min(data$t0),max(data$t),1),
                    start="t0", end="t")
  data[,"period"] <- data$bMonth + data$t0

  data$age <- floor(data$t0/12)

  data <- data[which(data$age >= nl & data$age <= nh),]

  data$py <- (data$t - data$t0)/12

  ###
  data$event <- NA
  data$status_bf <- NA
  data$status_af <- NA

  for(i in 1:max){
    event_m <- paste0("m", i, sep="")
    event_type <- paste0("event", i, sep="")
    event_statusbefore <- paste0("status_bf", i, sep="")
    event_statusafter <- paste0("status_af", i, sep="")


    temp <- cbind(data[, event_m], data$period)

    same.m <- data[which(temp[,1] == temp[,2]&!is.na(data$event)), ]
    same.m[, "event"] <- data[which(temp[,1] == temp[,2]&!is.na(data$event)), event_type]
    same.m[, "status_bf"] <- data[which(temp[,1] == temp[,2]&!is.na(data$event)), event_statusbefore]
    same.m[, "status_af"] <- data[which(temp[,1] == temp[,2]&!is.na(data$event)), event_statusafter]
    data[which(temp[,1] == temp[,2]), "event"] <- data[which(temp[,1] == temp[,2]), event_type]
    data[which(temp[,1] == temp[,2]), "status_bf"] <- data[which(temp[,1] == temp[,2]), event_statusbefore]
    data[which(temp[,1] == temp[,2]), "status_af"] <- data[which(temp[,1] == temp[,2]), event_statusafter]
    data <- rbind(data, same.m) %>% arrange(ID, t0)
  }

  ##这步有问题？？？
  # data[which(!(data$event %in% event_list)), "event"] <- NA
  data[which(is.na(data$event)), "event"] <- 0


  ### status
  ### 找prev 和post状态 如果这个人的最后一行是tMonth或者最大年龄，那么他的prev status就是在t2时候的status
  ##好像没有抓到状态？
  data$prev <- data$status_bf
  data$post <- data$status_af

  ##这里检查一下

  data[which(t2Month == (data$period)), "prev"] <- data[which(t2Month == (data$period)), "statusaf.t2"]
  data[which(data$t0==(nh*12+11)&is.na(data$prev)), "prev"] <- data[which(data$t0==(nh*12+11)&is.na(data$prev)), "statusaf.t2"]


  data <- data %>% group_by(ID) %>%
    mutate(neb.prev = ifelse(is.na(post), lag(post), post)) %>%
    mutate(neb.post = ifelse(is.na(prev), lead(prev), prev)) %>%
    mutate(post = ifelse(is.na(post), neb.post, post)) %>%
    mutate(prev = ifelse(is.na(prev), neb.prev, prev)) %>%
    select(-neb.prev, -neb.post)

  data <- as.data.frame(data %>% group_by(ID)
                        %>% fill(post, .direction="down")
                        %>% fill(prev, .direction="up"))
  data[which(is.na(data$post)), "post"] <- data[which(is.na(data$post)), "prev"]

  data <- data[, c("region", "weight", "race", "ru", "sex", "mar", "with_p", "with_c", "edu", "age", "event", "prev", "post", "py")]

  return(data)

}

data.prepare.def.py <- function(data, event_list, t1Month, t2Month, nl, nh){

  info <- data %>% select(region:events)
  evt <- data %>% select(ID, m1:ncol(data))
  long <- reshape(evt, idvar = "ID",
                  varying = names(evt)[2:ncol(evt)],
                  v.names = c("event", 'status_bf', 'status_af', "m"),
                  timevar = "event_index",
                  times = 1:((ncol(evt)-1)/4),
                  new.row.names = 1:10000000,
                  direction = "long") %>% arrange(ID, event_index)

  names(long)[3:6] <- names(long)[6:3] ##注意名字是不是对得上

  long <- long %>% relocate(event, .after = m)
  long <- long %>% relocate(status_bf, .after = event)


  def <- long %>% arrange(ID, m, event) %>%
    group_by(ID) %>%
    mutate(new_index = row_number()) %>%
    filter(rowSums(is.na(cbind(event, m)))<2 | new_index==1)

  def <- def %>% left_join(info, by = "ID")
  rm(evt)

  def2 <- def.t2(def, t2Month)
  def <- merge(def, def2) %>% relocate(event.t2, statusbf.t2, statusaf.t2, .after = events)

  #Generate an event-age dataframe
  evt <- def %>%
    mutate(event = replace(event, which(m<t1Month|m>t2Month|m<(nl*12+bMonth)|m>(nh*12+bMonth+12)), NA)) %>%
    mutate(m = replace(m, which(m<t1Month|m>t2Month|m<(nl*12+bMonth)|m>(nh*12+bMonth+12)), NA)) %>%
    filter(rowSums(is.na(cbind(event, m)))<2)
  evt$t0 <- (evt$m-evt$bMonth)/12
  evt$t <- ifelse((evt$t0==ceiling(evt$t0)&(ceiling(evt$t0)+1)>(t2Month+1-evt$bMonth)/12)|ceiling(evt$t0)>(t2Month+1-evt$bMonth)/12, (t2Month+1-evt$bMonth)/12,
                  ifelse(ceiling(evt$t0)==evt$t0, ceiling(evt$t0)+1, ceiling(evt$t0)))
  evt$age <- floor(evt$t0)

  #Generate a survival dataframe
  data <- def %>% select(region, ID, race, ru, sex, mar, with_c, with_p, edu, event.t2, statusbf.t2, statusaf.t2, tMonth, bMonth,
                        events, m, event, status_bf, status_af, new_index) %>%
    pivot_wider(names_from = c(new_index), values_from = c(m, event, status_bf, status_af), names_sep = "")

  surv <- data %>% select(region:events) #从第一列取到events
  surv$event_index <- NA
  surv$m <- NA
  surv$event <- NA
  surv$status_bf <- NA
  surv$status_af <- NA
  surv$new_index <- NA

  surv <- surv %>% relocate(event_index, m, event, status_bf, status_af, new_index, region, .after = "ID")
  surv[, "t"] <-  (t2Month - surv$bMonth + 1)/12
  surv[, "t0"] <-  (t1Month - surv$bMonth)/12
  surv[, "fake.event"] <- 1
  surv <- survSplit(Surv(surv$t0, surv$t, surv$fake.event)~., surv,
                    cut=seq(nl, nh+1, 1),
                    start="t0", end="t")
  surv[, "age"] <- floor(surv$t0)
  surv$event <- NA
  surv <- surv[-which(surv$age < nl | surv$age > nh+1),]

  #------------------------ to do
  ##Combine evt and surv together (double check)

  #把第一个事件拿出来
  # surv_tmp <- surv[,c('ID', 't0','t','age')]
  # evt_tmp <- evt[,c('ID','new_index', 't0','t','age')]
  #
  #起始to不为整数
  evt_1 <- evt %>% group_by(ID, t)%>%
    arrange(ID, t, t0) %>%
    mutate(new_index = row_number()) %>%
    filter(t0 != floor(t0) & new_index==1 & t0!=(t1Month-bMonth)/12)
  evt_1[, 2:7] <- NA
  evt_1=as.data.frame(evt_1)
  evt_1[,"t"] <- evt_1$t0
  evt_1[,"t0"] <- ifelse(floor(evt_1$t0)<(t1Month-evt_1$bMonth)/12, (t1Month-evt_1$bMonth)/12, floor(evt_1$t0))

  #创建区间
  evt_2 <- rbind(evt_1, evt) %>% group_by(ID) %>%
    arrange(ID, t0, t) %>%
    mutate(next.t0 = lead(t0)) %>%
    mutate(next.event = lead(event))
  evt_2[which(evt_2$t0==evt_2$next.t0&evt_2$event!=evt_2$next.event), "t"] <-
    evt_2[which(evt_2$t0==evt_2$next.t0&evt_2$event!=evt_2$next.event), "next.t0"]
  evt_2 <- evt_2 %>% select(-next.t0, -next.event)

  #合并数据
  combine <- rbind(evt_2, surv) %>% group_by(ID) %>%
    arrange(ID, t0, t) %>%
    relocate(t0, t, age, .after = "status_af") %>%
    mutate(next.evt = lead(event_index)) %>%
    mutate(last.t0 = lag(t0)) %>%
    mutate(next.t0 = lead(t0))
  #删除event前的多余行
  combine <- combine[-which(combine$next.t0!=floor(combine$next.t0)&
                              combine$t0==combine$last.t0&
                              !is.na(combine$next.evt)),]
  #删除m1等于t1month开始时间的多余行
  combine <- combine %>% filter(!(duplicated(cbind(t0, t))&is.na(event_index)))
  #如果后面有event，修改t
  combine[which(!is.na(combine$next.evt)), "t"] <- combine[which(!is.na(combine$next.evt)), "next.t0"]

  combine <- combine[which(combine$age >= nl & combine$age <= nh+1),] %>%
    select(ID:age)
  data <- combine %>% left_join(data, by="ID")
  data$py <- (data$t - data$t0)


  data[,"t2Month_y"] <- (t2Month-data$bMonth+1)/12

  data[which(is.na(data$event)), "event"] <- 0

  ### status
  ### 找prev 和post状态 如果这个人的最后一行是tMonth或者最大年龄，那么他的prev status就是在t2时候的status
  data$prev <- data$status_bf
  data$post <- data$status_af


  data[which(data$t2Month_y == (data$t)), "prev"] <- data[which(data$t2Month_y == (data$t)), "statusaf.t2"]
  data[which(data$t0==nh &is.na(data$prev)), "prev"] <- data[which(data$t0==nh & is.na(data$prev)), "statusaf.t2"]

  data <- data %>% group_by(ID) %>%
    mutate(neb.prev = ifelse(is.na(post), lag(post), post)) %>%
    mutate(neb.post = ifelse(is.na(prev), lead(prev), prev)) %>%
    mutate(post = ifelse(is.na(post), neb.post, post)) %>%
    mutate(prev = ifelse(is.na(prev), neb.prev, prev)) %>%
    select(-neb.prev, -neb.post)

  data <- as.data.frame(data %>% group_by(ID)
                        %>% fill(post, .direction="down")
                        %>% fill(prev, .direction="up"))

  data[which(is.na(data$post)), "post"] <- data[which(is.na(data$post)), "prev"]

  data <- data[, c("region", "weight", "race", "ru", "sex", "mar", "with_p", "with_c", "edu", "age", "event", "prev", "post", "py")]

  return(data)


}

##----Output functions---------
draw.udf.nRate <- function(def.all.oe, def.all.freq, nl, nh, title, period, plot.name, byvar,
                           cal_oe, cal_fre, event_list, region_list, race_list, marital_list){

  if (byvar=="sex"){
    wb <- createWorkbook()
    k <- 1

    if(cal_oe == TRUE){
      addWorksheet(wb, "oe")
      writeData(wb, sheet=k, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
      writeData(wb, sheet=k, paste0("Period: ", period, sep=""), startRow=2, startCol=1)

      for(i in 1:length(event_list[[1]])){
        cur_event <- event_list[[1]][i]
        cur_name <- event_list[[2]][i]

        row.index <- 3
        def.oe <- def.all.oe[[i]]
        p1 <- rates.plot(def.oe[which(def.oe$sex==1), c(1, 3:4)], nl, nh, "o/e rate", paste0("Figure 1.", i, cur_name,  " o/e rates, male", plot.name, sep=""))
        print(p1)
        insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")

        row.index <- row.index + 22
        p2 <- rates.plot(def.oe[which(def.oe$sex==2), c(1, 3:4)], nl, nh, "o/e rate", paste0("Figure 2.", i, cur_name,  " o/e rates, female", plot.name, sep=""))
        print(p2)
        insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")
      }

      k <- k+1
    }

    if(cal_fre == TRUE){
      addWorksheet(wb, "freq")
      writeData(wb, sheet=k, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
      writeData(wb, sheet=k, paste0("Period: ", period, sep=""), startRow=2, startCol=1)

      for(i in 1:length(event_list[[1]])){
        cur_event <- event_list[[1]][i]
        cur_name <- event_list[[2]][i]

        row.index <- 3
        def.freq <- def.all.freq[[i]]
        p1 <- rates.plot(def.freq[which(def.freq$sex==1), c(1, 3:4)], nl, nh, "frequency", paste0("Figure 3.", i, cur_name,  " frequencies, male", plot.name, sep=""))
        print(p1)
        insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")

        row.index <- row.index + 22
        p2 <- rates.plot(def.freq[which(def.freq$sex==2), c(1, 3:4)], nl, nh, "frequency", paste0("Figure 4.", i, cur_name,  " frequencies, female", plot.name, sep=""))
        print(p2)
        insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")
      }

    }

    output.dir <- getwd()
    plot.name <- gsub(", ", "-", plot.name)
    saveWorkbook(wb, paste0(output.dir, "/", title, " User defined event plot", plot.name, ".xlsx", sep=""), overwrite=TRUE)

  } else if (byvar=="sex.nosex"){
    
    wb <- createWorkbook()
    k <- 1
    
    if(cal_oe == TRUE){
      addWorksheet(wb, "oe")
      writeData(wb, sheet=k, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
      writeData(wb, sheet=k, paste0("Period: ", period, sep=""), startRow=2, startCol=1)
      
      for(i in 1:length(event_list[[1]])){
        cur_event <- event_list[[1]][i]
        cur_name <- event_list[[2]][i]
        
        row.index <- 3
        def.oe <- def.all.oe[[i]]
        p1 <- rates.plot(def.oe[, c(1, 2:3)], nl, nh, "o/e rate", paste0("Figure 1.", i, cur_name,  " o/e rates", plot.name, sep=""))
        print(p1)
        insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")
        
      }
      
      k <- k+1
    }
    
    if(cal_fre == TRUE){
      addWorksheet(wb, "freq")
      writeData(wb, sheet=k, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
      writeData(wb, sheet=k, paste0("Period: ", period, sep=""), startRow=2, startCol=1)
      
      for(i in 1:length(event_list[[1]])){
        cur_event <- event_list[[1]][i]
        cur_name <- event_list[[2]][i]
        
        row.index <- 3
        def.freq <- def.all.freq[[i]]
        p1 <- rates.plot(def.freq[, c(1, 2:3)], nl, nh, "frequency", paste0("Figure 3.", i, cur_name,  " frequencies", plot.name, sep=""))
        print(p1)
        insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")
        
      }
      
    }
    
    output.dir <- getwd()
    plot.name <- gsub(", ", "-", plot.name)
    saveWorkbook(wb, paste0(output.dir, "/", title, " User defined event plot", plot.name, ".xlsx", sep=""), overwrite=TRUE)
    
  } else if (byvar=="reg.ru"){

    for(j in 1:length(region_list[[1]])){
      r <- region_list[[1]][j]
      r.name <- region_list[[2]][j]
      r.code <- paste0(", ", r.name, sep="")
      plot.name <- r.code

      wb <- createWorkbook()
      k <- 1

      if(cal_oe == TRUE){
        addWorksheet(wb, "oe-rural")
        addWorksheet(wb, "oe-urban")
        writeData(wb, sheet=k, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
        writeData(wb, sheet=k, paste0("Period: ", period, sep=""), startRow=2, startCol=1)
        writeData(wb, sheet=k+1, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
        writeData(wb, sheet=k+1, paste0("Period: ", period, sep=""), startRow=2, startCol=1)

        for(i in 1:length(event_list[[1]])){
          cur_event <- event_list[[1]][i]
          cur_name <- event_list[[2]][i]

          row.index <- 3
          def.oe <- def.all.oe[[i]]
          p1 <- rates.plot(def.oe[which(def.oe$region==r&def.oe$sex==1&def.oe$ru==1), c(1, 5:6)], nl, nh, "o/e rate", paste0("Figure 1.", i, cur_name,  " o/e rates, male", plot.name, "-rural", sep=""))
          print(p1)
          insertPlot(wb, "oe-rural", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")

          p1 <- rates.plot(def.oe[which(def.oe$region==r&def.oe$sex==1&def.oe$ru==2), c(1, 5:6)], nl, nh, "o/e rate", paste0("Figure 1.", i, cur_name,  " o/e rates, male", plot.name, "-urban", sep=""))
          print(p1)
          insertPlot(wb, "oe-urban", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")

          row.index <- row.index + 22
          p2 <- rates.plot(def.oe[which(def.oe$region==r&def.oe$sex==2&def.oe$ru==1), c(1, 5:6)], nl, nh, "o/e rate", paste0("Figure 2.", i, cur_name,  " o/e rates, female", plot.name, "-rural", sep=""))
          print(p2)
          insertPlot(wb, "oe-rural", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")

          p2 <- rates.plot(def.oe[which(def.oe$region==r&def.oe$sex==2&def.oe$ru==2), c(1, 5:6)], nl, nh, "o/e rate", paste0("Figure 2.", i, cur_name,  " o/e rates, female", plot.name, "-urban", sep=""))
          print(p2)
          insertPlot(wb, "oe-urban", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")
        }

        k<-k+1

      }

      if(cal_fre == TRUE){
        addWorksheet(wb, "freq-rural")
        addWorksheet(wb, "freq-urban")
        writeData(wb, sheet=k, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
        writeData(wb, sheet=k, paste0("Period: ", period, sep=""), startRow=2, startCol=1)
        writeData(wb, sheet=k+1, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
        writeData(wb, sheet=k+1, paste0("Period: ", period, sep=""), startRow=2, startCol=1)

        for(i in 1:length(event_list[[1]])){
          cur_event <- event_list[[1]][i]
          cur_name <- event_list[[2]][i]

          row.index <- 3
          def.freq <- def.all.freq[[i]]
          p1 <- rates.plot(def.freq[which(def.freq$region==r&def.freq$sex==1&def.freq$ru==1), c(1, 5:6)], nl, nh, "frequency", paste0("Figure 3.", i, cur_name,  " frequencies, male", plot.name, "-rural", sep=""))
          print(p1)
          insertPlot(wb, "freq-rural", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")

          p1 <- rates.plot(def.freq[which(def.freq$region==r&def.freq$sex==1&def.freq$ru==2), c(1, 5:6)], nl, nh, "frequency", paste0("Figure 3.", i, cur_name,  " frequencies, male", plot.name, "-urban", sep=""))
          print(p1)
          insertPlot(wb, "freq-urban", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")

          row.index <- row.index + 22
          p2 <- rates.plot(def.freq[which(def.freq$region==r&def.freq$sex==2&def.freq$ru==1), c(1, 5:6)], nl, nh, "frequency", paste0("Figure 4.", i, cur_name,  " frequencies, female", plot.name, "-rural", sep=""))
          print(p2)
          insertPlot(wb, "freq-rural", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")

          p2 <- rates.plot(def.freq[which(def.freq$region==r&def.freq$sex==2&def.freq$ru==2), c(1, 5:6)], nl, nh, "frequency", paste0("Figure 4.", i, cur_name,  " frequencies, female", plot.name, "-urban", sep=""))
          print(p2)
          insertPlot(wb, "freq-urban", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")
        }

      }

      output.dir <- getwd()
      plot.name <- gsub(", ", "-", plot.name)
      saveWorkbook(wb, paste0(output.dir, "/", title, " User defined event plot", plot.name, "-by residence", ".xlsx", sep=""), overwrite=TRUE)
    }

  } else if (byvar=="mar.ru"){

    for(j in 1:length(marital_list[[1]])){
      m <- marital_list[[1]][j]
      m.name <- marital_list[[2]][j]
      m.code <- paste0(", ", m.name, sep="")
      plot.name <- m.code

      wb <- createWorkbook()
      k <- 1

      if(cal_oe == TRUE){
        addWorksheet(wb, "oe-rural")
        addWorksheet(wb, "oe-urban")
        writeData(wb, sheet=k, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
        writeData(wb, sheet=k, paste0("Period: ", period, sep=""), startRow=2, startCol=1)
        writeData(wb, sheet=k+1, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
        writeData(wb, sheet=k+1, paste0("Period: ", period, sep=""), startRow=2, startCol=1)

        for(i in 1:length(event_list[[1]])){
          cur_event <- event_list[[1]][i]
          cur_name <- event_list[[2]][i]

          row.index <- 3
          def.oe <- def.all.oe[[i]]
          p1 <- rates.plot(def.oe[which(def.oe$mar==m&def.oe$sex==1&def.oe$ru==1), c(1, 5:6)], nl, nh, "o/e rate", paste0("Figure 1.", i, cur_name,  " o/e rates, male", plot.name, "-rural", sep=""))
          print(p1)
          insertPlot(wb, "oe-rural", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")

          p1 <- rates.plot(def.oe[which(def.oe$mar==m&def.oe$sex==1&def.oe$ru==2), c(1, 5:6)], nl, nh, "o/e rate", paste0("Figure 1.", i, cur_name,  " o/e rates, male", plot.name, "-urban", sep=""))
          print(p1)
          insertPlot(wb, "oe-urban", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")

          row.index <- row.index + 22
          p2 <- rates.plot(def.oe[which(def.oe$mar==m&def.oe$sex==2&def.oe$ru==1), c(1, 5:6)], nl, nh, "o/e rate", paste0("Figure 2.", i, cur_name,  " o/e rates, female", plot.name, "-rural", sep=""))
          print(p2)
          insertPlot(wb, "oe-rural", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")

          p2 <- rates.plot(def.oe[which(def.oe$mar==m&def.oe$sex==2&def.oe$ru==2), c(1, 5:6)], nl, nh, "o/e rate", paste0("Figure 2.", i, cur_name,  " o/e rates, female", plot.name, "-urban", sep=""))
          print(p2)
          insertPlot(wb, "oe-urban", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")
        }

        k<-k+1

      }

      if(cal_fre == TRUE){
        addWorksheet(wb, "freq-rural")
        addWorksheet(wb, "freq-urban")
        writeData(wb, sheet=k, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
        writeData(wb, sheet=k, paste0("Period: ", period, sep=""), startRow=2, startCol=1)
        writeData(wb, sheet=k+1, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
        writeData(wb, sheet=k+1, paste0("Period: ", period, sep=""), startRow=2, startCol=1)

        for(i in 1:length(event_list[[1]])){
          cur_event <- event_list[[1]][i]
          cur_name <- event_list[[2]][i]

          row.index <- 3
          def.freq <- def.all.freq[[i]]
          p1 <- rates.plot(def.freq[which(def.freq$mar==m&def.freq$sex==1&def.freq$ru==1), c(1, 5:6)], nl, nh, "frequency", paste0("Figure 3.", i, cur_name,  " frequencies, male", plot.name, "-rural", sep=""))
          print(p1)
          insertPlot(wb, "freq-rural", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")

          p1 <- rates.plot(def.freq[which(def.freq$mar==m&def.freq$sex==1&def.freq$ru==2), c(1, 5:6)], nl, nh, "frequency", paste0("Figure 3.", i, cur_name,  " frequencies, male", plot.name, "-urban", sep=""))
          print(p1)
          insertPlot(wb, "freq-urban", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")

          row.index <- row.index + 22
          p2 <- rates.plot(def.freq[which(def.freq$mar==m&def.freq$sex==2&def.freq$ru==1), c(1, 5:6)], nl, nh, "frequency", paste0("Figure 4.", i, cur_name,  " frequencies, female", plot.name, "-rural", sep=""))
          print(p2)
          insertPlot(wb, "freq-rural", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")

          p2 <- rates.plot(def.freq[which(def.freq$mar==m&def.freq$sex==2&def.freq$ru==2), c(1, 5:6)], nl, nh, "frequency", paste0("Figure 4.", i, cur_name,  " frequencies, female", plot.name, "-urban", sep=""))
          print(p2)
          insertPlot(wb, "freq-urban", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")
        }

      }

      output.dir <- getwd()
      plot.name <- gsub(", ", "-", plot.name)
      saveWorkbook(wb, paste0(output.dir, "/", title, " User defined event plot", plot.name, "-by residence", ".xlsx", sep=""), overwrite=TRUE)
    }

  } else if (byvar=="reg.ru.nosex"){

    
    for(j in 1:length(region_list[[1]])){
      r <- region_list[[1]][j]
      r.name <- region_list[[2]][j]
      r.code <- paste0(", ", r.name, sep="")
      plot.name <- r.code
      
      wb <- createWorkbook()
      k <- 1
      
      if(cal_oe == TRUE){
        addWorksheet(wb, "oe-rural")
        addWorksheet(wb, "oe-urban")
        writeData(wb, sheet=k, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
        writeData(wb, sheet=k, paste0("Period: ", period, sep=""), startRow=2, startCol=1)
        writeData(wb, sheet=k+1, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
        writeData(wb, sheet=k+1, paste0("Period: ", period, sep=""), startRow=2, startCol=1)
        
        for(i in 1:length(event_list[[1]])){
          cur_event <- event_list[[1]][i]
          cur_name <- event_list[[2]][i]
          
          row.index <- 3
          def.oe <- def.all.oe[[i]]
          p1 <- rates.plot(def.oe[which(def.oe$region==r&def.oe$ru==1), c(1, 4:5)], nl, nh, "o/e rate", paste0("Figure 1.", i, cur_name,  " o/e rates", plot.name, "-rural", sep=""))
          print(p1)
          insertPlot(wb, "oe-rural", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")
          
          p1 <- rates.plot(def.oe[which(def.oe$region==r&def.oe$ru==2), c(1, 4:5)], nl, nh, "o/e rate", paste0("Figure 1.", i, cur_name,  " o/e rates", plot.name, "-urban", sep=""))
          print(p1)
          insertPlot(wb, "oe-urban", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")
          
        }
        
        k<-k+1
        
      }
      
      if(cal_fre == TRUE){
        addWorksheet(wb, "freq-rural")
        addWorksheet(wb, "freq-urban")
        writeData(wb, sheet=k, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
        writeData(wb, sheet=k, paste0("Period: ", period, sep=""), startRow=2, startCol=1)
        writeData(wb, sheet=k+1, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
        writeData(wb, sheet=k+1, paste0("Period: ", period, sep=""), startRow=2, startCol=1)
        
        for(i in 1:length(event_list[[1]])){
          cur_event <- event_list[[1]][i]
          cur_name <- event_list[[2]][i]
          
          row.index <- 3
          def.freq <- def.all.freq[[i]]
          p1 <- rates.plot(def.freq[which(def.freq$region==r&def.freq$ru==1), c(1, 4:5)], nl, nh, "frequency", paste0("Figure 3.", i, cur_name,  " frequencies", plot.name, "-rural", sep=""))
          print(p1)
          insertPlot(wb, "freq-rural", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")
          
          p1 <- rates.plot(def.freq[which(def.freq$region==r&def.freq$ru==2), c(1, 4:5)], nl, nh, "frequency", paste0("Figure 3.", i, cur_name,  " frequencies", plot.name, "-urban", sep=""))
          print(p1)
          insertPlot(wb, "freq-urban", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")
          
        }
        
      }
      
      output.dir <- getwd()
      plot.name <- gsub(", ", "-", plot.name)
      saveWorkbook(wb, paste0(output.dir, "/", title, " User defined event plot", plot.name, "-by residence", ".xlsx", sep=""), overwrite=TRUE)
    }
    
  } else {
    wb <- createWorkbook()
    k <- 1

    if(cal_oe == TRUE){
      addWorksheet(wb, "oe")
      writeData(wb, sheet=k, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
      writeData(wb, sheet=k, paste0("Period: ", period, sep=""), startRow=2, startCol=1)
      k <- k+1
    }

    if(cal_fre == TRUE){
      addWorksheet(wb, "freq")
      writeData(wb, sheet=k, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
      writeData(wb, sheet=k, paste0("Period: ", period, sep=""), startRow=2, startCol=1)
    }

    if (byvar=="ru"){

      ru.code <- c(1, 2)

      for(j in ru.code){
        r.name <- ifelse(j==1, "rural", "urban")
        r.code <- paste0(", ", r.name, sep="")
        plot.name <- r.code

        if(cal_oe == TRUE){

          for(i in 1:length(event_list[[1]])){
            cur_event <- event_list[[1]][i]
            cur_name <- event_list[[2]][i]

            row.index <- 3
            def.oe <- def.all.oe[[i]]
            p1 <- rates.plot(def.oe[which(def.oe$sex==1&def.oe$ru==j), c(1, 4:5)], nl, nh, "o/e rate", paste0("Figure 1.", i, cur_name,  " o/e rates, male", plot.name, sep=""))
            print(p1)
            insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")

            row.index <- row.index + 22
            p2 <- rates.plot(def.oe[which(def.oe$sex==2&def.oe$ru==j), c(1, 4:5)], nl, nh, "o/e rate", paste0("Figure 2.", i, cur_name,  " o/e rates, female", plot.name, sep=""))
            print(p2)
            insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")
          }
        }

        if(cal_fre == TRUE){

          for(i in 1:length(event_list[[1]])){
            cur_event <- event_list[[1]][i]
            cur_name <- event_list[[2]][i]

            row.index <- 3
            def.freq <- def.all.freq[[i]]
            p1 <- rates.plot(def.freq[which(def.freq$sex==1&def.freq$ru==j), c(1, 4:5)], nl, nh, "frequency", paste0("Figure 3.", i, cur_name,  " frequencies, male", plot.name, sep=""))
            print(p1)
            insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")

            row.index <- row.index + 22
            p2 <- rates.plot(def.freq[which(def.freq$sex==2&def.freq$ru==j), c(1, 4:5)], nl, nh, "frequency", paste0("Figure 4.", i, cur_name,  " frequencies, female", plot.name, sep=""))
            print(p2)
            insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")
          }

        }

        output.dir <- getwd()
        plot.name <- gsub(", ", "-", plot.name)
        saveWorkbook(wb, paste0(output.dir, "/", title, " User defined event plot", plot.name, ".xlsx", sep=""), overwrite=TRUE)

      }

    }

    if (byvar=="ru.nosex"){
      
      ru.code <- c(1, 2)
      
      for(j in ru.code){
        r.name <- ifelse(j==1, "rural", "urban")
        r.code <- paste0(", ", r.name, sep="")
        plot.name <- r.code
        
        if(cal_oe == TRUE){
          
          for(i in 1:length(event_list[[1]])){
            cur_event <- event_list[[1]][i]
            cur_name <- event_list[[2]][i]
            
            row.index <- 3
            def.oe <- def.all.oe[[i]]
            p1 <- rates.plot(def.oe[which(def.oe$ru==j), c(1, 3:4)], nl, nh, "o/e rate", paste0("Figure 1.", i, cur_name,  " o/e rates", plot.name, sep=""))
            print(p1)
            insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")
            
          }
        }
        
        if(cal_fre == TRUE){
          
          for(i in 1:length(event_list[[1]])){
            cur_event <- event_list[[1]][i]
            cur_name <- event_list[[2]][i]
            
            row.index <- 3
            def.freq <- def.all.freq[[i]]
            p1 <- rates.plot(def.freq[which(def.freq$ru==j), c(1, 3:4)], nl, nh, "frequency", paste0("Figure 3.", i, cur_name,  " frequencies", plot.name, sep=""))
            print(p1)
            insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")
            
          }
          
        }
        
        output.dir <- getwd()
        plot.name <- gsub(", ", "-", plot.name)
        saveWorkbook(wb, paste0(output.dir, "/", title, " User defined event plot", plot.name, ".xlsx", sep=""), overwrite=TRUE)
        
      }
      
    }
    
    if (byvar=="region"){

      for(j in 1:length(region_list[[1]])){
        r <- region_list[[1]][j]
        r.name <- region_list[[2]][j]
        r.code <- paste0(", ", r.name, sep="")
        plot.name <- r.code

        if(cal_oe == TRUE){

          for(i in 1:length(event_list[[1]])){
            cur_event <- event_list[[1]][i]
            cur_name <- event_list[[2]][i]

            row.index <- 3
            def.oe <- def.all.oe[[i]]
            p1 <- rates.plot(def.oe[which(def.oe$sex==1&def.oe$region==r), c(1, 4:5)], nl, nh, "o/e rate", paste0("Figure 1.", i, cur_name,  " o/e rates, male", plot.name, sep=""))
            print(p1)
            insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")

            row.index <- row.index + 22
            p2 <- rates.plot(def.oe[which(def.oe$sex==2&def.oe$region==r), c(1, 4:5)], nl, nh, "o/e rate", paste0("Figure 2.", i, cur_name,  " o/e rates, female", plot.name, sep=""))
            print(p2)
            insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")
          }
        }

        if(cal_fre == TRUE){

          for(i in 1:length(event_list[[1]])){
            cur_event <- event_list[[1]][i]
            cur_name <- event_list[[2]][i]

            row.index <- 3
            def.freq <- def.all.freq[[i]]
            p1 <- rates.plot(def.freq[which(def.freq$sex==1&def.freq$region==r), c(1, 4:5)], nl, nh, "frequency", paste0("Figure 3.", i, cur_name,  " frequencies, male", plot.name, sep=""))
            print(p1)
            insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")

            row.index <- row.index + 22
            p2 <- rates.plot(def.freq[which(def.freq$sex==2&def.freq$region==r), c(1, 4:5)], nl, nh, "frequency", paste0("Figure 4.", i, cur_name,  " frequencies, female", plot.name, sep=""))
            print(p2)
            insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")
          }

        }


        output.dir <- getwd()
        plot.name <- gsub(", ", "-", plot.name)
        saveWorkbook(wb, paste0(output.dir, "/", title, " User defined event plot", plot.name, ".xlsx", sep=""), overwrite=TRUE)

      }

    }

    if (byvar=="region.nosex"){
      
      for(j in 1:length(region_list[[1]])){
        r <- region_list[[1]][j]
        r.name <- region_list[[2]][j]
        r.code <- paste0(", ", r.name, sep="")
        plot.name <- r.code
        
        if(cal_oe == TRUE){
          
          for(i in 1:length(event_list[[1]])){
            cur_event <- event_list[[1]][i]
            cur_name <- event_list[[2]][i]
            
            row.index <- 3
            def.oe <- def.all.oe[[i]]
            p1 <- rates.plot(def.oe[which(def.oe$region==r), c(1, 3:4)], nl, nh, "o/e rate", paste0("Figure 1.", i, cur_name,  " o/e rates", plot.name, sep=""))
            print(p1)
            insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")
            
          }
        }
        
        if(cal_fre == TRUE){
          
          for(i in 1:length(event_list[[1]])){
            cur_event <- event_list[[1]][i]
            cur_name <- event_list[[2]][i]
            
            row.index <- 3
            def.freq <- def.all.freq[[i]]
            p1 <- rates.plot(def.freq[which(def.freq$region==r), c(1, 3:4)], nl, nh, "frequency", paste0("Figure 3.", i, cur_name,  " frequencies", plot.name, sep=""))
            print(p1)
            insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")
            
          }
          
        }
        
        
        output.dir <- getwd()
        plot.name <- gsub(", ", "-", plot.name)
        saveWorkbook(wb, paste0(output.dir, "/", title, " User defined event plot", plot.name, ".xlsx", sep=""), overwrite=TRUE)
        
      }
      
    }
    
    if (byvar=="race"){
      
      for(j in 1:length(race_list[[1]])){
        r <- race_list[[1]][j]
        r.name <- race_list[[2]][j]
        r.code <- paste0(", ", r.name, sep="")
        plot.name <- r.code
        
        if(cal_oe == TRUE){
          
          for(i in 1:length(event_list[[1]])){
            cur_event <- event_list[[1]][i]
            cur_name <- event_list[[2]][i]
            
            row.index <- 3
            def.oe <- def.all.oe[[i]]
            p1 <- rates.plot(def.oe[which(def.oe$sex==1&def.oe$race==r), c(1, 4:5)], nl, nh, "o/e rate", paste0("Figure 1.", i, cur_name,  " o/e rates, male", plot.name, sep=""))
            print(p1)
            insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")
            
            row.index <- row.index + 22
            p2 <- rates.plot(def.oe[which(def.oe$sex==2&def.oe$race==r), c(1, 4:5)], nl, nh, "o/e rate", paste0("Figure 2.", i, cur_name,  " o/e rates, female", plot.name, sep=""))
            print(p2)
            insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")
          }
        }
        
        if(cal_fre == TRUE){
          
          for(i in 1:length(event_list[[1]])){
            cur_event <- event_list[[1]][i]
            cur_name <- event_list[[2]][i]
            
            row.index <- 3
            def.freq <- def.all.freq[[i]]
            p1 <- rates.plot(def.freq[which(def.freq$sex==1&def.freq$race==r), c(1, 4:5)], nl, nh, "frequency", paste0("Figure 3.", i, cur_name,  " frequencies, male", plot.name, sep=""))
            print(p1)
            insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")
            
            row.index <- row.index + 22
            p2 <- rates.plot(def.freq[which(def.freq$sex==2&def.freq$race==r), c(1, 4:5)], nl, nh, "frequency", paste0("Figure 4.", i, cur_name,  " frequencies, female", plot.name, sep=""))
            print(p2)
            insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")
          }
          
        }
        
        
        output.dir <- getwd()
        plot.name <- gsub(", ", "-", plot.name)
        saveWorkbook(wb, paste0(output.dir, "/", title, " User defined event plot", plot.name, ".xlsx", sep=""), overwrite=TRUE)
        
      }
      
    }
    
    if (byvar=="race.nosex"){
      
      for(j in 1:length(race_list[[1]])){
        r <- race_list[[1]][j]
        r.name <- race_list[[2]][j]
        r.code <- paste0(", ", r.name, sep="")
        plot.name <- r.code
        
        if(cal_oe == TRUE){
          
          for(i in 1:length(event_list[[1]])){
            cur_event <- event_list[[1]][i]
            cur_name <- event_list[[2]][i]
            
            row.index <- 3
            def.oe <- def.all.oe[[i]]
            p1 <- rates.plot(def.oe[which(def.oe$race==r), c(1, 3:4)], nl, nh, "o/e rate", paste0("Figure 1.", i, cur_name,  " o/e rates", plot.name, sep=""))
            print(p1)
            insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")
            
          }
        }
        
        if(cal_fre == TRUE){
          
          for(i in 1:length(event_list[[1]])){
            cur_event <- event_list[[1]][i]
            cur_name <- event_list[[2]][i]
            
            row.index <- 3
            def.freq <- def.all.freq[[i]]
            p1 <- rates.plot(def.freq[which(def.freq$race==r), c(1, 3:4)], nl, nh, "frequency", paste0("Figure 3.", i, cur_name,  " frequencies", plot.name, sep=""))
            print(p1)
            insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")
            
          }
          
        }
        
        
        output.dir <- getwd()
        plot.name <- gsub(", ", "-", plot.name)
        saveWorkbook(wb, paste0(output.dir, "/", title, " User defined event plot", plot.name, ".xlsx", sep=""), overwrite=TRUE)
        
      }
      
    }
    
    if (byvar=="edu"){

      edu.code <- c(1:5)
      educode <- data.frame(code=c(1:5), name=c("No education", "Primary school",
                                                "Middle school", "High school", "College or higher"))

      for(j in edu.code){
        e.name <- educode[which(educode$code==j), "name"]
        e.code <- paste0(", ", e.name, sep="")
        plot.name <- e.code

        if(cal_oe == TRUE){
          
          for(i in 1:length(event_list[[1]])){
            cur_event <- event_list[[1]][i]
            cur_name <- event_list[[2]][i]
            
            row.index <- 3
            def.oe <- def.all.oe[[i]]
            p1 <- rates.plot(def.oe[which(def.oe$sex==1&def.oe$edu==j), c(1, 4:5)], nl, nh, "o/e rate", paste0("Figure 1.", i, cur_name,  " o/e rates, male", plot.name, sep=""))
            print(p1)
            insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")
            
            row.index <- row.index + 22
            p2 <- rates.plot(def.oe[which(def.oe$sex==2&def.oe$edu==j), c(1, 4:5)], nl, nh, "o/e rate", paste0("Figure 2.", i, cur_name,  " o/e rates, female", plot.name, sep=""))
            print(p2)
            insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")
          }
        }
        
        if(cal_fre == TRUE){
          
          for(i in 1:length(event_list[[1]])){
            cur_event <- event_list[[1]][i]
            cur_name <- event_list[[2]][i]
            
            row.index <- 3
            def.freq <- def.all.freq[[i]]
            p1 <- rates.plot(def.freq[which(def.freq$sex==1&def.freq$edu==j), c(1, 4:5)], nl, nh, "frequency", paste0("Figure 3.", i, cur_name,  " frequencies, male", plot.name, sep=""))
            print(p1)
            insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")
            
            row.index <- row.index + 22
            p2 <- rates.plot(def.freq[which(def.freq$sex==2&def.freq$edu==j), c(1, 4:5)], nl, nh, "frequency", paste0("Figure 4.", i, cur_name,  " frequencies, female", plot.name, sep=""))
            print(p2)
            insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")
          }
          
        }
        
        output.dir <- getwd()
        plot.name <- gsub(", ", "-", plot.name)
        saveWorkbook(wb, paste0(output.dir, "/", title, " User defined event plot", plot.name, ".xlsx", sep=""), overwrite=TRUE)
      }
    }

    if (byvar=="with_p"){

      p.code <- c(0:2)
      pcode <- data.frame(code=c(0:2),
                            name=c("Without parents", "With single parent", "With parents"))

      for(j in p.code){
        p.name <- pcode[which(pcode$code==j), "name"]
        p.code <- paste0(", ", p.name, sep="")
        plot.name <- p.code

        if(cal_oe == TRUE){

          for(i in 1:length(event_list[[1]])){
            cur_event <- event_list[[1]][i]
            cur_name <- event_list[[2]][i]

            row.index <- 3
            def.oe <- def.all.oe[[i]]
            p1 <- rates.plot(def.oe[which(def.oe$sex==1&def.oe$with_p==j), c(1, 4:5)], nl, nh, "o/e rate", paste0("Figure 1.", i, cur_name,  " o/e rates, male", plot.name, sep=""))
            print(p1)
            insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")

            row.index <- row.index + 22
            p2 <- rates.plot(def.oe[which(def.oe$sex==2&def.oe$with_p==j), c(1, 4:5)], nl, nh, "o/e rate", paste0("Figure 2.", i, cur_name,  " o/e rates, female", plot.name, sep=""))
            print(p2)
            insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")
          }
        }

        if(cal_fre == TRUE){

          for(i in 1:length(event_list[[1]])){
            cur_event <- event_list[[1]][i]
            cur_name <- event_list[[2]][i]

            row.index <- 3
            def.freq <- def.all.freq[[i]]
            p1 <- rates.plot(def.freq[which(def.freq$sex==1&def.freq$with_p==j), c(1, 4:5)], nl, nh, "frequency", paste0("Figure 3.", i, cur_name,  " frequencies, male", plot.name, sep=""))
            print(p1)
            insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")

            row.index <- row.index + 22
            p2 <- rates.plot(def.freq[which(def.freq$sex==2&def.freq$with_p==j), c(1, 4:5)], nl, nh, "frequency", paste0("Figure 4.", i, cur_name,  " frequencies, female", plot.name, sep=""))
            print(p2)
            insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")
          }

        }

        output.dir <- getwd()
        plot.name <- gsub(", ", "-", plot.name)
        saveWorkbook(wb, paste0(output.dir, "/", title, " User defined event plot", plot.name, ".xlsx", sep=""), overwrite=TRUE)

      }

    }

    if (byvar=="with_c"){

      c.code <- c(0, 1)

      for(j in c.code){
        c.name <- ifelse(j==0, "without children", "with children")
        c.code <- paste0(", ", c.name, sep="")
        plot.name <- c.code

        if(cal_oe == TRUE){

          for(i in 1:length(event_list[[1]])){
            cur_event <- event_list[[1]][i]
            cur_name <- event_list[[2]][i]

            row.index <- 3
            def.oe <- def.all.oe[[i]]
            p1 <- rates.plot(def.oe[which(def.oe$sex==1&def.oe$with_c==j), c(1, 4:5)], nl, nh, "o/e rate", paste0("Figure 1.", i, cur_name,  " o/e rates, male", plot.name, sep=""))
            print(p1)
            insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")

            row.index <- row.index + 22
            p2 <- rates.plot(def.oe[which(def.oe$sex==2&def.oe$with_c==j), c(1, 4:5)], nl, nh, "o/e rate", paste0("Figure 2.", i, cur_name,  " o/e rates, female", plot.name, sep=""))
            print(p2)
            insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")
          }
        }

        if(cal_fre == TRUE){

          for(i in 1:length(event_list[[1]])){
            cur_event <- event_list[[1]][i]
            cur_name <- event_list[[2]][i]

            row.index <- 3
            def.freq <- def.all.freq[[i]]
            p1 <- rates.plot(def.freq[which(def.freq$sex==1&def.freq$with_c==j), c(1, 4:5)], nl, nh, "frequency", paste0("Figure 3.", i, cur_name,  " frequencies, male", plot.name, sep=""))
            print(p1)
            insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")

            row.index <- row.index + 22
            p2 <- rates.plot(def.freq[which(def.freq$sex==2&def.freq$with_c==j), c(1, 4:5)], nl, nh, "frequency", paste0("Figure 4.", i, cur_name,  " frequencies, female", plot.name, sep=""))
            print(p2)
            insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")
          }

        }

        output.dir <- getwd()
        plot.name <- gsub(", ", "-", plot.name)
        saveWorkbook(wb, paste0(output.dir, "/", title, " User defined event plot", plot.name, ".xlsx", sep=""), overwrite=TRUE)

      }

    }

    if (byvar=="marital"){

      for(j in 1:length(marital_list[[1]])){
        m <- marital_list[[1]][j]
        m.name <- marital_list[[2]][j]
        m.code <- paste0(", ", m.name, sep="")
        plot.name <- m.code

        if(cal_oe == TRUE){

          for(i in 1:length(event_list[[1]])){
            cur_event <- event_list[[1]][i]
            cur_name <- event_list[[2]][i]

            row.index <- 3
            def.oe <- def.all.oe[[i]]
            p1 <- rates.plot(def.oe[which(def.oe$sex==1&def.oe$mar==m), c(1, 4:5)], nl, nh, "o/e rate", paste0("Figure 1.", i, cur_name,  " o/e rates, male", plot.name, sep=""))
            print(p1)
            insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")

            row.index <- row.index + 22
            p2 <- rates.plot(def.oe[which(def.oe$sex==2&def.oe$mar==m), c(1, 4:5)], nl, nh, "o/e rate", paste0("Figure 2.", i, cur_name,  " o/e rates, female", plot.name, sep=""))
            print(p2)
            insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")
          }
        }

        if(cal_fre == TRUE){

          for(i in 1:length(event_list[[1]])){
            cur_event <- event_list[[1]][i]
            cur_name <- event_list[[2]][i]

            row.index <- 3
            def.freq <- def.all.freq[[i]]
            p1 <- rates.plot(def.freq[which(def.freq$sex==1&def.freq$mar==m), c(1, 4:5)], nl, nh, "frequency", paste0("Figure 3.", i, cur_name,  " frequencies, male", plot.name, sep=""))
            print(p1)
            insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")

            row.index <- row.index + 22
            p2 <- rates.plot(def.freq[which(def.freq$sex==2&def.freq$mar==m), c(1, 4:5)], nl, nh, "frequency", paste0("Figure 4.", i, cur_name,  " frequencies, female", plot.name, sep=""))
            print(p2)
            insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1+(i-1)*6, width=12.55, height=10.4, units="cm")
          }

        }


        output.dir <- getwd()
        plot.name <- gsub(", ", "-", plot.name)
        saveWorkbook(wb, paste0(output.dir, "/", title, " User defined event plot", plot.name, ".xlsx", sep=""), overwrite=TRUE)

      }
    }
  }

}

freq.def <- function(frequency){

  #calculate total rates
  total.rates <- total.freq(frequency)
  #calculate mean ages
  mean.age <- mean.age.cal(frequency)
  
  #add total rates as the last row of frequency data frame.
  freq.total <- data.frame(age = "Total", total.freq(frequency))
  frequency <- rbind(frequency, freq.total)
  
  return(list(frequency=frequency, total.rates=total.rates, mean.age=mean.age))
}

#calculated # of risk population and # of events
pop.count.def <- function(data, nl, nh, event_list, status_list, len_ec, sex){
  
  if (sex==T){
    #calculated # of risk population and # of events by gender
    pop=pop.count.def.sex(data, nl, nh, event_list, status_list, len_ec)
  } else if (sex==F){
    #calculated # of risk population and # of events not by gender
    pop=pop.count.def.nosex(data, nl, nh, event_list, status_list, len_ec)
  }
  
  return(pop)
}

#calculated # of risk population and # of events by gender
pop.count.def.sex <- function(data, nl, nh, event_list, status_list, len_ec){

  #age range
  age <- data.frame(age=seq(nl, nh, 1))
  #select useful variables
  d.001 <- subset(data, select=c(age, sex, post, event, py))

  # status
  if(nrow(d.001)==0){
    temp <- data.frame(age=NA,sex=NA,status=NA, count=NA)[F,]
  } else {
    temp <- aggregate(cbind(d.001[0], count=d.001$py),
                      list(age=d.001$age, sex=d.001$sex, status=d.001$post), sum)
  }

  all.male.s  <- list()
  all.female.s  <- list()

  for(i in 1:length(status_list[[1]])){
    cur_status <- status_list[[1]][i]#拿status的code
    cur_name <- status_list[[2]][i]

    all.male.s <- c(all.male.s, list(merge(age, subset(temp, sex == 1 & status == cur_status, select=c(age, count)), all.x=TRUE)))
    all.female.s <- c(all.female.s, list(merge(age, subset(temp, sex == 2 & status == cur_status, select=c(age, count)), all.x=TRUE)))
  }

  ##求总的
  if(nrow(temp)==0){
    all <- data.frame(age=NA,sex=NA,x=NA)[F,]
  } else {
    all <- aggregate(temp[,4], list(age=temp$age, sex=temp$sex), sum)
  }

  all.male <- merge(age, subset(all, sex == 1, select=c(age, x)), all.x=TRUE)
  names(all.male) <- c("age", "count")
  All_male<- all.male$count
  all.female <- merge(age, subset(all, sex == 2, select=c(age, x)), all.x=TRUE)
  names(all.female) <- c("age", "count")
  All_female <- all.female$count

  #循环加上status的名字 （可以和前面的循环合在一起 todo)
  status.male  <- list()
  status.female  <- list()
  for(i in 1:length(status_list[[1]])){
    cur_status <- status_list[[1]][i]
    cur_name <- status_list[[2]][i]
    ##male
    male.s <- (all.male.s[[i]][2])
    colnames(male.s) <- paste0("Male_", cur_name, sep="")#记得改名字
    status.male <-c( status.male,male.s)
    ##female
    female.s <- (all.female.s[[i]][2])
    colnames(female.s) <- paste0("Female_", cur_name, sep="")
    status.female <-c( status.female,female.s)

  }

  status <- data.frame(age=age$age, All_male, status.male,
                       All_female, status.female)
  status[, 2:ncol(status)] <- round(status[, 2:ncol(status)], 1)
  risk.total <- data.frame(age = "Total", t(tmp.colSum.w.na(status[2:ncol(status)])))
  status <- rbind(status, risk.total)
  rm(risk.total)

  # event
  if(nrow(d.001)==0){
    temp <- data.frame(age=NA,sex=NA,event=NA, count=NA)[F,]
  } else {
    temp <- aggregate(cbind(d.001[0], count=1),
                      list(age=d.001$age, sex=d.001$sex, event=d.001$event), sum)
  }

  all.male.e  <- list()
  all.female.e  <- list()

  for(i in 1:length(event_list[[1]])){
    cur_event <- event_list[[1]][i]#拿event的code
    cur_name <- event_list[[2]][i]
    #male
    all.male.e <- c(all.male.e, list(merge(age, subset(temp, sex == 1 & event == cur_event, select=c(age, count)), all.x=TRUE)))
    #female
    all.female.e <- c(all.female.e, list(merge(age, subset(temp, sex == 2 & event == cur_event, select=c(age, count)), all.x=TRUE)))
  }


  event.male  <- list()
  event.female  <- list()
  for(i in 1:length(event_list[[1]])){
    cur_event <- event_list[[1]][i]#拿event的code
    cur_name <- event_list[[2]][i]
    #male
    male.e <- all.male.e[[i]][2]
    colnames(male.e) <- paste0("Male_", cur_name, sep="")
    event.male <-c(event.male, male.e)
    #female
    female.e <- all.female.e[[i]][2]
    colnames(female.e) <- paste0("FeMale_", cur_name, sep="")
    event.female <-c(event.female, female.e)
  }

  #求总的
  t <- temp[-which(temp$event == 0),]
  if(nrow(t)==0){
    all <- data.frame(age=NA,sex=NA,x=NA)[F,]
  } else {
    all <- aggregate(t[,4], list(age=t$age, sex=t$sex), sum)
  }

  all.male <- merge(age, subset(all, sex == 1, select=c(age, x)), all.x=TRUE)
  names(all.male) <- c("age", "count")
  All_male<- all.male$count

  all.female <- merge(age, subset(all, sex == 2, select=c(age, x)), all.x=TRUE)
  names(all.female) <- c("age", "count")
  All_female <- all.female$count

  event <- data.frame(age=age$age, All_male, event.male, All_female, event.female)

  event.total <- data.frame(age = "Total", t(colSums(event[2:ncol(event)], na.rm = T)))
  event <- rbind(event, event.total)
  rm(event.total)

  return(list(status=status, event=event))
}

#calculated # of risk population and # of events not by gender
pop.count.def.nosex <- function(data, nl, nh, event_list, status_list, len_ec){
  
  age <- data.frame(age=seq(nl, nh, 1))
  d.001 <- subset(data, select=c(age, post, event, py))
  
  # status
  if(nrow(d.001)==0){
    temp <- data.frame(age=NA,status=NA, count=NA)[F,]
  } else {
    temp <- aggregate(cbind(d.001[0], count=d.001$py),
                      list(age=d.001$age, status=d.001$post), sum)
  }
  
  all.s  <- list()

  for(i in 1:length(status_list[[1]])){
    cur_status <- status_list[[1]][i]#拿status的code
    cur_name <- status_list[[2]][i]
    
    all.s <- c(all.s, list(merge(age, subset(temp, status == cur_status, select=c(age, count)), all.x=TRUE)))
  }
  
  ##求总的
  if(nrow(temp)==0){
    all <- data.frame(age=NA,x=NA)[F,]
  } else {
    all <- aggregate(temp[,3], list(age=temp$age), sum)
  }
  
  all <- merge(age, subset(all, select=c(age, x)), all.x=TRUE)
  names(all) <- c("age", "count")
  All<- all$count

  #循环加上status的名字 （可以和前面的循环合在一起 todo)
  status <- list()
  for(i in 1:length(status_list[[1]])){
    cur_status <- status_list[[1]][i]
    cur_name <- status_list[[2]][i]

    s <- (all.s[[i]][2])
    colnames(s) <- cur_name
    status <-c(status, s)

  }
  
  status <- data.frame(age=age$age, All, status)
  status[, 2:ncol(status)] <- round(status[, 2:ncol(status)], 1)
  risk.total <- data.frame(age = "Total", t(tmp.colSum.w.na(status[2:ncol(status)])))
  status <- rbind(status, risk.total)
  rm(risk.total)
  
  # event
  if(nrow(d.001)==0){
    temp <- data.frame(age=NA,event=NA, count=NA)[F,]
  } else {
    temp <- aggregate(cbind(d.001[0], count=1),
                      list(age=d.001$age, event=d.001$event), sum)
  }
  
  all.e  <- list()

  for(i in 1:length(event_list[[1]])){
    cur_event <- event_list[[1]][i]#拿event的code
    cur_name <- event_list[[2]][i]
    
    all.e <- c(all.e, list(merge(age, subset(temp, event == cur_event, select=c(age, count)), all.x=TRUE)))
  }
  
  
  event  <- list()
  for(i in 1:length(event_list[[1]])){
    cur_event <- event_list[[1]][i]#拿event的code
    cur_name <- event_list[[2]][i]
    
    e <- all.e[[i]][2]
    colnames(e) <- cur_name
    event <-c(event, e)
  }
  
  #求总的
  t <- temp[-which(temp$event == 0),]
  if(nrow(t)==0){
    all <- data.frame(age=NA,x=NA)[F,]
  } else {
    all <- aggregate(t[,3], list(age=t$age), sum)
  }
  
  all <- merge(age, subset(all, select=c(age, x)), all.x=TRUE)
  names(all) <- c("age", "count")
  All<- all$count
  
  event <- data.frame(age=age$age, All, event)
  
  event.total <- data.frame(age = "Total", t(colSums(event[2:ncol(event)], na.rm = T)))
  event <- rbind(event, event.total)
  rm(event.total)
  
  return(list(status=status, event=event))
}

#output pop table results
write.pop.def <- function(pop, param, name, eve_num, status_num, sex){
  
  if (sex==T){
    #output pop table by gender results
    write.pop.def.sex(pop, param, name, eve_num, status_num)
  } else if (sex==F){
    #output pop table not by gender results
    write.pop.def.nosex(pop, param, name, eve_num, status_num)
  }
  
}

write.pop.def.sex <- function(pop, param, name, eve_num, status_num){
  forDatstl <- createStyle(halign="center", border="TopBottomLeftRight", wrapText = TRUE)
  forDatstl2 <- createStyle(halign="right", border="TopBottomLeftRight", numFmt = "#,##0.0")
  wb <- createWorkbook()
  addWorksheet(wb, "status")
  addWorksheet(wb, "event")
  title <- as.character(param$title)

  writeData(wb, sheet=1,
            paste0("Table M1. Person-years by age, gender and status, ", title, name, sep=""),
            startRow=4, startCol=2)
  writeData(wb, sheet=1, "Male", startRow=5, startCol=3, borders="all")
  mergeCells(wb, sheet=1, cols=3:(3+status_num), rows=5)
  writeData(wb, sheet=1, "Female", startRow=5, startCol=(4+status_num), borders="all")
  mergeCells(wb, sheet=1, cols= (4+status_num):(4+status_num*2), rows=5)
  addStyle(wb, sheet=1, style=forDatstl, rows=5, cols=2:(4+status_num*2))
  writeData(wb, sheet=1, pop$status, startRow=6, startCol=2, borders="all")
  addStyle(wb, sheet=1, style=forDatstl, rows=6, cols=2:(4+status_num*2))###
  addStyle(wb, sheet=1, style=forDatstl2, rows=7:(6+nrow(pop$status)), cols=3:(4+status_num*2), gridExpand = TRUE)
  setColWidths(wb, sheet=1, cols=2:(4+status_num*2), widths=13)
  setColWidths(wb, sheet=1, cols=2, widths=7)

  writeData(wb, sheet=2, paste0("Table M2. Number of event by age, gender and event type, ", title, name, sep=""),
            startRow=4, startCol=2)
  writeData(wb, sheet=2, "Male", startRow=5, startCol=3, borders="all")
  mergeCells(wb, sheet=2, cols=3:(3+eve_num), rows=5)
  writeData(wb, sheet=2, "Female", startRow=5, startCol=(4+eve_num), borders="all")
  mergeCells(wb, sheet=2, cols= (4+eve_num):(4+eve_num*2), rows=5)
  addStyle(wb, sheet=2, style=forDatstl, rows=5, cols=2:(4+eve_num*2))
  writeData(wb, sheet=2, pop$event, startRow=6, startCol=2, borders="all")
  addStyle(wb, sheet=2, style=forDatstl, rows=6, cols=2:(4+eve_num*2))###
  addStyle(wb, sheet=2, style=forDatstl2, rows=7:(6+nrow(pop$event)), cols=3:(4+eve_num*2), gridExpand = TRUE)
  setColWidths(wb, sheet=2, cols=2:(4+eve_num*2), widths=13)
  setColWidths(wb, sheet=2, cols=2, widths=7)

  output.dir <- getwd()
  name <- gsub(", ", "-", name)
  saveWorkbook(wb, paste0(output.dir, "/", title, " User defined event popTable", name, ".xlsx", sep=""), overwrite=T)
}

write.pop.def.nosex <- function(pop, param, name, eve_num, status_num){
  forDatstl <- createStyle(halign="center", border="TopBottomLeftRight", wrapText = TRUE)
  forDatstl2 <- createStyle(halign="right", border="TopBottomLeftRight", numFmt = "#,##0.0")
  wb <- createWorkbook()
  addWorksheet(wb, "status")
  addWorksheet(wb, "event")
  title <- as.character(param$title)
  
  writeData(wb, sheet=1,
            paste0("Table M1. Person-years by age and status, ", title, name, sep=""),
            startRow=4, startCol=2)
  mergeCells(wb, sheet=1, cols=2:7, rows=4)
  writeData(wb, sheet=1, pop$status, startRow=5, startCol=2, borders="all")
  addStyle(wb, sheet=1, style=forDatstl, rows=5, cols=2:(3+status_num))
  addStyle(wb, sheet=1, style=forDatstl2, rows=6:(5+nrow(pop$status)), cols=3:(3+status_num), gridExpand = TRUE)
  setColWidths(wb, sheet=1, cols=2:(4+status_num), widths=13)
  setColWidths(wb, sheet=1, cols=2, widths=7)
  
  writeData(wb, sheet=2, paste0("Table M2. Number of event by age and event type, ", title, name, sep=""),
            startRow=4, startCol=2)
  mergeCells(wb, sheet=2, cols=2:7, rows=4)
  writeData(wb, sheet=2, pop$event, startRow=5, startCol=2, borders="all")
  addStyle(wb, sheet=2, style=forDatstl, rows=5, cols=2:(3+eve_num))
  addStyle(wb, sheet=2, style=forDatstl2, rows=6:(5+nrow(pop$event)), cols=3:(3+eve_num), gridExpand = TRUE)
  setColWidths(wb, sheet=2, cols=2:(4+eve_num), widths=13)
  setColWidths(wb, sheet=2, cols=2, widths=7)
  
  output.dir <- getwd()
  name <- gsub(", ", "-", name)
  saveWorkbook(wb, paste0(output.dir, "/", title, " User defined event popTable", name, ".xlsx", sep=""), overwrite=T)
}

write.rates.def <- function(oe.rates, frequency, event_list, param, name, method, sex){
  
  if (sex==T){
    #output rates by gender
    write.rates.def.sex(oe.rates, frequency, event_list, param, name, method)
  } else if (sex==F){
    #output rates not by gender
    write.rates.def.nosex(oe.rates, frequency, event_list, param, name, method)
  }
  
}

write.rates.def.sex <- function(oe.rates, frequency, event_list, param, name, method){
  forDatstl <- createStyle(halign="center", border="TopBottomLeftRight")
  forDatstl2 <- createStyle(halign="right", border="TopBottomLeftRight", numFmt = "#,##0.0000")
  wb <- createWorkbook()
  title <- as.character(param$title)
  event_num <- length(event_list[[1]])
  
  nrate <- as.numeric(param$nRate)
  cal_oe <- (nrate==1 | nrate==2)
  cal_freq <- (nrate==1 | nrate==3)
  k<-1
  
  if(cal_oe==T){
    
    addWorksheet(wb, "oe.rates")
    
    ###oe
    writeData(wb, sheet=k, paste0("Table M1. User defined event o/e rates, ", title, name, sep=""),
              startRow=4, startCol=2)
    addStyle(wb, sheet=k, style=forDatstl, rows=5, cols=2)
    
    
    ## todo to test
    for(i in 1:length(event_list[[1]])){
      cur_event <- event_list[[1]][i]
      cur_name <- event_list[[2]][i]
      
      oe.rates1 = oe.rates[,1:1]
      
      for(j in 1:event_num){
        male_id = j+1
        female_id = j+1+event_num
        oe.rates1 = cbind(oe.rates1, oe.rates[,c(male_id,female_id)])
      }
      
      cur_col = 3+(i-1)*2
      writeData(wb, sheet=k, cur_name, startRow=5, startCol=cur_col, borders="all")
      mergeCells(wb, sheet=k, cols=cur_col:(cur_col+1), rows=5)
      addStyle(wb, sheet=k, style=forDatstl, rows=5, cols=cur_col:(cur_col+1))
    }
    
    colnames(oe.rates1) <- c("Age", rep(c("Male", "Female"), event_num))
    writeData(wb, sheet=k, oe.rates1, startRow=6, startCol=2, borders="all")
    addStyle(wb, sheet=k, style=forDatstl, rows=6, cols=2:(event_num*2+2))
    addStyle(wb, sheet=k, style=forDatstl2, rows=7:(6+nrow(oe.rates1)), cols=3:(event_num*2+2), gridExpand = TRUE)
    setColWidths(wb, sheet=k, cols=2:(event_num*2+2), widths=25)
    setColWidths(wb, sheet=k, cols=2, widths=6)
    
    k<-k+1
    
  }
  
  
  if(cal_freq==T){
    
    addWorksheet(wb, "frequency")
    
    ###freq
    writeData(wb, sheet=k, paste0("Table M2. User defined event frequencies, ", title, name, sep=""),
              startRow=4, startCol=2)
    addStyle(wb, sheet=k, style=forDatstl, rows=5, cols=2)
    
    for(i in 1:length(event_list[[1]])){
      cur_event <- event_list[[1]][i]
      cur_name <- event_list[[2]][i]
      
      frequency1 = frequency[,1:1]
      
      for(j in 1:event_num){
        male_id = j+1
        female_id = j+1+event_num
        frequency1 = cbind(frequency1, frequency[,c(male_id,female_id)])
      }
      
      cur_col = 3+(i-1)*2
      writeData(wb, sheet=k, cur_name, startRow=5, startCol=cur_col, borders="all")
      mergeCells(wb, sheet=k, cols=cur_col:(cur_col+1), rows=5)
      addStyle(wb, sheet=k, style=forDatstl, rows=5, cols=cur_col:(cur_col+1))
    }
    
    colnames(frequency1) <-c("Age", rep(c("Male", "Female"), event_num))
    writeData(wb, sheet=k, frequency1, startRow=6, startCol=2, borders="all")
    addStyle(wb, sheet=k, style=forDatstl, rows=6, cols=2:(event_num*2+2))
    addStyle(wb, sheet=k, style=forDatstl2, rows=7:(6+nrow(frequency1)), cols=3:(event_num*2+2), gridExpand = TRUE)
    setColWidths(wb, sheet=k, cols=2:(event_num*2+2), widths=25)
    setColWidths(wb, sheet=k, cols=2, widths=6)
    
  }
  
  
  output.dir <- getwd()
  name <- gsub(", ", "-", name)
  saveWorkbook(wb, paste0(output.dir, "/", title, " User defined event rates, ", method, name, ".xlsx", sep=""), overwrite=TRUE)
}

write.rates.def.nosex <- function(oe.rates, frequency, event_list, param, name, method){
  forDatstl <- createStyle(halign="center", border="TopBottomLeftRight")
  forDatstl2 <- createStyle(halign="right", border="TopBottomLeftRight", numFmt = "#,##0.0000")
  wb <- createWorkbook()
  title <- as.character(param$title)
  event_num <- length(event_list[[1]])
  
  nrate <- as.numeric(param$nRate)
  cal_oe <- (nrate==1 | nrate==2)
  cal_freq <- (nrate==1 | nrate==3)
  k<-1
  
  if(cal_oe==T){
    
    addWorksheet(wb, "oe.rates")
    
    ###oe
    writeData(wb, sheet=k, paste0("Table M1. User defined event o/e rates, ", title, name, sep=""),
              startRow=4, startCol=2)
    addStyle(wb, sheet=k, style=forDatstl, rows=4, cols=2)
    
    colnames(oe.rates) <- c("Age", rep(c("Rates"), event_num))
    writeData(wb, sheet=k, oe.rates, startRow=5, startCol=2, borders="all")
    addStyle(wb, sheet=k, style=forDatstl, rows=5, cols=2:(event_num+2))
    addStyle(wb, sheet=k, style=forDatstl2, rows=6:(5+nrow(oe.rates)), cols=3:(event_num+2), gridExpand = TRUE)
    setColWidths(wb, sheet=k, cols=2:(event_num+2), widths=25)
    setColWidths(wb, sheet=k, cols=2, widths=6)
    
    k<-k+1
    
  }
  
  
  if(cal_freq==T){
    
    addWorksheet(wb, "frequency")
    
    ###freq
    writeData(wb, sheet=k, paste0("Table M2. User defined event frequencies, ", title, name, sep=""),
              startRow=4, startCol=2)
    addStyle(wb, sheet=k, style=forDatstl, rows=4, cols=2)
    
    colnames(frequency) <-c("Age", rep(c("Rates"), event_num))
    writeData(wb, sheet=k, frequency, startRow=5, startCol=2, borders="all")
    addStyle(wb, sheet=k, style=forDatstl, rows=5, cols=2:(event_num+2))
    addStyle(wb, sheet=k, style=forDatstl2, rows=6:(5+nrow(frequency)), cols=3:(event_num+2), gridExpand = TRUE)
    setColWidths(wb, sheet=k, cols=2:(event_num+2), widths=25)
    setColWidths(wb, sheet=k, cols=2, widths=6)
    
  }
  
  
  output.dir <- getwd()
  name <- gsub(", ", "-", name)
  saveWorkbook(wb, paste0(output.dir, "/", title, " User defined event rates, ", method, name, ".xlsx", sep=""), overwrite=TRUE)
}

write.total.def <- function(total.rates, mean.age, event_list, param, name, sex){
  if (sex==T){
    #output total rates by gender results
    write.total.def.sex(total.rates, mean.age, event_list, param, name)
  } else if (sex==F){
    #output total rates not by gender results
    write.total.def.nosex(total.rates, mean.age, event_list, param, name)
  }
}

write.total.def.sex <- function(total.rates, mean.age, event_list, param, name){
  forDatstl <- createStyle(halign="center", border="TopBottomLeftRight", wrapText = TRUE)
  forDatstl2 <- createStyle(halign="left", border="TopBottomLeftRight", wrapText = TRUE)
  forDatstl3 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0000")
  forDatstl4 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0")
  forDatstl5 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0%")
  wb <- createWorkbook()
  addWorksheet(wb, "total rates")
  title <- as.character(param$title)
  event_num <- length(event_list[[1]])

  if (nrow(total.rates)==3){

    writeData(wb, sheet=1, paste0("Table M3. Total rates and mean ages of User defined event, ", title, sep=""),
              startRow=4, startCol=2)
    mergeCells(wb, sheet=1, cols=2:(event_num*2+2), rows=4)
    cname <- rep(c("Male", "Female"), event_num)
    cname <- as.data.frame(t(cname))

    total.rates.def = NA
    mean.age.def = NA
    addStyle(wb, sheet=1, style=forDatstl, rows=5, cols=2)

    for(i in 1:length(event_list[[1]])){
      cur_event <- event_list[[1]][i]
      cur_name <- event_list[[2]][i]

      j = i
      male_id = j
      female_id = j+event_num
      if(is.na(total.rates.def)){
        total.rates.def = total.rates[,c(male_id,female_id)]
        mean.age.def = mean.age[,c(male_id,female_id)]
      }
      else{
        total.rates.def = cbind(total.rates.def, total.rates[,c(male_id,female_id)])
        mean.age.def = cbind(mean.age.def, mean.age[,c(male_id,female_id)])
      }

      cur_col=3+(i-1)*2
      writeData(wb, sheet=1, cur_name, startRow=5, startCol=cur_col, borders="all")
      mergeCells(wb, sheet=1, cols=cur_col:(cur_col+1), rows=5)
      addStyle(wb, sheet=1, style=forDatstl, rows=5, cols=cur_col:(cur_col+1))
    }
    
    total.rates.def = as.data.frame(total.rates.def)
    mean.age.def = as.data.frame(mean.age.def)
    rownames(total.rates.def) <- c("Direct calculate", "Poisson estimate", "Difference%")
    rownames(mean.age.def) <- c("Direct calculate", "Poisson estimate", "Difference%")

    writeData(wb, sheet=1, cname, startRow=6, startCol=3, colNames = FALSE, borders="all")
    writeData(wb, sheet=1, "Total rate", startRow=7, startCol=2, borders="all")
    writeData(wb, sheet=1, total.rates.def, startRow=8, startCol=2, colNames = FALSE, rowNames = TRUE, borders="all")
    writeData(wb, sheet=1, "Mean age", startRow=11, startCol=2, borders="all")
    writeData(wb, sheet=1, mean.age.def, startRow=12, startCol=2, colNames = FALSE, rowNames = TRUE, borders="all")
    addStyle(wb, sheet=1, style=forDatstl, rows=6, cols=2:(event_num*2+2))
    addStyle(wb, sheet=1, style=forDatstl3, rows=7:9, cols=3:(event_num*2+2), gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl5, rows=10, cols=3:(event_num*2+2))
    addStyle(wb, sheet=1, style=forDatstl4, rows=11:13, cols=3:(event_num*2+2), gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl5, rows=14, cols=3:(event_num*2+2))
    setColWidths(wb, sheet=1, cols=3:(event_num*2+2), widths=20)
    setColWidths(wb, sheet=1, cols=2, widths=17)
  } else {

    addWorksheet(wb, "mean ages")
    
    writeData(wb, sheet=1, paste0("Table M3. Total rates of User defined event, ", title, sep=""),
              startRow=4, startCol=2)

    writeData(wb, sheet=2, paste0("Table M3. Mean ages of User defined event, ", title, sep=""),
              startRow=4, startCol=2)

    total.rates.def = NA
    mean.age.def = NA

    for(i in 1:length(event_list[[1]])){
      cur_event <- event_list[[1]][i]
      cur_name <- event_list[[2]][i]

      # only take the header
      total.rates.def = total.rates[,1:2]
      mean.age.def = mean.age[,1:2]

      for(j in 1:event_num){
        male_id = j+2
        female_id = j+2+event_num
        total.rates.def = cbind(total.rates.def, total.rates[,c(male_id,female_id)])
        mean.age.def = cbind(mean.age.def, mean.age[,c(male_id,female_id)])
      }
      
      cur_col = 4+(i-1)*2
      writeData(wb, sheet=1, cur_name, startRow=5, startCol=cur_col, borders="all")
      mergeCells(wb, sheet=1, cols=cur_col:(cur_col+1), rows=5)
      addStyle(wb, sheet=1, style=forDatstl, rows=5, cols=cur_col:(cur_col+1))
      
      writeData(wb, sheet=2, cur_name, startRow=5, startCol=cur_col, borders="all")
      mergeCells(wb, sheet=2, cols=cur_col:(cur_col+1), rows=5)
      addStyle(wb, sheet=2, style=forDatstl, rows=5, cols=cur_col:(cur_col+1))
      
    }

    total.rates.def = as.data.frame(total.rates.def)
    mean.age.def = as.data.frame(mean.age.def)

    colnames(total.rates.def) <-c("", "", rep(c("Male",'Female'), event_num))
    colnames(mean.age.def) <-c("", "", rep(c("Male",'Female'), event_num))
    
    writeData(wb, sheet=1, total.rates.def, startRow=6, startCol=2, borders="all")
    addStyle(wb, sheet=1, style=forDatstl, rows=5:6, cols=2:(event_num*2+3), gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl3, rows=7:(nrow(total.rates.def)+6), cols=2:(event_num*2+3), gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl5, rows=seq(1, nrow(total.rates.def), by=3)+8, cols=2:(event_num*2+3), gridExpand = TRUE)
    setColWidths(wb, sheet=1, cols=4:(event_num*2+3), widths=15)
    setColWidths(wb, sheet=1, cols=2, widths=35)
    setColWidths(wb, sheet=1, cols=3, widths=15)

    writeData(wb, sheet=2, mean.age.def, startRow=6, startCol=2, borders="all")
    addStyle(wb, sheet=2, style=forDatstl, rows=5:6, cols=2:(event_num*2+3), gridExpand = TRUE)
    addStyle(wb, sheet=2, style=forDatstl4, rows=7:(nrow(mean.age.def)+6), cols=2:(event_num*2+3), gridExpand = TRUE)
    addStyle(wb, sheet=2, style=forDatstl5, rows=seq(1, nrow(mean.age.def), by=3)+8, cols=2:(event_num*2+3), gridExpand = TRUE)
    setColWidths(wb, sheet=2, cols=4:(event_num*2+3), widths=15)
    setColWidths(wb, sheet=2, cols=2, widths=35)
    setColWidths(wb, sheet=2, cols=3, widths=15)
    
  }

  output.dir <- getwd()
  saveWorkbook(wb, paste0(output.dir, "/", title, " User defined event total rates", name, ".xlsx", sep=""), overwrite=T)
}

write.total.def.nosex <- function(total.rates, mean.age, event_list, param, name){
  forDatstl <- createStyle(halign="center", border="TopBottomLeftRight", wrapText = TRUE)
  forDatstl2 <- createStyle(halign="left", border="TopBottomLeftRight", wrapText = TRUE)
  forDatstl3 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0000")
  forDatstl4 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0")
  forDatstl5 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0%")
  wb <- createWorkbook()
  addWorksheet(wb, "total rates")
  title <- as.character(param$title)
  event_num <- length(event_list[[1]])
  
  if (nrow(total.rates)==3){
    
    writeData(wb, sheet=1, paste0("Table M3. Total rates and mean ages of User defined event, ", title, sep=""),
              startRow=4, startCol=2)
    mergeCells(wb, sheet=1, cols=2:(event_num+2), rows=4)

    total.rates.def = total.rates
    mean.age.def = mean.age

    ## add event names
    for(i in 1:length(event_list[[1]])){
      cur_event <- event_list[[1]][i]
      cur_name <- event_list[[2]][i]
      
      cur_col=3+(i-1)
      writeData(wb, sheet=1, cur_name, startRow=5, startCol=cur_col, borders="all")
    }
    
    addStyle(wb, sheet=1, style=forDatstl, rows=5, cols=2:(event_num+2))
    
    rownames(total.rates.def) <- c("Direct calculate", "Poisson estimate", "Difference%")
    rownames(mean.age.def) <- c("Direct calculate", "Poisson estimate", "Difference%")
    
    writeData(wb, sheet=1, "Total rate", startRow=6, startCol=2, borders="all")
    writeData(wb, sheet=1, total.rates.def, startRow=7, startCol=2, colNames = FALSE, rowNames = TRUE, borders="all")
    writeData(wb, sheet=1, "Mean age", startRow=10, startCol=2, borders="all")
    writeData(wb, sheet=1, mean.age.def, startRow=11, startCol=2, colNames = FALSE, rowNames = TRUE, borders="all")
    addStyle(wb, sheet=1, style=forDatstl, rows=5, cols=2:(event_num+2), gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl3, rows=6:8, cols=3:(event_num+2), gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl5, rows=9, cols=3:(event_num+2), gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl4, rows=10:12, cols=3:(event_num+2), gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl5, rows=13, cols=3:(event_num+2))
    setColWidths(wb, sheet=1, cols=3:(event_num+2), widths=20)
    setColWidths(wb, sheet=1, cols=2, widths=17)
    
  } else {
    
    addWorksheet(wb, "mean ages")
    
    writeData(wb, sheet=1, paste0("Table M3. Total rates of User defined event, ", title, sep=""),
              startRow=4, startCol=2)

    writeData(wb, sheet=2, paste0("Table M3. Mean ages of User defined event, ", title, sep=""),
              startRow=4, startCol=2)

    total.rates.def = total.rates
    mean.age.def = mean.age
    
    ## add event names
    for(i in 1:length(event_list[[1]])){
      cur_event <- event_list[[1]][i]
      cur_name <- event_list[[2]][i]
      
      cur_col = 4+(i-1)
      writeData(wb, sheet=1, cur_name, startRow=5, startCol=cur_col, borders="all")

      writeData(wb, sheet=2, cur_name, startRow=5, startCol=cur_col, borders="all")

    }
    
    addStyle(wb, sheet=1, style=forDatstl, rows=5, cols=2:(event_num+2))
    addStyle(wb, sheet=2, style=forDatstl, rows=5, cols=2:(event_num+2))
    
    writeData(wb, sheet=1, total.rates.def, startRow=6, startCol=2, colNames = FALSE, borders="all")
    addStyle(wb, sheet=1, style=forDatstl, rows=6, cols=2:(event_num+3), gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl3, rows=6:(nrow(total.rates.def)+5), cols=2:(event_num+3), gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl5, rows=seq(1, nrow(total.rates.def), by=3)+7, cols=2:(event_num+3), gridExpand = TRUE)
    setColWidths(wb, sheet=1, cols=4:(event_num+3), widths=15)
    setColWidths(wb, sheet=1, cols=2, widths=35)
    setColWidths(wb, sheet=1, cols=3, widths=15)
    
    writeData(wb, sheet=2, mean.age.def, startRow=6, startCol=2, colNames = FALSE, borders="all")
    addStyle(wb, sheet=2, style=forDatstl, rows=6, cols=2:(event_num+3), gridExpand = TRUE)
    addStyle(wb, sheet=2, style=forDatstl4, rows=6:(nrow(mean.age.def)+5), cols=2:(event_num+3), gridExpand = TRUE)
    addStyle(wb, sheet=2, style=forDatstl5, rows=seq(1, nrow(mean.age.def), by=3)+7, cols=2:(event_num+3), gridExpand = TRUE)
    setColWidths(wb, sheet=2, cols=4:(event_num+3), widths=15)
    setColWidths(wb, sheet=2, cols=2, widths=35)
    setColWidths(wb, sheet=2, cols=3, widths=15)
    
  }
  
  output.dir <- getwd()
  saveWorkbook(wb, paste0(output.dir, "/", title, " User defined event total rates", name, ".xlsx", sep=""), overwrite=T)
}

write.total.def.covar <- function(total.rates.ru, total.rates.ub, mean.age.ru, mean.age.ub, event_list, param, name, sex){
  if (sex==T){
    write.total.def.covar.sex(total.rates.ru, total.rates.ub, mean.age.ru, mean.age.ub, event_list, param, name)
  } else if (sex==F){
    write.total.def.covar.nosex(total.rates.ru, total.rates.ub, mean.age.ru, mean.age.ub, event_list, param, name)
  }
}

write.total.def.covar.sex <- function(total.rates.ru, total.rates.ub, mean.age.ru, mean.age.ub, event_list, param, name){
  forDatstl <- createStyle(halign="center", border="TopBottomLeftRight", wrapText = TRUE)
  forDatstl2 <- createStyle(halign="left", border="TopBottomLeftRight", wrapText = TRUE)
  forDatstl3 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0000")
  forDatstl4 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0")
  forDatstl5 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0%")
  wb <- createWorkbook()
  title <- as.character(param$title)
  event_num <- length(event_list[[1]])

  for (i in 1:4) {

    if (i==1){
      addWorksheet(wb, "total rates rural")
      writeData(wb, sheet=1, paste0("Table M5. Total rates of transitions, ", title, sep=""),
                startRow=4, startCol=2)
      input <- total.rates.ru
    }

    if (i==2){
      addWorksheet(wb, "mean ages rural")
      writeData(wb, sheet=2, paste0("Table M5. Mean ages of transitions, ", title, sep=""),
                startRow=4, startCol=2)
      input <- mean.age.ru
    }

    if (i==3){
      addWorksheet(wb, "total rates urban")
      writeData(wb, sheet=3, paste0("Table M5. Total rates of transitions, ", title, sep=""),
                startRow=4, startCol=2)
      input <- total.rates.ub
    }

    if (i==4){
      addWorksheet(wb, "mean ages urban")
      writeData(wb, sheet=4, paste0("Table M5. Mean ages of transitions, ", title, sep=""),
                startRow=4, startCol=2)
      input <- mean.age.ub
    }

    for(j in 1:length(event_list[[1]])){

      cur_event <- event_list[[1]][j]
      cur_name <- event_list[[2]][j]

      cur_col = 3+(j-1)*6
      writeData(wb, sheet=i, cur_name, startRow=5, startCol=cur_col, borders="all")
      mergeCells(wb, sheet=i, cols=cur_col:(cur_col+5), rows=5)
      writeData(wb, sheet=i, "Male", startRow=6, startCol=cur_col, borders="all")
      writeData(wb, sheet=i, "Female", startRow=6, startCol=cur_col+3, borders="all")
      mergeCells(wb, sheet=i, cols=cur_col:(cur_col+2), rows=6)
      mergeCells(wb, sheet=i, cols=(cur_col+3):(cur_col+5), rows=6)

      addStyle(wb, sheet=i, style=forDatstl2, rows=5:(nrow(input)+7), cols=2, gridExpand = TRUE)
      addStyle(wb, sheet=i, style=forDatstl, rows=5:7, cols=cur_col:(cur_col+5), gridExpand = TRUE)
      
      colnames(input) <- c(cur_name, rep(c("Direct Calculate", "Poisson Estimate", "Difference%"),event_num*2))
      writeData(wb, sheet=i, input, startRow=7, startCol=2, borders="all")
      
      if (i%%2==0){
        addStyle(wb, sheet=i, style=forDatstl4, rows=8:(nrow(input)+7), cols=c(cur_col:(cur_col+1),(cur_col+3):(cur_col+4)), gridExpand = TRUE)
      } else {
        addStyle(wb, sheet=i, style=forDatstl3, rows=8:(nrow(input)+7), cols=c(cur_col:(cur_col+1),(cur_col+3):(cur_col+4)), gridExpand = TRUE)
      }
      addStyle(wb, sheet=i, style=forDatstl5, rows=8:(nrow(input)+7), cols=c(cur_col+2, cur_col+5), gridExpand = TRUE)
      setColWidths(wb, sheet=i, cols=cur_col:(cur_col+5), widths=9)
    }

    setColWidths(wb, sheet=i, cols=2, widths=25)

  }

  output.dir <- getwd()
  saveWorkbook(wb, paste0(output.dir, "/", title, " User defined event total rates", name, ".xlsx", sep=""), overwrite=T)
}

write.total.def.covar.nosex <- function(total.rates.ru, total.rates.ub, mean.age.ru, mean.age.ub, event_list, param, name){
  forDatstl <- createStyle(halign="center", border="TopBottomLeftRight", wrapText = TRUE)
  forDatstl2 <- createStyle(halign="left", border="TopBottomLeftRight", wrapText = TRUE)
  forDatstl3 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0000")
  forDatstl4 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0")
  forDatstl5 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0%")
  wb <- createWorkbook()
  title <- as.character(param$title)
  event_num <- length(event_list[[1]])
  
  for (i in 1:4) {
    
    if (i==1){
      addWorksheet(wb, "total rates rural")
      writeData(wb, sheet=1, paste0("Table M5. Total rates of transitions, ", title, sep=""),
                startRow=4, startCol=2)
      input <- total.rates.ru
    }
    
    if (i==2){
      addWorksheet(wb, "mean ages rural")
      writeData(wb, sheet=2, paste0("Table M5. Mean ages of transitions, ", title, sep=""),
                startRow=4, startCol=2)
      input <- mean.age.ru
    }
    
    if (i==3){
      addWorksheet(wb, "total rates urban")
      writeData(wb, sheet=3, paste0("Table M5. Total rates of transitions, ", title, sep=""),
                startRow=4, startCol=2)
      input <- total.rates.ub
    }
    
    if (i==4){
      addWorksheet(wb, "mean ages urban")
      writeData(wb, sheet=4, paste0("Table M5. Mean ages of transitions, ", title, sep=""),
                startRow=4, startCol=2)
      input <- mean.age.ub
    }
    
    for(j in 1:length(event_list[[1]])){
      
      cur_event <- event_list[[1]][j]
      cur_name <- event_list[[2]][j]
      
      cur_col = 3+(j-1)*3

      colnames(input) <- c(cur_name, rep(c("Direct Calculate", "Poisson Estimate", "Difference%"),event_num))
      writeData(wb, sheet=i, input, startRow=5, startCol=2, borders="all")
      addStyle(wb, sheet=i, style=forDatstl2, rows=5:(nrow(input)+5), cols=2, gridExpand = TRUE)
      addStyle(wb, sheet=i, style=forDatstl, rows=5, cols=cur_col:(cur_col+2), gridExpand = TRUE)
      
      if (i%%2==0){
        addStyle(wb, sheet=i, style=forDatstl4, rows=6:(nrow(input)+5), cols=cur_col:(cur_col+1), gridExpand = TRUE)
      } else {
        addStyle(wb, sheet=i, style=forDatstl3, rows=6:(nrow(input)+5), cols=cur_col:(cur_col+1), gridExpand = TRUE)
      }
      addStyle(wb, sheet=i, style=forDatstl5, rows=6:(nrow(input)+5), cols=cur_col+2, gridExpand = TRUE)
      setColWidths(wb, sheet=i, cols=cur_col:(cur_col+2), widths=9)
    }
    
    setColWidths(wb, sheet=i, cols=2, widths=25)
    
  }
  
  output.dir <- getwd()
  saveWorkbook(wb, paste0(output.dir, "/", title, " User defined event total rates", name, ".xlsx", sep=""), overwrite=T)
}

