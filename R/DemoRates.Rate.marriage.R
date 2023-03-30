#This script saves all the function needed for DemoRates rates estimation.


#### The main function for marriage estimation with subset method ####
run.marriage.rates <- function(data, param, code, plot, method, mfp) {

  #### Parameters loading and data preparing ####

  #Load parameters from param
  nlm <- as.numeric(param$nlm)
  nhm <- as.numeric(param$nhm)
  marital <- ifelse(as.numeric(param$cohabit)==0, 4, 7)
  t1Month <- as.numeric(param$t1Month)
  t2Month <- as.numeric(param$t2Month)
  nrj <- as.numeric(param$nrj)
  nRegion <- as.numeric(param$nRegion)
  edu <- as.numeric(param$edu)
  nRace <- as.numeric(param$nRace)
  nrate <- as.numeric(param$nRate)
  nWeight <- as.numeric(param$nWeight)


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

  #Set table and graph name surfix
  if(nRegion == 1 & nRace == 1 & nrj == 1 & edu == 0){
    combined.name <- ""
  } else {
    combined.name <- ", all"
  }

  #prepare oe & freq data
  #change wide data into long data (1 person month/row)
  data_oe <- data_freq <- NA
  cal_freq <- cal_oe <- F
  if (nrate==1){
    cal_freq <- cal_oe <- T
    data_oe <- data_freq <- data.prepare.mar(data, marital, t1Month, t2Month, nlm, 95)
  } else if (nrate==2){          #only oe
    cal_oe <- T
    data_oe <- data.prepare.mar(data, marital, t1Month, t2Month, nlm, 95)
  } else if (nrate==3){          #only freq
    cal_freq <- T
    data_freq <- data.prepare.mar(data, marital, t1Month, t2Month, nlm, 95)
  }


  #estimate by what? according to parameters
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

  #### 4 marital status results output ####

  if (marital == 4){

    #generate pop table: # of risk population and # of events
    if (cal_oe==T){
      pop <- pop.count.four.sex(data_oe, nlm, 95)
      write.pop.four.sex(pop, param, name = combined.name)
    }

    if (cal_freq==T){
      #if have not generated pop table in the previous step (oe), generate it.
      #if have already generated, omit.
      if (cal_oe==F){
        pop <- pop.count.four.sex(data_freq, nlm, 95)
        write.pop.four.sex(pop, param, name =  combined.name)
      }
    }

    #estimate overall rates
    combined <- compute.four(data_oe, data_freq, plot=plot, param=param, plot.name=combined.name, method, mfp)
    #output overall rates
    write.rates.four.sex(combined$oe.rates, combined$frequency, param, combined.name,  paste0(method, " estimate", sep=""))
    write.rates.four.sex(combined$raw.oe.rates, combined$raw.frequency, param, combined.name, "Direct calculate")

    #generate and output overall total rates and mean ages
    if (subset=="overall"){
      total.rates <- combined$total.rates
      mean.age <- combined$mean.age
      write.total.four.subset(total.rates, mean.age, param, combined.name)
    }

    #by rural/urban, region, race, edu
    if (subset %in% c("ru", "region", "race", "edu")){

      #overall total rates & mean ages
      total.rates <- combined$total.rates
      total.rates <- rbind(NA, total.rates)       #empty row for subset names
      mean.age <- combined$mean.age
      mean.age <- rbind(NA, mean.age)             #empty row for subset names

      #define row names
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

        data_oe.i <- data_freq.i <- NA

        if (cal_oe==T){
          #Subset data according to the variable
          data_oe.i <- data_oe[which(eval(parse(text=paste("data_oe$",subset,sep="")))==i),]

          pop <- pop.count.four.sex(data_oe.i, nlm, 95)
          write.pop.four.sex(pop, param, i.code)
        }

        if (cal_freq==T){

          #Subset data according to the variable
          data_freq.i <- data_freq[which(eval(parse(text=paste("data_freq$",subset,sep="")))==i),]

          if (cal_oe==F){
            pop <- pop.count.four.sex(data_freq.i, nlm, 95)
            write.pop.four.sex(pop, param, i.code)
          }

        }

        i <- compute.four(data_oe.i, data_freq.i, plot=plot, param=param, plot.name=i.code, method, mfp)
        write.rates.four.sex(i$oe.rates, i$frequency, param, i.code, paste0(method, " estimate", sep=""))
        write.rates.four.sex(i$raw.oe.rates, i$raw.frequency, param, i.code, "Direct calculate")

        rowname <- c(i.name, "Direct calculate",  paste0(method, " estimate", sep=""), "Difference%")
        i$total.rates <- rbind(NA, i$total.rates)
        i$total.rates <- cbind(rowname, i$total.rates)
        total.rates <- rbind(total.rates, i$total.rates)
        i$mean.age <- rbind(NA, i$mean.age)
        i$mean.age <- cbind(rowname, i$mean.age)
        mean.age <- rbind(mean.age, i$mean.age)

      }

      write.total.four.subset(total.rates, mean.age, param, paste0("-all ", subset, sep=""))

    }

    #by region and rural/urban
    if (subset=="reg.ru"){

      #overall total rates
      total.rates <- combined$total.rates
      total.rates <- rbind(NA, total.rates)
      mean.age <- combined$mean.age
      mean.age <- rbind(NA, mean.age)

      rowname <- c("Total rates - All region combined",
                   "Direct calculate",  paste0(method, " estimate", sep=""), "Difference%")
      total.rates <- cbind(rowname, total.rates)
      rowname <- c("Mean age - All region combined",
                   "Direct calculate",  paste0(method, " estimate", sep=""), "Difference%")
      mean.age <- cbind(rowname, mean.age)

      data_oe.i <- data_oe.ru <- data_oe.ub <- NA
      data_freq.i <- data_freq.ru <- data_freq.ub <- NA

      #for each region
      for(k in 1:length(region_list[[1]])){
        i <- region_list[[1]][k]
        i.name <- region_list[[2]][k]

        i.code <- paste0(", ", i.name, sep="")
        i.code1 <- paste0(", ", i.name, "-rural", sep="")
        i.code2 <- paste0(", ", i.name, "-urban", sep="")

        if (cal_oe==T){
          data_oe.i <- data_oe[which(data_oe$region==i),]
          data_oe.ru <- data_oe[which(data_oe$ru == 1&data_oe$region==i),]
          data_oe.ub <- data_oe[which(data_oe$ru == 2&data_oe$region==i),]

          pop.i <- pop.count.four.sex(data_oe.i, nlm, 95)
          pop.ru <- pop.count.four.sex(data_oe.ru, nlm, 95)
          pop.ub <- pop.count.four.sex(data_oe.ub, nlm, 95)

          write.pop.four.sex(pop.i, param, name = i.code)
          write.pop.four.sex(pop.ru, param, name = i.code1)
          write.pop.four.sex(pop.ub, param, name = i.code2)

        }

        if (cal_freq==T){
          data_freq.i <- data_freq[which(data_freq$region==i),]
          data_freq.ru <- data_freq[which(data_freq$ru == 1&data_freq$region==i),]
          data_freq.ub <- data_freq[which(data_freq$ru == 2&data_freq$region==i),]

          if (cal_oe==F){

            pop.i <- pop.count.four.sex(data_freq.i, nlm, 95)
            pop.ru <- pop.count.four.sex(data_freq.ru, nlm, 95)
            pop.ub <- pop.count.four.sex(data_freq.ub, nlm, 95)

            write.pop.four.sex(pop.i, param, name = i.code)
            write.pop.four.sex(pop.ru, param, name = i.code1)
            write.pop.four.sex(pop.ub, param, name = i.code2)

          }
        }

        region <- compute.four(data_oe.i, data_freq.i, plot=plot, param=param, plot.name=i.code, method, mfp)

        region_ru <- compute.four(data_oe.ru, data_freq.ru, plot=plot, param=param, plot.name=i.code1, method, mfp)
        region_ub <- compute.four(data_oe.ub, data_freq.ub, plot=plot, param=param, plot.name=i.code2, method, mfp)

        write.rates.four.sex(region_ru$oe.rates, region_ru$frequency, param, i.code1,  paste0(method, " estimate", sep=""))
        write.rates.four.sex(region_ru$raw.oe.rates, region_ru$raw.frequency, param, i.code1, "Direct calculate")

        write.rates.four.sex(region_ub$oe.rates, region_ub$frequency, param, i.code2,  paste0(method, " estimate", sep=""))
        write.rates.four.sex(region_ub$raw.oe.rates, region_ub$raw.frequency, param, i.code2, "Direct calculate")

        total.rates_re <- rbind(NA, region$total.rates, NA, region_ru$total.rates, NA, region_ub$total.rates)
        mean.age_re <- rbind(NA, region$mean.age, NA, region_ru$mean.age, NA, region_ub$mean.age)
        rowname <- c("Total rates - Rural/urban Combined",
                     "Direct calculate",  paste0(method, " estimate", sep=""), "Difference%",
                     "Rural", "Direct calculate",  paste0(method, " estimate", sep=""), "Difference%",
                     "Urban", "Direct calculate",  paste0(method, " estimate", sep=""), "Difference%")
        total.rates_re <- cbind(rowname, total.rates_re)
        total.rates <- rbind(total.rates, total.rates_re)
        rowname <- c("Mean age - Rural/urban Combined",
                     "Direct calculate",  paste0(method, " estimate", sep=""), "Difference%",
                     "Rural", "Direct calculate",  paste0(method, " estimate", sep=""), "Difference%",
                     "Urban", "Direct calculate",  paste0(method, " estimate", sep=""), "Difference%")
        mean.age_re <- cbind(rowname, mean.age_re)
        mean.age <- rbind(mean.age, mean.age_re)
      }
      write.total.four.subset(total.rates, mean.age, param, "-region by residence")
    }

  }

  #### 7 marital status results output ####

  if (marital==7) {

    #generate pop table: # of risk population and # of events
    if (cal_oe==T){
      pop <- pop.count.seven.sex(data_oe, nlm, 95)
      write.pop.seven.sex(pop, param, name = combined.name)
    }

    if (cal_freq==T){
      #if have not generated pop table in the previous step (oe), generate it.
      #if have already generated, omit.
      if (cal_oe==F){
        pop <- pop.count.seven.sex(data_freq, nlm, 95)
        write.pop.seven.sex(pop, param, name =  combined.name)
      }
    }

    #estimate overall rates
    combined <- compute.seven(data_oe, data_freq, plot=plot, param=param, plot.name=combined.name, method, mfp)
    #output overall rates
    write.rates.seven.sex(combined$male.oe, combined$female.oe, combined$male.freq, combined$female.freq,
                          param, combined.name,  paste0(method, " estimate", sep=""))
    write.rates.seven.sex(combined$raw.male.oe, combined$raw.female.oe, combined$raw.male.freq, combined$raw.female.freq,
                          param, combined.name, "Direct calculate")

    #generate and output overall total rates and mean ages
    if (subset=="overall"){
      total.rates <- combined$total.rates
      mean.age <- combined$mean.age
      write.total.seven.subset(total.rates, mean.age, param, combined.name)
    }

    #by rural/urban, region, race, edu
    if(subset %in% c("ru", "region", "race", "edu")){

      #overall total rates & mean ages
      total.rates <- combined$total.rates
      total.rates <- rbind(NA, total.rates)
      mean.age <- combined$mean.age
      mean.age <- rbind(NA, mean.age)

      #define row names
      rowname <- c("Total rates - All region combined",
                   "Direct calculate",  paste0(method, " estimate", sep=""), "Difference%")
      total.rates <- cbind(rowname, total.rates)
      rowname <- c("Mean age - All region combined",
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

        data_oe.i <- data_freq.i <- NA

        if (cal_oe==T){

          #Subset data according to the variable
          data_oe.i <- data_oe[which(eval(parse(text=paste("data_oe$",subset,sep="")))==i),]

          pop <- pop.count.seven.sex(data_oe.i, nlm, 95)
          write.pop.seven.sex(pop, param, i.code)

        }

        if (cal_freq==T){

          #Subset data according to the variable
          data_freq.i <- data_freq[which(eval(parse(text=paste("data_freq$",subset,sep="")))==i),]

          if (cal_oe==F){
            pop <- pop.count.seven.sex(data_freq.i, nlm, 95)
            write.pop.seven.sex(pop, param, i.code)
          }

        }

        i <- compute.seven(data_oe.i, data_freq.i, plot=plot, param=param, plot.name=i.code, method, mfp)
        write.rates.seven.sex(i$male.oe, i$female.oe, i$male.freq, i$female.freq, param, i.code,  paste0(method, " estimate", sep=""))
        write.rates.seven.sex(i$raw.male.oe, i$raw.female.oe, i$raw.male.freq, i$raw.female.freq, param, i.code, "Direct calculate")

        rowname <- c(i.name, "Direct calculate",  paste0(method, " estimate", sep=""), "Difference%")
        i$total.rates <- rbind(NA, i$total.rates)
        i$total.rates <- cbind(rowname, i$total.rates)
        total.rates <- rbind(total.rates, i$total.rates)
        i$mean.age <- rbind(NA, i$mean.age)
        i$mean.age <- cbind(rowname, i$mean.age)
        mean.age <- rbind(mean.age, i$mean.age)

      }

      write.total.seven.subset(total.rates, mean.age, param, paste0("-all ", subset, sep=""))

    }

    #by region and rural/urban
    if (subset=="reg.ru"){

      #overall total rates
      total.rates <- combined$total.rates
      total.rates <- rbind(NA, total.rates)
      mean.age <- combined$mean.age
      mean.age <- rbind(NA, mean.age)

      rowname <- c("Total rates - All region combined",
                   "Direct calculate",  paste0(method, " estimate", sep=""), "Difference%")
      total.rates <- cbind(rowname, total.rates)
      rowname <- c("Mean age - All region combined",
                   "Direct calculate",  paste0(method, " estimate", sep=""), "Difference%")
      mean.age <- cbind(rowname, mean.age)

      data_oe.i <- data_oe.ru <- data_oe.ub <- NA
      data_freq.i <- data_freq.ru <- data_freq.ub <- NA

      #for each region
      for(k in 1:length(region_list[[1]])){
        i <- region_list[[1]][k]
        i.name <- region_list[[2]][k]

        i.code <- paste0(", ", i.name, sep="")
        i.code1 <- paste0(", ", i.name, "-rural", sep="")
        i.code2 <- paste0(", ", i.name, "-urban", sep="")

        if (cal_oe==T){
          data_oe.i <- data_oe[which(data_oe$region==i),]
          data_oe.ru <- data_oe[which(data_oe$ru == 1&data_oe$region==i),]
          data_oe.ub <- data_oe[which(data_oe$ru == 2&data_oe$region==i),]

          pop.i <- pop.count.seven.sex(data_oe.i, nlm, 95)
          pop.ru <- pop.count.seven.sex(data_oe.ru, nlm, 95)
          pop.ub <- pop.count.seven.sex(data_oe.ub, nlm, 95)

          write.pop.seven.sex(pop.i, param, name = i.code)
          write.pop.seven.sex(pop.ru, param, name = i.code1)
          write.pop.seven.sex(pop.ub, param, name = i.code2)
        }

        if (cal_freq==T){
          data_freq.i <- data_freq[which(data_freq$region==i),]
          data_freq.ru <- data_freq[which(data_freq$ru == 1&data_freq$region==i),]
          data_freq.ub <- data_freq[which(data_freq$ru == 2&data_freq$region==i),]

          if (cal_oe==F){

            pop.i <- pop.count.seven.sex(data_freq.i, nlm, 95)
            pop.ru <- pop.count.seven.sex(data_freq.ru, nlm, 95)
            pop.ub <- pop.count.seven.sex(data_freq.ub, nlm, 95)

            write.pop.seven.sex(pop.i, param, name = i.code)
            write.pop.seven.sex(pop.ru, param, name = i.code1)
            write.pop.seven.sex(pop.ub, param, name = i.code2)

          }
        }

        region <- compute.seven(data_oe.i, data_freq.i, plot=plot, param=param, plot.name=i.code, method, mfp)

        region_ru <- compute.seven(data_oe.ru, data_freq.ru, plot=plot, param=param, plot.name=i.code1, method, mfp)
        region_ub <- compute.seven(data_oe.ub, data_freq.ub, plot=plot, param=param, plot.name=i.code2, method, mfp)

        write.rates.seven.sex(region_ru$oe.rates, region_ru$frequency, param, i.code1,  paste0(method, " estimate", sep=""))
        write.rates.seven.sex(region_ru$raw.oe.rates, region_ru$raw.frequency, param, i.code1, "Direct calculate")

        write.rates.seven.sex(region_ub$oe.rates, region_ub$frequency, param, i.code2,  paste0(method, " estimate", sep=""))
        write.rates.seven.sex(region_ub$raw.oe.rates, region_ub$raw.frequency, param, i.code2, "Direct calculate")

        total.rates_re <- rbind(NA, region$total.rates, NA, region_ru$total.rates, NA, region_ub$total.rates)
        mean.age_re <- rbind(NA, region$mean.age, NA, region_ru$mean.age, NA, region_ub$mean.age)
        rowname <- c("Total rates - Rural/urban Combined",
                     "Direct calculate",  paste0(method, " estimate", sep=""), "Difference%",
                     "Rural", "Direct calculate",  paste0(method, " estimate", sep=""), "Difference%",
                     "Urban", "Direct calculate",  paste0(method, " estimate", sep=""), "Difference%")
        total.rates_re <- cbind(rowname, total.rates_re)
        total.rates <- rbind(total.rates, total.rates_re)
        rowname <- c("Mean age - Rural/urban Combined",
                     "Direct calculate",  paste0(method, " estimate", sep=""), "Difference%",
                     "Rural", "Direct calculate",  paste0(method, " estimate", sep=""), "Difference%",
                     "Urban", "Direct calculate",  paste0(method, " estimate", sep=""), "Difference%")
        mean.age_re <- cbind(rowname, mean.age_re)
        mean.age <- rbind(mean.age, mean.age_re)
      }
      write.total.seven.subset(total.rates, mean.age, param, "-region by residence")
    }

  }
}

#### The main function for marriage estimation with covariant method ####
run.marriage.rates.covar <- function(data, param, code, plot, sex, mfp) {

  #### Parameters loading and data preparing ####

  #Load parameters from param
  nlm <- as.numeric(param$nlm)
  nhm <- as.numeric(param$nhm)
  marital <- ifelse(as.numeric(param$cohabit)==0, 4, 7)
  t1Month <- as.numeric(param$t1Month)
  t2Month <- as.numeric(param$t2Month)
  nrj <- as.numeric(param$nrj)
  edu <- as.numeric(param$edu)
  nRegion <- as.numeric(param$nRegion)
  nRace <- as.numeric(param$nRace)
  nrate <- as.numeric(param$nRate)
  nWeight <- as.numeric(param$nWeight)

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

  #Set table and graph name surfix
  if(nRegion == 1 & nRace == 1 & nrj == 1 & edu == 0){
    combined.name <- ""
  } else {
    combined.name <- ", all"
  }

  #prepare oe & freq data
  #change wide data into long data (1 person month/row)
  data_oe <- data_freq <- NA
  cal_freq <- cal_oe <- F
  if (nrate==1){
    cal_freq <- cal_oe <- T
    data_oe <- data_freq <- data.prepare.mar(data, marital, t1Month, t2Month, nlm, 95)
  } else if (nrate==2){          #only oe
    cal_oe <- T
    data_oe <- data.prepare.mar(data, marital, t1Month, t2Month, nlm, 95)
  } else if (nrate==3){          #only freq
    cal_freq <- T
    data_freq <- data.prepare.mar(data, marital, t1Month, t2Month, nlm, 95)
  }


  ## define "byvar" according to parameters
  #sex.nosex -- general estimation, not by any factor
  if (nRegion == 1 & nrj == 1 & nRace == 1 & edu == 0 & sex == F) {byvar<- "sex.nosex"}
  #byvar=NA, only by gender
  if (nRegion == 1 & nrj == 1 & nRace == 1 & edu == 0 & sex == T) {byvar<- "sex"}
  #region.nosex -- only by region
  if (nRegion > 1 & nRegion <= 100 & sex==F & nrj == 1 & nRace == 1 & edu == 0) {byvar<- "region.nosex"}
  #ru.nosex -- only by rural/urban
  if (nRegion == 1 & nrj == 2 & sex==F & nRace == 1 & edu == 0) {byvar<- "ru.nosex"}
  #race.nosex -- only by rural/urban
  if (nRegion == 1 & nrj == 1 & sex==F & nRace > 1 & edu == 0) {byvar<- "race.nosex"}
  #region -- by region & sex
  if (nRegion > 1 & nRegion <= 100 & sex==T & nrj == 1 & nRace == 1 & edu == 0) {byvar<- "region"}
  #ru -- by rural/urban & sex
  if (nRegion == 1 & nrj == 2 & sex==T & nRace == 1 & edu == 0) {byvar<- "ru"}
  #race -- by race & sex
  if (nRegion == 1 & nrj == 1 & nRace > 1 & sex == T & edu == 0) {byvar<- "race"}
  #edu -- by education & sex
  if (nRegion == 1 & nrj == 1 & nRace == 1 & sex == T & edu == 1) {byvar<- "edu"}
  #reg.ru.nosex -- by region & rural/urban
  if (nrj==2 & nRegion > 1 & nRegion <= 100 & sex==F & nRace == 1 & edu == 0) {byvar<- "reg.ru.nosex"}
  #reg.ru -- by region, rural/urban & sex
  if (nrj==2 & nRegion > 1 & nRegion <= 100 & sex==T & nRace == 1 & edu == 0) {byvar<- "reg.ru"}


  ## Common settings for all cases ##

  #set row names for final total rates output
  rowname <- c("Direct calculate", "Poisson estimate", "Difference%")

  #### 4 marital status results ####
  if(marital == 4){

    #estimated oe, raw oe, estimated freq, raw freq
    est.covar <- compute.four.covar(data_oe, data_freq, param, code, plot, combined.name, byvar, mfp)

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
      total.rates.ru <- data.frame(t(rep(NA, 10)))[F,]
      total.rates.ub <- data.frame(t(rep(NA, 10)))[F,]
      mean.age.ru <- data.frame(t(rep(NA, 10)))[F,]
      mean.age.ub <- data.frame(t(rep(NA, 10)))[F,]
    } else {
      # for all cases other than reg.ru & reg.ru.nosex
      total.rates <- data.frame(t(rep(NA, 8)))[F,]
      mean.age <- data.frame(t(rep(NA, 8)))[F,]
    }

    #Case sex.nosex & sex
    if (byvar=="sex.nosex"|byvar=="sex") {

      if (cal_oe==T){

        #generate pop table: # of risk population and # of events
        pop <- pop.count.four(data_oe, nlm, 95, sex)
        write.pop.four(pop, param, name = ", all", sex)

        #extract estimated oe and raw oe
        oe.rates <- est.covar$oe.rates %>% arrange(age)
        raw.oe.rates <- est.covar$raw.oe.rates %>% arrange(age)

      }

      if (cal_freq==T){

        #if have not generated pop table in the previous step (oe), generate it.
        #if have already generated, omit.
        if(cal_oe==F){
          pop <- pop.count.four(data_freq, nlm, 95, sex)
          write.pop.four(pop, param, name =  ", all", sex)
        }

        #extract estimated freq and raw freq
        frequency <- est.covar$frequency %>% arrange(age)
        raw.frequency <- est.covar$raw.frequency %>% arrange(age)

        #Calculate estimated total rates and mean age based on estimated freq.
        poi.freq <- freq.mar4(frequency, sex)
        frequency <- poi.freq$frequency
        poi.total.rates <- poi.freq$total.rates
        poi.mean.age <- poi.freq$mean.age

        #Calculate raw total rates and mean age based on raw freq.
        raw.freq <- freq.mar4(raw.frequency, sex)
        raw.frequency <- raw.freq$frequency
        raw.total.rates <- raw.freq$total.rates
        raw.mean.age <- raw.freq$mean.age

        #combine estimated and raw total rates and frequencies, and calculate difference
        total.rates <- rbind(raw.total.rates, poi.total.rates, (poi.total.rates-raw.total.rates)/raw.total.rates)
        mean.age <- rbind(raw.mean.age, poi.mean.age, (poi.mean.age-raw.mean.age)/raw.mean.age)

        #replace Inf or NaN rates into NA.
        total.rates <- do.call(data.frame, lapply(total.rates, function(x) replace(x, is.infinite(x), NA)))

        write.total.four(total.rates, mean.age, param, combined.name, sex)

      }

      #output estimated rates
      write.rates.four(oe.rates, frequency, param, combined.name, "Poisson estimate", sex)
      #output raw rates
      write.rates.four(raw.oe.rates, raw.frequency, param, combined.name, "Direct calculate", sex)

    }

    #Case one covariant other than sex
    if (byvar %in% c("region.nosex", "ru.nosex", "race.nosex", "region", "ru", "race", "edu")){

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

          pop <- pop.count.four(data_oe[which(eval(parse(text=paste("data_oe$",var,sep="")))==i),], nlm, 95, sex)
          write.pop.four(pop, param, i.code, sex)

          oe.rates <- est.covar$oe.rates
          oe.rates <- oe.rates[which(eval(parse(text = paste("oe.rates$",var,sep=""))) == i), -which(names(oe.rates)==var)] %>% arrange(age)
          raw.oe.rates <- est.covar$raw.oe.rates
          raw.oe.rates <- raw.oe.rates[which(eval(parse(text = paste("raw.oe.rates$",var,sep=""))) == i), -which(names(raw.oe.rates)==var)] %>% arrange(age)

        }

        if (cal_freq==T){

          if (cal_oe==F){

            pop <- pop.count.four(data_freq[which(eval(parse(text=paste("data_freq$",var,sep="")))==i),], nlm, 95, sex)
            write.pop.four(pop, param, i.code, sex)

          }

          frequency <- est.covar$frequency
          frequency <- frequency[which(eval(parse(text = paste("frequency$",var,sep=""))) == i), -which(names(frequency)==var)] %>% arrange(age)
          raw.frequency <- est.covar$raw.frequency
          raw.frequency <- raw.frequency[which(eval(parse(text = paste("raw.frequency$",var,sep=""))) == i), -which(names(raw.frequency)==var)] %>% arrange(age)

          #Calculate estimated total rates and mean age based on estimated freq.
          poi.freq <- freq.mar4(frequency, sex)
          frequency <- poi.freq$frequency
          poi.total.rates <- poi.freq$total.rates
          poi.mean.age <- poi.freq$mean.age

          #Calculate raw total rates and mean age based on raw freq.
          raw.freq <- freq.mar4(raw.frequency, sex)
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

        write.rates.four(oe.rates, frequency, param, i.code, "Poisson estimate", sex)
        write.rates.four(raw.oe.rates, raw.frequency, param, i.code, "Direct calculate", sex)

      }

      write.total.four(total.rates, mean.age, param, paste0("-by ", var, sep=""), sex)

    }

    #Case region.ru & region.ru.nosex
    if(byvar=="region.ru"|byvar=="region.ru.nosex"){

      for(k in 1:length(region_list[[1]])){
        i <- region_list[[1]][k]
        i.name <- region_list[[2]][k]
        i.code <- paste0(", ", i.name, sep="")

        if (cal_oe==T){
          pop <- pop.count.four(data_oe[which(data_oe$region==i&data_oe$ru==1),], nlm, 95, sex)
          write.pop.four(pop, param, paste0(i.code, "-rural"), sex)
          pop <- pop.count.four(data_oe[which(data_oe$region==i&data_oe$ru==2),], nlm, 95, sex)
          write.pop.four(pop, param, paste0(i.code, "-urban"), sex)

          oe.rates.ru <- est.covar$oe.rates %>% filter(region == i&ru==1) %>% select(-region, -ru) %>% arrange(age)
          oe.rates.ub <- est.covar$oe.rates %>% filter(region == i&ru==2) %>% select(-region, -ru) %>% arrange(age)
          raw.oe.rates.ru <- est.covar$raw.oe.rates %>% filter(region == i&ru==1) %>% select(-region, -ru) %>% arrange(age)
          raw.oe.rates.ub <- est.covar$raw.oe.rates %>% filter(region == i&ru==2) %>% select(-region, -ru) %>% arrange(age)

        }

        if (cal_freq==T){

          if (cal_oe==F){
            pop <- pop.count.four(data_freq[which(data_freq$region==i&data_freq$ru==1),], nlm, 95, sex)
            write.pop.four(pop, param, paste0(i.code, "-rural"), sex)
            pop <- pop.count.four(data_freq[which(data_freq$region==i&data_freq$ru==2),], nlm, 95, sex)
            write.pop.four(pop, param, paste0(i.code, "-urban"), sex)
          }

          frequency.ru <- est.covar$frequency %>% filter(region == i&ru==1) %>% select(-region, -ru) %>% arrange(age)
          frequency.ub <- est.covar$frequency %>% filter(region == i&ru==2) %>% select(-region, -ru) %>% arrange(age)
          raw.frequency.ru <- est.covar$raw.frequency %>% filter(region == i&ru==1) %>% select(-region, -ru) %>% arrange(age)
          raw.frequency.ub <- est.covar$raw.frequency %>% filter(region == i&ru==2) %>% select(-region, -ru) %>% arrange(age)

          #Calculate estimated rural total rates and mean age based on estimated freq.
          poi.freq.ru <- freq.mar4(frequency.ru, sex)
          frequency.ru <- poi.freq.ru$frequency
          poi.total.rates.ru <- poi.freq.ru$total.rates
          poi.mean.age.ru <- poi.freq.ru$mean.age

          #Calculate raw rural total rates and mean age based on raw freq.
          raw.freq.ru <- freq.mar4(raw.frequency.ru, sex)
          raw.frequency.ru <- raw.freq.ru$frequency
          raw.total.rates.ru <- raw.freq.ru$total.rates
          raw.mean.age.ru <- raw.freq.ru$mean.age

          #Calculate estimated urban total rates and mean age based on estimated freq.
          poi.freq.ub <- freq.mar4(frequency.ub, sex)
          frequency.ub <- poi.freq.ub$frequency
          poi.total.rates.ub <- poi.freq.ub$total.rates
          poi.mean.age.ub <- poi.freq.ub$mean.age

          #Calculate raw urban total rates and mean age based on raw freq.
          raw.freq.ub <- freq.mar4(raw.frequency.ub, sex)
          raw.frequency.ub <- raw.freq.ub$frequency
          raw.total.rates.ub <- raw.freq.ub$total.rates
          raw.mean.age.ub <- raw.freq.ub$mean.age

          i.total.rates.ru <- data.frame(i.name, raw.total.rates.ru[1], poi.total.rates.ru[1], (poi.total.rates.ru[1]-raw.total.rates.ru[1])/raw.total.rates.ru[1],
                                         raw.total.rates.ru[2], poi.total.rates.ru[2], (poi.total.rates.ru[2]-raw.total.rates.ru[2])/raw.total.rates.ru[2],
                                         raw.total.rates.ru[3], poi.total.rates.ru[3], (poi.total.rates.ru[3]-raw.total.rates.ru[3])/raw.total.rates.ru[3])
          i.total.rates.ru <- do.call(data.frame, lapply(i.total.rates.ru, function(x) replace(x, is.infinite(x), NA)))

          i.mean.age.ru <- data.frame(i.name, raw.mean.age.ru[1], poi.mean.age.ru[1], (poi.mean.age.ru[1]-raw.mean.age.ru[1])/raw.mean.age.ru[1],
                                      raw.mean.age.ru[2], poi.mean.age.ru[2], (poi.mean.age.ru[2]-raw.mean.age.ru[2])/raw.mean.age.ru[2],
                                      raw.mean.age.ru[3], poi.mean.age.ru[3], (poi.mean.age.ru[3]-raw.mean.age.ru[3])/raw.mean.age.ru[3])

          i.total.rates.ub <- data.frame(i.name, raw.total.rates.ub[1], poi.total.rates.ub[1], (poi.total.rates.ub[1]-raw.total.rates.ub[1])/raw.total.rates.ub[1],
                                         raw.total.rates.ub[2], poi.total.rates.ub[2], (poi.total.rates.ub[2]-raw.total.rates.ub[2])/raw.total.rates.ub[2],
                                         raw.total.rates.ub[3], poi.total.rates.ub[3], (poi.total.rates.ub[3]-raw.total.rates.ub[3])/raw.total.rates.ub[3])
          i.total.rates.ub <- do.call(data.frame, lapply(i.total.rates.ub, function(x) replace(x, is.infinite(x), NA)))

          i.mean.age.ub <- data.frame(i.name, raw.mean.age.ub[1], poi.mean.age.ub[1], (poi.mean.age.ub[1]-raw.mean.age.ub[1])/raw.mean.age.ub[1],
                                      raw.mean.age.ub[2], poi.mean.age.ub[2], (poi.mean.age.ub[2]-raw.mean.age.ub[2])/raw.mean.age.ub[2],
                                      raw.mean.age.ub[3], poi.mean.age.ub[3], (poi.mean.age.ub[3]-raw.mean.age.ub[3])/raw.mean.age.ub[3])

          total.rates.ru <- rbind(total.rates.ru, i.total.rates.ru)
          total.rates.ub <- rbind(total.rates.ub, i.total.rates.ub)
          mean.age.ru <- rbind(mean.age.ru, i.mean.age.ru)
          mean.age.ub <- rbind(mean.age.ub, i.mean.age.ub)

        }

        write.rates.four(oe.rates.ru, frequency.ru, param, paste0(i.code, "-rural"), "Poisson estimate", sex)
        write.rates.four(oe.rates.ub, frequency.ub, param, paste0(i.code, "-urban"), "Poisson estimate", sex)
        write.rates.four(raw.oe.rates.ru, raw.frequency.ru, param, paste0(i.code, "-rural"), "Direct calculate", sex)
        write.rates.four(raw.oe.rates.ub, raw.frequency.ub, param, paste0(i.code, "-urban"), "Direct calculate", sex)

      }

      write.total.four.covar(total.rates.ru, total.rates.ub, mean.age.ru, mean.age.ub, param, "-region by residence", sex)

    }

  }

  #### 7 marital status results ####

  if(marital == 7){

    #estimated oe, raw oe, estimated freq, raw freq
    est.covar <- compute.seven.covar(data_oe, data_freq, param, code, plot, plot.name=combined.name, byvar, mfp)

    ## Set rates as NA first, change later if cal_oe/cal_freq==T
    if (byvar=="reg.ru.nosex"){
      # for reg.ru.nosex only
      oe.rates.ru <- raw.oe.rates.ru <- frequency.ru <- raw.frequency.ru <- NA
      oe.rates.ub <- raw.oe.rates.ub <- frequency.ub <- raw.frequency.ub <- NA
    } else if (byvar=="reg.ru"){
      # for reg.ru only
      oe.rates.ru.m <- raw.oe.rates.ru.m <- frequency.ru.m <- raw.frequency.ru.m <- NA
      oe.rates.ub.m <- raw.oe.rates.ub.m <- frequency.ub.m <- raw.frequency.ub.m <- NA
      oe.rates.ru.f <- raw.oe.rates.ru.f <- frequency.ru.f <- raw.frequency.ru.f <- NA
      oe.rates.ub.f <- raw.oe.rates.ub.f <- frequency.ub.f <- raw.frequency.ub.f <- NA
    } else if (byvar=="region.nosex"|byvar=="ru.nosex"|byvar=="sex.nosex"|byvar=="race.nosex"){
      # for sex.nosex, region.nosex, race.nosex & ru.nosex
      oe.rates <- raw.oe.rates <- frequency <- raw.frequency <- NA
    } else {
      # for other cases
      oe.rates.m <- raw.oe.rates.m <- frequency.m <- raw.frequency.m <- NA
      oe.rates.f <- raw.oe.rates.f <- frequency.f <- raw.frequency.f <- NA
    }

    ## generate empty data frames to store total rates and mean ages
    if (byvar=="reg.ru"|byvar=="reg.ru.nosex"){
      # for reg.ru & reg.ru.nosex only
      total.rates.ru <- data.frame(t(rep(NA, 10)))[F,]
      total.rates.ub <- data.frame(t(rep(NA, 10)))[F,]
      mean.age.ru <- data.frame(t(rep(NA, 10)))[F,]
      mean.age.ub <- data.frame(t(rep(NA, 10)))[F,]
    } else if (byvar=="region.nosex"|byvar=="ru.nosex"|byvar=="race.nosex"){
      # for reg.nosex & ru.nosex & race.nosex
      total.rates <- data.frame(t(rep(NA, 7)))[F,]
      mean.age <- data.frame(t(rep(NA, 7)))[F,]
    } else {
      # for other cases
      total.rates <- data.frame(t(rep(NA, 12)))[F,]
      mean.age <- data.frame(t(rep(NA, 12)))[F,]
    }

    #Case sex.nosex
    if (byvar=="sex.nosex") {

      if (cal_oe==T){

        pop <- pop.count.seven.nosex(data_oe, nlm, 95)
        write.pop.seven.nosex(pop, param, name = ", all")

        oe.rates <- est.covar$oe.rates.m %>% arrange(age)
        raw.oe.rates <- est.covar$raw.oe.rates.m %>% arrange(age)

      }

      if (cal_freq==T){

        #if have not generated pop table in the previous step (oe), generate it.
        #if have already generated, omit.
        if(cal_oe==F){
          pop <- pop.count.seven.nosex(data_freq, nlm, 95)
          write.pop.seven.nosex(pop, param, name =  ", all")
        }

        #extract estimated freq and raw freq
        frequency <- est.covar$frequency.m %>% arrange(age)
        raw.frequency <- est.covar$raw.frequency.m %>% arrange(age)

        #Calculate estimated total rates and mean age based on estimated freq.
        poi.freq <- freq.mar7(frequency, sex)
        frequency <- poi.freq$frequency
        poi.total.rates <- poi.freq$total.rates
        poi.mean.age <- poi.freq$mean.age

        #Calculate raw total rates and mean age based on raw freq.
        raw.freq <- freq.mar7(raw.frequency, sex)
        raw.frequency <- raw.freq$frequency
        raw.total.rates <- raw.freq$total.rates
        raw.mean.age <- raw.freq$mean.age

        #combine estimated and raw total rates and frequencies, and calculate difference
        total.rates <- rbind(raw.total.rates, poi.total.rates, (poi.total.rates-raw.total.rates)/raw.total.rates)
        mean.age <- rbind(raw.mean.age, poi.mean.age, (poi.mean.age-raw.mean.age)/raw.mean.age)

        #replace Inf or NaN rates into NA.
        total.rates <- do.call(data.frame, lapply(total.rates, function(x) replace(x, is.infinite(x), NA)))

        write.total.seven.nosex(total.rates, mean.age, param, combined.name)

      }

      #output estimated rates
      write.rates.seven.nosex(oe.rates, frequency, param, combined.name, "Poisson estimate")
      #output raw rates
      write.rates.seven.nosex(raw.oe.rates, raw.frequency, param, combined.name, "Direct calculate")

    }

    #Case sex
    if (byvar=="sex") {

      if (cal_oe==T){

        pop <- pop.count.seven.sex(data_oe, nlm, 95)
        write.pop.seven.sex(pop, param, name = ", all")

        oe.rates.m <- est.covar$oe.rates.m %>% arrange(age)
        oe.rates.f <- est.covar$oe.rates.f %>% arrange(age)
        raw.oe.rates.m <- est.covar$raw.oe.rates.m %>% arrange(age)
        raw.oe.rates.f <- est.covar$raw.oe.rates.f %>% arrange(age)

      }

      if (cal_freq==T){

        #if have not generated pop table in the previous step (oe), generate it.
        #if have already generated, omit.
        if(cal_oe==F){
          pop <- pop.count.seven.sex(data_freq, nlm, 95)
          write.pop.seven.sex(pop, param, name =  ", all")
        }

        #extract estimated freq and raw freq
        frequency.m <- est.covar$frequency.m %>% arrange(age)
        frequency.f <- est.covar$frequency.f %>% arrange(age)
        raw.frequency.m <- est.covar$raw.frequency.m %>% arrange(age)
        raw.frequency.f <- est.covar$raw.frequency.f %>% arrange(age)

        #Calculate estimated total rates and mean age based on estimated freq.
        poi.freq <- freq.mar7(list(frequency.m, frequency.f), sex)
        frequency.m <- poi.freq$frequency[[1]]
        frequency.f <- poi.freq$frequency[[2]]
        poi.total.rates <- poi.freq$total.rates
        poi.mean.age <- poi.freq$mean.age

        #Calculate raw total rates and mean age based on raw freq.
        raw.freq <- freq.mar7(list(raw.frequency.m, raw.frequency.f), sex)
        raw.frequency.m <- raw.freq$frequency[[1]]
        raw.frequency.f <- raw.freq$frequency[[2]]
        raw.total.rates <- raw.freq$total.rates
        raw.mean.age <- raw.freq$mean.age

        #combine estimated and raw total rates and frequencies, and calculate difference
        total.rates <- rbind(raw.total.rates, poi.total.rates, (poi.total.rates-raw.total.rates)/raw.total.rates)
        mean.age <- rbind(raw.mean.age, poi.mean.age, (poi.mean.age-raw.mean.age)/raw.mean.age)

        #replace Inf or NaN rates into NA.
        total.rates <- do.call(data.frame, lapply(total.rates, function(x) replace(x, is.infinite(x), NA)))

        write.total.seven.sex(total.rates, mean.age, param, combined.name)

      }

      #output estimated rates
      write.rates.seven.sex(oe.rates.m, oe.rates.f, frequency.m, frequency.f, param, combined.name, "Poisson estimate")
      #output raw rates
      write.rates.seven.sex(raw.oe.rates.m, raw.oe.rates.f, raw.frequency.m, raw.frequency.f, param, combined.name, "Direct calculate")

    }

    #Case one covariant & sex
    if (byvar %in% c("region", "ru", "race", "edu")){
      #define code list & varable name
      var <- byvar
      if (byvar=="region"){
        i.list <- region_list
      } else if (byvar=="ru") {
        i.list <- list(code=c(1,2), name=c("rural", "urban"))
      } else if (byvar=="edu"){
        i.list <- list(code=c(1:5),
                       name=c("No education", "Primary school",
                              "Middle school", "High school", "College or higher"))
      } else if (byvar=="race"){
        i.list <- race_list
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

          pop <- pop.count.seven.sex(data_oe[which(eval(parse(text=paste("data_oe$",var,sep="")))==i),], nlm, 95)
          write.pop.seven.sex(pop, param, i.code)

          oe.rates.m <- est.covar$oe.rates.m
          oe.rates.m <- oe.rates.m[which(eval(parse(text = paste("oe.rates.m$",var,sep=""))) == i), -which(names(oe.rates.m)==var)] %>% arrange(age)
          oe.rates.f <- est.covar$oe.rates.f
          oe.rates.f <- oe.rates.f[which(eval(parse(text = paste("oe.rates.f$",var,sep=""))) == i), -which(names(oe.rates.f)==var)] %>% arrange(age)
          raw.oe.rates.m <- est.covar$raw.oe.rates.m
          raw.oe.rates.m <- raw.oe.rates.m[which(eval(parse(text = paste("raw.oe.rates.m$",var,sep=""))) == i), -which(names(raw.oe.rates.m)==var)] %>% arrange(age)
          raw.oe.rates.f <- est.covar$raw.oe.rates.f
          raw.oe.rates.f <- raw.oe.rates.f[which(eval(parse(text = paste("raw.oe.rates.f$",var,sep=""))) == i), -which(names(raw.oe.rates.f)==var)] %>% arrange(age)

        }

        if (cal_freq==T){

          if (cal_oe==F){

            pop <- pop.count.seven.sex(data_freq[which(eval(parse(text=paste("data_freq$",var,sep="")))==i),], nlm, 95)
            write.pop.seven.sex(pop, param, i.code)

          }

          frequency.m <- est.covar$frequency.m
          frequency.m <- frequency.m[which(eval(parse(text = paste("frequency.m$",var,sep=""))) == i), -which(names(frequency.m)==var)] %>% arrange(age)
          frequency.f <- est.covar$frequency.f
          frequency.f <- frequency.f[which(eval(parse(text = paste("frequency.f$",var,sep=""))) == i), -which(names(frequency.f)==var)] %>% arrange(age)
          raw.frequency.m <- est.covar$raw.frequency.m
          raw.frequency.m <- raw.frequency.m[which(eval(parse(text = paste("raw.frequency.m$",var,sep=""))) == i), -which(names(raw.frequency.m)==var)] %>% arrange(age)
          raw.frequency.f <- est.covar$raw.frequency.f
          raw.frequency.f <- raw.frequency.f[which(eval(parse(text = paste("raw.frequency.f$",var,sep=""))) == i), -which(names(raw.frequency.f)==var)] %>% arrange(age)

          #Calculate estimated total rates and mean age based on estimated freq.
          poi.freq <- freq.mar7(list(frequency.m, frequency.f), sex)
          frequency.m <- poi.freq$frequency[[1]]
          frequency.f <- poi.freq$frequency[[2]]
          poi.total.rates <- poi.freq$total.rates
          poi.mean.age <- poi.freq$mean.age

          #Calculate raw total rates and mean age based on raw freq.
          raw.freq <- freq.mar7(list(raw.frequency.m, raw.frequency.f), sex)
          raw.frequency.m <- raw.freq$frequency[[1]]
          raw.frequency.f <- raw.freq$frequency[[2]]
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

        write.rates.seven.sex(oe.rates.m, oe.rates.f, frequency.m, frequency.f, param, i.code, "Poisson estimate")
        write.rates.seven.sex(raw.oe.rates.m, raw.oe.rates.f, raw.frequency.m, raw.frequency.f, param, i.code, "Direct calculate")

      }

      write.total.seven.sex(total.rates, mean.age, param, paste0("-by ", var, sep=""))

    }

    #Case one covariant & no sex
    if (byvar=="region.nosex"|byvar=="ru.nosex"|byvar=="race.nosex"){

      #define code list & varable name
      if (byvar=="region.nosex"){
        i.list <- region_list
        var <- "region"
      } else if (byvar=="ru.nosex") {
        i.list <- list(code=c(1,2), name=c("rural", "urban"))
        var <- "ru"
      } else if (byvar=="race.nosex") {
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

          pop <- pop.count.seven.nosex(data_oe[which(eval(parse(text=paste("data_oe$",var,sep="")))==i),], nlm, 95)
          write.pop.seven.nosex(pop, param, i.code)

          oe.rates <- est.covar$oe.rates.m
          oe.rates <- oe.rates[which(eval(parse(text = paste("oe.rates$",var,sep=""))) == i), -which(names(oe.rates)==var)] %>% arrange(age)
          raw.oe.rates <- est.covar$raw.oe.rates.m
          raw.oe.rates <- raw.oe.rates[which(eval(parse(text = paste("raw.oe.rates$",var,sep=""))) == i), -which(names(raw.oe.rates)==var)] %>% arrange(age)

        }

        if (cal_freq==T){

          if (cal_oe==F){

            pop <- pop.count.seven.nosex(data_freq[which(eval(parse(text=paste("data_freq$",var,sep="")))==i),], nlm, 95)
            write.pop.seven.nosex(pop, param, i.code)

          }

          frequency <- est.covar$frequency.m
          frequency <- frequency[which(eval(parse(text = paste("frequency$",var,sep=""))) == i), -which(names(frequency)==var)] %>% arrange(age)
          raw.frequency <- est.covar$raw.frequency.m
          raw.frequency <- raw.frequency[which(eval(parse(text = paste("raw.frequency$",var,sep=""))) == i), -which(names(raw.frequency)==var)] %>% arrange(age)

          #Calculate estimated total rates and mean age based on estimated freq.
          poi.freq <- freq.mar7(frequency, sex)
          frequency <- poi.freq$frequency
          poi.total.rates <- poi.freq$total.rates
          poi.mean.age <- poi.freq$mean.age

          #Calculate raw total rates and mean age based on raw freq.
          raw.freq <- freq.mar7(raw.frequency, sex)
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

        write.rates.seven.nosex(oe.rates, frequency, param, i.code, "Poisson estimate")
        write.rates.seven.nosex(raw.oe.rates, raw.frequency, param, i.code, "Direct calculate")

      }

      write.total.seven.nosex(total.rates, mean.age, param, paste0("-by ", var, sep=""))

    }

    #Case region.ru & region.ru.nosex
    if (byvar=="region.ru.nosex"){

      for(k in 1:length(region_list[[1]])){
        i <- region_list[[1]][k]
        i.name <- region_list[[2]][k]
        i.code <- paste0(", ", i.name, sep="")

        if (cal_oe==T){
          pop <- pop.count.seven.nosex(data_oe[which(data_oe$region==i&data_oe$ru==1),], nlm, 95)
          write.pop.seven.nosex(pop, param, paste0(i.code, "-rural"))
          pop <- pop.count.seven.nosex(data_oe[which(data_oe$region==i&data_oe$ru==2),], nlm, 95)
          write.pop.seven.nosex(pop, param, paste0(i.code, "-urban"))

          oe.rates.ru <- est.covar$oe.rates.m %>% filter(region == i&ru==1) %>% select(-region, -ru) %>% arrange(age)
          oe.rates.ub <- est.covar$oe.rates.m %>% filter(region == i&ru==2) %>% select(-region, -ru) %>% arrange(age)
          raw.oe.rates.ru <- est.covar$raw.oe.rates.m %>% filter(region == i&ru==1) %>% select(-region, -ru) %>% arrange(age)
          raw.oe.rates.ub <- est.covar$raw.oe.rates.m %>% filter(region == i&ru==2) %>% select(-region, -ru) %>% arrange(age)

        }

        if (cal_freq==T){

          if (cal_oe==F){
            pop <- pop.count.seven.nosex(data_freq[which(data_freq$region==i&data_freq$ru==1),], nlm, 95)
            write.pop.seven.nosex(pop, param, paste0(i.code, "-rural"))
            pop <- pop.count.seven.nosex(data_freq[which(data_freq$region==i&data_freq$ru==2),], nlm, 95)
            write.pop.seven.nosex(pop, param, paste0(i.code, "-urban"))
          }

          frequency.ru <- est.covar$frequency.m %>% filter(region == i&ru==1) %>% select(-region, -ru) %>% arrange(age)
          frequency.ub <- est.covar$frequency.m %>% filter(region == i&ru==2) %>% select(-region, -ru) %>% arrange(age)
          raw.frequency.ru <- est.covar$raw.frequency.m %>% filter(region == i&ru==1) %>% select(-region, -ru) %>% arrange(age)
          raw.frequency.ub <- est.covar$raw.frequency.m %>% filter(region == i&ru==2) %>% select(-region, -ru) %>% arrange(age)

          #Calculate estimated rural total rates and mean age based on estimated freq.
          poi.freq.ru <- freq.mar7(frequency.ru, sex)
          frequency.ru <- poi.freq.ru$frequency
          poi.total.rates.ru <- poi.freq.ru$total.rates
          poi.mean.age.ru <- poi.freq.ru$mean.age

          #Calculate raw rural total rates and mean age based on raw freq.
          raw.freq.ru <- freq.mar7(raw.frequency.ru, sex)
          raw.frequency.ru <- raw.freq.ru$frequency
          raw.total.rates.ru <- raw.freq.ru$total.rates
          raw.mean.age.ru <- raw.freq.ru$mean.age

          #Calculate estimated urban total rates and mean age based on estimated freq.
          poi.freq.ub <- freq.mar7(frequency.ub, sex)
          frequency.ub <- poi.freq.ub$frequency
          poi.total.rates.ub <- poi.freq.ub$total.rates
          poi.mean.age.ub <- poi.freq.ub$mean.age

          #Calculate raw urban total rates and mean age based on raw freq.
          raw.freq.ub <- freq.mar7(raw.frequency.ub, sex)
          raw.frequency.ub <- raw.freq.ub$frequency
          raw.total.rates.ub <- raw.freq.ub$total.rates
          raw.mean.age.ub <- raw.freq.ub$mean.age

          i.total.rates.ru <- data.frame(i.name)
          i.mean.age.ru <- data.frame(i.name)
          i.total.rates.ub <- data.frame(i.name)
          i.mean.age.ub <- data.frame(i.name)

          for (j in 1:5) {

            i.total.rates.ru <- cbind(i.total.rates.ru, data.frame(raw.total.rates.ru[j], poi.total.rates.ru[j], (poi.total.rates.ru[j]-raw.total.rates.ru[j])/raw.total.rates.ru[j]))
            i.total.rates.ru <- do.call(data.frame, lapply(i.total.rates.ru, function(x) replace(x, is.infinite(x), NA)))

            i.mean.age.ru <- cbind(i.mean.age.ru, data.frame(raw.mean.age.ru[j], poi.mean.age.ru[j], (poi.mean.age.ru[j]-raw.mean.age.ru[j])/raw.mean.age.ru[j]))

            i.total.rates.ub <- cbind(i.total.rates.ub, data.frame(raw.total.rates.ub[j], poi.total.rates.ub[j], (poi.total.rates.ub[j]-raw.total.rates.ub[j])/raw.total.rates.ub[j]))
            i.total.rates.ub <- do.call(data.frame, lapply(i.total.rates.ub, function(x) replace(x, is.infinite(x), NA)))

            i.mean.age.ub <- cbind(i.mean.age.ub, data.frame(raw.mean.age.ub[j], poi.mean.age.ub[j], (poi.mean.age.ub[j]-raw.mean.age.ub[j])/raw.mean.age.ub[j]))

          }

          total.rates.ru <- rbind(total.rates.ru, i.total.rates.ru)
          total.rates.ub <- rbind(total.rates.ub, i.total.rates.ub)
          mean.age.ru <- rbind(mean.age.ru, i.mean.age.ru)
          mean.age.ub <- rbind(mean.age.ub, i.mean.age.ub)

        }

        write.rates.seven.nosex(oe.rates.ru, frequency.ru, param, paste0(i.code, "-rural"), "Poisson estimate")
        write.rates.seven.nosex(oe.rates.ub, frequency.ub, param, paste0(i.code, "-urban"), "Poisson estimate")
        write.rates.seven.nosex(raw.oe.rates.ru, raw.frequency.ru, param, paste0(i.code, "-rural"), "Direct calculate")
        write.rates.seven.nosex(raw.oe.rates.ub, raw.frequency.ub, param, paste0(i.code, "-urban"), "Direct calculate")

      }

      write.total.seven.covar.nosex(total.rates.ru, total.rates.ub, mean.age.ru, mean.age.ub, param, "-region by residence")

    }

    if (byvar=="region.ru"){

      for(k in 1:length(region_list[[1]])){

        i <- region_list[[1]][k]
        i.name <- region_list[[2]][k]
        i.code <- paste0(", ", i.name, sep="")

        if (cal_oe==T){
          pop <- pop.count.seven.sex(data_oe[which(data_oe$region==i&data_oe$ru==1),], nlm, 95)
          write.pop.seven.sex(pop, param, paste0(i.code, "-rural"))
          pop <- pop.count.seven.sex(data_oe[which(data_oe$region==i&data_oe$ru==2),], nlm, 95)
          write.pop.seven.sex(pop, param, paste0(i.code, "-urban"))

          oe.rates.ru.m <- reg.ru$oe.rates.m %>% filter(region == i&ru==1) %>% select(-region, -ru) %>% arrange(age)
          oe.rates.ub.m <- reg.ru$oe.rates.m %>% filter(region == i&ru==2) %>% select(-region, -ru) %>% arrange(age)
          raw.oe.rates.ru.m <- reg.ru$raw.oe.rates.m %>% filter(region == i&ru==1) %>% select(-region, -ru) %>% arrange(age)
          raw.oe.rates.ub.m <- reg.ru$raw.oe.rates.m %>% filter(region == i&ru==2) %>% select(-region, -ru) %>% arrange(age)
          oe.rates.ru.f <- reg.ru$oe.rates.f %>% filter(region == i&ru==1) %>% select(-region, -ru) %>% arrange(age)
          oe.rates.ub.f <- reg.ru$oe.rates.f %>% filter(region == i&ru==2) %>% select(-region, -ru) %>% arrange(age)
          raw.oe.rates.ru.f <- reg.ru$raw.oe.rates.f %>% filter(region == i&ru==1) %>% select(-region, -ru) %>% arrange(age)
          raw.oe.rates.ub.f <- reg.ru$raw.oe.rates.f %>% filter(region == i&ru==2) %>% select(-region, -ru) %>% arrange(age)

        }

        if (cal_freq==T){

          if (cal_oe==F){
            pop <- pop.count.seven.sex(data_freq[which(data_freq$region==i&data_freq$ru==1),], nlm, 95)
            write.pop.seven.sex(pop, param, paste0(i.code, "-rural"))
            pop. <- pop.count.seven.sex(data_freq[which(data_freq$region==i&data_freq$ru==2),], nlm, 95)
            write.pop.seven.sex(pop, param, paste0(i.code, "-urban"))
          }

          frequency.ru.m <- reg.ru$frequency.m %>% filter(region == i&ru==1) %>% select(-region, -ru) %>% arrange(age)
          frequency.ub.m <- reg.ru$frequency.m %>% filter(region == i&ru==2) %>% select(-region, -ru) %>% arrange(age)
          raw.frequency.ru.m <- reg.ru$raw.frequency.m %>% filter(region == i&ru==1) %>% select(-region, -ru) %>% arrange(age)
          raw.frequency.ub.m <- reg.ru$raw.frequency.m %>% filter(region == i&ru==2) %>% select(-region, -ru) %>% arrange(age)
          frequency.ru.f <- reg.ru$frequency.f %>% filter(region == i&ru==1) %>% select(-region, -ru) %>% arrange(age)
          frequency.ub.f <- reg.ru$frequency.f %>% filter(region == i&ru==2) %>% select(-region, -ru) %>% arrange(age)
          raw.frequency.ru.f <- reg.ru$raw.frequency.f %>% filter(region == i&ru==1) %>% select(-region, -ru) %>% arrange(age)
          raw.frequency.ub.f <- reg.ru$raw.frequency.f %>% filter(region == i&ru==2) %>% select(-region, -ru) %>% arrange(age)

          #Calculate estimated rural total rates and mean age based on estimated freq.
          poi.freq.ru <- freq.mar7(list(frequency.ru.m, frequency.ru.f), sex)
          frequency.ru.m <- poi.freq.ru$frequency[[1]]
          frequency.ru.f <- poi.freq.ru$frequency[[2]]
          poi.total.rates.ru <- poi.freq.ru$total.rates
          poi.mean.age.ru <- poi.freq.ru$mean.age

          #Calculate raw rural total rates and mean age based on raw freq.
          raw.freq.ru <- freq.mar7(list(raw.frequency.ru.m, raw.frequency.ru.f), sex)
          raw.frequency.ru.m <- raw.freq.ru$frequency[[1]]
          raw.frequency.ru.f <- raw.freq.ru$frequency[[2]]
          raw.total.rates.ru <- raw.freq.ru$total.rates
          raw.mean.age.ru <- raw.freq.ru$mean.age

          #Calculate estimated urban total rates and mean age based on estimated freq.
          poi.freq.ub <- freq.mar7(list(frequency.ub.m, frequency.ub.f), sex)
          frequency.ub.m <- poi.freq.ub$frequency[[1]]
          frequency.ub.f <- poi.freq.ub$frequency[[2]]
          poi.total.rates.ub <- poi.freq.ub$total.rates
          poi.mean.age.ub <- poi.freq.ub$mean.age

          #Calculate raw urban total rates and mean age based on raw freq.
          raw.freq.ub <- freq.mar7(list(raw.frequency.ub.m, raw.frequency.ub.f), sex)
          raw.frequency.ub.m <- raw.freq.ub$frequency[[1]]
          raw.frequency.ub.f <- raw.freq.ub$frequency[[2]]
          raw.total.rates.ub <- raw.freq.ub$total.rates
          raw.mean.age.ub <- raw.freq.ub$mean.age

          i.total.rates.ru <- data.frame(i.name)
          i.total.rates.ub <- data.frame(i.name)
          i.mean.age.ru <- data.frame(i.name)
          i.mean.age.ub <- data.frame(i.name)

          for (j in 1:10){
            i.total.rates.ru <- cbind(i.total.rates.ru, data.frame(raw.total.rates.ru[j], poi.total.rates.ru[j], (poi.total.rates.ru[j]-raw.total.rates.ru[j])/raw.total.rates.ru[j]))
            i.total.rates.ub <- cbind(i.total.rates.ub, data.frame(raw.total.rates.ub[j], poi.total.rates.ub[j], (poi.total.rates.ub[j]-raw.total.rates.ub[j])/raw.total.rates.ub[j]))
            i.mean.age.ru <- cbind(i.mean.age.ru, data.frame(raw.mean.age.ru[j], poi.mean.age.ru[j], (poi.mean.age.ru[j]-raw.mean.age.ru[j])/raw.mean.age.ru[j]))
            i.mean.age.ub <- cbind(i.mean.age.ub, data.frame(raw.mean.age.ub[j], poi.mean.age.ub[j], (poi.mean.age.ub[j]-raw.mean.age.ub[j])/raw.mean.age.ub[j]))
          }

          i.total.rates.ru <- do.call(data.frame, lapply(i.total.rates.ru, function(x) replace(x, is.infinite(x), NA)))
          i.total.rates.ub <- do.call(data.frame, lapply(i.total.rates.ub, function(x) replace(x, is.infinite(x), NA)))

          total.rates.ru <- rbind(total.rates.ru, i.total.rates.ru)
          total.rates.ub <- rbind(total.rates.ub, i.total.rates.ub)
          mean.age.ru <- rbind(mean.age.ru, i.mean.age.ru)
          mean.age.ub <- rbind(mean.age.ub, i.mean.age.ub)

        }

        write.rates.seven.sex(oe.rates.ru.m, oe.rates.ru.f, frequency.ru.m, frequency.ru.f, param, paste0(r.code, "-rural"), "Poisson estimate")
        write.rates.seven.sex(oe.rates.ub.m, oe.rates.ub.f, frequency.ub.m, frequency.ub.f, param, paste0(r.code, "-urban"), "Poisson estimate")
        write.rates.seven.sex(raw.oe.rates.ru.m, raw.oe.rates.ru.f, raw.frequency.ru.m, raw.frequency.ru.f, param, paste0(r.code, "-rural"), "Direct calculate")
        write.rates.seven.sex(raw.oe.rates.ub.m, raw.oe.rates.ub.f, raw.frequency.ub.m, raw.frequency.ub.f, param, paste0(r.code, "-urban"), "Direct calculate")

      }

      write.total.seven.covar.sex(total.rates.ru, total.rates.ub, mean.age.ru, mean.age.ub, param, "-region by residence")

    }

  }

}

#### Estimation function for 4 marital status ####

##subset method
compute.four <- function(data_oe, data_freq, param, plot, plot.name, method, mfp){

  nrate <- as.numeric(param$nRate)
  nlm <- as.numeric(param$nlm)
  nhm <- as.numeric(param$nhm)
  agelist <- data.frame(age=seq(nlm, 95, 1))
  title <- as.character(param$title)
  t1Month <- as.numeric(param$t1Month)
  t2Month <- as.numeric(param$t2Month)
  nWeight <- as.numeric(param$nWeight)
  period <- paste0((t1Month-1)%/%12+1900, " - ",(t2Month-1)%/%12+1900, sep="")

  cal_oe <- (nrate==1 | nrate==2)
  cal_freq <- (nrate==1 | nrate==3)

  mrg1.male.oe <- divorced.male.oe <- rw.male.oe <- rd.male.oe <- NA
  mrg1.male.freq <- divorced.male.freq <- rw.male.freq <- rd.male.freq <- NA
  mrg1.female.oe <- divorced.female.oe <- rw.female.oe <- rd.female.oe <- NA
  mrg1.female.freq <- divorced.female.freq <- rw.female.freq <- rd.female.freq <- NA

  if(cal_oe == TRUE){
    male <- subset(data_oe, sex == 1)
    female <- subset(data_oe, sex == 2)

    #direct calculate
    # First Marriage
    mrg1.male.oe <- merge(agelist, oe.raw(male, nlm, nhm, 1, 1, nWeight), all.x=TRUE)
    mrg1.female.oe <- merge(agelist, oe.raw(female, nlm, nhm, 1, 1, nWeight), all.x=TRUE)
    # Divorced
    divorced.male.oe <- merge(agelist, oe.raw(male, nlm, 95, 2, 2, nWeight), all.x=TRUE)
    divorced.female.oe <- merge(agelist, oe.raw(female, nlm, 95, 2, 2, nWeight), all.x=TRUE)
    # Remarriage of Widowed
    rw.male.oe <- merge(agelist, oe.raw(male, nlm, 95, 3, 3, nWeight), all.x=TRUE)
    rw.female.oe <- merge(agelist, oe.raw(female, nlm, 95, 3, 3, nWeight), all.x=TRUE)
    # Remarriage of Divorced
    rd.male.oe <- merge(agelist, oe.raw(male, nlm, 95, 4, 4, nWeight), all.x=TRUE)
    rd.female.oe <- merge(agelist, oe.raw(female, nlm, 95, 4, 4, nWeight), all.x=TRUE)

    if (!is.na(method)){

      if (method=="Poisson") {

        # First Marriage
        mrg1.male.oe.est <- merge(agelist, oe.poi(male, nlm, nhm, 1, 1, nWeight, mfp), all.x=TRUE)
        mrg1.female.oe.est <- merge(agelist, oe.poi(female, nlm, nhm, 1, 1, nWeight, mfp), all.x=TRUE)
        # Divorced
        divorced.male.oe.est <- merge(agelist, oe.poi(male, nlm, 95, 2, 2, nWeight, mfp), all.x=TRUE)
        divorced.female.oe.est <- merge(agelist, oe.poi(female, nlm, 95, 2, 2, nWeight, mfp), all.x=TRUE)
        # Remarriage of Widowed
        rw.male.oe.est <- merge(agelist, oe.poi(male, nlm, 95, 3, 3, nWeight, mfp), all.x=TRUE)
        rw.female.oe.est <- merge(agelist, oe.poi(female, nlm, 95, 3, 3, nWeight, mfp), all.x=TRUE)
        # Remarriage of Divorced
        rd.male.oe.est <- merge(agelist, oe.poi(male, nlm, 95, 4, 4, nWeight, mfp), all.x=TRUE)
        rd.female.oe.est <- merge(agelist, oe.poi(female, nlm, 95, 4, 4, nWeight, mfp), all.x=TRUE)

      }

      # if (method=="Bayes.norm") {
      #
      #   # First Marriage
      #   mrg1.male.oe.est <- merge(agelist, est.bnorm(mrg1.male.oe), all.x=TRUE)
      #   mrg1.female.oe.est <- merge(agelist, est.bnorm(mrg1.female.oe), all.x=TRUE)
      #   # Divorced
      #   divorced.male.oe.est <- merge(agelist, est.bnorm(divorced.male.oe), all.x=TRUE)
      #   divorced.female.oe.est <- merge(agelist, est.bnorm(divorced.female.oe), all.x=TRUE)
      #   # Remarriage of Widowed
      #   rw.male.oe.est <- merge(agelist, est.bnorm(rw.male.oe), all.x=TRUE)
      #   rw.female.oe.est <- merge(agelist, est.bnorm(rw.female.oe), all.x=TRUE)
      #   # Remarriage of Divorced
      #   rd.male.oe.est <- merge(agelist, est.bnorm(rd.male.oe), all.x=TRUE)
      #   rd.female.oe.est <- merge(agelist, est.bnorm(rd.female.oe), all.x=TRUE)
      #
      # }
      #
      # if (method=="Bayes.log") {
      #
      #   # First Marriage
      #   mrg1.male.oe.est <- merge(agelist, est.blog(mrg1.male.oe), all.x=TRUE)
      #   mrg1.female.oe.est <- merge(agelist, est.blog(mrg1.female.oe), all.x=TRUE)
      #   # Divorced
      #   divorced.male.oe.est <- merge(agelist, est.blog(divorced.male.oe), all.x=TRUE)
      #   divorced.female.oe.est <- merge(agelist, est.blog(divorced.female.oe), all.x=TRUE)
      #   # Remarriage of Widowed
      #   rw.male.oe.est <- merge(agelist, est.blog(rw.male.oe), all.x=TRUE)
      #   rw.female.oe.est <- merge(agelist, est.blog(rw.female.oe), all.x=TRUE)
      #   # Remarriage of Divorced
      #   rd.male.oe.est <- merge(agelist, est.blog(rd.male.oe), all.x=TRUE)
      #   rd.female.oe.est <- merge(agelist, est.blog(rd.female.oe), all.x=TRUE)
      #
      # }

      #merge with direct calculate
      mrg1.male.oe <- merge(mrg1.male.oe, mrg1.male.oe.est, all.x=TRUE)
      mrg1.female.oe <- merge(mrg1.female.oe, mrg1.female.oe.est, all.x=TRUE)
      divorced.male.oe <- merge(divorced.male.oe, divorced.male.oe.est, all.x=TRUE)
      divorced.female.oe <- merge(divorced.female.oe, divorced.female.oe.est, all.x=TRUE)
      rw.male.oe <- merge(rw.male.oe, rw.male.oe.est, all.x=TRUE)
      rw.female.oe <- merge(rw.female.oe, rw.female.oe.est, all.x=TRUE)
      rd.male.oe <- merge(rd.male.oe, rd.male.oe.est, all.x=TRUE)
      rd.female.oe <- merge(rd.female.oe, rd.female.oe.est, all.x=TRUE)

    }

  }

  if(cal_freq == TRUE){
    male <- subset(data_freq, sex == 1)
    female <- subset(data_freq, sex == 2)

    #direct calculate
    # First Marriage
    mrg1.male.freq <- merge(agelist, freq.raw(male, nlm, nhm, 1, nWeight), all.x=TRUE)
    mrg1.female.freq <- merge(agelist, freq.raw(female, nlm, nhm, 1, nWeight), all.x=TRUE)
    # Divorced
    divorced.male.freq <- merge(agelist, freq.raw(male, nlm, 95, 2, nWeight), all.x=TRUE)
    divorced.female.freq <- merge(agelist, freq.raw(female, nlm, 95, 2, nWeight), all.x=TRUE)
    # Remarriage of Widowed
    rw.male.freq <- merge(agelist, freq.raw(male, nlm, 95, 3, nWeight), all.x=TRUE)
    rw.female.freq <- merge(agelist, freq.raw(female, nlm, 95, 3, nWeight), all.x=TRUE)
    # Remarriage of Divorced
    rd.male.freq <- merge(agelist, freq.raw(male, nlm, 95, 4, nWeight), all.x=TRUE)
    rd.female.freq <- merge(agelist, freq.raw(female, nlm, 95, 4, nWeight), all.x=TRUE)

    if (!is.na(method)){

      if (method=="Poisson"){

        # First Marriage
        mrg1.male.freq.est <- merge(agelist, freq.poi(male, nlm, nhm, 1, nWeight, mfp), all.x=TRUE)
        mrg1.female.freq.est <- merge(agelist, freq.poi(female, nlm, nhm, 1, nWeight, mfp), all.x=TRUE)
        # Divorced
        divorced.male.freq.est <- merge(agelist, freq.poi(male, nlm, 95, 2, nWeight, mfp), all.x=TRUE)
        divorced.female.freq.est <- merge(agelist, freq.poi(female, nlm, 95, 2, nWeight, mfp), all.x=TRUE)
        # Remarriage of Widowed
        rw.male.freq.est <- merge(agelist, freq.poi(male, nlm, 95, 3, nWeight, mfp), all.x=TRUE)
        rw.female.freq.est <- merge(agelist, freq.poi(female, nlm, 95, 3, nWeight, mfp), all.x=TRUE)
        # Remarriage of Divorced
        rd.male.freq.est <- merge(agelist, freq.poi(male, nlm, 95, 4, nWeight, mfp), all.x=TRUE)
        rd.female.freq.est <- merge(agelist, freq.poi(female, nlm, 95, 4, nWeight, mfp), all.x=TRUE)

      }

      # if (method=="Bayes.norm"){
      #
      #   # First Marriage
      #   mrg1.male.freq.est <- merge(agelist, est.bnorm(mrg1.male.freq), all.x=TRUE)
      #   mrg1.female.freq.est <- merge(agelist, est.bnorm(mrg1.female.freq), all.x=TRUE)
      #   # Divorced
      #   divorced.male.freq.est <- merge(agelist, est.bnorm(divorced.male.freq), all.x=TRUE)
      #   divorced.female.freq.est <- merge(agelist, est.bnorm(divorced.female.freq), all.x=TRUE)
      #   # Remarriage of Widowed
      #   rw.male.freq.est <- merge(agelist, est.bnorm(rw.male.freq), all.x=TRUE)
      #   rw.female.freq.est <- merge(agelist, est.bnorm(rw.female.freq), all.x=TRUE)
      #   # Remarriage of Divorced
      #   rd.male.freq.est <- merge(agelist, est.bnorm(rd.male.freq), all.x=TRUE)
      #   rd.female.freq.est <- merge(agelist, est.bnorm(rd.female.freq), all.x=TRUE)
      #
      # }
      #
      # if (method=="Bayes.log"){
      #
      #   # First Marriage
      #   mrg1.male.freq.est <- merge(agelist, est.blog(mrg1.male.freq), all.x=TRUE)
      #   mrg1.female.freq.est <- merge(agelist, est.blog(mrg1.female.freq), all.x=TRUE)
      #   # Divorced
      #   divorced.male.freq.est <- merge(agelist, est.blog(divorced.male.freq), all.x=TRUE)
      #   divorced.female.freq.est <- merge(agelist, est.blog(divorced.female.freq), all.x=TRUE)
      #   # Remarriage of Widowed
      #   rw.male.freq.est <- merge(agelist, est.blog(rw.male.freq), all.x=TRUE)
      #   rw.female.freq.est <- merge(agelist, est.blog(rw.female.freq), all.x=TRUE)
      #   # Remarriage of Divorced
      #   rd.male.freq.est <- merge(agelist, est.blog(rd.male.freq), all.x=TRUE)
      #   rd.female.freq.est <- merge(agelist, est.blog(rd.female.freq), all.x=TRUE)
      #
      # }

      #merge with direct calculate
      mrg1.male.freq <- merge(mrg1.male.freq, mrg1.male.freq.est, all.x=TRUE)
      mrg1.female.freq <- merge(mrg1.female.freq, mrg1.female.freq.est, all.x=TRUE)
      divorced.male.freq <- merge(divorced.male.freq, divorced.male.freq.est, all.x=TRUE)
      divorced.female.freq <- merge(divorced.female.freq, divorced.female.freq.est, all.x=TRUE)
      rw.male.freq <- merge(rw.male.freq, rw.male.freq.est, all.x=TRUE)
      rw.female.freq <- merge(rw.female.freq, rw.female.freq.est, all.x=TRUE)
      rd.male.freq <- merge(rd.male.freq, rd.male.freq.est, all.x=TRUE)
      rd.female.freq <- merge(rd.female.freq, rd.female.freq.est, all.x=TRUE)

    }

  }

  if(plot == TRUE){

    wb <- createWorkbook()
    k<-1

    if(cal_oe == TRUE){
      # oe
      addWorksheet(wb, "oe")
      writeData(wb, sheet=k, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
      writeData(wb, sheet=k, paste0("Period: ", period, sep=""), startRow=2, startCol=1)
      k<-k+1

      row.index <- 3
      p1 <- rates.plot(mrg1.male.oe, nlm, nhm, "o/e rate", paste0("Figure 5.1. First marriage o/e rates, males", plot.name, sep=""))
      print(p1)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
      p2 <- rates.plot(mrg1.female.oe, nlm, nhm, "o/e rate", paste0("Figure 5.2. First marriage o/e rates, females", plot.name, sep=""))
      print(p2)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

      row.index <- row.index + 22
      p5 <- rates.plot(divorced.male.oe, nlm, 95, "o/e rate", paste0("Figure 6.1. Divorced o/e rates, males", plot.name, sep=""))
      print(p5)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
      p6 <- rates.plot(divorced.female.oe, nlm, 95, "o/e rate", paste0("Figure 6.2. Divorced o/e rates, female", plot.name, sep=""))
      print(p6)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

      row.index <- row.index + 22
      p9 <- rates.plot(rw.male.oe, nlm, 95, "o/e rate", paste0("Figure 7.1. Widowed to remarriage o/e rates, males", plot.name, sep=""))
      print(p9)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
      p10 <- rates.plot(rw.female.oe, nlm, 95, "o/e rate", paste0("Figure 7.2. Widowed to remarriage o/e rates, females", plot.name, sep=""))
      print(p10)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

      row.index <- row.index + 22
      p13 <- rates.plot(rd.male.oe, nlm, 95, "o/e rate", paste0("Figure 8.1. Divorced to remarriage o/e rates, males", plot.name, sep=""))
      print(p13)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
      p14 <- rates.plot(rd.female.oe, nlm, 95, "o/e rate", paste0("Figure 8.2. Divorced to remarriage o/e rates, females", plot.name, sep=""))
      print(p14)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")
    }

    if(cal_freq == TRUE){
      # fre
      addWorksheet(wb, "freq")

      writeData(wb, sheet=k, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
      writeData(wb, sheet=k, paste0("Period: ", period, sep=""), startRow=2, startCol=1)

      row.index <- 3
      p3 <- rates.plot(mrg1.male.freq, nlm, nhm, "frequency", paste0("Figure 1.1. First marriage frequencies, males", plot.name, sep=""))
      print(p3)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
      p4 <- rates.plot(mrg1.female.freq, nlm, nhm, "frequency", paste0("Figure 1.2. First marriage frequencies, females", plot.name, sep=""))
      print(p4)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

      row.index <- row.index + 22
      p7 <- rates.plot(divorced.male.freq, nlm, 95, "frequency", paste0("Figure 2.1. Divorced frequencies, males", plot.name, sep=""))
      print(p7)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
      p8 <- rates.plot(divorced.female.freq, nlm, 95, "frequency", paste0("Figure 2.2. Divorced frequencies, females", plot.name, sep=""))
      print(p8)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

      row.index <- row.index + 22
      p11 <- rates.plot(rw.male.freq, nlm, 95, "frequency", paste0("Figure 3.1. Widowed to remarriage frequencies, males", plot.name, sep=""))
      print(p11)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
      p12 <- rates.plot(rw.female.freq, nlm, 95, "frequency", paste0("Figure 3.2. Widowed to remarriage frequencies, females", plot.name, sep=""))
      print(p12)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

      row.index <- row.index + 22
      p15 <- rates.plot(rd.male.freq, nlm, 95, "frequency", paste0("Figure 4.1. Divorced to remarriage frequencies, males", plot.name, sep=""))
      print(p15)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
      p16 <- rates.plot(rd.female.freq, nlm, 95, "frequency", paste0("Figure 4.2. Divorced to remarriage frequencies, females", plot.name, sep=""))
      print(p16)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")
    }
    output.dir <- getwd()
    plot.name <- gsub(", ", "-", plot.name)
    saveWorkbook(wb, paste0(output.dir, "/", title, " Mar4 plot", plot.name, ".xlsx", sep=""), overwrite=TRUE)
  }

  if(cal_oe == TRUE){

    if (!is.na(method)){
      oe.rates <- data.frame(age=agelist$age, male.mrg1=mrg1.male.oe$est.rates, male.divorced=divorced.male.oe$est.rates,
                             male.remarriage.widowed=rw.male.oe$est.rates, male.remarriage.divorced=rd.male.oe$est.rates,
                             female.mrg1=mrg1.female.oe$est.rates, female.divorced=divorced.female.oe$est.rates,
                             female.remarriage.widowed=rw.female.oe$est.rates, female.remarriage.divorced=rd.female.oe$est.rates)
    } else {
      oe.rates <- NA
    }

    raw.oe.rates <- data.frame(age=agelist$age, male.mrg1=mrg1.male.oe$raw.rates, male.divorced=divorced.male.oe$raw.rates,
                               male.remarriage.widowed=rw.male.oe$raw.rates, male.remarriage.divorced=rd.male.oe$raw.rates,
                               female.mrg1=mrg1.female.oe$raw.rates, female.divorced=divorced.female.oe$raw.rates,
                               female.remarriage.widowed=rw.female.oe$raw.rates, female.remarriage.divorced=rd.female.oe$raw.rates)

  } else {
    oe.rates <- NA
    raw.oe.rates <- NA
  }


  if(cal_freq == TRUE){

    raw.frequency <- data.frame(age=agelist$age, male.mrg1=mrg1.male.freq$raw.rates, male.divorced=divorced.male.freq$raw.rates,
                                male.remarriage.widowed=rw.male.freq$raw.rates, male.remarriage.divorced=rd.male.freq$raw.rates,
                                female.mrg1=mrg1.female.freq$raw.rates, female.divorced=divorced.female.freq$raw.rates,
                                female.remarriage.widowed=rw.female.freq$raw.rates, female.remarriage.divorced=rd.female.freq$raw.rates)

    #Calculate raw total rates and mean age based on raw freq.
    raw.freq <- freq.mar4(raw.frequency, T)
    raw.frequency <- raw.freq$frequency
    raw.total.rates <- raw.freq$total.rates
    raw.mean.age <- raw.freq$mean.age

    if (!is.na(method)){

      frequency <- data.frame(age=agelist$age, male.mrg1=mrg1.male.freq$est.rates, male.divorced=divorced.male.freq$est.rates,
                              male.remarriage.widowed=rw.male.freq$est.rates, male.remarriage.divorced=rd.male.freq$est.rates,
                              female.mrg1=mrg1.female.freq$est.rates, female.divorced=divorced.female.freq$est.rates,
                              female.remarriage.widowed=rw.female.freq$est.rates, female.remarriage.divorced=rd.female.freq$est.rates)

      #Calculate estimated total rates and mean age based on estimated freq.
      est.freq <- freq.mar4(frequency, T)
      frequency <- est.freq$frequency
      est.total.rates <- est.freq$total.rates
      est.mean.age <- est.freq$mean.age

      total.rates <- rbind(raw.total.rates, est.total.rates, (est.total.rates-raw.total.rates)/raw.total.rates)
      mean.age <- rbind(raw.mean.age, est.mean.age, (est.mean.age-raw.mean.age)/raw.mean.age)
      total.rates <- do.call(data.frame, lapply(total.rates, function(x) replace(x, is.infinite(x), NA)))

    } else {
      frequency <- NA
      total.rates <- NA
      mean.age <- NA
    }

  } else {
    frequency <- NA
    raw.frequency <- NA
    total.rates <- NA
    mean.age <- NA
  }

  return(list(oe.rates=oe.rates, frequency=frequency,
              raw.oe.rates = raw.oe.rates, raw.frequency = raw.frequency,
              total.rates = total.rates, mean.age = mean.age))
}

##covariate method
compute.four.covar <- function(data_oe, data_freq, param, code, plot, plot.name, byvar, mfp){

  nrate <- as.numeric(param$nRate)
  nlm <- as.numeric(param$nlm)
  nhm <- as.numeric(param$nhm)
  title <- as.character(param$title)
  t1Month <- as.numeric(param$t1Month)
  t2Month <- as.numeric(param$t2Month)
  nWeight <- as.numeric(param$nWeight)
  period <- paste0((t1Month-1)%/%12+1900, " - ",(t2Month-1)%/%12+1900, sep="")
  nRegion <- as.numeric(param$nRegion)
  nRace <- as.numeric(param$nRace)

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

  mrg1.oe <- divorced.oe <- rw.oe <- rd.oe <- NA
  mrg1.freq <- divorced.freq <- rw.freq <- rd.freq <- NA
  oe.rates <- raw.oe.rates <- frequency <- raw.frequency <- NA

  if(cal_oe == TRUE){
    agelist <- null.rates(data_oe, nlm, 95, byvar)

    #only set nhm for first marriage, all other events -> 95
    mrg1.oe <- merge(agelist, oe.est.byvar(data_oe, nlm, nhm, 1, 1, byvar, nWeight, mfp), all.x=TRUE)
    divorced.oe <- merge(agelist, oe.est.byvar(data_oe, nlm, 95, 2, 2, byvar, nWeight, mfp), all.x=TRUE)
    rw.oe <- merge(agelist, oe.est.byvar(data_oe, nlm, 95, 3, 3, byvar, nWeight, mfp), all.x=TRUE)
    rd.oe <- merge(agelist, oe.est.byvar(data_oe, nlm, 95, 4, 4, byvar, nWeight, mfp), all.x=TRUE)
  }

  if(cal_freq == TRUE){
    agelist <- null.rates(data_freq, nlm, 95, byvar)

    #only set nhm for first marriage, all other events -> 95
    mrg1.freq <- merge(agelist, freq.est.byvar(data_freq, nlm, nhm, 1, byvar, nWeight, mfp), all.x=TRUE)
    divorced.freq <- merge(agelist, freq.est.byvar(data_freq, nlm, 95, 2, byvar, nWeight, mfp), all.x=TRUE)
    rw.freq <- merge(agelist, freq.est.byvar(data_freq, nlm, 95, 3, byvar, nWeight, mfp), all.x=TRUE)
    rd.freq <- merge(agelist, freq.est.byvar(data_freq, nlm, 95, 4, byvar, nWeight, mfp), all.x=TRUE)
  }

  if(plot == TRUE){
    draw.mar4.nRate(mrg1.oe, mrg1.freq, divorced.oe, divorced.freq, rw.oe,
                    rw.freq, rd.oe, rd.freq, code, nlm, 95, title, period,
                    plot.name, byvar, cal_oe, cal_freq)
  }

  if (byvar!="reg.ru.nosex"&byvar!="region.nosex"&byvar!="sex.nosex"&byvar!="ru.nosex") {
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
    }

    if(cal_oe == TRUE){
      oe.rates1 <- data.frame(rates.by,
                              mrg1=mrg1.oe[which(mrg1.oe$sex==1), "est.rates"],
                              divorced=divorced.oe[which(divorced.oe$sex==1), "est.rates"],
                              remarriage.widowed=rw.oe[which(rw.oe$sex==1), "est.rates"],
                              remarriage.divorced=rd.oe[which(rd.oe$sex==1), "est.rates"])
      oe.rates2 <- data.frame(rates.by,
                              mrg1=mrg1.oe[which(mrg1.oe$sex==2), "est.rates"],
                              divorced=divorced.oe[which(divorced.oe$sex==2), "est.rates"],
                              remarriage.widowed=rw.oe[which(rw.oe$sex==2), "est.rates"],
                              remarriage.divorced=rd.oe[which(rd.oe$sex==2), "est.rates"])
      raw.oe.rates1 <- data.frame(rates.by,
                                  mrg1=mrg1.oe[which(mrg1.oe$sex==1), "raw.rates"],
                                  divorced=divorced.oe[which(divorced.oe$sex==1), "raw.rates"],
                                  remarriage.widowed=rw.oe[which(rw.oe$sex==1), "raw.rates"],
                                  remarriage.divorced=rd.oe[which(rd.oe$sex==1), "raw.rates"])
      raw.oe.rates2 <- data.frame(rates.by,
                                  mrg1=mrg1.oe[which(mrg1.oe$sex==2), "raw.rates"],
                                  divorced=divorced.oe[which(divorced.oe$sex==2), "raw.rates"],
                                  remarriage.widowed=rw.oe[which(rw.oe$sex==2), "raw.rates"],
                                  remarriage.divorced=rd.oe[which(rd.oe$sex==2), "raw.rates"])

      oe.rates <- merge(oe.rates1, oe.rates2, by=merge.by)
      raw.oe.rates <- merge(raw.oe.rates1, raw.oe.rates2, by=merge.by)

    }

    if(cal_freq == TRUE){
      frequency1 <- data.frame(rates.by,
                               mrg1.male=mrg1.freq[which(mrg1.freq$sex==1), "est.rates"],
                               divorced.male=divorced.freq[which(divorced.freq$sex==1), "est.rates"],
                               remarriage.widowed.male=rw.freq[which(rw.freq$sex==1), "est.rates"],
                               remarriage.divorced.male=rd.freq[which(rd.freq$sex==1), "est.rates"])
      frequency2 <- data.frame(rates.by,
                               mrg1.female=mrg1.freq[which(mrg1.freq$sex==2), "est.rates"],
                               divorced.female=divorced.freq[which(divorced.freq$sex==2), "est.rates"],
                               remarriage.widowed.female=rw.freq[which(rw.freq$sex==2), "est.rates"],
                               remarriage.divorced.female=rd.freq[which(rd.freq$sex==2), "est.rates"])
      raw.frequency1 <- data.frame(rates.by,
                                   mrg1.male=mrg1.freq[which(mrg1.freq$sex==1), "raw.rates"],
                                   divorced.male=divorced.freq[which(divorced.freq$sex==1), "raw.rates"],
                                   remarriage.widowed.male=rw.freq[which(rw.freq$sex==1), "raw.rates"],
                                   remarriage.divorced.male=rd.freq[which(rd.freq$sex==1), "raw.rates"])
      raw.frequency2 <- data.frame(rates.by,
                                   mrg1.female=mrg1.freq[which(mrg1.freq$sex==2), "raw.rates"],
                                   divorced.female=divorced.freq[which(divorced.freq$sex==2), "raw.rates"],
                                   remarriage.widowed.female=rw.freq[which(rw.freq$sex==2), "raw.rates"],
                                   remarriage.divorced.female=rd.freq[which(rd.freq$sex==2), "raw.rates"])

      frequency <- merge(frequency1, frequency2, by=merge.by)
      raw.frequency <- merge(raw.frequency1, raw.frequency2, by=merge.by)

    }

  } else if (byvar=="reg.ru.nosex"){

    agelist <- agelist %>% arrange(age, region, ru)
    rates.by <- data.frame(age=agelist$age, region=agelist$region, ru=agelist$ru)

    if(cal_oe == TRUE){
      oe.rates <- data.frame(rates.by, mrg1=mrg1.oe[, "est.rates"],
                             divorced=divorced.oe[, "est.rates"],
                             remarriage.widowed=rw.oe[, "est.rates"],
                             remarriage.divorced=rd.oe[, "est.rates"])
      raw.oe.rates <- data.frame(rates.by, mrg1=mrg1.oe[, "raw.rates"],
                                 divorced=divorced.oe[, "raw.rates"],
                                 remarriage.widowed=rw.oe[, "raw.rates"],
                                 remarriage.divorced=rd.oe[, "raw.rates"])
    }

    if(cal_freq == TRUE){
      frequency <- data.frame(rates.by, mrg1=mrg1.freq[, "est.rates"],
                              divorced=divorced.freq[, "est.rates"],
                              remarriage.widowed=rw.freq[, "est.rates"],
                              remarriage.divorced=rd.freq[, "est.rates"])
      raw.frequency <- data.frame(rates.by, mrg1=mrg1.freq[, "raw.rates"],
                                  divorced=divorced.freq[, "raw.rates"],
                                  remarriage.widowed=rw.freq[, "raw.rates"],
                                  remarriage.divorced=rd.freq[, "raw.rates"])
    }

  } else if (byvar=="region.nosex"){

    agelist <- agelist %>% arrange(age, region)
    rates.by <- data.frame(age=agelist$age, region=agelist$region)

    if(cal_oe == TRUE){
      oe.rates <- data.frame(rates.by, mrg1=mrg1.oe[, "est.rates"],
                             divorced=divorced.oe[, "est.rates"],
                             remarriage.widowed=rw.oe[, "est.rates"],
                             remarriage.divorced=rd.oe[, "est.rates"])
      raw.oe.rates <- data.frame(rates.by, mrg1=mrg1.oe[, "raw.rates"],
                                 divorced=divorced.oe[, "raw.rates"],
                                 remarriage.widowed=rw.oe[, "raw.rates"],
                                 remarriage.divorced=rd.oe[, "raw.rates"])
    }

    if(cal_freq == TRUE){
      frequency <- data.frame(rates.by, mrg1=mrg1.freq[, "est.rates"],
                              divorced=divorced.freq[, "est.rates"],
                              remarriage.widowed=rw.freq[, "est.rates"],
                              remarriage.divorced=rd.freq[, "est.rates"])
      raw.frequency <- data.frame(rates.by, mrg1=mrg1.freq[, "raw.rates"],
                                  divorced=divorced.freq[, "raw.rates"],
                                  remarriage.widowed=rw.freq[, "raw.rates"],
                                  remarriage.divorced=rd.freq[, "raw.rates"])
    }

  }  else if (byvar=="ru.nosex"){

    agelist <- agelist %>% arrange(age, ru)
    rates.by <- data.frame(age=agelist$age, ru=agelist$ru)

    if(cal_oe == TRUE){
      oe.rates <- data.frame(rates.by, mrg1=mrg1.oe[, "est.rates"],
                             divorced=divorced.oe[, "est.rates"],
                             remarriage.widowed=rw.oe[, "est.rates"],
                             remarriage.divorced=rd.oe[, "est.rates"])
      raw.oe.rates <- data.frame(rates.by, mrg1=mrg1.oe[, "raw.rates"],
                                 divorced=divorced.oe[, "raw.rates"],
                                 remarriage.widowed=rw.oe[, "raw.rates"],
                                 remarriage.divorced=rd.oe[, "raw.rates"])
    }

    if(cal_freq == TRUE){
      frequency <- data.frame(rates.by, mrg1=mrg1.freq[, "est.rates"],
                              divorced=divorced.freq[, "est.rates"],
                              remarriage.widowed=rw.freq[, "est.rates"],
                              remarriage.divorced=rd.freq[, "est.rates"])
      raw.frequency <- data.frame(rates.by, mrg1=mrg1.freq[, "raw.rates"],
                                  divorced=divorced.freq[, "raw.rates"],
                                  remarriage.widowed=rw.freq[, "raw.rates"],
                                  remarriage.divorced=rd.freq[, "raw.rates"])
    }

  } else if (byvar=="sex.nosex"){

    agelist <- agelist %>% arrange(age)
    rates.by <- data.frame(age=agelist$age)

    if(cal_oe == TRUE){
      oe.rates <- data.frame(rates.by, mrg1=mrg1.oe[, "est.rates"],
                             divorced=divorced.oe[, "est.rates"],
                             remarriage.widowed=rw.oe[, "est.rates"],
                             remarriage.divorced=rd.oe[, "est.rates"])
      raw.oe.rates <- data.frame(rates.by, mrg1=mrg1.oe[, "raw.rates"],
                                 divorced=divorced.oe[, "raw.rates"],
                                 remarriage.widowed=rw.oe[, "raw.rates"],
                                 remarriage.divorced=rd.oe[, "raw.rates"])
    }

    if(cal_freq == TRUE){
      frequency <- data.frame(rates.by, mrg1=mrg1.freq[, "est.rates"],
                              divorced=divorced.freq[, "est.rates"],
                              remarriage.widowed=rw.freq[, "est.rates"],
                              remarriage.divorced=rd.freq[, "est.rates"])
      raw.frequency <- data.frame(rates.by, mrg1=mrg1.freq[, "raw.rates"],
                                  divorced=divorced.freq[, "raw.rates"],
                                  remarriage.widowed=rw.freq[, "raw.rates"],
                                  remarriage.divorced=rd.freq[, "raw.rates"])
    }
  }

  return(list(oe.rates=oe.rates, frequency=frequency,
              raw.oe.rates = raw.oe.rates, raw.frequency = raw.frequency))
}

#### Estimation function for 7 marital status ####

#subset method
compute.seven <- function(data_oe, data_freq, param, plot, plot.name, method, mfp){

  nrate <- as.numeric(param$nRate)
  nlm <- as.numeric(param$nlm)
  nhm <- as.numeric(param$nhm)
  agelist <- data.frame(age=seq(nlm, 95, 1))
  title <- as.character(param$title)
  t1Month <- as.numeric(param$t1Month)
  t2Month <- as.numeric(param$t2Month)
  nWeight <- as.numeric(param$nWeight)
  period <- paste0((t1Month-1)%/%12+1900, " - ",(t2Month-1)%/%12+1900, sep="")

  cal_oe <- (nrate==1 | nrate==2)
  cal_freq <- (nrate==1 | nrate==3)

  if(cal_oe == TRUE){
    male <- subset(data_oe, sex == 1)
    female <- subset(data_oe, sex == 2)

    ##Direct calculate

    # never married & not cohabiting to married
    mrg1.male.oe <- merge(agelist, oe.raw(male, nlm, nhm, 1, 1, nWeight), all.x=TRUE)
    mrg1.female.oe <- merge(agelist, oe.raw(female, nlm, nhm, 1, 1, nWeight), all.x=TRUE)

    # never married & cohabiting to married
    nmCm.male.oe <- merge(agelist, oe.raw(male, nlm, nhm, 5, 11, nWeight), all.x=TRUE)
    nmCm.female.oe <- merge(agelist, oe.raw(female, nlm, nhm, 5, 11, nWeight), all.x=TRUE)

    # Divorced
    divorced.male.oe <- merge(agelist, oe.raw(male, nlm, 95, 2, 2, nWeight), all.x=TRUE)
    divorced.female.oe <- merge(agelist, oe.raw(female, nlm, 95, 2, 2, nWeight), all.x=TRUE)

    # Remarriage of Widowed & not cohabiting
    rw.male.oe <- merge(agelist, oe.raw(male, nlm, 95, 3, 3, nWeight), all.x=TRUE)
    rw.female.oe <- merge(agelist, oe.raw(female, nlm, 95, 3, 3, nWeight), all.x=TRUE)

    # remarriage of widowed & cohabiting
    rwC.male.oe <- merge(agelist, oe.raw(male, nlm, 95, 7, 12, nWeight), all.x=TRUE)
    rwC.female.oe <- merge(agelist, oe.raw(female, nlm, 95, 7, 12, nWeight), all.x=TRUE)

    # Remarriage of Divorced & not cohabiting
    rd.male.oe <- merge(agelist, oe.raw(male, nlm, 95, 4, 4, nWeight), all.x=TRUE)
    rd.female.oe <- merge(agelist, oe.raw(female, nlm, 95, 4, 4, nWeight), all.x=TRUE)

    # remarriage of divorced & cohabiting
    rdC.male.oe <- merge(agelist, oe.raw(male, nlm, 95, 8, 13, nWeight), all.x=TRUE)
    rdC.female.oe <- merge(agelist, oe.raw(female, nlm, 95, 8, 13, nWeight), all.x=TRUE)

    # never married to cohabiting
    nmtoC.male.oe <- merge(agelist, oe.raw(male, nlm, 95, 1, 5, nWeight), all.x=TRUE)
    nmtoC.female.oe <- merge(agelist,oe.raw(female, nlm, 95, 1, 5, nWeight), all.x=TRUE)

    # widowed to cohabiting
    wtoC.male.oe <- merge(agelist, oe.raw(male, nlm, 95, 3, 6, nWeight), all.x=TRUE)
    wtoC.female.oe <- merge(agelist, oe.raw(female, nlm, 95, 3, 6, nWeight), all.x=TRUE)

    # divorced to cohabiting
    dtoC.male.oe <- merge(agelist, oe.raw(male, nlm, 95, 4, 7, nWeight), all.x=TRUE)
    dtoC.female.oe <- merge(agelist, oe.raw(female, nlm, 95, 4, 7, nWeight), all.x=TRUE)

    # cohabiting to never married
    Ctonm.male.oe <- merge(agelist, oe.raw(male, nlm, 95, 5, 8, nWeight), all.x=TRUE)
    Ctonm.female.oe <- merge(agelist, oe.raw(female, nlm, 95, 5, 8, nWeight), all.x=TRUE)

    # cohabiting to widowed
    Ctow.male.oe <- merge(agelist, oe.raw(female, nlm, 95, 7, 9, nWeight), all.x=TRUE)
    Ctow.female.oe <- merge(agelist, oe.raw(female, nlm, 95, 7, 9, nWeight), all.x=TRUE)

    # cohabiting to divorced
    Ctod.male.oe <- merge(agelist, oe.raw(female, nlm, 95, 8, 10, nWeight), all.x=TRUE)
    Ctod.female.oe <- merge(agelist, oe.raw(female, nlm, 95, 8, 10, nWeight), all.x=TRUE)

    if (!is.na(method)){

      if (method=="Poisson"){

        # never married & not cohabiting to married
        mrg1.male.oe.est <- merge(agelist, oe.poi(male, nlm, nhm, 1, 1, nWeight, mfp), all.x=TRUE)
        mrg1.female.oe.est <- merge(agelist, oe.poi(female, nlm, nhm, 1, 1, nWeight, mfp), all.x=TRUE)

        # never married & cohabiting to married
        nmCm.male.oe.est <- merge(agelist, oe.poi(male, nlm, nhm, 5, 11, nWeight, mfp), all.x=TRUE)
        nmCm.female.oe.est <- merge(agelist, oe.poi(female, nlm, nhm, 5, 11, nWeight, mfp), all.x=TRUE)

        # Divorced
        divorced.male.oe.est <- merge(agelist, oe.poi(male, nlm, 95, 2, 2, nWeight, mfp), all.x=TRUE)
        divorced.female.oe.est <- merge(agelist, oe.poi(female, nlm, 95, 2, 2, nWeight, mfp), all.x=TRUE)

        # Remarriage of Widowed & not cohabiting
        rw.male.oe.est <- merge(agelist, oe.poi(male, nlm, 95, 3, 3, nWeight, mfp), all.x=TRUE)
        rw.female.oe.est <- merge(agelist, oe.poi(female, nlm, 95, 3, 3, nWeight, mfp), all.x=TRUE)

        # remarriage of widowed & cohabiting
        rwC.male.oe.est <- merge(agelist, oe.poi(male, nlm, 95, 7, 12, nWeight, mfp), all.x=TRUE)
        rwC.female.oe.est <- merge(agelist, oe.poi(female, nlm, 95, 7, 12, nWeight, mfp), all.x=TRUE)

        # Remarriage of Divorced & not cohabiting
        rd.male.oe.est <- merge(agelist, oe.poi(male, nlm, 95, 4, 4, nWeight, mfp), all.x=TRUE)
        rd.female.oe.est <- merge(agelist, oe.poi(female, nlm, 95, 4, 4, nWeight, mfp), all.x=TRUE)

        # remarriage of divorced & cohabiting
        rdC.male.oe.est <- merge(agelist, oe.poi(male, nlm, 95, 8, 13, nWeight, mfp), all.x=TRUE)
        rdC.female.oe.est <- merge(agelist, oe.poi(female, nlm, 95, 8, 13, nWeight, mfp), all.x=TRUE)

        # never married to cohabiting
        nmtoC.male.oe.est <- merge(agelist, oe.poi(male, nlm, 95, 1, 5, nWeight, mfp), all.x=TRUE)
        nmtoC.female.oe.est <- merge(agelist,oe.poi(female, nlm, 95, 1, 5, nWeight, mfp), all.x=TRUE)

        # widowed to cohabiting
        wtoC.male.oe.est <- merge(agelist, oe.poi(male, nlm, 95, 3, 6, nWeight, mfp), all.x=TRUE)
        wtoC.female.oe.est <- merge(agelist, oe.poi(female, nlm, 95, 3, 6, nWeight, mfp), all.x=TRUE)

        # divorced to cohabiting
        dtoC.male.oe.est <- merge(agelist, oe.poi(male, nlm, 95, 4, 7, nWeight, mfp), all.x=TRUE)
        dtoC.female.oe.est <- merge(agelist, oe.poi(female, nlm, 95, 4, 7, nWeight, mfp), all.x=TRUE)

        # cohabiting to never married
        Ctonm.male.oe.est <- merge(agelist, oe.poi(male, nlm, 95, 5, 8, nWeight, mfp), all.x=TRUE)
        Ctonm.female.oe.est <- merge(agelist, oe.poi(female, nlm, 95, 5, 8, nWeight, mfp), all.x=TRUE)

        # cohabiting to widowed
        Ctow.male.oe.est <- merge(agelist, oe.poi(female, nlm, 95, 7, 9, nWeight, mfp), all.x=TRUE)
        Ctow.female.oe.est <- merge(agelist, oe.poi(female, nlm, 95, 7, 9, nWeight, mfp), all.x=TRUE)

        # cohabiting to divorced
        Ctod.male.oe.est <- merge(agelist, oe.poi(female, nlm, 95, 8, 10, nWeight, mfp), all.x=TRUE)
        Ctod.female.oe.est <- merge(agelist, oe.poi(female, nlm, 95, 8, 10, nWeight, mfp), all.x=TRUE)

      }

      # if (method=="Bayes.norm"){
      #
      #   # never married & not cohabiting to married
      #   mrg1.male.oe.est <- merge(agelist, est.bnorm(mrg1.male.oe), all.x=TRUE)
      #   mrg1.female.oe.est <- merge(agelist, est.bnorm(mrg1.female.oe), all.x=TRUE)
      #
      #   # never married & cohabiting to married
      #   nmCm.male.oe.est <- merge(agelist, est.bnorm(nmCm.male.oe), all.x=TRUE)
      #   nmCm.female.oe.est <- merge(agelist, est.bnorm(nmCm.female.oe), all.x=TRUE)
      #
      #   # Divorced
      #   divorced.male.oe.est <- merge(agelist, est.bnorm(divorced.male.oe), all.x=TRUE)
      #   divorced.female.oe.est <- merge(agelist, est.bnorm(divorced.female.oe), all.x=TRUE)
      #
      #   # Remarriage of Widowed & not cohabiting
      #   rw.male.oe.est <- merge(agelist, est.bnorm(rw.male.oe), all.x=TRUE)
      #   rw.female.oe.est <- merge(agelist, est.bnorm(rw.female.oe), all.x=TRUE)
      #
      #   # remarriage of widowed & cohabiting
      #   rwC.male.oe.est <- merge(agelist, est.bnorm(rwC.male.oe), all.x=TRUE)
      #   rwC.female.oe.est <- merge(agelist, est.bnorm(rwC.female.oe), all.x=TRUE)
      #
      #   # Remarriage of Divorced & not cohabiting
      #   rd.male.oe.est <- merge(agelist, est.bnorm(rd.male.oe), all.x=TRUE)
      #   rd.female.oe.est <- merge(agelist, est.bnorm(rd.female.oe), all.x=TRUE)
      #
      #   # remarriage of divorced & cohabiting
      #   rdC.male.oe.est <- merge(agelist, est.bnorm(rdC.male.oe), all.x=TRUE)
      #   rdC.female.oe.est <- merge(agelist, est.bnorm(rdC.female.oe), all.x=TRUE)
      #
      #   # never married to cohabiting
      #   nmtoC.male.oe.est <- merge(agelist, est.bnorm(nmtoC.male.oe), all.x=TRUE)
      #   nmtoC.female.oe.est <- merge(agelist,est.bnorm(nmtoC.female.oe), all.x=TRUE)
      #
      #   # widowed to cohabiting
      #   wtoC.male.oe.est <- merge(agelist, est.bnorm(wtoC.male.oe), all.x=TRUE)
      #   wtoC.female.oe.est <- merge(agelist, est.bnorm(wtoC.female.oe), all.x=TRUE)
      #
      #   # divorced to cohabiting
      #   dtoC.male.oe.est <- merge(agelist, est.bnorm(dtoC.male.oe), all.x=TRUE)
      #   dtoC.female.oe.est <- merge(agelist, est.bnorm(dtoC.female.oe), all.x=TRUE)
      #
      #   # cohabiting to never married
      #   Ctonm.male.oe.est <- merge(agelist, est.bnorm(Ctonm.male.oe), all.x=TRUE)
      #   Ctonm.female.oe.est <- merge(agelist, est.bnorm(Ctonm.female.oe), all.x=TRUE)
      #
      #   # cohabiting to widowed
      #   Ctow.male.oe.est <- merge(agelist, est.bnorm(Ctow.male.oe), all.x=TRUE)
      #   Ctow.female.oe.est <- merge(agelist, est.bnorm(Ctow.female.oe), all.x=TRUE)
      #
      #   # cohabiting to divorced
      #   Ctod.male.oe.est <- merge(agelist, est.bnorm(Ctod.male.oe), all.x=TRUE)
      #   Ctod.female.oe.est <- merge(agelist, est.bnorm(Ctod.female.oe), all.x=TRUE)
      #
      # }
      #
      # if (method=="Bayes.log"){
      #
      #   # never married & not cohabiting to married
      #   mrg1.male.oe.est <- merge(agelist, est.blog(mrg1.male.oe), all.x=TRUE)
      #   mrg1.female.oe.est <- merge(agelist, est.blog(mrg1.female.oe), all.x=TRUE)
      #
      #   # never married & cohabiting to married
      #   nmCm.male.oe.est <- merge(agelist, est.blog(nmCm.male.oe), all.x=TRUE)
      #   nmCm.female.oe.est <- merge(agelist, est.blog(nmCm.female.oe), all.x=TRUE)
      #
      #   # Divorced
      #   divorced.male.oe.est <- merge(agelist, est.blog(divorced.male.oe), all.x=TRUE)
      #   divorced.female.oe.est <- merge(agelist, est.blog(divorced.female.oe), all.x=TRUE)
      #
      #   # Remarriage of Widowed & not cohabiting
      #   rw.male.oe.est <- merge(agelist, est.blog(rw.male.oe), all.x=TRUE)
      #   rw.female.oe.est <- merge(agelist, est.blog(rw.female.oe), all.x=TRUE)
      #
      #   # remarriage of widowed & cohabiting
      #   rwC.male.oe.est <- merge(agelist, est.blog(rwC.male.oe), all.x=TRUE)
      #   rwC.female.oe.est <- merge(agelist, est.blog(rwC.female.oe), all.x=TRUE)
      #
      #   # Remarriage of Divorced & not cohabiting
      #   rd.male.oe.est <- merge(agelist, est.blog(rd.male.oe), all.x=TRUE)
      #   rd.female.oe.est <- merge(agelist, est.blog(rd.female.oe), all.x=TRUE)
      #
      #   # remarriage of divorced & cohabiting
      #   rdC.male.oe.est <- merge(agelist, est.blog(rdC.male.oe), all.x=TRUE)
      #   rdC.female.oe.est <- merge(agelist, est.blog(rdC.female.oe), all.x=TRUE)
      #
      #   # never married to cohabiting
      #   nmtoC.male.oe.est <- merge(agelist, est.blog(nmtoC.male.oe), all.x=TRUE)
      #   nmtoC.female.oe.est <- merge(agelist,est.blog(nmtoC.female.oe), all.x=TRUE)
      #
      #   # widowed to cohabiting
      #   wtoC.male.oe.est <- merge(agelist, est.blog(wtoC.male.oe), all.x=TRUE)
      #   wtoC.female.oe.est <- merge(agelist, est.blog(wtoC.female.oe), all.x=TRUE)
      #
      #   # divorced to cohabiting
      #   dtoC.male.oe.est <- merge(agelist, est.blog(dtoC.male.oe), all.x=TRUE)
      #   dtoC.female.oe.est <- merge(agelist, est.blog(dtoC.female.oe), all.x=TRUE)
      #
      #   # cohabiting to never married
      #   Ctonm.male.oe.est <- merge(agelist, est.blog(Ctonm.male.oe), all.x=TRUE)
      #   Ctonm.female.oe.est <- merge(agelist, est.blog(Ctonm.female.oe), all.x=TRUE)
      #
      #   # cohabiting to widowed
      #   Ctow.male.oe.est <- merge(agelist, est.blog(Ctow.male.oe), all.x=TRUE)
      #   Ctow.female.oe.est <- merge(agelist, est.blog(Ctow.female.oe), all.x=TRUE)
      #
      #   # cohabiting to divorced
      #   Ctod.male.oe.est <- merge(agelist, est.blog(Ctod.male.oe), all.x=TRUE)
      #   Ctod.female.oe.est <- merge(agelist, est.blog(Ctod.female.oe), all.x=TRUE)
      #
      # }

      #merge with direct calculate
      mrg1.male.oe <- merge(mrg1.male.oe, mrg1.male.oe.est, all.x=TRUE)
      mrg1.female.oe <- merge(mrg1.female.oe, mrg1.female.oe.est, all.x=TRUE)

      divorced.male.oe <- merge(divorced.male.oe, divorced.male.oe.est, all.x=TRUE)
      divorced.female.oe <- merge(divorced.female.oe, divorced.female.oe.est, all.x=TRUE)

      rw.male.oe <- merge(rw.male.oe, rw.male.oe.est, all.x=TRUE)
      rw.female.oe <- merge(rw.female.oe, rw.female.oe.est, all.x=TRUE)

      rd.male.oe <- merge(rd.male.oe, rd.male.oe.est, all.x=TRUE)
      rd.female.oe <- merge(rd.female.oe, rd.female.oe.est, all.x=TRUE)

      nmCm.male.oe <- merge(nmCm.male.oe, nmCm.male.oe.est, all.x=TRUE)
      nmCm.female.oe <- merge(nmCm.female.oe, nmCm.female.oe.est, all.x=TRUE)

      rwC.male.oe <- merge(rwC.male.oe, rwC.male.oe.est, all.x=TRUE)
      rwC.female.oe <- merge(rwC.female.oe, rwC.female.oe.est, all.x=TRUE)

      rdC.male.oe <- merge(rdC.male.oe, rdC.male.oe.est, all.x=TRUE)
      rdC.female.oe <- merge(rdC.female.oe, rdC.female.oe.est, all.x=TRUE)

      nmtoC.male.oe <- merge(nmtoC.male.oe, nmtoC.male.oe.est, all.x=TRUE)
      nmtoC.female.oe <- merge(nmtoC.female.oe, nmtoC.female.oe.est, all.x=TRUE)

      wtoC.male.oe <- merge(wtoC.male.oe, wtoC.male.oe.est, all.x=TRUE)
      wtoC.female.oe <- merge(wtoC.female.oe, wtoC.female.oe.est, all.x=TRUE)

      dtoC.male.oe <- merge(dtoC.male.oe, dtoC.male.oe.est, all.x=TRUE)
      dtoC.female.oe <- merge(dtoC.female.oe, dtoC.female.oe.est, all.x=TRUE)

      Ctonm.male.oe <- merge(Ctonm.male.oe, Ctonm.male.oe.est, all.x=TRUE)
      Ctonm.female.oe <- merge(Ctonm.female.oe, Ctonm.female.oe.est, all.x=TRUE)

      Ctow.male.oe <- merge(Ctow.male.oe, Ctow.male.oe.est, all.x=TRUE)
      Ctow.female.oe <- merge(Ctow.female.oe, Ctow.female.oe.est, all.x=TRUE)

      Ctod.male.oe <- merge(Ctod.male.oe, Ctod.male.oe.est, all.x=TRUE)
      Ctod.female.oe <- merge(Ctod.female.oe, Ctod.female.oe.est, all.x=TRUE)

    }

  } else {
    mrg1.male.oe <- NA
    mrg1.female.oe <- NA
    nmCm.male.oe <- NA
    nmCm.female.oe <- NA
    divorced.male.oe <- NA
    divorced.female.oe <- NA
    rw.male.oe <- NA
    rw.female.oe <- NA
    rwC.male.oe <- NA
    rwC.female.oe <- NA
    rd.male.oe <- NA
    rd.female.oe <- NA
    rdC.male.oe <- NA
    rdC.female.oe <- NA
    nmtoC.male.oe <- NA
    nmtoC.female.oe <- NA
    wtoC.male.oe <- NA
    wtoC.female.oe <- NA
    dtoC.male.oe <- NA
    dtoC.female.oe <- NA
    Ctonm.male.oe <- NA
    Ctonm.female.oe <- NA
    Ctow.male.oe <- NA
    Ctow.female.oe <- NA
    Ctod.male.oe <- NA
    Ctod.female.oe <- NA
  }

  if(cal_freq == TRUE){
    male <- subset(data_freq, sex == 1)
    female <- subset(data_freq, sex == 2)

    ##Direct Calculate

    # never married & not cohabiting to married
    mrg1.male.freq <- merge(agelist, freq.raw(male, nlm, nhm, 1, nWeight), all.x=TRUE)
    mrg1.female.freq <- merge(agelist, freq.raw(female, nlm, nhm, 1, nWeight), all.x=TRUE)

    # never married & cohabiting to married
    nmCm.male.freq <- merge(agelist, freq.raw(male, nlm, nhm, 11, nWeight), all.x=TRUE)
    nmCm.female.freq <- merge(agelist, freq.raw(female, nlm, nhm, 11, nWeight), all.x=TRUE)

    # Divorced
    divorced.male.freq <- merge(agelist, freq.raw(male, nlm, 95, 2, nWeight), all.x=TRUE)
    divorced.female.freq <- merge(agelist, freq.raw(female, nlm, 95, 2, nWeight), all.x=TRUE)

    # Remarriage of Widowed & not cohabiting
    rw.male.freq <- merge(agelist, freq.raw(male, nlm, 95, 3, nWeight), all.x=TRUE)
    rw.female.freq <- merge(agelist, freq.raw(female, nlm, 95, 3, nWeight), all.x=TRUE)

    # remarriage of widowed & cohabiting
    rwC.male.freq <- merge(agelist, freq.raw(male, nlm, 95, 12, nWeight), all.x=TRUE)
    rwC.female.freq <- merge(agelist, freq.raw(female, nlm, 95, 12, nWeight), all.x=TRUE)

    # Remarriage of Divorced & not cohabiting
    rd.male.freq <- merge(agelist, freq.raw(male, nlm, 95, 4, nWeight), all.x=TRUE)
    rd.female.freq <- merge(agelist, freq.raw(female, nlm, 95, 4, nWeight), all.x=TRUE)

    # remarriage of divorced & cohabiting
    rdC.male.freq <- merge(agelist, freq.raw(male, nlm, 95, 13, nWeight), all.x=TRUE)
    rdC.female.freq <- merge(agelist, freq.raw(female, nlm, 95, 13, nWeight), all.x=TRUE)

    # never married to cohabiting
    nmtoC.male.freq <- merge(agelist, freq.raw(male, nlm, 95, 5, nWeight), all.x=TRUE)
    nmtoC.female.freq <- merge(agelist, freq.raw(female, nlm, 95, 5, nWeight), all.x=TRUE)

    # widowed to cohabiting
    wtoC.male.freq <- merge(agelist, freq.raw(male, nlm, 95, 6, nWeight), all.x=TRUE)
    wtoC.female.freq <- merge(agelist, freq.raw(female, nlm, 95, 6, nWeight), all.x=TRUE)

    # divorced to cohabiting
    dtoC.male.freq <- merge(agelist, freq.raw(male, nlm, 95, 7, nWeight), all.x=TRUE)
    dtoC.female.freq <- merge(agelist, freq.raw(female, nlm, 95, 7, nWeight), all.x=TRUE)

    # cohabiting to never married
    Ctonm.male.freq <- merge(agelist, freq.raw(male, nlm, 95, 8, nWeight), all.x=TRUE)
    Ctonm.female.freq <- merge(agelist, freq.raw(female, nlm, 95, 8, nWeight), all.x=TRUE)

    # cohabiting to widowed
    Ctow.male.freq <- merge(agelist, freq.raw(male, nlm, 95, 9, nWeight), all.x=TRUE)
    Ctow.female.freq <- merge(agelist, freq.raw(female, nlm, 95, 9, nWeight), all.x=TRUE)

    # cohabiting to divorced
    Ctod.male.freq <- merge(agelist, freq.raw(male, nlm, 95, 10, nWeight), all.x=TRUE)
    Ctod.female.freq <- merge(agelist, freq.raw(female, nlm, 95, 10, nWeight), all.x=TRUE)

    if (!is.na(method)){

      if (method=="Poisson"){

        # never married & not cohabiting to married
        mrg1.male.freq.est <- merge(agelist, freq.poi(male, nlm, nhm, 1, nWeight, mfp), all.x=TRUE)
        mrg1.female.freq.est <- merge(agelist, freq.poi(female, nlm, nhm, 1, nWeight, mfp), all.x=TRUE)

        # never married & cohabiting to married
        nmCm.male.freq.est <- merge(agelist, freq.poi(male, nlm, nhm, 11, nWeight, mfp), all.x=TRUE)
        nmCm.female.freq.est <- merge(agelist, freq.poi(female, nlm, nhm, 11, nWeight, mfp), all.x=TRUE)

        # Divorced
        divorced.male.freq.est <- merge(agelist, freq.poi(male, nlm, 95, 2, nWeight, mfp), all.x=TRUE)
        divorced.female.freq.est <- merge(agelist, freq.poi(female, nlm, 95, 2, nWeight, mfp), all.x=TRUE)

        # Remarriage of Widowed & not cohabiting
        rw.male.freq.est <- merge(agelist, freq.poi(male, nlm, 95, 3, nWeight, mfp), all.x=TRUE)
        rw.female.freq.est <- merge(agelist, freq.poi(female, nlm, 95, 3, nWeight, mfp), all.x=TRUE)

        # remarriage of widowed & cohabiting
        rwC.male.freq.est <- merge(agelist, freq.poi(male, nlm, 95, 12, nWeight, mfp), all.x=TRUE)
        rwC.female.freq.est <- merge(agelist, freq.poi(female, nlm, 95, 12, nWeight, mfp), all.x=TRUE)

        # Remarriage of Divorced & not cohabiting
        rd.male.freq.est <- merge(agelist, freq.poi(male, nlm, 95, 4, nWeight, mfp), all.x=TRUE)
        rd.female.freq.est <- merge(agelist, freq.poi(female, nlm, 95, 4, nWeight, mfp), all.x=TRUE)

        # remarriage of divorced & cohabiting
        rdC.male.freq.est <- merge(agelist, freq.poi(male, nlm, 95, 13, nWeight, mfp), all.x=TRUE)
        rdC.female.freq.est <- merge(agelist, freq.poi(female, nlm, 95, 13, nWeight, mfp), all.x=TRUE)

        # never married to cohabiting
        nmtoC.male.freq.est <- merge(agelist, freq.poi(male, nlm, 95, 5, nWeight, mfp), all.x=TRUE)
        nmtoC.female.freq.est <- merge(agelist,freq.poi(female, nlm, 95, 5, nWeight, mfp), all.x=TRUE)

        # widowed to cohabiting
        wtoC.male.freq.est <- merge(agelist, freq.poi(male, nlm, 95, 6, nWeight, mfp), all.x=TRUE)
        wtoC.female.freq.est <- merge(agelist, freq.poi(female, nlm, 95, 6, nWeight, mfp), all.x=TRUE)

        # divorced to cohabiting
        dtoC.male.freq.est <- merge(agelist, freq.poi(male, nlm, 95, 7, nWeight, mfp), all.x=TRUE)
        dtoC.female.freq.est <- merge(agelist, freq.poi(female, nlm, 95, 7, nWeight, mfp), all.x=TRUE)

        # cohabiting to never married
        Ctonm.male.freq.est <- merge(agelist, freq.poi(male, nlm, 95, 8, nWeight, mfp), all.x=TRUE)
        Ctonm.female.freq.est <- merge(agelist, freq.poi(female, nlm, 95, 8, nWeight, mfp), all.x=TRUE)

        # cohabiting to widowed
        Ctow.male.freq.est <- merge(agelist, freq.poi(female, nlm, 95, 9, nWeight, mfp), all.x=TRUE)
        Ctow.female.freq.est <- merge(agelist, freq.poi(female, nlm, 95, 9, nWeight, mfp), all.x=TRUE)

        # cohabiting to divorced
        Ctod.male.freq.est <- merge(agelist, freq.poi(female, nlm, 95, 10, nWeight, mfp), all.x=TRUE)
        Ctod.female.freq.est <- merge(agelist, freq.poi(female, nlm, 95, 10, nWeight, mfp), all.x=TRUE)

      }

      # if (method=="Bayes.norm"){
      #
      #   # never married & not cohabiting to married
      #   mrg1.male.freq.est <- merge(agelist, est.bnorm(mrg1.male.freq), all.x=TRUE)
      #   mrg1.female.freq.est <- merge(agelist, est.bnorm(mrg1.female.freq), all.x=TRUE)
      #
      #   # never married & cohabiting to married
      #   nmCm.male.freq.est <- merge(agelist, est.bnorm(nmCm.male.freq), all.x=TRUE)
      #   nmCm.female.freq.est <- merge(agelist, est.bnorm(nmCm.female.freq), all.x=TRUE)
      #
      #   # Divorced
      #   divorced.male.freq.est <- merge(agelist, est.bnorm(divorced.male.freq), all.x=TRUE)
      #   divorced.female.freq.est <- merge(agelist, est.bnorm(divorced.female.freq), all.x=TRUE)
      #
      #   # Remarriage of Widowed & not cohabiting
      #   rw.male.freq.est <- merge(agelist, est.bnorm(rw.male.freq), all.x=TRUE)
      #   rw.female.freq.est <- merge(agelist, est.bnorm(rw.female.freq), all.x=TRUE)
      #
      #   # remarriage of widowed & cohabiting
      #   rwC.male.freq.est <- merge(agelist, est.bnorm(rwC.male.freq), all.x=TRUE)
      #   rwC.female.freq.est <- merge(agelist, est.bnorm(rwC.female.freq), all.x=TRUE)
      #
      #   # Remarriage of Divorced & not cohabiting
      #   rd.male.freq.est <- merge(agelist, est.bnorm(rd.male.freq), all.x=TRUE)
      #   rd.female.freq.est <- merge(agelist, est.bnorm(rd.female.freq), all.x=TRUE)
      #
      #   # remarriage of divorced & cohabiting
      #   rdC.male.freq.est <- merge(agelist, est.bnorm(rdC.male.freq), all.x=TRUE)
      #   rdC.female.freq.est <- merge(agelist, est.bnorm(rdC.female.freq), all.x=TRUE)
      #
      #   # never married to cohabiting
      #   nmtoC.male.freq.est <- merge(agelist, est.bnorm(nmtoC.male.freq), all.x=TRUE)
      #   nmtoC.female.freq.est <- merge(agelist,est.bnorm(nmtoC.female.freq), all.x=TRUE)
      #
      #   # widowed to cohabiting
      #   wtoC.male.freq.est <- merge(agelist, est.bnorm(wtoC.male.freq), all.x=TRUE)
      #   wtoC.female.freq.est <- merge(agelist, est.bnorm(wtoC.female.freq), all.x=TRUE)
      #
      #   # divorced to cohabiting
      #   dtoC.male.freq.est <- merge(agelist, est.bnorm(dtoC.male.freq), all.x=TRUE)
      #   dtoC.female.freq.est <- merge(agelist, est.bnorm(dtoC.female.freq), all.x=TRUE)
      #
      #   # cohabiting to never married
      #   Ctonm.male.freq.est <- merge(agelist, est.bnorm(Ctonm.male.freq), all.x=TRUE)
      #   Ctonm.female.freq.est <- merge(agelist, est.bnorm(Ctonm.female.freq), all.x=TRUE)
      #
      #   # cohabiting to widowed
      #   Ctow.male.freq.est <- merge(agelist, est.bnorm(Ctow.male.freq), all.x=TRUE)
      #   Ctow.female.freq.est <- merge(agelist, est.bnorm(Ctow.female.freq), all.x=TRUE)
      #
      #   # cohabiting to divorced
      #   Ctod.male.freq.est <- merge(agelist, est.bnorm(Ctod.male.freq), all.x=TRUE)
      #   Ctod.female.freq.est <- merge(agelist, est.bnorm(Ctod.female.freq), all.x=TRUE)
      #
      # }
      #
      # if (method=="Bayes.log"){
      #
      #   # never married & not cohabiting to married
      #   mrg1.male.freq.est <- merge(agelist, est.blog(mrg1.male.freq), all.x=TRUE)
      #   mrg1.female.freq.est <- merge(agelist, est.blog(mrg1.female.freq), all.x=TRUE)
      #
      #   # never married & cohabiting to married
      #   nmCm.male.freq.est <- merge(agelist, est.blog(nmCm.male.freq), all.x=TRUE)
      #   nmCm.female.freq.est <- merge(agelist, est.blog(nmCm.female.freq), all.x=TRUE)
      #
      #   # Divorced
      #   divorced.male.freq.est <- merge(agelist, est.blog(divorced.male.freq), all.x=TRUE)
      #   divorced.female.freq.est <- merge(agelist, est.blog(divorced.female.freq), all.x=TRUE)
      #
      #   # Remarriage of Widowed & not cohabiting
      #   rw.male.freq.est <- merge(agelist, est.blog(rw.male.freq), all.x=TRUE)
      #   rw.female.freq.est <- merge(agelist, est.blog(rw.female.freq), all.x=TRUE)
      #
      #   # remarriage of widowed & cohabiting
      #   rwC.male.freq.est <- merge(agelist, est.blog(rwC.male.freq), all.x=TRUE)
      #   rwC.female.freq.est <- merge(agelist, est.blog(rwC.female.freq), all.x=TRUE)
      #
      #   # Remarriage of Divorced & not cohabiting
      #   rd.male.freq.est <- merge(agelist, est.blog(rd.male.freq), all.x=TRUE)
      #   rd.female.freq.est <- merge(agelist, est.blog(rd.female.freq), all.x=TRUE)
      #
      #   # remarriage of divorced & cohabiting
      #   rdC.male.freq.est <- merge(agelist, est.blog(rdC.male.freq), all.x=TRUE)
      #   rdC.female.freq.est <- merge(agelist, est.blog(rdC.female.freq), all.x=TRUE)
      #
      #   # never married to cohabiting
      #   nmtoC.male.freq.est <- merge(agelist, est.blog(nmtoC.male.freq), all.x=TRUE)
      #   nmtoC.female.freq.est <- merge(agelist,est.blog(nmtoC.female.freq), all.x=TRUE)
      #
      #   # widowed to cohabiting
      #   wtoC.male.freq.est <- merge(agelist, est.blog(wtoC.male.freq), all.x=TRUE)
      #   wtoC.female.freq.est <- merge(agelist, est.blog(wtoC.female.freq), all.x=TRUE)
      #
      #   # divorced to cohabiting
      #   dtoC.male.freq.est <- merge(agelist, est.blog(dtoC.male.freq), all.x=TRUE)
      #   dtoC.female.freq.est <- merge(agelist, est.blog(dtoC.female.freq), all.x=TRUE)
      #
      #   # cohabiting to never married
      #   Ctonm.male.freq.est <- merge(agelist, est.blog(Ctonm.male.freq), all.x=TRUE)
      #   Ctonm.female.freq.est <- merge(agelist, est.blog(Ctonm.female.freq), all.x=TRUE)
      #
      #   # cohabiting to widowed
      #   Ctow.male.freq.est <- merge(agelist, est.blog(Ctow.male.freq), all.x=TRUE)
      #   Ctow.female.freq.est <- merge(agelist, est.blog(Ctow.female.freq), all.x=TRUE)
      #
      #   # cohabiting to divorced
      #   Ctod.male.freq.est <- merge(agelist, est.blog(Ctod.male.freq), all.x=TRUE)
      #   Ctod.female.freq.est <- merge(agelist, est.blog(Ctod.female.freq), all.x=TRUE)
      #
      # }

      #merge with direct calculate
      mrg1.male.freq <- merge(mrg1.male.freq, mrg1.male.freq.est, all.x=TRUE)
      mrg1.female.freq <- merge(mrg1.female.freq, mrg1.female.freq.est, all.x=TRUE)

      divorced.male.freq <- merge(divorced.male.freq, divorced.male.freq.est, all.x=TRUE)
      divorced.female.freq <- merge(divorced.female.freq, divorced.female.freq.est, all.x=TRUE)

      rw.male.freq <- merge(rw.male.freq, rw.male.freq.est, all.x=TRUE)
      rw.female.freq <- merge(rw.female.freq, rw.female.freq.est, all.x=TRUE)

      rd.male.freq <- merge(rd.male.freq, rd.male.freq.est, all.x=TRUE)
      rd.female.freq <- merge(rd.female.freq, rd.female.freq.est, all.x=TRUE)

      nmCm.male.freq <- merge(nmCm.male.freq, nmCm.male.freq.est, all.x=TRUE)
      nmCm.female.freq <- merge(nmCm.female.freq, nmCm.female.freq.est, all.x=TRUE)

      rwC.male.freq <- merge(rwC.male.freq, rwC.male.freq.est, all.x=TRUE)
      rwC.female.freq <- merge(rwC.female.freq, rwC.female.freq.est, all.x=TRUE)

      rdC.male.freq <- merge(rdC.male.freq, rdC.male.freq.est, all.x=TRUE)
      rdC.female.freq <- merge(rdC.female.freq, rdC.female.freq.est, all.x=TRUE)

      nmtoC.male.freq <- merge(nmtoC.male.freq, nmtoC.male.freq.est, all.x=TRUE)
      nmtoC.female.freq <- merge(nmtoC.female.freq, nmtoC.female.freq.est, all.x=TRUE)

      wtoC.male.freq <- merge(wtoC.male.freq, wtoC.male.freq.est, all.x=TRUE)
      wtoC.female.freq <- merge(wtoC.female.freq, wtoC.female.freq.est, all.x=TRUE)

      dtoC.male.freq <- merge(dtoC.male.freq, dtoC.male.freq.est, all.x=TRUE)
      dtoC.female.freq <- merge(dtoC.female.freq, dtoC.female.freq.est, all.x=TRUE)

      Ctonm.male.freq <- merge(Ctonm.male.freq, Ctonm.male.freq.est, all.x=TRUE)
      Ctonm.female.freq <- merge(Ctonm.female.freq, Ctonm.female.freq.est, all.x=TRUE)

      Ctow.male.freq <- merge(Ctow.male.freq, Ctow.male.freq.est, all.x=TRUE)
      Ctow.female.freq <- merge(Ctow.female.freq, Ctow.female.freq.est, all.x=TRUE)

      Ctod.male.freq <- merge(Ctod.male.freq, Ctod.male.freq.est, all.x=TRUE)
      Ctod.female.freq <- merge(Ctod.female.freq, Ctod.female.freq.est, all.x=TRUE)

    }

  } else {
    mrg1.male.freq <- NA
    mrg1.female.freq <- NA
    nmCm.male.freq <- NA
    nmCm.female.freq <- NA
    divorced.male.freq <- NA
    divorced.female.freq <- NA
    rw.male.freq <- NA
    rw.female.freq <- NA
    rwC.male.freq <- NA
    rwC.female.freq <- NA
    rd.male.freq <- NA
    rd.female.freq <- NA
    rdC.male.freq <- NA
    rdC.female.freq <- NA
    nmtoC.male.freq <- NA
    nmtoC.female.freq <- NA
    wtoC.male.freq <- NA
    wtoC.female.freq <- NA
    dtoC.male.freq <- NA
    dtoC.female.freq <- NA
    Ctonm.male.freq <- NA
    Ctonm.female.freq <- NA
    Ctow.male.freq <- NA
    Ctow.female.freq <- NA
    Ctod.male.freq <- NA
    Ctod.female.freq <- NA
  }


  if(plot == TRUE){
    wb <- createWorkbook()
    k<-1

    if(cal_oe == TRUE){
      addWorksheet(wb, "oe")
      writeData(wb, sheet=k, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
      writeData(wb, sheet=k, paste0("Period: ", period, sep=""), startRow=2, startCol=1)
      k<-k+1

      row.index <- 3
      p1 <- rates.plot(mrg1.male.oe, nlm, nhm, "o/e rate", paste0("Figure 14.1. Never married & not-cohabiting to married o/e rates, males", plot.name, sep=""))
      print(p1)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
      p2 <- rates.plot(mrg1.female.oe, nlm, nhm, "o/e rate", paste0("Figure 14.2. Never married & not-cohabiting to married o/e rates, females", plot.name, sep=""))
      print(p2)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

      row.index <- row.index + 22
      p5 <- rates.plot(nmCm.male.oe, nlm, nhm, "o/e rate", paste0("Figure 15.1. Never married & cohabiting to married o/e rates, males", plot.name, sep=""))
      print(p5)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
      p6 <- rates.plot(nmCm.female.oe, nlm, nhm, "o/e rate", paste0("Figure 15.2. Never married & cohabiting to married o/e rates, females", plot.name, sep=""))
      print(p6)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

      row.index <- row.index + 22
      p9 <- rates.plot(divorced.male.oe, nlm, 95, "o/e rate", paste0("Figure 16.1. Divorced o/e rates, males", plot.name, sep=""))
      print(p9)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
      p10 <- rates.plot(divorced.female.oe, nlm, 95, "o/e rate", paste0("Figure 16.2. Divorced o/e rates, females", plot.name, sep=""))
      print(p10)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

      row.index <- row.index + 22
      p13 <- rates.plot(rw.male.oe, nlm, 95, "o/e rate", paste0("Figure 17.1. Remarriage of widowed & not-cohabiting o/e rates, males", plot.name, sep=""))
      print(p13)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
      p14 <- rates.plot(rw.female.oe, nlm, 95, "o/e rate", paste0("Figure 17.2. Remarriage of widowed & not-cohabiting o/e rates, females", plot.name, sep=""))
      print(p14)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

      row.index <- row.index + 22
      p17 <- rates.plot(rwC.male.oe, nlm, 95, "o/e rate", paste0("Figure 18.1. Remarriage of widowed & cohabiting o/e rates, males", plot.name, sep=""))
      print(p17)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
      p18 <- rates.plot(rwC.female.oe, nlm, 95, "o/e rate", paste0("Figure 18.2. Remarriage of widowed & cohabiting o/e rates, females", plot.name, sep=""))
      print(p18)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

      row.index <- row.index + 22
      p21 <- rates.plot(rd.male.oe, nlm, 95, "o/e rate", paste0("Figure 19.1. Remarriage of divorced & not-cohabiting o/e rates, males", plot.name, sep=""))
      print(p21)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
      p22 <- rates.plot(rd.female.oe, nlm, 95, "o/e rate", paste0("Figure 19.2. Remarriage of divorced & not-cohabiting o/e rates, females", plot.name, sep=""))
      print(p22)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

      row.index <- row.index + 22
      p25 <- rates.plot(rdC.male.oe, nlm, 95, "o/e rate", paste0("Figure 20.1. Remarriage of divorced & cohabiting o/e rates, males", plot.name, sep=""))
      print(p25)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
      p26 <- rates.plot(rdC.female.oe, nlm, 95, "o/e rate", paste0("Figure 20.2. Remarriage of divorced & cohabiting o/e rates, females", plot.name, sep=""))
      print(p26)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

      row.index <- row.index + 22
      p29 <- rates.plot(nmtoC.male.oe, nlm, 95, "o/e rate", paste0("Figure 21.1. Never married to cohabiting o/e rates, males", plot.name, sep=""))
      print(p29)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
      p30 <- rates.plot(nmtoC.female.oe, nlm, 95, "o/e rate", paste0("Figure 21.2. Never married to cohabiting o/e rates, females", plot.name, sep=""))
      print(p30)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

      row.index <- row.index + 22
      p33 <- rates.plot(wtoC.male.oe, nlm, 95, "o/e rate", paste0("Figure 22.1. Widowed to cohabiting o/e rates, males", plot.name, sep=""))
      print(p33)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
      p34 <- rates.plot(wtoC.female.oe, nlm, 95, "o/e rate", paste0("Figure 22.2. Widowed to cohabiting o/e rates, females", plot.name, sep=""))
      print(p34)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

      row.index <- row.index + 22
      p37 <- rates.plot(dtoC.male.oe, nlm, 95, "o/e rate", paste0("Figure 23.1. Divorced to cohabiting o/e rates, males", plot.name, sep=""))
      print(p37)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
      p38 <- rates.plot(dtoC.female.oe, nlm, 95, "o/e rate", paste0("Figure 23.2. Divorced to cohabiting o/e rates, females", plot.name, sep=""))
      print(p38)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

      row.index <- row.index + 22
      p41 <- rates.plot(Ctonm.male.oe, nlm, 95, "o/e rate", paste0("Figure 24.1. Cohabiting to never married o/e rates, males", plot.name, sep=""))
      print(p41)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
      p42 <- rates.plot(Ctonm.female.oe, nlm, 95, "o/e rate", paste0("Figure 24.2. Cohabiting to never married o/e rates, females", plot.name, sep=""))
      print(p42)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

      row.index <- row.index + 22
      p45 <- rates.plot(Ctow.male.oe, nlm, 95, "o/e rate", paste0("Figure 25.1. Cohabiting to widowed o/e rates, males", plot.name, sep=""))
      print(p45)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
      p46 <- rates.plot(Ctow.female.oe, nlm, 95, "o/e rate", paste0("Figure 25.2. Cohabiting to widowed o/e rates, females", plot.name, sep=""))
      print(p46)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

      row.index <- row.index + 22
      p49 <- rates.plot(Ctod.male.oe, nlm, 95, "o/e rate", paste0("Figure 26.1. Cohabiting to divorced o/e rates, males", plot.name, sep=""))
      print(p49)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
      p50 <- rates.plot(Ctod.female.oe, nlm, 95, "o/e rate", paste0("Figure 26.2. Cohabiting to divorced o/e rates, females", plot.name, sep=""))
      print(p50)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")
    }

    if(cal_freq == TRUE){
      addWorksheet(wb, "freq")
      writeData(wb, sheet=k, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
      writeData(wb, sheet=k, paste0("Period: ", period, sep=""), startRow=2, startCol=1)

      row.index <- 3
      p3 <- rates.plot(mrg1.male.freq, nlm, nhm, "frequency", paste0("Figure 1.1. Never married & not-cohabiting to married frequencies, males", plot.name, sep=""))
      print(p3)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
      p4 <- rates.plot(mrg1.female.freq, nlm, nhm, "frequency", paste0("Figure 1.2. Never married & not-cohabiting to married frequencies, females", plot.name, sep=""))
      print(p4)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

      row.index <- row.index + 22
      p7 <- rates.plot(nmCm.male.freq, nlm, nhm, "frequency", paste0("Figure 2.1. Never married & cohabiting to married frequencies, males", plot.name, sep=""))
      print(p7)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
      p8 <- rates.plot(nmCm.female.freq, nlm, nhm, "frequency", paste0("Figure 2.2. Never married & cohabiting to married frequencies, females", plot.name, sep=""))
      print(p8)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

      row.index <- row.index + 22
      p11 <- rates.plot(divorced.male.freq, nlm, 95, "frequency", paste0("Figure 3.1. Divorced frequencies, males", plot.name, sep=""))
      print(p11)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
      p12 <- rates.plot(divorced.female.freq, nlm, 95, "frequency", paste0("Figure 3.2. Divorced frequencies, females", plot.name, sep=""))
      print(p12)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

      row.index <- row.index + 22
      p15 <- rates.plot(rw.male.freq, nlm, 95, "frequency", paste0("Figure 4.1. Remarriage of widowed & not-cohabiting frequencies, males", plot.name, sep=""))
      print(p15)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
      p16 <- rates.plot(rw.female.freq, nlm, 95, "frequency", paste0("Figure 4.2. Remarriage of widowed & not-cohabiting frequencies, females", plot.name, sep=""))
      print(p16)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

      row.index <- row.index + 22
      p19 <- rates.plot(rwC.male.freq, nlm, 95, "frequency", paste0("Figure 5.1. Remarriage of widowed & cohabiting frequencies, males", plot.name, sep=""))
      print(p19)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
      p20 <- rates.plot(rwC.female.freq, nlm, 95, "frequency", paste0("Figure 5.2. Remarriage of widowed & cohabiting frequencies, females", plot.name, sep=""))
      print(p20)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

      row.index <- row.index + 22
      p23 <- rates.plot(rd.male.freq, nlm, 95, "frequency", paste0("Figure 6.1. Remarriage of divorced & not-cohabiting frequencies, males", plot.name, sep=""))
      print(p23)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
      p24 <- rates.plot(rd.female.freq, nlm, 95, "frequency", paste0("Figure 6.2. Remarriage of divorced & not-cohabiting frequencies, females", plot.name, sep=""))
      print(p24)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

      row.index <- row.index + 22
      p27 <- rates.plot(rdC.male.freq, nlm, 95, "frequency", paste0("Figure 7.1. Remarriage of divorced & cohabiting frequencies, males", plot.name, sep=""))
      print(p27)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
      p28 <- rates.plot(rdC.female.freq, nlm, 95, "frequency", paste0("Figure 7.2. Remarriage of divorced & cohabiting frequencies, females", plot.name, sep=""))
      print(p28)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

      row.index <- row.index + 22
      p31 <- rates.plot(nmtoC.male.freq, nlm, 95, "frequency", paste0("Figure 8.1. Never married to cohabiting frequencies, males", plot.name, sep=""))
      print(p31)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
      p32 <- rates.plot(nmtoC.female.freq, nlm, 95, "frequency", paste0("Figure 8.2. Never married to cohabiting frequencies, females", plot.name, sep=""))
      print(p32)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

      row.index <- row.index + 22
      p35 <- rates.plot(wtoC.male.freq, nlm, 95, "frequency", paste0("Figure 9.1. Widowed to cohabiting frequencies, males", plot.name, sep=""))
      print(p35)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
      p36 <- rates.plot(wtoC.female.freq, nlm, 95, "frequency", paste0("Figure 9.2. Widowed to cohabiting frequencies, females", plot.name, sep=""))
      print(p36)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

      row.index <- row.index + 22
      p39 <- rates.plot(dtoC.male.freq, nlm, 95, "frequency", paste0("Figure 10.1. Divorced to cohabiting frequencies, males", plot.name, sep=""))
      print(p39)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
      p40 <- rates.plot(dtoC.female.freq, nlm, 95, "frequency", paste0("Figure 10.2. Divorced to cohabiting frequencies, females", plot.name, sep=""))
      print(p40)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

      row.index <- row.index + 22
      p43 <- rates.plot(Ctonm.male.freq, nlm, 95, "frequency", paste0("Figure 11.1. Cohabiting to never married frequencies, males", plot.name, sep=""))
      print(p43)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
      p44 <- rates.plot(Ctonm.female.freq, nlm, 95, "frequency", paste0("Figure 11.2. Cohabiting to never married frequencies, females", plot.name, sep=""))
      print(p44)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

      row.index <- row.index + 22
      p47 <- rates.plot(Ctow.male.freq, nlm, 95, "frequency", paste0("Figure 12.1. Cohabiting to widowed frequencies, males", plot.name, sep=""))
      print(p47)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
      p48 <- rates.plot(Ctow.female.freq, nlm, 95, "frequency", paste0("Figure 12.2. Cohabiting to widowed frequencies, females", plot.name, sep=""))
      print(p48)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

      row.index <- row.index + 22
      p51 <- rates.plot(Ctod.male.freq, nlm, 95, "frequency", paste0("Figure 13.1. Cohabiting to divorced frequencies, males", plot.name, sep=""))
      print(p51)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
      p52 <- rates.plot(Ctod.female.freq, nlm, 95, "frequency", paste0("Figure 13.2. Cohabiting to divorced frequencies, females", plot.name, sep=""))
      print(p52)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")
    }

    output.dir <- getwd()
    plot.name <- gsub(", ", "-", plot.name)
    saveWorkbook(wb, paste0(output.dir, "/", title, " Mar7 plot", plot.name, ".xlsx", sep=""), overwrite=TRUE)
  }

  if(cal_oe == TRUE){

    if (!is.na(method)){
      male.oe <- data.frame(age=agelist$age, nmNC.married=mrg1.male.oe$est.rates, nmC.married=nmCm.male.oe$est.rates,
                            divorced=divorced.male.oe$est.rates, rwNC=rw.male.oe$est.rates, rwC=rwC.male.oe$est.rates,
                            rdNC=rd.male.oe$est.rates, rdC=rdC.male.oe$est.rates, nmtoC=nmtoC.male.oe$est.rates,
                            wtoC=wtoC.male.oe$est.rates, dtoC=dtoC.male.oe$est.rates, Ctonm=Ctonm.male.oe$est.rates,
                            Ctow=Ctow.male.oe$est.rates, Ctod=Ctod.male.oe$est.rates)
      female.oe <- data.frame(age=agelist$age, nmNC.married=mrg1.female.oe$est.rates, nmC.married=nmCm.female.oe$est.rates,
                              divorced=divorced.female.oe$est.rates, rwNC=rw.female.oe$est.rates, rwC=rwC.female.oe$est.rates,
                              rdNC=rd.female.oe$est.rates, rdC=rdC.female.oe$est.rates, nmtoC=nmtoC.female.oe$est.rates,
                              wtoC=wtoC.female.oe$est.rates, dtoC=dtoC.female.oe$est.rates, Ctonm=Ctonm.female.oe$est.rates,
                              Ctow=Ctow.female.oe$est.rates, Ctod=Ctod.female.oe$est.rates)
    } else {
      male.oe <- female.oe <- NA
    }

    raw.male.oe <- data.frame(age=agelist$age, nmNC.married=mrg1.male.oe$raw.rates, nmC.married=nmCm.male.oe$raw.rates,
                              divorced=divorced.male.oe$raw.rates, rwNC=rw.male.oe$raw.rates, rwC=rwC.male.oe$raw.rates,
                              rdNC=rd.male.oe$raw.rates, rdC=rdC.male.oe$raw.rates, nmtoC=nmtoC.male.oe$raw.rates,
                              wtoC=wtoC.male.oe$raw.rates, dtoC=dtoC.male.oe$raw.rates, Ctonm=Ctonm.male.oe$raw.rates,
                              Ctow=Ctow.male.oe$raw.rates, Ctod=Ctod.male.oe$raw.rates)
    raw.female.oe <- data.frame(age=agelist$age, nmNC.married=mrg1.female.oe$raw.rates, nmC.married=nmCm.female.oe$raw.rates,
                                divorced=divorced.female.oe$raw.rates, rwNC=rw.female.oe$raw.rates, rwC=rwC.female.oe$raw.rates,
                                rdNC=rd.female.oe$raw.rates, rdC=rdC.female.oe$raw.rates, nmtoC=nmtoC.female.oe$raw.rates,
                                wtoC=wtoC.female.oe$raw.rates, dtoC=dtoC.female.oe$raw.rates, Ctonm=Ctonm.female.oe$raw.rates,
                                Ctow=Ctow.female.oe$raw.rates, Ctod=Ctod.female.oe$raw.rates)
  } else {
    male.oe <- NA
    female.oe <- NA
    raw.male.oe <- NA
    raw.female.oe <- NA
  }

  if(cal_freq == TRUE){

    raw.male.freq <- data.frame(age=agelist$age, nmNC.married=mrg1.male.freq$raw.rates, nmC.married=nmCm.male.freq$raw.rates,
                                divorced=divorced.male.freq$raw.rates, rwNC=rw.male.freq$raw.rates, rwC=rwC.male.freq$raw.rates,
                                rdNC=rd.male.freq$raw.rates, rdC=rdC.male.freq$raw.rates, nmtoC=nmtoC.male.freq$raw.rates,
                                wtoC=wtoC.male.freq$raw.rates, dtoC=dtoC.male.freq$raw.rates, Ctonm=Ctonm.male.freq$raw.rates,
                                Ctow=Ctow.male.freq$raw.rates, Ctod=Ctod.male.freq$raw.rates)
    raw.female.freq <- data.frame(age=agelist$age, nmNC.married=mrg1.female.freq$raw.rates, nmC.married=nmCm.female.freq$raw.rates,
                                  divorced=divorced.female.freq$raw.rates, rwNC=rw.female.freq$raw.rates, rwC=rwC.female.freq$raw.rates,
                                  rdNC=rd.female.freq$raw.rates, rdC=rdC.female.freq$raw.rates, nmtoC=nmtoC.female.freq$raw.rates,
                                  wtoC=wtoC.female.freq$raw.rates, dtoC=dtoC.female.freq$raw.rates, Ctonm=Ctonm.female.freq$raw.rates,
                                  Ctow=Ctow.female.freq$raw.rates, Ctod=Ctod.female.freq$raw.rates)

    #Calculate raw total rates and mean age based on raw freq.
    raw.freq <- freq.mar7(list(raw.male.freq, raw.female.freq), T)
    raw.male.freq <- raw.freq$frequency[[1]]
    raw.female.freq <- raw.freq$frequency[[2]]
    raw.total.rates <- raw.freq$total.rates
    raw.mean.age <- raw.freq$mean.age

    if (!is.na(method)){
      male.freq <- data.frame(age=agelist$age, nmNC.married=mrg1.male.freq$est.rates, nmC.married=nmCm.male.freq$est.rates,
                              divorced=divorced.male.freq$est.rates, rwNC=rw.male.freq$est.rates, rwC=rwC.male.freq$est.rates,
                              rdNC=rd.male.freq$est.rates, rdC=rdC.male.freq$est.rates, nmtoC=nmtoC.male.freq$est.rates,
                              wtoC=wtoC.male.freq$est.rates, dtoC=dtoC.male.freq$est.rates, Ctonm=Ctonm.male.freq$est.rates,
                              Ctow=Ctow.male.freq$est.rates, Ctod=Ctod.male.freq$est.rates)
      female.freq <- data.frame(age=agelist$age, nmNC.married=mrg1.female.freq$est.rates, nmC.married=nmCm.female.freq$est.rates,
                                divorced=divorced.female.freq$est.rates, rwNC=rw.female.freq$est.rates, rwC=rwC.female.freq$est.rates,
                                rdNC=rd.female.freq$est.rates, rdC=rdC.female.freq$est.rates, nmtoC=nmtoC.female.freq$est.rates,
                                wtoC=wtoC.female.freq$est.rates, dtoC=dtoC.female.freq$est.rates, Ctonm=Ctonm.female.freq$est.rates,
                                Ctow=Ctow.female.freq$est.rates, Ctod=Ctod.female.freq$est.rates)

      #Calculate estimated total rates and mean age based on estimated freq.
      est.freq <- freq.mar7(list(male.freq, female.freq), T)
      male.freq <- est.freq$frequency[[1]]
      female.freq <- est.freq$frequency[[2]]
      est.total.rates <- est.freq$total.rates
      est.mean.age <- est.freq$mean.age

      total.rates <- rbind(raw.total.rates, est.total.rates, (est.total.rates-raw.total.rates)/raw.total.rates)
      mean.age <- rbind(raw.mean.age, est.mean.age, (est.mean.age-raw.mean.age)/raw.mean.age)
      total.rates <- do.call(data.frame, lapply(total.rates, function(x) replace(x, is.infinite(x), NA)))

    } else {
      male.freq <- female.freq <- NA
      total.rates <- NA
      mean.age <- NA
    }

  } else {
    male.freq <- NA
    female.freq <- NA
    raw.male.freq <- NA
    raw.female.freq <- NA
    total.rates <- NA
    mean.age <- NA
  }


  return(list(male.oe=male.oe, female.oe=female.oe, male.freq=male.freq, female.freq=female.freq,
              raw.male.oe=raw.male.oe, raw.female.oe=raw.female.oe,
              raw.male.freq=raw.male.freq, raw.female.freq=raw.female.freq,
              total.rates=total.rates, mean.age=mean.age))
}

##covariate method
compute.seven.covar <- function(data_oe, data_freq, param, code, plot, plot.name, byvar, mfp){

  nrate <- as.numeric(param$nRate)
  nlm <- as.numeric(param$nlm)
  nhm <- as.numeric(param$nhm)
  title <- as.character(param$title)
  t1Month <- as.numeric(param$t1Month)
  t2Month <- as.numeric(param$t2Month)
  nWeight <- as.numeric(param$nWeight)
  period <- paste0((t1Month-1)%/%12+1900, " - ",(t2Month-1)%/%12+1900, sep="")
  nRegion <- as.numeric(param$nRegion)
  nRace <- as.numeric(param$nRace)

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

  mrg1.oe <- divorced.oe <- rw.oe <- rd.oe <- NA
  nmCm.oe <- rwC.oe <- rdC.oe <- nmtoC.oe <- wtoC.oe <- dtoC.oe <- Ctonm.oe <- Ctow.oe <- Ctod.oe <- NA

  mrg1.freq <- divorced.freq <- rw.freq <- rd.freq <- NA
  nmCm.freq <- rwC.freq <- rdC.freq <- nmtoC.freq <- wtoC.freq <- dtoC.freq <- Ctonm.freq <- Ctow.freq <- Ctod.freq <- NA

  oe.rates.m <- oe.rates.f <- raw.oe.rates.m <- raw.oe.rates.f <- NA
  frequency.m <- frequency.f <- raw.frequency.m <- raw.frequency.f <- NA

  if(cal_oe == TRUE){

    agelist <- null.rates(data_oe, nlm, 95, byvar)

    #only set nhm for first marriage, all other events -> 95
    mrg1.oe <- merge(agelist, oe.est.byvar(data_oe, nlm, nhm, 1, 1, byvar, nWeight, mfp), all.x=TRUE)
    divorced.oe <- merge(agelist, oe.est.byvar(data_oe, nlm, 95, 2, 2, byvar, nWeight, mfp), all.x=TRUE)
    rw.oe <- merge(agelist, oe.est.byvar(data_oe, nlm, 95, 3, 3, byvar, nWeight, mfp), all.x=TRUE)
    rd.oe <- merge(agelist, oe.est.byvar(data_oe, nlm, 95, 4, 4, byvar, nWeight, mfp), all.x=TRUE)

    nmCm.oe <- merge(agelist, oe.est.byvar(data_oe, nlm, nhm, 5, 11, byvar, nWeight, mfp), all.x=TRUE)
    rwC.oe <- merge(agelist, oe.est.byvar(data_oe, nlm, 95, 7, 12, byvar, nWeight, mfp), all.x=TRUE)
    rdC.oe <- merge(agelist, oe.est.byvar(data_oe, nlm, 95, 8, 13, byvar, nWeight, mfp), all.x=TRUE)
    nmtoC.oe <- merge(agelist, oe.est.byvar(data_oe, nlm, 95, 1, 5, byvar, nWeight, mfp), all.x=TRUE)
    wtoC.oe <- merge(agelist, oe.est.byvar(data_oe, nlm, 95, 3, 6, byvar, nWeight, mfp), all.x=TRUE)
    dtoC.oe <- merge(agelist, oe.est.byvar(data_oe, nlm, 95, 4, 7, byvar, nWeight, mfp), all.x=TRUE)
    Ctonm.oe <- merge(agelist, oe.est.byvar(data_oe, nlm, 95, 5, 8, byvar, nWeight, mfp), all.x=TRUE)
    Ctow.oe <- merge(agelist, oe.est.byvar(data_oe, nlm, 95, 7, 9, byvar, nWeight, mfp), all.x=TRUE)
    Ctod.oe <- merge(agelist, oe.est.byvar(data_oe, nlm, 95, 8, 10, byvar, nWeight, mfp), all.x=TRUE)

  }

  if(cal_freq == TRUE){

    agelist <- null.rates(data_freq, nlm, 95, byvar)

    mrg1.freq <- merge(agelist, freq.est.byvar(data_freq, nlm, nhm, 1, byvar, nWeight, mfp), all.x=TRUE)
    divorced.freq <- merge(agelist, freq.est.byvar(data_freq, nlm, 95, 2, byvar, nWeight, mfp), all.x=TRUE)
    rw.freq <- merge(agelist, freq.est.byvar(data_freq, nlm, 95, 3, byvar, nWeight, mfp), all.x=TRUE)
    rd.freq <- merge(agelist, freq.est.byvar(data_freq, nlm, 95, 4, byvar, nWeight, mfp), all.x=TRUE)

    nmCm.freq <- merge(agelist, freq.est.byvar(data_freq, nlm, nhm, 11, byvar, nWeight, mfp), all.x=TRUE)
    rwC.freq <- merge(agelist, freq.est.byvar(data_freq, nlm, 95, 12, byvar, nWeight, mfp), all.x=TRUE)
    rdC.freq <- merge(agelist, freq.est.byvar(data_freq, nlm, 95, 13, byvar, nWeight, mfp), all.x=TRUE)
    nmtoC.freq <- merge(agelist, freq.est.byvar(data_freq, nlm, 95, 5, byvar, nWeight, mfp), all.x=TRUE)
    wtoC.freq <- merge(agelist, freq.est.byvar(data_freq, nlm, 95, 6, byvar, nWeight, mfp), all.x=TRUE)
    dtoC.freq <- merge(agelist, freq.est.byvar(data_freq, nlm, 95, 7, byvar, nWeight, mfp), all.x=TRUE)
    Ctonm.freq <- merge(agelist, freq.est.byvar(data_freq, nlm, 95, 8, byvar, nWeight, mfp), all.x=TRUE)
    Ctow.freq <- merge(agelist, freq.est.byvar(data_freq, nlm, 95, 9, byvar, nWeight, mfp), all.x=TRUE)
    Ctod.freq <- merge(agelist, freq.est.byvar(data_freq, nlm, 95, 10, byvar, nWeight, mfp), all.x=TRUE)

  }

  if(plot == TRUE){
    result.list <- list(mrg1.oe=mrg1.oe, mrg1.freq=mrg1.freq,
                        divorced.oe=divorced.oe, divorced.freq=divorced.freq,
                        rw.oe=rw.oe, rw.freq=rw.freq, rd.oe=rd.oe, rd.freq=rd.freq,
                        nmCm.oe=nmCm.oe, nmCm.freq=nmCm.freq,
                        rwC.oe=rwC.oe, rwC.freq=rwC.freq,
                        rdC.oe=rdC.oe, rdC.freq=rdC.freq,
                        nmtoC.oe=nmtoC.oe, nmtoC.freq=nmtoC.freq,
                        wtoC.oe=wtoC.oe, wtoC.freq=wtoC.freq,
                        dtoC.oe=dtoC.oe, dtoC.freq=dtoC.freq,
                        Ctonm.oe=Ctonm.oe, Ctonm.freq=Ctonm.freq,
                        Ctow.oe=Ctow.oe, Ctow.freq=Ctow.freq,
                        Ctod.oe=Ctod.oe, Ctod.freq=Ctod.freq)

    draw.mar7.nRate(result.list, code, nlm, 95, title, period,
                    plot.name, byvar, cal_oe, cal_freq)
  }

  if (byvar!="reg.ru.nosex"&byvar!="region.nosex"&byvar!="ru.nosex"&byvar!="sex.nosex") {

    agelist <- arrange.covar(agelist, byvar)
    agelist <- agelist %>% select(-sex) %>% distinct()
    rates.by <- agelist

    if(cal_oe == TRUE){

      oe.rates.m <- data.frame(rates.by,
                               nmNC.married=mrg1.oe[which(mrg1.oe$sex==1), "est.rates"],
                               nmC.married=nmCm.oe[which(nmCm.oe$sex==1), "est.rates"],
                               divorced=divorced.oe[which(divorced.oe$sex==1), "est.rates"],
                               rwNC=rw.oe[which(rw.oe$sex==1), "est.rates"],
                               rwC=rwC.oe[which(rwC.oe$sex==1), "est.rates"],
                               rdNC=rd.oe[which(rd.oe$sex==1), "est.rates"],
                               rdC=rdC.oe[which(rdC.oe$sex==1), "est.rates"],
                               nmtoC=nmtoC.oe[which(nmtoC.oe$sex==1), "est.rates"],
                               wtoC=wtoC.oe[which(wtoC.oe$sex==1), "est.rates"],
                               dtoC=dtoC.oe[which(dtoC.oe$sex==1), "est.rates"],
                               Ctonm=Ctonm.oe[which(Ctonm.oe$sex==1), "est.rates"],
                               Ctow=Ctow.oe[which(Ctow.oe$sex==1), "est.rates"],
                               Ctod=Ctod.oe[which(Ctod.oe$sex==1), "est.rates"])

      oe.rates.f <- data.frame(rates.by,
                               nmNC.married=mrg1.oe[which(mrg1.oe$sex==2), "est.rates"],
                               nmC.married=nmCm.oe[which(nmCm.oe$sex==2), "est.rates"],
                               divorced=divorced.oe[which(divorced.oe$sex==2), "est.rates"],
                               rwNC=rw.oe[which(rw.oe$sex==2), "est.rates"],
                               rwC=rwC.oe[which(rwC.oe$sex==2), "est.rates"],
                               rdNC=rd.oe[which(rd.oe$sex==2), "est.rates"],
                               rdC=rdC.oe[which(rdC.oe$sex==2), "est.rates"],
                               nmtoC=nmtoC.oe[which(nmtoC.oe$sex==2), "est.rates"],
                               wtoC=wtoC.oe[which(wtoC.oe$sex==2), "est.rates"],
                               dtoC=dtoC.oe[which(dtoC.oe$sex==2), "est.rates"],
                               Ctonm=Ctonm.oe[which(Ctonm.oe$sex==2), "est.rates"],
                               Ctow=Ctow.oe[which(Ctow.oe$sex==2), "est.rates"],
                               Ctod=Ctod.oe[which(Ctod.oe$sex==2), "est.rates"])

      raw.oe.rates.m <- data.frame(rates.by,
                                   nmNC.married=mrg1.oe[which(mrg1.oe$sex==1), "raw.rates"],
                                   nmC.married=nmCm.oe[which(nmCm.oe$sex==1), "raw.rates"],
                                   divorced=divorced.oe[which(divorced.oe$sex==1), "raw.rates"],
                                   rwNC=rw.oe[which(rw.oe$sex==1), "raw.rates"],
                                   rwC=rwC.oe[which(rwC.oe$sex==1), "raw.rates"],
                                   rdNC=rd.oe[which(rd.oe$sex==1), "raw.rates"],
                                   rdC=rdC.oe[which(rdC.oe$sex==1), "raw.rates"],
                                   nmtoC=nmtoC.oe[which(nmtoC.oe$sex==1), "raw.rates"],
                                   wtoC=wtoC.oe[which(wtoC.oe$sex==1), "raw.rates"],
                                   dtoC=dtoC.oe[which(dtoC.oe$sex==1), "raw.rates"],
                                   Ctonm=Ctonm.oe[which(Ctonm.oe$sex==1), "raw.rates"],
                                   Ctow=Ctow.oe[which(Ctow.oe$sex==1), "raw.rates"],
                                   Ctod=Ctod.oe[which(Ctod.oe$sex==1), "raw.rates"])

      raw.oe.rates.f <- data.frame(rates.by,
                                   nmNC.married=mrg1.oe[which(mrg1.oe$sex==2), "raw.rates"],
                                   nmC.married=nmCm.oe[which(nmCm.oe$sex==2), "raw.rates"],
                                   divorced=divorced.oe[which(divorced.oe$sex==2), "raw.rates"],
                                   rwNC=rw.oe[which(rw.oe$sex==2), "raw.rates"],
                                   rwC=rwC.oe[which(rwC.oe$sex==2), "raw.rates"],
                                   rdNC=rd.oe[which(rd.oe$sex==2), "raw.rates"],
                                   rdC=rdC.oe[which(rdC.oe$sex==2), "raw.rates"],
                                   nmtoC=nmtoC.oe[which(nmtoC.oe$sex==2), "raw.rates"],
                                   wtoC=wtoC.oe[which(wtoC.oe$sex==2), "raw.rates"],
                                   dtoC=dtoC.oe[which(dtoC.oe$sex==2), "raw.rates"],
                                   Ctonm=Ctonm.oe[which(Ctonm.oe$sex==2), "raw.rates"],
                                   Ctow=Ctow.oe[which(Ctow.oe$sex==2), "raw.rates"],
                                   Ctod=Ctod.oe[which(Ctod.oe$sex==2), "raw.rates"])

    }

    if(cal_freq == TRUE){

      frequency.m <- data.frame(rates.by,
                                nmNC.married=mrg1.freq[which(mrg1.freq$sex==1), "est.rates"],
                                nmC.married=nmCm.freq[which(nmCm.freq$sex==1), "est.rates"],
                                divorced=divorced.freq[which(divorced.freq$sex==1), "est.rates"],
                                rwNC=rw.freq[which(rw.freq$sex==1), "est.rates"],
                                rwC=rwC.freq[which(rwC.freq$sex==1), "est.rates"],
                                rdNC=rd.freq[which(rd.freq$sex==1), "est.rates"],
                                rdC=rdC.freq[which(rdC.freq$sex==1), "est.rates"],
                                nmtoC=nmtoC.freq[which(nmtoC.freq$sex==1), "est.rates"],
                                wtoC=wtoC.freq[which(wtoC.freq$sex==1), "est.rates"],
                                dtoC=dtoC.freq[which(dtoC.freq$sex==1), "est.rates"],
                                Ctonm=Ctonm.freq[which(Ctonm.freq$sex==1), "est.rates"],
                                Ctow=Ctow.freq[which(Ctow.freq$sex==1), "est.rates"],
                                Ctod=Ctod.freq[which(Ctod.freq$sex==1), "est.rates"])

      frequency.f <- data.frame(rates.by,
                                nmNC.married=mrg1.freq[which(mrg1.freq$sex==2), "est.rates"],
                                nmC.married=nmCm.freq[which(nmCm.freq$sex==2), "est.rates"],
                                divorced=divorced.freq[which(divorced.freq$sex==2), "est.rates"],
                                rwNC=rw.freq[which(rw.freq$sex==2), "est.rates"],
                                rwC=rwC.freq[which(rwC.freq$sex==2), "est.rates"],
                                rdNC=rd.freq[which(rd.freq$sex==2), "est.rates"],
                                rdC=rdC.freq[which(rdC.freq$sex==2), "est.rates"],
                                nmtoC=nmtoC.freq[which(nmtoC.freq$sex==2), "est.rates"],
                                wtoC=wtoC.freq[which(wtoC.freq$sex==2), "est.rates"],
                                dtoC=dtoC.freq[which(dtoC.freq$sex==2), "est.rates"],
                                Ctonm=Ctonm.freq[which(Ctonm.freq$sex==2), "est.rates"],
                                Ctow=Ctow.freq[which(Ctow.freq$sex==2), "est.rates"],
                                Ctod=Ctod.freq[which(Ctod.freq$sex==2), "est.rates"])

      raw.frequency.m <- data.frame(rates.by,
                                    nmNC.married=mrg1.freq[which(mrg1.freq$sex==1), "raw.rates"],
                                    nmC.married=nmCm.freq[which(nmCm.freq$sex==1), "raw.rates"],
                                    divorced=divorced.freq[which(divorced.freq$sex==1), "raw.rates"],
                                    rwNC=rw.freq[which(rw.freq$sex==1), "raw.rates"],
                                    rwC=rwC.freq[which(rwC.freq$sex==1), "raw.rates"],
                                    rdNC=rd.freq[which(rd.freq$sex==1), "raw.rates"],
                                    rdC=rdC.freq[which(rdC.freq$sex==1), "raw.rates"],
                                    nmtoC=nmtoC.freq[which(nmtoC.freq$sex==1), "raw.rates"],
                                    wtoC=wtoC.freq[which(wtoC.freq$sex==1), "raw.rates"],
                                    dtoC=dtoC.freq[which(dtoC.freq$sex==1), "raw.rates"],
                                    Ctonm=Ctonm.freq[which(Ctonm.freq$sex==1), "raw.rates"],
                                    Ctow=Ctow.freq[which(Ctow.freq$sex==1), "raw.rates"],
                                    Ctod=Ctod.freq[which(Ctod.freq$sex==1), "raw.rates"])

      raw.frequency.f <- data.frame(rates.by,
                                    nmNC.married=mrg1.freq[which(mrg1.freq$sex==2), "raw.rates"],
                                    nmC.married=nmCm.freq[which(nmCm.freq$sex==2), "raw.rates"],
                                    divorced=divorced.freq[which(divorced.freq$sex==2), "raw.rates"],
                                    rwNC=rw.freq[which(rw.freq$sex==2), "raw.rates"],
                                    rwC=rwC.freq[which(rwC.freq$sex==2), "raw.rates"],
                                    rdNC=rd.freq[which(rd.freq$sex==2), "raw.rates"],
                                    rdC=rdC.freq[which(rdC.freq$sex==2), "raw.rates"],
                                    nmtoC=nmtoC.freq[which(nmtoC.freq$sex==2), "raw.rates"],
                                    wtoC=wtoC.freq[which(wtoC.freq$sex==2), "raw.rates"],
                                    dtoC=dtoC.freq[which(dtoC.freq$sex==2), "raw.rates"],
                                    Ctonm=Ctonm.freq[which(Ctonm.freq$sex==2), "raw.rates"],
                                    Ctow=Ctow.freq[which(Ctow.freq$sex==2), "raw.rates"],
                                    Ctod=Ctod.freq[which(Ctod.freq$sex==2), "raw.rates"])

    }

  } else if (byvar=="reg.ru.nosex"){

    agelist <- agelist %>% arrange(age, region, ru)
    rates.by <- data.frame(age=agelist$age, region=agelist$region, ru=agelist$ru)

    if(cal_oe == TRUE){

      oe.rates.m <- data.frame(rates.by,
                               nmNC.married=mrg1.oe[ , "est.rates"],
                               nmC.married=nmCm.oe[ , "est.rates"],
                               divorced=divorced.oe[ , "est.rates"],
                               rwNC=rw.oe[ , "est.rates"],
                               rwC=rwC.oe[ , "est.rates"],
                               rdNC=rd.oe[ , "est.rates"],
                               rdC=rdC.oe[ , "est.rates"],
                               nmtoC=nmtoC.oe[ , "est.rates"],
                               wtoC=wtoC.oe[ , "est.rates"],
                               dtoC=dtoC.oe[ , "est.rates"],
                               Ctonm=Ctonm.oe[ , "est.rates"],
                               Ctow=Ctow.oe[ , "est.rates"],
                               Ctod=Ctod.oe[ , "est.rates"])

      raw.oe.rates.m <- data.frame(rates.by,
                                   nmNC.married=mrg1.oe[ , "raw.rates"],
                                   nmC.married=nmCm.oe[ , "raw.rates"],
                                   divorced=divorced.oe[ , "raw.rates"],
                                   rwNC=rw.oe[ , "raw.rates"],
                                   rwC=rwC.oe[ , "raw.rates"],
                                   rdNC=rd.oe[ , "raw.rates"],
                                   rdC=rdC.oe[ , "raw.rates"],
                                   nmtoC=nmtoC.oe[ , "raw.rates"],
                                   wtoC=wtoC.oe[ , "raw.rates"],
                                   dtoC=dtoC.oe[ , "raw.rates"],
                                   Ctonm=Ctonm.oe[ , "raw.rates"],
                                   Ctow=Ctow.oe[ , "raw.rates"],
                                   Ctod=Ctod.oe[ , "raw.rates"])
    }

    if(cal_freq == TRUE){

      frequency.m <- data.frame(rates.by,
                                nmNC.married=mrg1.freq[ , "est.rates"],
                                nmC.married=nmCm.freq[ , "est.rates"],
                                divorced=divorced.freq[ , "est.rates"],
                                rwNC=rw.freq[ , "est.rates"],
                                rwC=rwC.freq[ , "est.rates"],
                                rdNC=rd.freq[ , "est.rates"],
                                rdC=rdC.freq[ , "est.rates"],
                                nmtoC=nmtoC.freq[ , "est.rates"],
                                wtoC=wtoC.freq[ , "est.rates"],
                                dtoC=dtoC.freq[ , "est.rates"],
                                Ctonm=Ctonm.freq[ , "est.rates"],
                                Ctow=Ctow.freq[ , "est.rates"],
                                Ctod=Ctod.freq[ , "est.rates"])

      raw.frequency.m <- data.frame(rates.by,
                                    nmNC.married=mrg1.freq[ , "raw.rates"],
                                    nmC.married=nmCm.freq[ , "raw.rates"],
                                    divorced=divorced.freq[ , "raw.rates"],
                                    rwNC=rw.freq[ , "raw.rates"],
                                    rwC=rwC.freq[ , "raw.rates"],
                                    rdNC=rd.freq[ , "raw.rates"],
                                    rdC=rdC.freq[ , "raw.rates"],
                                    nmtoC=nmtoC.freq[ , "raw.rates"],
                                    wtoC=wtoC.freq[ , "raw.rates"],
                                    dtoC=dtoC.freq[ , "raw.rates"],
                                    Ctonm=Ctonm.freq[ , "raw.rates"],
                                    Ctow=Ctow.freq[ , "raw.rates"],
                                    Ctod=Ctod.freq[ , "raw.rates"])
    }

  } else if (byvar=="region.nosex"){

    agelist <- agelist %>% arrange(age, region)
    rates.by <- data.frame(age=agelist$age, region=agelist$region)

    if(cal_oe == TRUE){

      oe.rates.m <- data.frame(rates.by,
                               nmNC.married=mrg1.oe[ , "est.rates"],
                               nmC.married=nmCm.oe[ , "est.rates"],
                               divorced=divorced.oe[ , "est.rates"],
                               rwNC=rw.oe[ , "est.rates"],
                               rwC=rwC.oe[ , "est.rates"],
                               rdNC=rd.oe[ , "est.rates"],
                               rdC=rdC.oe[ , "est.rates"],
                               nmtoC=nmtoC.oe[ , "est.rates"],
                               wtoC=wtoC.oe[ , "est.rates"],
                               dtoC=dtoC.oe[ , "est.rates"],
                               Ctonm=Ctonm.oe[ , "est.rates"],
                               Ctow=Ctow.oe[ , "est.rates"],
                               Ctod=Ctod.oe[ , "est.rates"])

      raw.oe.rates.m <- data.frame(rates.by,
                                   nmNC.married=mrg1.oe[ , "raw.rates"],
                                   nmC.married=nmCm.oe[ , "raw.rates"],
                                   divorced=divorced.oe[ , "raw.rates"],
                                   rwNC=rw.oe[ , "raw.rates"],
                                   rwC=rwC.oe[ , "raw.rates"],
                                   rdNC=rd.oe[ , "raw.rates"],
                                   rdC=rdC.oe[ , "raw.rates"],
                                   nmtoC=nmtoC.oe[ , "raw.rates"],
                                   wtoC=wtoC.oe[ , "raw.rates"],
                                   dtoC=dtoC.oe[ , "raw.rates"],
                                   Ctonm=Ctonm.oe[ , "raw.rates"],
                                   Ctow=Ctow.oe[ , "raw.rates"],
                                   Ctod=Ctod.oe[ , "raw.rates"])
    }

    if(cal_freq == TRUE){

      frequency.m <- data.frame(rates.by,
                                nmNC.married=mrg1.freq[ , "est.rates"],
                                nmC.married=nmCm.freq[ , "est.rates"],
                                divorced=divorced.freq[ , "est.rates"],
                                rwNC=rw.freq[ , "est.rates"],
                                rwC=rwC.freq[ , "est.rates"],
                                rdNC=rd.freq[ , "est.rates"],
                                rdC=rdC.freq[ , "est.rates"],
                                nmtoC=nmtoC.freq[ , "est.rates"],
                                wtoC=wtoC.freq[ , "est.rates"],
                                dtoC=dtoC.freq[ , "est.rates"],
                                Ctonm=Ctonm.freq[ , "est.rates"],
                                Ctow=Ctow.freq[ , "est.rates"],
                                Ctod=Ctod.freq[ , "est.rates"])

      raw.frequency.m <- data.frame(rates.by,
                                    nmNC.married=mrg1.freq[ , "raw.rates"],
                                    nmC.married=nmCm.freq[ , "raw.rates"],
                                    divorced=divorced.freq[ , "raw.rates"],
                                    rwNC=rw.freq[ , "raw.rates"],
                                    rwC=rwC.freq[ , "raw.rates"],
                                    rdNC=rd.freq[ , "raw.rates"],
                                    rdC=rdC.freq[ , "raw.rates"],
                                    nmtoC=nmtoC.freq[ , "raw.rates"],
                                    wtoC=wtoC.freq[ , "raw.rates"],
                                    dtoC=dtoC.freq[ , "raw.rates"],
                                    Ctonm=Ctonm.freq[ , "raw.rates"],
                                    Ctow=Ctow.freq[ , "raw.rates"],
                                    Ctod=Ctod.freq[ , "raw.rates"])
    }

  } else if (byvar=="ru.nosex"){

    agelist <- agelist %>% arrange(age, ru)
    rates.by <- data.frame(age=agelist$age, ru=agelist$ru)

    if(cal_oe == TRUE){

      oe.rates.m <- data.frame(rates.by,
                               nmNC.married=mrg1.oe[ , "est.rates"],
                               nmC.married=nmCm.oe[ , "est.rates"],
                               divorced=divorced.oe[ , "est.rates"],
                               rwNC=rw.oe[ , "est.rates"],
                               rwC=rwC.oe[ , "est.rates"],
                               rdNC=rd.oe[ , "est.rates"],
                               rdC=rdC.oe[ , "est.rates"],
                               nmtoC=nmtoC.oe[ , "est.rates"],
                               wtoC=wtoC.oe[ , "est.rates"],
                               dtoC=dtoC.oe[ , "est.rates"],
                               Ctonm=Ctonm.oe[ , "est.rates"],
                               Ctow=Ctow.oe[ , "est.rates"],
                               Ctod=Ctod.oe[ , "est.rates"])

      raw.oe.rates.m <- data.frame(rates.by,
                                   nmNC.married=mrg1.oe[ , "raw.rates"],
                                   nmC.married=nmCm.oe[ , "raw.rates"],
                                   divorced=divorced.oe[ , "raw.rates"],
                                   rwNC=rw.oe[ , "raw.rates"],
                                   rwC=rwC.oe[ , "raw.rates"],
                                   rdNC=rd.oe[ , "raw.rates"],
                                   rdC=rdC.oe[ , "raw.rates"],
                                   nmtoC=nmtoC.oe[ , "raw.rates"],
                                   wtoC=wtoC.oe[ , "raw.rates"],
                                   dtoC=dtoC.oe[ , "raw.rates"],
                                   Ctonm=Ctonm.oe[ , "raw.rates"],
                                   Ctow=Ctow.oe[ , "raw.rates"],
                                   Ctod=Ctod.oe[ , "raw.rates"])
    }

    if(cal_freq == TRUE){

      frequency.m <- data.frame(rates.by,
                                nmNC.married=mrg1.freq[ , "est.rates"],
                                nmC.married=nmCm.freq[ , "est.rates"],
                                divorced=divorced.freq[ , "est.rates"],
                                rwNC=rw.freq[ , "est.rates"],
                                rwC=rwC.freq[ , "est.rates"],
                                rdNC=rd.freq[ , "est.rates"],
                                rdC=rdC.freq[ , "est.rates"],
                                nmtoC=nmtoC.freq[ , "est.rates"],
                                wtoC=wtoC.freq[ , "est.rates"],
                                dtoC=dtoC.freq[ , "est.rates"],
                                Ctonm=Ctonm.freq[ , "est.rates"],
                                Ctow=Ctow.freq[ , "est.rates"],
                                Ctod=Ctod.freq[ , "est.rates"])

      raw.frequency.m <- data.frame(rates.by,
                                    nmNC.married=mrg1.freq[ , "raw.rates"],
                                    nmC.married=nmCm.freq[ , "raw.rates"],
                                    divorced=divorced.freq[ , "raw.rates"],
                                    rwNC=rw.freq[ , "raw.rates"],
                                    rwC=rwC.freq[ , "raw.rates"],
                                    rdNC=rd.freq[ , "raw.rates"],
                                    rdC=rdC.freq[ , "raw.rates"],
                                    nmtoC=nmtoC.freq[ , "raw.rates"],
                                    wtoC=wtoC.freq[ , "raw.rates"],
                                    dtoC=dtoC.freq[ , "raw.rates"],
                                    Ctonm=Ctonm.freq[ , "raw.rates"],
                                    Ctow=Ctow.freq[ , "raw.rates"],
                                    Ctod=Ctod.freq[ , "raw.rates"])
    }

  } else if (byvar=="sex.nosex"){

    agelist <- agelist %>% arrange(age)
    rates.by <- data.frame(age=agelist$age)

    if(cal_oe == TRUE){

      oe.rates.m <- data.frame(rates.by,
                               nmNC.married=mrg1.oe[ , "est.rates"],
                               nmC.married=nmCm.oe[ , "est.rates"],
                               divorced=divorced.oe[ , "est.rates"],
                               rwNC=rw.oe[ , "est.rates"],
                               rwC=rwC.oe[ , "est.rates"],
                               rdNC=rd.oe[ , "est.rates"],
                               rdC=rdC.oe[ , "est.rates"],
                               nmtoC=nmtoC.oe[ , "est.rates"],
                               wtoC=wtoC.oe[ , "est.rates"],
                               dtoC=dtoC.oe[ , "est.rates"],
                               Ctonm=Ctonm.oe[ , "est.rates"],
                               Ctow=Ctow.oe[ , "est.rates"],
                               Ctod=Ctod.oe[ , "est.rates"])

      raw.oe.rates.m <- data.frame(rates.by,
                                   nmNC.married=mrg1.oe[ , "raw.rates"],
                                   nmC.married=nmCm.oe[ , "raw.rates"],
                                   divorced=divorced.oe[ , "raw.rates"],
                                   rwNC=rw.oe[ , "raw.rates"],
                                   rwC=rwC.oe[ , "raw.rates"],
                                   rdNC=rd.oe[ , "raw.rates"],
                                   rdC=rdC.oe[ , "raw.rates"],
                                   nmtoC=nmtoC.oe[ , "raw.rates"],
                                   wtoC=wtoC.oe[ , "raw.rates"],
                                   dtoC=dtoC.oe[ , "raw.rates"],
                                   Ctonm=Ctonm.oe[ , "raw.rates"],
                                   Ctow=Ctow.oe[ , "raw.rates"],
                                   Ctod=Ctod.oe[ , "raw.rates"])
    }

    if(cal_freq == TRUE){

      frequency.m <- data.frame(rates.by,
                                nmNC.married=mrg1.freq[ , "est.rates"],
                                nmC.married=nmCm.freq[ , "est.rates"],
                                divorced=divorced.freq[ , "est.rates"],
                                rwNC=rw.freq[ , "est.rates"],
                                rwC=rwC.freq[ , "est.rates"],
                                rdNC=rd.freq[ , "est.rates"],
                                rdC=rdC.freq[ , "est.rates"],
                                nmtoC=nmtoC.freq[ , "est.rates"],
                                wtoC=wtoC.freq[ , "est.rates"],
                                dtoC=dtoC.freq[ , "est.rates"],
                                Ctonm=Ctonm.freq[ , "est.rates"],
                                Ctow=Ctow.freq[ , "est.rates"],
                                Ctod=Ctod.freq[ , "est.rates"])

      raw.frequency.m <- data.frame(rates.by,
                                    nmNC.married=mrg1.freq[ , "raw.rates"],
                                    nmC.married=nmCm.freq[ , "raw.rates"],
                                    divorced=divorced.freq[ , "raw.rates"],
                                    rwNC=rw.freq[ , "raw.rates"],
                                    rwC=rwC.freq[ , "raw.rates"],
                                    rdNC=rd.freq[ , "raw.rates"],
                                    rdC=rdC.freq[ , "raw.rates"],
                                    nmtoC=nmtoC.freq[ , "raw.rates"],
                                    wtoC=wtoC.freq[ , "raw.rates"],
                                    dtoC=dtoC.freq[ , "raw.rates"],
                                    Ctonm=Ctonm.freq[ , "raw.rates"],
                                    Ctow=Ctow.freq[ , "raw.rates"],
                                    Ctod=Ctod.freq[ , "raw.rates"])
    }


  }

  return(list(oe.rates.m=oe.rates.m, frequency.m=frequency.m,
              oe.rates.f=oe.rates.f, frequency.f=frequency.f,
              raw.oe.rates.m = raw.oe.rates.m, raw.frequency.m = raw.frequency.m,
              raw.oe.rates.f = raw.oe.rates.f, raw.frequency.f = raw.frequency.f))
}

##------Prepare Data-------
data.prepare.mar <- function(data, marital, t1Month, t2Month, nlm, nhm){

  info <- data %>% select(region:events)
  evt <- data %>% select(ID, m1:ncol(data))
  long <- reshape(evt, idvar = "ID",
                  varying = names(evt)[2:ncol(evt)],
                  v.names = c("event", "m"),
                  timevar = "event_index",
                  times = 1:((ncol(evt)-1)/2),
                  new.row.names = 1:10000000,
                  direction = "long") %>% arrange(ID, event_index)
  names(long)[3:4] <- names(long)[4:3]

  data <- long %>% arrange(ID, m, event) %>%
    group_by(ID) %>%
    mutate(new_index = row_number()) %>%
    filter(rowSums(is.na(cbind(event, m)))<2 | new_index==1)
  data <- data %>% left_join(info, by = "ID")
  rm(long)
  rm(evt)

  if (marital==4) {
    data <- mar4.t2(data, t2Month)
  } else if (marital==7) {data <- mar7.t2(data, t2Month)}

  data <- data %>% select(region, ID, weight, race, ru, sex, mar, mar2, edu, tMonth, bMonth,
                          events, m, event, new_index) %>%
    pivot_wider(names_from = c(new_index), values_from = c(m, event), names_sep = "")

  data[, "t"] <-  round(t2Month - data$bMonth + 1, 0)
  data[, "t0"] <-  round(t1Month - data$bMonth, 0)

  data[,"fake.event"] <- 1
  data <- survSplit(Surv(data$t0, data$t, data$fake.event)~., data, cut=seq(min(data$t0),max(data$t),1),
                    start="t0", end="t")
  data[,"period"] <- data$bMonth + data$t0
  data[,"year"] <- (data$period-1)%/%12+1900

  data$age <- floor(data$t0/12)
  data <- data[which(data$age >= nlm & data$age <= nhm),]

  data$py <- (data$t - data$t0)/12

  data$event <- NA
  for(i in 1:max(data$events)){
    event_m <- paste0("m", i, sep="")
    temp <- cbind(data[, event_m], data$period)
    event_type <- paste0("event", i, sep="")

    same.m <- data[which(temp[,1] == temp[,2]&!is.na(data$event)), ]

    same.m[, "event"] <- data[which(temp[,1] == temp[,2]&!is.na(data$event)), event_type]
    data[which(temp[,1] == temp[,2]&!is.na(data$event)), "t"] <- data[which(temp[,1] == temp[,2]&!is.na(data$event)), "t0"]
    data[which(temp[,1] == temp[,2]&!is.na(data$event)), "py"] <- data[which(temp[,1] == temp[,2]&!is.na(data$event)), "t"]-data[which(temp[,1] == temp[,2]&!is.na(data$event)), "t0"]
    #event1 type
    data[which(temp[,1] == temp[,2]&is.na(data$event)), "event"] <- data[which(temp[,1] == temp[,2]&is.na(data$event)), event_type]

    data <- rbind(data, same.m) %>% arrange(ID, t0)
  }

  data[which(data$event > 17), "event"] <- NA_real_
  data[which(is.na(data$event)), "event"] <- 0

  #status
  data$prev <- data$post <- NA_real_
  data[which(t2Month == (data$period)), "prev"] <- data[which(t2Month == (data$period)), "mar2"]
  data[which(data$t0==(nhm*12+11)&is.na(data$prev)), "prev"] <- data[which(data$t0==(nhm*12+11)&is.na(data$prev)), "mar2"]

  data[which(data$event == 1), "post"] <- 2
  data[which(data$event == 2), "post"] <- 4
  data[which(data$event == 3), "post"] <- 2
  data[which(data$event == 4), "post"] <- 2
  data[which(data$event == 17), "post"] <- 3
  data[which(data$event == 5), "post"] <- 5
  data[which(data$event == 6), "post"] <- 7
  data[which(data$event == 7), "post"] <- 8
  data[which(data$event == 8), "post"] <- 1
  data[which(data$event == 9), "post"] <- 3
  data[which(data$event == 10), "post"] <- 4
  data[which(data$event == 11), "post"] <- 2
  data[which(data$event == 12), "post"] <- 2
  data[which(data$event == 13), "post"] <- 2

  data[which(data$event == 1), "prev"] <- 1
  data[which(data$event == 2), "prev"] <- 2
  data[which(data$event == 3), "prev"] <- 3
  data[which(data$event == 4), "prev"] <- 4
  data[which(data$event == 17), "prev"] <- 2

  data[which(data$event == 5), "prev"] <- 1
  data[which(data$event == 6), "prev"] <- 3
  data[which(data$event == 7), "prev"] <- 4
  data[which(data$event == 8), "prev"] <- 5
  data[which(data$event == 9), "prev"] <- 7
  data[which(data$event == 10), "prev"] <- 8
  data[which(data$event == 11), "prev"] <- 5
  data[which(data$event == 12), "prev"] <- 7
  data[which(data$event == 13), "prev"] <- 8

  data <- as.data.frame(data %>% group_by(ID) %>% fill(post, .direction="down") %>%
                          fill(prev, .direction="up"))

  data[which(is.na(data$post)), "post"] <- data[which(is.na(data$post)), "prev"]

  data <- data[which(data$period <= data$tMonth),]

  data <- data[, c("region", "ID", "weight", "race", "ru", "sex", "edu", "age", "event", "prev", "post", "py", "year")]

  return(data)
}

data.prepare.mar.py <- function(data, marital, t1Month, t2Month, nlm, nhm){

  info <- data %>% select(region:events)
  evt <- data %>% select(ID, m1:ncol(data))
  long <- reshape(evt, idvar = "ID",
                  varying = names(evt)[2:ncol(evt)],
                  v.names = c("event", "m"),
                  timevar = "event_index",
                  times = 1:((ncol(evt)-1)/2),
                  new.row.names = 1:10000000,
                  direction = "long") %>% arrange(ID, event_index)

  names(long)[3:4] <- names(long)[4:3]

  long <- long %>% arrange(ID, m, event) %>%
    group_by(ID) %>%
    mutate(new_index = row_number()) %>%
    filter(rowSums(is.na(cbind(event, m)))<2 | new_index==1)

  long <- long %>% left_join(info, by = "ID")
  rm(evt)

  if (marital==4) {
    long <- mar4.t2(long, t2Month)
  } else if (marital==7) {long <- mar7.t2(long, t2Month)}

  data <- long %>% select(region, ID, weight, race, ru, sex, mar, mar2, edu, tMonth, bMonth,
                          events, m, event, new_index) %>%
    pivot_wider(names_from = c(new_index), values_from = c(m, event), names_sep = "")

  #Generate an event-age dataframe
  evt <- long %>%
    mutate(event = replace(event, which(m<t1Month|m>t2Month|m<(nlm*12+bMonth)|m>(nhm*12+bMonth+12)), NA)) %>%
    mutate(m = replace(m, which(m<t1Month|m>t2Month|m<(nlm*12+bMonth)|m>(nhm*12+bMonth+12)), NA)) %>%
    filter(rowSums(is.na(cbind(event, m)))<2)
  evt$t0 <- (evt$m-evt$bMonth)/12
  evt$t <- ifelse((evt$t0==ceiling(evt$t0)&(ceiling(evt$t0)+1)>(t2Month+1-evt$bMonth)/12)|ceiling(evt$t0)>(t2Month+1-evt$bMonth)/12, (t2Month+1-evt$bMonth)/12,
                  ifelse(ceiling(evt$t0)==evt$t0, ceiling(evt$t0)+1, ceiling(evt$t0)))
  evt$age <- floor(evt$t0)

  #Generate a survival dataframe
  surv <- data %>% select(region:events)
  surv$event_index <- NA
  surv$m <- NA
  surv$event <- NA
  surv$new_index <- NA

  surv <- surv %>% relocate(event_index, m, event, new_index, region, .after = "ID")
  surv[, "t"] <-  (t2Month - surv$bMonth + 1)/12
  surv[, "age_t"] <- (surv$tMonth - surv$bMonth + 1)/12
  surv$t <- ifelse(surv$t<surv$age_t, surv$t, surv$age_t)
  surv[, "t0"] <-  (t1Month - surv$bMonth)/12
  surv[, "fake.event"] <- 1
  surv <- survSplit(Surv(surv$t0, surv$t, surv$fake.event)~., surv,
                    cut=seq(nlm, nhm+1, 1),
                    start="t0", end="t")
  surv[, "age"] <- floor(surv$t0)
  surv$event <- NA
  surv <- surv[-which(surv$age < nlm | surv$age > nhm+1),]

  #------------------------ to do
  ##Combine evt and surv together (double check)

  #把第一个事件拿出来
  #起始t0不为整数
  evt_1 <- evt %>% group_by(ID) %>%
    arrange(ID, new_index) %>%
    mutate(new_index = row_number()) %>%
    filter(t0 != floor(t0) & new_index==1 & t0!=(t1Month-bMonth)/12)
  evt_1[, 2:5] <- NA
  evt_1 <- as.data.frame(evt_1)
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
    relocate(t0, t, age, .after = "event") %>%
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

  combine <- combine[which(combine$age >= nlm & combine$age <= nhm+1),] %>%
    select(ID:age)
  data <- combine %>% left_join(data, by="ID")
  data$py <- (data$t - data$t0)

  data[,"t2Month_y"] <- (t2Month-data$bMonth+1)/12

  data[which(data$event > 17), "event"] <- NA_real_
  data[which(is.na(data$event)), "event"] <- 0

  ### status
  data$prev <- data$post <- NA_real_
  data[which(data$t2Month_y == (data$t)), "prev"] <- data[which(data$t2Month_y == (data$t)), "mar2"]
  data[which(data$t0==nhm&is.na(data$prev)), "prev"] <- data[which(data$t0==nhm&is.na(data$prev)), "mar2"]

  data[which(data$event == 1), "post"] <- 2
  data[which(data$event == 2), "post"] <- 4
  data[which(data$event == 3), "post"] <- 2
  data[which(data$event == 4), "post"] <- 2
  data[which(data$event == 17), "post"] <- 3
  data[which(data$event == 5), "post"] <- 5
  data[which(data$event == 6), "post"] <- 7
  data[which(data$event == 7), "post"] <- 8
  data[which(data$event == 8), "post"] <- 1
  data[which(data$event == 9), "post"] <- 3
  data[which(data$event == 10), "post"] <- 4
  data[which(data$event == 11), "post"] <- 2
  data[which(data$event == 12), "post"] <- 2
  data[which(data$event == 13), "post"] <- 2

  data[which(data$event == 1), "prev"] <- 1
  data[which(data$event == 2), "prev"] <- 2
  data[which(data$event == 3), "prev"] <- 3
  data[which(data$event == 4), "prev"] <- 4
  data[which(data$event == 17), "prev"] <- 2

  data[which(data$event == 5), "prev"] <- 1
  data[which(data$event == 6), "prev"] <- 3
  data[which(data$event == 7), "prev"] <- 4
  data[which(data$event == 8), "prev"] <- 5
  data[which(data$event == 9), "prev"] <- 7
  data[which(data$event == 10), "prev"] <- 8
  data[which(data$event == 11), "prev"] <- 5
  data[which(data$event == 12), "prev"] <- 7
  data[which(data$event == 13), "prev"] <- 8

  data <- as.data.frame(data %>% group_by(ID) %>% fill(post, .direction="down") %>%
                          fill(prev, .direction="up"))

  data[which(is.na(data$post)), "post"] <- data[which(is.na(data$post)), "prev"]

  data <- data[, c("region", "ID", "weight", "race", "ru", "sex", "edu", "age", "event", "prev", "post", "py")]

  return(data)

}

mar4.t2 <- function(data, t2Month){
  mar2 <- data %>%
    mutate(event.cp = event) %>%
    mutate(event = replace(event, which(m>t2Month), NA)) %>%
    mutate(m = replace(m, which(m>t2Month), NA)) %>%
    filter(rowSums(is.na(cbind(event, m)))<2 | new_index==1)

  mar2 <- mar2 %>% group_by(ID) %>%
    arrange(ID, m) %>%
    mutate(n = n()) %>%
    mutate(row = row_number())

  mar2$mar2 <- dplyr::case_when(mar2$events==0 ~ mar2$mar,
                                mar2$events>=mar2$n & mar2$row==mar2$n & mar2$event %in% c(1, 3:4) ~ 2,
                                mar2$events>=mar2$n & mar2$row==mar2$n & mar2$event==2 ~ 4,
                                mar2$events>=mar2$n & mar2$row==mar2$n & mar2$event==17 ~ 3,
                                mar2$events>=mar2$n & mar2$row==mar2$n & is.na(mar2$event) & mar2$event.cp==1 ~ 1,
                                mar2$events>=mar2$n & mar2$row==mar2$n & is.na(mar2$event) & mar2$event.cp %in% c(2, 17) ~ 2,
                                mar2$events>=mar2$n & mar2$row==mar2$n & is.na(mar2$event) & mar2$event.cp==3 ~ 3,
                                mar2$events>=mar2$n & mar2$row==mar2$n & is.na(mar2$event) & mar2$event.cp==4 ~ 4)

  mar2 <- mar2 %>% fill(mar2, .direction = "up") %>% select(-n, -row)
  mar2$mar2 <- ifelse(is.na(mar2$mar2), mar2$mar, mar2$mar2)
  mar2 <- mar2 %>% select(ID, mar2)
  data <- merge(data, unique(mar2), by="ID") %>% relocate(mar2, .after = mar)
  return(data)
}

mar7.t2 <- function(data, t2Month){
  mar2 <- data %>%
    mutate(event.cp = event) %>%
    mutate(event = replace(event, which(m>t2Month), NA)) %>%
    mutate(m = replace(m, which(m>t2Month|is.na(event)), NA)) %>%
    filter(rowSums(is.na(cbind(event, m)))<2 | new_index==1)

  mar2 <- mar2 %>% group_by(ID) %>%
    arrange(ID, m) %>%
    mutate(n = n()) %>%
    mutate(row = row_number())

  mar2$mar2 <- dplyr::case_when(mar2$events==0 ~ mar2$mar,
                                mar2$events>=mar2$n & mar2$row==mar2$n & mar2$event %in% c(1, 3:4, 11:13) ~ 2,
                                mar2$events>=mar2$n & mar2$row==mar2$n & mar2$event %in% c(2, 10) ~ 4,
                                mar2$events>=mar2$n & mar2$row==mar2$n & mar2$event %in% c(9, 17) ~ 3,
                                mar2$events>=mar2$n & mar2$row==mar2$n & mar2$event==5 ~ 5,
                                mar2$events>=mar2$n & mar2$row==mar2$n & mar2$event==6 ~ 7,
                                mar2$events>=mar2$n & mar2$row==mar2$n & mar2$event==7 ~ 8,
                                mar2$events>=mar2$n & mar2$row==mar2$n & mar2$event==8 ~ 1,
                                mar2$events>=mar2$n & mar2$row==mar2$n & is.na(mar2$event) & mar2$event.cp %in% c(1, 5) ~ 1,
                                mar2$events>=mar2$n & mar2$row==mar2$n & is.na(mar2$event) & mar2$event.cp %in% c(2, 17) ~ 2,
                                mar2$events>=mar2$n & mar2$row==mar2$n & is.na(mar2$event) & mar2$event.cp %in% c(3, 6) ~ 3,
                                mar2$events>=mar2$n & mar2$row==mar2$n & is.na(mar2$event) & mar2$event.cp %in% c(4, 7) ~ 4,
                                mar2$events>=mar2$n & mar2$row==mar2$n & is.na(mar2$event) & mar2$event.cp %in% c(8, 11) ~ 5,
                                mar2$events>=mar2$n & mar2$row==mar2$n & is.na(mar2$event) & mar2$event.cp %in% c(9, 12) ~ 7,
                                mar2$events>=mar2$n & mar2$row==mar2$n & is.na(mar2$event) & mar2$event.cp %in% c(10, 13) ~ 8)

  mar2 <- mar2 %>% fill(mar2, .direction = "up") %>% select(-n, -row)

  mar2$mar2 <- ifelse(is.na(mar2$mar2), mar2$mar, mar2$mar2)
  mar2 <- mar2 %>% select(ID, mar2)
  data <- merge(data, unique(mar2), by="ID") %>% relocate(mar2, .after = mar)
  return(data)
}

##------4 Marital-------
freq.mar4 <- function(frequency, sex){

  freq <- data.frame(age = frequency$age)

  if (sex==F){

    #combine event columns into first marriage, remarriage and divorce.
    freq <- data.frame(freq,
                       mrg1 = frequency[2],
                       mrg = tmp.rowSum.w.na(frequency[4:5]),
                       div = frequency[3])

  } else if (sex==T){

    #combine event columns into first marriage, remarriage and divorce by sex.
    freq <- data.frame(freq,
                       male.mrg1 = frequency[2],
                       female.mrg1 = frequency[6],
                       male.mrg = tmp.rowSum.w.na(frequency[4:5]),
                       female.mrg = tmp.rowSum.w.na(frequency[8:9]),
                       male.div = frequency[3],
                       female.div = frequency[7])

  }

  #calculate total rates
  total.rates <- total.freq(freq)
  #calculate mean ages
  mean.age <- mean.age.cal(freq)

  #add total rates as the last row of frequency data frame.
  freq.total <- data.frame(age = "Total", total.freq(frequency))
  frequency <- rbind(frequency, freq.total)

  return(list(frequency=frequency, total.rates=total.rates, mean.age=mean.age))
}

pop.count.four <- function(data, nlm, nhm, sex){

  if (sex==T){
    pop=pop.count.four.sex(data, nlm, nhm)
  } else if (sex==F){
    pop=pop.count.four.nosex(data, nlm, nhm)
  }

  return(pop)
}

pop.count.four.sex <- function(data, nlm, nhm){

  age <- data.frame(age=seq(nlm, nhm, 1))
  d.001 <- subset(data, select=c(age, sex, post, event, py))

  # marital status
  if(nrow(d.001)==0){       #if not such people, leave empty
    temp <- data.frame(age=NA,sex=NA,status=NA, count=NA)[F,]
  } else {
    temp <- aggregate(cbind(d.001[0], count=d.001$py),
                      list(age=d.001$age, sex=d.001$sex, status=d.001$post), sum)
  }

  if(nrow(d.001)==0){       #if not such people, leave empty
    all <- data.frame(age=NA,sex=NA,x=NA)[F,]
  } else {
    all <- aggregate(cbind(d.001[0], count=d.001$py),
                     list(age=d.001$age, sex=d.001$sex), sum)
    all <- merge(age, all, all.x = T)
  }

  male.nm <- merge(age, subset(temp, sex == 1 & status == 1, select=c(age, count)), all.x=TRUE)
  female.nm <- merge(age, subset(temp, sex == 2 & status == 1, select=c(age, count)), all.x=TRUE)
  male.married <- merge(age, subset(temp, sex == 1 & status == 2, select=c(age, count)), all.x=TRUE)
  female.married <- merge(age, subset(temp, sex == 2 & status == 2, select=c(age, count)), all.x=TRUE)
  male.widowed <- merge(age, subset(temp, sex == 1 & status == 3, select=c(age, count)), all.x=TRUE)
  female.widowed <- merge(age, subset(temp, sex == 2 & status == 3, select=c(age, count)), all.x=TRUE)
  male.divorced <- merge(age, subset(temp, sex == 1 & status == 4, select=c(age, count)), all.x=TRUE)
  female.divorced <- merge(age, subset(temp, sex == 2 & status == 4, select=c(age, count)), all.x=TRUE)
  all.male <- merge(age, subset(all, sex == 1, select=c(age, count)), all.x=TRUE)
  names(all.male) <- c("age", "count")
  all.female <- merge(age, subset(all, sex == 2, select=c(age, count)), all.x=TRUE)
  names(all.female) <- c("age", "count")

  status <- data.frame(age=age$age, all.male=all.male$count, male.never.married=male.nm$count,
                       male.married=male.married$count, male.widowed=male.widowed$count,
                       male.divorced=male.divorced$count, all.female=all.female$count,
                       female.never.married=female.nm$count, female.married=female.married$count,
                       female.widowed=female.widowed$count, female.divorced=female.divorced$count)

  status[, 2:ncol(status)] <- round(status[, 2:ncol(status)], 1)
  risk.total <- data.frame(age = "Total", t(colSums(status[2:ncol(status)], na.rm = T)))
  status <- rbind(status, risk.total)
  rm(risk.total)

  # marital event
  if(nrow(d.001)==0){       #if not such people, leave empty
    temp <- data.frame(age=NA,sex=NA,event=NA, count=NA)[F,]
  } else {
    temp <- aggregate(cbind(d.001[0], count=1),
                      list(age=d.001$age, sex=d.001$sex, event=d.001$event), sum)
  }

  male.mrg1 <-  merge(age, subset(temp, sex == 1 & event == 1, select=c(age, count)), all.x=TRUE)
  female.mrg1 <-  merge(age, subset(temp, sex == 2 & event == 1, select=c(age, count)), all.x=TRUE)
  male.divorced <-  merge(age, subset(temp, sex == 1 & event == 2, select=c(age, count)), all.x=TRUE)
  female.divorced <-  merge(age, subset(temp, sex == 2 & event == 2, select=c(age, count)), all.x=TRUE)
  male.rw <-  merge(age, subset(temp, sex == 1 & event == 3, select=c(age, count)), all.x=TRUE)
  female.rw <-  merge(age, subset(temp, sex == 2 & event == 3, select=c(age, count)), all.x=TRUE)
  male.rd <-  merge(age, subset(temp, sex == 1 & event == 4, select=c(age, count)), all.x=TRUE)
  female.rd <-  merge(age, subset(temp, sex == 2 & event == 4, select=c(age, count)), all.x=TRUE)
  t <- temp[-which(temp$event == 0),]

  if(nrow(t)==0){       #if not such event, leave empty
    all <- data.frame(age=NA,sex=NA,x=NA)[F,]
  } else {
    all <- aggregate(t[,4], list(age=t$age, sex=t$sex), sum)
  }

  all.male <-  merge(age, subset(all, sex == 1, select=c(age, x)), all.x=TRUE)
  names(all.male) <- c("age", "count")
  all.female <-  merge(age, subset(all, sex == 2, select=c(age, x)), all.x=TRUE)
  names(all.female) <- c("age", "count")

  event <- data.frame(age=age$age, all.male=all.male$count, male.mrg1=male.mrg1$count,
                      male.divorced=male.divorced$count, male.remarriage.widowed=male.rw$count,
                      male.remarriage.divorced=male.rd$count, all.female=all.female$count,
                      female.mrg1=female.mrg1$count, female.divorced=female.divorced$count,
                      female.remarriage.widowed=female.rw$count, female.remarriage.divorced=female.rd$count)

  event.total <- data.frame(age = "Total", t(colSums(event[2:ncol(event)], na.rm = T)))
  event <- rbind(event, event.total)
  rm(event.total)

  return(list(status=status, event=event))
}

pop.count.four.nosex <- function(data, nlm, nhm){

  age <- data.frame(age=seq(nlm, nhm, 1))
  d.001 <- subset(data, select=c(age, post, event, py))

  # marital status
  if(nrow(d.001)==0){
    temp <- data.frame(age=NA,status=NA, count=NA)[F,]
  } else {
    temp <- aggregate(cbind(d.001[0], count=d.001$py),
                      list(age=d.001$age, status=d.001$post), sum)
  }

  if(nrow(d.001)==0){
    all <- data.frame(age=NA,x=NA)[F,]
  } else {
    all <- aggregate(cbind(d.001[0], count=d.001$py),
                     list(age=d.001$age), sum)
    all <- merge(age, all, all.x = T)
  }

  nm <- merge(age, subset(temp, status == 1, select=c(age, count)), all.x=TRUE)
  married <- merge(age, subset(temp, status == 2, select=c(age, count)), all.x=TRUE)
  widowed <- merge(age, subset(temp, status == 3, select=c(age, count)), all.x=TRUE)
  divorced <- merge(age, subset(temp, status == 4, select=c(age, count)), all.x=TRUE)
  names(all) <- c("age", "count")

  status <- data.frame(age=age$age, all=all$count, never.married=nm$count,
                       married=married$count, widowed=widowed$count,
                       divorced=divorced$count)

  status[, 2:ncol(status)] <- round(status[, 2:ncol(status)], 1)
  risk.total <- data.frame(age = "Total", t(colSums(status[2:ncol(status)], na.rm = T)))
  status <- rbind(status, risk.total)
  rm(risk.total)

  # marital event
  if(nrow(d.001)==0){
    temp <- data.frame(age=NA,event=NA, count=NA)[F,]
  } else {
    temp <- aggregate(cbind(d.001[0], count=1),
                      list(age=d.001$age, event=d.001$event), sum)
  }

  mrg1 <-  merge(age, subset(temp, event == 1, select=c(age, count)), all.x=TRUE)
  divorced <-  merge(age, subset(temp, event == 2, select=c(age, count)), all.x=TRUE)
  rw <-  merge(age, subset(temp, event == 3, select=c(age, count)), all.x=TRUE)
  rd <-  merge(age, subset(temp, event == 4, select=c(age, count)), all.x=TRUE)
  t <- temp[-which(temp$event == 0),]
  if(nrow(t)==0){
    all <- data.frame(age=NA,x=NA)[F,]
  } else {
    all <- aggregate(t[,3], list(age=t$age), sum)
    all <- merge(age, all, all.x = T)
  }
  names(all) <- c("age", "count")

  event <- data.frame(age=age$age, all=all$count, mrg1=mrg1$count,
                      divorced=divorced$count, remarriage.widowed=rw$count,
                      remarriage.divorced=rd$count)

  event.total <- data.frame(age = "Total", t(colSums(event[2:ncol(event)], na.rm = T)))
  event <- rbind(event, event.total)
  rm(event.total)

  return(list(status=status, event=event))
}

draw.mar4.nRate <- function(mrg1.oe, mrg1.freq, divorced.oe, divorced.freq, rw.oe, rw.freq, rd.oe, rd.freq,
                            code, nlm, nhm, title, period, plot.name, byvar, cal_oe, cal_freq){

  region.code <-  code$`Region Code`
  region.code <- region.code[!is.na(region.code)]

  race.code <- code$`Race Code`
  race.code <- race.code[!is.na(race.code)]

  if (byvar=="sex"){
    wb <- createWorkbook()
    k <- 1

    if(cal_oe == TRUE){
      addWorksheet(wb, "oe")
      writeData(wb, sheet=k, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
      writeData(wb, sheet=k, paste0("Period: ", period, sep=""), startRow=2, startCol=1)

      k <- k+1
      row.index <- 3
      p1 <- rates.plot(mrg1.oe[which(mrg1.oe$sex==1), c(1, 3:4)], nlm, nhm, "o/e rate", paste0("Figure 5.1. First marriage o/e rates, males", plot.name, sep=""))
      print(p1)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
      p2 <- rates.plot(mrg1.oe[which(mrg1.oe$sex==2), c(1, 3:4)], nlm, nhm, "o/e rate", paste0("Figure 5.2. First marriage o/e rates, females", plot.name, sep=""))
      print(p2)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

      row.index <- row.index + 22
      p5 <- rates.plot(divorced.oe[which(divorced.oe$sex==1), c(1, 3:4)], nlm, nhm, "o/e rate", paste0("Figure 6.1. Divorced o/e rates, males", plot.name, sep=""))
      print(p5)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
      p6 <- rates.plot(divorced.oe[which(divorced.oe$sex==2), c(1, 3:4)], nlm, nhm, "o/e rate", paste0("Figure 6.2. Divorced o/e rates, female", plot.name, sep=""))
      print(p6)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

      row.index <- row.index + 22
      p9 <- rates.plot(rw.oe[which(rw.oe$sex==1), c(1, 3:4)], nlm, nhm, "o/e rate", paste0("Figure 7.1. Widowed to remarriage o/e rates, males", plot.name, sep=""))
      print(p9)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
      p10 <- rates.plot(rw.oe[which(rw.oe$sex==2), c(1, 3:4)], nlm, nhm, "o/e rate", paste0("Figure 7.2. Widowed to remarriage o/e rates, females", plot.name, sep=""))
      print(p10)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

      row.index <- row.index + 22
      p13 <- rates.plot(rd.oe[which(rd.oe$sex==1), c(1, 3:4)], nlm, nhm, "o/e rate", paste0("Figure 8.1. Divorced to remarriage o/e rates, males", plot.name, sep=""))
      print(p13)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
      p14 <- rates.plot(rd.oe[which(rd.oe$sex==2), c(1, 3:4)], nlm, nhm, "o/e rate", paste0("Figure 8.2. Divorced to remarriage o/e rates, females", plot.name, sep=""))
      print(p14)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")
    }

    if(cal_freq == TRUE){
      addWorksheet(wb, "freq")
      writeData(wb, sheet=k, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
      writeData(wb, sheet=k, paste0("Period: ", period, sep=""), startRow=2, startCol=1)

      row.index <- 3
      p3 <- rates.plot(mrg1.freq[which(mrg1.freq$sex==1), c(1, 3:4)], nlm, nhm, "frequency", paste0("Figure 1.1. First marriage frequencies, males", plot.name, sep=""))
      print(p3)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
      p4 <- rates.plot(mrg1.freq[which(mrg1.freq$sex==2), c(1, 3:4)], nlm, nhm, "frequency", paste0("Figure 1.2. First marriage frequencies, females", plot.name, sep=""))
      print(p4)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

      row.index <- row.index + 22
      p7 <- rates.plot(divorced.freq[which(divorced.freq$sex==1), c(1, 3:4)], nlm, nhm, "frequency", paste0("Figure 2.1. Divorced frequencies, males", plot.name, sep=""))
      print(p7)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
      p8 <- rates.plot(divorced.freq[which(divorced.freq$sex==2), c(1, 3:4)], nlm, nhm, "frequency", paste0("Figure 2.2. Divorced frequencies, females", plot.name, sep=""))
      print(p8)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

      row.index <- row.index + 22
      p11 <- rates.plot(rw.freq[which(rw.freq$sex==1), c(1, 3:4)], nlm, nhm, "frequency", paste0("Figure 3.1. Widowed to remarriage frequencies, males", plot.name, sep=""))
      print(p11)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
      p12 <- rates.plot(rw.freq[which(rw.freq$sex==2), c(1, 3:4)], nlm, nhm, "frequency", paste0("Figure 3.2. Widowed to remarriage frequencies, females", plot.name, sep=""))
      print(p12)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

      row.index <- row.index + 22
      p15 <- rates.plot(rd.freq[which(rd.freq$sex==1), c(1, 3:4)], nlm, nhm, "frequency", paste0("Figure 4.1. Divorced to remarriage frequencies, males", plot.name, sep=""))
      print(p15)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
      p16 <- rates.plot(rd.freq[which(rd.freq$sex==2), c(1, 3:4)], nlm, nhm, "frequency", paste0("Figure 4.2. Divorced to remarriage frequencies, females", plot.name, sep=""))
      print(p16)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")
    }

    output.dir <- getwd()
    plot.name <- gsub(", ", "-", plot.name)
    saveWorkbook(wb, paste0(output.dir, "/", title, " Mar4 plot", plot.name, ".xlsx", sep=""), overwrite=TRUE)

  } else if (byvar=="sex.nosex"){
    wb <- createWorkbook()
    k <- 1

    if(cal_oe == TRUE){
      addWorksheet(wb, "oe")
      writeData(wb, sheet=k, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
      writeData(wb, sheet=k, paste0("Period: ", period, sep=""), startRow=2, startCol=1)

      k <- k+1
      row.index <- 3
      p1 <- rates.plot(mrg1.oe[, c(1, 2:3)], nlm, nhm, "o/e rate", paste0("Figure 5.1. First marriage o/e rates, males", plot.name, sep=""))
      print(p1)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")

      row.index <- row.index + 22
      p5 <- rates.plot(divorced.oe[, c(1, 2:3)], nlm, nhm, "o/e rate", paste0("Figure 6.1. Divorced o/e rates, males", plot.name, sep=""))
      print(p5)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")

      row.index <- row.index + 22
      p9 <- rates.plot(rw.oe[, c(1, 2:3)], nlm, nhm, "o/e rate", paste0("Figure 7.1. Widowed to remarriage o/e rates, males", plot.name, sep=""))
      print(p9)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")

      row.index <- row.index + 22
      p13 <- rates.plot(rd.oe[, c(1, 2:3)], nlm, nhm, "o/e rate", paste0("Figure 8.1. Divorced to remarriage o/e rates, males", plot.name, sep=""))
      print(p13)
      insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
    }

    if(cal_freq == TRUE){
      addWorksheet(wb, "freq")
      writeData(wb, sheet=k, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
      writeData(wb, sheet=k, paste0("Period: ", period, sep=""), startRow=2, startCol=1)

      row.index <- 3
      p3 <- rates.plot(mrg1.freq[, c(1, 2:3)], nlm, nhm, "frequency", paste0("Figure 1.1. First marriage frequencies, males", plot.name, sep=""))
      print(p3)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")

      row.index <- row.index + 22
      p7 <- rates.plot(divorced.freq[, c(1, 2:3)], nlm, nhm, "frequency", paste0("Figure 2.1. Divorced frequencies, males", plot.name, sep=""))
      print(p7)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")

      row.index <- row.index + 22
      p11 <- rates.plot(rw.freq[, c(1, 2:3)], nlm, nhm, "frequency", paste0("Figure 3.1. Widowed to remarriage frequencies, males", plot.name, sep=""))
      print(p11)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")

      row.index <- row.index + 22
      p15 <- rates.plot(rd.freq[, c(1, 2:3)], nlm, nhm, "frequency", paste0("Figure 4.1. Divorced to remarriage frequencies, males", plot.name, sep=""))
      print(p15)
      insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
    }

    output.dir <- getwd()
    plot.name <- gsub(", ", "-", plot.name)
    saveWorkbook(wb, paste0(output.dir, "/", title, " Mar4 plot", plot.name, ".xlsx", sep=""), overwrite=TRUE)

  } else if (byvar=="reg.ru"){

    for(i in region.code){
      r.name <- code[which(code$`Region Code` == i), 4]
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

        k <- k+2
        row.index <- 3
        p1 <- rates.plot(mrg1.oe[which(mrg1.oe$region==i&mrg1.oe$sex==1&mrg1.oe$ru==1), c(1, 5:6)], nlm, nhm, "o/e rate", paste0("Figure 5.1. First marriage o/e rates, males", plot.name, "-rural", sep=""))
        print(p1)
        insertPlot(wb, "oe-rural", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
        p2 <- rates.plot(mrg1.oe[which(mrg1.oe$region==i&mrg1.oe$sex==2&mrg1.oe$ru==1), c(1, 5:6)], nlm, nhm, "o/e rate", paste0("Figure 5.2. First marriage o/e rates, females", plot.name, "-rural", sep=""))
        print(p2)
        insertPlot(wb, "oe-rural", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

        p1 <- rates.plot(mrg1.oe[which(mrg1.oe$region==i&mrg1.oe$sex==1&mrg1.oe$ru==2), c(1, 5:6)], nlm, nhm, "o/e rate", paste0("Figure 5.1. First marriage o/e rates, males", plot.name, "-urban", sep=""))
        print(p1)
        insertPlot(wb, "oe-urban", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
        p2 <- rates.plot(mrg1.oe[which(mrg1.oe$region==i&mrg1.oe$sex==2&mrg1.oe$ru==2), c(1, 5:6)], nlm, nhm, "o/e rate", paste0("Figure 5.2. First marriage o/e rates, females", plot.name, "-urban", sep=""))
        print(p2)
        insertPlot(wb, "oe-urban", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

        row.index <- row.index + 22
        p5 <- rates.plot(divorced.oe[which(divorced.oe$region==i&divorced.oe$ru==1&divorced.oe$sex==1), c(1, 5:6)], nlm, nhm, "o/e rate", paste0("Figure 6.1. Divorced o/e rates, males", plot.name, "-rural", sep=""))
        print(p5)
        insertPlot(wb, "oe-rural", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
        p6 <- rates.plot(divorced.oe[which(divorced.oe$region==i&divorced.oe$ru==1&divorced.oe$sex==2), c(1, 5:6)], nlm, nhm, "o/e rate", paste0("Figure 6.2. Divorced o/e rates, female", plot.name, "-rural", sep=""))
        print(p6)
        insertPlot(wb, "oe-rural", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

        p5 <- rates.plot(divorced.oe[which(divorced.oe$region==i&divorced.oe$ru==2&divorced.oe$sex==1), c(1, 5:6)], nlm, nhm, "o/e rate", paste0("Figure 6.1. Divorced o/e rates, males", plot.name, "-urban", sep=""))
        print(p5)
        insertPlot(wb, "oe-urban", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
        p6 <- rates.plot(divorced.oe[which(divorced.oe$region==i&divorced.oe$ru==2&divorced.oe$sex==2), c(1, 5:6)], nlm, nhm, "o/e rate", paste0("Figure 6.2. Divorced o/e rates, female", plot.name, "-urban", sep=""))
        print(p6)
        insertPlot(wb, "oe-urban", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

        row.index <- row.index + 22
        p9 <- rates.plot(rw.oe[which(rw.oe$region==i&rw.oe$ru==1&rw.oe$sex==1), c(1, 5:6)], nlm, nhm, "o/e rate", paste0("Figure 7.1. Widowed to remarriage o/e rates, males", plot.name, "-rural", sep=""))
        print(p9)
        insertPlot(wb, "oe-rural", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
        p10 <- rates.plot(rw.oe[which(rw.oe$region==i&rw.oe$ru==1&rw.oe$sex==2), c(1, 5:6)], nlm, nhm, "o/e rate", paste0("Figure 7.2. Widowed to remarriage o/e rates, females", plot.name, "-rural", sep=""))
        print(p10)
        insertPlot(wb, "oe-rural", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

        p9 <- rates.plot(rw.oe[which(rw.oe$region==i&rw.oe$ru==2&rw.oe$sex==1), c(1, 5:6)], nlm, nhm, "o/e rate", paste0("Figure 7.1. Widowed to remarriage o/e rates, males", plot.name, "-urban", sep=""))
        print(p9)
        insertPlot(wb, "oe-urban", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
        p10 <- rates.plot(rw.oe[which(rw.oe$region==i&rw.oe$ru==2&rw.oe$sex==2), c(1, 5:6)], nlm, nhm, "o/e rate", paste0("Figure 7.2. Widowed to remarriage o/e rates, females", plot.name, "-urban", sep=""))
        print(p10)
        insertPlot(wb, "oe-urban", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

        row.index <- row.index + 22
        p13 <- rates.plot(rd.oe[which(rd.oe$region==i&rd.oe$ru==1&rd.oe$sex==1), c(1, 5:6)], nlm, nhm, "o/e rate", paste0("Figure 8.1. Divorced to remarriage o/e rates, males", plot.name, "-rural", sep=""))
        print(p13)
        insertPlot(wb, "oe-rural", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
        p14 <- rates.plot(rd.oe[which(rd.oe$region==i&rd.oe$ru==1&rd.oe$sex==2), c(1, 5:6)], nlm, nhm, "o/e rate", paste0("Figure 8.2. Divorced to remarriage o/e rates, females", plot.name, "-rural", sep=""))
        print(p14)
        insertPlot(wb, "oe-rural", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

        p13 <- rates.plot(rd.oe[which(rd.oe$region==i&rd.oe$ru==2&rd.oe$sex==1), c(1, 5:6)], nlm, nhm, "o/e rate", paste0("Figure 8.1. Divorced to remarriage o/e rates, males", plot.name, "-urban", sep=""))
        print(p13)
        insertPlot(wb, "oe-urban", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
        p14 <- rates.plot(rd.oe[which(rd.oe$region==i&rd.oe$ru==2&rd.oe$sex==2), c(1, 5:6)], nlm, nhm, "o/e rate", paste0("Figure 8.2. Divorced to remarriage o/e rates, females", plot.name, "-urban", sep=""))
        print(p14)
        insertPlot(wb, "oe-urban", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")
      }

      if(cal_freq == TRUE){
        addWorksheet(wb, "freq-rural")
        addWorksheet(wb, "freq-urban")
        writeData(wb, sheet=k, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
        writeData(wb, sheet=k, paste0("Period: ", period, sep=""), startRow=2, startCol=1)
        writeData(wb, sheet=k+1, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
        writeData(wb, sheet=k+1, paste0("Period: ", period, sep=""), startRow=2, startCol=1)

        row.index <- 3
        p3 <- rates.plot(mrg1.freq[which(mrg1.freq$region==i&mrg1.freq$sex==1&mrg1.freq$ru==1), c(1, 5:6)], nlm, nhm, "frequency", paste0("Figure 1.1. First marriage frequencies, males", plot.name, "-rural", sep=""))
        print(p3)
        insertPlot(wb, "freq-rural", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
        p4 <- rates.plot(mrg1.freq[which(mrg1.freq$region==i&mrg1.freq$sex==2&mrg1.freq$ru==1), c(1, 5:6)], nlm, nhm, "frequency", paste0("Figure 1.2. First marriage frequencies, females", plot.name, "-rural", sep=""))
        print(p4)
        insertPlot(wb, "freq-rural", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

        p3 <- rates.plot(mrg1.freq[which(mrg1.freq$region==i&mrg1.freq$sex==1&mrg1.freq$ru==2), c(1, 5:6)], nlm, nhm, "frequency", paste0("Figure 1.1. First marriage frequencies, males", plot.name, "-urban", sep=""))
        print(p3)
        insertPlot(wb, "freq-urban", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
        p4 <- rates.plot(mrg1.freq[which(mrg1.freq$region==i&mrg1.freq$sex==2&mrg1.freq$ru==2), c(1, 5:6)], nlm, nhm, "frequency", paste0("Figure 1.2. First marriage frequencies, females", plot.name, "-urban", sep=""))
        print(p4)
        insertPlot(wb, "freq-urban", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

        row.index <- row.index + 22
        p7 <- rates.plot(divorced.freq[which(divorced.freq$region==i&divorced.freq$ru==1&divorced.freq$sex==1), c(1, 5:6)], nlm, nhm, "frequency", paste0("Figure 2.1. Divorced frequencies, males", plot.name, "-rural", sep=""))
        print(p7)
        insertPlot(wb, "freq-rural", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
        p8 <- rates.plot(divorced.freq[which(divorced.freq$region==i&divorced.freq$ru==1&divorced.freq$sex==2), c(1, 5:6)], nlm, nhm, "frequency", paste0("Figure 2.2. Divorced frequencies, females", plot.name, "-rural", sep=""))
        print(p8)
        insertPlot(wb, "freq-rural", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

        p7 <- rates.plot(divorced.freq[which(divorced.freq$region==i&divorced.freq$ru==2&divorced.freq$sex==1), c(1, 5:6)], nlm, nhm, "frequency", paste0("Figure 2.1. Divorced frequencies, males", plot.name, "-urban", sep=""))
        print(p7)
        insertPlot(wb, "freq-urban", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
        p8 <- rates.plot(divorced.freq[which(divorced.freq$region==i&divorced.freq$ru==2&divorced.freq$sex==2), c(1, 5:6)], nlm, nhm, "frequency", paste0("Figure 2.2. Divorced frequencies, females", plot.name, "-urban", sep=""))
        print(p8)
        insertPlot(wb, "freq-urban", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

        row.index <- row.index + 22
        p11 <- rates.plot(rw.freq[which(rw.freq$region==i&rw.freq$ru==1&rw.freq$sex==1), c(1, 5:6)], nlm, nhm, "frequency", paste0("Figure 3.1. Widowed to remarriage frequencies, males", plot.name, "-rural", sep=""))
        print(p11)
        insertPlot(wb, "freq-rural", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
        p12 <- rates.plot(rw.freq[which(rw.freq$region==i&rw.freq$ru==1&rw.freq$sex==2), c(1, 5:6)], nlm, nhm, "frequency", paste0("Figure 3.2. Widowed to remarriage frequencies, females", plot.name, "-rural", sep=""))
        print(p12)
        insertPlot(wb, "freq-rural", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

        p11 <- rates.plot(rw.freq[which(rw.freq$region==i&rw.freq$ru==2&rw.freq$sex==1), c(1, 5:6)], nlm, nhm, "frequency", paste0("Figure 3.1. Widowed to remarriage frequencies, males", plot.name, "-urban", sep=""))
        print(p11)
        insertPlot(wb, "freq-urban", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
        p12 <- rates.plot(rw.freq[which(rw.freq$region==i&rw.freq$ru==2&rw.freq$sex==2), c(1, 5:6)], nlm, nhm, "frequency", paste0("Figure 3.2. Widowed to remarriage frequencies, females", plot.name, "-urban", sep=""))
        print(p12)
        insertPlot(wb, "freq-urban", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

        row.index <- row.index + 22
        p15 <- rates.plot(rd.freq[which(rd.freq$region==i&rd.freq$ru==1&rd.freq$sex==1), c(1, 5:6)], nlm, nhm, "frequency", paste0("Figure 4.1. Divorced to remarriage frequencies, males", plot.name, "-rural", sep=""))
        print(p15)
        insertPlot(wb, "freq-rural", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
        p16 <- rates.plot(rd.freq[which(rd.freq$region==i&rd.freq$ru==1&rd.freq$sex==2), c(1, 5:6)], nlm, nhm, "frequency", paste0("Figure 4.2. Divorced to remarriage frequencies, females", plot.name, "-rural", sep=""))
        print(p16)
        insertPlot(wb, "freq-rural", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

        p15 <- rates.plot(rd.freq[which(rd.freq$region==i&rd.freq$ru==2&rd.freq$sex==1), c(1, 5:6)], nlm, nhm, "frequency", paste0("Figure 4.1. Divorced to remarriage frequencies, males", plot.name, "-urban", sep=""))
        print(p15)
        insertPlot(wb, "freq-urban", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
        p16 <- rates.plot(rd.freq[which(rd.freq$region==i&rd.freq$ru==2&rd.freq$sex==2), c(1, 5:6)], nlm, nhm, "frequency", paste0("Figure 4.2. Divorced to remarriage frequencies, females", plot.name, "-urban", sep=""))
        print(p16)
        insertPlot(wb, "freq-urban", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")
      }

      output.dir <- getwd()
      plot.name <- gsub(", ", "-", plot.name)
      saveWorkbook(wb, paste0(output.dir, "/", title, " Mar4 plot", plot.name, ".xlsx", sep=""), overwrite=TRUE)
    }

  } else if (byvar=="reg.ru.nosex"){

    for(i in region.code){
      r.name <- code[which(code$`Region Code` == i), 4]
      r.code <- paste0(", ", r.name, sep="")
      plot.name <- r.code

      wb <- createWorkbook()
      k <- 1

      if(cal_oe == TRUE){
        addWorksheet(wb, "oe")
        writeData(wb, sheet=k, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
        writeData(wb, sheet=k, paste0("Period: ", period, sep=""), startRow=2, startCol=1)

        k <- k+1
        row.index <- 3
        p1 <- rates.plot(mrg1.oe[which(mrg1.oe$region==i&mrg1.oe$ru==1), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure 5.1. First marriage o/e rates", plot.name, "-rural", sep=""))
        print(p1)
        insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
        p2 <- rates.plot(mrg1.oe[which(mrg1.oe$region==i&mrg1.oe$ru==2), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure 5.2. First marriage o/e rates", plot.name, "-urban", sep=""))
        print(p2)
        insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

        row.index <- row.index + 22
        p5 <- rates.plot(divorced.oe[which(divorced.oe$region==i&divorced.oe$ru==1), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure 6.1. Divorced o/e rates", plot.name, "-rural", sep=""))
        print(p5)
        insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
        p6 <- rates.plot(divorced.oe[which(divorced.oe$region==i&divorced.oe$ru==2), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure 6.2. Divorced o/e rates", plot.name, "-urban", sep=""))
        print(p6)
        insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

        row.index <- row.index + 22
        p9 <- rates.plot(rw.oe[which(rw.oe$region==i&rw.oe$ru==1), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure 7.1. Widowed to remarriage o/e rates", plot.name, "-rural", sep=""))
        print(p9)
        insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
        p10 <- rates.plot(rw.oe[which(rw.oe$region==i&rw.oe$ru==2), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure 7.2. Widowed to remarriage o/e rates", plot.name, "-urban", sep=""))
        print(p10)
        insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

        row.index <- row.index + 22
        p13 <- rates.plot(rd.oe[which(rd.oe$region==i&rd.oe$ru==1), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure 8.1. Divorced to remarriage o/e rates", plot.name, "-rural", sep=""))
        print(p13)
        insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
        p14 <- rates.plot(rd.oe[which(rd.oe$region==i&rd.oe$ru==2), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure 8.2. Divorced to remarriage o/e rates", plot.name, "-urban", sep=""))
        print(p14)
        insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")
      }

      if(cal_freq == TRUE){
        addWorksheet(wb, "freq")
        writeData(wb, sheet=k, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
        writeData(wb, sheet=k, paste0("Period: ", period, sep=""), startRow=2, startCol=1)

        row.index <- 3
        p3 <- rates.plot(mrg1.freq[which(mrg1.freq$region==i&mrg1.freq$ru==1), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure 1.1. First marriage frequencies", plot.name, "-rural", sep=""))
        print(p3)
        insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
        p4 <- rates.plot(mrg1.freq[which(mrg1.freq$region==i&mrg1.freq$ru==2), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure 1.2. First marriage frequencies", plot.name, "-urban", sep=""))
        print(p4)
        insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

        row.index <- row.index + 22
        p7 <- rates.plot(divorced.freq[which(divorced.freq$region==i&divorced.freq$ru==1), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure 2.1. Divorced frequencies", plot.name, "-rural", sep=""))
        print(p7)
        insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
        p8 <- rates.plot(divorced.freq[which(divorced.freq$region==i&divorced.freq$ru==2), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure 2.2. Divorced frequencies", plot.name, "-urban", sep=""))
        print(p8)
        insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

        row.index <- row.index + 22
        p11 <- rates.plot(rw.freq[which(rw.freq$region==i&rw.freq$ru==1), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure 3.1. Widowed to remarriage frequencies", plot.name, "-rural", sep=""))
        print(p11)
        insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
        p12 <- rates.plot(rw.freq[which(rw.freq$region==i&rw.freq$ru==2), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure 3.2. Widowed to remarriage frequencies", plot.name, "-urban", sep=""))
        print(p12)
        insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

        row.index <- row.index + 22
        p15 <- rates.plot(rd.freq[which(rd.freq$region==i&rd.freq$ru==1), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure 4.1. Divorced to remarriage frequencies", plot.name, "-rural", sep=""))
        print(p15)
        insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
        p16 <- rates.plot(rd.freq[which(rd.freq$region==i&rd.freq$ru==2), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure 4.2. Divorced to remarriage frequencies", plot.name, "-urban", sep=""))
        print(p16)
        insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")
      }

      output.dir <- getwd()
      plot.name <- gsub(", ", "-", plot.name)
      saveWorkbook(wb, paste0(output.dir, "/", title, " Mar4 plot", plot.name, ".xlsx", sep=""), overwrite=TRUE)
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

    if(cal_freq == TRUE){
      addWorksheet(wb, "freq")
      writeData(wb, sheet=k, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
      writeData(wb, sheet=k, paste0("Period: ", period, sep=""), startRow=2, startCol=1)
    }

    if (byvar=="ru"){

      ru.code <- c(1, 2)

      for(i in ru.code){
        r.name <- ifelse(i==1, "rural", "urban")
        r.code <- paste0(", ", r.name, sep="")
        plot.name <- r.code

        if(cal_oe == TRUE){
          row.index <- 3
          p1 <- rates.plot(mrg1.oe[which(mrg1.oe$sex==1&mrg1.oe$ru==i), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure 5.1. First marriage o/e rates, males", plot.name, sep=""))
          print(p1)
          insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
          p2 <- rates.plot(mrg1.oe[which(mrg1.oe$sex==2&mrg1.oe$ru==i), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure 5.2. First marriage o/e rates, females", plot.name, sep=""))
          print(p2)
          insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

          row.index <- row.index + 22
          p5 <- rates.plot(divorced.oe[which(divorced.oe$sex==1&divorced.oe$ru==i), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure 6.1. Divorced o/e rates, males", plot.name, sep=""))
          print(p5)
          insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
          p6 <- rates.plot(divorced.oe[which(divorced.oe$sex==2&divorced.oe$ru==i), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure 6.2. Divorced o/e rates, female", plot.name, sep=""))
          print(p6)
          insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

          row.index <- row.index + 22
          p9 <- rates.plot(rw.oe[which(rw.oe$sex==1&rw.oe$ru==i), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure 7.1. Widowed to remarriage o/e rates, males", plot.name, sep=""))
          print(p9)
          insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
          p10 <- rates.plot(rw.oe[which(rw.oe$sex==2&rw.oe$ru==i), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure 7.2. Widowed to remarriage o/e rates, females", plot.name, sep=""))
          print(p10)
          insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

          row.index <- row.index + 22
          p13 <- rates.plot(rd.oe[which(rd.oe$sex==1&rd.oe$ru==i), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure 8.1. Divorced to remarriage o/e rates, males", plot.name, sep=""))
          print(p13)
          insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
          p14 <- rates.plot(rd.oe[which(rd.oe$sex==2&rd.oe$ru==i), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure 8.2. Divorced to remarriage o/e rates, females", plot.name, sep=""))
          print(p14)
          insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")
        }

        if(cal_freq == TRUE){
          row.index <- 3
          p3 <- rates.plot(mrg1.freq[which(mrg1.freq$sex==1&mrg1.freq$ru==i), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure 1.1. First marriage frequencies, males", plot.name, sep=""))
          print(p3)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
          p4 <- rates.plot(mrg1.freq[which(mrg1.freq$sex==2&mrg1.freq$ru==i), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure 1.2. First marriage frequencies, females", plot.name, sep=""))
          print(p4)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

          row.index <- row.index + 22
          p7 <- rates.plot(divorced.freq[which(divorced.freq$sex==1&divorced.freq$ru==i), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure 2.1. Divorced frequencies, males", plot.name, sep=""))
          print(p7)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
          p8 <- rates.plot(divorced.freq[which(divorced.freq$sex==2&divorced.freq$ru==i), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure 2.2. Divorced frequencies, females", plot.name, sep=""))
          print(p8)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

          row.index <- row.index + 22
          p11 <- rates.plot(rw.freq[which(rw.freq$sex==1&rw.freq$ru==i), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure 3.1. Widowed to remarriage frequencies, males", plot.name, sep=""))
          print(p11)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
          p12 <- rates.plot(rw.freq[which(rw.freq$sex==2&rw.freq$ru==i), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure 3.2. Widowed to remarriage frequencies, females", plot.name, sep=""))
          print(p12)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

          row.index <- row.index + 22
          p15 <- rates.plot(rd.freq[which(rd.freq$sex==1&rd.freq$ru==i), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure 4.1. Divorced to remarriage frequencies, males", plot.name, sep=""))
          print(p15)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
          p16 <- rates.plot(rd.freq[which(rd.freq$sex==2&rd.freq$ru==i), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure 4.2. Divorced to remarriage frequencies, females", plot.name, sep=""))
          print(p16)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")
        }

        output.dir <- getwd()
        plot.name <- gsub(", ", "-", plot.name)
        saveWorkbook(wb, paste0(output.dir, "/", title, " Mar4 plot", plot.name, ".xlsx", sep=""), overwrite=TRUE)

      }
    }

    if (byvar=="region"){

      for(i in region.code){
        r.name <- code[which(code$`Region Code` == i), 4]
        r.code <- paste0(", ", r.name, sep="")
        plot.name <- r.code

        if(cal_oe == TRUE){
          row.index <- 3
          p1 <- rates.plot(mrg1.oe[which(mrg1.oe$sex==1&mrg1.oe$region==i), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure 5.1. First marriage o/e rates, males", plot.name, sep=""))
          print(p1)
          insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
          p2 <- rates.plot(mrg1.oe[which(mrg1.oe$sex==2&mrg1.oe$region==i), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure 5.2. First marriage o/e rates, females", plot.name, sep=""))
          print(p2)
          insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

          row.index <- row.index + 22
          p5 <- rates.plot(divorced.oe[which(divorced.oe$sex==1&divorced.oe$region==i), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure 6.1. Divorced o/e rates, males", plot.name, sep=""))
          print(p5)
          insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
          p6 <- rates.plot(divorced.oe[which(divorced.oe$sex==2&divorced.oe$region==i), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure 6.2. Divorced o/e rates, female", plot.name, sep=""))
          print(p6)
          insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

          row.index <- row.index + 22
          p9 <- rates.plot(rw.oe[which(rw.oe$sex==1&rw.oe$region==i), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure 7.1. Widowed to remarriage o/e rates, males", plot.name, sep=""))
          print(p9)
          insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
          p10 <- rates.plot(rw.oe[which(rw.oe$sex==2&rw.oe$region==i), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure 7.2. Widowed to remarriage o/e rates, females", plot.name, sep=""))
          print(p10)
          insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

          row.index <- row.index + 22
          p13 <- rates.plot(rd.oe[which(rd.oe$sex==1&rd.oe$region==i), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure 8.1. Divorced to remarriage o/e rates, males", plot.name, sep=""))
          print(p13)
          insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
          p14 <- rates.plot(rd.oe[which(rd.oe$sex==2&rd.oe$region==i), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure 8.2. Divorced to remarriage o/e rates, females", plot.name, sep=""))
          print(p14)
          insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")
        }

        if(cal_freq == TRUE){
          row.index <- 3
          p3 <- rates.plot(mrg1.freq[which(mrg1.freq$sex==1&mrg1.freq$region==i), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure 1.1. First marriage frequencies, males", plot.name, sep=""))
          print(p3)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
          p4 <- rates.plot(mrg1.freq[which(mrg1.freq$sex==2&mrg1.freq$region==i), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure 1.2. First marriage frequencies, females", plot.name, sep=""))
          print(p4)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

          row.index <- row.index + 22
          p7 <- rates.plot(divorced.freq[which(divorced.freq$sex==1&divorced.freq$region==i), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure 2.1. Divorced frequencies, males", plot.name, sep=""))
          print(p7)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
          p8 <- rates.plot(divorced.freq[which(divorced.freq$sex==2&divorced.freq$region==i), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure 2.2. Divorced frequencies, females", plot.name, sep=""))
          print(p8)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

          row.index <- row.index + 22
          p11 <- rates.plot(rw.freq[which(rw.freq$sex==1&rw.freq$region==i), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure 3.1. Widowed to remarriage frequencies, males", plot.name, sep=""))
          print(p11)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
          p12 <- rates.plot(rw.freq[which(rw.freq$sex==2&rw.freq$region==i), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure 3.2. Widowed to remarriage frequencies, females", plot.name, sep=""))
          print(p12)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

          row.index <- row.index + 22
          p15 <- rates.plot(rd.freq[which(rd.freq$sex==1&rd.freq$region==i), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure 4.1. Divorced to remarriage frequencies, males", plot.name, sep=""))
          print(p15)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
          p16 <- rates.plot(rd.freq[which(rd.freq$sex==2&rd.freq$region==i), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure 4.2. Divorced to remarriage frequencies, females", plot.name, sep=""))
          print(p16)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")
        }

        output.dir <- getwd()
        plot.name <- gsub(", ", "-", plot.name)
        saveWorkbook(wb, paste0(output.dir, "/", title, " Mar4 plot", plot.name, ".xlsx", sep=""), overwrite=TRUE)

      }
    }

    if (byvar=="region.nosex"){

      for(i in region.code){
        r.name <- code[which(code$`Region Code` == i), 4]
        r.code <- paste0(", ", r.name, sep="")
        plot.name <- r.code

        if(cal_oe == TRUE){
          row.index <- 3
          p1 <- rates.plot(mrg1.oe[which(mrg1.oe$region==i), c(1, 3:4)], nlm, nhm, "o/e rate", paste0("Figure 5.1. First marriage o/e rates", plot.name, sep=""))
          print(p1)
          insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")

          row.index <- row.index + 22
          p5 <- rates.plot(divorced.oe[which(divorced.oe$region==i), c(1, 3:4)], nlm, nhm, "o/e rate", paste0("Figure 6.1. Divorced o/e rates", plot.name, sep=""))
          print(p5)
          insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")

          row.index <- row.index + 22
          p9 <- rates.plot(rw.oe[which(rw.oe$region==i), c(1, 3:4)], nlm, nhm, "o/e rate", paste0("Figure 7.1. Widowed to remarriage o/e rates", plot.name, sep=""))
          print(p9)
          insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")

          row.index <- row.index + 22
          p13 <- rates.plot(rd.oe[which(rd.oe$region==i), c(1, 3:4)], nlm, nhm, "o/e rate", paste0("Figure 8.1. Divorced to remarriage o/e rates", plot.name, sep=""))
          print(p13)
          insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
        }

        if(cal_freq == TRUE){
          row.index <- 3
          p3 <- rates.plot(mrg1.freq[which(mrg1.freq$region==i), c(1, 3:4)], nlm, nhm, "frequency", paste0("Figure 1.1. First marriage frequencies", plot.name, sep=""))
          print(p3)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")

          row.index <- row.index + 22
          p7 <- rates.plot(divorced.freq[which(divorced.freq$region==i), c(1, 3:4)], nlm, nhm, "frequency", paste0("Figure 2.1. Divorced frequencies", plot.name, sep=""))
          print(p7)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")

          row.index <- row.index + 22
          p11 <- rates.plot(rw.freq[which(rw.freq$region==i), c(1, 3:4)], nlm, nhm, "frequency", paste0("Figure 3.1. Widowed to remarriage frequencies", plot.name, sep=""))
          print(p11)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")

          row.index <- row.index + 22
          p15 <- rates.plot(rd.freq[which(rd.freq$region==i), c(1, 3:4)], nlm, nhm, "frequency", paste0("Figure 4.1. Divorced to remarriage frequencies", plot.name, sep=""))
          print(p15)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
        }

        output.dir <- getwd()
        plot.name <- gsub(", ", "-", plot.name)
        saveWorkbook(wb, paste0(output.dir, "/", title, " Mar4 plot", plot.name, ".xlsx", sep=""), overwrite=TRUE)

      }
    }

    if (byvar=="ru.nosex"){
      ru.code <- c(1, 2)

      for(i in ru.code){
        r.name <- ifelse(i==1, "rural", "urban")
        r.code <- paste0(", ", r.name, sep="")
        plot.name <- r.code

        if(cal_oe == TRUE){
          row.index <- 3
          p1 <- rates.plot(mrg1.oe[which(mrg1.oe$ru==i), c(1, 3:4)], nlm, nhm, "o/e rate", paste0("Figure 5.1. First marriage o/e rates", plot.name, sep=""))
          print(p1)
          insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")

          row.index <- row.index + 22
          p5 <- rates.plot(divorced.oe[which(divorced.oe$ru==i), c(1, 3:4)], nlm, nhm, "o/e rate", paste0("Figure 6.1. Divorced o/e rates", plot.name, sep=""))
          print(p5)
          insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")

          row.index <- row.index + 22
          p9 <- rates.plot(rw.oe[which(rw.oe$ru==i), c(1, 3:4)], nlm, nhm, "o/e rate", paste0("Figure 7.1. Widowed to remarriage o/e rates", plot.name, sep=""))
          print(p9)
          insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")

          row.index <- row.index + 22
          p13 <- rates.plot(rd.oe[which(rd.oe$ru==i), c(1, 3:4)], nlm, nhm, "o/e rate", paste0("Figure 8.1. Divorced to remarriage o/e rates", plot.name, sep=""))
          print(p13)
          insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
        }

        if(cal_freq == TRUE){
          row.index <- 3
          p3 <- rates.plot(mrg1.freq[which(mrg1.freq$ru==i), c(1, 3:4)], nlm, nhm, "frequency", paste0("Figure 1.1. First marriage frequencies", plot.name, sep=""))
          print(p3)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")

          row.index <- row.index + 22
          p7 <- rates.plot(divorced.freq[which(divorced.freq$ru==i), c(1, 3:4)], nlm, nhm, "frequency", paste0("Figure 2.1. Divorced frequencies", plot.name, sep=""))
          print(p7)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")

          row.index <- row.index + 22
          p11 <- rates.plot(rw.freq[which(rw.freq$ru==i), c(1, 3:4)], nlm, nhm, "frequency", paste0("Figure 3.1. Widowed to remarriage frequencies", plot.name, sep=""))
          print(p11)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")

          row.index <- row.index + 22
          p15 <- rates.plot(rd.freq[which(rd.freq$ru==i), c(1, 3:4)], nlm, nhm, "frequency", paste0("Figure 4.1. Divorced to remarriage frequencies", plot.name, sep=""))
          print(p15)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
        }

        output.dir <- getwd()
        plot.name <- gsub(", ", "-", plot.name)
        saveWorkbook(wb, paste0(output.dir, "/", title, " Mar4 plot", plot.name, ".xlsx", sep=""), overwrite=TRUE)

      }
    }

    if (byvar=="race"){

      for(i in race.code){
        r.name <- code[which(code$`Race Code` == i), 2]
        r.code <- paste0(", ", r.name, sep="")
        plot.name <- r.code

        if(cal_oe == TRUE){
          row.index <- 3
          p1 <- rates.plot(mrg1.oe[which(mrg1.oe$sex==1&mrg1.oe$race==i), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure 5.1. First marriage o/e rates, males", plot.name, sep=""))
          print(p1)
          insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
          p2 <- rates.plot(mrg1.oe[which(mrg1.oe$sex==2&mrg1.oe$race==i), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure 5.2. First marriage o/e rates, females", plot.name, sep=""))
          print(p2)
          insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

          row.index <- row.index + 22
          p5 <- rates.plot(divorced.oe[which(divorced.oe$sex==1&divorced.oe$race==i), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure 6.1. Divorced o/e rates, males", plot.name, sep=""))
          print(p5)
          insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
          p6 <- rates.plot(divorced.oe[which(divorced.oe$sex==2&divorced.oe$race==i), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure 6.2. Divorced o/e rates, female", plot.name, sep=""))
          print(p6)
          insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

          row.index <- row.index + 22
          p9 <- rates.plot(rw.oe[which(rw.oe$sex==1&rw.oe$race==i), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure 7.1. Widowed to remarriage o/e rates, males", plot.name, sep=""))
          print(p9)
          insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
          p10 <- rates.plot(rw.oe[which(rw.oe$sex==2&rw.oe$race==i), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure 7.2. Widowed to remarriage o/e rates, females", plot.name, sep=""))
          print(p10)
          insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

          row.index <- row.index + 22
          p13 <- rates.plot(rd.oe[which(rd.oe$sex==1&rd.oe$race==i), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure 8.1. Divorced to remarriage o/e rates, males", plot.name, sep=""))
          print(p13)
          insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
          p14 <- rates.plot(rd.oe[which(rd.oe$sex==2&rd.oe$race==i), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure 8.2. Divorced to remarriage o/e rates, females", plot.name, sep=""))
          print(p14)
          insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")
        }

        if(cal_freq == TRUE){
          row.index <- 3
          p3 <- rates.plot(mrg1.freq[which(mrg1.freq$sex==1&mrg1.freq$race==i), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure 1.1. First marriage frequencies, males", plot.name, sep=""))
          print(p3)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
          p4 <- rates.plot(mrg1.freq[which(mrg1.freq$sex==2&mrg1.freq$race==i), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure 1.2. First marriage frequencies, females", plot.name, sep=""))
          print(p4)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

          row.index <- row.index + 22
          p7 <- rates.plot(divorced.freq[which(divorced.freq$sex==1&divorced.freq$race==i), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure 2.1. Divorced frequencies, males", plot.name, sep=""))
          print(p7)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
          p8 <- rates.plot(divorced.freq[which(divorced.freq$sex==2&divorced.freq$race==i), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure 2.2. Divorced frequencies, females", plot.name, sep=""))
          print(p8)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

          row.index <- row.index + 22
          p11 <- rates.plot(rw.freq[which(rw.freq$sex==1&rw.freq$race==i), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure 3.1. Widowed to remarriage frequencies, males", plot.name, sep=""))
          print(p11)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
          p12 <- rates.plot(rw.freq[which(rw.freq$sex==2&rw.freq$race==i), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure 3.2. Widowed to remarriage frequencies, females", plot.name, sep=""))
          print(p12)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

          row.index <- row.index + 22
          p15 <- rates.plot(rd.freq[which(rd.freq$sex==1&rd.freq$race==i), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure 4.1. Divorced to remarriage frequencies, males", plot.name, sep=""))
          print(p15)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
          p16 <- rates.plot(rd.freq[which(rd.freq$sex==2&rd.freq$race==i), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure 4.2. Divorced to remarriage frequencies, females", plot.name, sep=""))
          print(p16)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")
        }

        output.dir <- getwd()
        plot.name <- gsub(", ", "-", plot.name)
        saveWorkbook(wb, paste0(output.dir, "/", title, " Mar4 plot", plot.name, ".xlsx", sep=""), overwrite=TRUE)
      }
    }

    if (byvar=="edu"){

      edu.code <- c(1:5)
      educode <- data.frame(code=c(1:5), name=c("No education", "Primary school",
                                                "Middle school", "High school", "College or higher"))

      for(i in edu.code){
        e.name <- educode[which(educode$code==i), "name"]
        e.code <- paste0(", ", e.name, sep="")
        plot.name <- e.code

        if(cal_oe == TRUE){
          row.index <- 3
          p1 <- rates.plot(mrg1.oe[which(mrg1.oe$sex==1&mrg1.oe$edu==i), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure 5.1. First marriage o/e rates, males", plot.name, sep=""))
          print(p1)
          insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
          p2 <- rates.plot(mrg1.oe[which(mrg1.oe$sex==2&mrg1.oe$edu==i), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure 5.2. First marriage o/e rates, females", plot.name, sep=""))
          print(p2)
          insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

          row.index <- row.index + 22
          p5 <- rates.plot(divorced.oe[which(divorced.oe$sex==1&divorced.oe$edu==i), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure 6.1. Divorced o/e rates, males", plot.name, sep=""))
          print(p5)
          insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
          p6 <- rates.plot(divorced.oe[which(divorced.oe$sex==2&divorced.oe$edu==i), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure 6.2. Divorced o/e rates, female", plot.name, sep=""))
          print(p6)
          insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

          row.index <- row.index + 22
          p9 <- rates.plot(rw.oe[which(rw.oe$sex==1&rw.oe$edu==i), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure 7.1. Widowed to remarriage o/e rates, males", plot.name, sep=""))
          print(p9)
          insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
          p10 <- rates.plot(rw.oe[which(rw.oe$sex==2&rw.oe$edu==i), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure 7.2. Widowed to remarriage o/e rates, females", plot.name, sep=""))
          print(p10)
          insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

          row.index <- row.index + 22
          p13 <- rates.plot(rd.oe[which(rd.oe$sex==1&rd.oe$edu==i), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure 8.1. Divorced to remarriage o/e rates, males", plot.name, sep=""))
          print(p13)
          insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
          p14 <- rates.plot(rd.oe[which(rd.oe$sex==2&rd.oe$edu==i), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure 8.2. Divorced to remarriage o/e rates, females", plot.name, sep=""))
          print(p14)
          insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")
        }

        if(cal_freq == TRUE){
          row.index <- 3
          p3 <- rates.plot(mrg1.freq[which(mrg1.freq$sex==1&mrg1.freq$edu==i), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure 1.1. First marriage frequencies, males", plot.name, sep=""))
          print(p3)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
          p4 <- rates.plot(mrg1.freq[which(mrg1.freq$sex==2&mrg1.freq$edu==i), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure 1.2. First marriage frequencies, females", plot.name, sep=""))
          print(p4)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

          row.index <- row.index + 22
          p7 <- rates.plot(divorced.freq[which(divorced.freq$sex==1&divorced.freq$edu==i), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure 2.1. Divorced frequencies, males", plot.name, sep=""))
          print(p7)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
          p8 <- rates.plot(divorced.freq[which(divorced.freq$sex==2&divorced.freq$edu==i), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure 2.2. Divorced frequencies, females", plot.name, sep=""))
          print(p8)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

          row.index <- row.index + 22
          p11 <- rates.plot(rw.freq[which(rw.freq$sex==1&rw.freq$edu==i), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure 3.1. Widowed to remarriage frequencies, males", plot.name, sep=""))
          print(p11)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
          p12 <- rates.plot(rw.freq[which(rw.freq$sex==2&rw.freq$edu==i), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure 3.2. Widowed to remarriage frequencies, females", plot.name, sep=""))
          print(p12)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

          row.index <- row.index + 22
          p15 <- rates.plot(rd.freq[which(rd.freq$sex==1&rd.freq$edu==i), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure 4.1. Divorced to remarriage frequencies, males", plot.name, sep=""))
          print(p15)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
          p16 <- rates.plot(rd.freq[which(rd.freq$sex==2&rd.freq$edu==i), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure 4.2. Divorced to remarriage frequencies, females", plot.name, sep=""))
          print(p16)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")
        }

        output.dir <- getwd()
        plot.name <- gsub(", ", "-", plot.name)
        saveWorkbook(wb, paste0(output.dir, "/", title, " Mar4 plot", plot.name, ".xlsx", sep=""), overwrite=TRUE)
      }
    }
  }

}

write.rates.four <- function(oe.rates, frequency, param, name, method, sex){

  if (sex==T){
    write.rates.four.sex(oe.rates, frequency, param, name, method)
  } else if (sex==F){
    write.rates.four.nosex(oe.rates, frequency, param, name, method)
  }

}

write.rates.four.sex <- function(oe.rates, frequency, param, name, method){
  forDatstl <- createStyle(halign="center", border="TopBottomLeftRight")
  forDatstl2 <- createStyle(halign="right", border="TopBottomLeftRight", numFmt = "#,##0.0000")
  wb <- createWorkbook()
  title <- as.character(param$title)

  nrate <- as.numeric(param$nRate)
  cal_oe <- (nrate==1 | nrate==2)
  cal_freq <- (nrate==1 | nrate==3)
  k<-1

  if (cal_oe==T){
    addWorksheet(wb, "oe.rates")

    writeData(wb, sheet=k, paste0("Table M4. Age-gender-specific marital status transition o/e rates, ", title, name, sep=""),
              startRow=4, startCol=2)
    writeData(wb, sheet=k, "Male", startRow=5, startCol=3, borders="all")
    mergeCells(wb, sheet=k, cols=3:6, rows=5)
    addStyle(wb, sheet=k, style=forDatstl, rows=5, cols=2:6)
    writeData(wb, sheet=k, "Female", startRow=5, startCol=7, borders="all")
    mergeCells(wb, sheet=k, cols=7:10, rows=5)
    addStyle(wb, sheet=k, style=forDatstl, rows=5, cols=7:10)
    colnames(oe.rates) <-
      c("Age", rep(c("First marriage", "Divorced", "Remarriage of widowed", "Remarriage of divorced"),2))
    writeData(wb, sheet=k, oe.rates, startRow=6, startCol=2, borders="all")
    addStyle(wb, sheet=k, style=forDatstl, rows=6, cols=2:10)
    addStyle(wb, sheet=k, style=forDatstl2, rows=7:(6+nrow(oe.rates)), cols=3:10, gridExpand = TRUE)
    setColWidths(wb, sheet=k, cols=2:10, widths=10)
    setColWidths(wb, sheet=k, cols=2, widths=6)

    k<-k+1
  }


  if (cal_freq==T){
    addWorksheet(wb, "frequency")

    writeData(wb, sheet=k, paste0("Table M3. Age-gender-specific marital status transition frequencies, ", title, name, sep=""),
              startRow=4, startCol=2)
    writeData(wb, sheet=k, "Male", startRow=5, startCol=3, borders="all")
    mergeCells(wb, sheet=k, cols=3:6, rows=5)
    addStyle(wb, sheet=k, style=forDatstl, rows=5, cols=2:6)
    writeData(wb, sheet=k, "Female", startRow=5, startCol=7, borders="all")
    mergeCells(wb, sheet=k, cols=7:10, rows=5)
    addStyle(wb, sheet=k, style=forDatstl, rows=5, cols=7:10)
    colnames(frequency) <-
      c("Age", rep(c("First marriage", "Divorced", "Remarriage of widowed", "Remarriage of divorced"),2))
    writeData(wb, sheet=k, frequency, startRow=6, startCol=2, borders="all")
    addStyle(wb, sheet=k, style=forDatstl, rows=6, cols=2:10)
    addStyle(wb, sheet=k, style=forDatstl2, rows=7:(6+nrow(frequency)), cols=3:10, gridExpand = TRUE)
    setColWidths(wb, sheet=k, cols=2:10, widths=10)
    setColWidths(wb, sheet=k, cols=2, widths=6)
  }

  output.dir <- getwd()
  name <- gsub(", ", "-", name)
  saveWorkbook(wb, paste0(output.dir, "/", title, " Mar4 ", method, " rates", name, ".xlsx", sep=""), overwrite=TRUE)
}

write.rates.four.nosex <- function(oe.rates, frequency, param, name, method){
  forDatstl <- createStyle(halign="center", border="TopBottomLeftRight")
  forDatstl2 <- createStyle(halign="right", border="TopBottomLeftRight", numFmt = "#,##0.0000")
  wb <- createWorkbook()
  title <- as.character(param$title)

  nrate <- as.numeric(param$nRate)
  cal_oe <- (nrate==1 | nrate==2)
  cal_freq <- (nrate==1 | nrate==3)
  k<-1

  if (cal_oe==T){
    addWorksheet(wb, "oe.rates")

    writeData(wb, sheet=k, paste0("Table M4. Age-specific marital status transition o/e rates, ", title, name, sep=""),
              startRow=4, startCol=2)
    colnames(oe.rates) <-
      c("Age", "First marriage", "Divorced", "Remarriage of widowed", "Remarriage of divorced")
    writeData(wb, sheet=k, oe.rates, startRow=5, startCol=2, borders="all")
    addStyle(wb, sheet=k, style=forDatstl, rows=5, cols=2:6)
    addStyle(wb, sheet=k, style=forDatstl2, rows=6:(5+nrow(oe.rates)), cols=3:6, gridExpand = TRUE)
    setColWidths(wb, sheet=k, cols=2:6, widths=10)
    setColWidths(wb, sheet=k, cols=2, widths=6)

    k<-k+1
  }

  if (cal_freq==T){
    addWorksheet(wb, "frequency")

    writeData(wb, sheet=k, paste0("Table M3. Age-specific marital status transition frequencies, ", title, name, sep=""),
              startRow=4, startCol=2)
    colnames(frequency) <-
      c("Age", "First marriage", "Divorced", "Remarriage of widowed", "Remarriage of divorced")
    writeData(wb, sheet=k, frequency, startRow=5, startCol=2, borders="all")
    addStyle(wb, sheet=k, style=forDatstl, rows=5, cols=2:6)
    addStyle(wb, sheet=k, style=forDatstl2, rows=6:(5+nrow(frequency)), cols=3:6, gridExpand = TRUE)
    setColWidths(wb, sheet=k, cols=2:6, widths=10)
    setColWidths(wb, sheet=k, cols=2, widths=6)
  }

  output.dir <- getwd()
  name <- gsub(", ", "-", name)
  saveWorkbook(wb, paste0(output.dir, "/", title, " Mar4 ", method, " rates", name, ".xlsx", sep=""), overwrite=TRUE)
}

write.pop.four <- function(pop, param, name, sex){

  if (sex==T){
    write.pop.four.sex(pop, param, name)
  } else if (sex==F){
    write.pop.four.nosex(pop, param, name)
  }

}

write.pop.four.sex <- function(pop, param, name){

  #define formate
  forDatstl <- createStyle(halign="center", border="TopBottomLeftRight", wrapText = TRUE)
  forDatstl2 <- createStyle(halign="right", border="TopBottomLeftRight", numFmt = "#,##0.0")

  #create work book
  wb <- createWorkbook()

  #add worksheets
  addWorksheet(wb, "status")
  addWorksheet(wb, "event")

  title <- as.character(param$title)

  #write status sheet
  writeData(wb, sheet=1,
            paste0("Table M1. Person-years by age, gender and marital status, ", title, name, sep=""),
            startRow=4, startCol=2)
  mergeCells(wb, sheet=1, cols=2:9, rows=4)
  writeData(wb, sheet=1, "Male", startRow=5, startCol=3, borders="all")
  mergeCells(wb, sheet=1, cols=3:7, rows=5)
  addStyle(wb, sheet=1, style=forDatstl, rows=5, cols=2:7)
  writeData(wb, sheet=1, "Female", startRow=5, startCol=8, borders="all")
  mergeCells(wb, sheet=1, cols=8:12, rows=5)
  addStyle(wb, sheet=1, style=forDatstl, rows=5, cols=8:12)
  colnames(pop$status) <-
    c("Age", rep(c("All", "Never married", "Married", "Widowed", "Divorced"),2))
  writeData(wb, sheet=1, pop$status, startRow=6, startCol=2, borders="all")
  addStyle(wb, sheet=1, style=forDatstl, rows=6, cols=2:12)
  addStyle(wb, sheet=1, style=forDatstl2, rows=7:(6+nrow(pop$status)), cols=3:12, gridExpand = TRUE)
  setColWidths(wb, sheet=1, cols=2:12, widths=13)
  setColWidths(wb, sheet=1, cols=2, widths=7)

  #write event sheet
  writeData(wb, sheet=2, paste0("Table M2. Number of event by age, gender and event type, ", title, name, sep=""),
            startRow=4, startCol=2)
  mergeCells(wb, sheet=2, cols=2:9, rows=4)
  writeData(wb, sheet=2, "Male", startRow=5, startCol=3, borders="all")
  mergeCells(wb, sheet=2, cols=3:7, rows=5)
  addStyle(wb, sheet=2, style=forDatstl, rows=5, cols=2:7)
  writeData(wb, sheet=2, "Female", startRow=5, startCol=8, borders="all")
  mergeCells(wb, sheet=2, cols=8:12, rows=5)
  addStyle(wb, sheet=2, style=forDatstl, rows=5, cols=8:12)
  colnames(pop$event) <-
    c("Age", rep(c("All", "First marriage", "Divorced", "Remarriage of widowed", "Remarriage of divorced"),2))
  writeData(wb, sheet=2, pop$event, startRow=6, startCol=2, borders="all")
  addStyle(wb, sheet=2, style=forDatstl, rows=6, cols=2:12)
  setColWidths(wb, sheet=2, cols=3:12, widths=12)
  setColWidths(wb, sheet=2, cols=2, widths=6)

  #output pop table
  output.dir <- getwd()
  name <- gsub(", ", "-", name)
  saveWorkbook(wb, paste0(output.dir, "/", title, " Mar4 popTable", name, ".xlsx", sep=""), overwrite=T)
}

write.pop.four.nosex <- function(pop, param, name){

  forDatstl <- createStyle(halign="center", border="TopBottomLeftRight", wrapText = TRUE)
  forDatstl2 <- createStyle(halign="right", border="TopBottomLeftRight", numFmt = "#,##0.0")
  wb <- createWorkbook()
  addWorksheet(wb, "status")
  addWorksheet(wb, "event")
  title <- as.character(param$title)

  writeData(wb, sheet=1,
            paste0("Table M1. Person-years by age and marital status, ", title, name, sep=""),
            startRow=4, startCol=2)
  mergeCells(wb, sheet=1, cols=2:7, rows=4)
  colnames(pop$status) <-
    c("Age", "All", "Never married", "Married", "Widowed", "Divorced")
  writeData(wb, sheet=1, pop$status, startRow=5, startCol=2, borders="all")
  addStyle(wb, sheet=1, style=forDatstl, rows=5, cols=2:7)
  addStyle(wb, sheet=1, style=forDatstl2, rows=6:(5+nrow(pop$status)), cols=3:7, gridExpand = TRUE)
  setColWidths(wb, sheet=1, cols=3:7, widths=13)
  setColWidths(wb, sheet=1, cols=2, widths=7)

  writeData(wb, sheet=2, paste0("Table M2. Number of event by age and event type, ", title, name, sep=""),
            startRow=4, startCol=2)
  mergeCells(wb, sheet=2, cols=2:7, rows=4)
  colnames(pop$event) <-
    c("Age", "All", "First marriage", "Divorced", "Remarriage of widowed", "Remarriage of divorced")
  writeData(wb, sheet=2, pop$event, startRow=5, startCol=2, borders="all")
  addStyle(wb, sheet=2, style=forDatstl, rows=5, cols=2:7)
  setColWidths(wb, sheet=2, cols=3:7, widths=12)
  setColWidths(wb, sheet=2, cols=2, widths=6)

  output.dir <- getwd()
  name <- gsub(", ", "-", name)
  saveWorkbook(wb, paste0(output.dir, "/", title, " Mar4 popTable", name, ".xlsx", sep=""), overwrite=T)
}

write.total.four <- function(total.rates, mean.age, param, name, sex){
  if (sex==T){
    write.total.four.sex(total.rates, mean.age, param, name)
  } else if (sex==F){
    write.total.four.nosex(total.rates, mean.age, param, name)
  }
}

write.total.four.subset <- function(total.rates, mean.age, param, name){
  #name = "-rural and urban"
  forDatstl <- createStyle(halign="center", border="TopBottomLeftRight", wrapText = TRUE)
  forDatstl2 <- createStyle(halign="left", border="TopBottomLeftRight", wrapText = TRUE)
  forDatstl3 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0000")
  forDatstl4 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0")
  forDatstl5 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0%")
  wb <- createWorkbook()
  addWorksheet(wb, "total rates")
  title <- as.character(param$title)

  if (nrow(total.rates)==3){
    writeData(wb, sheet=1, paste0("Table M5. Total rates and mean ages of marital status transitions, ", title, sep=""),
              startRow=4, startCol=2)
    mergeCells(wb, sheet=1, cols=2:8, rows=4)
    writeData(wb, sheet=1, "First Marriage", startRow=5, startCol=3, borders="all")
    mergeCells(wb, sheet=1, cols=3:4, rows=5)
    writeData(wb, sheet=1, "Remarriage", startRow=5, startCol=5, borders="all")
    mergeCells(wb, sheet=1, cols=5:6, rows=5)
    writeData(wb, sheet=1, "Divorce", startRow=5, startCol=7, borders="all")
    mergeCells(wb, sheet=1, cols=7:8, rows=5)
    rownames(total.rates) <- c("Direct calculate",  paste0(method, " estimate", sep=""), "Difference%")
    rownames(mean.age) <- c("Direct calculate",  paste0(method, " estimate", sep=""), "Difference%")
    cname <- c(rep(c("Male", "Female"),3))
    cname <- as.data.frame(t(cname))
    writeData(wb, sheet=1, cname, startRow=6, startCol=3, colNames = FALSE, borders="all")
    writeData(wb, sheet=1, "Total rates", startRow=7, startCol=2, borders="all")
    writeData(wb, sheet=1, total.rates, startRow=8, startCol=2, colNames = FALSE, rowNames = TRUE, borders="all")
    writeData(wb, sheet=1, "Mean age", startRow=11, startCol=2, borders="all")
    writeData(wb, sheet=1, mean.age, startRow=12, startCol=2, colNames = FALSE, rowNames = TRUE, borders="all")
    addStyle(wb, sheet=1, style=forDatstl, rows=5:6, cols=2:8, gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl3, rows=7:9, cols=3:8, gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl5, rows=10, cols=3:8)
    addStyle(wb, sheet=1, style=forDatstl4, rows=11:13, cols=3:8, gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl5, rows=14, cols=3:8)
    setColWidths(wb, sheet=1, cols=3:8, widths=7)
    setColWidths(wb, sheet=1, cols=2, widths=17)
  } else {
    writeData(wb, sheet=1, paste0("Table M5. Total rates and mean ages of marital status transitions, ", title, sep=""),
              startRow=4, startCol=2)
    mergeCells(wb, sheet=1, cols=2:7, rows=4)
    writeData(wb, sheet=1, "First Marriage", startRow=5, startCol=3, borders="all")
    mergeCells(wb, sheet=1, cols=3:4, rows=5)
    writeData(wb, sheet=1, "Remarriage", startRow=5, startCol=5, borders="all")
    mergeCells(wb, sheet=1, cols=5:6, rows=5)
    writeData(wb, sheet=1, "Divorce", startRow=5, startCol=7, borders="all")
    mergeCells(wb, sheet=1, cols=7:8, rows=5)
    colnames(total.rates) <- c(" ", rep(c("Male", "Female"),3))
    writeData(wb, sheet=1, total.rates, startRow=6, startCol=2, borders="all")
    writeData(wb, sheet=1, mean.age, startRow=7+nrow(total.rates), startCol=2, colNames = FALSE, borders="all")
    addStyle(wb, sheet=1, style=forDatstl, rows=5:6, cols=2:8, gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl3, rows=7:(nrow(total.rates)+6), cols=3:8, gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl4, rows=(nrow(total.rates)+7):(nrow(total.rates)+nrow(mean.age)+6), cols=3:8, gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl5, rows=seq(1, nrow(total.rates)+nrow(mean.age), by=4)+9, cols=3:8, gridExpand = TRUE)
    setColWidths(wb, sheet=1, cols=3:8, widths=7)
    setColWidths(wb, sheet=1, cols=2, widths=17)
  }

  output.dir <- getwd()
  saveWorkbook(wb, paste0(output.dir, "/", title, " Mar4 total rates", name, ".xlsx", sep=""), overwrite=T)
}

write.total.four.sex <- function(total.rates, mean.age, param, name){
  forDatstl <- createStyle(halign="center", border="TopBottomLeftRight", wrapText = TRUE)
  forDatstl2 <- createStyle(halign="left", border="TopBottomLeftRight", wrapText = TRUE)
  forDatstl3 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0000")
  forDatstl4 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0")
  forDatstl5 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0%")
  wb <- createWorkbook()
  addWorksheet(wb, "total rates")
  title <- as.character(param$title)

  if (nrow(total.rates)==3){
    writeData(wb, sheet=1, paste0("Table M5. Total rates and mean ages of marital status transitions, ", title, sep=""),
              startRow=4, startCol=2)
    mergeCells(wb, sheet=1, cols=2:8, rows=4)
    writeData(wb, sheet=1, "First Marriage", startRow=5, startCol=3, borders="all")
    mergeCells(wb, sheet=1, cols=3:4, rows=5)
    writeData(wb, sheet=1, "Remarriage", startRow=5, startCol=5, borders="all")
    mergeCells(wb, sheet=1, cols=5:6, rows=5)
    writeData(wb, sheet=1, "Divorce", startRow=5, startCol=7, borders="all")
    mergeCells(wb, sheet=1, cols=7:8, rows=5)
    rownames(total.rates) <- c("Direct calculate", "Poisson estimate", "Difference%")
    rownames(mean.age) <- c("Direct calculate", "Poisson estimate", "Difference%")
    cname <- c(rep(c("Male", "Female"),3))
    cname <- as.data.frame(t(cname))
    writeData(wb, sheet=1, cname, startRow=6, startCol=3, colNames = FALSE, borders="all")
    writeData(wb, sheet=1, "Total rates", startRow=7, startCol=2, borders="all")
    writeData(wb, sheet=1, total.rates, startRow=8, startCol=2, colNames = FALSE, rowNames = TRUE, borders="all")
    writeData(wb, sheet=1, "Mean age", startRow=11, startCol=2, borders="all")
    writeData(wb, sheet=1, mean.age, startRow=12, startCol=2, colNames = FALSE, rowNames = TRUE, borders="all")
    addStyle(wb, sheet=1, style=forDatstl, rows=5:6, cols=2:8, gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl3, rows=7:9, cols=3:8, gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl5, rows=10, cols=3:8)
    addStyle(wb, sheet=1, style=forDatstl4, rows=11:13, cols=3:8, gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl5, rows=14, cols=3:8)
    setColWidths(wb, sheet=1, cols=3:8, widths=7)
    setColWidths(wb, sheet=1, cols=2, widths=17)
  } else {
    writeData(wb, sheet=1, paste0("Table M5. Total rates of marital status transitions, ", title, sep=""),
              startRow=4, startCol=2)
    mergeCells(wb, sheet=1, cols=2:9, rows=4)
    writeData(wb, sheet=1, "First Marriage", startRow=5, startCol=4, borders="all")
    mergeCells(wb, sheet=1, cols=4:5, rows=5)
    writeData(wb, sheet=1, "Remarriage", startRow=5, startCol=6, borders="all")
    mergeCells(wb, sheet=1, cols=6:7, rows=5)
    writeData(wb, sheet=1, "Divorce", startRow=5, startCol=8, borders="all")
    mergeCells(wb, sheet=1, cols=8:9, rows=5)
    colnames(total.rates) <- c("", "", rep(c("Male", "Female"),3))
    writeData(wb, sheet=1, total.rates, startRow=6, startCol=2, borders="all")
    writeData(wb, sheet=1, "Name", startRow=5, startCol=2, borders="all")
    mergeCells(wb, sheet=1, cols=2, rows=5:6)
    writeData(wb, sheet=1, "Direct calculate and Poisson estimate", startRow=5, startCol=3, borders="all")
    mergeCells(wb, sheet=1, cols=3, rows=5:6)
    addStyle(wb, sheet=1, style=forDatstl, rows=5:6, cols=2:9, gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl3, rows=7:(nrow(total.rates)+6), cols=4:9, gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl5, rows=seq(1, nrow(total.rates), by=3)+8, cols=4:9, gridExpand = TRUE)
    setColWidths(wb, sheet=1, cols=4:9, widths=7)
    setColWidths(wb, sheet=1, cols=2, widths=13)
    setColWidths(wb, sheet=1, cols=3, widths=17)

    addWorksheet(wb, "mean ages")
    writeData(wb, sheet=2, paste0("Table M5. Mean ages of marital status transitions, ", title, sep=""),
              startRow=4, startCol=2)
    mergeCells(wb, sheet=2, cols=2:9, rows=4)
    writeData(wb, sheet=2, "First Marriage", startRow=5, startCol=4, borders="all")
    mergeCells(wb, sheet=2, cols=4:5, rows=5)
    writeData(wb, sheet=2, "Remarriage", startRow=5, startCol=6, borders="all")
    mergeCells(wb, sheet=2, cols=6:7, rows=5)
    writeData(wb, sheet=2, "Divorce", startRow=5, startCol=8, borders="all")
    mergeCells(wb, sheet=2, cols=8:9, rows=5)
    colnames(mean.age) <- c("", "", rep(c("Male", "Female"),3))
    writeData(wb, sheet=2, mean.age, startRow=6, startCol=2, borders="all")
    writeData(wb, sheet=2, "Name", startRow=5, startCol=2, borders="all")
    mergeCells(wb, sheet=2, cols=2, rows=5:6)
    writeData(wb, sheet=2, "Direct calculate and Poisson estimate", startRow=5, startCol=3, borders="all")
    mergeCells(wb, sheet=2, cols=3, rows=5:6)
    addStyle(wb, sheet=2, style=forDatstl, rows=5:6, cols=2:9, gridExpand = TRUE)
    addStyle(wb, sheet=2, style=forDatstl4, rows=7:(nrow(mean.age)+6), cols=4:9, gridExpand = TRUE)
    addStyle(wb, sheet=2, style=forDatstl5, rows=seq(1, nrow(mean.age), by=3)+8, cols=4:9, gridExpand = TRUE)
    setColWidths(wb, sheet=2, cols=4:9, widths=7)
    setColWidths(wb, sheet=2, cols=2, widths=13)
    setColWidths(wb, sheet=2, cols=3, widths=17)
  }

  output.dir <- getwd()
  saveWorkbook(wb, paste0(output.dir, "/", title, " Mar4 total rates", name, ".xlsx", sep=""), overwrite=T)
}

write.total.four.nosex <- function(total.rates, mean.age, param, name){
  forDatstl <- createStyle(halign="center", border="TopBottomLeftRight", wrapText = TRUE)
  forDatstl2 <- createStyle(halign="left", border="TopBottomLeftRight", wrapText = TRUE)
  forDatstl3 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0000")
  forDatstl4 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0")
  forDatstl5 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0%")
  wb <- createWorkbook()
  addWorksheet(wb, "total rates")
  title <- as.character(param$title)

  if (nrow(total.rates)==3){
    writeData(wb, sheet=1, paste0("Table M5. Total rates and mean ages of marital status transitions, ", title, sep=""),
              startRow=4, startCol=2)
    mergeCells(wb, sheet=1, cols=2:5, rows=4)
    rownames(total.rates) <- c("Direct calculate", "Poisson estimate", "Difference%")
    rownames(mean.age) <- c("Direct calculate", "Poisson estimate", "Difference%")
    cname <- c("First Marriage", "Remarriage", "Divorce")
    cname <- as.data.frame(t(cname))
    writeData(wb, sheet=1, cname, startRow=5, startCol=3, colNames = FALSE, borders="all")
    writeData(wb, sheet=1, "Total rates", startRow=6, startCol=2, borders="all")
    writeData(wb, sheet=1, total.rates, startRow=7, startCol=2, colNames = FALSE, rowNames = TRUE, borders="all")
    writeData(wb, sheet=1, "Mean age", startRow=10, startCol=2, borders="all")
    writeData(wb, sheet=1, mean.age, startRow=11, startCol=2, colNames = FALSE, rowNames = TRUE, borders="all")
    addStyle(wb, sheet=1, style=forDatstl, rows=4:5, cols=2:5, gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl3, rows=6:8, cols=3:5, gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl5, rows=9, cols=3:5)
    addStyle(wb, sheet=1, style=forDatstl4, rows=10:12, cols=3:5, gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl5, rows=13, cols=3:5)
    setColWidths(wb, sheet=1, cols=3:5, widths=10)
    setColWidths(wb, sheet=1, cols=2, widths=17)
  } else {
    writeData(wb, sheet=1, paste0("Table M5. Total rates of marital status transitions, ", title, sep=""),
              startRow=4, startCol=2)
    mergeCells(wb, sheet=1, cols=2:6, rows=4)
    colnames(total.rates) <- c("Name", "Direct calculate and Poisson estimate", "First Marriage", "Remarriage", "Divorce")
    writeData(wb, sheet=1, total.rates, startRow=5, startCol=2, borders="all")
    addStyle(wb, sheet=1, style=forDatstl, rows=5, cols=2:6, gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl3, rows=6:(nrow(total.rates)+5), cols=4:6, gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl5, rows=seq(1, nrow(total.rates), by=3)+7, cols=4:6, gridExpand = TRUE)
    setColWidths(wb, sheet=1, cols=4:6, widths=10)
    setColWidths(wb, sheet=1, cols=2, widths=13)
    setColWidths(wb, sheet=1, cols=3, widths=17)

    addWorksheet(wb, "mean ages")
    writeData(wb, sheet=2, paste0("Table M5. Mean ages of marital status transitions, ", title, sep=""),
              startRow=4, startCol=2)
    mergeCells(wb, sheet=2, cols=2:6, rows=4)
    colnames(mean.age) <- c("Name", "Direct calculate and Poisson estimate", "First Marriage", "Remarriage", "Divorce")
    writeData(wb, sheet=2, mean.age, startRow=5, startCol=2, borders="all")
    addStyle(wb, sheet=2, style=forDatstl, rows=5, cols=2:6, gridExpand = TRUE)
    addStyle(wb, sheet=2, style=forDatstl4, rows=6:(nrow(mean.age)+5), cols=4:6, gridExpand = TRUE)
    addStyle(wb, sheet=2, style=forDatstl5, rows=seq(1, nrow(mean.age), by=3)+7, cols=4:6, gridExpand = TRUE)
    setColWidths(wb, sheet=2, cols=4:6, widths=10)
    setColWidths(wb, sheet=2, cols=2, widths=13)
    setColWidths(wb, sheet=2, cols=3, widths=17)
  }

  output.dir <- getwd()
  saveWorkbook(wb, paste0(output.dir, "/", title, " Mar4 total rates", name, ".xlsx", sep=""), overwrite=T)
}

write.total.four.covar <- function(total.rates.ru, total.rates.ub, mean.age.ru, mean.age.ub, param, name, sex){
  if (sex==T){
    write.total.four.covar.sex(total.rates.ru, total.rates.ub, mean.age.ru, mean.age.ub, param, name)
  } else if (sex==F){
    write.total.four.covar.nosex(total.rates.ru, total.rates.ub, mean.age.ru, mean.age.ub, param, name)
  }
}

write.total.four.covar.sex <- function(total.rates.ru, total.rates.ub, mean.age.ru, mean.age.ub, param, name){
  forDatstl <- createStyle(halign="center", border="TopBottomLeftRight", wrapText = TRUE)
  forDatstl2 <- createStyle(halign="left", border="TopBottomLeftRight", wrapText = TRUE)
  forDatstl3 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0000")
  forDatstl4 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0")
  forDatstl5 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0%")
  wb <- createWorkbook()
  title <- as.character(param$title)

  for (i in 1:4) {

    if (i==1){
      addWorksheet(wb, "total rates rural")
      writeData(wb, sheet=1, paste0("Table M5. Total rates of marital status transitions, ", title, sep=""),
                startRow=4, startCol=2)
      input <- total.rates.ru
    }

    if (i==2){
      addWorksheet(wb, "mean ages rural")
      writeData(wb, sheet=2, paste0("Table M5. Mean ages of marital status transitions, ", title, sep=""),
                startRow=4, startCol=2)
      input <- mean.age.ru
    }

    if (i==3){
      addWorksheet(wb, "total rates urban")
      writeData(wb, sheet=3, paste0("Table M5. Total rates of marital status transitions, ", title, sep=""),
                startRow=4, startCol=2)
      input <- total.rates.ub
    }

    if (i==4){
      addWorksheet(wb, "mean ages urban")
      writeData(wb, sheet=4, paste0("Table M5. Mean ages of marital status transitions, ", title, sep=""),
                startRow=4, startCol=2)
      input <- mean.age.ub
    }

    writeData(wb, sheet=i, "First Marriage", startRow=5, startCol=3, borders="all")
    mergeCells(wb, sheet=i, cols=3:8, rows=5)
    writeData(wb, sheet=i, "Remarriage", startRow=5, startCol=9, borders="all")
    mergeCells(wb, sheet=i, cols=9:14, rows=5)
    writeData(wb, sheet=i, "Divorce", startRow=5, startCol=15, borders="all")
    mergeCells(wb, sheet=i, cols=15:20, rows=5)
    writeData(wb, sheet=i, "Male", startRow=6, startCol=3, borders="all")
    writeData(wb, sheet=i, "Male", startRow=6, startCol=9, borders="all")
    writeData(wb, sheet=i, "Male", startRow=6, startCol=15, borders="all")
    writeData(wb, sheet=i, "Female", startRow=6, startCol=6, borders="all")
    writeData(wb, sheet=i, "Female", startRow=6, startCol=12, borders="all")
    writeData(wb, sheet=i, "Female", startRow=6, startCol=18, borders="all")
    mergeCells(wb, sheet=i, cols=3:5, rows=6)
    mergeCells(wb, sheet=i, cols=6:8, rows=6)
    mergeCells(wb, sheet=i, cols=9:11, rows=6)
    mergeCells(wb, sheet=i, cols=12:14, rows=6)
    mergeCells(wb, sheet=i, cols=15:17, rows=6)
    mergeCells(wb, sheet=i, cols=18:20, rows=6)
    colnames(input) <- c("Name", rep(c("Direct Calculate", "Poisson Estimate", "Difference%"),6))
    writeData(wb, sheet=i, input, startRow=7, startCol=2, borders="all")
    addStyle(wb, sheet=i, style=forDatstl, rows=5:7, cols=2:20, gridExpand = TRUE)
    addStyle(wb, sheet=i, style=forDatstl2, rows=8:(nrow(input)+7), cols=2, gridExpand = TRUE)

    if (i%%2==0){
      addStyle(wb, sheet=i, style=forDatstl4, rows=8:(nrow(input)+7), cols=c(3:4, 6:7, 9:10, 12:13, 15:16, 18:19), gridExpand = TRUE)
    } else {
      addStyle(wb, sheet=i, style=forDatstl3, rows=8:(nrow(input)+7), cols=c(3:4, 6:7, 9:10, 12:13, 15:16, 18:19), gridExpand = TRUE)
    }

    addStyle(wb, sheet=i, style=forDatstl5, rows=8:(nrow(input)+7), cols=seq(5, 20, 3), gridExpand = TRUE)
    setColWidths(wb, sheet=i, cols=3:20, widths=9)
    setColWidths(wb, sheet=i, cols=2, widths=13)

  }

  output.dir <- getwd()
  saveWorkbook(wb, paste0(output.dir, "/", title, " Mar4 total rates", name, ".xlsx", sep=""), overwrite=T)
}

write.total.four.covar.nosex <- function(total.rates.ru, total.rates.ub, mean.age.ru, mean.age.ub, param, name){
  forDatstl <- createStyle(halign="center", border="TopBottomLeftRight", wrapText = TRUE)
  forDatstl2 <- createStyle(halign="left", border="TopBottomLeftRight", wrapText = TRUE)
  forDatstl3 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0000")
  forDatstl4 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0")
  forDatstl5 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0%")
  wb <- createWorkbook()
  title <- as.character(param$title)

  for (i in 1:4) {

    if (i==1){
      addWorksheet(wb, "total rates rural")
      writeData(wb, sheet=1, paste0("Table M5. Total rates of marital status transitions, ", title, sep=""),
                startRow=4, startCol=2)
      input <- total.rates.ru
    }

    if (i==2){
      addWorksheet(wb, "mean ages rural")
      writeData(wb, sheet=2, paste0("Table M5. Mean ages of marital status transitions, ", title, sep=""),
                startRow=4, startCol=2)
      input <- mean.age.ru
    }

    if (i==3){
      addWorksheet(wb, "total rates urban")
      writeData(wb, sheet=3, paste0("Table M5. Total rates of marital status transitions, ", title, sep=""),
                startRow=4, startCol=2)
      input <- total.rates.ub
    }

    if (i==4){
      addWorksheet(wb, "mean ages urban")
      writeData(wb, sheet=4, paste0("Table M5. Mean ages of marital status transitions, ", title, sep=""),
                startRow=4, startCol=2)
      input <- mean.age.ub
    }

    writeData(wb, sheet=i, "First Marriage", startRow=5, startCol=3, borders="all")
    mergeCells(wb, sheet=i, cols=3:5, rows=5)
    writeData(wb, sheet=i, "Remarriage", startRow=5, startCol=6, borders="all")
    mergeCells(wb, sheet=i, cols=6:8, rows=5)
    writeData(wb, sheet=i, "Divorce", startRow=5, startCol=9, borders="all")
    mergeCells(wb, sheet=i, cols=9:11, rows=5)
    colnames(input) <- c("Region Name", rep(c("Direct Calculate", "Poisson Estimate", "Difference%"),3))
    writeData(wb, sheet=i, input, startRow=6, startCol=2, borders="all")
    addStyle(wb, sheet=i, style=forDatstl, rows=5:6, cols=2:11, gridExpand = TRUE)
    addStyle(wb, sheet=i, style=forDatstl2, rows=7:(nrow(input)+6), cols=2, gridExpand = TRUE)

    if (i%%2==0){
      addStyle(wb, sheet=i, style=forDatstl4, rows=7:(nrow(input)+6), cols=c(3:4, 6:7, 9:10), gridExpand = TRUE)
    } else {
      addStyle(wb, sheet=i, style=forDatstl3, rows=7:(nrow(input)+6), cols=c(3:4, 6:7, 9:10), gridExpand = TRUE)
    }

    addStyle(wb, sheet=i, style=forDatstl5, rows=7:(nrow(input)+6), cols=c(5, 8, 11), gridExpand = TRUE)
    setColWidths(wb, sheet=i, cols=3:11, widths=9)
    setColWidths(wb, sheet=i, cols=2, widths=13)


  }

  output.dir <- getwd()
  saveWorkbook(wb, paste0(output.dir, "/", title, " Mar4 total rates", name, ".xlsx", sep=""), overwrite=T)
}

##------7 Marital-------
freq.mar7 <- function(frequency, sex){

  if (sex==F){

    freq <- data.frame(age = frequency$age)

    #combine event columns into first marriage, remarriage, divorce, cohabitation, and cohabitation dissolution.
    freq <- data.frame(freq,
                       mrg1 = tmp.rowSum.w.na(frequency[c(2:3)]),
                       mrg = tmp.rowSum.w.na(frequency[c(5:8)]),
                       div = tmp.rowSum.w.na(frequency[4]),
                       coh = tmp.rowSum.w.na(frequency[c(9:11)]),
                       cd = tmp.rowSum.w.na(frequency[c(12:14)]))

  } else if (sex==T){

    freq <- data.frame(age = frequency[[1]]$age)

    #combine event columns into first marriage, remarriage, divorce, cohabitation, and cohabitation dissolution by sex.
    freq <- data.frame(freq,
                       male.mrg1 = tmp.rowSum.w.na(frequency[[1]][c(2:3)]),
                       female.mrg1 = tmp.rowSum.w.na(frequency[[2]][c(2:3)]),
                       male.mrg = tmp.rowSum.w.na(frequency[[1]][c(5:8)]),
                       female.mrg = tmp.rowSum.w.na(frequency[[2]][c(5:8)]),
                       male.div = tmp.rowSum.w.na(frequency[[1]][4]),
                       female.div = tmp.rowSum.w.na(frequency[[2]][4]),
                       male.coh = tmp.rowSum.w.na(frequency[[1]][c(9:11)]),
                       female.coh = tmp.rowSum.w.na(frequency[[2]][c(9:11)]),
                       male.cd = tmp.rowSum.w.na(frequency[[1]][c(12:14)]),
                       female.cd = tmp.rowSum.w.na(frequency[[2]][c(12:14)]))

  }

  #calculate total rates
  total.rates <- total.freq(freq)
  #calculate mean ages
  mean.age <- mean.age.cal(freq)

  #add total rates as the last row of frequency data frame.
  if (sex==F){

    freq.total <- data.frame(age = "Total", total.freq(frequency))
    frequency <- rbind(frequency, freq.total)

  } else if (sex==T){

    freq.total <- data.frame(age = "Total", total.freq(frequency[[1]]))
    frequency[[1]] <- rbind(frequency[[1]], freq.total)

    freq.total <- data.frame(age = "Total", total.freq(frequency[[2]]))
    frequency[[2]] <- rbind(frequency[[2]], freq.total)

  }

  return(list(frequency=frequency, total.rates=total.rates, mean.age=mean.age))
}

pop.count.seven.sex <- function(data, nlm, nhm){
  age <- data.frame(age=seq(nlm, nhm, 1))
  d.001 <- subset(data, select=c(age, sex, post, event, py))

  # marital status
  if(nrow(d.001)==0){
    temp <- data.frame(age=NA,sex=NA,status=NA, count=NA)[F,]
  } else {
    temp <- aggregate(cbind(d.001[0], count=d.001$py),
                      list(age=d.001$age, sex=d.001$sex, status=d.001$post), sum)
  }

  if(nrow(d.001)==0){
    all <- data.frame(age=NA,sex=NA,x=NA)[F,]
  } else {
    all <- aggregate(cbind(d.001[0], count=d.001$py),
                     list(age=d.001$age, sex=d.001$sex), sum)
    all <- merge(age, all, all.x = T)
  }

  male.nm.NC <- merge(age, subset(temp, sex == 1 & status == 1, select=c(age, count)), all.x=TRUE)
  female.nm.NC <- merge(age, subset(temp, sex == 2 & status == 1, select=c(age, count)), all.x=TRUE)
  male.married <- merge(age, subset(temp, sex == 1 & status == 2, select=c(age, count)), all.x=TRUE)
  female.married <- merge(age, subset(temp, sex == 2 & status == 2, select=c(age, count)), all.x=TRUE)
  male.widowed.NC <- merge(age, subset(temp, sex == 1 & status == 3, select=c(age, count)), all.x=TRUE)
  female.widowed.NC <- merge(age, subset(temp, sex == 2 & status == 3, select=c(age, count)), all.x=TRUE)
  male.divorced.NC <- merge(age, subset(temp, sex == 1 & status == 4, select=c(age, count)), all.x=TRUE)
  female.divorced.NC <- merge(age, subset(temp, sex == 2 & status == 4, select=c(age, count)), all.x=TRUE)
  male.nm.C <- merge(age, subset(temp, sex == 1 & status == 5, select=c(age, count)), all.x=TRUE)
  female.nm.C <- merge(age, subset(temp, sex == 2 & status == 5, select=c(age, count)), all.x=TRUE)
  male.widowed.C <- merge(age, subset(temp, sex == 1 & status == 7, select=c(age, count)), all.x=TRUE)
  female.widowed.C <- merge(age, subset(temp, sex == 2 & status == 7, select=c(age, count)), all.x=TRUE)
  male.divorced.C <- merge(age, subset(temp, sex == 1 & status == 8, select=c(age, count)), all.x=TRUE)
  female.divorced.C <- merge(age, subset(temp, sex == 2 & status == 8, select=c(age, count)), all.x=TRUE)
  all.male <- merge(age, subset(all, sex == 1, select=c(age, count)), all.x=TRUE)
  names(all.male) <- c("age", "count")
  all.female <- merge(age, subset(all, sex == 2, select=c(age, count)), all.x=TRUE)
  names(all.female) <- c("age", "count")

  # month
  male.status <- data.frame(age=age$age, all.male=all.male$count, male.never.married.NC=male.nm.NC$count,
                            male.married=male.married$count, male.widowed.NC=male.widowed.NC$count,
                            male.divorced.NC=male.divorced.NC$count, male.never.married.C=male.nm.C$count,
                            male.widowed.C=male.widowed.C$count, male.divorced.C=male.divorced.C$count)
  female.status <- data.frame(age=age$age, all.female=all.female$count, female.never.married.NC=female.nm.NC$count,
                              female.married=female.married$count, female.widowed.NC=female.widowed.NC$count,
                              female.divorced.NC=female.divorced.NC$count, female.never.married.C=female.nm.C$count,
                              female.widowed.C=female.widowed.C$count, female.divorced.C=female.divorced.C$count)
  male.status[, 2:ncol(male.status)] <- round(male.status[, 2:ncol(male.status)], 1)
  female.status[, 2:ncol(female.status)] <- round(female.status[, 2:ncol(female.status)], 1)

  risk.total1 <- data.frame(age = "Total", t(colSums(male.status[2:ncol(male.status)], na.rm = T)))
  risk.total2 <- data.frame(age = "Total", t(colSums(female.status[2:ncol(female.status)], na.rm = T)))
  male.status <- rbind(male.status, risk.total1)
  female.status <- rbind(female.status, risk.total2)
  rm(risk.total1)
  rm(risk.total2)

  # marital event
  if(nrow(d.001)==0){
    temp <- data.frame(age=NA,sex=NA,event=NA, count=NA)[F,]
  } else {
    temp <- aggregate(cbind(d.001[0], count=1),
                      list(age=d.001$age, sex=d.001$sex, event=d.001$event), sum)
  }

  male.type <- female.type <- matrix(NA, nrow=nrow(age), ncol=14)
  for(i in 1:14) {
    type <- ifelse(i == 14, i + 3, i)
    male <- merge(age, subset(temp, sex == 1 & event == type, select=c(age, count)), all.x=TRUE)
    male.type[,i] <- male$count
    female <- merge(age, subset(temp, sex == 2 & event == type, select=c(age, count)), all.x=TRUE)
    female.type[,i] <- female$count
  }
  t <- temp[-which(temp$event == 0),]
  if(nrow(t)==0){
    all <- data.frame(age=NA,sex=NA,x=NA)[F,]
  } else {
    all <- aggregate(t[,4], list(age=t$age, sex=t$sex), sum)
  }
  all.male <-  merge(age, subset(all, sex == 1, select=c(age, x)), all.x=TRUE)
  names(all.male) <- c("age", "count")
  all.female <-  merge(age, subset(all, sex == 2, select=c(age, x)), all.x=TRUE)
  names(all.female) <- c("age", "count")

  # month
  male.event <- data.frame(age=age$age, all.male=all.male$count, male.nmNC.married=male.type[,1],
                           male.married.dNC=male.type[,2], male.wNC.married=male.type[,3],
                           male.dNC.married=male.type[,4], male.nmNC.nmC=male.type[,5], male.wNC.wC=male.type[,6],
                           male.dNC.dC=male.type[,7], male.nmC.nmNC=male.type[,8], male.wC.wNC=male.type[,9],
                           male.dC.dNC=male.type[,10], male.nmC.married=male.type[,11], male.wC.married=male.type[,12],
                           male.dC.married=male.type[,13])
  female.event <- data.frame(age=age$age, all.female=all.female$count, female.nmNC.married=female.type[,1],
                             female.married.dNC=female.type[,2], female.wNC.married=female.type[,3],
                             female.dNC.married=female.type[,4], female.nmNC.nmC=female.type[,5], female.wNC.wC=female.type[,6],
                             female.dNC.dC=female.type[,7], female.nmC.nmNC=female.type[,8], female.wC.wNC=female.type[,9],
                             female.dC.dNC=female.type[,10], female.nmC.married=female.type[,11], female.wC.married=female.type[,12],
                             female.dC.married=female.type[,13])

  event.total1 <- data.frame(age = "Total", t(colSums(male.event[2:ncol(male.event)], na.rm = T)))
  event.total2 <- data.frame(age = "Total", t(colSums(female.event[2:ncol(female.event)], na.rm = T)))
  male.event <- rbind(male.event, event.total1)
  female.event <- rbind(female.event, event.total2)
  rm(event.total1)
  rm(event.total2)

  return(list(male.status=male.status, female.status=female.status,
              male.event=male.event, female.event=female.event))
}

pop.count.seven.nosex <- function(data, nlm, nhm){
  age <- data.frame(age=seq(nlm, nhm, 1))
  d.001 <- subset(data, select=c(age, post, event, py))

  # marital status
  if(nrow(d.001)==0){
    temp <- data.frame(age=NA,status=NA, count=NA)[F,]
  } else {
    temp <- aggregate(cbind(d.001[0], count=d.001$py),
                      list(age=d.001$age, status=d.001$post), sum)
  }

  if(nrow(d.001)==0){
    all <- data.frame(age=NA,x=NA)[F,]
  } else {
    all <- aggregate(cbind(d.001[0], count=d.001$py),
                     list(age=d.001$age), sum)
    all <- merge(age, all, all.x = T)
  }

  nm.NC <- merge(age, subset(temp, status == 1, select=c(age, count)), all.x=TRUE)
  married <- merge(age, subset(temp, status == 2, select=c(age, count)), all.x=TRUE)
  widowed.NC <- merge(age, subset(temp, status == 3, select=c(age, count)), all.x=TRUE)
  divorced.NC <- merge(age, subset(temp, status == 4, select=c(age, count)), all.x=TRUE)
  nm.C <- merge(age, subset(temp, status == 5, select=c(age, count)), all.x=TRUE)
  widowed.C <- merge(age, subset(temp, status == 7, select=c(age, count)), all.x=TRUE)
  divorced.C <- merge(age, subset(temp, status == 8, select=c(age, count)), all.x=TRUE)
  names(all) <- c("age", "count")

  # month
  status <- data.frame(age=age$age, all=all$count, never.married.NC=nm.NC$count,
                       married=married$count, widowed.NC=widowed.NC$count,
                       divorced.NC=divorced.NC$count, never.married.C=nm.C$count,
                       widowed.C=widowed.C$count, divorced.C=divorced.C$count)
  status[, 2:ncol(status)] <- round(status[, 2:ncol(status)], 1)

  risk.total <- data.frame(age = "Total", t(colSums(status[2:ncol(status)], na.rm = T)))
  status <- rbind(status, risk.total)

  # marital event
  if(nrow(d.001)==0){
    temp <- data.frame(age=NA,event=NA, count=NA)[F,]
  } else {
    temp <- aggregate(cbind(d.001[0], count=1),
                      list(age=d.001$age, event=d.001$event), sum)
    temp <- merge(age, temp, all.x = T)
  }

  tot.type <- matrix(NA, nrow=nrow(age), ncol=14)
  for(i in 1:14) {
    type <- ifelse(i == 14, i + 3, i)
    male <- merge(age, subset(temp, event == type, select=c(age, count)), all.x=TRUE)
    tot.type[,i] <- male$count
  }
  t <- temp[-which(temp$event == 0),]
  if(nrow(t)==0){
    all <- data.frame(age=NA,x=NA)[F,]
  } else {
    all <- aggregate(t[,3], list(age=t$age), sum)
  }
  all <-  merge(age, subset(all, select=c(age, x)), all.x=TRUE)
  names(all) <- c("age", "count")

  # month
  event <- data.frame(age=age$age, all=all$count, nmNC.married=tot.type[,1],
                      married.dNC=tot.type[,2], wNC.married=tot.type[,3],
                      dNC.married=tot.type[,4], nmNC.nmC=tot.type[,5], wNC.wC=tot.type[,6],
                      dNC.dC=tot.type[,7], nmC.nmNC=tot.type[,8], wC.wNC=tot.type[,9],
                      dC.dNC=tot.type[,10], nmC.married=tot.type[,11], wC.married=tot.type[,12],
                      dC.married=tot.type[,13])

  event.total <- data.frame(age = "Total", t(colSums(event[2:ncol(event)], na.rm = T)))
  event <- rbind(event, event.total)

  return(list(status=status, event=event))
}

draw.mar7.nRate <- function(result.list, code, nlm, nhm, title, period, plot.name, byvar, cal_oe, cal_freq){

  region.code <-  code$`Region Code`
  region.code <- region.code[!is.na(region.code)]

  race.code <- code$`Race Code`
  race.code <- race.code[!is.na(race.code)]

  event.name <- c("Never married & not-cohabiting to married",
                  "Divorced",
                  "Remarriage of widowed & not-cohabiting",
                  "Remarriage of divorced & not-cohabiting",
                  "Never married & cohabiting to married",
                  "Remarriage of widowed & cohabiting",
                  "Remarriage of divorced & cohabiting",
                  "Never married to cohabiting",
                  "Widowed to cohabiting",
                  "Divorced to cohabiting",
                  "Cohabiting to never married",
                  "Cohabiting to widowed",
                  "Cohabiting to divorced")

  if (byvar=="sex"){
    wb <- createWorkbook()
    s <- 1

    if(cal_oe == TRUE){
      addWorksheet(wb, "oe")
      writeData(wb, sheet=s, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
      writeData(wb, sheet=s, paste0("Period: ", period, sep=""), startRow=2, startCol=1)

      s <- s+1
      row.index <- 3

      for (k in 1:13) {
        p1 <- rates.plot(result.list[[2*k-1]][which(result.list[[2*k-1]]$sex==1), c(1, 3:4)], nlm, nhm, "o/e rate", paste0("Figure ", k+13, ".1. ", event.name[k], " o/e rates, males", plot.name, sep=""))
        print(p1)
        insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
        p2 <- rates.plot(result.list[[2*k-1]][which(result.list[[2*k-1]]$sex==2), c(1, 3:4)], nlm, nhm, "o/e rate", paste0("Figure ", k+13, ".2. ", event.name[k], " o/e rates, females", plot.name, sep=""))
        print(p2)
        insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

        row.index <- row.index + 22
      }

    }

    if(cal_freq == TRUE){
      addWorksheet(wb, "freq")
      writeData(wb, sheet=s, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
      writeData(wb, sheet=s, paste0("Period: ", period, sep=""), startRow=2, startCol=1)

      row.index <- 3

      for (k in 1:13) {
        p1 <- rates.plot(result.list[[2*k]][which(result.list[[2*k]]$sex==1), c(1, 3:4)], nlm, nhm, "frequency", paste0("Figure ", k, ".1. ", event.name[k], " frequencies, males", plot.name, sep=""))
        print(p1)
        insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
        p2 <- rates.plot(result.list[[2*k]][which(result.list[[2*k]]$sex==2), c(1, 3:4)], nlm, nhm, "frequency", paste0("Figure ", k, ".2. ", event.name[k], " frequencies, females", plot.name, sep=""))
        print(p2)
        insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

        row.index <- row.index + 22
      }

    }

    output.dir <- getwd()
    plot.name <- gsub(", ", "-", plot.name)
    saveWorkbook(wb, paste0(output.dir, "/", title, " Mar7 plot", plot.name, ".xlsx", sep=""), overwrite=TRUE)

  } else if (byvar=="reg.ru"){

    for(i in region.code){
      r.name <- code[which(code$`Region Code` == i), 4]
      r.code <- paste0(", ", r.name, sep="")
      plot.name <- r.code

      wb <- createWorkbook()
      s <- 1

      if(cal_oe == TRUE){
        addWorksheet(wb, "oe-rural")
        addWorksheet(wb, "oe-urban")
        writeData(wb, sheet=s, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
        writeData(wb, sheet=s, paste0("Period: ", period, sep=""), startRow=2, startCol=1)
        writeData(wb, sheet=s+1, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
        writeData(wb, sheet=s+1, paste0("Period: ", period, sep=""), startRow=2, startCol=1)

        s <- s+2
        row.index <- 3

        for (k in 1:13) {

          p1 <- rates.plot(result.list[[2*k-1]][which(result.list[[2*k-1]]$region==i&result.list[[2*k-1]]$ru==1&result.list[[2*k-1]]$sex==1), c(1, 5:6)], nlm, nhm, "o/e rate", paste0("Figure ", k+13, ".1. ", event.name[k], " o/e rates, males", plot.name, "-rural", sep=""))
          print(p1)
          insertPlot(wb, "oe-rural", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
          p2 <- rates.plot(result.list[[2*k-1]][which(result.list[[2*k-1]]$region==i&result.list[[2*k-1]]$ru==1&result.list[[2*k-1]]$sex==2), c(1, 5:6)], nlm, nhm, "o/e rate", paste0("Figure ", k+13, ".2. ", event.name[k], " o/e rates, females", plot.name, "-rural", sep=""))
          print(p2)
          insertPlot(wb, "oe-rural", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

          p1 <- rates.plot(result.list[[2*k-1]][which(result.list[[2*k-1]]$region==i&result.list[[2*k-1]]$ru==2&result.list[[2*k-1]]$sex==1), c(1, 5:6)], nlm, nhm, "o/e rate", paste0("Figure ", k+13, ".1. ", event.name[k], " o/e rates, males", plot.name, "-urban", sep=""))
          print(p1)
          insertPlot(wb, "oe-urban", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
          p2 <- rates.plot(result.list[[2*k-1]][which(result.list[[2*k-1]]$region==i&result.list[[2*k-1]]$ru==2&result.list[[2*k-1]]$sex==2), c(1, 5:6)], nlm, nhm, "o/e rate", paste0("Figure ", k+13, ".2. ", event.name[k], " o/e rates, females", plot.name, "-urban", sep=""))
          print(p2)
          insertPlot(wb, "oe-urban", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

          row.index <- row.index + 22
        }

      }

      if(cal_freq == TRUE){
        addWorksheet(wb, "freq-rural")
        addWorksheet(wb, "freq-urban")
        writeData(wb, sheet=s, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
        writeData(wb, sheet=s, paste0("Period: ", period, sep=""), startRow=2, startCol=1)
        writeData(wb, sheet=s+1, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
        writeData(wb, sheet=s+1, paste0("Period: ", period, sep=""), startRow=2, startCol=1)

        row.index <- 3

        for (k in 1:13) {

          p1 <- rates.plot(result.list[[2*k]][which(result.list[[2*k]]$region==i&result.list[[2*k]]$ru==1&result.list[[2*k]]$sex==1), c(1, 5:6)], nlm, nhm, "frequency", paste0("Figure ", k, ".1. ", event.name[k], " frequencies, males", plot.name, "-rural", sep=""))
          print(p1)
          insertPlot(wb, "freq-rural", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
          p2 <- rates.plot(result.list[[2*k]][which(result.list[[2*k]]$region==i&result.list[[2*k]]$ru==1&result.list[[2*k]]$sex==2), c(1, 5:6)], nlm, nhm, "frequency", paste0("Figure ", k, ".2. ", event.name[k], " frequencies, females", plot.name, "-rural", sep=""))
          print(p2)
          insertPlot(wb, "freq-rural", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

          p1 <- rates.plot(result.list[[2*k]][which(result.list[[2*k]]$region==i&result.list[[2*k]]$ru==2&result.list[[2*k]]$sex==1), c(1, 5:6)], nlm, nhm, "frequency", paste0("Figure ", k, ".1. ", event.name[k], " frequencies, males", plot.name, "-urban", sep=""))
          print(p1)
          insertPlot(wb, "freq-urban", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
          p2 <- rates.plot(result.list[[2*k]][which(result.list[[2*k]]$region==i&result.list[[2*k]]$ru==2&result.list[[2*k]]$sex==2), c(1, 5:6)], nlm, nhm, "frequency", paste0("Figure ", k, ".2. ", event.name[k], " frequencies, females", plot.name, "-urban", sep=""))
          print(p2)
          insertPlot(wb, "freq-urban", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

          row.index <- row.index + 22
        }

      }

      output.dir <- getwd()
      plot.name <- gsub(", ", "-", plot.name)
      saveWorkbook(wb, paste0(output.dir, "/", title, " Mar7 plot", plot.name, ".xlsx", sep=""), overwrite=TRUE)
    }

  } else if (byvar=="reg.ru.nosex"){

    for(i in region.code){
      r.name <- code[which(code$`Region Code` == i), 4]
      r.code <- paste0(", ", r.name, sep="")
      plot.name <- r.code

      wb <- createWorkbook()
      s <- 1

      if(cal_oe == TRUE){
        addWorksheet(wb, "oe")
        writeData(wb, sheet=s, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
        writeData(wb, sheet=s, paste0("Period: ", period, sep=""), startRow=2, startCol=1)

        s <- s+1
        row.index <- 3

        for (k in 1:13) {
          p1 <- rates.plot(result.list[[2*k-1]][which(result.list[[2*k-1]]$ru==1&result.list[[2*k-1]]$region==i), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure ", k+13, ".1. ", event.name[k], " o/e rates", plot.name, "-rural", sep=""))
          print(p1)
          insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
          p2 <- rates.plot(result.list[[2*k-1]][which(result.list[[2*k-1]]$ru==2&result.list[[2*k-1]]$region==i), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure ", k+13, ".2. ", event.name[k], " o/e rates", plot.name, "-urban", sep=""))
          print(p2)
          insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

          row.index <- row.index + 22
        }

      }

      if(cal_freq == TRUE){
        addWorksheet(wb, "freq")
        writeData(wb, sheet=s, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
        writeData(wb, sheet=s, paste0("Period: ", period, sep=""), startRow=2, startCol=1)

        row.index <- 3

        for (k in 1:13) {
          p1 <- rates.plot(result.list[[2*k]][which(result.list[[2*k]]$ru==1&result.list[[2*k]]$region==i), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure ", k, ".1. ", event.name[k], " frequencies", plot.name, "-rural", sep=""))
          print(p1)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
          p2 <- rates.plot(result.list[[2*k]][which(result.list[[2*k]]$ru==2&result.list[[2*k]]$region==i), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure ", k, ".2. ", event.name[k], " frequencies", plot.name, "-urban", sep=""))
          print(p2)
          insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

          row.index <- row.index + 22
        }

      }

      output.dir <- getwd()
      plot.name <- gsub(", ", "-", plot.name)
      saveWorkbook(wb, paste0(output.dir, "/", title, " Mar7 plot", plot.name, ".xlsx", sep=""), overwrite=TRUE)
    }

  } else {
    wb <- createWorkbook()
    s <- 1

    if(cal_oe == TRUE){
      addWorksheet(wb, "oe")
      writeData(wb, sheet=s, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
      writeData(wb, sheet=s, paste0("Period: ", period, sep=""), startRow=2, startCol=1)
      s <- s+1
    }

    if(cal_freq == TRUE){
      addWorksheet(wb, "freq")
      writeData(wb, sheet=s, paste0("Data Source: ", title, sep=""), startRow=1, startCol=1)
      writeData(wb, sheet=s, paste0("Period: ", period, sep=""), startRow=2, startCol=1)
    }

    if (byvar=="ru"){

      ru.code <- c(1, 2)

      for(i in ru.code){
        r.name <- ifelse(i==1, "rural", "urban")
        r.code <- paste0(", ", r.name, sep="")
        plot.name <- r.code

        if(cal_oe == TRUE){

          row.index <- 3

          for (k in 1:13) {
            p1 <- rates.plot(result.list[[2*k-1]][which(result.list[[2*k-1]]$sex==1&result.list[[2*k-1]]$ru==i), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure ", k+13, ".1. ", event.name[k], " o/e rates, males", plot.name, sep=""))
            print(p1)
            insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
            p2 <- rates.plot(result.list[[2*k-1]][which(result.list[[2*k-1]]$sex==2&result.list[[2*k-1]]$ru==i), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure ", k+13, ".2. ", event.name[k], " o/e rates, females", plot.name, sep=""))
            print(p2)
            insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

            row.index <- row.index + 22
          }

        }

        if(cal_freq == TRUE){

          row.index <- 3

          for (k in 1:13) {
            p1 <- rates.plot(result.list[[2*k]][which(result.list[[2*k]]$sex==1&result.list[[2*k]]$ru==i), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure ", k, ".1. ", event.name[k], " frequencies, males", plot.name, sep=""))
            print(p1)
            insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
            p2 <- rates.plot(result.list[[2*k]][which(result.list[[2*k]]$sex==2&result.list[[2*k]]$ru==i), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure ", k, ".2. ", event.name[k], " frequencies, females", plot.name, sep=""))
            print(p2)
            insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

            row.index <- row.index + 22
          }

        }

        output.dir <- getwd()
        plot.name <- gsub(", ", "-", plot.name)
        saveWorkbook(wb, paste0(output.dir, "/", title, " Mar7 plot", plot.name, ".xlsx", sep=""), overwrite=TRUE)

      }
    }

    if (byvar=="region"){

      for(i in region.code){
        r.name <- code[which(code$`Region Code` == i), 4]
        r.code <- paste0(", ", r.name, sep="")
        plot.name <- r.code

        if(cal_oe == TRUE){

          row.index <- 3

          for (k in 1:13) {
            p1 <- rates.plot(result.list[[2*k-1]][which(result.list[[2*k-1]]$sex==1&result.list[[2*k-1]]$region==i), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure ", k+13, ".1. ", event.name[k], " o/e rates, males", plot.name, sep=""))
            print(p1)
            insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
            p2 <- rates.plot(result.list[[2*k-1]][which(result.list[[2*k-1]]$sex==2&result.list[[2*k-1]]$region==i), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure ", k+13, ".2. ", event.name[k], " o/e rates, females", plot.name, sep=""))
            print(p2)
            insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

            row.index <- row.index + 22
          }

        }

        if(cal_freq == TRUE){

          row.index <- 3

          for (k in 1:13) {
            p1 <- rates.plot(result.list[[2*k]][which(result.list[[2*k]]$sex==1&result.list[[2*k]]$region==i), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure ", k, ".1. ", event.name[k], " frequencies, males", plot.name, sep=""))
            print(p1)
            insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
            p2 <- rates.plot(result.list[[2*k]][which(result.list[[2*k]]$sex==2&result.list[[2*k]]$region==i), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure ", k, ".2. ", event.name[k], " frequencies, females", plot.name, sep=""))
            print(p2)
            insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

            row.index <- row.index + 22
          }

        }

        output.dir <- getwd()
        plot.name <- gsub(", ", "-", plot.name)
        saveWorkbook(wb, paste0(output.dir, "/", title, " Mar7 plot", plot.name, ".xlsx", sep=""), overwrite=TRUE)

      }
    }

    if (byvar=="region.nosex"){

      for(i in region.code){
        r.name <- code[which(code$`Region Code` == i), 4]
        r.code <- paste0(", ", r.name, sep="")
        plot.name <- r.code

        if(cal_oe == TRUE){

          row.index <- 3

          for (k in 1:13) {

            p1 <- rates.plot(result.list[[2*k-1]][which(result.list[[2*k-1]]$region==i), c(1, 3:4)], nlm, nhm, "o/e rate", paste0("Figure ", k+13, ".1. ", event.name[k], " o/e rates", plot.name, sep=""))
            print(p1)
            insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")

            row.index <- row.index + 22
          }

        }

        if(cal_freq == TRUE){

          row.index <- 3

          for (k in 1:13) {

            p1 <- rates.plot(result.list[[2*k]][which(result.list[[2*k]]$region==i), c(1, 3:4)], nlm, nhm, "frequency", paste0("Figure ", k, ".1. ", event.name[k], " frequencies", plot.name, sep=""))
            print(p1)
            insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")

            row.index <- row.index + 22
          }

        }

        output.dir <- getwd()
        plot.name <- gsub(", ", "-", plot.name)
        saveWorkbook(wb, paste0(output.dir, "/", title, " Mar7 plot", plot.name, ".xlsx", sep=""), overwrite=TRUE)

      }
    }

    if (byvar=="race"){

      for(i in race.code){
        r.name <- code[which(code$`Race Code` == i), 2]
        r.code <- paste0(", ", r.name, sep="")
        plot.name <- r.code

        if(cal_oe == TRUE){

          row.index <- 3

          for (k in 1:13) {
            p1 <- rates.plot(result.list[[2*k-1]][which(result.list[[2*k-1]]$sex==1&result.list[[2*k-1]]$race==i), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure ", k+13, ".1. ", event.name[k], " o/e rates, males", plot.name, sep=""))
            print(p1)
            insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
            p2 <- rates.plot(result.list[[2*k-1]][which(result.list[[2*k-1]]$sex==2&result.list[[2*k-1]]$race==i), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure ", k+13, ".2. ", event.name[k], " o/e rates, females", plot.name, sep=""))
            print(p2)
            insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

            row.index <- row.index + 22
          }

        }

        if(cal_freq == TRUE){

          row.index <- 3

          for (k in 1:13) {
            p1 <- rates.plot(result.list[[2*k]][which(result.list[[2*k]]$sex==1&result.list[[2*k]]$race==i), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure ", k, ".1. ", event.name[k], " frequencies, males", plot.name, sep=""))
            print(p1)
            insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
            p2 <- rates.plot(result.list[[2*k]][which(result.list[[2*k]]$sex==2&result.list[[2*k]]$race==i), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure ", k, ".2. ", event.name[k], " frequencies, females", plot.name, sep=""))
            print(p2)
            insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

            row.index <- row.index + 22
          }

        }

        output.dir <- getwd()
        plot.name <- gsub(", ", "-", plot.name)
        saveWorkbook(wb, paste0(output.dir, "/", title, " Mar7 plot", plot.name, ".xlsx", sep=""), overwrite=TRUE)
      }
    }

    if (byvar=="edu"){

      edu.code <- c(1:5)
      educode <- data.frame(code=c(1:5), name=c("No education", "Primary school",
                                                "Middle school", "High school", "College or higher"))

      for(i in edu.code){
        e.name <- educode[which(educode$code==i), "name"]
        e.code <- paste0(", ", e.name, sep="")
        plot.name <- e.code

        if(cal_oe == TRUE){

          row.index <- 3

          for (k in 1:13) {
            p1 <- rates.plot(result.list[[2*k-1]][which(result.list[[2*k-1]]$sex==1&result.list[[2*k-1]]$edu==i), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure ", k+13, ".1. ", event.name[k], " o/e rates, males", plot.name, sep=""))
            print(p1)
            insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
            p2 <- rates.plot(result.list[[2*k-1]][which(result.list[[2*k-1]]$sex==2&result.list[[2*k-1]]$edu==i), c(1, 4:5)], nlm, nhm, "o/e rate", paste0("Figure ", k+13, ".2. ", event.name[k], " o/e rates, females", plot.name, sep=""))
            print(p2)
            insertPlot(wb, "oe", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

            row.index <- row.index + 22
          }

        }

        if(cal_freq == TRUE){

          row.index <- 3

          for (k in 1:13) {
            p1 <- rates.plot(result.list[[2*k]][which(result.list[[2*k]]$sex==1&result.list[[2*k]]$edu==i), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure ", k, ".1. ", event.name[k], " frequencies, males", plot.name, sep=""))
            print(p1)
            insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=1, width=12.55, height=10.4, units="cm")
            p2 <- rates.plot(result.list[[2*k]][which(result.list[[2*k]]$sex==2&result.list[[2*k]]$edu==i), c(1, 4:5)], nlm, nhm, "frequency", paste0("Figure ", k, ".2. ", event.name[k], " frequencies, females", plot.name, sep=""))
            print(p2)
            insertPlot(wb, "freq", fileType = "png", startRow=row.index, startCol=7, width=12.55, height=10.4, units="cm")

            row.index <- row.index + 22
          }

        }

        output.dir <- getwd()
        plot.name <- gsub(", ", "-", plot.name)
        saveWorkbook(wb, paste0(output.dir, "/", title, " Mar7 plot", plot.name, ".xlsx", sep=""), overwrite=TRUE)
      }
    }
  }

}

write.rates.seven.sex <- function(male.oe, female.oe, male.freq, female.freq, param, name, method){
  forDatstl <- createStyle(halign="center", border="TopBottomLeftRight", wrapText=TRUE)
  forDatstl2 <- createStyle(halign="right", border="TopBottomLeftRight", numFmt = "#,##0.0000")
  wb <- createWorkbook()
  title <- as.character(param$title)

  nrate <- as.numeric(param$nRate)
  cal_oe <- (nrate==1 | nrate==2)
  cal_freq <- (nrate==1 | nrate==3)
  k<-1

  if (cal_oe==T){

    addWorksheet(wb, "oe.rates")

    writeData(wb, sheet=k, paste0("Table M4.1. Age-gender-specific marital/union status transition o/e rates, males, ", title, name, sep=""),
              startRow=4, startCol=2)
    mergeCells(wb, sheet=k, cols=2:15, rows=4)
    colnames(male.oe) <-
      c("Age", "Never married & not cohabiting to married", "Never married & cohabiting to married", "Divorced",
        "Remarriage of widowed & not cohabiting", "Remarraige of widowed & cohabiting", "Remarriage of divorced & not cohabiting",
        "Remarriage of divorced & cohabiting", "Never married to cohabiting", "Widowed to cohabiting", "Divorced to cohabiting",
        "Cohabiting to never married", "Cohabiting to widowed", "Cohabiting to divorced")
    writeData(wb, sheet=k, male.oe, startRow=5, startCol=2, borders="all")
    setColWidths(wb, sheet=k, cols=2:15, widths=15)
    setColWidths(wb, sheet=k, cols=2, widths=6)
    addStyle(wb, sheet=k, style=forDatstl, rows=5, cols=2:15)
    addStyle(wb, sheet=k, style=forDatstl2, rows=6:(5+nrow(male.oe)), cols=3:15, gridExpand = T)

    writeData(wb, sheet=k, paste0("Table M4.2. Age-gender-specific marital/union status transition o/e rates, females, ", title, name, sep=""),
              startRow=nrow(male.oe)+8, startCol=2)
    mergeCells(wb, sheet=k, cols=2:15, rows=nrow(male.oe)+8)
    colnames(female.oe) <-
      c("Age", "Never married & not cohabiting to married", "Never married & cohabiting to married", "Divorced",
        "Remarriage of widowed & not cohabiting", "Remarraige of widowed & cohabiting", "Remarriage of divorced & not cohabiting",
        "Remarriage of divorced & cohabiting", "Never married to cohabiting", "Widowed to cohabiting", "Divorced to cohabiting",
        "Cohabiting to never married", "Cohabiting to widowed", "Cohabiting to divorced")
    writeData(wb, sheet=k, female.oe, startRow=nrow(male.oe)+9, startCol=2, borders="all")
    setColWidths(wb, sheet=k, cols=2:15, widths=15)
    setColWidths(wb, sheet=k, cols=2, widths=6)
    addStyle(wb, sheet=k, style=forDatstl, rows=nrow(male.oe)+9, cols=2:15)
    addStyle(wb, sheet=k, style=forDatstl2,
             rows=(nrow(male.oe)+10):(nrow(male.oe)+9+nrow(female.oe)),
             cols=3:15, gridExpand = TRUE)

    k<-k+1

  }


  if (cal_freq==T){

    addWorksheet(wb, "frequency")

    writeData(wb, sheet=k, paste0("Table M3.1. Age-gender-specific marital/union status transition frequencies, males, ", title, name, sep=""),
              startRow=4, startCol=2)
    mergeCells(wb, sheet=k, cols=2:15, rows=4)
    colnames(male.freq) <-
      c("Age", "Never married & not cohabiting to married", "Never married & cohabiting to married", "Divorced",
        "Remarriage of widowed & not cohabiting", "Remarraige of widowed & cohabiting", "Remarriage of divorced & not cohabiting",
        "Remarriage of divorced & cohabiting", "Never married to cohabiting", "Widowed to cohabiting", "Divorced to cohabiting",
        "Cohabiting to never married", "Cohabiting to widowed", "Cohabiting to divorced")
    writeData(wb, sheet=k, male.freq, startRow=5, startCol=2, borders="all")
    setColWidths(wb, sheet=k, cols=2:15, widths=15)
    setColWidths(wb, sheet=k, cols=2, widths=6)
    addStyle(wb, sheet=k, style=forDatstl, rows=5, cols=2:15)
    addStyle(wb, sheet=k, style=forDatstl2, rows=6:(5+nrow(male.freq)), cols=3:15, gridExpand = T)

    writeData(wb, sheet=k, paste0("Table M3.2. Age-gender-specific marital/union status transition frequencies, females, ", title, name, sep=""),
              startRow=nrow(male.freq)+9, startCol=2)
    mergeCells(wb, sheet=k, cols=2:15, rows=nrow(male.freq)+9)
    colnames(female.freq) <-
      c("Age", "Never married & not cohabiting to married", "Never married & cohabiting to married", "Divorced",
        "Remarriage of widowed & not cohabiting", "Remarraige of widowed & cohabiting", "Remarriage of divorced & not cohabiting",
        "Remarriage of divorced & cohabiting", "Never married to cohabiting", "Widowed to cohabiting", "Divorced to cohabiting",
        "Cohabiting to never married", "Cohabiting to widowed", "Cohabiting to divorced")
    writeData(wb, sheet=k, female.freq, startRow=nrow(male.freq)+10, startCol=2, borders="all")
    setColWidths(wb, sheet=k, cols=2:15, widths=15)
    setColWidths(wb, sheet=k, cols=2, widths=6)
    addStyle(wb, sheet=k, style=forDatstl, rows=nrow(male.freq)+10, cols=2:15)
    addStyle(wb, sheet=k, style=forDatstl2,
             rows=(nrow(male.freq)+11):(nrow(male.freq)+10+nrow(female.freq)),
             cols=3:15, gridExpand = TRUE)

  }

  output.dir <- getwd()
  name <- gsub(", ", "-", name)
  saveWorkbook(wb, paste0(output.dir, "/", title, " Mar7 ", method, " rates", name, ".xlsx", sep=""), overwrite=T)
}

write.rates.seven.nosex <- function(male.oe, male.freq, param, name, method){
  forDatstl <- createStyle(halign="center", border="TopBottomLeftRight", wrapText=TRUE)
  forDatstl2 <- createStyle(halign="right", border="TopBottomLeftRight", numFmt = "#,##0.0000")
  wb <- createWorkbook()
  title <- as.character(param$title)

  nrate <- as.numeric(param$nRate)
  cal_oe <- (nrate==1 | nrate==2)
  cal_freq <- (nrate==1 | nrate==3)
  k<-1

  if (cal_oe==T){

    addWorksheet(wb, "oe.rates")

    writeData(wb, sheet=k, paste0("Table M4.1. Age-specific marital/union status transition o/e rates, ", title, name, sep=""),
              startRow=4, startCol=2)
    mergeCells(wb, sheet=k, cols=2:15, rows=4)
    colnames(male.oe) <-
      c("Age", "Never married & not cohabiting to married", "Never married & cohabiting to married", "Divorced",
        "Remarriage of widowed & not cohabiting", "Remarraige of widowed & cohabiting", "Remarriage of divorced & not cohabiting",
        "Remarriage of divorced & cohabiting", "Never married to cohabiting", "Widowed to cohabiting", "Divorced to cohabiting",
        "Cohabiting to never married", "Cohabiting to widowed", "Cohabiting to divorced")
    writeData(wb, sheet=k, male.oe, startRow=5, startCol=2, borders="all")
    setColWidths(wb, sheet=k, cols=2:15, widths=15)
    setColWidths(wb, sheet=k, cols=2, widths=6)
    addStyle(wb, sheet=k, style=forDatstl, rows=5, cols=2:15)
    addStyle(wb, sheet=k, style=forDatstl2, rows=6:(5+nrow(male.oe)), cols=3:15, gridExpand = T)

    k<-k+1

  }


  if (cal_freq==T){

    addWorksheet(wb, "frequency")

    writeData(wb, sheet=k, paste0("Table M3.1. Age-specific marital/union status transition frequencies, ", title, name, sep=""),
              startRow=4, startCol=2)
    mergeCells(wb, sheet=k, cols=2:15, rows=4)
    colnames(male.freq) <-
      c("Age", "Never married & not cohabiting to married", "Never married & cohabiting to married", "Divorced",
        "Remarriage of widowed & not cohabiting", "Remarraige of widowed & cohabiting", "Remarriage of divorced & not cohabiting",
        "Remarriage of divorced & cohabiting", "Never married to cohabiting", "Widowed to cohabiting", "Divorced to cohabiting",
        "Cohabiting to never married", "Cohabiting to widowed", "Cohabiting to divorced")
    writeData(wb, sheet=k, male.freq, startRow=5, startCol=2, borders="all")
    setColWidths(wb, sheet=k, cols=2:15, widths=15)
    setColWidths(wb, sheet=k, cols=2, widths=6)
    addStyle(wb, sheet=k, style=forDatstl, rows=5, cols=2:15)
    addStyle(wb, sheet=k, style=forDatstl2, rows=6:(5+nrow(male.freq)), cols=3:15, gridExpand = T)

  }

  output.dir <- getwd()
  name <- gsub(", ", "-", name)
  saveWorkbook(wb, paste0(output.dir, "/", title, " Mar7 ", method, " rates", name, ".xlsx", sep=""), overwrite=T)
}

write.pop.seven.sex <- function(pop, param, name){
  forDatstl <- createStyle(halign="center", border="TopBottomLeftRight", wrapText=TRUE)
  forDatstl2 <- createStyle(halign="right", border="TopBottomLeftRight", numFmt = "#,##0.0")
  wb <- createWorkbook()
  addWorksheet(wb, "status")
  addWorksheet(wb, "event")
  title <- as.character(param$title)

  # male status
  writeData(wb, sheet=1, paste0("Table M1.1. Male person-years by age and marital/union status, ", title, name, sep=""),
            startRow=4, startCol=2)
  mergeCells(wb, sheet=1, cols=2:9, rows=4)
  colnames(pop$male.status) <-
    c("Age", "All", "Never married & not cohabiting", "Married", "Widowed & not cohabiting", "Divorced & not cohabiting",
      "Never married & cohabiting", "Widowed & cohabiting", "Divorced & cohabiting")
  writeData(wb, sheet=1, pop$male.status, startRow=5, startCol=2, borders="all")
  setColWidths(wb, sheet=1, cols=2:10, widths=20)
  setColWidths(wb, sheet=1, cols=2, widths=6)
  addStyle(wb, sheet=1, style=forDatstl, rows=5, cols=2:10)
  addStyle(wb, sheet=1, style=forDatstl2, rows=6:(5+nrow(pop$male.status)), cols=3:10, gridExpand = TRUE)

  # female status
  writeData(wb, sheet=1, paste0("Table M1.2. Female person-years by age and marital/union status, ", title, name, sep=""),
            startRow=nrow(pop$male.status)+8, startCol=2)
  mergeCells(wb, sheet=1, cols=2:9, rows=nrow(pop$male.status)+8)
  colnames(pop$female.status) <-
    c("Age", "All", "Never married & not cohabiting", "Married", "Widowed & not cohabiting", "Divorced & not cohabiting",
      "Never married & cohabiting", "Widowed & cohabiting", "Divorced & cohabiting")
  writeData(wb, sheet=1, pop$female.status, startRow=nrow(pop$male.status)+9, startCol=2, borders="all")
  setColWidths(wb, sheet=1, cols=2:10, widths=20)
  setColWidths(wb, sheet=1, cols=2, widths=6)
  addStyle(wb, sheet=1, style=forDatstl, rows=nrow(pop$male.status)+9, cols=2:10)
  addStyle(wb, sheet=1, style=forDatstl2,
           rows=(nrow(pop$male.status)+10):(nrow(pop$male.status)+9+nrow(pop$female.status)),
           cols=3:10, gridExpand = TRUE)

  # male event
  writeData(wb, sheet=2, paste0("Table M2.1. Number of event by age and event type, males, ", title, name, sep=""),
            startRow=4, startCol=2)
  mergeCells(wb, sheet=2, cols=2:9, rows=4)
  colnames(pop$male.event) <-
    c("Age", "All", "Never married & not cohabiting to married", "Divorced", "Remarriage of widowed & not cohabiting",
      "Remarriage of divorced & not cohabiting", "Never married to cohabiting", "Widowed to cohabiting",
      "Divorced to cohabiting", "Cohabiting to never married", "Cohabiting to widowed", "Cohabiting to divorced",
      "Never married & cohabiting to married", "Remarriage of widowed & cohabiting", "Remarriage of divorced & cohabiting")
  writeData(wb, sheet=2, pop$male.event, startRow=5, startCol=2, borders="all")
  setColWidths(wb, sheet=2, cols=2:16, widths=15)
  setColWidths(wb, sheet=2, cols=2:3, widths=6)
  addStyle(wb, sheet=2, style=forDatstl, rows=5, cols=2:16)

  # female event
  writeData(wb, sheet=2, paste0("Table M2.2. Number of event by age and event type, females, ", title, name, sep=""),
            startRow=nrow(pop$male.event)+8, startCol=2)
  mergeCells(wb, sheet=2, cols=2:9, rows=nrow(pop$male.event)+8)
  colnames(pop$female.event) <-
    c("Age", "All", "Never married & not cohabiting to married", "Divorced", "Remarriage of widowed & not cohabiting",
      "Remarriage of divorced & not cohabiting", "Never married to cohabiting", "Widowed to cohabiting",
      "Divorced to cohabiting", "Cohabiting to never married", "Cohabiting to widowed", "Cohabiting to divorced",
      "Never married & cohabiting to married", "Remarriage of widowed & cohabiting", "Remarriage of divorced & cohabiting")
  writeData(wb, sheet=2, pop$female.event, startRow=nrow(pop$male.event)+9, startCol=2, borders="all")
  setColWidths(wb, sheet=2, cols=2:16, widths=15)
  setColWidths(wb, sheet=2, cols=2:3, widths=6)
  addStyle(wb, sheet=2, style=forDatstl, rows=nrow(pop$male.event)+9, cols=2:16)

  output.dir <- getwd()
  name <- gsub(", ", "-", name)
  saveWorkbook(wb, paste0(output.dir, "/", title, " Mar7 popTable", name, ".xlsx", sep=""), overwrite=TRUE)
}

write.pop.seven.nosex <- function(pop, param, name){

  forDatstl <- createStyle(halign="center", border="TopBottomLeftRight", wrapText=TRUE)
  forDatstl2 <- createStyle(halign="right", border="TopBottomLeftRight", numFmt = "#,##0.0")
  wb <- createWorkbook()
  addWorksheet(wb, "status")
  addWorksheet(wb, "event")
  title <- as.character(param$title)

  # status
  writeData(wb, sheet=1, paste0("Table M1.1. Person-years by age and marital/union status, ", title, name, sep=""),
            startRow=4, startCol=2)
  mergeCells(wb, sheet=1, cols=2:9, rows=4)
  colnames(pop$status) <-
    c("Age", "All", "Never married & not cohabiting", "Married", "Widowed & not cohabiting", "Divorced & not cohabiting",
      "Never married & cohabiting", "Widowed & cohabiting", "Divorced & cohabiting")
  writeData(wb, sheet=1, pop$status, startRow=5, startCol=2, borders="all")
  setColWidths(wb, sheet=1, cols=2:10, widths=20)
  setColWidths(wb, sheet=1, cols=2, widths=6)
  addStyle(wb, sheet=1, style=forDatstl, rows=5, cols=2:10)
  addStyle(wb, sheet=1, style=forDatstl2, rows=6:(5+nrow(pop$status)), cols=3:10, gridExpand = TRUE)

  # event
  writeData(wb, sheet=2, paste0("Table M2.1. Number of event by age and event type, ", title, name, sep=""),
            startRow=4, startCol=2)
  mergeCells(wb, sheet=2, cols=2:9, rows=4)
  colnames(pop$event) <-
    c("Age", "All", "Never married & not cohabiting to married", "Divorced", "Remarriage of widowed & not cohabiting",
      "Remarriage of divorced & not cohabiting", "Never married to cohabiting", "Widowed to cohabiting",
      "Divorced to cohabiting", "Cohabiting to never married", "Cohabiting to widowed", "Cohabiting to divorced",
      "Never married & cohabiting to married", "Remarriage of widowed & cohabiting", "Remarriage of divorced & cohabiting")
  writeData(wb, sheet=2, pop$event, startRow=5, startCol=2, borders="all")
  setColWidths(wb, sheet=2, cols=2:16, widths=15)
  setColWidths(wb, sheet=2, cols=2:3, widths=6)
  addStyle(wb, sheet=2, style=forDatstl, rows=5, cols=2:16)

  output.dir <- getwd()
  name <- gsub(", ", "-", name)
  saveWorkbook(wb, paste0(output.dir, "/", title, " Mar7 popTable", name, ".xlsx", sep=""), overwrite=TRUE)
}

write.total.seven.subset <- function(total.rates, mean.age, param, name){
  forDatstl <- createStyle(halign="center", border="TopBottomLeftRight", wrapText = TRUE)
  forDatstl2 <- createStyle(halign="left", border="TopBottomLeftRight", wrapText = TRUE)
  forDatstl3 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0000")
  forDatstl4 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0")
  forDatstl5 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0%")
  wb <- createWorkbook()
  addWorksheet(wb, "total rates")
  title <- as.character(param$title)

  if (nrow(total.rates)==3){
    writeData(wb, sheet=1, paste0("Table M5. Total rates and mean ages of marital status transitions, ", title, sep=""),
              startRow=4, startCol=2)
    mergeCells(wb, sheet=1, cols=2:12, rows=4)
    writeData(wb, sheet=1, "First Marriage", startRow=5, startCol=3, borders="all")
    mergeCells(wb, sheet=1, cols=3:4, rows=5)
    writeData(wb, sheet=1, "Remarriage", startRow=5, startCol=5, borders="all")
    mergeCells(wb, sheet=1, cols=5:6, rows=5)
    writeData(wb, sheet=1, "Divorce", startRow=5, startCol=7, borders="all")
    mergeCells(wb, sheet=1, cols=7:8, rows=5)
    writeData(wb, sheet=1, "Cohabitation", startRow=5, startCol=9, borders="all")
    mergeCells(wb, sheet=1, cols=9:10, rows=5)
    writeData(wb, sheet=1, "Cohabitation dissolution", startRow=5, startCol=11, borders="all")
    mergeCells(wb, sheet=1, cols=11:12, rows=5)
    rownames(total.rates) <- c("Direct calculate",  paste0(method, " estimate", sep=""), "Difference%")
    rownames(mean.age) <- c("Direct calculate",  paste0(method, " estimate", sep=""), "Difference%")
    cname <- c(rep(c("Male", "Female"),5))
    cname <- as.data.frame(t(cname))
    writeData(wb, sheet=1, cname, startRow=6, startCol=3, colNames = FALSE, borders="all")
    writeData(wb, sheet=1, "Total rates", startRow=7, startCol=2, borders="all")
    writeData(wb, sheet=1, total.rates, startRow=8, startCol=2, colNames = FALSE, rowNames = TRUE, borders="all")
    writeData(wb, sheet=1, "Mean age", startRow=11, startCol=2, borders="all")
    writeData(wb, sheet=1, mean.age, startRow=12, startCol=2, colNames = FALSE, rowNames = TRUE, borders="all")
    addStyle(wb, sheet=1, style=forDatstl, rows=5:6, cols=2:12, gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl3, rows=7:9, cols=3:12, gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl5, rows=10, cols=3:12)
    addStyle(wb, sheet=1, style=forDatstl4, rows=11:13, cols=3:12, gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl5, rows=14, cols=3:12)
    setColWidths(wb, sheet=1, cols=3:12, widths=10)
    setColWidths(wb, sheet=1, cols=2, widths=17)
  } else {
    writeData(wb, sheet=1, paste0("Table M5. Total rates and mean ages of marital status transitions, ", title, sep=""),
              startRow=4, startCol=2)
    mergeCells(wb, sheet=1, cols=2:12, rows=4)
    writeData(wb, sheet=1, "First Marriage", startRow=5, startCol=3, borders="all")
    mergeCells(wb, sheet=1, cols=3:4, rows=5)
    writeData(wb, sheet=1, "Remarriage", startRow=5, startCol=5, borders="all")
    mergeCells(wb, sheet=1, cols=5:6, rows=5)
    writeData(wb, sheet=1, "Divorce", startRow=5, startCol=7, borders="all")
    mergeCells(wb, sheet=1, cols=7:8, rows=5)
    writeData(wb, sheet=1, "Cohabitation", startRow=5, startCol=9, borders="all")
    mergeCells(wb, sheet=1, cols=9:10, rows=5)
    writeData(wb, sheet=1, "Cohabitation dissolution", startRow=5, startCol=11, borders="all")
    mergeCells(wb, sheet=1, cols=11:12, rows=5)
    colnames(total.rates) <- c(" ", rep(c("Male", "Female"),5))
    writeData(wb, sheet=1, total.rates, startRow=6, startCol=2, borders="all")
    writeData(wb, sheet=1, mean.age, startRow=7+nrow(total.rates), startCol=2, colNames = FALSE, borders="all")
    addStyle(wb, sheet=1, style=forDatstl, rows=5:6, cols=2:12, gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl3, rows=7:(nrow(total.rates)+6), cols=3:12, gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl4, rows=(nrow(total.rates)+7):(nrow(total.rates)+nrow(mean.age)+6), cols=3:12, gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl5, rows=seq(1, nrow(total.rates)+nrow(mean.age), by=4)+9, cols=3:12, gridExpand = TRUE)
    setColWidths(wb, sheet=1, cols=3:12, widths=10)
    setColWidths(wb, sheet=1, cols=2, widths=17)
  }

  output.dir <- getwd()
  saveWorkbook(wb, paste0(output.dir, "/", title, " Mar7 total rates", name, ".xlsx", sep=""), overwrite=T)
}

write.total.seven.sex <- function(total.rates, mean.age, param, name){
  forDatstl <- createStyle(halign="center", border="TopBottomLeftRight", wrapText = TRUE)
  forDatstl2 <- createStyle(halign="left", border="TopBottomLeftRight", wrapText = TRUE)
  forDatstl3 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0000")
  forDatstl4 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0")
  forDatstl5 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0%")
  wb <- createWorkbook()
  addWorksheet(wb, "total rates")
  title <- as.character(param$title)

  if (nrow(total.rates)==3){
    writeData(wb, sheet=1, paste0("Table M5. Total rates and mean ages of marital status transitions, ", title, sep=""),
              startRow=4, startCol=2)
    mergeCells(wb, sheet=1, cols=2:12, rows=4)
    writeData(wb, sheet=1, "First Marriage", startRow=5, startCol=3, borders="all")
    mergeCells(wb, sheet=1, cols=3:4, rows=5)
    writeData(wb, sheet=1, "Remarriage", startRow=5, startCol=5, borders="all")
    mergeCells(wb, sheet=1, cols=5:6, rows=5)
    writeData(wb, sheet=1, "Divorce", startRow=5, startCol=7, borders="all")
    mergeCells(wb, sheet=1, cols=7:8, rows=5)
    writeData(wb, sheet=1, "Cohabitation", startRow=5, startCol=9, borders="all")
    mergeCells(wb, sheet=1, cols=9:10, rows=5)
    writeData(wb, sheet=1, "Cohabitation dissolution", startRow=5, startCol=11, borders="all")
    mergeCells(wb, sheet=1, cols=11:12, rows=5)
    rownames(total.rates) <- c("Direct calculate", "Poisson estimate", "Difference%")
    rownames(mean.age) <- c("Direct calculate", "Poisson estimate", "Difference%")
    cname <- c(rep(c("Male", "Female"),5))
    cname <- as.data.frame(t(cname))
    writeData(wb, sheet=1, cname, startRow=6, startCol=3, colNames = FALSE, borders="all")
    writeData(wb, sheet=1, "Total rates", startRow=7, startCol=2, borders="all")
    writeData(wb, sheet=1, total.rates, startRow=8, startCol=2, colNames = FALSE, rowNames = TRUE, borders="all")
    writeData(wb, sheet=1, "Mean age", startRow=11, startCol=2, borders="all")
    writeData(wb, sheet=1, mean.age, startRow=12, startCol=2, colNames = FALSE, rowNames = TRUE, borders="all")
    addStyle(wb, sheet=1, style=forDatstl, rows=5:6, cols=2:12, gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl3, rows=7:9, cols=3:12, gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl5, rows=10, cols=3:12)
    addStyle(wb, sheet=1, style=forDatstl4, rows=11:13, cols=3:12, gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl5, rows=14, cols=3:12)
    setColWidths(wb, sheet=1, cols=3:12, widths=10)
    setColWidths(wb, sheet=1, cols=2, widths=17)
  } else {

    writeData(wb, sheet=1, paste0("Table M5. Total rates of marital status transitions, ", title, sep=""),
              startRow=4, startCol=2)
    mergeCells(wb, sheet=1, cols=2:13, rows=4)
    writeData(wb, sheet=1, "First Marriage", startRow=5, startCol=4, borders="all")
    mergeCells(wb, sheet=1, cols=4:5, rows=5)
    writeData(wb, sheet=1, "Remarriage", startRow=5, startCol=6, borders="all")
    mergeCells(wb, sheet=1, cols=6:7, rows=5)
    writeData(wb, sheet=1, "Divorce", startRow=5, startCol=8, borders="all")
    mergeCells(wb, sheet=1, cols=8:9, rows=5)
    writeData(wb, sheet=1, "Cohabitation", startRow=5, startCol=10, borders="all")
    mergeCells(wb, sheet=1, cols=10:11, rows=5)
    writeData(wb, sheet=1, "Cohabitation dissolution", startRow=5, startCol=12, borders="all")
    mergeCells(wb, sheet=1, cols=12:13, rows=5)
    colnames(total.rates) <- c("", "", rep(c("Male", "Female"),5))
    writeData(wb, sheet=1, total.rates, startRow=6, startCol=2, borders="all")
    writeData(wb, sheet=1, "Name", startRow=5, startCol=2, borders="all")
    mergeCells(wb, sheet=1, cols=2, rows=5:6)
    writeData(wb, sheet=1, "Direct calculate and Poisson estimate", startRow=5, startCol=3, borders="all")
    mergeCells(wb, sheet=1, cols=3, rows=5:6)
    addStyle(wb, sheet=1, style=forDatstl, rows=5:6, cols=2:13, gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl3, rows=7:(nrow(total.rates)+6), cols=4:13, gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl5, rows=seq(1, nrow(total.rates), by=3)+8, cols=4:13, gridExpand = TRUE)
    setColWidths(wb, sheet=1, cols=4:13, widths=10)
    setColWidths(wb, sheet=1, cols=2, widths=13)
    setColWidths(wb, sheet=1, cols=3, widths=17)

    addWorksheet(wb, "mean ages")
    writeData(wb, sheet=2, paste0("Table M5. Mean ages of marital status transitions, ", title, sep=""),
              startRow=4, startCol=2)
    mergeCells(wb, sheet=2, cols=2:13, rows=4)
    writeData(wb, sheet=2, "First Marriage", startRow=5, startCol=4, borders="all")
    mergeCells(wb, sheet=2, cols=4:5, rows=5)
    writeData(wb, sheet=2, "Remarriage", startRow=5, startCol=6, borders="all")
    mergeCells(wb, sheet=2, cols=6:7, rows=5)
    writeData(wb, sheet=2, "Divorce", startRow=5, startCol=8, borders="all")
    mergeCells(wb, sheet=2, cols=8:9, rows=5)
    writeData(wb, sheet=2, "Cohabitation", startRow=5, startCol=10, borders="all")
    mergeCells(wb, sheet=2, cols=10:11, rows=5)
    writeData(wb, sheet=2, "Cohabitation dissolution", startRow=5, startCol=12, borders="all")
    mergeCells(wb, sheet=2, cols=12:13, rows=5)
    colnames(mean.age) <- c("", "", rep(c("Male", "Female"),5))
    writeData(wb, sheet=2, mean.age, startRow=6, startCol=2, borders="all")
    writeData(wb, sheet=2, "Name", startRow=5, startCol=2, borders="all")
    mergeCells(wb, sheet=2, cols=2, rows=5:6)
    writeData(wb, sheet=2, "Direct calculate and Poisson estimate", startRow=5, startCol=3, borders="all")
    mergeCells(wb, sheet=2, cols=3, rows=5:6)
    addStyle(wb, sheet=2, style=forDatstl, rows=5:6, cols=2:13, gridExpand = TRUE)
    addStyle(wb, sheet=2, style=forDatstl4, rows=7:(nrow(mean.age)+6), cols=4:13, gridExpand = TRUE)
    addStyle(wb, sheet=2, style=forDatstl5, rows=seq(1, nrow(mean.age), by=3)+8, cols=4:13, gridExpand = TRUE)
    setColWidths(wb, sheet=2, cols=4:13, widths=10)
    setColWidths(wb, sheet=2, cols=2, widths=13)
    setColWidths(wb, sheet=2, cols=3, widths=17)

  }

  output.dir <- getwd()
  saveWorkbook(wb, paste0(output.dir, "/", title, " Mar7 total rates", name, ".xlsx", sep=""), overwrite=T)
}

write.total.seven.nosex <- function(total.rates, mean.age, param, name){
  forDatstl <- createStyle(halign="center", border="TopBottomLeftRight", wrapText = TRUE)
  forDatstl2 <- createStyle(halign="left", border="TopBottomLeftRight", wrapText = TRUE)
  forDatstl3 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0000")
  forDatstl4 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0")
  forDatstl5 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0%")
  wb <- createWorkbook()
  addWorksheet(wb, "total rates")
  title <- as.character(param$title)

  if (nrow(total.rates)==3){
    writeData(wb, sheet=1, paste0("Table M5. Total rates and mean ages of marital status transitions, ", title, sep=""),
              startRow=4, startCol=2)
    mergeCells(wb, sheet=1, cols=2:7, rows=4)
    rownames(total.rates) <- c("Direct calculate", "Poisson estimate", "Difference%")
    rownames(mean.age) <- c("Direct calculate", "Poisson estimate", "Difference%")
    cname <- c("First Marriage", "Remarriage", "Divorce", "Cohabitation", "Cohabitation dissolution")
    cname <- as.data.frame(t(cname))
    writeData(wb, sheet=1, cname, startRow=5, startCol=3, colNames = FALSE, borders="all")
    writeData(wb, sheet=1, "Total rates", startRow=6, startCol=2, borders="all")
    writeData(wb, sheet=1, total.rates, startRow=7, startCol=2, colNames = FALSE, rowNames = TRUE, borders="all")
    writeData(wb, sheet=1, "Mean age", startRow=10, startCol=2, borders="all")
    writeData(wb, sheet=1, mean.age, startRow=11, startCol=2, colNames = FALSE, rowNames = TRUE, borders="all")
    addStyle(wb, sheet=1, style=forDatstl, rows=4:5, cols=2:7, gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl3, rows=6:8, cols=3:7, gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl5, rows=9, cols=3:7)
    addStyle(wb, sheet=1, style=forDatstl4, rows=10:12, cols=3:7, gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl5, rows=13, cols=3:7)
    setColWidths(wb, sheet=1, cols=3:7, widths=10)
    setColWidths(wb, sheet=1, cols=2, widths=17)
  } else {
    writeData(wb, sheet=1, paste0("Table M5. Total rates of marital status transitions, ", title, sep=""),
              startRow=4, startCol=2)
    mergeCells(wb, sheet=1, cols=2:8, rows=4)
    colnames(total.rates) <- c("Name", "Direct calculate and Poisson estimate",
                               "First Marriage", "Remarriage", "Divorce",
                               "Cohabitation", "Cohabitation dissolution")
    writeData(wb, sheet=1, total.rates, startRow=5, startCol=2, borders="all")
    addStyle(wb, sheet=1, style=forDatstl, rows=5, cols=2:8, gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl3, rows=6:(nrow(total.rates)+5), cols=4:8, gridExpand = TRUE)
    addStyle(wb, sheet=1, style=forDatstl5, rows=seq(1, nrow(total.rates), by=3)+7, cols=4:8, gridExpand = TRUE)
    setColWidths(wb, sheet=1, cols=4:8, widths=10)
    setColWidths(wb, sheet=1, cols=2, widths=13)
    setColWidths(wb, sheet=1, cols=3, widths=17)

    addWorksheet(wb, "mean ages")
    writeData(wb, sheet=2, paste0("Table M5. Mean ages of marital status transitions, ", title, sep=""),
              startRow=4, startCol=2)
    mergeCells(wb, sheet=2, cols=2:8, rows=4)
    colnames(mean.age) <- c("Name", "Direct calculate and Poisson estimate",
                            "First Marriage", "Remarriage", "Divorce",
                            "Cohabitation", "Cohabitation dissolution")
    writeData(wb, sheet=2, mean.age, startRow=5, startCol=2, borders="all")
    addStyle(wb, sheet=2, style=forDatstl, rows=5, cols=2:8, gridExpand = TRUE)
    addStyle(wb, sheet=2, style=forDatstl4, rows=6:(nrow(mean.age)+5), cols=4:8, gridExpand = TRUE)
    addStyle(wb, sheet=2, style=forDatstl5, rows=seq(1, nrow(mean.age), by=3)+7, cols=4:8, gridExpand = TRUE)
    setColWidths(wb, sheet=2, cols=4:8, widths=10)
    setColWidths(wb, sheet=2, cols=2, widths=13)
    setColWidths(wb, sheet=2, cols=3, widths=17)
  }

  output.dir <- getwd()
  saveWorkbook(wb, paste0(output.dir, "/", title, " Mar7 total rates", name, ".xlsx", sep=""), overwrite=T)
}

write.total.seven.covar.sex <- function(total.rates.ru, total.rates.ub, mean.age.ru, mean.age.ub, param, name){
  forDatstl <- createStyle(halign="center", border="TopBottomLeftRight", wrapText = TRUE)
  forDatstl2 <- createStyle(halign="left", border="TopBottomLeftRight", wrapText = TRUE)
  forDatstl3 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0000")
  forDatstl4 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0")
  forDatstl5 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0%")
  wb <- createWorkbook()
  title <- as.character(param$title)

  for (i in 1:4) {

    if (i==1){
      addWorksheet(wb, "total rates rural")
      writeData(wb, sheet=1, paste0("Table M5. Total rates of marital status transitions, ", title, sep=""),
                startRow=4, startCol=2)
      input <- total.rates.ru
    }

    if (i==2){
      addWorksheet(wb, "mean ages rural")
      writeData(wb, sheet=2, paste0("Table M5. Mean ages of marital status transitions, ", title, sep=""),
                startRow=4, startCol=2)
      input <- mean.age.ru
    }

    if (i==3){
      addWorksheet(wb, "total rates urban")
      writeData(wb, sheet=3, paste0("Table M5. Total rates of marital status transitions, ", title, sep=""),
                startRow=4, startCol=2)
      input <- total.rates.ub
    }

    if (i==4){
      addWorksheet(wb, "mean ages urban")
      writeData(wb, sheet=4, paste0("Table M5. Mean ages of marital status transitions, ", title, sep=""),
                startRow=4, startCol=2)
      input <- mean.age.ub
    }

    writeData(wb, sheet=i, "First Marriage", startRow=5, startCol=3, borders="all")
    mergeCells(wb, sheet=i, cols=3:8, rows=5)
    writeData(wb, sheet=i, "Remarriage", startRow=5, startCol=9, borders="all")
    mergeCells(wb, sheet=i, cols=9:14, rows=5)
    writeData(wb, sheet=i, "Divorce", startRow=5, startCol=15, borders="all")
    mergeCells(wb, sheet=i, cols=15:20, rows=5)
    writeData(wb, sheet=i, "Cohabitation", startRow=5, startCol=21, borders="all")
    mergeCells(wb, sheet=i, cols=21:26, rows=5)
    writeData(wb, sheet=i, "Cohabitation dissolution", startRow=5, startCol=27, borders="all")
    mergeCells(wb, sheet=i, cols=27:32, rows=5)
    writeData(wb, sheet=i, "Male", startRow=6, startCol=3, borders="all")
    writeData(wb, sheet=i, "Male", startRow=6, startCol=9, borders="all")
    writeData(wb, sheet=i, "Male", startRow=6, startCol=15, borders="all")
    writeData(wb, sheet=i, "Male", startRow=6, startCol=21, borders="all")
    writeData(wb, sheet=i, "Male", startRow=6, startCol=27, borders="all")
    writeData(wb, sheet=i, "Female", startRow=6, startCol=6, borders="all")
    writeData(wb, sheet=i, "Female", startRow=6, startCol=12, borders="all")
    writeData(wb, sheet=i, "Female", startRow=6, startCol=18, borders="all")
    writeData(wb, sheet=i, "Female", startRow=6, startCol=24, borders="all")
    writeData(wb, sheet=i, "Female", startRow=6, startCol=30, borders="all")
    mergeCells(wb, sheet=i, cols=3:5, rows=6)
    mergeCells(wb, sheet=i, cols=6:8, rows=6)
    mergeCells(wb, sheet=i, cols=9:11, rows=6)
    mergeCells(wb, sheet=i, cols=12:14, rows=6)
    mergeCells(wb, sheet=i, cols=15:17, rows=6)
    mergeCells(wb, sheet=i, cols=18:20, rows=6)
    mergeCells(wb, sheet=i, cols=21:23, rows=6)
    mergeCells(wb, sheet=i, cols=24:26, rows=6)
    mergeCells(wb, sheet=i, cols=27:29, rows=6)
    mergeCells(wb, sheet=i, cols=30:32, rows=6)
    colnames(input) <- c("Name", rep(c("Direct Calculate", "Poisson Estimate", "Difference%"),10))
    writeData(wb, sheet=i, input, startRow=7, startCol=2, borders="all")
    addStyle(wb, sheet=i, style=forDatstl, rows=5:7, cols=2:32, gridExpand = TRUE)
    addStyle(wb, sheet=i, style=forDatstl2, rows=8:(nrow(input)+7), cols=2, gridExpand = TRUE)

    if (i%%2==0){
      addStyle(wb, sheet=i, style=forDatstl4, rows=8:(nrow(input)+7), cols=c(3:4, 6:7, 9:10, 12:13, 15:16, 18:19, 21:22, 24:25, 27:28, 30:31), gridExpand = TRUE)
    } else {
      addStyle(wb, sheet=i, style=forDatstl3, rows=8:(nrow(input)+7), cols=c(3:4, 6:7, 9:10, 12:13, 15:16, 18:19, 21:22, 24:25, 27:28, 30:31), gridExpand = TRUE)
    }

    addStyle(wb, sheet=i, style=forDatstl5, rows=8:(nrow(input)+7), cols=seq(5, 32, 3), gridExpand = TRUE)
    setColWidths(wb, sheet=i, cols=3:32, widths=9)
    setColWidths(wb, sheet=i, cols=2, widths=13)

  }

  output.dir <- getwd()
  saveWorkbook(wb, paste0(output.dir, "/", title, " Mar7 total rates", name, ".xlsx", sep=""), overwrite=T)
}

write.total.seven.covar.nosex <- function(total.rates.ru, total.rates.ub, mean.age.ru, mean.age.ub, param, name){
  forDatstl <- createStyle(halign="center", border="TopBottomLeftRight", wrapText = TRUE)
  forDatstl2 <- createStyle(halign="left", border="TopBottomLeftRight", wrapText = TRUE)
  forDatstl3 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0000")
  forDatstl4 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0")
  forDatstl5 <- createStyle(halign="right", border="TopBottomLeftRight", wrapText = TRUE, numFmt = "#,##0.0%")
  wb <- createWorkbook()
  title <- as.character(param$title)

  for (i in 1:4) {

    if (i==1){
      addWorksheet(wb, "total rates rural")
      writeData(wb, sheet=1, paste0("Table M5. Total rates of marital status transitions, ", title, sep=""),
                startRow=4, startCol=2)
      input <- total.rates.ru
    }

    if (i==2){
      addWorksheet(wb, "mean ages rural")
      writeData(wb, sheet=2, paste0("Table M5. Mean ages of marital status transitions, ", title, sep=""),
                startRow=4, startCol=2)
      input <- mean.age.ru
    }

    if (i==3){
      addWorksheet(wb, "total rates urban")
      writeData(wb, sheet=3, paste0("Table M5. Total rates of marital status transitions, ", title, sep=""),
                startRow=4, startCol=2)
      input <- total.rates.ub
    }

    if (i==4){
      addWorksheet(wb, "mean ages urban")
      writeData(wb, sheet=4, paste0("Table M5. Mean ages of marital status transitions, ", title, sep=""),
                startRow=4, startCol=2)
      input <- mean.age.ub
    }

    writeData(wb, sheet=i, "First Marriage", startRow=5, startCol=3, borders="all")
    mergeCells(wb, sheet=i, cols=3:5, rows=5)
    writeData(wb, sheet=i, "Remarriage", startRow=5, startCol=6, borders="all")
    mergeCells(wb, sheet=i, cols=6:8, rows=5)
    writeData(wb, sheet=i, "Divorce", startRow=5, startCol=9, borders="all")
    mergeCells(wb, sheet=i, cols=9:11, rows=5)
    writeData(wb, sheet=i, "Cohabitation", startRow=5, startCol=12, borders="all")
    mergeCells(wb, sheet=i, cols=12:14, rows=5)
    writeData(wb, sheet=i, "Cohabitation dissolution", startRow=5, startCol=15, borders="all")
    mergeCells(wb, sheet=i, cols=15:17, rows=5)
    colnames(input) <- c("Region Name", rep(c("Direct Calculate", "Poisson Estimate", "Difference%"),5))
    writeData(wb, sheet=i, input, startRow=6, startCol=2, borders="all")
    addStyle(wb, sheet=i, style=forDatstl, rows=5:6, cols=2:17, gridExpand = TRUE)
    addStyle(wb, sheet=i, style=forDatstl2, rows=7:(nrow(input)+6), cols=2, gridExpand = TRUE)

    if (i%%2==0){
      addStyle(wb, sheet=i, style=forDatstl4, rows=7:(nrow(input)+6), cols=c(3:4, 6:7, 9:10, 12:13, 15:16), gridExpand = TRUE)
    } else {
      addStyle(wb, sheet=i, style=forDatstl3, rows=7:(nrow(input)+6), cols=c(3:4, 6:7, 9:10, 12:13, 15:16), gridExpand = TRUE)
    }

    addStyle(wb, sheet=i, style=forDatstl5, rows=7:(nrow(input)+6), cols=seq(5, 17, 3), gridExpand = TRUE)
    setColWidths(wb, sheet=i, cols=3:17, widths=9)
    setColWidths(wb, sheet=i, cols=2, widths=13)

  }

  output.dir <- getwd()
  saveWorkbook(wb, paste0(output.dir, "/", title, " Mar7 total rates", name, ".xlsx", sep=""), overwrite=T)
}

write.list.four <- function(param, name){
  wb <- createWorkbook()
  addWorksheet(wb, "Table")
  addWorksheet(wb, "Figure")
  title <- as.character(param$title)

  writeData(wb, sheet=1, paste0("Table M1. Person-years by age, gender and marital status, ", title, name, sep=""),
            startRow=2, startCol=2)
  writeData(wb, sheet=1, paste0("Table M2. Number of event by age, gender and event type, ", title, name, sep=""),
            startRow=3, startCol=2)
  writeData(wb, sheet=1, paste0("Table M3. Age-gender-specific marital status transition frequencies, ", title, name, sep=""),
            startRow=4, startCol=2)
  writeData(wb, sheet=1, paste0("Table M4. Age-gender-specific marital status transition o/e rates, ", title, name, sep=""),
            startRow=5, startCol=2)
  writeData(wb, sheet=1, paste0("Table M5. Total rates and mean ages of marital status transitions, ", title, sep=""),
            startRow=6, startCol=2)
  setColWidths(wb, sheet=1, cols=2, widths=90)

  writeData(wb, sheet=2, paste0("Figure 1.1. First marriage frequencies, males", name, sep=""),
            startRow=2, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 1.2. First marriage frequencies, females", name, sep=""),
            startRow=3, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 2.1. Divorced frequencies, males", name, sep=""),
            startRow=4, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 2.2. Divorced frequencies, females", name, sep=""),
            startRow=5, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 3.1. Widowed to remarriage frequencies, males", name, sep=""),
            startRow=6, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 3.2. Widowed to remarriage frequencies, females", name, sep=""),
            startRow=7, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 4.1. Divorced to remarriage frequencies, males", name, sep=""),
            startRow=8, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 4.2. Divorced to remarriage frequencies, females", name, sep=""),
            startRow=9, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 5.1. First marriage o/e rates, males", name, sep=""),
            startRow=10, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 5.2. First marriage o/e rates, females", name, sep=""),
            startRow=11, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 6.1. Divorced o/e rates, males", name, sep=""),
            startRow=12, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 6.2. Divorced o/e rates, females", name, sep=""),
            startRow=13, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 7.1. Widowed to remarriage o/e rates, males", name, sep=""),
            startRow=14, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 7.2. Widowed to remarriage o/e rates, females", name, sep=""),
            startRow=15, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 8.1. Divorced to remarriage o/e rates, males", name, sep=""),
            startRow=16, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 8.2. Divorced to remarriage o/e rates, females", name, sep=""),
            startRow=17, startCol=2)
  setColWidths(wb, sheet=2, cols=2, widths=90)

  output.dir <- getwd()
  saveWorkbook(wb, paste0(output.dir, "/", title, " List of Tables and Figures", name, ".xlsx", sep=""), overwrite=T)
}

write.list.seven <- function(param, name){
  wb <- createWorkbook()
  addWorksheet(wb, "Table")
  addWorksheet(wb, "Figure")
  title <- as.character(param$title)

  writeData(wb, sheet=1, paste0("Table M1.1. Male person-years by age and marital/union status, ", title, name, sep=""),
            startRow=2, startCol=2)
  writeData(wb, sheet=1, paste0("Table M1.2. Female person-years by age and marital/union status, ", title, name, sep=""),
            startRow=3, startCol=2)
  writeData(wb, sheet=1, paste0("Table M2.1. Number of event by age and event type, males, ", title, name, sep=""),
            startRow=4, startCol=2)
  writeData(wb, sheet=1, paste0("Table M2.2. Number of event by age and event type, females, ", title, name, sep=""),
            startRow=5, startCol=2)
  writeData(wb, sheet=1, paste0("Table M3.1. Age-gender-specific marital/union status transition frequencies, males, ", title, name, sep=""),
            startRow=6, startCol=2)
  writeData(wb, sheet=1, paste0("Table M3.2. Age-gender-specific marital/union status transition frequencies, females, ", title, name, sep=""),
            startRow=7, startCol=2)
  writeData(wb, sheet=1, paste0("Table M4.1. Age-gender-specific marital/union status transition o/e rates, males, ", title, name, sep=""),
            startRow=8, startCol=2)
  writeData(wb, sheet=1, paste0("Table M4.2. Age-gender-specific marital/union status transition o/e rates, females, ", title, name, sep=""),
            startRow=9, startCol=2)
  writeData(wb, sheet=1, paste0("Table M5. Total rates and mean ages of marital/union status transitions, ", title, sep=""),
            startRow=10, startCol=2)
  setColWidths(wb, sheet=1, cols=2, widths=90)

  writeData(wb, sheet=2, paste0("Figure 1.1. Never married & not-cohabiting to married frequencies, males", name, sep=""),
            startRow=2, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 1.2. Never married & not-cohabiting to married frequencies, females", name, sep=""),
            startRow=3, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 2.1. Never married & cohabiting to married frequencies, males", name, sep=""),
            startRow=4, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 2.2. Never married & cohabiting to married frequencies, females", name, sep=""),
            startRow=5, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 3.1. Divorced frequencies, males", name, sep=""),
            startRow=6, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 3.2. Divorced frequencies, females", name, sep=""),
            startRow=7, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 4.1. Remarriage of widowed & not-cohabiting frequencies, males", name, sep=""),
            startRow=8, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 4.2. Remarriage of widowed & not-cohabiting frequencies, females", name, sep=""),
            startRow=9, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 5.1. Remarriage of widowed & cohabiting frequencies, males", name, sep=""),
            startRow=10, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 5.2. Remarriage of widowed & cohabiting frequencies, females", name, sep=""),
            startRow=11, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 6.1. Remarriage of divorced & not-cohabiting frequencies, males", name, sep=""),
            startRow=12, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 6.2. Remarriage of divorced & not-cohabiting frequencies, females", name, sep=""),
            startRow=13, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 7.1. Remarriage of divorced & cohabiting frequencies, males", name, sep=""),
            startRow=14, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 7.2. Remarriage of divorced & cohabiting frequencies, females", name, sep=""),
            startRow=15, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 8.1. Never married to cohabiting frequencies, males", name, sep=""),
            startRow=16, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 8.2. Never married to cohabiting frequencies, females", name, sep=""),
            startRow=17, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 9.1. Widowed to cohabiting frequencies, males", name, sep=""),
            startRow=18, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 9.2. Widowed to cohabiting frequencies, females", name, sep=""),
            startRow=19, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 10.1. Divorced to cohabiting frequencies, males", name, sep=""),
            startRow=20, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 10.2. Divorced to cohabiting frequencies, females", name, sep=""),
            startRow=21, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 11.1. Cohabiting to never married frequencies, males", name, sep=""),
            startRow=22, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 11.2. Cohabiting to never married frequencies, females", name, sep=""),
            startRow=23, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 12.1. Cohabiting to widowed frequencies, males", name, sep=""),
            startRow=24, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 12.2. Cohabiting to widowed frequencies, females", name, sep=""),
            startRow=25, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 13.1. Cohabiting to divorced frequencies, males", name, sep=""),
            startRow=26, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 13.2. Cohabiting to divorced frequencies, females", name, sep=""),
            startRow=27, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 14.1. Never married & not-cohabiting to married o/e rates, males", name, sep=""),
            startRow=28, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 14.2. Never married & not-cohabiting to married o/e rates, females", name, sep=""),
            startRow=29, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 15.1. Never married & cohabiting to married o/e rates, males", name, sep=""),
            startRow=30, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 15.2. Never married & cohabiting to married o/e rates, females", name, sep=""),
            startRow=31, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 16.1. Divorced o/e rates, males", name, sep=""),
            startRow=32, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 16.2. Divorced o/e rates, females", name, sep=""),
            startRow=33, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 17.1. Remarriage of widowed & not-cohabiting o/e rates, males", name, sep=""),
            startRow=34, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 17.2. Remarriage of widowed & not-cohabiting o/e rates, females", name, sep=""),
            startRow=35, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 18.1. Remarriage of widowed & cohabiting o/e rates, males", name, sep=""),
            startRow=36, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 18.2. Remarriage of widowed & cohabiting o/e rates, females", name, sep=""),
            startRow=37, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 19.1. Remarriage of divorced & not-cohabiting o/e rates, males", name, sep=""),
            startRow=38, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 19.2. Remarriage of divorced & not-cohabiting o/e rates, females", name, sep=""),
            startRow=39, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 20.1. Remarriage of divorced & cohabiting o/e rates, males", name, sep=""),
            startRow=40, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 20.2. Remarriage of divorced & cohabiting o/e rates, females", name, sep=""),
            startRow=41, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 21.1. Never married to cohabiting o/e rates, males", name, sep=""),
            startRow=42, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 21.2. Never married to cohabiting o/e rates, females", name, sep=""),
            startRow=43, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 22.1. Widowed to cohabiting o/e rates, males", name, sep=""),
            startRow=44, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 22.2. Widowed to cohabiting o/e rates, females", name, sep=""),
            startRow=45, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 23.1. Divorced to cohabiting o/e rates, males", name, sep=""),
            startRow=46, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 23.2. Divorced to cohabiting o/e rates, females", name, sep=""),
            startRow=47, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 24.1. Cohabiting to never married o/e rates, males", name, sep=""),
            startRow=48, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 24.2. Cohabiting to never married o/e rates, females", name, sep=""),
            startRow=49, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 25.1. Cohabiting to widowed o/e rates, males", name, sep=""),
            startRow=50, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 25.2. Cohabiting to widowed o/e rates, females", name, sep=""),
            startRow=51, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 26.1. Cohabiting to divorced o/e rates, males", name, sep=""),
            startRow=52, startCol=2)
  writeData(wb, sheet=2, paste0("Figure 26.2. Cohabiting to divorced o/e rates, females", name, sep=""),
            startRow=53, startCol=2)
  setColWidths(wb, sheet=2, cols=2, widths=90)

  output.dir <- getwd()
  saveWorkbook(wb, paste0(output.dir, "/", title, " Mar7 List of Tables and Figures", name, ".xlsx", sep=""), overwrite=T)
}


