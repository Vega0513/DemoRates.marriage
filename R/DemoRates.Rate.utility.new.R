#' DemoRates run.rates: Estimate demographic transition rates and frequency.
#'
#' The function \code{run.rates} deals with the cleaned Excel file with spreadsheets of Parameters, Code Names and Person Record. This function conducts Poisson regression model to estimate the transition rates and frequencies of events.
#'
#' @import readxl
#' @import writexl
#' @import openxlsx
#' @import dplyr
#' @import ggplot2
#' @import mfp
#' @import srvyr
#' @import parallel
#' @import doParallel
#' @importFrom survival survSplit Surv
#' @importFrom stats aggregate na.omit poisson predict reshape setNames
#' @importFrom tidyr fill
#' @importFrom magrittr %>%
#'
#' @param file Tthe input Excel clear data file output by clear.data function.
#' @param csv Either \code{TRUE} or \code{FALSE}. Indicate whether the input data is in csv format.
#' @param para If the Person Record data is imported in csv format, a separated Excel file with spreadsheets Parameters and Name (optional) should be provided.
#' @param plot Either \code{TRUE} or \code{FALSE}.
#'     If \code{TRUE}, the graph of the comparison between the direct estimations and the Poisson regression results are generated for users. If \code{FALSE}, no plot file is generated. By default, \code{plot = FALSE}.
#' @param sex Only available for covariate method. Either \code{TRUE} or \code{FALSE}.
#'     If \code{TRUE}, gender is considered as a factor in estimation. By default, \code{plot = TRUE}.
#' @param method Only available for subset method. Choose the estimation model. Either \code{NA}, "Poisson".
#'     If \code{NA}, only direct calculated results will be output. By default, \code{method = NA}.
#' @param mfp Choose whether to use mfp package or DemoRates-defined function selection process to run Poisson estimation.
#'     Either \code{TRUE} or \code{FALSE}. If \code{TRUE}, mfp package is used. By default, \code{mfp = TRUE}.
#' @param sep.last When split the time interval, decide whether the reminder time period is together with the last time session or not.
#'     Either \code{TRUE} or \code{FALSE}. If \code{TRUE}, the reminder time is separated. By default, \code{sep.last = TRUE}.
#'
#' @export
#'
run.rates <- function(file=NA, csv=FALSE, para=NA, plot=FALSE, sex=TRUE, method=NA, mfp=TRUE, sep.last=TRUE) {
  #### Import data from excel input file

  if (csv==FALSE) {

    ##If normal .xlsx file

    # Person record sheet
    data <- read_excel(file, sheet="Person Record", col_types = "numeric")
    data <- as.data.frame(data)

    # Parameters sheet
    param <- read_excel(file, sheet="Parameters")
    param <- as.data.frame(param)

    #check parameters and name sheet consistency
    nRegion <- as.numeric(param$nRegion)
    nRace <- as.numeric(param$nRace)
    nInterval <- as.numeric(param$output)
    sheetnames <- excel_sheets(file)

    if ("Name" %in% sheetnames) {

      #If there is Name sheet, import
      code <- read_excel(file, col_names = TRUE, sheet="Name")
      code <- as.data.frame(code)

    } else if (nRace>1|nRegion>1){

      #if no Name sheet, but need to estimate by race or region -> generate name sheet.
      `Race Code` <- na.omit(unique(data$race))
      `Race Name` <- as.character(na.omit(unique(data$race)))
      `Region Code` <- na.omit(unique(data$region))
      `Region Name` <- as.character(na.omit(unique(data$region)))
      max.len <- max(length(`Race Code`), length(`Region Code`))
      `Race Code` = c(`Race Code`, rep(NA, max.len - length(`Race Code`)))
      `Race Name` = c(`Race Name`, rep(NA, max.len - length(`Race Name`)))
      `Region Code` = c(`Region Code`, rep(NA, max.len - length(`Region Code`)))
      `Region Name` = c(`Region Name`, rep(NA, max.len - length(`Region Name`)))
      code <- data.frame(`Race Code`, `Race Name`, `Region Code`, `Region Name`)

    } else {code <- NA}

  } else {

    #if input file is .csv file
    data <- read.csv(file, header = T, colClasses = "numeric")
    data <- as.data.frame(as.data.frame(lapply(data, as.numeric)))
    names(data)[1] <- "region"
    param <- read_excel(para, col_names = TRUE, col_types = c(rep("numeric", 21), "text"), sheet="Parameters")
    param <- as.data.frame(param)
    nRegion <- as.numeric(param$nRegion)
    nRace <- as.numeric(param$nRace)
    nInterval <- as.numeric(param$output)
    sheetnames <- excel_sheets(para)

    if ("Name" %in% sheetnames) {
      code <- read_excel(para, col_names = TRUE, sheet="Name")
      code <- as.data.frame(code)
    } else if (nRace>1|nRegion>1){
      `Race Code` <- na.omit(unique(data$race))
      `Race Name` <- as.character(na.omit(unique(data$race)))
      `Region Code` <- na.omit(unique(data$region))
      `Region Name` <- as.character(na.omit(unique(data$region)))
      max.len <- max(length(`Race Code`), length(`Region Code`))
      `Race Code` = c(`Race Code`, rep(NA, max.len - length(`Race Code`)))
      `Race Name` = c(`Race Name`, rep(NA, max.len - length(`Race Name`)))
      `Region Code` = c(`Region Code`, rep(NA, max.len - length(`Region Code`)))
      `Region Name` = c(`Region Name`, rep(NA, max.len - length(`Region Name`)))
      code <- data.frame(`Race Code`, `Race Name`, `Region Code`, `Region Name`)

    } else {code <- NA}
  }

  # if nInterval is 0, then process all data at all
  if (nInterval == 0 ){
    nInterval = 1e10
  }

  nFunction <- as.numeric(param$nFunction)
  nCovariant <- as.numeric(param$nCovariant)
  ratetype <- as.numeric(param$ratetype)

  # original estimation period start and end year-month
  # -> change into century month code
  param$t1Month <- as.numeric(param$t1Month)
  param$t1Year <- as.numeric(param$t1Year)
  param$t2Month <- as.numeric(param$t2Month)
  param$t2Year <- as.numeric(param$t2Year)
  param <- param %>% mutate(t1Month = replace(t1Month, TRUE, (t1Year-1900)*12+t1Month)) %>%
    mutate(t2Month = replace(t2Month, TRUE, (t2Year-1900)*12+t2Month)) %>%
    select(-t1Year, -t2Year)

  t1Month <- as.numeric(param$t1Month)
  t2Month <- as.numeric(param$t2Month)
  title_ <- param$title

  #number of output periods
  if (sep.last==T){
    total_nInterval <- max(1, ceiling((t2Month - t1Month + 1) /nInterval/12))
  } else {
    total_nInterval <- max(1, floor((t2Month - t1Month + 1) /nInterval/12))
  }

  #split estimation period according to output parameter, e.g. every 5 years or 10 years.
  for(i in 1:total_nInterval) {
    t1Month_ <- t1Month + nInterval*(i-1)*12
    t2Month_ <- t1Month + nInterval*i*12 -1

    if (sep.last==T){
      #reminder years separated from the last period
      if (t2Month < t2Month_ & t2Month_ - t2Month< nInterval*12){
        t2Month_ <- t2Month
      }
    } else {
      # reminder years together with the last period
      if (t2Month - t2Month_ < nInterval*12){
        t2Month_ <- t2Month
      }
    }


    # only filter out the current interval's data （to do)
    param <- param %>% mutate(t1Month = replace(t1Month, TRUE, t1Month_)) %>%
      mutate(t2Month = replace(t2Month, TRUE, t2Month_)) %>%
      mutate(title = replace(title, TRUE, paste(title_, " ", (t1Month_-1)%/%12+1900, (t1Month_-1)%%12+1,
                                                "-", (t2Month_-1)%/%12+1900, (t2Month_-1)%%12+1)))

    # skip if no data in current interval
    if (dim(data)[1] != 0) {

      if (nFunction==1){           #marriage or fertility

        if (ratetype == 1) {       #marriage

          if (nCovariant==0){                       ## subset method
            run.marriage.rates(data, param, code, plot, method, mfp)
          } else if (nCovariant==1){                ## covariant method
            run.marriage.rates.covar(data, param, code, plot, sex, mfp)
          }

        }

        # else if (ratetype == 2) {       #fertility
        #
        #   if (nCovariant==0){                       ## subset method
        #     run.fertility.rates(data, param, code, plot, method, mfp)
        #   } else if (nCovariant==1){                ## covariant method
        #     run.fertility.rates.covar(data, param, code, plot, sex, mfp)
        #   }
        #
        # }

      }

      # else if (nFunction==2){           #migration
      #
      #   if (nCovariant==0){
      #     run.migration.rates(data, param, code, plot, method, mfp)
      #   } else if (nCovariant==1){
      #     run.migration.rates.covar(data, param, code, plot, sex, mfp)
      #   }
      #
      # } else if (nFunction==3){           #children leaving home
      #
      #   if (nCovariant==0){
      #     run.leaving.rates(data, param, code, plot, method, mfp)
      #   } else if (nCovariant==1){
      #     run.leaving.rates.covar(data, param, code, plot, sex, mfp)
      #   }
      #
      # } else if (nFunction==4){           #user-defined events
      #
      #   if (nCovariant==0){
      #     run.defined.rates(data, param, code, plot, method, mfp)
      #   } else if (nCovariant==1){
      #     run.defined.rates.covar(data, param, code, plot, sex, mfp)
      #
      #   }
      # }

    }
  }

}

##------Estimation-------

# generate empty data frame for not estimated cases
null.rates <- function(d0, nl, nh, byvar){

  if (byvar=="sex"){         #only by sex

    sex.code <- c(1, 2)

    # number of rows = # of ages (nl -> nh) * # of sex (2)
    null.rates <- data.frame(age=rep(seq(nl, nh, 1), 2),
                             sex=rep(sex.code, each=(nh-nl+1)))

  }

  if (byvar=="sex.nosex") {         #not by any factor
    null.rates <- data.frame(age=seq(nl, nh, 1))
  }

  if (byvar=="ru.nosex"){         #only by rural

    ru.code <- c(1, 2)
    # number of rows = # of ages (nl -> nh) * # of rural/urban (2)
    null.rates <- data.frame(age=rep(seq(nl, nh, 1), 2),
                             ru=rep(ru.code, each = (nh-nl+1)))
  }

  if (byvar=="region.nosex"){      #only by region

    region.code <- na.omit(unique(d0$region))

    # number of rows = # of ages (nl -> nh) * # of regions
    null.rates <- data.frame(age=rep(seq(nl, nh, 1), length(region.code)),
                             region=rep(region.code, each = (nh-nl+1)))
  }

  if (byvar=="race.nosex"){      #only by race

    race.code <- na.omit(unique(d0$race))

    # number of rows = # of ages (nl -> nh) * # of races
    null.rates <- data.frame(age=rep(seq(nl, nh, 1), length(race.code)),
                             race=rep(race.code, each = (nh-nl+1)))
  }

  if (byvar=="ru"){                #by rural and sex

    ru.code <- c(1, 2)
    sex.code <- c(1, 2)

    # number of rows = # of ages (nl -> nh) * # of sex (2) * # of rural/urban (2)
    null.rates <- data.frame(age=rep(seq(nl, nh, 1), 4),
                             ru=rep(ru.code, each = (nh-nl+1)*2),
                             sex=rep(rep(sex.code, each=(nh-nl+1)), 2))
  }

  if (byvar=="region"){            #by region and sex

    region.code <- na.omit(unique(d0$region))
    sex.code <- c(1, 2)

    # number of rows = # of ages (nl -> nh) * # of sex (2) * # of regions
    null.rates <- data.frame(age=rep(seq(nl, nh, 1), length(region.code)*2),
                             region=rep(region.code, each = (nh-nl+1)*2),
                             sex=rep(rep(sex.code, each=(nh-nl+1)), length(region.code)))
  }

  if (byvar=="race"){              #by race and sex
    race.code <- na.omit(unique(d0$race))
    sex.code <- c(1, 2)
    null.rates <- data.frame(age=rep(seq(nl, nh, 1), length(race.code)*2),
                             race=rep(race.code, each = (nh-nl+1)*2),
                             sex=rep(rep(sex.code, each=(nh-nl+1)), length(race.code)))
  }

  if (byvar=="edu"){              #by education and sex
    edu.code <- na.omit(unique(d0$edu))
    sex.code <- c(1, 2)
    null.rates <- data.frame(age=rep(seq(nl, nh, 1), length(edu.code)*2),
                             edu=rep(edu.code, each = (nh-nl+1)*2),
                             sex=rep(rep(sex.code, each=(nh-nl+1)), length(edu.code)))
  }

  if (byvar=="with_p"){              #by living with parents and sex
    p.code <- na.omit(unique(d0$with_p))
    sex.code <- c(1, 2)
    null.rates <- data.frame(age=rep(seq(nl, nh, 1), length(p.code)*2),
                             with_p=rep(p.code, each = (nh-nl+1)*2),
                             sex=rep(rep(sex.code, each=(nh-nl+1)), length(p.code)))
  }

  if (byvar=="with_c"){              #by living with children and sex
    c.code <- na.omit(unique(d0$with_c))
    sex.code <- c(1, 2)
    null.rates <- data.frame(age=rep(seq(nl, nh, 1), length(c.code)*2),
                             with_c=rep(c.code, each = (nh-nl+1)*2),
                             sex=rep(rep(sex.code, each=(nh-nl+1)), length(c.code)))
  }

  if (byvar=="marital"){              #by marital status and sex
    m.code <- na.omit(unique(d0$mar))
    sex.code <- c(1, 2)
    null.rates <- data.frame(age=rep(seq(nl, nh, 1), length(m.code)*2),
                             mar=rep(m.code, each = (nh-nl+1)*2),
                             sex=rep(rep(sex.code, each=(nh-nl+1)), length(m.code)))
  }

  if (byvar=="reg.ru.nosex"){              #by region and rural

    region.code <- na.omit(unique(d0$region))
    ru.code <- c(1, 2)

    # number of rows = # of ages (nl -> nh) * # of rural/urban (2) * # of regions
    null.rates <- data.frame(age=rep(seq(nl, nh, 1), length(region.code)*2),
                             region=rep(region.code, each=(nh-nl+1)*2),
                             ru=rep(rep(ru.code, each = (nh-nl+1)), length(region.code)))
  }

  if (byvar=="reg.ru") {          # by region, rural and sex

    region.code <- na.omit(unique(d0$region))
    ru.code <- c(1, 2)
    sex.code <- c(1, 2)

    # number of rows = # of ages (nl -> nh) * # of sex (2) * # of rural/urban (2) * # of regions
    null.rates <- data.frame(age=rep(seq(nl, nh, 1), length(region.code)*4),
                             region=rep(region.code, each=(nh-nl+1)*4),
                             ru=rep(rep(ru.code, each = (nh-nl+1)*2), length(region.code)),
                             sex=rep(rep(sex.code, each=(nh-nl+1)), length(region.code)*2))
  }

  if (byvar=="mar.ru") {                   #by marital status, rural and sex
    mar.code <- na.omit(unique(d0$mar))
    ru.code <- c(1, 2)
    sex.code <- c(1, 2)
    null.rates <- data.frame(age=rep(seq(nl, nh, 1), length(mar.code)*4),
                             mar=rep(mar.code, each=(nh-nl+1)*4),
                             ru=rep(rep(ru.code, each = (nh-nl+1)*2), length(mar.code)),
                             sex=rep(rep(sex.code, each=(nh-nl+1)), length(mar.code)*2))
  }

  return(null.rates)
}

# generate partially empty data frame to store predicted values
prediction.data <- function(d0, nl, nh, byvar){

  if (byvar=="sex"){
    sex.code <- c(1, 2)

    pred.data <- data.frame(matrix(NA, nrow = (nh-nl+1)*2, ncol = ncol(d0)))
    names(pred.data) <- names(d0)
    pred.data$age <- rep(seq(nl, nh, 1), 2)
    pred.data$sex <- rep(sex.code, each=(nh-nl+1))
    pred.data <- as.data.frame(lapply(pred.data,as.numeric))

  }

  if (byvar=="sex.nosex"){
    pred.data <- data.frame(matrix(NA, nrow = (nh-nl+1), ncol = ncol(d0)))
    names(pred.data) <- names(d0)
    pred.data$age <- rep(seq(nl, nh, 1))
    pred.data <- as.data.frame(lapply(pred.data,as.numeric))
  }

  if (byvar=="reg.ru"){
    region.code <- na.omit(unique(d0$region))
    ru.code <- c(1, 2)
    sex.code <- c(1, 2)

    pred.data <- data.frame(matrix(NA, nrow = (nh-nl+1)*length(region.code)*4, ncol = ncol(d0)))
    names(pred.data) <- names(d0)
    pred.data$age <- rep(seq(nl, nh, 1), length(region.code)*4)
    pred.data$region <- rep(region.code, each=(nh-nl+1)*4)
    pred.data$ru <- rep(rep(ru.code, each = (nh-nl+1)*2), length(region.code))
    pred.data$sex <- rep(rep(sex.code, each=(nh-nl+1)), length(region.code)*2)
    pred.data <- as.data.frame(lapply(pred.data,as.numeric))
  }

  if (byvar=="ru"){
    ru.code <- c(1, 2)
    sex.code <- c(1, 2)

    pred.data <- data.frame(matrix(NA, nrow = (nh-nl+1)*4, ncol = ncol(d0)))
    names(pred.data) <- names(d0)
    pred.data$age <- rep(seq(nl, nh, 1), 4)
    pred.data$ru <- rep(ru.code, each = (nh-nl+1)*2)
    pred.data$sex <- rep(rep(sex.code, each=(nh-nl+1)), 2)
    pred.data <- as.data.frame(lapply(pred.data,as.numeric))
  }

  if (byvar=="region"){
    region.code <- na.omit(unique(d0$region))
    sex.code <- c(1, 2)

    pred.data <- data.frame(matrix(NA, nrow = (nh-nl+1)*length(region.code)*2, ncol = ncol(d0)))
    names(pred.data) <- names(d0)
    pred.data$age <- rep(seq(nl, nh, 1), length(region.code)*2)
    pred.data$region <- rep(region.code, each = (nh-nl+1)*2)
    pred.data$sex <- rep(rep(sex.code, each=(nh-nl+1)), length(region.code))
    pred.data <- as.data.frame(lapply(pred.data,as.numeric))
  }

  if (byvar=="region.nosex"){
    region.code <- na.omit(unique(d0$region))

    pred.data <- data.frame(matrix(NA, nrow = (nh-nl+1)*length(region.code), ncol = ncol(d0)))
    names(pred.data) <- names(d0)
    pred.data$age <- rep(seq(nl, nh, 1), length(region.code))
    pred.data$region <- rep(region.code, each = (nh-nl+1))
    pred.data <- as.data.frame(lapply(pred.data,as.numeric))
  }

  if (byvar=="ru.nosex"){
    ru.code <- c(1, 2)

    pred.data <- data.frame(matrix(NA, nrow = (nh-nl+1)*2, ncol = ncol(d0)))
    names(pred.data) <- names(d0)
    pred.data$age <- rep(seq(nl, nh, 1), 2)
    pred.data$ru <- rep(ru.code, each = (nh-nl+1))
    pred.data <- as.data.frame(lapply(pred.data,as.numeric))
  }

  if (byvar=="race.nosex"){
    race.code <- na.omit(unique(d0$race))

    pred.data <- data.frame(matrix(NA, nrow = (nh-nl+1)*length(race.code), ncol = ncol(d0)))
    names(pred.data) <- names(d0)
    pred.data$age <- rep(seq(nl, nh, 1), length(race.code))
    pred.data$race <- rep(race.code, each = (nh-nl+1))
    pred.data <- as.data.frame(lapply(pred.data,as.numeric))
  }

  if (byvar=="race"){
    race.code <- na.omit(unique(d0$race))
    sex.code <- c(1, 2)

    pred.data <- data.frame(matrix(NA, nrow = (nh-nl+1)*length(race.code)*2, ncol = ncol(d0)))
    names(pred.data) <- names(d0)
    pred.data$age <- rep(seq(nl, nh, 1), length(race.code)*2)
    pred.data$race <- rep(race.code, each = (nh-nl+1)*2)
    pred.data$sex <- rep(rep(sex.code, each=(nh-nl+1)), length(race.code))
    pred.data <- as.data.frame(lapply(pred.data,as.numeric))
  }

  if (byvar=="edu"){
    edu.code <- na.omit(unique(d0$edu))
    sex.code <- c(1, 2)

    pred.data <- data.frame(matrix(NA, nrow = (nh-nl+1)*length(edu.code)*2, ncol = ncol(d0)))
    names(pred.data) <- names(d0)
    pred.data$age <- rep(seq(nl, nh, 1), length(edu.code)*2)
    pred.data$edu <- rep(edu.code, each = (nh-nl+1)*2)
    pred.data$sex <- rep(rep(sex.code, each=(nh-nl+1)), length(edu.code))
    pred.data <- as.data.frame(lapply(pred.data,as.numeric))
  }

  if (byvar=="with_p"){
    p.code <- na.omit(unique(d0$with_p))
    sex.code <- c(1, 2)

    pred.data <- data.frame(matrix(NA, nrow = (nh-nl+1)*length(with_p)*2, ncol = ncol(d0)))
    names(pred.data) <- names(d0)
    pred.data$age <- rep(seq(nl, nh, 1), length(with_p)*2)
    pred.data$edu <- rep(with_p, each = (nh-nl+1)*2)
    pred.data$sex <- rep(rep(sex.code, each=(nh-nl+1)), length(with_p))
    pred.data <- as.data.frame(lapply(pred.data,as.numeric))
  }

  if (byvar=="with_c"){
    c.code <- na.omit(unique(d0$with_c))
    sex.code <- c(1, 2)

    pred.data <- data.frame(matrix(NA, nrow = (nh-nl+1)*length(c.code)*2, ncol = ncol(d0)))
    names(pred.data) <- names(d0)
    pred.data$age <- rep(seq(nl, nh, 1), length(c.code)*2)
    pred.data$edu <- rep(c.code, each = (nh-nl+1)*2)
    pred.data$sex <- rep(rep(sex.code, each=(nh-nl+1)), length(c.code))
    pred.data <- as.data.frame(lapply(pred.data,as.numeric))
  }

  if (byvar=="marital"){
    m.code <- na.omit(unique(d0$mar))
    sex.code <- c(1, 2)

    pred.data <- data.frame(matrix(NA, nrow = (nh-nl+1)*length(m.code)*2, ncol = ncol(d0)))
    names(pred.data) <- names(d0)
    pred.data$age <- rep(seq(nl, nh, 1), length(m.code)*2)
    pred.data$edu <- rep(m.code, each = (nh-nl+1)*2)
    pred.data$sex <- rep(rep(sex.code, each=(nh-nl+1)), length(m.code))
    pred.data <- as.data.frame(lapply(pred.data,as.numeric))
  }

  if (byvar=="reg.ru.nosex"){
    region.code <- na.omit(unique(d0$region))
    ru.code <- c(1, 2)

    pred.data <- data.frame(matrix(NA, nrow = (nh-nl+1)*length(region.code)*2, ncol = ncol(d0)))
    names(pred.data) <- names(d0)
    pred.data$age <- rep(seq(nl, nh, 1), length(region.code)*2)
    pred.data$region <- rep(region.code, each=(nh-nl+1)*2)
    pred.data$ru <- rep(rep(ru.code, each = (nh-nl+1)), length(region.code))
    pred.data <- as.data.frame(lapply(pred.data,as.numeric))
  }

  if (byvar=="mar.ru"){
    mar.code <- na.omit(unique(d0$mar))
    ru.code <- c(1, 2)
    sex.code <- c(1, 2)

    pred.data <- data.frame(matrix(NA, nrow = (nh-nl+1)*length(mar.code)*4, ncol = ncol(d0)))
    names(pred.data) <- names(d0)
    pred.data$age <- rep(seq(nl, nh, 1), length(mar.code)*4)
    pred.data$mar <- rep(mar.code, each=(nh-nl+1)*4)
    pred.data$ru <- rep(rep(ru.code, each = (nh-nl+1)*2), length(mar.code))
    pred.data$sex <- rep(rep(sex.code, each=(nh-nl+1)), length(mar.code)*2)
    pred.data <- as.data.frame(lapply(pred.data,as.numeric))
  }


  return(pred.data)
}

oe.raw <- function(data, nl, nh, status, evt, nWeight){

  # (revised) count in weights
  if (nWeight == 0){
    data$weight <- 1
  }
  data$py = data$py * data$weight
  # (revise over)

  d0 <- subset(data, prev==status)
  d0 <- d0[which(d0$age <= nh),]

  if(nrow(d0) == 0){
    oe <- data.frame(age=seq(nl, nh, 1), raw.rates=NA)
  } else{
    d0[which(d0$event != evt & d0$event != 0), "event"] <- 0
    d0[which(d0$event == evt), "event"] <- 1
    dpy <- subset(data, post == status)
    age <- data.frame(Group.1 = seq(nl, nh, 1))
    t.py <- aggregate(dpy$py, list(dpy$age), sum)
    t.py <- left_join(age, t.py, by = "Group.1")
    raw.event <- aggregate(d0$event * d0$weight, list(d0$age), sum)
    raw.event <- left_join(age, raw.event, by = "Group.1")


    if(sum(raw.event$x, na.rm = T) < 30){
      oe <- data.frame(age=seq(nl, nh, 1), raw.rates=NA)
    } else{
      oe <- data.frame(raw.event$Group.1, raw.event$x/t.py$x)
      names(oe) <- c("age", "raw.rates")
      oe[,2] <- round(oe[,2], 10)
    }

  }

  return(oe)
}

oe.poi <- function(data, nl, nh, status, evt, nWeight, mfp){

  # (revised) count in weights
  if (nWeight == 0){
    data$weight <- 1
  }
  data$py = data$py * data$weight
  # (revise over)

  d0 <- subset(data, prev==status)
  d0 <- d0[which(d0$age <= nh),]

  if(nrow(d0) == 0){
    oe <- data.frame(age=seq(nl, nh, 1), est.rates=NA)
  } else{
    minage <- min(d0$age)
    pred.data <- data.frame(matrix(NA, nrow = nh-minage+1, ncol = ncol(d0)))
    names(pred.data) <- names(d0)
    pred.data$age <- seq(minage, nh, 1)
    pred.data <- as.data.frame(lapply(pred.data,as.numeric))
    d0[which(d0$event != evt & d0$event != 0), "event"] <- 0
    d0[which(d0$event == evt), "event"] <- 1
    raw.event <- aggregate(d0$event* d0$weight, list(d0$age), sum)


    if(sum(raw.event$x, na.rm = T) < 30){
      oe <- data.frame(age=seq(nl, nh, 1), est.rates=NA)
    } else{

      if (mfp==T){
        model <- mfp(event ~ fp(age), data=d0, family=poisson)
        pred <- predict(model, pred.data, type="response")
      } else {
        # (revised)
        # if (nWeight==0){
        #   pred <- fpPoisson("event", "age", weights = NA, data = d0, pred.data = pred.data)
        # } else {
        #   pred <- fpPoisson("event", "age", weights = "weight", data = d0, pred.data = pred.data)
        # }
        pred <- fpPoisson("event", "age", weights = "py", data = d0, pred.data = pred.data)
        # (revise over)
      }

      oe <- data.frame(pred.data$age, pred*12)
      names(oe) <- c("age", "est.rates")
      oe[,2] <- round(oe[,2], 10)
    }

  }

  return(oe)
}

freq.raw <- function(data, nl, nh, evt, nWeight){

  # (revised) count in weights
  if (nWeight == 0){
    data$weight <- 1
  }
  data$py = data$py * data$weight
  # (revise over)

  d0 <- data
  d0 <- d0[which(d0$age <= nh),]

  if(nrow(d0) == 0){
    freq <- data.frame(age=seq(nl, nh, 1), raw.rates=NA)
  } else{
    t.py <- aggregate(d0$py, list(d0$age), sum)

    d0[which(d0$event != evt & d0$event != 0), "event"] <- 0
    d0[which(d0$event == evt), "event"] <- 1
    raw.event <- aggregate(d0$event* d0$weight, list(d0$age), sum)


    if(sum(raw.event$x, na.rm = T) == 0){
      freq <- data.frame(age=seq(nl, nh, 1), raw.rates=NA)
    } else{
      freq <- data.frame(raw.event$Group.1, raw.event$x/t.py$x)
      freq[,2] <- round(freq[,2], 10)
      names(freq) <- c("age", "raw.rates")}
  }

  return(freq)
}

freq.poi <- function(data, nl, nh, evt, nWeight, mfp){

  # (revised) count in weights
  if (nWeight == 0){
    data$weight <- 1
  }
  data$py = data$py * data$weight
  # (revise over)

  d0 <- data
  d0 <- d0[which(d0$age <= nh),]

  if(nrow(d0) == 0){
    freq <- data.frame(age=seq(nl, nh, 1), est.rates=NA)
  } else{
    t.py <- aggregate(d0$py, list(d0$age), sum)

    d0[which(d0$event != evt & d0$event != 0), "event"] <- 0
    d0[which(d0$event == evt), "event"] <- 1
    raw.event <- aggregate(d0$event* d0$weight, list(d0$age), sum)

    if(sum(raw.event$x, na.rm = T) == 0){
      freq <- data.frame(age=seq(nl, nh, 1), est.rates=NA)
    } else{

      if (mfp==T){
        model <- mfp(event ~ fp(age), data=d0, family=poisson)
        pred <- predict(model, type="response")
      } else {
        # (revised)
        # if (nWeight==0){
        #   pred <- fpPoisson("event", "age", weights = NA, data = d0, pred.data = pred.data)
        # } else {
        #   pred <- fpPoisson("event", "age", weights = "weight", data = d0, pred.data = pred.data)
        # }
        pred <- fpPoisson("event", "age", weights = "py", data = d0, pred.data = d0)
        # (revise over)
      }

      l <- data.frame(d0$age, pred)
      # poi.event <- aggregate(l$pred, list(l$d0.age), sum)
      poi.event <- aggregate(l$pred*d0$weight, list(l$d0.age), sum)

      freq <- data.frame(raw.event$Group.1, poi.event$x/t.py$x)
      freq[,2] <- round(freq[,2], 10)
      names(freq) <- c("age", "est.rates")}
  }

  return(freq)
}

freqM.raw <- function(data, nl, nh, status, evt, nWeight){

  # (revised) count in weights
  if (nWeight == 0){
    data$weight <- 1
  }
  data$py = data$py * data$weight
  # (revise over)

  d0 <- data

  if(nrow(d0) == 0){
    freq <- data.frame(age=seq(nl, nh, 1), raw.rates=NA)
  } else{
    t.py <- aggregate(d0$py, list(d0$age), sum)

    d0[which((d0$event != evt|d0$mar.bf != status) & d0$event != 0), "event"] <- 0
    d0[which(d0$event == evt & d0$mar.bf == status), "event"] <- 1
    raw.event <- aggregate(d0$event* d0$weight, list(d0$age), sum)

    if(sum(raw.event$x, na.rm = T) == 0){
      freq <- data.frame(age=seq(nl, nh, 1), raw.rates=NA)
    } else{
      freq <- data.frame(raw.event$Group.1, raw.event$x/t.py$x)
      freq[,2] <- round(freq[,2], 10)
      names(freq) <- c("age", "raw.rates")}
  }

  return(freq)
}

freqM.poi <- function(data, nl, nh, status, evt, nWeight, mfp){

  # (revised) count in weights
  if (nWeight == 0){
    data$weight <- 1
  }
  data$py = data$py * data$weight
  # (revise over)

  d0 <- data

  if(nrow(d0) == 0){
    freq <- data.frame(age=seq(nl, nh, 1), est.rates=NA)
  } else{
    t.py <- aggregate(d0$py, list(d0$age), sum)

    d0[which((d0$event != evt|d0$mar.bf != status) & d0$event != 0), "event"] <- 0
    d0[which(d0$event == evt & d0$mar.bf == status), "event"] <- 1
    raw.event <- aggregate(d0$event* d0$weight, list(d0$age), sum)

    pred.data<-as.data.frame(d0$age)
    colnames(pred.data) <- c("age")

    if(sum(raw.event$x, na.rm = T) == 0){
      freq <- data.frame(age=seq(nl, nh, 1), est.rates=NA)
    } else{

      if (mfp==T){
        model <- mfp(event ~ fp(age), data=d0, family=poisson)
        pred <- predict(model, type="response")
      } else {
        # (revised)
        # if (nWeight==0){
        #   pred <- fpPoisson("event", "age", weights = NA, data = d0, pred.data = pred.data)
        # } else {
        #   pred <- fpPoisson("event", "age", weights = "weight", data = d0, pred.data = pred.data)
        # }
        pred <- fpPoisson("event", "age", weights = "py", data = d0, pred.data = d0)
        # (revise over)
      }

      l <- data.frame(d0$age, pred)
      # poi.event <- aggregate(l$pred, list(l$d0.age), sum)
      poi.event <- aggregate(l$pred*d0$weight, list(l$d0.age), sum)

      freq <- data.frame(raw.event$Group.1, poi.event$x/t.py$x)
      freq[,2] <- round(freq[,2], 10)
      names(freq) <- c("age", "est.rates")}
  }

  return(freq)
}

oe.est.byvar <- function(data, nl, nh, status, evt, byvar, nWeight, mfp){

  # (revised) count in weights
  if (nWeight == 0){
    data$weight <- 1
  }
  data$py = data$py * data$weight
  # (revise over)

  #select risk populations to the event
  d0 <- subset(data, prev==status)
  d0 <- d0[which(d0$age <= nh),]

  if (length(unique(d0$sex))==1&substr(byvar, nchar(byvar)-5, nchar(byvar))!=".nosex"){
    byvar <- paste0(byvar, ".nosex", sep="")
  }

  if(nrow(d0) == 0){                 #if no risk population, leave empty
    oe <- data.frame(null.rates(data, nl, nh, byvar), raw.rates=NA, est.rates=NA)
  } else{

    #generate prediction data frame to store rates.
    pred.data <- prediction.data(d0, nl, nh, byvar)

    #select target events
    d0[which(d0$event != evt & d0$event != 0), "event"] <- 0
    d0[which(d0$event == evt), "event"] <- 1
    #select person-years of the risk populations
    #use post status to filter out those whose status changed after event happened
    dpy <- subset(data, post == status)

    age <- null.rates(d0, nl, nh, byvar)

    if (byvar=="sex") {
      names(age) <- c("Group.1", "Group.2")

      #calculate person years by age and sex
      t.py <- aggregate(dpy$py, list(dpy$age, dpy$sex), sum)
      t.py <- left_join(age, t.py, by = c("Group.1", "Group.2"))

      #calculate number of events by age and sex
      raw.event <- aggregate(d0$event * d0$weight, list(d0$age, d0$sex), sum)
      raw.event <- left_join(age, raw.event, by = c("Group.1", "Group.2"))

      if(sum(raw.event$x, na.rm = T) < 30){       #omit estimation if number of events < 30
        oe <- data.frame(null.rates(d0, nl, nh, byvar), raw.rates=NA, est.rates=NA)
      } else{

        if (mfp==T){            #if use mfp function
          model <- mfp(event ~ fp(age)+as.factor(sex), data=d0, family=poisson)
          pred <- predict(model, pred.data, type="response")
        } else {               #if use self-defined formula selection
          # (revised)
          # if (nWeight==0){
          #   pred <- fpPoisson("event", "age", "sex", weights = NA, data=d0, pred.data = pred.data)
          # } else {
          #   pred <- fpPoisson("event", "age", "sex", weights = "weight", data=d0, pred.data = pred.data)
          # }
          # the estimation of "event" is rate/month
          pred <- fpPoisson("event", "age", "sex", weights = "py", data=d0, pred.data = pred.data)
          # (revise over)
        }

        l <- data.frame(pred.data$age, pred.data$sex, pred*12)
        names(l) <- c("age", "sex", "est.rates")
        oe <- data.frame(raw.event$Group.1, raw.event$Group.2, raw.event$x/t.py$x)
        names(oe) <- c("age", "sex", "raw.rates")
        oe <- merge(oe, l, all=T) %>% arrange(age, sex)
      }
    }

    if (byvar=="sex.nosex") {
      names(age) <- c("Group.1")

      t.py <- aggregate(dpy$py, list(dpy$age), sum)
      t.py <- left_join(age, t.py, by = c("Group.1"))

      raw.event <- aggregate(d0$event * d0$weight, list(d0$age), sum)
      raw.event <- left_join(age, raw.event, by = c("Group.1"))

      if(sum(raw.event$x, na.rm = T) < 30){
        oe <- data.frame(null.rates(d0, nl, nh, byvar), raw.rates=NA, est.rates=NA)
      } else{

        if (mfp==T){
          model <- mfp(event ~ fp(age), data=d0, family=poisson)
          pred <- predict(model, pred.data, type="response")
        } else {
          # (revised)
          # if (nWeight==0){
          #   pred <- fpPoisson("event", "age", NA, weights = NA, data=d0, pred.data = pred.data)
          # } else {
          #   pred <- fpPoisson("event", "age", NA, weights = "weight", data=d0, pred.data = pred.data)
          # }
          pred <- fpPoisson("event", "age", NA, weights = "py", data=d0, pred.data = pred.data)
          # (revise over)
        }

        l <- data.frame(pred.data$age, pred*12)
        names(l) <- c("age", "est.rates")
        oe <- data.frame(raw.event$Group.1, raw.event$x/t.py$x)
        names(oe) <- c("age", "raw.rates")
        oe <- merge(oe, l, all=T) %>% arrange(age)
      }
    }

    if (byvar=="reg.ru") {

      names(age) <- c("Group.1", "Group.2", "Group.3", "Group.4")

      t.py <- aggregate(dpy$py, list(dpy$age, dpy$region, dpy$ru, dpy$sex), sum)
      t.py <- left_join(age, t.py, by = c("Group.1", "Group.2", "Group.3", "Group.4"))

      raw.event <- aggregate(d0$event * d0$weight, list(d0$age, d0$region, d0$ru, d0$sex), sum)
      raw.event <- left_join(age, raw.event, by = c("Group.1", "Group.2", "Group.3", "Group.4"))

      if(sum(raw.event$x, na.rm = T) < 30){
        oe <- data.frame(null.rates(d0, nl, nh, byvar), raw.rates=NA, est.rates=NA)
      } else{

        if (mfp==T){
          model <- mfp(event ~ fp(age)+as.factor(region)+as.factor(sex)+as.factor(ru), data=d0, family=poisson)
          pred <- predict(model, pred.data, type="response")
        } else {
          # (revised)
          # if (nWeight==0){
          #   pred <- fpPoisson("event", "age", "sex region ru", weights = NA, data=d0, pred.data = pred.data)
          # } else {
          #   pred <- fpPoisson("event", "age", "sex region ru", weights = "weight", data=d0, pred.data = pred.data)
          # }
          pred <- fpPoisson("event", "age", "sex region ru", weights = "py", data=d0, pred.data = pred.data)
          # (revise over)
        }

        l <- data.frame(pred.data$region, pred.data$age, pred.data$ru, pred.data$sex, pred*12)
        names(l) <- c("region", "age", "ru", "sex", "est.rates")
        oe <- data.frame(raw.event$Group.2, raw.event$Group.1, raw.event$Group.3, raw.event$Group.4, raw.event$x/t.py$x)
        names(oe) <- c("region", "age", "ru", "sex", "raw.rates")
        oe <- merge(oe, l, all=T) %>% arrange(age, sex, region, ru)
      }
    }

    if (byvar=="ru") {
      names(age) <- c("Group.1", "Group.2", "Group.3")
      t.py <- aggregate(dpy$py, list(dpy$age, dpy$ru, dpy$sex), sum)
      t.py <- left_join(age, t.py, by = c("Group.1", "Group.2", "Group.3"))
      raw.event <- aggregate(d0$event * d0$weight, list(d0$age, d0$ru, d0$sex), sum)
      raw.event <- left_join(age, raw.event, by = c("Group.1", "Group.2", "Group.3"))
      if(sum(raw.event$x, na.rm = T) < 30){
        oe <- data.frame(null.rates(d0, nl, nh, byvar), raw.rates=NA, est.rates=NA)
      } else{

        if (mfp==T){
          model <- mfp(event ~ fp(age)+as.factor(ru)+as.factor(sex), data=d0, family=poisson)
          pred <- predict(model, pred.data, type="response")
        } else {

          # if (nWeight==0){
          #   pred <- fpPoisson("event", "age", "sex ru", weights = NA, data=d0, pred.data = pred.data)
          # } else {
          #   pred <- fpPoisson("event", "age", "sex ru", weights = "weight", data=d0, pred.data = pred.data)
          # }
          pred <- fpPoisson("event", "age", "sex ru", weights = "py", data=d0, pred.data = pred.data)

        }

        l <- data.frame(pred.data$ru, pred.data$age, pred.data$sex, pred*12)
        names(l) <- c("ru", "age", "sex", "est.rates")
        oe <- data.frame(raw.event$Group.2, raw.event$Group.1, raw.event$Group.3, raw.event$x/t.py$x)
        names(oe) <- c("ru", "age", "sex", "raw.rates")
        oe <- merge(oe, l, all=T) %>% arrange(age, sex, ru)
      }
    }

    if (byvar=="region") {

      names(age) <- c("Group.1", "Group.2", "Group.3")

      t.py <- aggregate(dpy$py, list(dpy$age, dpy$region, dpy$sex), sum)
      t.py <- left_join(age, t.py, by = c("Group.1", "Group.2", "Group.3"))

      raw.event <- aggregate(d0$event * d0$weight, list(d0$age, d0$region, d0$sex), sum)
      raw.event <- left_join(age, raw.event, by = c("Group.1", "Group.2", "Group.3"))

      if(sum(raw.event$x, na.rm = T) < 30){
        oe <- data.frame(null.rates(d0, nl, nh, byvar), raw.rates=NA, est.rates=NA)
      } else{

        if (mfp==T){
          model <- mfp(event ~ fp(age)+as.factor(region)+as.factor(sex), data=d0, family=poisson)
          pred <- predict(model, pred.data, type="response")
        } else {
          # if (nWeight==0){
          #   pred <- fpPoisson("event", "age", "sex region", weights = NA, data=d0, pred.data = pred.data)
          # } else {
          #   pred <- fpPoisson("event", "age", "sex region", weights = "weight", data=d0, pred.data = pred.data)
          # }
          pred <- fpPoisson("event", "age", "sex region", weights = "py", data=d0, pred.data = pred.data)
        }

        l <- data.frame(pred.data$region, pred.data$age, pred.data$sex, pred*12)
        names(l) <- c("region", "age", "sex", "est.rates")
        oe <- data.frame(raw.event$Group.2, raw.event$Group.1, raw.event$Group.3, raw.event$x/t.py$x)
        names(oe) <- c("region", "age", "sex", "raw.rates")
        oe <- merge(oe, l, all=T) %>% arrange(age, sex, region)
      }
    }

    if (byvar=="region.nosex") {

      names(age) <- c("Group.1", "Group.2")

      t.py <- aggregate(dpy$py, list(dpy$age, dpy$region), sum)
      t.py <- left_join(age, t.py, by = c("Group.1", "Group.2"))

      raw.event <- aggregate(d0$event * d0$weight, list(d0$age, d0$region), sum)
      raw.event <- left_join(age, raw.event, by = c("Group.1", "Group.2"))

      if(sum(raw.event$x, na.rm = T) < 30){
        oe <- data.frame(null.rates(d0, nl, nh, byvar), raw.rates=NA, est.rates=NA)
      } else{

        if (mfp==T){
          model <- mfp(event ~ fp(age)+as.factor(region), data=d0, family=poisson)
          pred <- predict(model, pred.data, type="response")
        } else {
          # if (nWeight==0){
          #   pred <- fpPoisson("event", "age", "region", weights = NA, data=d0, pred.data = pred.data)
          # } else {
          #   pred <- fpPoisson("event", "age", "region", weights = "weight", data=d0, pred.data = pred.data)
          # }
          pred <- fpPoisson("event", "age", "region", weights = "py", data=d0, pred.data = pred.data)
        }

        l <- data.frame(pred.data$region, pred.data$age, pred*12)
        names(l) <- c("region", "age", "est.rates")
        oe <- data.frame(raw.event$Group.2, raw.event$Group.1, raw.event$x/t.py$x)
        names(oe) <- c("region", "age", "raw.rates")
        oe <- merge(oe, l, all=T) %>% arrange(age, region)
      }
    }

    if (byvar=="ru.nosex") {

      names(age) <- c("Group.1", "Group.2")

      t.py <- aggregate(dpy$py, list(dpy$age, dpy$ru), sum)
      t.py <- left_join(age, t.py, by = c("Group.1", "Group.2"))

      raw.event <- aggregate(d0$event * d0$weight, list(d0$age, d0$ru), sum)
      raw.event <- left_join(age, raw.event, by = c("Group.1", "Group.2"))

      if(sum(raw.event$x, na.rm = T) < 30){
        oe <- data.frame(null.rates(d0, nl, nh, byvar), raw.rates=NA, est.rates=NA)
      } else{

        if (mfp==T){
          model <- mfp(event ~ fp(age)+as.factor(ru), data=d0, family=poisson)
          pred <- predict(model, pred.data, type="response")
        } else {
          # if (nWeight==0){
          #   pred <- fpPoisson("event", "age", "ru", weights = NA, data=d0, pred.data = pred.data)
          # } else {
          #   pred <- fpPoisson("event", "age", "ru", weights = "weight", data=d0, pred.data = pred.data)
          # }
          pred <- fpPoisson("event", "age", "ru", weights = "py", data=d0, pred.data = pred.data)
        }

        l <- data.frame(pred.data$ru, pred.data$age, pred*12)
        names(l) <- c("ru", "age", "est.rates")
        oe <- data.frame(raw.event$Group.2, raw.event$Group.1, raw.event$x/t.py$x)
        names(oe) <- c("ru", "age", "raw.rates")
        oe <- merge(oe, l, all=T) %>% arrange(age, ru)
      }
    }

    if (byvar=="race") {

      names(age) <- c("Group.1", "Group.2", "Group.3")

      t.py <- aggregate(dpy$py, list(dpy$age, dpy$race, dpy$sex), sum)
      t.py <- left_join(age, t.py, by = c("Group.1", "Group.2", "Group.3"))

      raw.event <- aggregate(d0$event * d0$weight, list(d0$age, d0$race, d0$sex), sum)
      raw.event <- left_join(age, raw.event, by = c("Group.1", "Group.2", "Group.3"))

      if(sum(raw.event$x, na.rm = T) < 30){
        oe <- data.frame(null.rates(d0, nl, nh, byvar), raw.rates=NA, est.rates=NA)
      } else{

        if (mfp==T){
          model <- mfp(event ~ fp(age)+as.factor(race)+as.factor(sex), data=d0, family=poisson)
          pred <- predict(model, pred.data, type="response")
        } else {
          # if (nWeight==0){
          #   pred <- fpPoisson("event", "age", "race sex", weights = NA, data=d0, pred.data = pred.data)
          # } else {
          #   pred <- fpPoisson("event", "age", "race sex", weights = "weight", data=d0, pred.data = pred.data)
          # }
          pred <- fpPoisson("event", "age", "race sex", weights = "py", data=d0, pred.data = pred.data)

        }

        l <- data.frame(pred.data$race, pred.data$age, pred.data$sex, pred*12)
        names(l) <- c("race", "age", "sex", "est.rates")
        oe <- data.frame(raw.event$Group.2, raw.event$Group.1, raw.event$Group.3, raw.event$x/t.py$x)
        names(oe) <- c("race", "age", "sex", "raw.rates")
        oe <- merge(oe, l, all=T) %>% arrange(age, sex, race)
      }
    }

    if (byvar=="race.nosex") {

      names(age) <- c("Group.1", "Group.2")

      t.py <- aggregate(dpy$py, list(dpy$age, dpy$race), sum)
      t.py <- left_join(age, t.py, by = c("Group.1", "Group.2"))

      raw.event <- aggregate(d0$event * d0$weight, list(d0$age, d0$race), sum)
      raw.event <- left_join(age, raw.event, by = c("Group.1", "Group.2"))

      if(sum(raw.event$x, na.rm = T) < 30){
        oe <- data.frame(null.rates(d0, nl, nh, byvar), raw.rates=NA, est.rates=NA)
      } else{

        if (mfp==T){
          model <- mfp(event ~ fp(age)+as.factor(race), data=d0, family=poisson)
          pred <- predict(model, pred.data, type="response")
        } else {
          # if (nWeight==0){
          #   pred <- fpPoisson("event", "age", "race", weights = NA, data=d0, pred.data = pred.data)
          # } else {
          #   pred <- fpPoisson("event", "age", "race", weights = "weight", data=d0, pred.data = pred.data)
          # }
          pred <- fpPoisson("event", "age", "race", weights = "py", data=d0, pred.data = pred.data)
        }

        l <- data.frame(pred.data$race, pred.data$age, pred*12)
        names(l) <- c("race", "age", "est.rates")
        oe <- data.frame(raw.event$Group.2, raw.event$Group.1, raw.event$x/t.py$x)
        names(oe) <- c("race", "age", "raw.rates")
        oe <- merge(oe, l, all=T) %>% arrange(age, race)
      }
    }

    if (byvar=="edu") {

      names(age) <- c("Group.1", "Group.2", "Group.3")

      t.py <- aggregate(dpy$py, list(dpy$age, dpy$edu, dpy$sex), sum)
      t.py <- left_join(age, t.py, by = c("Group.1", "Group.2", "Group.3"))

      raw.event <- aggregate(d0$event * d0$weight, list(d0$age, d0$edu, d0$sex), sum)
      raw.event <- left_join(age, raw.event, by = c("Group.1", "Group.2", "Group.3"))

      if(sum(raw.event$x, na.rm = T) < 30){
        oe <- data.frame(null.rates(d0, nl, nh, byvar), raw.rates=NA, est.rates=NA)
      } else{

        if (mfp==T){
          model <- mfp(event ~ fp(age)+as.factor(edu)+as.factor(sex), data=d0, family=poisson)
          pred <- predict(model, pred.data, type="response")
        } else {
          # if (nWeight==0){
          #   pred <- fpPoisson("event", "age", "edu sex", weights = NA, data=d0, pred.data = pred.data)
          # } else {
          #   pred <- fpPoisson("event", "age", "edu sex", weights = "weight", data=d0, pred.data = pred.data)
          # }
          pred <- fpPoisson("event", "age", "edu sex", weights = "py", data=d0, pred.data = pred.data)

        }

        l <- data.frame(pred.data$edu, pred.data$age, pred.data$sex, pred*12)
        names(l) <- c("edu", "age", "sex", "est.rates")
        oe <- data.frame(raw.event$Group.2, raw.event$Group.1, raw.event$Group.3, raw.event$x/t.py$x)
        names(oe) <- c("edu", "age", "sex", "raw.rates")
        oe <- merge(oe, l, all=T) %>% arrange(age, sex, edu)
      }
    }

    if (byvar=="with_p") {

      names(age) <- c("Group.1", "Group.2", "Group.3")

      t.py <- aggregate(dpy$py, list(dpy$age, dpy$with_p, dpy$sex), sum)
      t.py <- left_join(age, t.py, by = c("Group.1", "Group.2", "Group.3"))

      raw.event <- aggregate(d0$event * d0$weight, list(d0$age, d0$with_p, d0$sex), sum)
      raw.event <- left_join(age, raw.event, by = c("Group.1", "Group.2", "Group.3"))

      if(sum(raw.event$x, na.rm = T) < 30){
        oe <- data.frame(null.rates(d0, nl, nh, byvar), raw.rates=NA, est.rates=NA)
      } else{

        if (mfp==T){
          model <- mfp(event ~ fp(age)+as.factor(with_p)+as.factor(sex), data=d0, family=poisson)
          pred <- predict(model, pred.data, type="response")
        } else {
          # if (nWeight==0){
          #   pred <- fpPoisson("event", "age", "with_p sex", weights = NA, data=d0, pred.data = pred.data)
          # } else {
          #   pred <- fpPoisson("event", "age", "with_p sex", weights = "weight", data=d0, pred.data = pred.data)
          # }
          pred <- fpPoisson("event", "age", "with_p sex", weights = "py", data=d0, pred.data = pred.data)

        }

        l <- data.frame(pred.data$with_p, pred.data$age, pred.data$sex, pred*12)
        names(l) <- c("with_p", "age", "sex", "est.rates")
        oe <- data.frame(raw.event$Group.2, raw.event$Group.1, raw.event$Group.3, raw.event$x/t.py$x)
        names(oe) <- c("with_p", "age", "sex", "raw.rates")
        oe <- merge(oe, l, all=T) %>% arrange(age, sex, with_p)
      }
    }

    if (byvar=="with_c") {

      names(age) <- c("Group.1", "Group.2", "Group.3")

      t.py <- aggregate(dpy$py, list(dpy$age, dpy$with_c, dpy$sex), sum)
      t.py <- left_join(age, t.py, by = c("Group.1", "Group.2", "Group.3"))

      raw.event <- aggregate(d0$event * d0$weight, list(d0$age, d0$with_c, d0$sex), sum)
      raw.event <- left_join(age, raw.event, by = c("Group.1", "Group.2", "Group.3"))

      if(sum(raw.event$x, na.rm = T) < 30){
        oe <- data.frame(null.rates(d0, nl, nh, byvar), raw.rates=NA, est.rates=NA)
      } else{

        if (mfp==T){
          model <- mfp(event ~ fp(age)+as.factor(with_c)+as.factor(sex), data=d0, family=poisson)
          pred <- predict(model, pred.data, type="response")
        } else {
          # if (nWeight==0){
          #   pred <- fpPoisson("event", "age", "with_c sex", weights = NA, data=d0, pred.data = pred.data)
          # } else {
          #   pred <- fpPoisson("event", "age", "with_c sex", weights = "weight", data=d0, pred.data = pred.data)
          # }
          pred <- fpPoisson("event", "age", "with_c sex", weights = "py", data=d0, pred.data = pred.data)

        }

        l <- data.frame(pred.data$with_c, pred.data$age, pred.data$sex, pred*12)
        names(l) <- c("with_c", "age", "sex", "est.rates")
        oe <- data.frame(raw.event$Group.2, raw.event$Group.1, raw.event$Group.3, raw.event$x/t.py$x)
        names(oe) <- c("with_c", "age", "sex", "raw.rates")
        oe <- merge(oe, l, all=T) %>% arrange(age, sex, with_c)
      }
    }

    if (byvar=="marital") {

      names(age) <- c("Group.1", "Group.2", "Group.3")

      t.py <- aggregate(dpy$py, list(dpy$age, dpy$mar, dpy$sex), sum)
      t.py <- left_join(age, t.py, by = c("Group.1", "Group.2", "Group.3"))

      raw.event <- aggregate(d0$event * d0$weight, list(d0$age, d0$mar, d0$sex), sum)
      raw.event <- left_join(age, raw.event, by = c("Group.1", "Group.2", "Group.3"))

      if(sum(raw.event$x, na.rm = T) < 30){
        oe <- data.frame(null.rates(d0, nl, nh, byvar), raw.rates=NA, est.rates=NA)
      } else{

        if (mfp==T){
          model <- mfp(event ~ fp(age)+as.factor(mar)+as.factor(sex), data=d0, family=poisson)
          pred <- predict(model, pred.data, type="response")
        } else {
          # if (nWeight==0){
          #   pred <- fpPoisson("event", "age", "mar sex", weights = NA, data=d0, pred.data = pred.data)
          # } else {
          #   pred <- fpPoisson("event", "age", "mar sex", weights = "weight", data=d0, pred.data = pred.data)
          # }
          pred <- fpPoisson("event", "age", "mar sex", weights = "py", data=d0, pred.data = pred.data)
        }

        l <- data.frame(pred.data$mar, pred.data$age, pred.data$sex, pred*12)
        names(l) <- c("mar", "age", "sex", "est.rates")
        oe <- data.frame(raw.event$Group.2, raw.event$Group.1, raw.event$Group.3, raw.event$x/t.py$x)
        names(oe) <- c("mar", "age", "sex", "raw.rates")
        oe <- merge(oe, l, all=T) %>% arrange(age, sex, mar)
      }
    }

    if (byvar=="reg.ru.nosex"){

      names(age) <- c("Group.1", "Group.2", "Group.3")

      t.py <- aggregate(dpy$py, list(dpy$age, dpy$region, dpy$ru), sum)
      t.py <- left_join(age, t.py, by = c("Group.1", "Group.2", "Group.3"))

      raw.event <- aggregate(d0$event * d0$weight, list(d0$age, d0$region, d0$ru), sum)
      raw.event <- left_join(age, raw.event, by = c("Group.1", "Group.2", "Group.3"))

      if(sum(raw.event$x, na.rm = T) < 30){
        oe <- data.frame(null.rates(d0, nl, nh, byvar), raw.rates=NA, est.rates=NA)
      } else{

        if (mfp==T){
          model <- mfp(event ~ fp(age)+as.factor(region)+as.factor(ru), data=d0, family=poisson)
          pred <- predict(model, pred.data, type="response")
        } else {
          # if (nWeight==0){
          #   pred <- fpPoisson("event", "age", "region ru", weights = NA, data=d0, pred.data = pred.data)
          # } else {
          #   pred <- fpPoisson("event", "age", "region ru", weights = "weight", data=d0, pred.data = pred.data)
          # }
          pred <- fpPoisson("event", "age", "region ru", weights = "py", data=d0, pred.data = pred.data)
        }

        l <- data.frame(pred.data$region, pred.data$age, pred.data$ru, pred*12)
        names(l) <- c("region", "age", "ru", "est.rates")
        oe <- data.frame(raw.event$Group.2, raw.event$Group.1, raw.event$Group.3, raw.event$x/t.py$x)
        names(oe) <- c("region", "age", "ru", "raw.rates")
        oe <- merge(oe, l, all=T) %>% arrange(age, region, ru)
      }
    }

    if (byvar=="mar.ru") {

      names(age) <- c("Group.1", "Group.2", "Group.3", "Group.4")

      t.py <- aggregate(dpy$py, list(dpy$age, dpy$mar, dpy$ru, dpy$sex), sum)
      t.py <- left_join(age, t.py, by = c("Group.1", "Group.2", "Group.3", "Group.4"))

      raw.event <- aggregate(d0$event * d0$weight, list(d0$age, d0$mar, d0$ru, d0$sex), sum)
      raw.event <- left_join(age, raw.event, by = c("Group.1", "Group.2", "Group.3", "Group.4"))

      if(sum(raw.event$x, na.rm = T) < 30){
        oe <- data.frame(null.rates(d0, nl, nh, byvar), raw.rates=NA, est.rates=NA)
      } else{

        if (mfp==T){
          model <- mfp(event ~ fp(age)+as.factor(mar)+as.factor(sex)+as.factor(ru), data=d0, family=poisson)
          pred <- predict(model, pred.data, type="response")
        } else {
          # if (nWeight==0){
          #   pred <- fpPoisson("event", "age", "mar sex ru", weights = NA, data=d0, pred.data = pred.data)
          # } else {
          #   pred <- fpPoisson("event", "age", "mar sex ru", weights = "weight", data=d0, pred.data = pred.data)
          # }
          pred <- fpPoisson("event", "age", "mar sex ru", weights = "py", data=d0, pred.data = pred.data)
        }

        l <- data.frame(pred.data$mar, pred.data$age, pred.data$ru, pred.data$sex, pred*12)
        names(l) <- c("mar", "age", "ru", "sex", "est.rates")
        oe <- data.frame(raw.event$Group.2, raw.event$Group.1, raw.event$Group.3, raw.event$Group.4, raw.event$x/t.py$x)
        names(oe) <- c("mar", "age", "ru", "sex", "raw.rates")
        oe <- merge(oe, l, all=T) %>% arrange(age, sex, mar, ru)
      }
    }

  }

  return(oe)
}

freq.est.byvar <- function(data, nl, nh, evt, byvar, nWeight, mfp){

  # (revised)count in weights
  if (nWeight == 0){
    data$weight <- 1
  }
  data$py = data$py * data$weight
  # (revise over)

  d0 <- data
  d0 <- d0[which(d0$age <= nh),]

  #check gender
  if (length(unique(d0$sex))==1&substr(byvar, nchar(byvar)-5, nchar(byvar))!=".nosex"){
    byvar <- paste0(byvar, ".nosex", sep="")
  }

  if(nrow(d0) == 0){
    freq <- data.frame(null.rates(data, nl, nh, byvar), raw.rates=NA, est.rates=NA)
  } else{

    d0[which(d0$event != evt & d0$event != 0), "event"] <- 0
    d0[which(d0$event == evt), "event"] <- 1

    if (byvar=="sex"){
      t.py <- aggregate(d0$py, list(d0$age, d0$sex), sum)
      raw.event <- aggregate(d0$event * d0$weight, list(d0$age, d0$sex), sum)
      if(sum(raw.event$x, na.rm = T) == 0){
        freq <- data.frame(null.rates(d0, nl, nh, byvar), raw.rates=NA, est.rates=NA)
      } else{

        if (mfp==T){
          model <- mfp(event ~ fp(age)+as.factor(sex), data=d0, family=poisson)
          pred <- predict(model, data.frame(age=d0$age, sex=as.factor(d0$sex)), type="response")
        } else {
          # if (nWeight==0){
          #   pred <- fpPoisson("event", "age", "sex", weights = NA, data=d0, pred.data = d0)
          # } else {
          #   pred <- fpPoisson("event", "age", "sex", weights = "weight", data=d0, pred.data = d0)
          # }
          pred <- fpPoisson("event", "age", "sex", weights = "py", data=d0, pred.data = d0)
        }

        l <- data.frame(d0$age, d0$sex, pred)
        # poi.event <- aggregate(l$pred, list(l$d0.age, l$d0.sex), sum)
        poi.event <- aggregate(l$pred*d0$weight, list(l$d0.age, l$d0.sex), sum)
        freq <- data.frame(raw.event$Group.1, raw.event$Group.2, raw.event$x/t.py$x, poi.event$x/t.py$x)
        names(freq) <- c("age", "sex", "raw.rates", "est.rates")
        freq <- freq %>% arrange(age, sex)
      }
    }

    if (byvar=="sex.nosex"){
      t.py <- aggregate(d0$py, list(d0$age), sum)
      raw.event <- aggregate(d0$event * d0$weight, list(d0$age), sum)
      if(sum(raw.event$x, na.rm = T) == 0){
        freq <- data.frame(null.rates(d0, nl, nh, byvar), raw.rates=NA, est.rates=NA)
      } else{

        if (mfp==T){
          model <- mfp(event ~ fp(age), data=d0, family=poisson)
          pred <- predict(model,data.frame(age=d0$age),type="response")
        } else {
          # if (nWeight==0){
          #   pred <- fpPoisson("event", "age", NA, weights = NA, data=d0, pred.data = d0)
          # } else {
          #   pred <- fpPoisson("event", "age", NA, weights = "weight", data=d0, pred.data = d0)
          # }
          pred <- fpPoisson("event", "age", NA, weights = "py", data=d0, pred.data = d0)
        }

        l <- data.frame(d0$age, pred)
        poi.event <- aggregate(l$pred*d0$weight, list(l$d0.age), sum)
        freq <- data.frame(raw.event$Group.1, raw.event$x/t.py$x, poi.event$x/t.py$x)
        names(freq) <- c("age", "raw.rates", "est.rates")
        freq <- freq %>% arrange(age)
      }
    }

    if (byvar=="reg.ru"){
      t.py <- aggregate(d0$py, list(d0$age, d0$region, d0$ru, d0$sex), sum)
      raw.event <- aggregate(d0$event * d0$weight, list(d0$age, d0$region, d0$ru, d0$sex), sum)
      if(sum(raw.event$x, na.rm = T) == 0){
        freq <- data.frame(null.rates(d0, nl, nh, byvar), raw.rates=NA, est.rates=NA)
      } else{

        if (mfp==T){
          model <- mfp(event ~ fp(age)+as.factor(region)+as.factor(ru)+as.factor(sex), data=d0, family=poisson)
          pred <- predict(model,data.frame(age=d0$age, region=as.factor(d0$region), ru=as.factor(d0$ru), sex=as.factor(d0$sex)),type="response")
        } else {
          # if (nWeight==0){
          #   pred <- fpPoisson("event", "age", "region ru sex", weights = NA, data=d0, pred.data = d0)
          # } else {
          #   pred <- fpPoisson("event", "age", "region ru sex", weights = "weight", data=d0, pred.data = d0)
          # }
          pred <- fpPoisson("event", "age", "region ru sex", weights = "py", data=d0, pred.data = d0)
        }

        l <- data.frame(d0$age, d0$region, d0$ru, d0$sex, pred)
        poi.event <- aggregate(l$pred*d0$weight, list(l$d0.age, l$d0.region, l$d0.ru, l$d0.sex), sum)
        freq <- data.frame(raw.event$Group.2, raw.event$Group.1, raw.event$Group.3, raw.event$Group.4, raw.event$x/t.py$x, poi.event$x/t.py$x)
        names(freq) <- c("region", "age", "ru", "sex", "raw.rates", "est.rates")
        freq <- freq %>% arrange(age, sex, region, ru)
      }
    }

    if (byvar=="reg.ru.nosex"){
      t.py <- aggregate(d0$py, list(d0$age, d0$region, d0$ru), sum)
      raw.event <- aggregate(d0$event * d0$weight, list(d0$age, d0$region, d0$ru), sum)
      if(sum(raw.event$x, na.rm = T) == 0){
        freq <- data.frame(null.rates(d0, nl, nh, byvar), raw.rates=NA, est.rates=NA)
      } else{

        if (mfp==T){
          model <- mfp(event ~ fp(age)+as.factor(region)+as.factor(ru), data=d0, family=poisson)
          pred <- predict(model,data.frame(age=d0$age, region=as.factor(d0$region), ru=as.factor(d0$ru)),type="response")
        } else {
          # if (nWeight==0){
          #   pred <- fpPoisson("event", "age", "region ru", weights = NA, data=d0, pred.data = d0)
          # } else {
          #   pred <- fpPoisson("event", "age", "region ru", weights = "weight", data=d0, pred.data = d0)
          # }
          pred <- fpPoisson("event", "age", "region ru", weights = "py", data=d0, pred.data = d0)
        }

        l <- data.frame(d0$age, d0$region, d0$ru, pred)
        poi.event <- aggregate(l$pred*d0$weight, list(l$d0.age, l$d0.region, l$d0.ru), sum)
        freq <- data.frame(raw.event$Group.2, raw.event$Group.1, raw.event$Group.3, raw.event$x/t.py$x, poi.event$x/t.py$x)
        names(freq) <- c("region", "age", "ru", "raw.rates", "est.rates")
        freq <- freq %>% arrange(age, region, ru)
      }
    }

    if (byvar=="ru"){
      t.py <- aggregate(d0$py, list(d0$age, d0$ru, d0$sex), sum)
      raw.event <- aggregate(d0$event * d0$weight, list(d0$age, d0$ru, d0$sex), sum)
      if(sum(raw.event$x, na.rm = T) == 0){
        freq <- data.frame(null.rates(d0, nl, nh, byvar), raw.rates=NA, est.rates=NA)
      } else{

        if (mfp==T){
          model <- mfp(event ~ fp(age)+as.factor(ru)+as.factor(sex), data=d0, family=poisson)
          pred <- predict(model,data.frame(age=d0$age, ru=as.factor(d0$ru), sex=as.factor(d0$sex)), type="response")
        } else {
          # if (nWeight==0){
          #   pred <- fpPoisson("event", "age", "sex ru", weights = NA, data=d0, pred.data = d0)
          # } else {
          #   pred <- fpPoisson("event", "age", "sex ru", weights = "weight", data=d0, pred.data = d0)
          # }
          pred <- fpPoisson("event", "age", "sex ru", weights = "py", data=d0, pred.data = d0)
        }

        l <- data.frame(d0$age, d0$ru, d0$sex, pred)
        poi.event <- aggregate(l$pred*d0$weight, list(l$d0.age, l$d0.ru, l$d0.sex), sum)
        freq <- data.frame(raw.event$Group.2, raw.event$Group.1, raw.event$Group.3, raw.event$x/t.py$x, poi.event$x/t.py$x)
        names(freq) <- c("ru", "age", "sex", "raw.rates", "est.rates")
        freq <- freq %>% arrange(age, sex, ru)
      }
    }

    if (byvar=="region"){
      t.py <- aggregate(d0$py, list(d0$age, d0$region, d0$sex), sum)
      raw.event <- aggregate(d0$event * d0$weight, list(d0$age, d0$region, d0$sex), sum)
      if(sum(raw.event$x, na.rm = T) == 0){
        freq <- data.frame(null.rates(d0, nl, nh, byvar), raw.rates=NA, est.rates=NA)
      } else{

        if (mfp==T){
          model <- mfp(event ~ fp(age)+as.factor(region)+as.factor(sex), data=d0, family=poisson)
          pred <- predict(model,data.frame(age=d0$age, region=as.factor(d0$region), sex=as.factor(d0$sex)),type="response")
        } else {
          # if (nWeight==0){
          #   pred <- fpPoisson("event", "age", "sex region", weights = NA, data=d0, pred.data = d0)
          # } else {
          #   pred <- fpPoisson("event", "age", "sex region", weights = "weight", data=d0, pred.data = d0)
          # }
          pred <- fpPoisson("event", "age", "sex region", weights = "py", data=d0, pred.data = d0)
        }

        l <- data.frame(d0$age, d0$region, d0$sex, pred)
        poi.event <- aggregate(l$pred*d0$weight, list(l$d0.age, l$d0.region, l$d0.sex), sum)
        freq <- data.frame(raw.event$Group.2, raw.event$Group.1, raw.event$Group.3, raw.event$x/t.py$x, poi.event$x/t.py$x)
        names(freq) <- c("region", "age", "sex", "raw.rates", "est.rates")
        freq <- freq %>% arrange(age, sex, region)
      }
    }

    if (byvar=="region.nosex"){
      t.py <- aggregate(d0$py, list(d0$age, d0$region), sum)
      raw.event <- aggregate(d0$event * d0$weight, list(d0$age, d0$region), sum)
      if(sum(raw.event$x, na.rm = T) == 0){
        freq <- data.frame(null.rates(d0, nl, nh, byvar), raw.rates=NA, est.rates=NA)
      } else{

        if (mfp==T){
          model <- mfp(event ~ fp(age)+as.factor(region), data=d0, family=poisson)
          pred <- predict(model,data.frame(age=d0$age, region=as.factor(d0$region)),type="response")
        } else {
          # if (nWeight==0){
          #   pred <- fpPoisson("event", "age", "region", weights = NA, data=d0, pred.data = d0)
          # } else {
          #   pred <- fpPoisson("event", "age", "region", weights = "weight", data=d0, pred.data = d0)
          # }
          pred <- fpPoisson("event", "age", "region", weights = "py", data=d0, pred.data = d0)
        }

        l <- data.frame(d0$age, d0$region, pred)
        poi.event <- aggregate(l$pred*d0$weight, list(l$d0.age, l$d0.region), sum)
        freq <- data.frame(raw.event$Group.2, raw.event$Group.1, raw.event$x/t.py$x, poi.event$x/t.py$x)
        names(freq) <- c("region", "age", "raw.rates", "est.rates")
        freq <- freq %>% arrange(age, region)
      }
    }

    if (byvar=="ru.nosex"){
      t.py <- aggregate(d0$py, list(d0$age, d0$ru), sum)
      raw.event <- aggregate(d0$event * d0$weight, list(d0$age, d0$ru), sum)
      if(sum(raw.event$x, na.rm = T) == 0){
        freq <- data.frame(null.rates(d0, nl, nh, byvar), raw.rates=NA, est.rates=NA)
      } else{

        if (mfp==T){
          model <- mfp(event ~ fp(age)+as.factor(ru), data=d0, family=poisson)
          pred <- predict(model,data.frame(age=d0$age, ru=as.factor(d0$ru)),type="response")
        } else {
          # if (nWeight==0){
          #   pred <- fpPoisson("event", "age", "ru", weights = NA, data=d0, pred.data = d0)
          # } else {
          #   pred <- fpPoisson("event", "age", "ru", weights = "weight", data=d0, pred.data = d0)
          # }
          pred <- fpPoisson("event", "age", "ru", weights = "py", data=d0, pred.data = d0)
        }

        l <- data.frame(d0$age, d0$ru, pred)
        poi.event <- aggregate(l$pred*d0$weight, list(l$d0.age, l$d0.ru), sum)
        freq <- data.frame(raw.event$Group.2, raw.event$Group.1, raw.event$x/t.py$x, poi.event$x/t.py$x)
        names(freq) <- c("ru", "age", "raw.rates", "est.rates")
        freq <- freq %>% arrange(age, ru)
      }
    }

    if (byvar=="race.nosex"){
      t.py <- aggregate(d0$py, list(d0$age, d0$race), sum)
      raw.event <- aggregate(d0$event * d0$weight, list(d0$age, d0$race), sum)
      if(sum(raw.event$x, na.rm = T) == 0){
        freq <- data.frame(null.rates(d0, nl, nh, byvar), raw.rates=NA, est.rates=NA)
      } else{

        if (mfp==T){
          model <- mfp(event ~ fp(age)+as.factor(race), data=d0, family=poisson)
          pred <- predict(model,data.frame(age=d0$age, race=as.factor(d0$race)),type="response")
        } else {
          # if (nWeight==0){
          #   pred <- fpPoisson("event", "age", "race", weights = NA, data=d0, pred.data = d0)
          # } else {
          #   pred <- fpPoisson("event", "age", "race", weights = "weight", data=d0, pred.data = d0)
          # }
          pred <- fpPoisson("event", "age", "race", weights = "py", data=d0, pred.data = d0)
        }

        l <- data.frame(d0$age, d0$race, pred)
        poi.event <- aggregate(l$pred*d0$weight, list(l$d0.age, l$d0.race), sum)
        freq <- data.frame(raw.event$Group.2, raw.event$Group.1, raw.event$x/t.py$x, poi.event$x/t.py$x)
        names(freq) <- c("race", "age", "raw.rates", "est.rates")
        freq <- freq %>% arrange(age, race)
      }
    }

    if (byvar=="race"){
      t.py <- aggregate(d0$py, list(d0$age, d0$race, d0$sex), sum)
      raw.event <- aggregate(d0$event * d0$weight, list(d0$age, d0$race, d0$sex), sum)
      if(sum(raw.event$x, na.rm = T) == 0){
        freq <- data.frame(null.rates(d0, nl, nh, byvar), raw.rates=NA, est.rates=NA)
      } else{

        if (mfp==T){
          model <- mfp(event ~ fp(age)+as.factor(race)+as.factor(sex), data=d0, family=poisson)
          pred <- predict(model,data.frame(age=d0$age,race=as.factor(d0$race), sex=as.factor(d0$sex)),type="response")
        } else {
          # if (nWeight==0){
          #   pred <- fpPoisson("event", "age", "race sex", weights = NA, data=d0, pred.data = d0)
          # } else {
          #   pred <- fpPoisson("event", "age", "race sex", weights = "weight", data=d0, pred.data = d0)
          # }
          pred <- fpPoisson("event", "age", "race sex", weights = "py", data=d0, pred.data = d0)
        }

        l <- data.frame(d0$age, d0$race, d0$sex, pred)
        poi.event <- aggregate(l$pred*d0$weight, list(l$d0.age, l$d0.race, l$d0.sex), sum)
        freq <- data.frame(raw.event$Group.2, raw.event$Group.1, raw.event$Group.3, raw.event$x/t.py$x, poi.event$x/t.py$x)
        names(freq) <- c("race", "age", "sex", "raw.rates", "est.rates")
        freq <- freq %>% arrange(age, sex, race)
      }
    }

    if (byvar=="edu"){
      t.py <- aggregate(d0$py, list(d0$age, d0$edu, d0$sex), sum)
      raw.event <- aggregate(d0$event * d0$weight, list(d0$age, d0$edu, d0$sex), sum)
      if(sum(raw.event$x, na.rm = T) == 0){
        freq <- data.frame(null.rates(d0, nl, nh, byvar), raw.rates=NA, est.rates=NA)
      } else{

        if (mfp==T){
          model <- mfp(event ~ fp(age)+as.factor(edu)+as.factor(sex), data=d0, family=poisson)
          pred <- predict(model,data.frame(age=d0$age, edu=as.factor(d0$edu), sex=as.factor(d0$sex)),type="response")
        } else {
          # if (nWeight==0){
          #   pred <- fpPoisson("event", "age", "edu sex", weights = NA, data=d0, pred.data = d0)
          # } else {
          #   pred <- fpPoisson("event", "age", "edu sex", weights = "weight", data=d0, pred.data = d0)
          # }
          pred <- fpPoisson("event", "age", "edu sex", weights = "py", data=d0, pred.data = d0)
        }

        l <- data.frame(d0$age, d0$edu, d0$sex, pred)
        poi.event <- aggregate(l$pred*d0$weight, list(l$d0.age, l$d0.edu, l$d0.sex), sum)
        freq <- data.frame(raw.event$Group.2, raw.event$Group.1, raw.event$Group.3, raw.event$x/t.py$x, poi.event$x/t.py$x)
        names(freq) <- c("edu", "age", "sex", "raw.rates", "est.rates")
        freq <- freq %>% arrange(age, sex, edu)
      }
    }

    if (byvar=="with_p"){
      t.py <- aggregate(d0$py, list(d0$age, d0$with_p, d0$sex), sum)
      raw.event <- aggregate(d0$event * d0$weight, list(d0$age, d0$with_p, d0$sex), sum)
      if(sum(raw.event$x, na.rm = T) == 0){
        freq <- data.frame(null.rates(d0, nl, nh, byvar), raw.rates=NA, est.rates=NA)
      } else{

        if (mfp==T){
          model <- mfp(event ~ fp(age)+as.factor(with_p)+as.factor(sex), data=d0, family=poisson)
          pred <- predict(model,data.frame(age=d0$age,with_p=as.factor(d0$with_p), sex=as.factor(d0$sex)),type="response")
        } else {
          # if (nWeight==0){
          #   pred <- fpPoisson("event", "age", "with_p sex", weights = NA, data=d0, pred.data = d0)
          # } else {
          #   pred <- fpPoisson("event", "age", "with_p sex", weights = "weight", data=d0, pred.data = d0)
          # }
          pred <- fpPoisson("event", "age", "with_p sex", weights = "py", data=d0, pred.data = d0)
        }

        l <- data.frame(d0$age, d0$with_p, d0$sex, pred)
        poi.event <- aggregate(l$pred*d0$weight, list(l$d0.age, l$d0.with_p, l$d0.sex), sum)
        freq <- data.frame(raw.event$Group.2, raw.event$Group.1, raw.event$Group.3, raw.event$x/t.py$x, poi.event$x/t.py$x)
        names(freq) <- c("with_p", "age", "sex", "raw.rates", "est.rates")
        freq <- freq %>% arrange(age, sex, with_p)
      }
    }

    if (byvar=="with_c"){
      t.py <- aggregate(d0$py, list(d0$age, d0$with_c, d0$sex), sum)
      raw.event <- aggregate(d0$event * d0$weight, list(d0$age, d0$with_c, d0$sex), sum)
      if(sum(raw.event$x, na.rm = T) == 0){
        freq <- data.frame(null.rates(d0, nl, nh, byvar), raw.rates=NA, est.rates=NA)
      } else{

        if (mfp==T){
          model <- mfp(event ~ fp(age)+as.factor(with_c)+as.factor(sex), data=d0, family=poisson)
          pred <- predict(model,data.frame(age=d0$age, with_c=as.factor(d0$with_c), sex=as.factor(d0$sex)),type="response")
        } else {
          # if (nWeight==0){
          #   pred <- fpPoisson("event", "age", "with_c sex", weights = NA, data=d0, pred.data = d0)
          # } else {
          #   pred <- fpPoisson("event", "age", "with_c sex", weights = "weight", data=d0, pred.data = d0)
          # }
          pred <- fpPoisson("event", "age", "with_c sex", weights = "py", data=d0, pred.data = d0)
        }

        l <- data.frame(d0$age, d0$with_c, d0$sex, pred)
        poi.event <- aggregate(l$pred*d0$weight, list(l$d0.age, l$d0.with_c, l$d0.sex), sum)
        freq <- data.frame(raw.event$Group.2, raw.event$Group.1, raw.event$Group.3, raw.event$x/t.py$x, poi.event$x/t.py$x)
        names(freq) <- c("with_c", "age", "sex", "raw.rates", "est.rates")
        freq <- freq %>% arrange(age, sex, with_c)
      }
    }

    if (byvar=="marital"){
      t.py <- aggregate(d0$py, list(d0$age, d0$mar, d0$sex), sum)
      raw.event <- aggregate(d0$event * d0$weight, list(d0$age, d0$mar, d0$sex), sum)
      if(sum(raw.event$x, na.rm = T) == 0){
        freq <- data.frame(null.rates(d0, nl, nh, byvar), raw.rates=NA, est.rates=NA)
      } else{

        if (mfp==T){
          model <- mfp(event ~ fp(age)+as.factor(mar)+as.factor(sex), data=d0, family=poisson)
          pred <- predict(model,data.frame(age=d0$age, mar=as.factor(d0$mar), sex=as.factor(d0$sex)),type="response")
        } else {
          # if (nWeight==0){
          #   pred <- fpPoisson("event", "age", "mar sex", weights = NA, data=d0, pred.data = d0)
          # } else {
          #   pred <- fpPoisson("event", "age", "mar sex", weights = "weight", data=d0, pred.data = d0)
          # }
          pred <- fpPoisson("event", "age", "mar sex", weights = "py", data=d0, pred.data = d0)
        }

        l <- data.frame(d0$age, d0$mar, d0$sex, pred)
        poi.event <- aggregate(l$pred*d0$weight, list(l$d0.age, l$d0.mar, l$d0.sex), sum)
        freq <- data.frame(raw.event$Group.2, raw.event$Group.1, raw.event$Group.3, raw.event$x/t.py$x, poi.event$x/t.py$x)
        names(freq) <- c("mar", "age", "sex", "raw.rates", "est.rates")
        freq <- freq %>% arrange(age, sex, mar)
      }
    }

    if (byvar=="mar.ru"){
      t.py <- aggregate(d0$py, list(d0$age, d0$mar, d0$ru, d0$sex), sum)
      raw.event <- aggregate(d0$event * d0$weight, list(d0$age, d0$mar, d0$ru, d0$sex), sum)
      if(sum(raw.event$x, na.rm = T) == 0){
        freq <- data.frame(null.rates(d0, nl, nh, byvar), raw.rates=NA, est.rates=NA)
      } else{

        if (mfp==T){
          model <- mfp(event ~ fp(age)+as.factor(mar)+as.factor(ru)+as.factor(sex), data=d0, family=poisson)
          pred <- predict(model,data.frame(age=d0$age, mar=as.factor(d0$mar), ru=as.factor(d0$ru), sex=as.factor(d0$sex)),type="response")
        } else {
          # if (nWeight==0){
          #   pred <- fpPoisson("event", "age", "mar ru sex", weights = NA, data=d0, pred.data = d0)
          # } else {
          #   pred <- fpPoisson("event", "age", "mar ru sex", weights = "weight", data=d0, pred.data = d0)
          # }
          pred <- fpPoisson("event", "age", "mar ru sex", weights = "py", data=d0, pred.data = d0)
        }

        l <- data.frame(d0$age, d0$mar, d0$ru, d0$sex, pred)
        poi.event <- aggregate(l$pred*d0$weight, list(l$d0.age, l$d0.mar, l$d0.ru, l$d0.sex), sum)
        freq <- data.frame(raw.event$Group.2, raw.event$Group.1, raw.event$Group.3, raw.event$Group.4, raw.event$x/t.py$x, poi.event$x/t.py$x)
        names(freq) <- c("mar", "age", "ru", "sex", "raw.rates", "est.rates")
        freq <- freq %>% arrange(age, sex, mar, ru)
      }
    }

  }

  return(freq)
}

total.freq <- function(freq){

  total.freq <- as.data.frame(t(tmp.colSum.w.na(freq[2:ncol(freq)])))

  return(total.freq)
}

mean.age.cal <- function(freq){

  mean.age <- as.data.frame(t(colSums(freq[ ,2:ncol(freq)]*(freq$age+0.5), na.rm = T)/
                                ifelse(colSums(freq[,2:ncol(freq)], na.rm = T)==0,
                                       NA_real_, colSums(freq[,2:ncol(freq)], na.rm = T))))

  return(mean.age)
}

rates.plot <- function(rates, nl, nh, y.name="", plot.title=""){
  lwd <- c(0.9, 1.2)
  col <- c("#80B1D3","#FB8072")
  if (colSums(rates, na.rm = T)[3]==0) {
    ggplot(data = rates) +
      labs(x="Age", y=y.name, title=plot.title) +
      theme(legend.title=element_blank(), legend.position="bottom", plot.title=element_text(hjust=0.5, size=10),
            axis.text.x=element_text(size=9), axis.text.y=element_text(size=9), axis.title=element_text(size=10),
            panel.background=element_rect(fill="white", color="grey")) +
      xlim(nl, nh)
  } else {
    if (y.name=="o/e rate"){
      ggplot(data = rates) +
        geom_line(aes(x = rates[,1], y = rates[,2], color="Direct calculate"), lwd=lwd[1]) +
        geom_line(aes(x = rates[,1], y = rates[,3], color="Poisson estimate"), lwd=lwd[2]) +
        labs(x="Age", y=y.name, title=plot.title) +
        scale_colour_manual(values = c("Direct calculate"=col[1], "Poisson estimate"=col[2])) +
        theme(legend.title=element_blank(), legend.position="bottom", plot.title=element_text(hjust=0.5, size=10),
              axis.text.x=element_text(size=9), axis.text.y=element_text(size=9), axis.title=element_text(size=10),
              panel.background=element_rect(fill="white", color="grey")) +
        scale_x_continuous(breaks=seq(nl, nh, by=5))
    } else if (y.name=="frequency") {
      ggplot(data = rates) +
        geom_line(aes(x = rates[,1], y = rates[,2], color="Direct calculate"), lwd=lwd[1]) +
        geom_line(aes(x = rates[,1], y = rates[,3], color="Poisson estimate"), lwd=lwd[2]) +
        labs(x="Age", y=y.name, title=plot.title) +
        scale_colour_manual(values = c("Direct calculate"=col[1], "Poisson estimate"=col[2])) +
        theme(legend.title=element_blank(), legend.position="bottom", plot.title=element_text(hjust=0.5, size=10),
              axis.text.x=element_text(size=9), axis.text.y=element_text(size=9), axis.title=element_text(size=10),
              panel.background=element_rect(fill="white", color="grey")) +
        scale_x_continuous(breaks=seq(nl, nh, by=5))
    }
  }
}

arrange.covar <- function(data, byvar){
  if (byvar=="sex"){
    data <- data %>% arrange(age, sex)
  } else if (byvar=="reg.ru") {
    data <- data %>% arrange(age, sex, region, ru)
  } else if (byvar=="ru") {
    data <- data %>% arrange(age, sex, ru)
  } else if (byvar=="race"){
    data <- data %>% arrange(age, sex, race)
  } else if (byvar=="region"){
    data <- data %>% arrange(age, sex, region)
  } else if (byvar=="edu"){
    data <- data %>% arrange(age, sex, edu)
  } else if (byvar=="reg.ru.nosex"){
    data <- data %>% arrange(age, region, ru)
  } else if (byvar=="region.nosex"){
    data <- data %>% arrange(age, region)
  } else if (byvar=="ru.nosex"){
    data <- data %>% arrange(age, ru)
  } else if (byvar=="sex.nosex"){
    data <- data %>% arrange(age)
  } else if (byvar=="with_p"){
    data <- data %>% arrange(age, sex, with_p)
  } else if (byvar=="with_c"){
    data <- data %>% arrange(age, sex, with_c)
  } else if (byvar=="marital"){
    data <- data %>% arrange(age, sex, mar)
  } else if (byvar=="mar.ru"){
    data <- data %>% arrange(age, sex, mar, ru)
  }

  return(data)
}

tmp.rowSum.w.na <- function(d){
  tmp_all <- rowSums(d, na.rm = T)
  tmp_all <- ifelse(rowSums(is.na(d)) == length(d), NA, tmp_all)
  return(tmp_all)
}

tmp.colSum.w.na <- function(d){
  tmp_all <- colSums(d, na.rm = T)
  tmp_all <- ifelse(colSums(is.na(d)) == nrow(d), NA, tmp_all)
  return(tmp_all)
}


##------Fitting-------
fpPoisson <- function(depvar, fpvar, byvar = NA, weights = NA, data, pred.data){
  # 分数多项式-泊松回归
  #  Args:
  #   depvar (character): 因变量，event
  #   fpvar (character): 需要做fp的连续型自变量，age
  #   byvar (character): 其余不做处理的自变量，可以为多个，每个变量名间用空格分隔，sex ru
  #   weights (character): glm函数调用的权重，汇总后的人月py
  #   data (dataframe): 汇总的人月数据d0_group
  #   pred.data (dataframe): 预测所需要的自变量
  #  Returns:
  #   pred (vector): 对应拟合结果
  # pred <- fpPoisson("event", "age", "sex", weights = "weight", data=d0, pred.data = pred.data)
  # depvar="event"; fpvar="age";byvar="sex";weights = "py";data=d0;

  #检查数据中是否有所需变量
  depin <- depvar %in% names(data)
  fpin <- fpvar %in% names(data)
  weightin <- is.na(weights) || weights %in% names(data)

  #判断合法则分情况拟合模型
  if(depin && fpin && weightin){
    #没有协变量
    if(is.na(byvar)){
      if(is.na(weights)){
        #若不加入权重，直接调用selectfp
        fpmodel <- select_fp(Y = eval(parse(text = paste0("data$", depvar))),
                             X = eval(parse(text = paste0("data$", fpvar))))
      }else{
        #若加入权重，先汇总数据
        data_group <- aggregate(eval(parse(text = paste0("data$", weights))),
                                list(eval(parse(text = paste0("data$", fpvar))),
                                     eval(parse(text = paste0("data$", depvar)))),
                                sum)
        colnames(data_group) = c(fpvar, depvar, weights)
        data_group <- arrange(data_group, fpvar, depvar)
        #对汇总后数据调用selectfp
        fpmodel <- select_fp(Y = eval(parse(text = paste0("data_group$", depvar))),
                             X = eval(parse(text = paste0("data_group$", fpvar))),
                             weight = eval(parse(text = paste0("data_group$", weights))))
      }
    }else{#存在协变量
      byvars <- strsplit(byvar, " ")[[1]] #识别byvar中的字符串
      #检查数据集中是否有所需协变量
      if(all(byvars %in% names(data) == T)){
        bydata <- subset(data, select=byvars)
        bydata[] <- lapply(bydata, factor)

        if(is.na(weights)){
          #若不加入权重，直接调用selectfp
          fpmodel <- select_fp(Y = eval(parse(text = paste0("data$", depvar))),
                               X = eval(parse(text = paste0("data$", fpvar))),
                               byvar = bydata)
        }else{
          #若加入权重，先汇总数据
          datalist <- as.list(bydata)
          datalist[[fpvar]] <- eval(parse(text = paste0("data$", fpvar)))
          datalist[[depvar]] <- eval(parse(text = paste0("data$", depvar)))
          data_group <- aggregate(eval(parse(text = paste0("data$", weights))),
                                  datalist, sum)
          data_group <- arrange(data_group, fpvar, depvar)
          bydata_group <- subset(data_group, select=byvars)
          bydata_group[] <- lapply(bydata_group, factor)

          #对汇总后数据调用selectfp
          fpmodel <- select_fp(Y = eval(parse(text = paste0("data_group$", depvar))),
                               X = eval(parse(text = paste0("data_group$", fpvar))),
                               byvar = bydata_group,
                               weight = data_group$x)
        }
      }else{
        print("Non-existent covariable") #报错
      }
    }
  }else{
    print("Non-existent mainvariable") #报错
  }

  #预测
  ##准备预测数据
  pred <- c()

  pred.fp <- eval(parse(text = paste0("pred.data$", fpvar)))
  pred.fp <- pred.fp / fpmodel$scale
  pow <- c(-2,-1,-0.5,0,0.5,1,2,3)
  k <- 17; rem <- list()
  for (i in 1:7){
    for (j in (i+1):8){
      rem[[k]] <- c(i,j)
      k <- k+1
    }
  }

  ## 协变量因子化
  if(length(byvars) > 1){
    bydata <- select(pred.data, byvars)
    pred.data0 <- select(pred.data, -byvars)
    bydata[] <- lapply(bydata, factor)
    pred.data <- c(pred.data0, bydata)
  }

  m <- fpmodel$power
  if(m>=1 && m<=8){
    # pred.data$'fp...i.' <- pred.fp^pow[m]
    pred.data$'fp1' <- pred.fp^pow[m]
    pred <- predict(fpmodel$model, pred.data, type="response")
  }else if(m>=9 && m<=16){
    # pred.data$'fp...i.' <- pred.fp^pow[m-8]
    pred.data$'fp1' <- pred.fp^pow[m-8]
    # pred.data$'fp_log...i.' <- pred.fp^pow[m-8]*log(pred.fp)
    pred.data$'fp2' <- pred.fp^pow[m-8]*log(pred.fp)
    pred <- predict(fpmodel$model, pred.data, type="response")
  }else{
    # pred.data$'fp...i.' <- pred.fp^pow[rem[[m]]][1]
    # pred.data$'fp1' <- pred.fp^pow[rem[[m]]][1]
    if (pow[rem[[m]]][1] == 0){
      pred.data$'fp1' = log(pred.fp)
    }else{
      pred.data$'fp1' <- pred.fp^pow[rem[[m]]][1]
    }
    # pred.data$'fp...j.' <- pred.fp^pow[rem[[m]]][2]
    if (pow[rem[[m]]][2] == pow[rem[[m]]][1]){
      pred.data$'fp2' <- pred.fp^pow[rem[[m]]][1] * log(pred.fp)
    }else if(pow[rem[[m]]][2] == 0){
      pred.data$'fp2' <- log(pred.fp)
    }else{
      pred.data$'fp2' <- pred.fp^pow[rem[[m]]][2]
    }
    # pred.data$'fp2' <- pred.fp^pow[rem[[m]]][2]
    pred <- predict(fpmodel$model, pred.data, type="response")
  }

  return(pred)
}

# Y = eval(parse(text = paste0("data_group$", depvar)));
# X = eval(parse(text = paste0("data_group$", fpvar)));
# weight = eval(parse(text = paste0("data_group$", weights)));
# byvar = data.frame(rep(NA, length(X)));

select_fp <-function(Y = depvar, X = fpvar, byvar = data.frame(rep(NA, length(X))), weight = NA) {

  range <- mean(X)
  scale <- 10^(sign(log10(range)) * round(abs(log10(range))))
  X <- X / scale
  ##Compute the powers of fpvar

  fp <-
    data.frame(X ^ (-2), X ^ (-1), X ^ (-0.5), log(X), X ^ 0.5, X, X ^ 2, X ^ 3)
  fp_log <- data.frame(
    X ^ (-2) * log(X),
    X ^ (-1) * log(X),
    X ^ (-0.5) * log(X),
    log(X) * log(X),
    X ^ 0.5 * log(X),
    X * log(X),
    X ^ 2 * log(X),
    X ^ 3 * log(X)
  )

  X_all <-
    list() #The list of all variables(including depvar,fpvar and byvar)
  rem <- matrix(nrow = 28, ncol = 2)

  ##The list of results
  result <- list()
  result_dev <- c()

  ##Treat byvar as categorical
  byvar[] <- lapply(byvar, factor)

  ##Model fitting
  if (length(weight) == 1) {
    ###Unary(8 types)
    for (i in 1:8) {
      temp <- data.frame(cbind(Y, fp[, i], byvar[,!apply(is.na(byvar), 2, any)]))
      if(dim(as.data.frame(byvar[,!apply(is.na(byvar), 2, any)]))[2]==0){colnames(byvar)=NULL}
      colnames(temp) <- c('Y', 'fp1', colnames(byvar))
      X_all[[i]] <- temp
      # if (is.na(byvar)){X_all[[i]] <- data.frame(cbind(Y, fp[, i]))}
      # else {X_all[[i]] <- data.frame(cbind(Y, fp[, i], byvar))}
      result[[i]] <-
        glm(
          Y ~ .,
          family = poisson(link = log),
          data = c(X_all[[i]]),
          na.action = 'na.exclude',
        )
      result_dev[i] <- result[[i]]$dev
    }

    ###Binary with same powers(8 types)
    for (i in 1:8) {
      temp <- data.frame(cbind(Y, fp[, i], fp_log[, i], byvar[,!apply(is.na(byvar), 2, any)]))
      if(dim(as.data.frame(byvar[,!apply(is.na(byvar), 2, any)]))[2]==0){colnames(byvar)=NULL}
      colnames(temp) <- c('Y', 'fp1', 'fp2', colnames(byvar))
      X_all[[i+8]] <- temp
      # if (is.na(byvar)){X_all[[i + 8]] <- data.frame(cbind(Y, fp[, i], fp_log[, i]))}
      # else {X_all[[i + 8]] <- data.frame(cbind(Y, fp[, i], fp_log[, i], byvar))}
      result[[i + 8]] <-
        glm(
          Y ~ .,
          family = poisson(link = log),
          data = c(X_all[[i + 8]]),
          na.action = 'na.exclude',
        )
      result_dev[i + 8] <- result[[i + 8]]$dev
    }

    ###Binary with different powers(28 types)
    k <- 17
    for (i in 1:7) {
      for (j in (i + 1):8) {
        temp <- data.frame(cbind(Y, fp[, i], fp[, j], byvar[,!apply(is.na(byvar), 2, any)]))
        if(dim(as.data.frame(byvar[,!apply(is.na(byvar), 2, any)]))[2]==0){colnames(byvar)=NULL}
        colnames(temp) <- c('Y', 'fp1', 'fp2', colnames(byvar))
        X_all[[k]] <- temp
        # X_all[[k]] <- data.frame(cbind(Y, fp[, i], byvar[,!apply(is.na(byvar), 2, any)]))
        # if (is.na(byvar)){X_all[[k]] <- data.frame(cbind(Y, fp[, i], fp[, j]))}
        # else {X_all[[k]] <- data.frame(cbind(Y, fp[, i], fp[, j], byvar))}

        result[[k]] <-
          glm(
            Y ~ .,
            family = poisson(link = log),
            data = c(X_all[[k]]),
            na.action = 'na.exclude',
          )
        result_dev[k] <- result[[k]]$dev
        rem[(k - 16), 1] <- i
        rem[(k - 16), 2] <- j
        k <- k + 1
      }
    }
  }
  else{
    ###Unary(8 types)
    for (i in 1:8) {
      temp <- data.frame(cbind(Y, fp[, i], byvar[,!apply(is.na(byvar), 2, any)]))
      if(dim(as.data.frame(byvar[,!apply(is.na(byvar), 2, any)]))[2]==0){colnames(byvar)=NULL}
      colnames(temp) <- c('Y', 'fp1', colnames(byvar))
      X_all[[i]] <- temp
      # if (all(is.na(byvar))){X_all[[i]] <- data.frame(cbind(Y, fp[, i]))}
      # else {X_all[[i]] <- data.frame(cbind(Y, fp[, i], byvar))}
      result[[i]] <-
        glm(
          Y ~ .,
          family = poisson(link = log),
          data = c(X_all[[i]]),
          weights = weight,
          na.action = 'na.exclude',
        )
      result_dev[i] <- result[[i]]$dev
    }

    ###Binary with same powers(8 types)
    for (i in 1:8) {
      temp <- data.frame(cbind(Y, fp[, i], fp_log[, i], byvar[,!apply(is.na(byvar), 2, any)]))
      if(dim(as.data.frame(byvar[,!apply(is.na(byvar), 2, any)]))[2]==0){colnames(byvar)=NULL}
      colnames(temp) <- c('Y', 'fp1', 'fp2', colnames(byvar))
      X_all[[i+8]] <- temp
      # if (is.na(byvar)){X_all[[i + 8]] <- data.frame(cbind(Y, fp[, i], fp_log[, i]))}
      # else {X_all[[i + 8]] <- data.frame(cbind(Y, fp[, i], fp_log[, i], byvar))}
      result[[i + 8]] <-
        glm(
          Y ~ .,
          family = poisson(link = log),
          data = c(X_all[[i + 8]]),
          weights = weight,
          na.action = 'na.exclude',
        )
      result_dev[i + 8] <- result[[i + 8]]$dev
    }

    ###Binary with different powers(28 types)
    k <- 17
    for (i in 1:7) {
      for (j in (i + 1):8) {
        temp <- data.frame(cbind(Y, fp[, i], fp[, j], byvar[,!apply(is.na(byvar), 2, any)]))
        if(dim(as.data.frame(byvar[,!apply(is.na(byvar), 2, any)]))[2]==0){colnames(byvar)=NULL}
        colnames(temp) <- c('Y', 'fp1', 'fp2', colnames(byvar))
        X_all[[k]] <- temp
        # if (is.na(byvar)){X_all[[k]] <- data.frame(cbind(Y, fp[, i], fp[, j]))}
        # else {X_all[[k]] <- data.frame(cbind(Y, fp[, i], fp[, j], byvar))}
        result[[k]] <-
          glm(
            Y ~ .,
            family = poisson(link = log),
            data = c(X_all[[k]]),
            weights = weight,
            na.action = 'na.exclude',
          )
        result_dev[k] <- result[[k]]$dev
        rem[(k - 16), 1] <- i
        rem[(k - 16), 2] <- j
        k <- k + 1
      }
    }
  }

  ##Choose the model with least dev
  m <- which.min(result_dev)
  # pow <- c(-2,-1,-0.5, 0, 0.5, 1, 2, 3)
  # print("The power(s) of the fpvar is/are: ")
  # res <- case_when((m >= 1 && m <= 8) ~ pow[m],
  #                  (m >= 9 &&
  #                     m <= 16) ~ c(pow[m - 8], pow[m - 8]),
  #                  (m >= 17 &&
  #                     m <= 44) ~ c(pow[rem[abs(m - 16), 1]], pow[rem[abs(m - 16), 2]]))
  # print(res)

  fpmodel <- list()
  fpmodel$power <- m
  fpmodel$model <- result[[m]]
  fpmodel$scale <- scale

  ##Return the model
  return(fpmodel)
}
