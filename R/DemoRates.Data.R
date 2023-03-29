#This script saves all the function needed for DemoRates data cleansing.

##------Error&Correction-------
correction.ym <- function(data, dob, dobm, intw, eventm, dup){
  deleted <- data[F,]
  imputed <- data[F,]
  corrected <- data[F,]

  if (dup==TRUE & sum(data$err12, na.rm = TRUE)>0){
    deleted1 <- data %>% filter(!is.na(err12))
    data <- data %>% filter(is.na(err12))
    deleted <- rbind(deleted, deleted1)
    rm(deleted1)
  }

  if (intw==TRUE & sum(data$err1, na.rm = TRUE)>0 | sum(data$err10, na.rm = TRUE)>0) {
    data <- data %>% mutate(t.time = tYear*12+tMonth)
    max.y <- max(data$t.time-1, na.rm = TRUE)%/%12
    max.m <- max(data$t.time-1, na.rm = TRUE)%%12
    data <- data %>% select(-t.time)
    imputed1 <- data %>% filter(!is.na(err1))
    corrected <- data %>% filter(!is.na(err10))
    data$tYear <- ifelse(is.na(data$err1)&is.na(data$err10), data$tYear, max.y)
    data$tMonth <- ifelse(is.na(data$err1)&is.na(data$err10), data$tMonth, max.m)
    imputed <- rbind(imputed, imputed1)
    rm(imputed1)
  }

  if (eventm==TRUE & sum(data$err3, na.rm = TRUE)>0) {
    #if event month missing but event year not missing
    imputed1 <- data %>% filter(is.na(m)&!is.na(y))
    set.seed(2021)
    randm <- sample(1:12, nrow(imputed1), replace = T)
    data[which(is.na(data$m)&!is.na(data$y)), "m"] <- randm
    imputed <- rbind(imputed, imputed1)
    rm(imputed1)
  }

  if (sum(data$err2, na.rm = TRUE)>0 | sum(data$err11, na.rm = TRUE)>0) {
    if(dob==TRUE & dobm==TRUE){
      #if birthday month is missing/not valid but birthday year is not: set bMonth as 6
      imputed1 <- data %>% filter(is.na(bMonth)&bYear>=1000&!is.na(bYear))
      set.seed(2022)
      randm1 <- sample(1:12, nrow(imputed1), replace = T)

      corrected1 <- data %>% filter((bMonth<0|bMonth>12)&bYear>=1000&!is.na(bYear))
      set.seed(2023)
      randm2 <- sample(1:12, nrow(corrected1), replace = T)

      data[which(is.na(data$bMonth)&data$bYear>=1000&!is.na(data$bYear)), "bMonth"] <- randm1
      data[which((data$bMonth<0|data$bMonth>12)&data$bYear>=1000&!is.na(data$bYear)), "bMonth"] <- randm2
      imputed <- rbind(imputed, imputed1)
      rm(imputed1)
      #if birthday year is missing/not valid: delete
      deleted1 <- data %>% filter(is.na(bYear)|bYear<1000)
      data <- data %>% filter(!is.na(bYear)&bYear>=1000)
      deleted <- rbind(deleted, deleted1)
      rm(deleted1)
    } else if (dob==TRUE & dobm==FALSE) {
      #if birthday year or month is missing or not valid: delete
      deleted1 <- data %>% filter(!(is.na(err2)&is.na(err11)))
      data <- data %>% filter(is.na(err2)&is.na(err11))
      deleted <- rbind(deleted, deleted1)
      rm(deleted1)
    } else if (dob==FALSE & dobm==TRUE) {
      #if birthday month is missing/not valid but birthday year is not: set bMonth as 6
      imputed1 <- data %>% filter(is.na(bMonth)&bYear>=1000&!is.na(bYear))
      corrected1 <- data %>% filter((bMonth<0|bMonth>12)&bYear>=1000&!is.na(bYear))
      set.seed(2022)
      randm1 <- sample(1:12, nrow(imputed1), replace = T)
      set.seed(2023)
      randm2 <- sample(1:12, nrow(corrected1), replace = T)
      data[which(is.na(data$bMonth)&data$bYear>=1000&!is.na(data$bYear)), "bMonth"] <- randm1
      data[which((data$bMonth<0|data$bMonth>12)&data$bYear>=1000&!is.na(data$bYear)), "bMonth"] <- randm2
      imputed <- rbind(imputed, imputed1)
      rm(imputed1)
    }
  }

  corr.sum <- data.frame(modification = c("Deletion", "Imputation", "Correction"),
                         count = c(nrow(deleted), nrow(imputed), nrow(corrected)))

  imputed <- imputed %>% unite("error.type", err1:err12, sep = ", ", remove = FALSE, na.rm = TRUE) %>%
    select(1, 2, error.type) %>%
    pivot_wider(names_from = c(event_index), values_from = c(error.type), names_sep = "")
  if (nrow(imputed)!=0) {
    names(imputed)[2:ncol(imputed)] <- paste("event", names(imputed)[2:ncol(imputed)], sep = "")
  }

  deleted <- deleted %>% unite("error.type", err1:err12, sep = ", ", remove = FALSE, na.rm = TRUE) %>%
    select(1, 2, error.type) %>%
    pivot_wider(names_from = c(event_index), values_from = c(error.type), names_sep = "")
  if (nrow(deleted)!=0) {
    names(deleted)[2:ncol(deleted)] <- paste("event", names(deleted)[2:ncol(deleted)], sep = "")
  }

  corrected <- corrected %>% unite("error.type", err1:err12, sep = ", ", remove = FALSE, na.rm = TRUE) %>%
    select(1, 2, error.type) %>%
    pivot_wider(names_from = c(event_index), values_from = c(error.type), names_sep = "")
  if (nrow(corrected)!=0) {
    names(corrected)[2:ncol(corrected)] <- paste("event", names(corrected)[2:ncol(corrected)], sep = "")
  }

  return(list(data = data, deleted = deleted, imputed = imputed, corrected = corrected,
              summary = corr.sum))
}

correction.cmc <- function(data, dob, intw, eventm, dup){
  deleted <- data[F,]
  imputed <- data[F,]
  corrected <- data[F,]

  if (dup==TRUE & sum(data$err12, na.rm = TRUE)>0){
    deleted1 <- data %>% filter(!is.na(err12))
    data <- data %>% filter(is.na(err12))
    deleted <- rbind(deleted, deleted1)
    rm(deleted1)
  }

  if (intw==TRUE & sum(data$err1, na.rm = TRUE)>0 | sum(data$err10, na.rm = TRUE)>0) {
    imputed1 <- data %>%
      mutate(tMonth = replace(tMonth, which(!is.na(err1)), min(tMonth, na.rm = T))) %>%
      filter(!is.na(err1))
    corrected <- data %>%
      mutate(tMonth = replace(tMonth, which(!is.na(err10)), min(tMonth, na.rm = T))) %>%
      filter(!is.na(err10))
    data$tMonth <- ifelse(is.na(data$err1)&is.na(data$err10), data$tMonth, min(data$tMonth, na.rm = T))
    imputed <- rbind(imputed, imputed1)
    rm(imputed1)
  }

  if (sum(data$err2, na.rm = TRUE)>0 | sum(data$err11, na.rm = TRUE)>0) {
    sys.year <- as.integer(format(Sys.Date(), "%Y"))
    sys.mon <- as.integer(format(Sys.Date(), "%m"))
    sys.cmc <- (sys.year-1900)*12+sys.mon

    if(dob==TRUE){
      deleted1 <- data %>% filter(!is.na(err2)|!is.na(err11))
      data <- data %>% filter(is.na(err2)&is.na(err11))
      deleted <- rbind(deleted, deleted1)
      rm(deleted1)
    }
  }

  corr.sum <- data.frame(modification = c("Deletion", "Imputation", "Correction"),
                         count = c(nrow(deleted), nrow(imputed), nrow(corrected)))

  imputed <- imputed %>% unite("error.type", err1:err12, sep = ", ", remove = FALSE, na.rm = TRUE) %>%
    select(1, 2, error.type) %>%
    pivot_wider(names_from = c(event_index), values_from = c(error.type), names_sep = "")
  if (nrow(imputed)!=0) {
    names(imputed)[2:ncol(imputed)] <- paste("event", names(imputed)[2:ncol(imputed)], sep = "")
  }

  deleted <- deleted %>% unite("error.type", err1:err12, sep = ", ", remove = FALSE, na.rm = TRUE) %>%
    select(1, 2, error.type) %>%
    pivot_wider(names_from = c(event_index), values_from = c(error.type), names_sep = "")
  if (nrow(deleted)!=0) {
    names(deleted)[2:ncol(deleted)] <- paste("event", names(deleted)[2:ncol(deleted)], sep = "")
  }

  corrected <- corrected %>% unite("error.type", err1:err12, sep = ", ", remove = FALSE, na.rm = TRUE) %>%
    select(1, 2, error.type) %>%
    pivot_wider(names_from = c(event_index), values_from = c(error.type), names_sep = "")
  if (nrow(corrected)!=0) {
    names(corrected)[2:ncol(corrected)] <- paste("event", names(corrected)[2:ncol(corrected)], sep = "")
  }

  return(list(data = data, deleted = deleted, imputed = imputed, corrected = corrected,
              summary = corr.sum))
}

define.descrip <- function(mode) {
  if (mode=="mar"|mode=="general"|mode=="lh"|mode=="mig"|mode=="defined"){
    description <- c("Missing Interview time",
                     "Missing Birthday",
                     "Interview time is not valid",
                     "Birthday is not valid",
                     "Missing Event time",
                     "Missing Event Type",
                     "Event time not logical",
                     "Event type is not logical",
                     "Event type not valid",
                     "Event Year not valid",
                     "Event Month not valid",
                     "Duplicate Event")
  } else if (mode=="fert") {
    description <- c("Missing Interview time",
                     "Missing Birthday",
                     "Interview time is not valid",
                     "Birthday is not valid",
                     "Missing Event time",
                     "Missing Event Type",
                     "Event time not logical",
                     "Different parities within 8 months",
                     "Event type not valid",
                     "Event Year not valid",
                     "Event Month not valid",
                     "Gender is not valid")
  } else if (mode=="fertM") {
    description <- c("Missing Interview time",
                     "Missing Birthday",
                     "Interview time is not valid",
                     "Birthday is not valid",
                     "Missing Event time",
                     "Missing Event Type",
                     "Event time not logical",
                     "Event type is not logical/Different parities within 8 months",
                     "Event type not valid",
                     "Event Year not valid",
                     "Event Month not valid",
                     "Duplicate Event/Gender is not valid")
  }

  return(description)
}

error.summary <- function(data, description){
  data <- data %>% unite("error.type", err1:err12, sep = ", ", remove = FALSE, na.rm = TRUE)
  details <- data %>% select(ID, event_index, error.type) %>%
    pivot_wider(names_from = c(event_index), values_from = c(error.type), names_sep = "")
  names(details)[2:ncol(details)] <- paste("event", names(details)[2:ncol(details)],
                                           sep = "")

  summary <- data.frame(c(colSums(!is.na(data[,c("err1","err2","err10","err11")])&!duplicated(data[,c("ID","err1","err2","err10","err11")])),
                          colSums(!is.na(data[,c("err3","err4","err5","err6","err7","err8","err9","err12")]))))
  names(summary) <- "count"
  summary$error.code <- rownames(summary)
  summary$Description <- description
  summary <- summary %>% select(error.code, Description, count)

  return(list(err.details = details, err.sum = summary))
}

write.error.report <- function(error, name){
  forDatstl <- createStyle(halign="center", border="TopBottomLeftRight")
  forDatstl2 <- createStyle(halign="left", border="TopBottomLeftRight")
  forDatstl3 <- createStyle(halign="right", border="TopBottomLeftRight")
  forDatstl4 <- createStyle(textDecoration = "bold")
  wb <- createWorkbook()
  addWorksheet(wb, "List of Tables")
  addWorksheet(wb, "Error Summary")
  addWorksheet(wb, "Error Details")

  writeData(wb, sheet=1, "List of error report table", startRow=1, startCol=2)
  writeData(wb, sheet=1, paste0("Table D1. Summary of error by type of error, ", name, sep=""),
            startRow=2, startCol=2)
  writeData(wb, sheet=1, paste0("Table D2. The list of error by personal ID and error type, ", name, sep=""),
            startRow=3, startCol=2)
  addStyle(wb, sheet=1, style=forDatstl4, rows=1, cols=2)
  setColWidths(wb, sheet=1, cols=2, widths=86)

  writeData(wb, sheet=2, paste0("Table D1. Summary of error by type of error, ", name, sep=""),
            startRow=2, startCol=2)
  mergeCells(wb, sheet=2, cols=2:4, rows=2)
  colnames(error$err.sum) <- c("Error Type", "Description", "Count")
  writeData(wb, sheet=2, error$err.sum, startRow=3, startCol=2, borders="all")
  addStyle(wb, sheet=2, style=forDatstl, rows=3, cols=2:4, gridExpand = TRUE)
  addStyle(wb, sheet=2, style=forDatstl2, rows=4:(3+nrow(error$err.sum)), cols=2:3, gridExpand = TRUE)
  addStyle(wb, sheet=2, style=forDatstl3, rows=4:(3+nrow(error$err.sum)), cols=4, gridExpand = TRUE)
  setColWidths(wb, sheet=2, cols=2, widths=13)
  setColWidths(wb, sheet=2, cols=3, widths=30)
  setColWidths(wb, sheet=2, cols=4, widths=8)

  writeData(wb, sheet=3, paste0("Table D2. The list of error by personal ID and error type, ", name, sep=""),
            startRow=2, startCol=2)
  mergeCells(wb, sheet=3, cols=2:(1+ncol(error$err.details)), rows=2)
  colnames(error$err.details)[1] <- "Personal ID"
  writeData(wb, sheet=3, error$err.details, startRow=3, startCol=2, borders="all")
  addStyle(wb, sheet=3, style=forDatstl, rows=3, cols=2:(1+ncol(error$err.details)), gridExpand = TRUE)
  addStyle(wb, sheet=3, style=forDatstl3, rows=4:(3+nrow(error$err.details)),
           cols=2:(1+ncol(error$err.details)), gridExpand = TRUE)
  setColWidths(wb, sheet=3, cols=2, widths=12)
  setColWidths(wb, sheet=3, cols=3:(1+ncol(error$err.details)), widths=8)

  output.dir <- getwd()
  name <- gsub(", 4 Marital Status", " Mar4", name)
  name <- gsub(", 7 Marital Status", " Mar7", name)
  name <- gsub(", non-marital Birth", " Fert", name)
  name <- gsub(", marital Birth", " FertM", name)
  saveWorkbook(wb, paste0(output.dir, "/", name, " Error Report.xlsx", sep=""), overwrite=TRUE)
}

write.correction.report <- function(error, correction, name){
  forDatstl <- createStyle(halign="center", border="TopBottomLeftRight")
  forDatstl2 <- createStyle(halign="left", border="TopBottomLeftRight")
  forDatstl3 <- createStyle(halign="right", border="TopBottomLeftRight")
  forDatstl4 <- createStyle(textDecoration = "bold")
  wb <- createWorkbook()
  addWorksheet(wb, "List of Tables")
  addWorksheet(wb, "Error Summary")
  addWorksheet(wb, "Error Details")
  addWorksheet(wb, "Modification Summary")
  addWorksheet(wb, "Deletion")
  addWorksheet(wb, "Imputation")
  addWorksheet(wb, "Correction")

  writeData(wb, sheet=1, "List of error report table", startRow=1, startCol=2)
  writeData(wb, sheet=1, paste0("Table D1. Summary of error by type of error, ", name, sep=""),
            startRow=2, startCol=2)
  writeData(wb, sheet=1, paste0("Table D2. The list of error by personal ID and error type, ", name, sep=""),
            startRow=3, startCol=2)
  writeData(wb, sheet=1, paste0("Table D3. Number of imputed, deleted and corrected events, ", name, sep=""),
            startRow=4, startCol=2)
  writeData(wb, sheet=1, paste0("Table D4. The list of deleted event by personal ID and error type, ", name, sep=""),
            startRow=5, startCol=2)
  writeData(wb, sheet=1, paste0("Table D5. The list of event imputed by personal ID and error type, ", name, sep=""),
            startRow=6, startCol=2)
  writeData(wb, sheet=1, paste0("Table D6. The list of correction by personal ID and error type, ", name, sep=""),
            startRow=7, startCol=2)
  addStyle(wb, sheet=1, style=forDatstl4, rows=1, cols=2)
  setColWidths(wb, sheet=1, cols=2, widths=86)

  writeData(wb, sheet=2, paste0("Table D1. Summary of error by type of error, ", name, sep=""),
            startRow=2, startCol=2)
  mergeCells(wb, sheet=2, cols=2:4, rows=2)
  colnames(error$err.sum) <- c("Error Type", "Description", "Count")
  writeData(wb, sheet=2, error$err.sum, startRow=3, startCol=2, borders="all")
  addStyle(wb, sheet=2, style=forDatstl, rows=3, cols=2:4, gridExpand = TRUE)
  addStyle(wb, sheet=2, style=forDatstl2, rows=4:(3+nrow(error$err.sum)), cols=2:3, gridExpand = TRUE)
  addStyle(wb, sheet=2, style=forDatstl3, rows=4:(3+nrow(error$err.sum)), cols=4, gridExpand = TRUE)
  setColWidths(wb, sheet=2, cols=2, widths=13)
  setColWidths(wb, sheet=2, cols=3, widths=30)
  setColWidths(wb, sheet=2, cols=4, widths=8)

  writeData(wb, sheet=3, paste0("Table D2. The list of error by personal ID and error type, ", name, sep=""),
            startRow=2, startCol=2)
  mergeCells(wb, sheet=3, cols=2:(1+ncol(error$err.details)), rows=2)
  if (nrow(error$err.details)==0) {
    colnames(error$err.details) <- c("Personal ID", "event")
    writeData(wb, sheet=3, error$err.details, startRow=3, startCol=2, borders="all")
    addStyle(wb, sheet=3, style=forDatstl, rows=3, cols=2:3, gridExpand = TRUE)
    addStyle(wb, sheet=3, style=forDatstl3, rows=4, cols=2:3, gridExpand = TRUE)
  } else {
    colnames(error$err.details)[1] <- "Personal ID"
    writeData(wb, sheet=3, error$err.details, startRow=3, startCol=2, borders="all")
    addStyle(wb, sheet=3, style=forDatstl, rows=3, cols=2:(1+ncol(error$err.details)), gridExpand = TRUE)
    addStyle(wb, sheet=3, style=forDatstl2, rows=4:(3+nrow(error$err.details)), cols=2, gridExpand = TRUE)
    addStyle(wb, sheet=3, style=forDatstl3, rows=4:(3+nrow(error$err.details)),
             cols=3:(1+ncol(error$err.details)), gridExpand = TRUE)
  }
  setColWidths(wb, sheet=3, cols=2, widths=12)
  setColWidths(wb, sheet=3, cols=3:(1+ncol(error$err.details)), widths=8)

  writeData(wb, sheet=4, paste0("Table D3. Number of imputed, deleted and corrected events, ", name, sep=""),
            startRow=2, startCol=2)
  mergeCells(wb, sheet=4, cols=2:3, rows=2)
  colnames(correction$summary) <- c("", "Count")
  writeData(wb, sheet=4, correction$summary, startRow=3, startCol=2, borders="all")
  addStyle(wb, sheet=4, style=forDatstl, rows=3, cols=2:3, gridExpand = TRUE)
  addStyle(wb, sheet=4, style=forDatstl2, rows=4:6, cols=2, gridExpand = TRUE)
  addStyle(wb, sheet=4, style=forDatstl3, rows=4:6, cols=3, gridExpand = TRUE)
  setColWidths(wb, sheet=4, cols=2, widths=15)
  setColWidths(wb, sheet=4, cols=3, widths=8)

  writeData(wb, sheet=5, paste0("Table D4. The list of deleted event by personal ID and error type, ", name, sep=""),
            startRow=2, startCol=2)
  mergeCells(wb, sheet=5, cols=2:(1+ncol(correction$deleted)), rows=2)
  if (nrow(correction$deleted)==0) {
    #colnames(correction$deleted) <- c("Personal ID", "event")
    writeData(wb, sheet=5, correction$deleted, startRow=3, startCol=2, borders="all")
    addStyle(wb, sheet=5, style=forDatstl, rows=3, cols=2:3, gridExpand = TRUE)
    addStyle(wb, sheet=5, style=forDatstl3, rows=4, cols=2:3, gridExpand = TRUE)
  } else {
    colnames(correction$deleted)[1] <- "Personal ID"
    writeData(wb, sheet=5, correction$deleted, startRow=3, startCol=2, borders="all")
    addStyle(wb, sheet=5, style=forDatstl, rows=3, cols=2:(1+ncol(correction$deleted)), gridExpand = TRUE)
    addStyle(wb, sheet=5, style=forDatstl2, rows=4:(3+nrow(correction$deleted)), cols=2, gridExpand = TRUE)
    addStyle(wb, sheet=5, style=forDatstl3, rows=4:(3+nrow(correction$deleted)),
             cols=3:(1+ncol(correction$deleted)), gridExpand = TRUE)
  }
  setColWidths(wb, sheet=5, cols=2, widths=12)
  setColWidths(wb, sheet=5, cols=3:(1+ncol(correction$deleted)), widths=8)

  writeData(wb, sheet=6, paste0("Table D5. The list of event imputed by personal ID and error type, ", name, sep=""),
            startRow=2, startCol=2)
  mergeCells(wb, sheet=6, cols=2:(1+ncol(correction$imputed)), rows=2)
  if (nrow(correction$imputed)==0) {
    #colnames(correction$imputed) <- c("Personal ID", "event")
    writeData(wb, sheet=6, correction$imputed, startRow=3, startCol=2, borders="all")
    addStyle(wb, sheet=6, style=forDatstl, rows=3, cols=2:3, gridExpand = TRUE)
    addStyle(wb, sheet=6, style=forDatstl3, rows=4, cols=2:3, gridExpand = TRUE)
  } else {
    #colnames(correction$imputed)[1] <- "Personal ID"
    writeData(wb, sheet=6, correction$imputed, startRow=3, startCol=2, borders="all")
    addStyle(wb, sheet=6, style=forDatstl, rows=3, cols=2:(1+ncol(correction$imputed)), gridExpand = TRUE)
    addStyle(wb, sheet=6, style=forDatstl2, rows=4:(3+nrow(correction$imputed)), cols=2, gridExpand = TRUE)
    addStyle(wb, sheet=6, style=forDatstl3, rows=4:(3+nrow(correction$imputed)),
             cols=3:(1+ncol(correction$imputed)), gridExpand = TRUE)
  }
  setColWidths(wb, sheet=6, cols=2, widths=12)
  setColWidths(wb, sheet=6, cols=3:(1+ncol(correction$imputed)), widths=8)

  writeData(wb, sheet=7, paste0("Table D6. The list of correction by personal ID and error type, ", name, sep=""),
            startRow=2, startCol=2)
  mergeCells(wb, sheet=7, cols=2:(1+ncol(correction$corrected)), rows=2)
  if (nrow(correction$corrected)==0) {
    #colnames(correction$corrected) <- c("Personal ID", "event")
    writeData(wb, sheet=7, correction$corrected, startRow=3, startCol=2, borders="all")
    addStyle(wb, sheet=7, style=forDatstl, rows=3, cols=2:3, gridExpand = TRUE)
    addStyle(wb, sheet=7, style=forDatstl3, rows=4, cols=2:3, gridExpand = TRUE)
  } else {
    #colnames(correction$corrected)[1] <- "Personal ID"
    writeData(wb, sheet=7, correction$corrected, startRow=3, startCol=2, borders="all")
    addStyle(wb, sheet=7, style=forDatstl, rows=3, cols=2:(1+ncol(correction$corrected)), gridExpand = T)
    addStyle(wb, sheet=7, style=forDatstl2, rows=4:(3+nrow(correction$corrected)), cols=2, gridExpand = T)
    addStyle(wb, sheet=7, style=forDatstl3, rows=4:(3+nrow(correction$corrected)),
             cols=3:(1+ncol(correction$corrected)), gridExpand = TRUE)
  }
  setColWidths(wb, sheet=7, cols=2, widths=12)
  setColWidths(wb, sheet=7, cols=3:(1+ncol(correction$corrected)), widths=8)

  output.dir <- getwd()
  name <- gsub(", 4 Marital Status", " Mar4", name)
  name <- gsub(", 7 Marital Status", " Mar7", name)
  name <- gsub(", non-marital Birth", " Fert", name)
  name <- gsub(", marital Birth", " FertM", name)
  saveWorkbook(wb, paste0(output.dir, "/", name, " Error Report.xlsx", sep=""), overwrite=T)
}

ym.to.cmc <- function(data) {
  data <- data %>%
    mutate(tMonth = replace(tMonth, TRUE, (tYear-1900)*12+tMonth)) %>%
    mutate(bMonth = replace(bMonth, TRUE, (bYear-1900)*12+bMonth)) %>%
    mutate(m = replace(m, TRUE, (y-1900)*12+m)) %>%
    select(-y, -bYear, -tYear)
  return(data)
}

##------Marital-------
screen1.ym <- function(data) {
  info <- data %>% select(region:bMonth)
  evt <- data %>% select(ID, y1:ncol(data))
  long <- reshape(evt, idvar = "ID",
                  varying = names(evt)[2:ncol(evt)],
                  v.names = c("event", "m", "y"),
                  timevar = "event_index",
                  times = 1:((ncol(evt)-1)/3),
                  new.row.names = 1:1000000000000000,
                  direction = "long") %>% arrange(ID, event_index)
  names(long)[3:5] <- names(long)[5:3]

  data <- long %>% arrange(ID, y, m, event) %>%
    group_by(ID) %>%
    mutate(new_index = row_number()) %>%
    filter(rowSums(is.na(cbind(event, y, m)))<3 | new_index==1)

  sys.year <- as.integer(format(Sys.Date(), "%Y"))
  sys.mon <- as.integer(format(Sys.Date(), "%m"))
  info <- info %>%
    mutate(err1 = ifelse(is.na(tYear)|is.na(tMonth),1,NA)) %>%
    mutate(err2 = ifelse(is.na(bYear)|is.na(bMonth),2,NA)) %>%
    mutate(err10 = ifelse(is.na(err1)&(tYear<1000|(tYear*12+tMonth)>(sys.year*12+sys.mon)|!(tMonth %in% 1:12)),10,NA)) %>%
    mutate(err11 = ifelse(is.na(err2)&(bYear<1000|(bYear*12+bMonth)>(sys.year*12+sys.mon)|!(bMonth %in% 1:12)|(bYear*12+bMonth)>(tYear*12+tMonth)|((tYear*12+tMonth)-(bYear*12+bMonth))>120*12),11,NA))

  data <- data %>% left_join(info, by = "ID")

  data <- data %>%
    mutate(err4 = ifelse(is.na(event)&rowSums(is.na(cbind(event, y, m)))!=3, 4, NA)) %>%
    mutate(err3 = ifelse((is.na(y)|is.na(m))&rowSums(is.na(cbind(event, y, m)))!=3, 3, NA)) %>%
    mutate(err7 = ifelse(!(is.na(event)|event %in% c(1:6, 9)), 7, NA)) %>%
    mutate(err8 = ifelse((!is.na(y))&y<1000, 8, NA)) %>%
    mutate(err9 = ifelse(!(is.na(m)|m %in% 1:12), 9, NA))

  data <- data %>% relocate(err1, err2, err3, err4, err7, err8, err9, err10, err11,
                            .before = new_index)
  return(data)
}

screen1.cmc <- function(data) {
  info <- data %>% select(region:bMonth)
  evt <- data %>% select(ID, m1:ncol(data))
  long <- reshape(evt, idvar = "ID",
                  varying = names(evt)[2:ncol(evt)],
                  v.names = c("event", "m"),
                  timevar = "event_index",
                  times = 1:((ncol(evt)-1)/2),
                  new.row.names = 1:1000000000000,
                  direction = "long") %>% arrange(ID, event_index)
  names(long)[3:4] <- names(long)[4:3]

  data <- long %>% arrange(ID, m, event) %>%
    group_by(ID) %>%
    mutate(new_index = row_number()) %>%
    filter(rowSums(is.na(cbind(event, m)))<2 | new_index==1)

  sys.year <- as.integer(format(Sys.Date(), "%Y"))
  sys.mon <- as.integer(format(Sys.Date(), "%m"))
  sys.cmc <- (sys.year-1900)*12+sys.mon
  info <- info %>%
    mutate(err1 = ifelse(is.na(tMonth),1,NA)) %>%
    mutate(err2 = ifelse(is.na(bMonth),2,NA)) %>%
    mutate(err10 = ifelse(is.na(err1)&(tMonth>sys.cmc),10,NA)) %>%
    mutate(err11 = ifelse(is.na(err2)&(bMonth>sys.cmc|bMonth>tMonth|(tMonth-bMonth)>120*12),11,NA))

  data <- data %>% left_join(info, by = "ID")

  data <- data %>%
    mutate(err4 = ifelse(is.na(event)&rowSums(is.na(cbind(event, m)))!=2, 4, NA)) %>%
    mutate(err3 = ifelse(is.na(m)&rowSums(is.na(cbind(event, m)))!=2, 3, NA)) %>%
    mutate(err7 = ifelse(!(is.na(event)|event %in% c(1:6, 9)), 7, NA)) %>%
    mutate(err8 = NA) %>%
    mutate(err9 = NA)

  data <- data %>% relocate(err1, err2, err3, err4, err7, err8, err9, err10, err11,
                            .before = new_index)
  return(data)
}

screen2.mar4.ym <- function(data) {

  #Keep only 4 marital status events
  data <- data %>%
    mutate(y = replace(y, which(event %in% c(5, 6, 9)), NA)) %>%
    mutate(m = replace(m, which(event %in% c(5, 6, 9)), NA)) %>%
    mutate(event = replace(event, which(event %in% c(5, 6, 9)), NA)) %>%
    arrange(ID, y, m, event) %>%
    group_by(ID) %>%
    mutate(new_index = row_number()) %>%
    filter(rowSums(is.na(cbind(event, y, m)))<3 | new_index==1)

  #Check duplicates with the same event time and type
  data_dup <- data %>%
    filter(duplicated(cbind(ID, event, y, m))) %>%
    mutate(err12 = 12)
  data_nodup <- data %>%
    filter(!duplicated(cbind(ID, event, y, m))) %>%
    mutate(err12 = NA) %>%
    select(-new_index)

  #Consider cases with same event time but different types
  #can only deal with no more than 2 consecutive same time
  data_nodup <- data_nodup %>% group_by(ID) %>% arrange(ID, y, m, event) %>%
    mutate(same.t = ifelse((lag(y)*12+lag(m))==m|(lead(y)*12+lead(m))==m, 1, NA)) %>%
    mutate(last = ifelse(same.t==1&is.na(lag(same.t)), lag(event),
                         ifelse(same.t==1&lag(same.t)==1, lag(lag(event)), NA))) %>%
    mutate(lead = ifelse(same.t==1&is.na(lead(same.t)), lead(event),
                         ifelse(same.t==1&lead(same.t)==1, lead(lead(event)), NA)))

  attach(data_nodup)
  data_nodup$same.o <-
    dplyr::case_when(same.t==1 & event %in% c(1,2) & last %in% c(3,4) ~ 1,
              same.t==1 & event %in% c(1,2) & lead %in% c(3,4) ~ 2,
              same.t==1 & event %in% c(1,2) & last %in% c(1,2) ~ 2,
              same.t==1 & event %in% c(1,2) & lead %in% c(1,2) ~ 1,
              same.t==1 & event %in% c(3,4) & last %in% c(1,2) ~ 1,
              same.t==1 & event %in% c(3,4) & lead %in% c(1,2) ~ 2,
              same.t==1 & event %in% c(3,4) & last %in% c(3,4) ~ 2,
              same.t==1 & event %in% c(3,4) & lead %in% c(3,4) ~ 1,
              TRUE ~ NA)
  detach(data_nodup)

  data_nodup <- data_nodup %>% group_by(ID) %>% arrange(ID, y, m, same.o) %>%
    mutate(new_index = row_number()) %>%
    arrange(ID, new_index) %>%
    select(-same.t, -lead, -last, -same.o)

  #Check first marriage duplicates（same time/within half a year）
  dup_check <- data_nodup %>% group_by(ID) %>%
    mutate(err12 = replace(err12, which(event==2&lag(event)==1&y==lag(y)&m==lag(m)), 12)) %>%
    mutate(err12 = replace(err12, which(event==2&lag(event)==1&((y*12+m)-(lag(y)*12+lag(m)))<=6), 12))
  data_dup1 <- dup_check %>% filter(err12==12)
  data_nodup <- dup_check %>% filter(is.na(err12))
  data_dup <- rbind(data_dup, data_dup1)
  rm(dup_check)
  rm(data_dup1)

  #Check duplicates within half a year
  dup_check <- data_nodup %>% group_by(ID) %>%
    mutate(err12 = replace(err12, which(event==lag(event) & ((y*12+m)-(lag(y)*12+lag(m)))<=6), 12))
  data_dup2 <-  dup_check %>% filter(err12==12)
  data_nodup <- dup_check %>% filter(is.na(err12))
  data_dup <- rbind(data_dup, data_dup2)
  rm(dup_check)
  rm(data_dup2)

  #Check event time logic
  data_nodup <- data_nodup %>%
    mutate(err5 = ifelse((y*12+m)>(tYear*12+tMonth)|(y*12+m)<(bYear*12+bMonth), 5, NA))

  #Check event type logic
  data_nodup <- data_nodup %>% group_by(ID) %>%
    mutate(lag.event = lag(event)) %>%
    mutate(next.event = lead(event)) %>%
    mutate(mar_check = ifelse((lag.event==1|lag.event==2) &
                                (event %in% c(1, 2)), FALSE, TRUE)) %>%
    mutate(mardis_check = ifelse((lag.event==3|lag.event==4) &
                                   (event %in% c(1, 3, 4)), FALSE, TRUE))
  data_nodup <- data_nodup %>%
    mutate(err6 = ifelse(mar_check==FALSE|mardis_check==FALSE, 6, NA)) %>%
    select(-lag.event, -next.event, -mar_check, -mardis_check)

  data <- rbind(data_nodup, data_dup)
  rm(data_nodup)
  rm(data_dup)
  data <- data %>%
    relocate(err1, err2, err3, err4, err5, err6, err7, err8, err9, err10, err11, err12,
             .before = new_index) %>% select(ID:err12)

  return(data)
}

screen2.mar4.cmc <- function(data) {

  #Keep only 4 marital status events
  data <- data %>%
    mutate(m = replace(m, which(event %in% c(5, 6, 9)), NA)) %>%
    mutate(event = replace(event, which(event %in% c(5, 6, 9)), NA)) %>%
    arrange(ID, m, event) %>%
    group_by(ID) %>%
    mutate(new_index = row_number()) %>%
    filter(rowSums(is.na(cbind(event, m)))<2 | new_index==1)

  #Check duplicates with the same event time and type
  data_dup <- data %>%
    filter(duplicated(cbind(ID, event, m))) %>%
    mutate(err12 = 12)
  data_nodup <- data %>%
    filter(!duplicated(cbind(ID, event, m))) %>%
    mutate(err12 = NA) %>%
    select(-new_index)

  #Consider cases with same event time but different types
  #can only deal with no more than 2 consecutive same time
  data_nodup <- data_nodup %>% group_by(ID) %>% arrange(ID, m, event) %>%
    mutate(same.t = ifelse(lag(m)==m|lead(m)==m, 1, NA)) %>%
    mutate(last = ifelse(same.t==1&is.na(lag(same.t)), lag(event),
                         ifelse(same.t==1&lag(same.t)==1, lag(lag(event)), NA))) %>%
    mutate(lead = ifelse(same.t==1&is.na(lead(same.t)), lead(event),
                         ifelse(same.t==1&lead(same.t)==1, lead(lead(event)), NA)))

  attach(data_nodup)
  data_nodup$same.o <-
    dplyr::case_when(same.t==1 & event %in% c(1,2) & last %in% c(3,4) ~ 1,
              same.t==1 & event %in% c(1,2) & lead %in% c(3,4) ~ 2,
              same.t==1 & event %in% c(1,2) & last %in% c(1,2) ~ 2,
              same.t==1 & event %in% c(1,2) & lead %in% c(1,2) ~ 1,
              same.t==1 & event %in% c(3,4) & last %in% c(1,2) ~ 1,
              same.t==1 & event %in% c(3,4) & lead %in% c(1,2) ~ 2,
              same.t==1 & event %in% c(3,4) & last %in% c(3,4) ~ 2,
              same.t==1 & event %in% c(3,4) & lead %in% c(3,4) ~ 1,
              TRUE ~ NA)
  detach(data_nodup)

  data_nodup <- data_nodup %>% group_by(ID) %>% arrange(ID, m, same.o) %>%
    mutate(new_index = row_number()) %>%
    arrange(ID, new_index) %>%
    select(-same.t, -lead, -last, -same.o)

  #Check first marriage duplicates（same time/within half a year）
  dup_check <- data_nodup %>% group_by(ID) %>%
    mutate(err12 = replace(err12, which(event==2&lag(event)==1&m==lag(m)), 12)) %>%
    mutate(err12 = replace(err12, which(event==2&lag(event)==1&(m-lag(m))<=6), 12))
  data_dup1 <- dup_check %>% filter(err12==12)
  data_nodup <- dup_check %>% filter(is.na(err12))
  data_dup <- rbind(data_dup, data_dup1)
  rm(dup_check)
  rm(data_dup1)

  #Check duplicates within half a year
  dup_check <- data_nodup %>% group_by(ID) %>%
    mutate(err12 = replace(err12, which(event==lag(event) & (m-lag(m))<=6), 12))
  data_dup2 <-  dup_check %>% filter(err12==12)
  data_nodup <- dup_check %>% filter(is.na(err12))
  data_dup <- rbind(data_dup, data_dup2)
  rm(dup_check)
  rm(data_dup2)

  #Check event time logic
  data_nodup <- data_nodup %>%
    mutate(err5 = ifelse(m>tMonth|m<bMonth, 5, NA))

  #Check event type logic
  data_nodup <- data_nodup %>% group_by(ID) %>%
    mutate(lag.event = lag(event)) %>%
    mutate(next.event = lead(event)) %>%
    mutate(mar_check = ifelse((lag.event==1|lag.event==2) &
                                (event %in% c(1, 2)), FALSE, TRUE)) %>%
    mutate(mardis_check = ifelse((lag.event==3|lag.event==4) &
                                   (event %in% c(1, 3, 4)), FALSE, TRUE))
  data_nodup <- data_nodup %>%
    mutate(err6 = ifelse(mar_check==FALSE|mardis_check==FALSE, 6, NA)) %>%
    select(-lag.event, -next.event, -mar_check, -mardis_check)

  data <- rbind(data_nodup, data_dup)
  rm(data_nodup)
  rm(data_dup)
  data <- data %>%
    relocate(err1, err2, err3, err4, err5, err6, err7, err8, err9, err10, err11, err12,
             .before = new_index) %>% select(ID:err12)

  return(data)
}

recode.mar4 <- function(data) {
  data <- data %>% group_by(ID) %>%
    arrange(ID, m) %>%
    mutate(lag.event = lag(event))

  attach(data)
  data$type4 <- dplyr::case_when(event==1 ~ 1, event==2&lag.event==3 ~ 3,
                          event==3 ~ 17, event==4 ~ 2, event==2&lag.event==4 ~ 4,
                          event==2&is.na(lag.event)&(m-bMonth)/12<65 ~ 4,
                          event==2&is.na(lag.event)&(m-bMonth)/12>=65 ~ 3,
                          TRUE ~ NA_real_)
  detach(data)
  data <- data %>% select(-lag.event) %>%
    mutate(new_index = row_number()) %>%
    mutate(events = ifelse(rowSums(is.na(cbind(event, m)))==2&n()==1, 0, n())) %>%
    relocate(events, .after = bMonth) %>%
    relocate(type4, .after = m) %>% select(-event)
  names(data)[4] <- "event"

  return(data)
}

screen2.mar7.ym <- function(data) {

  #Keep only 7 marital status events
  data <- data %>%
    mutate(y = replace(y, which(event==9), NA)) %>%
    mutate(m = replace(m, which(event==9), NA)) %>%
    mutate(event = replace(event, which(event==9), NA)) %>%
    arrange(ID, y, m, event) %>%
    group_by(ID) %>%
    mutate(new_index = row_number()) %>%
    filter(rowSums(is.na(cbind(event, y, m)))<3 | new_index==1)

  #Check duplicates with the same event time and type
  data_dup <- data %>%
    filter(duplicated(cbind(ID, event, y, m))) %>%
    mutate(err12 = 12)
  data_nodup <- data %>%
    filter(!duplicated(cbind(ID, event, y, m))) %>%
    mutate(err12 = NA) %>%
    select(-new_index)

  #Consider cases with same event time but different types
  #can only deal with no more than 2 consecutive same time
  data_nodup <- data_nodup %>% group_by(ID) %>% arrange(ID, y, m, event) %>%
    mutate(same.t = ifelse((lag(y)*12+lag(m))==(y*12+m)|(lead(y)*12+lead(m))==(y*12+m), 1, NA)) %>%
    mutate(last = ifelse(same.t==1&is.na(lag(same.t)), lag(event),
                         ifelse(same.t==1&lag(same.t)==1, lag(lag(event)), NA))) %>%
    mutate(lead = ifelse(same.t==1&is.na(lead(same.t)), lead(event),
                         ifelse(same.t==1&lead(same.t)==1, lead(lead(event)), NA)))

  attach(data_nodup)
  data_nodup$same.o <-
    dplyr::case_when(same.t==1 & event %in% c(1,2,5) & last %in% c(3,4,6) ~ 1,
              same.t==1 & event %in% c(1,2,5) & lead %in% c(3,4,6) ~ 2,
              same.t==1 & event %in% c(1,2,5) & last %in% c(1,2,5) ~ 2,
              same.t==1 & event %in% c(1,2,5) & lead %in% c(1,2,5) ~ 1,
              same.t==1 & event %in% c(3,4,6) & last %in% c(1,2,5) ~ 1,
              same.t==1 & event %in% c(3,4,6) & lead %in% c(1,2,5) ~ 2,
              same.t==1 & event %in% c(3,4,6) & last %in% c(3,4,6) ~ 2,
              same.t==1 & event %in% c(3,4,6) & lead %in% c(3,4,6) ~ 1,
              TRUE ~ NA)
  detach(data_nodup)

  data_nodup <- data_nodup %>% group_by(ID) %>% arrange(ID, y, m, same.o) %>%
    mutate(new_index = row_number()) %>%
    arrange(ID, new_index) %>%
    select(-same.t, -lead, -last, -same.o)

  #Generate lagged marital events (not cohabiting event)
  data_nodup <- data_nodup %>% group_by(ID) %>%
    mutate(lag.mar = ifelse(lag(event) %in% c(1:4), lag(event), NA)) %>%
    fill(lag.mar) %>%
    mutate(lag.mar.y = ifelse(lag(event) %in% c(1:4), lag(y), NA)) %>%
    fill(lag.mar.y) %>%
    mutate(lag.mar.m = ifelse(lag(event) %in% c(1:4), lag(m), NA)) %>%
    fill(lag.mar.m)

  #Check first marriage duplicates（same time/within half a year）
  dup_check <- data_nodup %>% group_by(ID) %>%
    mutate(err12 = replace(err12, which(event==2&lag.mar==1&y==lag.mar.y&m==lag.mar.m), 12)) %>%
    mutate(err12 = replace(err12, which(event==2&lag.mar==1&((y*12+m)-(lag.mar.y*12+lag.mar.m)<=6)), 12))
  data_dup1 <- dup_check %>% filter(err12==12)
  data_nodup <- dup_check %>% filter(is.na(err12))
  data_dup <- rbind(data_dup, data_dup1)
  rm(dup_check)
  rm(data_dup1)

  #Check duplicates within half a year
  dup_check <- data_nodup %>% group_by(ID) %>%
    mutate(err12 = replace(err12, which(event==lag.mar & ((y*12+m)-(lag.mar.y*12+lag.mar.m))<=6), 12))
  data_dup2 <-  dup_check %>% filter(err12==12)
  data_nodup <- dup_check %>% filter(is.na(err12))
  data_dup <- rbind(data_dup, data_dup2)
  rm(dup_check)
  rm(data_dup2)

  #Check event time logic
  data_nodup <- data_nodup %>%
    mutate(err5 = ifelse((y*12+m)>(tYear*12+tMonth)|(y*12+m)<(bYear*12+bMonth), 5, NA))

  #Check event type logic
  data_nodup <- data_nodup %>% group_by(ID) %>%
    mutate(lag.event = lag(event)) %>%
    mutate(mar_check = ifelse((lag.event==1|lag.event==2) &
                                (event %in% c(1, 2, 5, 6)), FALSE, TRUE)) %>%
    mutate(mardis_check = ifelse((lag.event==3|lag.event==4) &
                                   (event %in% c(1, 3, 4, 6)), FALSE, TRUE)) %>%
    mutate(coh_check = ifelse(lag.event==5&(event %in% c(3, 4, 5)), FALSE, TRUE)) %>%
    mutate(cohdis_check = ifelse(lag.event==6&(event %in% c(3, 4, 6)), FALSE, TRUE)) %>%
    mutate(cohmar1_check = ifelse((lag.event==5|lag.event==6) & event==1 &
                                    (lag.mar %in% 1:4), FALSE, TRUE)) %>%
    mutate(cohmar_check = ifelse((lag.event==5|lag.event==6) & event==2 &
                                   (lag.mar %in% 1:2), FALSE, TRUE))
  data_nodup <- data_nodup %>%
    mutate(err6 = ifelse(mar_check==FALSE|mardis_check==FALSE|coh_check==FALSE|cohdis_check==FALSE|
                           cohmar1_check==FALSE|cohmar_check==FALSE, 6, NA)) %>%
    select(-lag.event, -lag.mar, -lag.mar.y, -lag.mar.m,
           -mar_check, -mardis_check, -coh_check, -cohdis_check, -cohmar1_check, -cohmar_check)

  data <- rbind(data_nodup, data_dup)
  data <- data %>%
    relocate(err1, err2, err3, err4, err5, err6, err7, err8, err9, err10, err11, err12,
             .before = new_index) %>% select(ID:err12)

  return(data)
}

screen2.mar7.cmc <- function(data) {

  #Keep only 7 marital status events
  data <- data %>%
    mutate(m = replace(m, which(event==9), NA)) %>%
    mutate(event = replace(event, which(event==9), NA)) %>%
    arrange(ID, m, event) %>%
    group_by(ID) %>%
    mutate(new_index = row_number()) %>%
    filter(rowSums(is.na(cbind(event, m)))<2 | new_index==1)

  #Check duplicates with the same event time and type
  data_dup <- data %>%
    filter(duplicated(cbind(ID, event, m))) %>%
    mutate(err12 = 12)
  data_nodup <- data %>%
    filter(!duplicated(cbind(ID, event, m))) %>%
    mutate(err12 = NA) %>%
    select(-new_index)

  #Consider cases with same event time but different types
  #can only deal with no more than 2 consecutive same time
  data_nodup <- data_nodup %>% group_by(ID) %>% arrange(ID, m, event) %>%
    mutate(same.t = ifelse(lag(m)==m|lead(m)==m, 1, NA)) %>%
    mutate(last = ifelse(same.t==1&is.na(lag(same.t)), lag(event),
                         ifelse(same.t==1&lag(same.t)==1, lag(lag(event)), NA))) %>%
    mutate(lead = ifelse(same.t==1&is.na(lead(same.t)), lead(event),
                         ifelse(same.t==1&lead(same.t)==1, lead(lead(event)), NA)))

  attach(data_nodup)
  data_nodup$same.o <-
    dplyr::case_when(same.t==1 & event %in% c(1,2,5) & last %in% c(3,4,6) ~ 1,
              same.t==1 & event %in% c(1,2,5) & lead %in% c(3,4,6) ~ 2,
              same.t==1 & event %in% c(1,2,5) & last %in% c(1,2,5) ~ 2,
              same.t==1 & event %in% c(1,2,5) & lead %in% c(1,2,5) ~ 1,
              same.t==1 & event %in% c(3,4,6) & last %in% c(1,2,5) ~ 1,
              same.t==1 & event %in% c(3,4,6) & lead %in% c(1,2,5) ~ 2,
              same.t==1 & event %in% c(3,4,6) & last %in% c(3,4,6) ~ 2,
              same.t==1 & event %in% c(3,4,6) & lead %in% c(3,4,6) ~ 1,
              TRUE ~ NA)
  detach(data_nodup)

  data_nodup <- data_nodup %>% group_by(ID) %>% arrange(ID, m, same.o) %>%
    mutate(new_index = row_number()) %>%
    arrange(ID, new_index) %>%
    select(-same.t, -lead, -last, -same.o)

  #Generate lagged marital events (not cohabiting event)
  data_nodup <- data_nodup %>% group_by(ID) %>%
    mutate(lag.mar = ifelse(lag(event) %in% c(1:4), lag(event), NA)) %>%
    fill(lag.mar) %>%
    mutate(lag.mar.m = ifelse(lag(event) %in% c(1:4), lag(m), NA)) %>%
    fill(lag.mar.m)

  #Check first marriage duplicates（same time/within half a year）
  dup_check <- data_nodup %>% group_by(ID) %>%
    mutate(err12 = replace(err12, which(event==2&lag.mar==1&m==lag.mar.m), 12)) %>%
    mutate(err12 = replace(err12, which(event==2&lag.mar==1&(m-lag.mar.m<=6)), 12))
  data_dup1 <- dup_check %>% filter(err12==12)
  data_nodup <- dup_check %>% filter(is.na(err12))
  data_dup <- rbind(data_dup, data_dup1)
  rm(dup_check)
  rm(data_dup1)

  #Check duplicates within half a year
  dup_check <- data_nodup %>% group_by(ID) %>%
    mutate(err12 = replace(err12, which(event==lag.mar & (m-lag.mar.m)<=6), 12))
  data_dup2 <-  dup_check %>% filter(err12==12)
  data_nodup <- dup_check %>% filter(is.na(err12))
  data_dup <- rbind(data_dup, data_dup2)
  rm(dup_check)
  rm(data_dup2)

  #Check event time logic
  data_nodup <- data_nodup %>%
    mutate(err5 = ifelse(m>tMonth|m<bMonth, 5, NA))

  #Check event type logic
  data_nodup <- data_nodup %>% group_by(ID) %>%
    mutate(lag.event = lag(event)) %>%
    mutate(mar_check = ifelse((lag.event==1|lag.event==2) &
                                (event %in% c(1, 2, 5, 6)), FALSE, TRUE)) %>%
    mutate(mardis_check = ifelse((lag.event==3|lag.event==4) &
                                   (event %in% c(1, 3, 4, 6)), FALSE, TRUE)) %>%
    mutate(coh_check = ifelse(lag.event==5&(event %in% c(3, 4, 5)), FALSE, TRUE)) %>%
    mutate(cohdis_check = ifelse(lag.event==6&(event %in% c(3, 4, 6)), FALSE, TRUE)) %>%
    mutate(cohmar1_check = ifelse((lag.event==5|lag.event==6) & event==1 &
                                    (lag.mar %in% 1:4), FALSE, TRUE)) %>%
    mutate(cohmar_check = ifelse((lag.event==5|lag.event==6) & event==2 &
                                   (lag.mar %in% 1:2), FALSE, TRUE))
  data_nodup <- data_nodup %>%
    mutate(err6 = ifelse(mar_check==FALSE|mardis_check==FALSE|coh_check==FALSE|cohdis_check==FALSE|
                           cohmar1_check==FALSE|cohmar_check==FALSE, 6, NA)) %>%
    select(-lag.event, -lag.mar, -lag.mar.m,
           -mar_check, -mardis_check, -coh_check, -cohdis_check, -cohmar1_check, -cohmar_check)

  data <- rbind(data_nodup, data_dup)
  data <- data %>%
    relocate(err1, err2, err3, err4, err5, err6, err7, err8, err9, err10, err11, err12,
             .before = new_index) %>% select(ID:err12)

  return(data)
}

recode.mar7 <- function(data) {
  data <- data %>% group_by(ID) %>%
    arrange(ID, m) %>% mutate(new_index = row_number()) %>%
    mutate(lag.mar = ifelse(lag(event) %in% c(1:4), lag(event), NA)) %>%
    fill(lag.mar) %>%
    mutate(next.mar = ifelse(lead(event) %in% c(1:4), lead(event), NA)) %>%
    mutate(next.age = ifelse(lead(event) %in% c(1:4), (lead(m)-bMonth)/12, NA)) %>%
    fill(next.mar, next.age, .direction = "up")
  data <- data %>% group_by(ID) %>%
    arrange(ID, m) %>%
    mutate(lag.event = lag(event))

  attach(data)
  data$type7 <- dplyr::case_when(event==3 ~ 17,                                             #From Married to Widowed & Not Cohabiting
                          event==4 ~ 2,                                              #From Married to Divorced & Not Cohabiting
                          is.na(lag.mar)&is.na(lag.event)&event==1 ~ 1,              #From Never Married & Not Cohabiting to Married
                          is.na(lag.mar)&lag.event==6&event==1 ~ 1,                  #From Never Married & Not Cohabiting to Married
                          is.na(lag.mar)&lag.event==5&event==1 ~ 11,                 #From Never Marred & Cohabiting to Married
                          is.na(lag.mar)&is.na(lag.event)&(m-bMonth)/12<65&event==2 ~ 4,  #From Divorced & Not Cohabiting to Married
                          is.na(lag.mar)&is.na(lag.event)&(m-bMonth)/12>=65&event==2 ~ 3, #From Widowed & Not Cohabiting to Married
                          is.na(lag.mar)&lag.event==5&(m-bMonth)/12<65&event==2 ~ 13,  #From Divorced & Cohabiting to Married
                          is.na(lag.mar)&lag.event==5&(m-bMonth)/12>=65&event==2 ~ 12, #From Widowed & Cohabiting to Married
                          is.na(lag.mar)&lag.event==6&(m-bMonth)/12<65&event==2 ~ 4,   #From Divorced & Not Cohabiting to Married
                          is.na(lag.mar)&lag.event==6&(m-bMonth)/12>=65&event==2 ~ 3,  #From Widowed & Not Cohabiting to Married
                          is.na(lag.mar)&next.mar==1&event==5 ~ 5,                   #From Never Married & Not Cohabiting to Never Married & Cohabiting
                          is.na(lag.mar)&is.na(next.mar)&mar==1&event==5 ~ 5,        #From Never Married & Not Cohabiting to Never Married & Cohabiting
                          is.na(lag.mar)&is.na(next.mar)&mar==3&event==5 ~ 6,        #From Widowed & Not Cohabiting to Widowed & Cohabiting
                          is.na(lag.mar)&is.na(next.mar)&mar==4&event==5 ~ 7,        #From Divorced & Not Cohabiting to Divorced & Cohabiting
                          is.na(lag.mar)&next.mar==2&next.age<65&event==5 ~ 7,       #From Divorced & Not Cohabiting to Divorced & Cohabiting
                          is.na(lag.mar)&next.mar==2&next.age>=65&event==5 ~ 6,      #From Widowed & Not Cohabiting to  Widowed & Cohabiting
                          is.na(lag.mar)&next.mar==1&event==6 ~ 8,                   #From Never Married & Cohabiting to Never Marred & Not cohabiting
                          is.na(lag.mar)&is.na(next.mar)&mar==1&event==6 ~ 8,        #From Never Married & Cohabiting to Never Marred & Not cohabiting
                          is.na(lag.mar)&is.na(next.mar)&mar==3&event==6 ~ 9,        #From Widowed & Cohabiting to Widowed & Not cohabiting
                          is.na(lag.mar)&is.na(next.mar)&mar==4&event==6 ~ 10,       #From Divorced & Cohabiting to Divorced & Not cohabiting
                          is.na(lag.mar)&next.mar==2&next.age<65&event==6 ~ 10,      #From Divorced & Cohabiting to Divorced & Not cohabiting
                          is.na(lag.mar)&next.mar==2&next.age>=65&event==6 ~ 9,      #From Widowed & Cohabiting to Widowed & Not cohabiting
                          lag.event==5&lag.mar==3&event==2 ~ 12,                     #From Widowed & Cohabiting to Married
                          lag.event==6&lag.mar==3&event==2 ~ 3,                      #From Widowed & Not Cohabiting to Married
                          lag.event==3&event==2 ~ 3,                                 #From Widowed & Not Cohabiting to Married
                          lag.event==5&lag.mar==4&event==2 ~ 13,                     #From Divorced & Cohabiting to Married
                          lag.event==6&lag.mar==4&event==2 ~ 4,                      #From Divorced & Not Cohabiting to Married
                          lag.event==4&event==2 ~ 4,                                 #From Divorced & Not Cohabiting to Married
                          lag.mar==3&event==5 ~ 6,                                   #From Widowed & Not Cohabiting to  Widowed & Cohabiting
                          lag.mar==3&event==6 ~ 9,                                   #From Widowed & Cohabiting to Widowed & Not cohabiting
                          lag.mar==4&event==5 ~ 7,                                   #From Divorced & Not Cohabiting to Divorced & Cohabiting
                          lag.mar==4&event==6 ~ 10,                                  #From Divorced & Cohabiting to Divorced & Not cohabiting
                          TRUE ~ NA_real_)
  detach(data)

  data <- data %>% mutate(new_index = row_number()) %>%
    mutate(events = ifelse(rowSums(is.na(cbind(event, m)))==2&n()==1, 0, n())) %>%
    mutate(m = replace(m,TRUE,ceiling(m))) %>%
    select(-event, -lag.mar, -lag.event, -next.age, -next.mar) %>%
    relocate(events, .after = bMonth) %>%
    relocate(type7, .after = m)

  names(data)[4] <- "event"

  return(data)
}

##------Fertility-------
screen2.birth.ym <- function(data) {

  #Keep only birth events
  data <- data %>%
    mutate(y = replace(y, which(event != 9), NA)) %>%
    mutate(m = replace(m, which(event != 9), NA)) %>%
    mutate(event = replace(event, which(event != 9), NA)) %>%
    arrange(ID, y, m, event) %>%
    group_by(ID) %>%
    mutate(new_index = row_number()) %>%
    filter(rowSums(is.na(cbind(event, y, m)))<3 | new_index==1)

  #Check event time logical or not: 1. between birthday and interview time
  data <- data %>%
    mutate(err5 = ifelse((y*12+m)>(tYear*12+tMonth)|(y*12+m)<(bYear*12+bMonth), 5, NA))

  #Check event time logical or not: > 8 months between different parities
  data <- data %>% group_by(ID) %>% arrange(ID, y, m) %>%
    mutate(lag.y = lag(y)) %>%
    mutate(lag.m = lag(m)) %>%
    mutate(err6 = ifelse(((lag.y*12+lag.m)!=(y*12+m)) & ((y*12+m)-(lag.y*12+lag.m))<8, 6, NA)) %>%
    select(-lag.y, -lag.m)

  #Check if female or not
  data <- data %>% mutate(err12 = ifelse(sex!=2, 12, NA))

  data <- data %>%
    relocate(err1, err2, err3, err4, err5, err6, err7, err8, err9, err10, err11, err12,
             .after = bMonth) %>% select(1:29)

  return(data)
}

screen2.birth.cmc <- function(data) {

  #Keep only birth events
  data <- data %>%
    mutate(m = replace(m, which(event != 9), NA)) %>%
    mutate(event = replace(event, which(event != 9), NA)) %>%
    arrange(ID, m, event) %>%
    group_by(ID) %>%
    mutate(new_index = row_number()) %>%
    filter(rowSums(is.na(cbind(event, m)))<2 | new_index==1)

  #Check event time logical or not: 1. between birthday and interview time
  data <- data %>%
    mutate(err5 = ifelse(m>tMonth|m<bMonth, 5, NA))

  #Check event time logical or not: > 8 months between different parities
  data <- data %>% group_by(ID) %>% arrange(ID, m) %>%
    mutate(lag.m = lag(m)) %>%
    mutate(err6 = ifelse((lag.m!=m) & (m-lag.m)<8, 6, NA)) %>%
    select(-lag.m)

  #Check if female or not
  data <- data %>% mutate(err12 = ifelse(sex!=2, 12, NA))

  data <- data %>%
    relocate(err1, err2, err3, err4, err5, err6, err7, err8, err9, err10, err11, err12,
             .after = bMonth) %>% select(1:26)

  return(data)
}

screen2.birthM.ym <- function(data, cohabit) {
  birth <- screen2.birth.ym(data) %>% select(-new_index)
  if (cohabit==0){
    mar <- screen2.mar4.ym(data)
  } else {
    mar <- screen2.mar7.ym(data)
  }

  data <- rbind(birth, mar) %>% arrange(ID, y, m) %>%
    mutate(err12 = ifelse(sex!=2, 12, NA)) %>%
    filter(!(is.na(event)&is.na(y)&is.na(m)&duplicated(cbind(ID, event, y, m))))
  data <- data %>% group_by(ID) %>% arrange(ID, event_index, y, m) %>%
    filter(!(!is.na(lag(event_index))&event_index==lag(event_index)&is.na(event)&is.na(y)&is.na(m)))

  rm(mar)
  rm(birth)
  return(data)
}

screen2.birthM.cmc <- function(data, cohabit) {
  birth <- screen2.birth.cmc(data) %>% select(-new_index)
  if (cohabit==0){
    mar <- screen2.mar4.cmc(data)
  } else {
    mar <- screen2.mar7.cmc(data)
  }

  data <- rbind(birth, mar) %>% arrange(ID, m) %>%
    mutate(err12 = ifelse(sex!=2, 12, NA)) %>%
    filter(!(is.na(event)&is.na(m)&duplicated(cbind(ID, event, m))))
  data <- data %>% group_by(ID) %>% arrange(ID, event_index, m) %>%
    filter(!(event_index==lag(event_index)&is.na(event)&is.na(m)))

  rm(mar)
  rm(birth)
  return(data)
}

recode.birth <- function(data, nr2, fertM, cohabit){
  if (fertM==0){
    data <- data %>% group_by(ID) %>%
      arrange(ID, m) %>% mutate(new_index = row_number()) %>%
      mutate(events = ifelse(rowSums(is.na(cbind(event, m)))==2&n()==1, 0, n()))
    data <- data %>% mutate(event = ifelse(is.na(event), NA_real_, parity-events+new_index))
    data$mar.bf = ifelse(is.na(data$event), NA, 0)
  } else if (fertM==1&cohabit==0) {
    birth <- data %>%
      mutate(m = replace(m, which(event %in% c(1:4)), NA)) %>%
      mutate(event = replace(event, which(event %in% c(1:4)), NA)) %>%
      arrange(ID, m, event) %>%
      group_by(ID) %>%
      mutate(new_index = row_number()) %>%
      filter(rowSums(is.na(cbind(event, m)))<2 | new_index==1) %>%
      mutate(events = ifelse(rowSums(is.na(cbind(event, m)))==2&n()==1, 0, n()))
    birth <- birth %>% mutate(new_event = ifelse(is.na(event), NA_real_, parity-events+new_index)) %>%
      relocate(events, .after = bMonth) %>%
      relocate(new_event, .after = event)

    data$events <- NA_real_
    data <- data %>% relocate(events, .after = bMonth) %>% filter(event!=9) %>%
      mutate(new_event = event+100) %>%
      relocate(new_event, .after = event)
    data <- rbind(data, birth) %>% group_by(ID) %>% arrange(ID, new_event, m) %>%
      fill(events, .direction = "downup") %>% arrange(ID, m, event) %>%
      mutate(new_index = row_number()) %>%
      filter(!(is.na(event)&is.na(m)&new_index!=1))
    rm(birth)

    data <- data %>% group_by(ID) %>%
      arrange(ID, m, event) %>% mutate(new_index = row_number()) %>%
      mutate(lag.mar = ifelse(lag(new_event) %in% c(101:104), lag(new_event), NA)) %>%
      fill(lag.mar) %>%
      mutate(next.mar = ifelse(lead(new_event) %in% c(101:104), lead(new_event), NA)) %>%
      mutate(next.age = ifelse(lead(new_event) %in% c(101:104), (lead(m)-bMonth)/12, NA)) %>%
      fill(next.mar, next.age, .direction = "up")
    data <- data %>% group_by(ID) %>%
      arrange(ID, m) %>%
      mutate(mar.bf = dplyr::case_when(lag.mar==103 ~ 3,
                                lag.mar==104 ~ 4,
                                lag.mar==101&new_event<100 ~ 2,
                                lag.mar==102&new_event<100 ~ 2,
                                new_event==101 ~ 1,
                                new_event==103 ~ 2,
                                new_event==104 ~ 2,
                                is.na(lag.mar)&new_event==102&(m-bMonth)/12<65 ~ 4,
                                is.na(lag.mar)&new_event==102&(m-bMonth)/12>=65 ~ 3,
                                is.na(lag.mar)&next.mar==101&new_event<100 ~ 1,
                                is.na(lag.mar)&next.mar==102&next.age<65&new_event<100 ~ 4,
                                is.na(lag.mar)&next.mar==102&next.age>=65&new_event<100 ~ 3,
                                is.na(lag.mar)&next.mar==103&new_event<100 ~ 2,
                                is.na(lag.mar)&next.mar==104&new_event<100 ~ 2,
                                is.na(lag.mar)&is.na(next.mar)&new_event<100 ~ mar,
                                TRUE ~ NA_real_)) %>%
      relocate(mar.bf, .after = new_event) %>%
      select(-lag.mar, -next.mar, -next.age)

    data <- data %>% select(-event)
    names(data)[4] <- "event"
  } else if (fertM==1&cohabit==1) {
    birth <- data %>%
      mutate(m = replace(m, which(event %in% c(1:6)), NA)) %>%
      mutate(event = replace(event, which(event %in% c(1:6)), NA)) %>%
      arrange(ID, m, event) %>%
      group_by(ID) %>%
      mutate(new_index = row_number()) %>%
      filter(rowSums(is.na(cbind(event, m)))<2 | new_index==1) %>%
      mutate(events = ifelse(rowSums(is.na(cbind(event, m)))==2&n()==1, 0, n()))
    birth <- birth %>% mutate(new_event = ifelse(is.na(event), NA_real_, parity-events+new_index)) %>%
      relocate(events, .after = bMonth) %>%
      relocate(new_event, .after = event)

    data$events <- NA_real_
    data <- data %>% relocate(events, .after = bMonth) %>% filter(event!=9) %>%
      mutate(new_event = event+100) %>%
      relocate(new_event, .after = event)
    data <- rbind(data, birth) %>% group_by(ID) %>% arrange(ID, new_event, m) %>%
      fill(events, .direction = "downup") %>% arrange(ID, m, event) %>%
      mutate(new_index = row_number()) %>%
      filter(!(is.na(event)&is.na(m)&new_index!=1))
    rm(birth)

    data <- data %>% group_by(ID) %>%
      arrange(ID, m, event) %>% mutate(new_index = row_number()) %>%
      mutate(lag.mar = ifelse(lag(new_event) %in% c(101:104), lag(new_event), NA)) %>%
      fill(lag.mar) %>%
      mutate(next.mar = ifelse(lead(new_event) %in% c(101:104), lead(new_event), NA)) %>%
      mutate(next.mar.age = ifelse(lead(new_event) %in% c(101:104), (lead(m)-bMonth)/12, NA)) %>%
      fill(next.mar, next.mar.age, .direction = "up") %>%
      mutate(lag.union = ifelse(lag(new_event) %in% c(105:106), lag(new_event), NA)) %>%
      fill(lag.union) %>%
      mutate(next.union = ifelse(lead(new_event) %in% c(105:106), lead(new_event), NA)) %>%
      mutate(next.union.age = ifelse(lead(new_event) %in% c(105:106), (lead(m)-bMonth)/12, NA)) %>%
      fill(next.union, next.union.age, .direction = "up")

    data <- data %>% group_by(ID) %>%
      arrange(ID, m) %>%
      mutate(marbf4 = dplyr::case_when(lag.mar==103 ~ 003,
                                lag.mar==104 ~ 004,
                                lag.mar==101&new_event<100 ~ 002,
                                lag.mar==102&new_event<100 ~ 002,
                                new_event==101 ~ 001,
                                new_event==103 ~ 002,
                                new_event==104 ~ 002,
                                is.na(lag.mar)&new_event==102&(m-bMonth)/12<65 ~ 004,
                                is.na(lag.mar)&new_event==102&(m-bMonth)/12>=65 ~ 003,
                                is.na(lag.mar)&next.mar==101&new_event<100 ~ 001,
                                is.na(lag.mar)&next.mar==102&next.mar.age<65&new_event<100 ~ 004,
                                is.na(lag.mar)&next.mar==102&next.mar.age>=65&new_event<100 ~ 003,
                                is.na(lag.mar)&next.mar==103&new_event<100 ~ 002,
                                is.na(lag.mar)&next.mar==104&new_event<100 ~ 002,
                                is.na(lag.mar)&is.na(next.mar)&new_event<100 ~ mar,
                                TRUE ~ NA_real_))%>%
      relocate(marbf4, .after = new_event)

    data <- data %>% group_by(ID) %>%
      arrange(ID, m) %>%
      mutate(mar.bf = dplyr::case_when(marbf4 == 001 ~ 1,
                                marbf4 == 002 ~ 2,
                                marbf4 == 003 ~ 3,
                                marbf4 == 004 ~ 4,
                                marbf4 == 001&lag.union==106 ~ 1,
                                marbf4 == 001&lag.union==105 ~ 5,
                                marbf4 == 003&lag.union==106 ~ 3,
                                marbf4 == 003&lag.union==105 ~ 7,
                                marbf4 == 004&lag.union==106 ~ 4,
                                marbf4 == 004&lag.union==105 ~ 8,
                                marbf4 == 1&lag.union == 105 ~5,
                                marbf4 == 1&lag.union == 106 ~1,
                                marbf4 == 3&lag.union == 105 ~7,
                                marbf4 == 3&lag.union == 106 ~3,
                                marbf4 == 4&lag.union == 105 ~8,
                                marbf4 == 4&lag.union == 106 ~4,
                                marbf4 == 5&lag.union == 105 ~5,
                                marbf4 == 5&lag.union == 106 ~1,
                                marbf4 == 6&lag.union == 105 ~7,
                                marbf4 == 6&lag.union == 106 ~3,
                                marbf4 == 7&lag.union == 105 ~8,
                                marbf4 == 7&lag.union == 106 ~4,
                                #use next status
                                is.na(lag.union)&marbf4 == 001&next.union==106 ~ 5,
                                is.na(lag.union)&marbf4 == 001&next.union==105 ~ 1,
                                is.na(lag.union)&marbf4 == 003&next.union==106 ~ 7,
                                is.na(lag.union)&marbf4 == 003&next.union==105 ~ 3,
                                is.na(lag.union)&marbf4 == 004&next.union==106 ~ 8,
                                is.na(lag.union)&marbf4 == 004&next.union==105 ~ 4,
                                is.na(lag.union)&marbf4 == 1&next.union == 105 ~1,
                                is.na(lag.union)&marbf4 == 1&next.union == 106 ~5,
                                is.na(lag.union)&marbf4 == 3&next.union == 105 ~3,
                                is.na(lag.union)&marbf4 == 3&next.union == 106 ~7,
                                is.na(lag.union)&marbf4 == 4&next.union == 105 ~4,
                                is.na(lag.union)&marbf4 == 4&next.union == 106 ~8,
                                is.na(lag.union)&marbf4 == 5&next.union == 105 ~1,
                                is.na(lag.union)&marbf4 == 5&next.union == 106 ~5,
                                is.na(lag.union)&marbf4 == 6&next.union == 105 ~3,
                                is.na(lag.union)&marbf4 == 6&next.union == 106 ~7,
                                is.na(lag.union)&marbf4 == 7&next.union == 105 ~4,
                                is.na(lag.union)&marbf4 == 7&next.union == 106 ~8,
                                #new event == 105 106 not marbf4
                                marbf4 == 1&new_event == 105 ~1,
                                marbf4 == 1&new_event == 106 ~5,
                                marbf4 == 3&new_event == 105 ~3,
                                marbf4 == 3&new_event == 106 ~7,
                                marbf4 == 4&new_event == 105 ~4,
                                marbf4 == 4&new_event == 106 ~8,
                                marbf4 == 5&new_event == 105 ~1,
                                marbf4 == 5&new_event == 106 ~5,
                                marbf4 == 6&new_event == 105 ~3,
                                marbf4 == 6&new_event == 106 ~7,
                                marbf4 == 7&new_event == 105 ~4,
                                marbf4 == 7&new_event == 106 ~8,
                                new_event==105&next.mar== 101 ~ 1,
                                new_event==106&next.mar== 101 ~ 5,
                                new_event==105&next.mar== 102&(m-bMonth)/12>=65 ~ 3,
                                new_event==105&next.mar== 102&(m-bMonth)/12<65 ~ 4,
                                new_event==106&next.mar== 102&(m-bMonth)/12>=65 ~ 7,
                                new_event==106&next.mar== 102&(m-bMonth)/12<65 ~ 8,
                                is.na(lag.mar)&new_event == 105 ~1,
                                is.na(lag.mar)&new_event == 106 ~5,
                                is.na(lag.mar)&is.na(lag.union)&is.na(next.mar)&is.na(next.union)&new_event<100 ~ mar,
                                TRUE ~ NA_real_)) %>%
      relocate(mar.bf, .after = new_event) %>%
      select(-lag.mar, -next.mar, -next.mar.age, -lag.union, -next.union, -next.union.age, -marbf4)
    data
    data <- data %>% select(-event)
    names(data)[4] <- "event"   ## change name of new event to do
  }

  return(data)
}

##------Leaving Home-------
screen1.lh.ym <- function(data) {
  info <- data %>% select(region:bMonth)
  evt <- data %>% select(ID, y1:ncol(data))
  long <- reshape(evt, idvar = "ID",
                  varying = names(evt)[2:ncol(evt)],
                  v.names = c("event", "ru", "m", "y"),
                  timevar = "event_index",
                  times = 1:((ncol(evt)-1)/4),
                  new.row.names = 1:100000000000,
                  direction = "long") %>% arrange(ID, event_index)
  names(long)[3:6] <- names(long)[6:3]
  names(long)[4:5] <- names(long)[5:4]
  names(long)[names(long)=="ru"] = "live_ru"

  data <- long %>% arrange(ID, y, m, live_ru, event) %>%        #给每一个events赋一个编号newindex
    group_by(ID) %>%
    mutate(new_index = row_number()) %>%
    mutate(events = n()) %>%
    filter(rowSums(is.na(cbind(event, y, m, live_ru)))<4 | new_index==1)

  data[, 'events'] = ifelse(is.na(data$event), NA, data$events)       #自己添加定义好的events

  sys.year <- as.integer(format(Sys.Date(), "%Y"))
  sys.mon <- as.integer(format(Sys.Date(), "%m"))
  info <- info %>%
    mutate(err1 = ifelse(is.na(tYear)|is.na(tMonth),1,NA)) %>%
    mutate(err2 = ifelse(is.na(bYear)|is.na(bMonth),2,NA)) %>%
    mutate(err10 = ifelse(is.na(err1)&(tYear<1000|(tYear*12+tMonth)>(sys.year*12+sys.mon)|!(tMonth %in% 1:12)),10,NA)) %>%
    mutate(err11 = ifelse(is.na(err2)&(bYear<1000|(bYear*12+bMonth)>(sys.year*12+sys.mon)|!(bMonth %in% 1:12)|(bYear*12+bMonth)>(tYear*12+tMonth)|((tYear*12+tMonth)-(bYear*12+bMonth))>120*12),11,NA))

  data <- data %>% left_join(info, by = "ID")

  data <- data %>%
    mutate(err4 = ifelse(is.na(event)&rowSums(is.na(cbind(event, y, m)))!=3, 4, NA)) %>%
    mutate(err3 = ifelse((is.na(y)|is.na(m))&rowSums(is.na(cbind(event, y, m)))!=3, 3, NA)) %>%
    mutate(err7 = ifelse(!(is.na(event)|event %in% c(7, 8)), 7, NA)) %>%
    mutate(err8 = ifelse((!is.na(y))&y<1000, 8, NA)) %>%
    mutate(err9 = ifelse(!(is.na(m)|m %in% 1:12), 9, NA))

  data <- data %>% relocate(err1, err2, err3, err4, err7, err8, err9, err10, err11,
                            .before = new_index)
  return(data)
}

screen1.lh.cmc <- function(data) {
  info <- data %>% select(region:bMonth)
  evt <- data %>% select(ID, m1:ncol(data))
  long <- reshape(evt, idvar = "ID",
                  varying = names(evt)[2:ncol(evt)],
                  v.names = c("event", "ru", "m"),
                  timevar = "event_index",
                  times = 1:((ncol(evt)-1)/3),
                  new.row.names = 1:100000000000,
                  direction = "long") %>% arrange(ID, event_index)
  names(long)[3:5] <- names(long)[5:3]
  names(long)[names(long)=="ru"] = "live_ru"

  data <- long %>% arrange(ID, m, live_ru, event) %>%        #给每一个events赋一个编号newindex
    group_by(ID) %>%
    mutate(new_index = row_number()) %>%
    mutate(events = n()) %>%
    filter(rowSums(is.na(cbind(event, m, live_ru)))<3 | new_index==1)

  data[, 'events'] = ifelse(is.na(data$event), NA, data$events)       #自己添加定义好的events

  sys.year <- as.integer(format(Sys.Date(), "%Y"))
  sys.mon <- as.integer(format(Sys.Date(), "%m"))
  sys.cmc <- (sys.year-1900)*12+sys.mon
  info <- info %>%
    mutate(err1 = ifelse(is.na(tMonth),1,NA)) %>%
    mutate(err2 = ifelse(is.na(bMonth),2,NA)) %>%
    mutate(err10 = ifelse(is.na(err1)&(tMonth>sys.cmc),10,NA)) %>%
    mutate(err11 = ifelse(is.na(err2)&(bMonth>sys.cmc|bMonth>tMonth|(tMonth-bMonth)>120*12),11,NA))

  data <- data %>% left_join(info, by = "ID")

  data <- data %>%
    mutate(err4 = ifelse(is.na(event)&rowSums(is.na(cbind(event, m)))!=2, 4, NA)) %>%
    mutate(err3 = ifelse(is.na(m)&rowSums(is.na(cbind(event, m)))!=2, 3, NA)) %>%
    mutate(err7 = ifelse(!(is.na(event)|event %in% c(7, 8)), 7, NA)) %>%
    mutate(err8 = NA) %>%
    mutate(err9 = NA)

  data <- data %>% relocate(err1, err2, err3, err4, err7, err8, err9, err10, err11,
                            .before = new_index)
  return(data)
}

screen2.leave.ym <- function(data) {

  #Keep only leaving or returning events
  data <- data %>%
    mutate(y = replace(y, which(event != 7 & event != 8), NA)) %>%
    mutate(m = replace(m, which(event != 7 & event != 8), NA)) %>%
    mutate(event = replace(event, which(event != 7 & event != 8), NA)) %>%
    arrange(ID, y, m, event) %>%
    group_by(ID) %>%
    mutate(new_index = row_number()) %>%
    filter(rowSums(is.na(cbind(event, y, m, live_ru)))<4 | new_index==1)

  #Check duplicates with the same event time and type
  data_dup <- data %>%
    filter(duplicated(cbind(ID, event, y, m))) %>%
    mutate(err12 = 12)
  data_nodup <- data %>%
    filter(!duplicated(cbind(ID, event, y, m))) %>%
    mutate(err12 = NA) %>%
    select(-new_index) %>%
    group_by(ID) %>%
    mutate(new_index = row_number()) %>%
    arrange(ID, new_index)

  #Check duplicates within half a year
  dup_check <- data_nodup %>% group_by(ID) %>%
    mutate(err12 = replace(err12, which(event==lag(event) & ((y*12+m)-(lag(y)*12+lag(m)))<=6), 12))
  data_dup2 <-  dup_check %>% filter(err12==12)
  data_nodup <- dup_check %>% filter(is.na(err12))
  data_dup <- rbind(data_dup, data_dup2)
  rm(dup_check)
  rm(data_dup2)

  #Check event time logic
  data_nodup <- data_nodup %>%
    mutate(err5 = ifelse((y*12+m)>(tYear*12+tMonth)|(y*12+m)<(bYear*12+bMonth), 5, NA))

  #Check event type logic
  data_nodup <- data_nodup %>% group_by(ID) %>%
    mutate(lag.event = lag(event)) %>%                          #第一条数据的lag是NA，最后一条数据lead是NA
    mutate(next.event = lead(event)) %>%
    mutate(leave_check = ifelse((lag.event==7) &
                                  (event == 7), FALSE, TRUE)) %>%     #未实现，有没有跟现在在家状态有冲突的event，我想的是第一次是什么event要不要检查一下
    mutate(return_check = ifelse((lag.event==8) &
                                   (event == 8), FALSE, TRUE))
  data_nodup <- data_nodup %>%
    mutate(err6 = ifelse(leave_check==FALSE|return_check==FALSE, 6, NA)) %>%
    select(-lag.event, -next.event, -leave_check, -return_check)

  data <- rbind(data_nodup, data_dup)
  rm(data_nodup)
  rm(data_dup)
  data <- data %>%
    relocate(err1, err2, err3, err4, err5, err6, err7, err8, err9, err10, err11, err12,
             .before = new_index) %>% select(ID:err12)

  return(data)
}

screen2.leave.cmc <- function(data) {

  #Keep only leaving or returning events
  data <- data %>%
    mutate(m = replace(m, which(event != 7 & event != 8), NA)) %>%
    mutate(event = replace(event, which(event != 7 & event != 8), NA)) %>%
    arrange(ID, m, event) %>%
    group_by(ID) %>%
    mutate(new_index = row_number()) %>%
    filter(rowSums(is.na(cbind(event, m, live_ru)))<3 | new_index==1)

  #Check duplicates with the same event time and type
  data_dup <- data %>%
    filter(duplicated(cbind(ID, event, m))) %>%
    mutate(err12 = 12)
  data_nodup <- data %>%
    filter(!duplicated(cbind(ID, event, m))) %>%
    mutate(err12 = NA) %>%
    select(-new_index) %>%
    group_by(ID) %>%
    mutate(new_index = row_number()) %>%
    arrange(ID, new_index)

  #Check duplicates within half a year
  dup_check <- data_nodup %>% group_by(ID) %>%
    mutate(err12 = replace(err12, which(event==lag(event) & (m-lag(m))<=6), 12))
  data_dup2 <-  dup_check %>% filter(err12==12)
  data_nodup <- dup_check %>% filter(is.na(err12))
  data_dup <- rbind(data_dup, data_dup2)
  rm(dup_check)
  rm(data_dup2)

  #Check event time logic
  data_nodup <- data_nodup %>%
    mutate(err5 = ifelse(m>tMonth|m<bMonth, 5, NA))

  #Check event type logic
  data_nodup <- data_nodup %>% group_by(ID) %>%
    mutate(lag.event = lag(event)) %>%                          #第一条数据的lag是NA，最后一条数据lead是NA
    mutate(next.event = lead(event)) %>%
    mutate(leave_check = ifelse((lag.event==7) &
                                  (event == 7), FALSE, TRUE)) %>%     #未实现，有没有跟现在在家状态有冲突的event，我想的是第一次是什么event要不要检查一下
    mutate(return_check = ifelse((lag.event==8) &
                                   (event == 8), FALSE, TRUE))
  data_nodup <- data_nodup %>%
    mutate(err6 = ifelse(leave_check==FALSE|return_check==FALSE, 6, NA)) %>%
    select(-lag.event, -next.event, -leave_check, -return_check)

  data <- rbind(data_nodup, data_dup)
  rm(data_nodup)
  rm(data_dup)
  data <- data %>%
    relocate(err1, err2, err3, err4, err5, err6, err7, err8, err9, err10, err11, err12,
             .before = new_index) %>% select(ID:err12)

  return(data)
}

recode.lh <- function(data) {
  data <- data %>% group_by(ID) %>%
    arrange(ID, m) %>%
    mutate(lh = dplyr::case_when(event==7 ~ 7,
                          event==8 ~ 8,
                          TRUE ~ NA_real_)) %>%
    mutate(new_index = row_number()) %>%    #这样就计算在同一个ID下，这是第几个
    mutate(events = ifelse(rowSums(is.na(cbind(event, m)))==2&n()==1, 0, n())) %>%
    relocate(events, .after = bMonth) %>%
    relocate(lh, .after = m) %>% select(-event)
  names(data)[5] <- "event"

  return(data)
}

##------Migration------
screen1.mig.ym <- function(data,code) {

  #从name中取所有国家和地区的代码
  name <- as.data.frame(code)
  if (is.null(name$`Region code`) & is.null(name$`Country code`))  {
    abort("Error! Please input the region code and country code in Name sheet")
  }else{
    region_code <- na.omit(unique(name$`Region code`))
    country_code <- na.omit(unique(name$`Country code`))
  }
  #合成一个code
  state_code <- c(region_code, country_code)

  ##用leaving的判定,把event的代码修改
  info <- data %>% select(region:bMonth)
  evt <- data %>% select(ID, y1:ncol(data))
  long <- reshape(evt, idvar = "ID",
                  varying = names(evt)[2:ncol(evt)],
                  v.names = c("event", "ru", "m", "y"),
                  timevar = "event_index",
                  times = 1:((ncol(evt)-1)/4),
                  new.row.names = 1:100000000000,
                  direction = "long") %>% arrange(ID, event_index)
  names(long)[3:6] <- names(long)[6:3]       ### 把event idex后面的列的名字换了过来
  names(long)[4:5] <- names(long)[5:4]       ### 把 ru和m换位置
  names(long)[names(long)=="ru"] = "movebf_ru" # ru为移动之前的城乡状态


  data <- long %>% arrange(ID, y, m, movebf_ru, event) %>%        #给每一个events赋一个编号newindex，（新建了event是 和 newindex
    group_by(ID) %>%
    mutate(new_index = row_number()) %>%
    mutate(events = n()) %>%
    filter(rowSums(is.na(cbind(event, y, m, movebf_ru)))<4 | new_index==1)

  data[, 'events'] = ifelse(is.na(data$event), NA, data$events)       #自己添加定义好的events

  sys.year <- as.integer(format(Sys.Date(), "%Y"))
  sys.mon <- as.integer(format(Sys.Date(), "%m"))
  info <- info %>%
    mutate(err1 = ifelse(is.na(tYear)|is.na(tMonth),1,NA)) %>%
    mutate(err2 = ifelse(is.na(bYear)|is.na(bMonth),2,NA)) %>%
    mutate(err10 = ifelse(is.na(err1)&(tYear<1000|(tYear*12+tMonth)>(sys.year*12+sys.mon)|!(tMonth %in% 1:12)),10,NA)) %>%
    mutate(err11 = ifelse(is.na(err2)&(bYear<1000|(bYear*12+bMonth)>(sys.year*12+sys.mon)|!(bMonth %in% 1:12)|(bYear*12+bMonth)>(tYear*12+tMonth)|((tYear*12+tMonth)-(bYear*12+bMonth))>120*12),11,NA))

  data <- data %>% left_join(info, by = "ID")

  data <- data %>%
    mutate(err4 = ifelse(is.na(event)&rowSums(is.na(cbind(event, y, m)))!=3, 4, NA)) %>%
    mutate(err3 = ifelse((is.na(y)|is.na(m))&rowSums(is.na(cbind(event, y, m)))!=3, 3, NA)) %>%
    mutate(err7 = ifelse(!(is.na(event)|event %in% state_code), 7, NA)) %>%
    mutate(err8 = ifelse((!is.na(y))&y<1000, 8, NA)) %>%
    mutate(err9 = ifelse(!(is.na(m)|m %in% 1:12), 9, NA))

  data <- data %>% relocate(err1, err2, err3, err4, err7, err8, err9, err10, err11,
                            .before = new_index)
  return(data)
}

screen1.mig.cmc <- function(data, code) {
  #从name中取所有国家和地区的代码
  name <- as.data.frame(code)
  if (is.null(name$`Region code`) & is.null(name$`Country code`))  {
    print("Error! Please input the region code and country code in Name sheet")
  }else{
    region_code <- na.omit(unique(name$`Region code`))
    country_code <- na.omit(unique(name$`Country code`))
  }
  #合成一个code
  state_code <- c(region_code, country_code)

  ## todo
  info <- data %>% select(region:bMonth)
  evt <- data %>% select(ID, m1:ncol(data))
  long <- reshape(evt, idvar = "ID",
                  varying = names(evt)[2:ncol(evt)],
                  v.names = c("event", "ru", "m"),
                  timevar = "event_index",
                  times = 1:((ncol(evt)-1)/3),
                  new.row.names = 1:100000000000,
                  direction = "long") %>% arrange(ID, event_index)

  names(long)[3:5] <- names(long)[5:3]##注意看看有没有对
  names(long)[4:5] <- names(long)[5:4]
  names(long)[names(long)=="ru"] = "movebf_ru"

  long
  data <- long %>% arrange(ID, m, movebf_ru, event) %>%        #给每一个events赋一个编号newindex
    group_by(ID) %>%
    mutate(new_index = row_number()) %>%
    mutate(events = n()) %>%                                #和screen 1 ym不一样？
    filter(rowSums(is.na(cbind(event, m, movebf_ru)))<3 | new_index==1)


  sys.year <- as.integer(format(Sys.Date(), "%Y"))
  sys.mon <- as.integer(format(Sys.Date(), "%m"))
  sys.cmc <- (sys.year-1900)*12+sys.mon

  info <- info %>%
    mutate(err1 = ifelse(is.na(tMonth),1,NA)) %>%
    mutate(err2 = ifelse(is.na(bMonth),2,NA)) %>%
    mutate(err10 = ifelse(is.na(err1)&(tMonth>sys.cmc),10,NA)) %>%
    mutate(err11 = ifelse(is.na(err2)&(bMonth>sys.cmc|bMonth>tMonth|(tMonth-bMonth)>120*12),11,NA))

  data <- data %>% left_join(info, by = "ID")

  data <- data %>%
    mutate(err4 = ifelse(is.na(event)&rowSums(is.na(cbind(event, m)))!=2, 4, NA)) %>%
    mutate(err3 = ifelse(is.na(m)&rowSums(is.na(cbind(event, m)))!=2, 3, NA)) %>%
    mutate(err7 = ifelse(!(is.na(event)|event %in% state_code), 7, NA)) %>%
    mutate(err8 = NA) %>%
    mutate(err9 = NA)

  data <- data %>% relocate(err1, err2, err3, err4, err7, err8, err9, err10, err11,
                            .before = new_index)
  return(data)
}

screen2.mig.ym <- function(data,code) {

  name <- as.data.frame(code)
  region_code <- na.omit(unique(name$`Region code`))
  country_code <- na.omit(unique(name$`Country code`))
  state_code <- c(region_code, country_code)

  #Keep domestic migration or international migration events
  # event <- data$event
  # # event_tem <- ifelse(event %in% state_code, 1, 0)
  # event_tem <- which(!(event %in% state_code))
  data <- data %>%
    mutate(y = replace(y, which(!(event %in% state_code)), NA)) %>%   ##是否可以用？
    mutate(m = replace(m, which(!(event %in% state_code)), NA)) %>%
    mutate(event = replace(event, which(!(event %in% state_code)), NA)) %>%
    arrange(ID, y, m, event) %>%
    group_by(ID) %>%
    mutate(new_index = row_number()) %>%
    filter(rowSums(is.na(cbind(event, y, m, movebf_ru)))<4 | new_index==1)


  #Check duplicates with the same event time and type
  data_dup <- data %>%
    filter(duplicated(cbind(ID, event, y, m))) %>%
    mutate(err12 = 12)
  data_nodup <- data %>%
    filter(!duplicated(cbind(ID, event, y, m))) %>%
    mutate(err12 = NA) %>%
    select(-new_index) %>%
    group_by(ID) %>%
    mutate(new_index = row_number()) %>%
    arrange(ID, new_index)


  #Check duplicates within half a year
  dup_check <- data_nodup %>% group_by(ID) %>%
    mutate(err12 = replace(err12, which(event==lag(event) & ((y*12+m)-(lag(y)*12+lag(m)))<=6), 12))
  data_dup2 <-  dup_check %>% filter(err12==12)
  data_nodup <- dup_check %>% filter(is.na(err12))
  data_dup <- rbind(data_dup, data_dup2)
  rm(dup_check)
  rm(data_dup2)

  #Check event time logic
  data_nodup <- data_nodup %>%
    mutate(err5 = ifelse((y*12+m)>(tYear*12+tMonth)|(y*12+m)<(bYear*12+bMonth), 5, NA))

  #Check event type logic
  data_nodup <- data_nodup %>% group_by(ID) %>%
    mutate(lag.event = lag(event)) %>%
    mutate(next.event = lead(event)) %>%
    mutate(mig_check = ifelse(lag.event == event, FALSE, TRUE))  # to do？ 看是否和现在所在的state有冲突

  data_nodup <- data_nodup %>%
    mutate(err6 = ifelse(mig_check==FALSE, 6, NA)) %>%
    select(-lag.event, -next.event, -mig_check)

  data <- rbind(data_nodup, data_dup)
  rm(data_nodup)
  rm(data_dup)
  data <- data %>%
    relocate(err1, err2, err3, err4, err5, err6, err7, err8, err9, err10, err11, err12,
             .before = new_index) %>% select(ID:err12)

  return(data)
}

screen2.mig.cmc <- function(data, code) {

  name <- as.data.frame(code)
  region_code <- na.omit(unique(name$`Region code`))
  country_code <- na.omit(unique(name$`Country code`))
  state_code <- c(region_code, country_code)

  #Keep domestic migration or international migration events

  data <- data %>%
    mutate(m = replace(m, which(!(event %in% state_code)), NA)) %>%
    mutate(event = replace(event, which(!(event %in% state_code)), NA)) %>%
    arrange(ID,  m, event) %>%
    group_by(ID) %>%
    mutate(new_index = row_number()) %>%
    filter(rowSums(is.na(cbind(event, m, movebf_ru)))<3 | new_index==1)


  #Check duplicates with the same event time and type
  data_dup <- data %>%
    filter(duplicated(cbind(ID, event, m))) %>%
    mutate(err12 = 12)
  data_nodup <- data %>%
    filter(!duplicated(cbind(ID, event, m))) %>%
    mutate(err12 = NA) %>%
    select(-new_index) %>%
    group_by(ID) %>%
    mutate(new_index = row_number()) %>%
    arrange(ID, new_index)


  #Check duplicates within half a year
  dup_check <- data_nodup %>% group_by(ID) %>%
    mutate(err12 = replace(err12, which(event==lag(event) & (m-lag(m))<=6), 12))
  data_dup2 <-  dup_check %>% filter(err12==12)
  data_nodup <- dup_check %>% filter(is.na(err12))
  data_dup <- rbind(data_dup, data_dup2)
  rm(dup_check)
  rm(data_dup2)

  #Check event time logic
  data_nodup <- data_nodup %>%
    mutate(err5 = ifelse(m>tMonth|m<bMonth, 5, NA))

  #Check event type logic
  data_nodup <- data_nodup %>% group_by(ID) %>%
    mutate(lag.event = lag(event)) %>%
    mutate(next.event = lead(event)) %>%
    mutate(mig_check = ifelse((lag.event == event), FALSE, TRUE)) # to do？ 看是否和现在所在的state有冲突

  data_nodup <- data_nodup %>%
    mutate(err6 = ifelse(mig_check==FALSE, 6, NA)) %>%
    select(-lag.event, -next.event, -mig_check)

  data <- rbind(data_nodup, data_dup)
  rm(data_nodup)
  rm(data_dup)
  data <- data %>%
    relocate(err1, err2, err3, err4, err5, err6, err7, err8, err9, err10, err11, err12,
             .before = new_index) %>% select(ID:err12)

  return(data)
}

recode.mig <- function(data, code) {
  #取地区代码
  name <- as.data.frame(code)
  region_code <- na.omit(unique(name$`Region code`))
  country_code <- na.omit(unique(name$`Country code`))
  state_code <- c(region_code, country_code)

  #增加events列
  data <- data %>% group_by(ID) %>%
    arrange(ID, m, event) %>%
    mutate(new_index = row_number()) %>%    #这样就计算在同一个ID下，这是第几个
    mutate(events = ifelse(rowSums(is.na(cbind(event, m)))==2&n()==1, 0, n())) %>%
    relocate(events, .after = bMonth)

  ##找出move的前后的state
  data <- data %>% group_by(ID) %>%
    arrange(ID, m, event) %>% mutate(new_index = row_number()) %>%
    mutate(lag.state = lag(event)) %>%
    fill(lag.state) %>%
    mutate(next.state = lead(event)) %>%
    fill(next.state, .direction = "up")


  data <- data %>% group_by(ID) %>%
    arrange(ID, m) %>%
    mutate(move.bf = event) %>%
    mutate(move.af = dplyr::case_when(!(is.na(next.state)) ~ next.state,
                               is.na(next.state) & !(is.na(move.bf)) ~ region,
                               TRUE ~ NA_real_))%>%
    relocate(move.bf, .after = event) %>%
    relocate(move.af, .after = move.bf) %>%
    select(-lag.state, -next.state)


  # #找出前后的ru状态 ##movebf_ru 得是迁移前的城乡状态 ！！ todo再进行核查

  data <- data %>% group_by(ID) %>%
    arrange(ID, m, event) %>% mutate(new_index = row_number()) %>%
    mutate(lag.bfru = lag(movebf_ru)) %>%
    fill(lag.bfru) %>%
    mutate(next.bfru = lead(movebf_ru)) %>%
    fill(next.bfru, .direction = "up")


  data <- data %>% group_by(ID) %>%
    arrange(ID, m) %>%
    mutate(moveaf_ru = dplyr::case_when(!(is.na(next.bfru)) ~ next.bfru,
                                 is.na(next.bfru) & !(is.na(movebf_ru )) ~ ru,
                                 TRUE ~ NA_real_))%>%
    relocate(moveaf_ru, .after = movebf_ru) %>%
    select(-lag.bfru, -next.bfru)

  data
  return(data)
}

##------General-------

screen1.def.ym <- function(data,code) {

  #从name中取所有status
  name <- as.data.frame(code)
  if (is.null(name$`Event code`) & is.null(name$`Status code`))  {
    abort("Error! Please input the event code and status code in Name sheet")
  }else{
    event_code <- na.omit(unique(name$`Event code`))
    status_code <- na.omit(unique(name$`Status code`))
    len_ec <- nchar(status_code[1])
  }

  ## todo, check all event lengths are the same



  info <- data %>% select(region:bMonth)
  evt <- data %>% select(ID, y1:ncol(data))
  long <- reshape(evt, idvar = "ID",
                  varying = names(evt)[2:ncol(evt)],
                  v.names = c("event", "m", "y"),
                  timevar = "event_index",
                  times = 1:((ncol(evt)-1)/3),
                  new.row.names = 1:100000000000,
                  direction = "long") %>% arrange(ID, event_index)
  names(long)[3:5] <- names(long)[5:3]

  ##拆解event代码
  long <- long %>% mutate("status_bf" = floor(event/(10**len_ec))) %>%
    mutate("status_af" = event%%(10**len_ec))

  long
  data <- long %>% arrange(ID, y, m, status_bf, status_af, event) %>%        #给每一个events赋一个编号newindex，（新建了event是 和 newindex
    group_by(ID) %>%
    mutate(new_index = row_number()) %>%
    filter(rowSums(is.na(cbind(event, y, m, status_bf, status_af)))<5 | new_index==1) %>%
    mutate(events = n())

  data[, 'events'] = ifelse(is.na(data$event), NA, data$events)       #自己添加定义好的events

  sys.year <- as.integer(format(Sys.Date(), "%Y"))
  sys.mon <- as.integer(format(Sys.Date(), "%m"))
  info <- info %>%
    mutate(err1 = ifelse(is.na(tYear)|is.na(tMonth),1,NA)) %>%
    mutate(err2 = ifelse(is.na(bYear)|is.na(bMonth),2,NA)) %>%
    mutate(err10 = ifelse(is.na(err1)&(tYear<1000|(tYear*12+tMonth)>(sys.year*12+sys.mon)|!(tMonth %in% 1:12)),10,NA)) %>%
    mutate(err11 = ifelse(is.na(err2)&(bYear<1000|(bYear*12+bMonth)>(sys.year*12+sys.mon)|!(bMonth %in% 1:12)|(bYear*12+bMonth)>(tYear*12+tMonth)|((tYear*12+tMonth)-(bYear*12+bMonth))>120*12),11,NA))

  data <- data %>% left_join(info, by = "ID")

  data <- data %>%
    mutate(err4 = ifelse(is.na(event)&rowSums(is.na(cbind(event, y, m)))!=3, 4, NA)) %>%
    mutate(err3 = ifelse((is.na(y)|is.na(m))&rowSums(is.na(cbind(event, y, m)))!=3, 3, NA)) %>%
    mutate(err7 = ifelse(!(is.na(event)|event %in% event_code), 7, NA)) %>%
    mutate(err8 = ifelse((!is.na(y))&y<1000, 8, NA)) %>%
    mutate(err9 = ifelse(!(is.na(m)|m %in% 1:12), 9, NA))

  data <- data %>% relocate(err1, err2, err3, err4, err7, err8, err9, err10, err11,
                            .before = new_index)
  return(data)
}

screen2.def.ym <- function(data,code) {

  name <- as.data.frame(code)
  event_code <- na.omit(unique(name$`Event code`))
  status_code <- na.omit(unique(name$`Status code`))

  #Keep events

  data <- data %>%
    mutate(y = replace(y, which(!(event %in% event_code)), NA)) %>%
    mutate(m = replace(m, which(!(event %in% event_code)), NA)) %>%
    mutate(event = replace(event, which(!(event %in% event_code)), NA)) %>%
    mutate(status_bf = replace(status_bf, which(!(event %in% event_code)), NA)) %>%
    mutate(status_af = replace(status_af, which(!(event %in% event_code)), NA)) %>%
    arrange(ID, y, m, event) %>%
    group_by(ID) %>%
    mutate(new_index = row_number()) %>%
    filter(rowSums(is.na(cbind(event, y, m, status_bf, status_af)))<5 | new_index==1)


  #Check duplicates with the same event time and type
  data_dup <- data %>%
    filter(duplicated(cbind(ID, event, y, m))) %>%
    mutate(err12 = 12)
  data_nodup <- data %>%
    filter(!duplicated(cbind(ID, event, y, m))) %>%
    mutate(err12 = NA) %>%
    select(-new_index) %>%
    group_by(ID) %>%
    mutate(new_index = row_number()) %>%
    arrange(ID, new_index)


  #Check duplicates within half a year
  dup_check <- data_nodup %>% group_by(ID) %>%
    mutate(err12 = replace(err12, which(event==lag(event) & ((y*12+m)-(lag(y)*12+lag(m)))<=6), 12))
  data_dup2 <-  dup_check %>% filter(err12==12)
  data_nodup <- dup_check %>% filter(is.na(err12))
  data_dup <- rbind(data_dup, data_dup2)
  rm(dup_check)
  rm(data_dup2)

  #Check event time logic
  data_nodup <- data_nodup %>%
    mutate(err5 = ifelse((y*12+m)>(tYear*12+tMonth)|(y*12+m)<(bYear*12+bMonth), 5, NA))

  #Check event type logic(前后两个event可以一样，但是status不能一样，比较前后的转台是否一样)
  data_nodup <- data_nodup %>% group_by(ID) %>%
    mutate(def_check = ifelse(status_af == status_bf, FALSE, TRUE))

  data_nodup <- data_nodup %>%
    mutate(err6 = ifelse(def_check==FALSE, 6, NA)) %>%
    select(-def_check)

  data <- rbind(data_nodup, data_dup)
  rm(data_nodup)
  rm(data_dup)
  data <- data %>%
    relocate(err1, err2, err3, err4, err5, err6, err7, err8, err9, err10, err11, err12,
             .before = new_index) %>% select(ID:err12)

  return(data)
}

screen1.def.cmc <- function(data,code) {

  #从name中取所有status
  name <- as.data.frame(code)
  if (is.null(name$`Event code`) & is.null(name$`Status code`))  {
    abort("Error! Please input the event code and status code in Name sheet")
  }else{
    event_code <- na.omit(unique(name$`Event code`))
    status_code <- na.omit(unique(name$`Status code`))
    len_ec <- nchar(status_code[1])
  }

  ## todo, check all event lengths are the same



  info <- data %>% select(region:bMonth)
  evt <- data %>% select(ID, m1:ncol(data))
  long <- reshape(evt, idvar = "ID",
                  varying = names(evt)[2:ncol(evt)],
                  v.names = c("event", "m"),
                  timevar = "event_index",
                  times = 1:((ncol(evt)-1)/2),
                  new.row.names = 1:100000000000,
                  direction = "long") %>% arrange(ID, event_index)
  names(long)[3:4] <- names(long)[4:3]

  ##拆解event代码
  long <- long %>% mutate("status_bf" = floor(event/(10**len_ec))) %>%
    mutate("status_af" = event%%(10**len_ec))

  data <- long %>% arrange(ID, m, status_bf, status_af, event) %>%        #给每一个events赋一个编号newindex，（新建了event是 和 newindex
    group_by(ID) %>%
    mutate(new_index = row_number()) %>%
    filter(rowSums(is.na(cbind(event, m, status_bf, status_af)))<4 | new_index==1) %>%
    mutate(events = n())

  data[, 'events'] = ifelse(is.na(data$event), NA, data$events)       #自己添加定义好的events

  sys.year <- as.integer(format(Sys.Date(), "%Y"))
  sys.mon <- as.integer(format(Sys.Date(), "%m"))
  sys.cmc <- (sys.year-1900)*12+sys.mon
  info <- info %>%
    mutate(err1 = ifelse(is.na(tMonth),1,NA)) %>%
    mutate(err2 = ifelse(is.na(bMonth),2,NA)) %>%
    mutate(err10 = ifelse(is.na(err1)&(tMonth>sys.cmc),10,NA)) %>%
    mutate(err11 = ifelse(is.na(err2)&(bMonth>sys.cmc|bMonth>tMonth|(tMonth-bMonth)>120*12),11,NA))

  data <- data %>% left_join(info, by = "ID")

  data <- data %>%
    mutate(err4 = ifelse(is.na(event)&rowSums(is.na(cbind(event, m)))!=2, 4, NA)) %>%
    mutate(err3 = ifelse(is.na(m)&rowSums(is.na(cbind(event, m)))!=2, 3, NA)) %>%
    mutate(err7 = ifelse(!(is.na(event)|event %in% event_code), 7, NA)) %>%
    mutate(err8 = NA) %>%
    mutate(err9 = NA)

  data <- data %>% relocate(err1, err2, err3, err4, err7, err8, err9, err10, err11,
                            .before = new_index)
  return(data)
}

screen2.def.cmc <- function(data,code) {

  name <- as.data.frame(code)
  event_code <- na.omit(unique(name$`Event code`))
  status_code <- na.omit(unique(name$`Status code`))

  #Keep events

  data <- data %>%
    mutate(m = replace(m, which(!(event %in% event_code)), NA)) %>%
    mutate(event = replace(event, which(!(event %in% event_code)), NA)) %>%
    mutate(status_bf = replace(status_bf, which(!(event %in% event_code)), NA)) %>%
    mutate(status_af = replace(status_af, which(!(event %in% event_code)), NA)) %>%
    arrange(ID, m, event) %>%
    group_by(ID) %>%
    mutate(new_index = row_number()) %>%
    filter(rowSums(is.na(cbind(event, m, status_bf, status_af)))<4 | new_index==1)


  #Check duplicates with the same event time and type
  data_dup <- data %>%
    filter(duplicated(cbind(ID, event, m))) %>%
    mutate(err12 = 12)
  data_nodup <- data %>%
    filter(!duplicated(cbind(ID, event, m))) %>%
    mutate(err12 = NA) %>%
    select(-new_index) %>%
    group_by(ID) %>%
    mutate(new_index = row_number()) %>%
    arrange(ID, new_index)


  #Check event time logic
  data_nodup <- data_nodup %>%
    mutate(err5 = ifelse(m>tMonth|m<bMonth, 5, NA))

  #Check event type logic(前后两个event可以一样，但是status不能一样，比较前后的转台是否一样)
  data_nodup <- data_nodup %>% group_by(ID) %>%
    mutate(def_check = ifelse(status_af == status_bf, FALSE, TRUE))

  data_nodup <- data_nodup %>%
    mutate(err6 = ifelse(def_check==FALSE, 6, NA)) %>%
    select(-def_check)

  data <- rbind(data_nodup, data_dup)
  rm(data_nodup)
  rm(data_dup)
  data <- data %>%
    relocate(err1, err2, err3, err4, err5, err6, err7, err8, err9, err10, err11, err12,
             .before = new_index) %>% select(ID:err12)

  return(data)
}


##------Clear functions-------
clear.mar <- function(data, param, code, csv, dobm, dob, intw, eventm, dup){
  cohabit <- as.numeric(param$cohabit)
  CMC <- as.numeric(param$CMC)
  output.path <- getwd()
  title <- as.character(param$title)
  param$t1Month <- as.numeric(param$t1Month)
  param$t2Month <- as.numeric(param$t2Month)
  param$t1Year <- as.numeric(param$t1Year)
  param$t2Year <- as.numeric(param$t2Year)

  #convert marital status into 4 marital status if 5,7,8 in data
  if (cohabit==0){
    data$mar <- ifelse(data$mar==5, 1,
                       ifelse(data$mar==7, 3,
                              ifelse(data$mar==8, 4, data$mar)))
  }

  if (CMC==0) {

    #Screen 1: Basic validation
    data <- screen1.ym(data)

    if (cohabit==0) {
      name <- paste0(title, ", 4 Marital Status", sep = "")
      #Screen 2: Duplicates and logic detectors
      data <- screen2.mar4.ym(data)

      error_data <- data %>%
        filter(rowSums(!is.na(cbind(err1, err2, err3, err4, err5, err6, err7,
                                    err8, err9, err10, err11, err12)), na.rm = TRUE)>0) %>%
        select(c(ID:event, err1:err12))
      description <- define.descrip(mode = "mar")
      error <- error.summary(error_data, description)

      if (sum(error$err.sum$count)>0){
        if (sum(dob, dobm, intw, eventm, dup) > 0) {
          correction <- correction.ym(data, dob, dobm, intw, eventm, dup)
          data <- correction$data
          write.correction.report(error, correction, name)
        } else {
          write.error.report(error, name)
        }
        data <- ym.to.cmc(data)
        data <- recode.mar4(data)

        data <- data %>% select(region, ID, weight, race, ru, sex, mar, edu, tMonth, bMonth,
                                events, m, event, new_index) %>%
          pivot_wider(names_from = c(new_index), values_from = c(m, event), names_sep = "")
        param$CMC <- 1

        #re-arrange column names
        idx_m <- which(names(data)=="m1")
        idx_e <- which(names(data)=="event1")
        max <- max(data$events)
        temp <- 0
        for (i in 1:max) {
          temp = (i-1)*2
          data <- data %>% relocate(names(data)[idx_e+i-1], .after = names(data)[idx_m+temp])
        }

        if (is.data.frame(code)){
          sheets <- list("Parameters" = param, "Person Record" = data, "Name" = code)
        } else {
          sheets <- list("Parameters" = param, "Person Record" = data)
        }
      } else {
        data <- ym.to.cmc(data)
        data <- recode.mar4(data)

        data <- data %>% select(region, ID, weight, race, ru, sex, mar, edu, tMonth, bMonth,
                                events, m, event, new_index) %>%
          pivot_wider(names_from = c(new_index), values_from = c(m, event), names_sep = "")
        param$CMC <- 1

        #re-arrange column names
        idx_m <- which(names(data)=="m1")
        idx_e <- which(names(data)=="event1")
        max <- max(data$events)
        temp <- 0
        for (i in 1:max) {
          temp = (i-1)*2
          data <- data %>% relocate(names(data)[idx_e+i-1], .after = names(data)[idx_m+temp])
        }

        if (is.data.frame(code)){
          sheets <- list("Parameters" = param, "Person Record" = data, "Name" = code)
        } else {
          sheets <- list("Parameters" = param, "Person Record" = data)
        }
        print("There is no error.")
      }
      if (csv==FALSE){
        write_xlsx(sheets, paste(output.path, paste("/", title, " Mar4 Clear.xlsx", sep = ""), sep = ""))
      } else {
        write.csv(data, paste(output.path, paste("/", title, " Mar4 Clear.csv", sep = ""), sep = ""), col.names = T, row.names = F, na = "")
      }
    } else if (cohabit==1) {
      name <- paste0(title, ", 7 Marital Status", sep = "")
      #Screen 2: Duplicates and logic detectors
      data <- screen2.mar7.ym(data)
      error_data <- data %>%
        filter(rowSums(!is.na(cbind(err1, err2, err3, err4, err5, err6, err7,
                                    err8, err9, err10, err11, err12)), na.rm = TRUE)>0) %>%
        select(c(ID:event, err1:err12))
      description <- define.descrip(mode = "mar")
      error <- error.summary(error_data, description)

      if (sum(error$err.sum$count)>0){
        if (sum(dob, dobm, intw, eventm, dup) > 0) {
          correction <- correction.ym(data, dob, dobm, intw, eventm, dup)
          data <- correction$data
          write.correction.report(error, correction, name)
        } else {
          write.error.report(error, name)
        }
        data <- ym.to.cmc(data)
        data <- recode.mar7(data)

        data <- data %>% select(region, ID, weight, race, ru, sex, mar, edu, tMonth, bMonth,
                                events, m, event, new_index) %>%
          pivot_wider(names_from = c(new_index), values_from = c(m, event), names_sep = "")
        param$CMC <- 1

        #re-arrange column names
        idx_m <- which(names(data)=="m1")
        idx_e <- which(names(data)=="event1")
        max <- max(data$events)
        temp <- 0
        for (i in 1:max) {
          temp = (i-1)*2
          data <- data %>% relocate(names(data)[idx_e+i-1], .after = names(data)[idx_m+temp])
        }

        if (is.data.frame(code)){
          sheets <- list("Parameters" = param, "Person Record" = data, "Name" = code)
        } else {
          sheets <- list("Parameters" = param, "Person Record" = data)
        }
      } else {
        data <- ym.to.cmc(data)
        data <- recode.mar7(data)

        data <- data %>% select(region, ID, weight, race, ru, sex, mar, edu, tMonth, bMonth,
                                events, m, event, new_index) %>%
          pivot_wider(names_from = c(new_index), values_from = c(m, event), names_sep = "")
        param$CMC <- 1

        #re-arrange column names
        idx_m <- which(names(data)=="m1")
        idx_e <- which(names(data)=="event1")
        max <- max(data$events)
        temp <- 0
        for (i in 1:max) {
          temp = (i-1)*2
          data <- data %>% relocate(names(data)[idx_e+i-1], .after = names(data)[idx_m+temp])
        }

        if (is.data.frame(code)){
          sheets <- list("Parameters" = param, "Person Record" = data, "Name" = code)
        } else {
          sheets <- list("Parameters" = param, "Person Record" = data)
        }
        print("There is no error.")
      }
      if (csv==FALSE){
        write_xlsx(sheets, paste(output.path, paste("/", title, " Mar7 Clear.xlsx", sep = ""), sep = ""))
      } else {
        write.csv(data, paste(output.path, paste("/", title, " Mar7 Clear.csv", sep = ""), sep = ""), col.names = T, row.names = F, na = "")
      }
    }
  } else if (CMC==1) {
    #Screen 1: Basic validation
    data <- screen1.cmc(data)
    if (cohabit==0) {
      name <- paste0(title, ", 4 Marital Status", sep = "")
      #Screen 2: Duplicates and logic detectors
      data <- screen2.mar4.cmc(data)
      error_data <- data %>%
        filter(rowSums(!is.na(cbind(err1, err2, err3, err4, err5, err6, err7,
                                    err8, err9, err10, err11, err12)), na.rm = TRUE)>0) %>%
        select(c(ID:event, err1:err12))
      description <- define.descrip(mode = "mar")
      error <- error.summary(error_data, description)

      if (sum(error$err.sum$count)>0){
        if (sum(dob, intw, eventm, dup) > 0) {
          correction <- correction.cmc(data, dob, intw, eventm, dup)
          data <- correction$data
          write.correction.report(error, correction, name)
        } else {
          write.error.report(error, name)
        }
        data <- recode.mar4(data)

        data <- data %>% select(region, ID, weight, race, ru, sex, mar, edu, tMonth, bMonth,
                                events, m, event, new_index) %>%
          pivot_wider(names_from = c(new_index), values_from = c(m, event), names_sep = "")
        param$CMC <- 1

        #re-arrange column names
        idx_m <- which(names(data)=="m1")
        idx_e <- which(names(data)=="event1")
        max <- max(data$events)
        temp <- 0
        for (i in 1:max) {
          temp = (i-1)*2
          data <- data %>% relocate(names(data)[idx_e+i-1], .after = names(data)[idx_m+temp])
        }

        if (is.data.frame(code)){
          sheets <- list("Parameters" = param, "Person Record" = data, "Name" = code)
        } else {
          sheets <- list("Parameters" = param, "Person Record" = data)
        }
      } else {
        data <- recode.mar4(data)

        data <- data %>% select(region, ID, weight, race, ru, sex, mar, edu, tMonth, bMonth,
                                events, m, event, new_index) %>%
          pivot_wider(names_from = c(new_index), values_from = c(m, event), names_sep = "")
        param$CMC <- 1

        #re-arrange column names
        idx_m <- which(names(data)=="m1")
        idx_e <- which(names(data)=="event1")
        max <- max(data$events)
        temp <- 0
        for (i in 1:max) {
          temp = (i-1)*2
          data <- data %>% relocate(names(data)[idx_e+i-1], .after = names(data)[idx_m+temp])
        }

        if (is.data.frame(code)){
          sheets <- list("Parameters" = param, "Person Record" = data, "Name" = code)
        } else {
          sheets <- list("Parameters" = param, "Person Record" = data)
        }
        print("There is no error.")
      }
      if (csv==FALSE){
        write_xlsx(sheets, paste(output.path, paste("/", title, " Mar4 Clear.xlsx", sep = ""), sep = ""))
      } else {
        write.csv(data, paste(output.path, paste("/", title, " Mar4 Clear.csv", sep = ""), sep = ""), col.names = T, row.names = F, na = "")
      }
    } else if (cohabit==1) {
      name <- paste0(title, ", 7 Marital Status", sep = "")
      #Screen 2: Duplicates and logic detectors
      data <- screen2.mar7.cmc(data)
      error_data <- data %>%
        filter(rowSums(!is.na(cbind(err1, err2, err3, err4, err5, err6, err7,
                                    err8, err9, err10, err11, err12)), na.rm = TRUE)>0) %>%
        select(c(ID:event, err1:err12))
      description <- define.descrip(mode = "mar")
      error <- error.summary(error_data, description)

      if (sum(error$err.sum$count)>0){
        if (sum(dob, intw, eventm, dup) > 0) {
          correction <- correction.cmc(data, dob, intw, eventm, dup)
          data <- correction$data
          write.correction.report(error, correction, name)
        } else {
          write.error.report(error, name)
        }
        data <- recode.mar7(data)

        data <- data %>% select(region, ID, weight, race, ru, sex, mar, edu, tMonth, bMonth,
                                events, m, event, new_index) %>%
          pivot_wider(names_from = c(new_index), values_from = c(m, event), names_sep = "")
        param$CMC <- 1

        #re-arrange column names
        idx_m <- which(names(data)=="m1")
        idx_e <- which(names(data)=="event1")
        max <- max(data$events)
        temp <- 0
        for (i in 1:max) {
          temp = (i-1)*2
          data <- data %>% relocate(names(data)[idx_e+i-1], .after = names(data)[idx_m+temp])
        }

        if (is.data.frame(code)){
          sheets <- list("Parameters" = param, "Person Record" = data, "Name" = code)
        } else {
          sheets <- list("Parameters" = param, "Person Record" = data)
        }
      } else {
        data <- recode.mar7(data)

        data <- data %>% select(region, ID, weight, race, ru, sex, mar, edu, tMonth, bMonth,
                                events, m, event, new_index) %>%
          pivot_wider(names_from = c(new_index), values_from = c(m, event), names_sep = "")
        param$CMC <- 1

        #re-arrange column names
        idx_m <- which(names(data)=="m1")
        idx_e <- which(names(data)=="event1")
        max <- max(data$events)
        temp <- 0
        for (i in 1:max) {
          temp = (i-1)*2
          data <- data %>% relocate(names(data)[idx_e+i-1], .after = names(data)[idx_m+temp])
        }

        if (is.data.frame(code)){
          sheets <- list("Parameters" = param, "Person Record" = data, "Name" = code)
        } else {
          sheets <- list("Parameters" = param, "Person Record" = data)
        }
        print("There is no error.")
      }
      if (csv==FALSE){
        write_xlsx(sheets, paste(output.path, paste("/", title, " Mar7 Clear.xlsx", sep = ""), sep = ""))
      } else {
        write.csv(data, paste(output.path, paste("/", title, " Mar7 Clear.csv", sep = ""), sep = ""), col.names = T, row.names = F, na = "")
      }
    }
  }
}

clear.birth <- function(data, param, code, csv, dobm, dob, intw, eventm, dup){
  fertM <- as.numeric(param$fertM)
  cohabit <- as.numeric(param$cohabit)
  nr2 <- as.numeric(param$nr2)
  CMC <- as.numeric(param$CMC)
  output.path <- getwd()
  title <- as.character(param$title)
  param$t1Month <- as.numeric(param$t1Month)
  param$t2Month <- as.numeric(param$t2Month)
  param$t1Year <- as.numeric(param$t1Year)
  param$t2Year <- as.numeric(param$t2Year)

  if (CMC==0) {
    #Screen 1: Basic validation
    data <- screen1.ym(data)
    if (fertM==0) {
      name <- paste0(title, ", non-marital Birth", sep = "")
      #Screen 2: Duplicates and logic detectors
      data <- screen2.birth.ym(data)
      error_data <- data %>%
        filter(rowSums(!is.na(cbind(err1, err2, err3, err4, err5, err6, err7,
                                    err8, err9, err10, err11, err12)), na.rm = TRUE)>0) %>%
        select(c(ID:event, err1:err12))
      description <- define.descrip(mode = "fert")
      error <- error.summary(error_data, description)

      if (sum(error$err.sum$count)>0){
        if (sum(dob, dobm, intw, eventm, dup) > 0) {
          correction <- correction.ym(data, dob, dobm, intw, eventm, dup)
          data <- correction$data
          write.correction.report(error, correction, name)
        } else {
          write.error.report(error, name)
        }
      } else {
        print("There is no error.")
      }

      data <- ym.to.cmc(data)
      data <- recode.birth(data, nr2, fertM, cohabit)
      data <- data %>% select(region, ID, weight, race, ru, sex, mar, parity, edu, tMonth, bMonth,
                              events, m, event, mar.bf, new_index) %>%
        pivot_wider(names_from = c(new_index), values_from = c(m, event, mar.bf), names_sep = "")
      param$CMC <- 1

      #re-arrange column names
      idx_m <- which(names(data)=="m1")
      idx_e <- which(names(data)=="event1")
      idx_mar <- which(names(data)=="mar.bf1")
      max <- max(data$events)
      for (i in 1:max) {
        temp = (i-1)*2
        data <- data %>% relocate(names(data)[idx_e+i-1], .after = names(data)[idx_m+temp])
      }
      for (i in 1:max) {
        temp = (i-1)*3
        data <- data %>% relocate(names(data)[idx_mar+i-1], .after = names(data)[idx_m+1+temp])
      }

      if (is.data.frame(code)){
        sheets <- list("Parameters" = param, "Person Record" = data, "Name" = code)
      } else {
        sheets <- list("Parameters" = param, "Person Record" = data)
      }

      if (csv==FALSE){
        write_xlsx(sheets, paste(output.path, paste("/", title, " non-mar Birth.xlsx", sep = ""), sep = ""))
      } else {
        write.csv(data, paste(output.path, paste("/", title, " non-mar Birth.csv", sep = ""), sep = ""), col.names = T, row.names = F, na = "")
      }
    } else if (fertM==1) {
      name <- paste0(title, ", marital Birth", sep = "")
      #Screen 2: Duplicates and logic detectors
      data <- screen2.birthM.ym(data, cohabit)
      error_data <- data %>%
        filter(rowSums(!is.na(cbind(err1, err2, err3, err4, err5, err6, err7,
                                    err8, err9, err10, err11, err12)), na.rm = TRUE)>0) %>%
        select(c(ID:event, err1:err12))
      description <- define.descrip(mode = "fertM")
      error <- error.summary(error_data, description)

      if (sum(error$err.sum$count)>0){
        if (sum(dob, dobm, intw, eventm, dup) > 0) {
          correction <- correction.ym(data, dob, dobm, intw, eventm, dup)
          data <- correction$data
          write.correction.report(error, correction, name)
        } else {
          write.error.report(error, name)
        }
      } else {
        print("There is no error.")
      }

      data <- ym.to.cmc(data)
      data <- recode.birth(data, nr2, fertM, cohabit)
      data <- data %>% select(region, ID, weight, race, ru, sex, mar, parity, edu, tMonth, bMonth,
                              events, m, event, mar.bf, new_index) %>%
        pivot_wider(names_from = c(new_index), values_from = c(m, event, mar.bf), names_sep = "")
      param$CMC <- 1

      #re-arrange column names
      idx_m <- which(names(data)=="m1")
      idx_e <- which(names(data)=="event1")
      idx_mar <- which(names(data)=="mar.bf1")
      max <- idx_e-idx_m
      for (i in 1:max) {
        temp = (i-1)*2
        data <- data %>% relocate(names(data)[idx_e+i-1], .after = names(data)[idx_m+temp])
      }
      for (i in 1:max) {
        temp = (i-1)*3
        data <- data %>% relocate(names(data)[idx_mar+i-1], .after = names(data)[idx_m+1+temp])
      }

      if (is.data.frame(code)){
        sheets <- list("Parameters" = param, "Person Record" = data, "Name" = code)
      } else {
        sheets <- list("Parameters" = param, "Person Record" = data)
      }

      if (csv==FALSE){
        write_xlsx(sheets, paste(output.path, paste("/", title, " mar Birth.xlsx", sep = ""), sep = ""))
      } else {
        write.csv(data, paste(output.path, paste("/", title, " mar Birth.csv", sep = ""), sep = ""), col.names = T, row.names = F, na = "")
      }
    }
  } else if (CMC==1) {
    #Screen 1: Basic validation
    data <- screen1.cmc(data)
    if (fertM==0) {
      name <- paste0(title, ", non-marital Birth", sep = "")
      #Screen 2: Duplicates and logic detectors
      data <- screen2.birth.cmc(data)
      error_data <- data %>%
        filter(rowSums(!is.na(cbind(err1, err2, err3, err4, err5, err6, err7,
                                    err8, err9, err10, err11, err12)), na.rm = TRUE)>0) %>%
        select(c(ID:event, err1:err12))
      description <- define.descrip(mode = "fert")
      error <- error.summary(error_data, description)

      if (sum(error$err.sum$count)>0){
        if (sum(dob, intw, eventm, dup) > 0) {
          correction <- correction.cmc(data, dob, intw, eventm, dup)
          data <- correction$data
          write.correction.report(error, correction, name)
        } else {
          write.error.report(error, name)
        }
      } else {
        print("There is no error.")
      }
      data <- recode.birth(data, nr2, fertM, cohabit)

      data <- data %>% select(region, ID, weight, race, ru, sex, mar, parity, edu, tMonth, bMonth,
                              events, m, event, mar.bf, new_index) %>%
        pivot_wider(names_from = c(new_index), values_from = c(m, event, mar.bf), names_sep = "")
      param$CMC <- 1

      #re-arrange column names
      idx_m <- which(names(data)=="m1")
      idx_e <- which(names(data)=="event1")
      idx_mar <- which(names(data)=="mar.bf1")
      max <- max(data$events)
      for (i in 1:max) {
        temp = (i-1)*2
        data <- data %>% relocate(names(data)[idx_e+i-1], .after = names(data)[idx_m+temp])
      }
      for (i in 1:max) {
        temp = (i-1)*3
        data <- data %>% relocate(names(data)[idx_mar+i-1], .after = names(data)[idx_m+1+temp])
      }

      if (is.data.frame(code)){
        sheets <- list("Parameters" = param, "Person Record" = data, "Name" = code)
      } else {
        sheets <- list("Parameters" = param, "Person Record" = data)
      }

      if (csv==FALSE){
        write_xlsx(sheets, paste(output.path, paste("/", title, " non-mar Birth.xlsx", sep = ""), sep = ""))
      } else {
        write.csv(data, paste(output.path, paste("/", title, " non-mar Birth.csv", sep = ""), sep = ""), col.names = T, row.names = F, na = "")
      }
    } else if (fertM==1) {
      name <- paste0(title, ", marital Birth", sep = "")
      #Screen 2: Duplicates and logic detectors
      data <- screen2.birthM.cmc(data, cohabit)
      error_data <- data %>%
        filter(rowSums(!is.na(cbind(err1, err2, err3, err4, err5, err6, err7,
                                    err8, err9, err10, err11, err12)), na.rm = TRUE)>0) %>%
        select(c(ID:event, err1:err12))
      description <- define.descrip(mode = "fertM")
      error <- error.summary(error_data, description)

      if (sum(error$err.sum$count)>0){
        if (sum(dob, intw, eventm, dup) > 0) {
          correction <- correction.cmc(data, dob, intw, eventm, dup)
          data <- correction$data
          write.correction.report(error, correction, name)
        } else {
          write.error.report(error, name)
        }
      } else {
        print("There is no error.")
      }

      data <- recode.birth(data, nr2, fertM, cohabit)
      data <- data %>% select(region, ID, weight, race, ru, sex, mar, parity, edu, tMonth, bMonth,
                              events, m, event, mar.bf, new_index) %>%
        pivot_wider(names_from = c(new_index), values_from = c(m, event, mar.bf), names_sep = "")

      #re-arrange column names
      idx_m <- which(names(data)=="m1")
      idx_e <- which(names(data)=="event1")
      idx_mar <- which(names(data)=="mar.bf1")
      max <- idx_e-idx_m
      for (i in 1:max) {
        temp = (i-1)*2
        data <- data %>% relocate(names(data)[idx_e+i-1], .after = names(data)[idx_m+temp])
      }
      for (i in 1:max) {
        temp = (i-1)*3
        data <- data %>% relocate(names(data)[idx_mar+i-1], .after = names(data)[idx_m+1+temp])
      }

      if (is.data.frame(code)){
        sheets <- list("Parameters" = param, "Person Record" = data, "Name" = code)
      } else {
        sheets <- list("Parameters" = param, "Person Record" = data)
      }

      if (csv==FALSE){
        write_xlsx(sheets, paste(output.path, paste("/", title, " mar Birth.xlsx", sep = ""), sep = ""))
      } else {
        write.csv(data, paste(output.path, paste("/", title, " mar Birth.csv", sep = ""), sep = ""), col.names = T, row.names = F, na = "")
      }
    }
  }
}

clear.lh <- function(data, param, code, csv, dobm, dob, intw, eventm, dup){
  CMC <- as.numeric(param$CMC)
  output.path <- getwd()
  title <- as.character(param$title)
  param$t1Month <- as.numeric(param$t1Month)
  param$t2Month <- as.numeric(param$t2Month)
  param$t1Year <- as.numeric(param$t1Year)
  param$t2Year <- as.numeric(param$t2Year)

  if (CMC==0) {
    data <- screen1.lh.ym(data)
    name <- paste0(title, ", Leaving Home", sep = "")
    #Screen 2: Duplicates and logic detectors
    data <- screen2.leave.ym(data)
    error_data <- data %>%
      filter(rowSums(!is.na(cbind(err1, err2, err3, err4, err5, err6, err7,
                                  err8, err9, err10, err11, err12)), na.rm = TRUE)>0) %>%
      select(c(ID:m, err1:err12))
    description <- define.descrip(mode = "lh")
    error <- error.summary(error_data, description)

    if (sum(error$err.sum$count)>0){
      if (sum(dob, dobm, intw, eventm, dup) > 0) {
        correction <- correction.ym(data, dob, dobm, intw, eventm, dup)
        data <- correction$data
        write.correction.report(error, correction, name)
      } else {
        write.error.report(error, name)
      }
    } else {
      print("There is no error.")
    }

    data <- ym.to.cmc(data)
    data <- recode.lh(data)
    data <- data %>% select(region, ID, weight, race, ru, sex, k, edu, tMonth, bMonth,
                            events, m, event, live_ru, new_index) %>%
      pivot_wider(names_from = c(new_index), values_from = c(m, event, live_ru), names_sep = "")
    param$CMC <- 1

    #re-arrange column names
    idx_m <- which(names(data)=="m1")
    idx_e <- which(names(data)=="event1")
    max <- max(data$events)                                #length(names(data)[11:ncol(data)])
    temp <- 0
    for (i in 1:max) {
      temp = (i-1)*2
      data <- data %>% relocate(names(data)[idx_e+i-1], .after = names(data)[idx_m+temp])
    }

    idx_e <- which(names(data)=="event1")
    idx_ru <- which(names(data)=="live_ru1")
    max <- max(data$events)
    temp <- 0
    for (i in 1:max) {
      temp = (i-1)*3
      data <- data %>% relocate(names(data)[idx_ru-1], .after = names(data)[idx_e+temp])
    }

    if (is.data.frame(code)){
      sheets <- list("Parameters" = param, "Person Record" = data, "Name" = code)
    } else {
      sheets <- list("Parameters" = param, "Person Record" = data)
    }

    if (csv==FALSE){
      write_xlsx(sheets, paste(output.path, paste("/", title, " Leaving Home Clear.xlsx", sep = ""), sep = ""))
    } else {
      write.csv(data, paste(output.path, paste("/", title, " Leaving Home Clear.csv", sep = ""), sep = ""), col.names = T, row.names = F, na = "")
    }

  } else if (CMC==1) {
    data <- screen1.lh.cmc(data)
    name <- paste0(title, ", Leaving Home", sep = "")
    #Screen 2: Duplicates and logic detectors
    data <- screen2.leave.cmc(data)
    error_data <- data %>%
      filter(rowSums(!is.na(cbind(err1, err2, err3, err4, err5, err6, err7,
                                  err8, err9, err10, err11, err12)), na.rm = TRUE)>0) %>%
      select(c(ID:m, err1:err12))
    description <- define.descrip(mode = "lh")
    error <- error.summary(error_data, description)

    if (sum(error$err.sum$count)>0){
      if (sum(dob, dobm, intw, eventm, dup) > 0) {
        correction <- correction.cmc(data, dob, intw, eventm, dup)
        data <- correction$data
        write.correction.report(error, correction, name)
      } else {
        write.error.report(error, name)
      }
    } else {
      print("There is no error.")
    }
    data <- recode.lh(data)

    data <- data %>% select(region, ID, weight, race, ru, sex, k, edu, tMonth, bMonth,
                            events, m, event, live_ru, new_index) %>%
      pivot_wider(names_from = c(new_index), values_from = c(m, event, live_ru), names_sep = "")
    param$CMC <- 1

    #re-arrange column names
    idx_m <- which(names(data)=="m1")
    idx_e <- which(names(data)=="event1")
    max <- max(data$events)                                #length(names(data)[11:ncol(data)])
    temp <- 0
    for (i in 1:max) {
      temp = (i-1)*2
      data <- data %>% relocate(names(data)[idx_e+i-1], .after = names(data)[idx_m+temp])
    }

    idx_e <- which(names(data)=="event1")
    idx_ru <- which(names(data)=="live_ru1")
    max <- max(data$events)
    temp <- 0
    for (i in 1:max) {
      temp = (i-1)*3
      data <- data %>% relocate(names(data)[idx_ru-1], .after = names(data)[idx_e+temp])
    }

    if (is.data.frame(code)){
      sheets <- list("Parameters" = param, "Person Record" = data, "Name" = code)
    } else {
      sheets <- list("Parameters" = param, "Person Record" = data)
    }

    if (csv==FALSE){
      write_xlsx(sheets, paste(output.path, paste("/", title, " Leaving Home Clear.xlsx", sep = ""), sep = ""))
    } else {
      write.csv(data, paste(output.path, paste("/", title, " Leaving Home Clear.csv", sep = ""), sep = ""), col.names = T, row.names = F, na = "")
    }

  }

}

clear.mig <- function(data, param, code, csv, dobm, dob, intw, eventm, dup){
  CMC <- as.numeric(param$CMC)
  output.path <- getwd()
  title <- as.character(param$title)
  param$t1Month <- as.numeric(param$t1Month)
  param$t2Month <- as.numeric(param$t2Month)
  param$t1Year <- as.numeric(param$t1Year)
  param$t2Year <- as.numeric(param$t2Year)

  ##
  name <- as.data.frame(code)
  region_code <- na.omit(unique(name$`Region code`))
  country_code <- na.omit(unique(name$`Country code`))
  state_code <- c(region_code, country_code)


  if (CMC==0) {
    data <- screen1.mig.ym(data, code)
    name <- paste0(title, ", Migration", sep = "")
    #Screen 2: Duplicates and logic detectors
    data <- screen2.mig.ym(data, code)
    error_data <- data %>%
      filter(rowSums(!is.na(cbind(err1, err2, err3, err4, err5, err6, err7,
                                  err8, err9, err10, err11, err12)), na.rm = TRUE)>0) %>%
      select(c(ID:m, err1:err12))
    description <- define.descrip(mode = "mig")
    error <- error.summary(error_data, description)

    if (sum(error$err.sum$count)>0){
      if (sum(dob, dobm, intw, eventm, dup) > 0) {
        correction <- correction.ym(data, dob, dobm, intw, eventm, dup)
        data <- correction$data
        write.correction.report(error, correction, name)
      } else {
        write.error.report(error, name)
      }
    } else {
      print("There is no error.")
    }

    data <- ym.to.cmc(data)
    data <- recode.mig(data,code)###重新编码
    data <- data %>% select(region, ID, weight, race, ru, sex, edu, tMonth, bMonth,
                            events, m, event, move.bf, move.af, movebf_ru, moveaf_ru, new_index) %>%
      pivot_wider(names_from = c(new_index), values_from = c(m, event, move.bf, move.af, movebf_ru, moveaf_ru), names_sep = "")
    param$CMC <- 1

    #re-arrange column names
    idx_m <- which(names(data)=="m1")
    idx_e <- which(names(data)=="event1")
    idx_bf <- which(names(data)=="move.bf1")
    idx_af <- which(names(data)=="move.af1")
    idx_bfru <- which(names(data)=="movebf_ru1")
    idx_afru <- which(names(data)=="moveaf_ru1")


    max <- max(data$events)                                #length(names(data)[11:ncol(data)])
    temp <- 0
    ##to do
    for (i in 1:max) {
      temp = (i-1)*2
      data <- data %>% relocate(names(data)[idx_e+i-1], .after = names(data)[idx_m+temp])
    }

    for (i in 1:max) {
      temp = (i-1)*3
      data <- data %>% relocate(names(data)[idx_bf+i-1], .after = names(data)[idx_m+1+temp])
    }

    for (i in 1:max) {
      temp = (i-1)*4
      data <- data %>% relocate(names(data)[idx_af+i-1], .after = names(data)[idx_m+2+temp])
    }

    for (i in 1:max) {
      temp = (i-1)*5
      data <- data %>% relocate(names(data)[idx_bfru+i-1], .after = names(data)[idx_m+3+temp])
    }

    for (i in 1:max) {
      temp = (i-1)*6
      data <- data %>% relocate(names(data)[idx_afru+i-1], .after = names(data)[idx_m+4+temp])
    }


    #####

    if (is.data.frame(code)){
      sheets <- list("Parameters" = param, "Person Record" = data, "Name" = code)
    } else {
      sheets <- list("Parameters" = param, "Person Record" = data)
    }

    if (csv==FALSE){
      write_xlsx(sheets, paste(output.path, paste("/", title, " Migration Clear.xlsx", sep = ""), sep = ""))
    } else {
      write.csv(data, paste(output.path, paste("/", title, " Migration Clear.csv", sep = ""), sep = ""), col.names = T, row.names = F, na = "")
    }


  } else if (CMC==1) {

    #Screen 1: Basic validation
    data <- screen1.mig.cmc(data, code)
    name <- paste0(title, ", Migration", sep = "")

    #Screen 2: Duplicates and logic detectors
    data <- screen2.mig.cmc(data, code)
    error_data <- data %>%
      filter(rowSums(!is.na(cbind(err1, err2, err3, err4, err5, err6, err7,
                                  err8, err9, err10, err11, err12)), na.rm = TRUE)>0) %>%
      select(c(ID:m, err1:err12))
    description <- define.descrip(mode = "mig")
    error <- error.summary(error_data, description)

    if (sum(error$err.sum$count)>0){
      if (sum(dob, intw, eventm, dup) > 0) {
        correction <- correction.cmc(data, dob, intw, eventm, dup)
        data <- correction$data
        write.correction.report(error, correction, name)
      } else {
        write.error.report(error, name)
      }
    } else {
      print("There is no error.")
    }


    data <- recode.mig(data,code)###

    data <- data %>% select(region, ID, weight, race, ru, sex, edu, tMonth, bMonth,
                            events, m, event, move.bf, move.af, movebf_ru, moveaf_ru, new_index) %>%
      pivot_wider(names_from = c(new_index), values_from = c(m, event, move.bf, move.af, movebf_ru, moveaf_ru), names_sep = "")

    param$CMC <- 1

    #re-arrange column names
    idx_m <- which(names(data)=="m1")
    idx_e <- which(names(data)=="event1")
    idx_bf <- which(names(data)=="move.bf1")
    idx_af <- which(names(data)=="move.af1")
    idx_bfru <- which(names(data)=="movebf_ru1")
    idx_afru <- which(names(data)=="moveaf_ru1")


    max <- max(data$events)
    temp <- 0
    ##to do
    for (i in 1:max) {
      temp = (i-1)*2
      data <- data %>% relocate(names(data)[idx_e+i-1], .after = names(data)[idx_m+temp])
    }

    for (i in 1:max) {
      temp = (i-1)*3
      data <- data %>% relocate(names(data)[idx_bf+i-1], .after = names(data)[idx_m+1+temp])
    }

    for (i in 1:max) {
      temp = (i-1)*4
      data <- data %>% relocate(names(data)[idx_af+i-1], .after = names(data)[idx_m+2+temp])
    }

    for (i in 1:max) {
      temp = (i-1)*5
      data <- data %>% relocate(names(data)[idx_bfru+i-1], .after = names(data)[idx_m+3+temp])
    }

    for (i in 1:max) {
      temp = (i-1)*6
      data <- data %>% relocate(names(data)[idx_afru+i-1], .after = names(data)[idx_m+4+temp])
    }



    if (is.data.frame(code)){
      sheets <- list("Parameters" = param, "Person Record" = data, "Name" = code)
    } else {
      sheets <- list("Parameters" = param, "Person Record" = data)
    }

    if (csv==FALSE){
      write_xlsx(sheets, paste(output.path, paste("/", title, " Migration Clear.xlsx", sep = ""), sep = ""))
    } else {
      write.csv(data, paste(output.path, paste("/", title, " Migration Clear.csv", sep = ""), sep = ""), col.names = T, row.names = F, na = "")
    }

  }
}

clear.defined <- function(data, param, code, csv, dobm, dob, intw, eventm, dup){
  CMC <- as.numeric(param$CMC)
  output.path <- getwd()
  title <- as.character(param$title)
  param$t1Month <- as.numeric(param$t1Month)
  param$t2Month <- as.numeric(param$t2Month)
  param$t1Year <- as.numeric(param$t1Year)
  param$t2Year <- as.numeric(param$t2Year)

  ##
  name <- as.data.frame(code)
  event_code <- na.omit(unique(name$`Event code`))
  event_name <- na.omit(unique(name$`Event name`))
  status_code <- na.omit(unique(name$`Status code`))

  if (CMC==0) {
    #Screen 1: Basic validation
    data <- screen1.def.ym(data, code)
    name <- paste0(title, ", User-defined", sep = "")
    #Screen 2: Duplicates and logic detectors
    data <- screen2.def.ym(data, code)
    error_data <- data %>%
      filter(rowSums(!is.na(cbind(err1, err2, err3, err4, err5, err6, err7,
                                  err8, err9, err10, err11, err12)), na.rm = TRUE)>0) %>%
      select(c(ID:m, err1:err12))
    description <- define.descrip(mode = "defined")
    error <- error.summary(error_data, description)

    #这步报错'names' attribute [2] must be the same length as the vector [1]

    if (sum(error$err.sum$count)>0){
      if (sum(dob, dobm, intw, eventm, dup) > 0) {
        correction <- correction.ym(data, dob, dobm, intw, eventm, dup)
        data <- correction$data
        write.correction.report(error, correction, name)
      } else {
        write.error.report(error, name)
      } } else {
        print("There is no error.")
      }

    data <- ym.to.cmc(data)

    #添加events列
    data <- data %>% group_by(ID) %>%
      arrange(ID, m, event) %>%
      mutate(new_index = row_number()) %>%    #这样就计算在同一个ID下，这是第几个
      mutate(events = ifelse(rowSums(is.na(cbind(event, m)))==2&n()==1, 0, n())) %>%
      relocate(events, .after = bMonth)


    data <- data %>% select(region, ID, weight, race, ru, sex, mar, with_p, with_c, k, edu, tMonth, bMonth,
                            events, m, event, status_bf, status_af, new_index) %>%
      pivot_wider(names_from = c(new_index), values_from = c(m, event, status_bf, status_af), names_sep = "")##todo???pivot的作用
    param$CMC <- 1

    #re-arrange column names
    idx_m <- which(names(data)=="m1")
    idx_e <- which(names(data)=="event1")
    idx_bf <- which(names(data)=="status_bf1")
    idx_af <- which(names(data)=="status_af1")


    max <- max(data$events)                                #length(names(data)[11:ncol(data)])
    temp <- 0

    for (i in 1:max) {
      temp = (i-1)*2
      data <- data %>% relocate(names(data)[idx_e+i-1], .after = names(data)[idx_m+temp])
    }

    for (i in 1:max) {
      temp = (i-1)*3
      data <- data %>% relocate(names(data)[idx_bf+i-1], .after = names(data)[idx_m+1+temp])
    }

    for (i in 1:max) {
      temp = (i-1)*4
      data <- data %>% relocate(names(data)[idx_af+i-1], .after = names(data)[idx_m+2+temp])
    }


    if (is.data.frame(code)){
      sheets <- list("Parameters" = param, "Person Record" = data, "Name" = code)
    } else {
      sheets <- list("Parameters" = param, "Person Record" = data)
    }

    if (csv==FALSE){
      write_xlsx(sheets, paste(output.path, paste("/", title, " ", event_name, " Clear.xlsx", sep = ""), sep = ""))
    } else {
      write.csv(data, paste(output.path, paste("/", title, " ", event_name, " Clear.csv", sep = ""), sep = ""), col.names = T, row.names = F, na = "")
    }


  } else if (CMC==1) {
    #Screen 1: Basic validation
    data <- screen1.def.cmc(data, code)
    name <- paste0(title, ", User-defined", sep = "")
    #Screen 2: Duplicates and logic detectors
    data <- screen2.def.cmc(data, code)
    # error_data <- data %>%
    #   filter(rowSums(!is.na(cbind(err1, err2, err3, err4, err5, err6, err7,
    #                               err8, err9, err10, err11, err12)), na.rm = TRUE)>0) %>%
    #   select(c(ID:m, err1:err12))
    #description <- define.descrip(mode = "defined")
    #error <- error.summary(error_data, description)

    correction <- correction.cmc(data, dob, intw, eventm, dup)
    data <- correction$data

    # if (sum(error$err.sum$count)>0){
    #   if (sum(dob, intw, eventm, dup) > 0) {
    #     correction <- correction.cmc(data, dob, intw, eventm, dup)
    #     data <- correction$data
    #     write.correction.report(error, correction, name)
    #   } else {
    #     write.error.report(error, name)
    #   }
    # } else {
    #   print("There is no error.")
    # }

    #添加events列
    data <- data %>% group_by(ID) %>%
      arrange(ID, m, event) %>%
      mutate(new_index = row_number()) %>%    #这样就计算在同一个ID下，这是第几个
      mutate(events = ifelse(rowSums(is.na(cbind(event, m)))==2&n()==1, 0, n())) %>%
      relocate(events, .after = bMonth)


    data <- data %>% select(region, ID, weight, race, ru, sex, mar, with_p, with_c, k, edu, tMonth, bMonth,
                            events, m, event, status_bf, status_af, new_index) %>%
      pivot_wider(names_from = c(new_index), values_from = c(m, event, status_bf, status_af), names_sep = "")##todo???pivot的作用

    #re-arrange column names
    idx_m <- which(names(data)=="m1")
    idx_e <- which(names(data)=="event1")
    idx_bf <- which(names(data)=="status_bf1")
    idx_af <- which(names(data)=="status_af1")


    max <- max(data$events)                                #length(names(data)[11:ncol(data)])
    temp <- 0


    for (i in 1:max) {
      temp = (i-1)*2
      data <- data %>% relocate(names(data)[idx_e+i-1], .after = names(data)[idx_m+temp])
    }

    for (i in 1:max) {
      temp = (i-1)*3
      data <- data %>% relocate(names(data)[idx_bf+i-1], .after = names(data)[idx_m+1+temp])
    }

    for (i in 1:max) {
      temp = (i-1)*4
      data <- data %>% relocate(names(data)[idx_af+i-1], .after = names(data)[idx_m+2+temp])
    }


    if (is.data.frame(code)){
      sheets <- list("Parameters" = param, "Person Record" = data, "Name" = code)
    } else {
      sheets <- list("Parameters" = param, "Person Record" = data)
    }

    if (csv==FALSE){
      write_xlsx(sheets, paste(output.path, paste("/", title, " ", event_name, " Clear.xlsx", sep = ""), sep = ""))
    } else {
      write.csv(data, paste(output.path, paste("/", title, " ", event_name, " Clear.csv", sep = ""), sep = ""), col.names = T, row.names = F, na = "")
    }

  }
}

#' DemoRates clear.data: Prepare valid data for demographic transition rates and frequency estimation
#'
#' The function of \code{clear.data} deals with user-customized Excel files, namely the spreadsheets for parameters, code names and person record. It has the functions of data validation, logic check, data correction, and data conversion. Users could specify the use of function for different types of events by defining the nFunction parameter in the Parameters spreadsheet.
#'
#' @import readxl
#' @import writexl
#' @import openxlsx
#' @import dplyr
#' @importFrom tidyr unite pivot_wider fill
#' @importFrom magrittr %>%
#' @importFrom utils read.csv write.csv

#' @param file the input Excel data file with spreadsheets Parameters, Person Record and Name (optional).
#' @param csv Either \code{TRUE} or \code{FALSE}. Indicate whether the input Person Record is in csv format.
#' @param para If the Person Record data is imported in csv format, a separated Excel file with spreadsheets Parameters and Name (optional) should be provided.
#' @param dobm Either \code{TRUE} or \code{FALSE}. Details see in \code{dob}.
#' @param dob Either \code{TRUE} or \code{FALSE}.
#'     \code{dobm} controls the imputation/correction option for missing or invalid birth month, while \code{dob} controls the deletion option for missing or invalid birth year and month.
#'
#'     If \code{dobm} and \code{dob} are both \code{TRUE}, person records with missing or invalid birth year are deleted. Person records with complete birth year but missing or invalid birth month are imputed with a random month as the birth month.
#'
#'     If \code{dobm = FALSE} and \code{dob = TRUE}, person records with missing or invalid birth year or month are deleted.
#'
#'     If \code{dobm = TRUE} and \code{dob = FALSE}, person records with missing or invalid birth year are not changed, and person records with complete birth year but missing or invalid birth month are imputed with a random month as birth month.
#'
#'     If both \code{dobm} and \code{dob} are \code{FALSE}, no modification is made. By default, \code{dobm} and \code{dob} are \code{FALSE}.
#' @param intw Either \code{TRUE} or \code{FALSE}. If \code{TRUE}, person records with missing or invalid interview time are imputed with the earliest interview time. If \code{FALSE}, no modification is made. By default, \code{intw = FALSE}.
#' @param eventm Either \code{TRUE} or \code{FALSE}. If \code{TRUE}, person records with complete event year but missing or invalid event month are imputed with a random month as event month. If \code{FALSE}, no modification is made. By default, \code{eventm = FALSE}.
#' @param dup Either \code{TRUE} or \code{FALSE}. If \code{TRUE}, duplicated events are deleted. If \code{FALSE}, no modification is made. By default, \code{dup = FALSE}.
#'
#' @export
#'
clear.data <- function(file = NA, csv = FALSE, para = NA, dobm = FALSE,
                       dob = FALSE, intw = FALSE, eventm = FALSE, dup = FALSE){
  if (csv==FALSE){
    data <- read_excel(file, sheet="Person Record", col_types = "numeric")
    data <- as.data.frame(data)
    names(data)[1] <- "region"
    param <- read_excel(file, col_names = TRUE, col_types = "text", sheet="Parameters")
    param <- as.data.frame(param)
    nRace <- as.numeric(param$nRace)
    nRegion <- as.numeric(param$nRegion)
    sheetnames <- excel_sheets(file)
    if ("Name" %in% sheetnames) {
      code <- read_excel(file, col_names = TRUE, sheet="Name")
      code <- as.data.frame(code)
      n.racename <- na.omit(code$`Race Code`)
      n.regname <- na.omit(code$`Region Code`)
      if (nRace>1&length(n.racename)<nRace) {
        print("Error! The number of Race Codes in Name sheet is less than the value of nRace.")
        return()
      } else if (nRegion>1&length(n.regname)<nRegion) {
        print("Error! The number of Region Codes in Name sheet is less than the value of nRegion.")
        return()
      }
    } else {code <- NA}
  } else {
    data <- read.csv(file, header = T, colClasses = "numeric")
    data <- as.data.frame(as.data.frame(lapply(data, as.numeric)))
    names(data)[1] <- "region"
    param <- read_excel(para, col_names = TRUE, col_types = "text", sheet="Parameters")
    param <- as.data.frame(param)
    nRace <- as.numeric(param$nRace)
    nRegion <- as.numeric(param$nRegion)
    sheetnames <- excel_sheets(para)
    if ("Name" %in% sheetnames) {
      code <- read_excel(para, col_names = TRUE, sheet="Name")
      code <- as.data.frame(code)
      n.racename <- na.omit(code$`Race Code`)
      n.regname <- na.omit(code$`Region Code`)
      if (nRace>1&length(n.racename)<nRace) {
        print("Error! The number of Race Codes in Name sheet is less than the value of nRace.")
        return()
      } else if (nRegion>1&length(n.regname)<nRegion) {
        print("Error! The number of Region Codes in Name sheet is less than the value of nRegion.")
        return()
      }
    } else {code <- NA}
  }

  nFunction <- as.numeric(param$nFunction)
  ratetype <- as.numeric(param$ratetype)

  if (nFunction==1){
    if (ratetype == 1) {
      data <- clear.mar(data, param, code, csv, dobm, dob, intw, eventm, dup)
    } else if (ratetype == 2) {
      data <- clear.birth(data, param, code, csv, dobm, dob, intw, eventm, dup)
    }
  } else if (nFunction==2){
    clear.mig(data, param, code, csv, dobm, dob, intw, eventm, dup)
  } else if (nFunction==3){
    clear.lh(data, param, code, csv, dobm, dob, intw, eventm, dup)
  } else if (nFunction==4){
    clear.defined(data, param, code, csv, dobm, dob, intw, eventm, dup)
  }

}

