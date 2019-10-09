#' Generate MUAC z scores for age for children/adolescents in a dataset given their age, sex, and muac values.
#'
#' @description Generates mid upper arm circumference (MUAC) for age z scores for children and
#' adolescents aged 5 to 19 years that can be used to assess nutritional and health status,
#'  and  define risk of adverse health events from a given dataset.
#'
#' @param Datafm A DataFrame with variables including pid (unique subject identification),
#' age (in months), sex (1, 2 or "Male", "Female"), muac (numeric: in cm).
#'
#' @param verbose Is FALSE by default. If set to TRUE, 'notes' will be printed on the console about the nature, range of variables allowed and number of records processed.
#'
#' @importFrom epiDisplay tab1
#'
#' @return A DataFrame with MUAC z scores
#'
#' @references
#' Mramba L., Ngari M., Mwangome M., Muchai L., Bauni E., Walker A.S., Gibb D.M., Fegan G. and Berkley J.A. (2017)
#' \emph{A growth reference for mid upper arm circumference for age among school age children and adolescents, and validation for mortality: growth curve construction and longitudinal cohort study}
#' BMJ 2017;358:j3423 <doi:10.1136/bmj.j3423> <https://www.bmj.com/content/358/bmj.j3423>
#'
#' <https://www.who.int/childgrowth/standards/Technical_report.pdf>
#'
#' <https://www.who.int/childgrowth/standards/ac_for_age/en/>
#'
#' @examples
#' ## Example 1: Generate data and calculate muac Z-scores
#'
#' dat1 <- data.frame(age = c(60,65,90,120,250),
#' sex = c("Male","Female","Male","Male","Female"),
#' muac = c(8.7, 13.3, 15.4, 17.8, 25.1))
#' ans1 <- muaczs(Datafm = dat1)
#' # ans1 <- muaczs(Datafm = dat1, verbose=TRUE) # Prints NOTES
#'
#' ## Example 2: Hypothetical longitudinal data with unique patient identification numbers (pid)
#'
#' dat2 <- data.frame(pid = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5),
#' age = c(60, 72, 84, 70, 84, 96, 90, 102, 114, 100, 112, 124, 200, 212, 224),
#' sex = c(1, 1, 1, 2, 2, 2, 1, 1, 1, 2, 2, 2, 2, 2, 2),
#' muac= c(10.1, 13.4, 16.9, 15.3, 16.6, 18.1, 13.2, 15.3, 17.5, 15.7, 17.1, 19.8, 14.1, 16.4, 19.4))
#' ans2 <- muaczs(Datafm = dat2) # saves results in ans2
#' # ans2[order(ans2$pid),] # sorts the data by pid
#'
#' ## Example 3: Data with single observations
#'
#' sex <- c(1)
#' age <- c(190)
#' muac <- c(15)
#' data1 <- data.frame(age = age, sex = sex, muac = muac)
#' ans3 <- muaczs(Datafm = data1)
#' # ans4 <- muaczs(Datafm = data1, verbose=TRUE) # Prints NOTES
#'
#' @seealso \code{\link{indivmuaczs}} and \code{\link{plotref}}.
#'
#' @export
#'
muaczs<-function(Datafm, verbose=FALSE){
  # Datafm is your dataset (a data frame)
  # Important notes.
  if(verbose){
    print(sprintf("Notes:"))
    print(sprintf("Age must be numeric in months: 60 to 300 months"))
    print(sprintf("Sex should be numeric: 1 (male) or 2 (female) or can be character (Male, Female)"))
    print(sprintf("MUAC must be numeric in cm: 5 to 50cm"))
  }
  # stop if Data is not a dataframe
  stopifnot(inherits(Datafm, "data.frame"))
  # Age restriction:
  # set flags for bad data, but continue to process

  # Checks for age
  stopifnot(is.numeric(Datafm$age))
  Datafm$age<-round(Datafm$age) # round to integer
  if(any(Datafm$age <60 | Datafm$age >300)){
    warning("Some Age values are outside the range: 60 to 300 months.")
    # Datafm$age_flag<-0
    Datafm$age_flag <- ifelse(Datafm$age <60 | Datafm$age >300, 1, 0)
    Datafm$age_flag <-factor(Datafm$age_flag, levels=c(0,1),
                             labels=c('Age OK','Out of range'))
    # produce a table of 0/1 for flagged ages
    print(epiDisplay::tab1(Datafm$age_flag, col="grey"))
  }

  # Make sure muac is numeric
  stopifnot(is.numeric(Datafm$muac))
  if(any(Datafm$muac < 5 | Datafm$muac > 50 | is.na(Datafm$muac))) {
    warning("Some MUAC values are outside the range: 5cm to 50 cm.")
    Datafm$muac_flag <- ifelse(Datafm$muac <5 | Datafm$muac >50 | is.na(Datafm$muac), 1, 0)
    Datafm$muac_flag <-factor(Datafm$muac_flag, levels=c(0, 1),
                              labels=c('MUAC OK','Out of range'))
    # produce a table of 0/1 for flagged muac values
    print(epiDisplay::tab1(Datafm$muac_flag, col="grey"))
  }

  # default output to zero
  tempsex<-rep(0,length(Datafm$sex))
  mm <- c("M","MALE","BOY","1")
  ff <- c("F", "FEMALE", "GIRL","2")
  # make all the input upper case characters
  usex <- toupper(as.character(Datafm$sex))
  # look for the text variants
  tempsex[usex %in% mm] <- 1
  tempsex[usex %in% ff] <- 2
  Datafm$sex <- tempsex

  # Second step: Merge with z score parameter lookup table
  # This data has variables sex, age,  l, median, and S.
  # Load the look up data
  # datalms <- MuacZScores:::datalms
  # merge m:m age sex using "FINAL_zscores_36000_BCCG.dta"
  merged1 <- merge(Datafm, datalms, by = c("age","sex"), all.x = TRUE)
  # Third step: generate MUAC Z score from the LMS parameters
  # compute individual muac z scores
  l <- s <- median <- NULL
  merged1$muacz <- (((merged1$muac/merged1$median)^merged1$l)-1)/(merged1$l*merged1$s)
  merged1$muacz[merged1$muac < 5 | merged1$muac > 50] <- NA
  # We need these 4 variables for later
  sd3pos <- sd23pos <- sd3neg <- sd23neg <-NULL
  merged1$sd3pos <- merged1$median*(1+merged1$l*merged1$s*3)^(1/merged1$l)
  merged1$sd23pos <- merged1$median*(1+merged1$l*merged1$s*3)^(1/merged1$l) - merged1$median*(1+merged1$l*merged1$s*2)^(1/merged1$l)
  merged1$sd3neg <- merged1$median*(1+merged1$l*merged1$s*(-3))^(1/merged1$l)
  merged1$sd23neg <- merged1$median*(1+merged1$l*merged1$s*(-2))^(1/merged1$l) - merged1$median*(1+merged1$l*merged1$s*(-3))^(1/merged1$l)
  # Compute final muac z scores
  merged1$muaczfinal <- ifelse(abs(merged1$muacz) <=3, merged1$muacz,
                               ifelse(merged1$muacz >3, 3+((merged1$muac - merged1$sd3pos)/merged1$sd23pos),
                                      ifelse(merged1$muacz < -3, -3+((merged1$muac - merged1$sd3neg)/merged1$sd23neg),NA)))
  ### Set NA to muaczscores and muaczscorefinal
  merged1$muacz <- ifelse(is.na(merged1$l), NA, merged1$muacz)
  merged1$muaczfinal <- ifelse(is.na(merged1$l), NA, merged1$muaczfinal)
  # then drop the parameters
  # drop l s median
  # drop muacz and rename muaczfinal as muacz
  merged1 <- subset(merged1, select = -c(l, s, median, sd3pos, sd23pos, sd3neg, sd23neg, muacz))
  # create muac groups
  muacz <- NULL
  merged1$muacz <-  merged1$muaczfinal
  muaczfinal <- NULL
  merged1 <- subset(merged1, select = -c(muaczfinal)) # drops muaczfinal
  merged1$muacgrp[merged1$muacz >= -2] <- 0
  merged1$muacgrp[merged1$muacz >= -3 & merged1$muacz < -2] <- 1
  merged1$muacgrp[merged1$muacz < -3] <- 2
  merged1$muacgrp[is.na(merged1$muacz) | merged1$muacz==9] <- NA
  # Label the levels of the factor variable (muac group)
  merged1$muacgrp <- factor(merged1$muacgrp,
                            levels = c(0,1,2),
                            labels = c("z>=-2","-3<z<-2","z<-3"))
  if(verbose){
    print(paste0("Total records processed: ", nrow(merged1[!(is.na(merged1$muacz)),])), quote = FALSE)
  }
  merged1
}
#' Generate MUAC z scores for age for single individuals given their age, sex and muac values.
#'
#' @description Generates MUAC z Scores for an individual subject by entering their age (in months), sex and muac (in cm) directly.
#' This is useful for children and adolescents aged 5 to 19 years to assess their nutritional and health status, and  define risk of adverse health events.
#'
#' @param age a numeric value (in months) between 60 and 300.
#'
#' @param sex preferably numeric (1 = Male, 2 = Female). Strings can also be used.
#'
#' @param muac a numeric value in cm between 5 and 50.
#'
#' @param verbose Is FALSE by default. If set to TRUE, 'notes' will be printed on the console about the nature, range of variables allowed and number of records processed.
#'
#' @importFrom epiDisplay tab1
#'
#' @return A DataFrame with MUAC z scores.
#'
#' @references
#' Mramba L., Ngari M., Mwangome M., Muchai L., Bauni E., Walker A.S., Gibb D.M., Fegan G. and Berkley J.A. (2017)
#' \emph{A growth reference for mid upper arm circumference for age among school age children and adolescents, and validation for mortality: growth curve construction and longitudinal cohort study}
#' BMJ 2017;358:j3423 <doi:10.1136/bmj.j3423> <https://www.bmj.com/content/358/bmj.j3423>
#'
#' <https://www.who.int/childgrowth/standards/Technical_report.pdf>
#'
#' <https://www.who.int/childgrowth/standards/ac_for_age/en/>
#'
#' @examples
#' ## Provide values for one individual for age, sex and muac
#' test1 <- indivmuaczs(age=62, sex=1, muac=20)
#' test2 <- indivmuaczs(age=65, sex=2, muac=12.5)
#' test3 <- indivmuaczs(age=70, sex="Female", muac=15.8)
#' test4 <- indivmuaczs(age=92, sex="Male", muac=5.3)
#' # test5 <- indivmuaczs(age=92, sex="Male", muac=5.3, verbose=TRUE) # Prints NOTES
#'
#' @seealso \code{\link{muaczs}}  and \code{\link{plotref}}.
#'
#' @export

indivmuaczs<-function(age=60, sex=1, muac = 10, verbose=FALSE){
  # Datafm is your dataset (a data frame)
  # Important notes.
  Datafm <- data.frame(age=age, sex=sex, muac=muac)
  if(verbose){
  print(sprintf("Notes:"))
  print(sprintf("Age must be numeric in months: 60 to 300 months"))
  print(sprintf("Sex should be numeric: 1 (male) or 2 (female) or can be character (Male, Female)"))
  print(sprintf("MUAC must be numeric in cm: 5 to 50cm"))
  }
  # stop if Data is not a dataframe
  stopifnot(inherits(Datafm, "data.frame"))
  # Age restriction:
  # set flags for bad data, but continue to process
  # Checks for age
  stopifnot(is.numeric(Datafm$age))
  Datafm$age<-round(Datafm$age) # round to integer
  if(any(Datafm$age <60 | Datafm$age >300)){
    warning("Some Age values are outside the range: 60 to 300 months.")
    # Datafm$age_flag<-0
    Datafm$age_flag <- ifelse(Datafm$age <60 | Datafm$age >300, 1, 0)
    Datafm$age_flag <-factor(Datafm$age_flag, levels=c(0,1),
                             labels=c('Age OK','Out of range'))
    # produce a table of 0/1 for flagged ages
    print(epiDisplay::tab1(Datafm$age_flag, col="grey"))
  }
  # Make sure muac is numeric
  stopifnot(is.numeric(Datafm$muac))
  if(any(Datafm$muac < 5 | Datafm$muac > 50 | is.na(Datafm$muac))) {
    warning("Some MUAC values are outside the range: 5cm to 50 cm.")
    Datafm$muac_flag <- ifelse(Datafm$muac <5 | Datafm$muac >50 | is.na(Datafm$muac), 1, 0)
    Datafm$muac_flag <-factor(Datafm$muac_flag, levels=c(0, 1),
                              labels=c('MUAC OK','Out of range'))
    # produce a table of 0/1 for flagged muac values
    print(epiDisplay::tab1(Datafm$muac_flag, col="grey"))
  }
  # default output to zero
  tempsex<-rep(0,length(Datafm$sex))
  mm <- c("M","MALE","BOY","1")
  ff <- c("F", "FEMALE", "GIRL","2")
  # make all the input upper case characters
  usex <- toupper(as.character(Datafm$sex))
  # look for the text variants
  tempsex[usex %in% mm] <- 1
  tempsex[usex %in% ff] <- 2
  Datafm$sex <- tempsex
  # Second step: Merge with z score parameter lookup table
  # This data has variables sex, age,  l, median, and S.
  # Load the look up data
  # merge m:m age sex using "FINAL_zscores_36000_BCCG.dta"
  merged1 <- merge(Datafm, datalms, by = c("age","sex"), all.x = TRUE)
  # Third step: generate MUAC Z score from the LMS parameters
  # compute individual muac z scores
  l <- s <- median <- NULL
  merged1$muacz <- (((merged1$muac/merged1$median)^merged1$l)-1)/(merged1$l*merged1$s)
  merged1$muacz[merged1$muac < 5 | merged1$muac > 50] <- NA
  # We need these 4 variables for later
  sd3pos <- sd23pos <- sd3neg <- sd23neg <-NULL
  merged1$sd3pos <- merged1$median*(1+merged1$l*merged1$s*3)^(1/merged1$l)
  merged1$sd23pos <- merged1$median*(1+merged1$l*merged1$s*3)^(1/merged1$l) - merged1$median*(1+merged1$l*merged1$s*2)^(1/merged1$l)
  merged1$sd3neg <- merged1$median*(1+merged1$l*merged1$s*(-3))^(1/merged1$l)
  merged1$sd23neg <- merged1$median*(1+merged1$l*merged1$s*(-2))^(1/merged1$l) - merged1$median*(1+merged1$l*merged1$s*(-3))^(1/merged1$l)
  # Compute final muac z scores
  merged1$muaczfinal <- ifelse(abs(merged1$muacz) <=3, merged1$muacz,
                               ifelse(merged1$muacz >3, 3+((merged1$muac - merged1$sd3pos)/merged1$sd23pos),
                                      ifelse(merged1$muacz < -3, -3+((merged1$muac - merged1$sd3neg)/merged1$sd23neg),NA)))
  ### Set NA to muaczscores and muaczscorefinal
  merged1$muacz <- ifelse(is.na(merged1$l), NA, merged1$muacz)
  merged1$muaczfinal <- ifelse(is.na(merged1$l), NA, merged1$muaczfinal)
  # then drop the parameters
  # drop l s median muacz
  merged1 <- subset(merged1, select = -c(l, s, median, sd3pos, sd23pos, sd3neg, sd23neg, muacz))
  # create muac groups
  muacz <- NULL
  merged1$muacz <- merged1$muaczfinal
  muaczfinal <- NULL
  merged1 <- subset(merged1, select = -c(muaczfinal)) # drops muaczfinal

  merged1$muacgrp[merged1$muacz >= -2] <- 0
  merged1$muacgrp[merged1$muacz >= -3 & merged1$muacz < -2] <- 1
  merged1$muacgrp[merged1$muacz < -3] <- 2
  merged1$muacgrp[is.na(merged1$muacz) | merged1$muacz==9] <- NA
  # Label the levels of the factor variable (muac group)
  merged1$muacgrp <- factor(merged1$muacgrp,
                            levels = c(0,1,2),
                            labels = c("z>=-2","-3<z<-2","z<-3"))
  if(verbose){
    print(paste0("Total records processed: ", nrow(merged1[!(is.na(merged1$muacz)),])), quote = FALSE)
  }
  merged1
}

