#' Generate MUAC z-scores and percentiles for age for children and adolescents given their age, sex, and MUAC values.
#'
#' @description Generates mid upper arm circumference (MUAC) for age z-scores and percentiles based on LMS-method for children and
#' adolescents aged 3 months to 19 years.
#'
#' @param Datafm A DataFrame with variables including pid (unique subject identification),
#' age (in months), sex (1, 2 or "Male", "Female"), muac (numeric: in cm).
#'
##' @param age_range age range in months. Input has to be characters.
#' Options allowed are: "3-60" which is the default, or "60-228"
#'
#' @param digits.zscore The number of digits for z-score variable
#'
#' @param digits.perc The number of digits for percentile variable
#'
#' @param Notes Is FALSE by default. If set to TRUE, 'notes' will be
#' printed on the console about the nature, range of variables allowed
#' and number of records processed.
#'
#' @importFrom epiDisplay tab1
#'
#' @importFrom stats pnorm
#'
#' @return A DataFrame with MUAC z scores for age and percentiles.
#'
#' @references
#' Mramba L., Ngari M., Mwangome M., Muchai L., Bauni E., Walker A.S., Gibb D.M., Fegan G. and Berkley J.A. (2017)
#' \emph{A growth reference for mid upper arm circumference for age among school age children and adolescents, and validation for mortality:
#' growth curve construction and longitudinal cohort study}
#' BMJ 2017;358:j3423 <doi:10.1136/bmj.j3423>
#'
#' <https://www.bmj.com/content/358/bmj.j3423>
#'
#' <https://www.bmj.com/content/358/bmj.j3423/related#datasupp>
#'
#' <https://www.who.int/childgrowth/standards/Technical_report.pdf>
#'
#' <https://www.who.int/childgrowth/standards/ac_for_age/en/>
#'
#' @examples
#' ## Example 1: younger age range is the default: 3 months - 5 years
#' # Creating a hypothetical dataset
#'
#' dat1 <- data.frame(age = c(3, 5, 12, 18, 23, 24, 36, 48, 60, 24, 36, 48, 60),
#'         sex = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2),
#'         muac = c(15.6, 14.1, 15.8, 18.7, 12.9, 13,
#'         14.5, 16.1, 21.7, 12.7, 14.4, 16.2, 22.5))
#' # Run the function
#' ans1 <- muaczs(Datafm = dat1) # save the output
#' # ans1
#'
#'  ## Example 2: For children 5-19 years old, specify their age range "61-228"
#'  # Creating a hypothetical dataset
#'  dat1 <- data.frame(age = c(60, 65, 90, 120, 220),
#'          sex = c("Male","Female","Male","Male","Female"),
#'          muac = c(20, 14.3, 15.4, 17.8, 25.1))
#'
#' ans1 <- muaczs(Datafm = dat1, age_range = "60-228")
#' # ans1
#'
#' @seealso \code{\link{indivmuaczs}}, \code{\link{bmizs}}, \code{\link{muacz.bmiz}}, \code{\link{plotmuac}} and \code{\link{plotbmi}}.
#'
#' @export
#'
muaczs <- function(Datafm, age_range = "3-60",
                   digits.zscore = 2,
                   digits.perc = 2,
                   Notes = FALSE){
  # Datafm is your dataset (a data frame)
  # Important notes.
  if(Notes){
    print(sprintf("Notes:"))
    print(sprintf("Age must be numeric in months: 3 to 228 months"))
    print(sprintf("Sex should be numeric: 1 (male) or 2 (female) or can be character (Male, Female)"))
    print(sprintf("MUAC must be numeric in cm"))
    print(sprintf("Age_range in months: 3-60, 60-228"))
  }
  # stop if Data is not a dataframe
  stopifnot(inherits(Datafm, "data.frame"))
  # Age restriction:
  # set flags for bad data, but continue to process
  # Checks for age
  stopifnot(is.numeric(Datafm$age))
  Datafm$age <- round(Datafm$age) # round to integer
  if(any(Datafm$age < 3 | Datafm$age > 228)){
    warning("Some Age values are outside the range: 60 to 300 months.")
    # Datafm$age_flag<-0
    Datafm$age_flag <- ifelse(Datafm$age < 3 | Datafm$age > 228, 1, 0)
    Datafm$age_flag <-factor(Datafm$age_flag, levels = c(0,1),
                             labels=c('Age OK','Out of range'))
    # produce a table of 0/1 for flagged ages
    print(epiDisplay::tab1(Datafm$age_flag, col="grey"))
  }
  # Make sure muac is numeric
  stopifnot(is.numeric(Datafm$muac))
  # default output to zero
  tempsex <- rep(0,length(Datafm$sex))
  mm <- c("M","MALE","BOY","1")
  ff <- c("F", "FEMALE", "GIRL","2")
  # make all the input upper case characters
  usex <- toupper(as.character(Datafm$sex))
  # look for the text variants
  tempsex[usex %in% mm] <- 1
  tempsex[usex %in% ff] <- 2
  Datafm$sex <- tempsex
  if(any(Datafm$age < 3 | Datafm$age > 60) & age_range == "3-60"){
    warning("Age range is not correct!")
  }
  if(any(Datafm$age < 60 | Datafm$age > 228) & age_range == "60-228"){
    warning("Age range is not correct!")
  }
  if(age_range == "3-60"){
    datalms <- dataACFA
    }
  if(age_range == "60-228"){
    datalms <- datalms5_19
  }
  merged1 <- merge(Datafm, datalms, by = c("age","sex"), all.x = TRUE)
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
  muacz <- NULL
  merged1$muacz <-  merged1$muaczfinal
  muaczfinal <- NULL
  merged1 <- subset(merged1, select = -c(muaczfinal)) # drops muaczfinal
  merged1$percentile <- round(pnorm(merged1$muacz)*100, digits = digits.perc)
  merged1$muacz <-  round(merged1$muacz, digits = digits.zscore)
  if(Notes){
    print(paste0("Total records processed: ", nrow(merged1[!(is.na(merged1$muacz)),])), quote = FALSE)
  }
  merged1
}
#' Generate MUAC z scores and percentiles for age for single individuals given their age, sex and MUAC values.
#'
#' @description Generates MUAC z-Scores and percentiles for individual subjects by entering their age (in months), sex and muac (in cm) directly.
#' This is useful for children and adolescents aged 3 months to 19 years and to assess their nutritional and health status, and  define risk of adverse health events.
#'
#' @param age a numeric value (in months) between 3 and 228 depending on the age_range.
#'
#' @param sex preferably numeric (1 = Male, 2 = Female). Strings can also be used.
#'
#' @param muac a numeric mid upper arm circumference value in cm.
#'
#' @param age_range age range in months. Input has to be characters.
#' Options allowed are: "3-60" the default, or "60-228".
#'
#' @param digits.zscore The number of digits for z-score variable
#'
#' @param digits.perc The number of digits for percentile variable
#'
#' @param Notes Is FALSE by default. If set to TRUE, 'notes' will be printed on the console about the nature,
#' range of variables allowed and number of records processed.
#'
#' @importFrom epiDisplay tab1
#'
#' @importFrom stats pnorm
#'
#' @return A DataFrame with MUAC z-scores and percentiles.
#'
#' @references
#' Mramba L., Ngari M., Mwangome M., Muchai L., Bauni E., Walker A.S., Gibb D.M., Fegan G. and Berkley J.A. (2017)
#' \emph{A growth reference for mid upper arm circumference for age among school age children and adolescents,
#' and validation for mortality: growth curve construction and longitudinal cohort study}
#' BMJ 2017;358:j3423 <doi:10.1136/bmj.j3423>
#'
#' <https://www.bmj.com/content/358/bmj.j3423>
#'
#' <https://www.bmj.com/content/358/bmj.j3423/related#datasupp>
#'
#' <https://www.who.int/childgrowth/standards/Technical_report.pdf>
#'
#' <https://www.who.int/childgrowth/standards/ac_for_age/en/>
#'
#' @examples
#' test0 <- indivmuaczs(age = 70, sex = "Female", muac = 15.8,
#'           age_range = "60-228")
#' # test0
#'
#' test1 <- indivmuaczs(age = c(4, 40, 60), sex = "Male",
#'           muac = c(17.2, 17.2, 19.8), age_range = "3-60")
#' # test1
#'
#' test2 <- indivmuaczs(age = c(4, 40, 60), sex = 2,
#'          muac = c(14.9, 17.7, 19))
#' # test2
#'
#' res2 <- indivmuaczs(age = 70, sex = c(1, 1, 2, 2, 2, 1),
#'                    muac = c(23.1, 15.2, 18.4, 13.9, 19.5, 14.6),
#'                    age_range = "60-228")
#' # res2
#'
#'
#' @seealso \code{\link{muaczs}}, \code{\link{bmizs}}, \code{\link{muacz.bmiz}}, \code{\link{plotmuac}} and \code{\link{plotbmi}}.
#'
#' @export

indivmuaczs<-function(age=60, sex=1, muac = 10,
                      age_range = "3-60",
                      digits.zscore = 2,
                      digits.perc = 2,
                      Notes = FALSE) {
  # Datafm is your dataset (a data frame)
  # Important notes.
  Datafm <- data.frame(age=age, sex=sex, muac=muac)
  if(Notes){
  print(sprintf("Notes:"))
  print(sprintf("Age must be numeric in months: 3 to 228 months"))
  print(sprintf("Sex should be numeric: 1 (male) or 2 (female) or can be character (Male, Female)"))
  print(sprintf("MUAC must be numeric in cm"))
  print(sprintf("Age_range in months: 3-60, 60-228"))
  }
  # stop if Data is not a dataframe
  stopifnot(inherits(Datafm, "data.frame"))
  # Age restriction:
  # set flags for bad data, but continue to process
  # Checks for age
  stopifnot(is.numeric(Datafm$age))
  Datafm$age<-round(Datafm$age) # round to integer
  if(any(Datafm$age < 3 | Datafm$age > 228)){
    warning("Some Age values are outside the range: 60 to 300 months.")
    # Datafm$age_flag<-0
    Datafm$age_flag <- ifelse(Datafm$age < 3 | Datafm$age > 228, 1, 0)
    Datafm$age_flag <-factor(Datafm$age_flag, levels = c(0,1),
                             labels=c('Age OK','Out of range'))
    # produce a table of 0/1 for flagged ages
    print(epiDisplay::tab1(Datafm$age_flag, col="grey"))
  }
  # Make sure muac is numeric
  stopifnot(is.numeric(Datafm$muac))
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
  if(any(Datafm$age < 3 | Datafm$age > 60) & age_range=="3-60"){
    warning("Age range is not correct!")
  }
  if(any(Datafm$age < 60 | Datafm$age > 228) & age_range=="60-228"){
    warning("Age range is not correct!")
  }
  if(age_range =="3-60"){
    datalms <- dataACFA
  }
  if(age_range =="60-228"){
    datalms <- datalms5_19
  }
  merged1 <- merge(Datafm, datalms, by = c("age","sex"), all.x = TRUE)
  # compute individual muac z scores
  l <- s <- median <- NULL
  merged1$muacz <- (((merged1$muac/merged1$median)^merged1$l)-1)/(merged1$l*merged1$s)
  merged1$muacz[merged1$muac < 5 | merged1$muac > 50] <- NA # extreme outliers
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
  # drop l s median muacz
  merged1 <- subset(merged1, select = -c(l, s, median, sd3pos, sd23pos, sd3neg, sd23neg, muacz))
  # create muac groups
  muacz <- NULL
  merged1$muacz <-  merged1$muaczfinal
  muaczfinal <- NULL
  merged1 <- subset(merged1, select = -c(muaczfinal)) # drops muaczfinal
  merged1$percentile <- round(pnorm(merged1$muacz)*100, digits = digits.perc)
  merged1$muacz <- round(merged1$muacz, digits = digits.zscore)
  if(Notes){
    print(paste0("Total records processed: ", nrow(merged1[!(is.na(merged1$muacz)),])), quote = FALSE)
  }
  merged1
}

