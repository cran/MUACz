#' Generate Body Mass Index z-scores for age  and percentiles for children/adolescents given their age, sex, and bmi values.
#'
#' @description Generates Body Mass Index (BMI) for age z-scores and percentiles based on LMS method for children and
#' adolescents aged 0 to 19 years.
#'
#' @param Datafm A DataFrame with variables age (in months), sex (1, 2 or "Male", "Female"),
#'  bmi (numeric: in kilograms per meter squared (kg/m^2)).
#'
#' @param age_range age range in months. Input has to be characters.
#' It allows "0-24" by default. Other inputs allowed are "24-60" or "61-228".
#'
#' @param Notes Is FALSE by default. If set to TRUE, 'notes' will be printed on the console about the nature,
#' range of variables allowed and number of records processed.
#'
#' @importFrom epiDisplay tab1
#'
#' @importFrom stats pnorm
#'
#' @return A DataFrame with BMI z scores for age and percentiles.
#'
#' @references
#' <https://www.who.int/childgrowth/standards/bmi_for_age/en/>
#'
#' <https://www.who.int/growthref/who2007_bmi_for_age/en/>
#'
#' <https://www.who.int/childgrowth/standards/Technical_report.pdf>
#'
#' @examples
#' ## Example 1: for younger age range = "0-24" months
#' ## No need to specify age_range
#' ## creating a hypothetical dataset
#'
#' dat1 <- data.frame(age = c(5, 6, 12, 12, 18, 18, 23, 23),
#'         sex = c(1, 2, 1, 2, 1, 2, 1, 2),
#'         bmi = c(17.3, 18.6, 18.2, 12.7, 20.8, 20.8, 13.6, 18.4))
#'
#' ans1 <- bmizs(Datafm = dat1)
#' ans1 <- bmizs(Datafm = dat1, Notes = TRUE) # Will also print notes
#' # ans1
#'
#' ## Example 2: specify age range = "24-60" months
#' ## creating a hypothetical dataset
#'
#' dat2 <- data.frame(age = c(25, 36, 48, 60),
#'         sex = 2, bmi = c(15.7, 16.8, 20.6, 12.7))
#'
#' ans2 <- bmizs(Datafm = dat2, age_range = "24-60")
#' # ans2
#'
#' ## Example 3: specify age range = "61-228" months
#' ## creating a hypothetical dataset
#'
#' dat3 <- data.frame(age = c(61, 73, 181, 217),
#'         sex = 1, bmi = c(12.1, 14.1, 27.1, 35.4))
#'
#' ans3 <- bmizs(Datafm = dat3, age_range = "61-228")
#' # ans3
#'
#' @seealso \code{\link{indivmuaczs}}, \code{\link{muaczs}}, \code{\link{muacz.bmiz}}, \code{\link{plotmuac}} and \code{\link{plotbmi}}.
#'
#' @export
#'
bmizs <- function(Datafm, age_range = "0-24", Notes = FALSE){
  # Datafm is your dataset (a data frame)
  # Important notes.
  if(Notes){
    print(sprintf("Notes:"))
    print(sprintf("Age must be numeric in months: 0 to 228"))
    print(sprintf("Sex should be numeric: 1 (male) or 2 (female) or can be character (Male, Female)"))
    print(sprintf("Age_range in months: 0-24; 24-60, 61-228"))
    print(sprintf("BMI must be numeric in Kg/M^2"))
  }
  # stop if Data is not a dataframe
  stopifnot(inherits(Datafm, "data.frame"))
  # Checks for age
  stopifnot(is.numeric(Datafm$age))
  Datafm$age <- round(Datafm$age) # round to integer
  # Make sure bmi is numeric
  stopifnot(is.numeric(Datafm$bmi))
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
  if(any(Datafm$age > 24) & age_range == "0-24"){
    warning("Age range is not correct!")
  }
  if(any(Datafm$age < 24 | Datafm$age > 60) & age_range == "24-60"){
    warning("Age range is not correct!")
  }
  if(any(Datafm$age < 61 | Datafm$age > 228) & age_range == "61-228"){
    warning("Age range is not correct!")
  }
  # age_range must be either "0-24" or "24-60" or "61-228"
  if (age_range =="0-24")
  {
    datalms <- dataBMI_0_2
  }
  if (age_range =="24-60")
  {
    datalms <- dataBMI_2_5
  }
  if (age_range =="61-228")
  {
    datalms <- dataBMI_5_19
  }
  merged1 <- merge(Datafm, datalms, by = c("age","sex"), all.x = TRUE)
  # compute individual bmi z scores
  l <- s <- median <- NULL
  merged1$bmiz <- (((merged1$bmi/merged1$median)^merged1$l)-1)/(merged1$l*merged1$s)
  merged1$bmiz[merged1$bmi < 5 | merged1$bmi > 50] <- NA
  # We need these 4 variables for later
  sd3pos <- sd23pos <- sd3neg <- sd23neg <-NULL
  merged1$sd3pos <- merged1$median*(1+merged1$l*merged1$s*3)^(1/merged1$l)
  merged1$sd23pos <- merged1$median*(1+merged1$l*merged1$s*3)^(1/merged1$l) - merged1$median*(1+merged1$l*merged1$s*2)^(1/merged1$l)
  merged1$sd3neg <- merged1$median*(1+merged1$l*merged1$s*(-3))^(1/merged1$l)
  merged1$sd23neg <- merged1$median*(1+merged1$l*merged1$s*(-2))^(1/merged1$l) - merged1$median*(1+merged1$l*merged1$s*(-3))^(1/merged1$l)
  # Compute final bmi z scores
  merged1$bmizfinal <- ifelse(abs(merged1$bmiz) <=3, merged1$bmiz,
                               ifelse(merged1$bmiz >3, 3+((merged1$bmi - merged1$sd3pos)/merged1$sd23pos),
                                      ifelse(merged1$bmiz < -3, -3+((merged1$bmi - merged1$sd3neg)/merged1$sd23neg),NA)))
  ### Set NA to bmizscores and bmizscorefinal
  merged1$bmiz <- ifelse(is.na(merged1$l), NA, merged1$bmiz)
  merged1$bmizfinal <- ifelse(is.na(merged1$l), NA, merged1$bmizfinal)
  # drop l s median
  # drop bmiz and rename bmizfinal as bmiz
  merged1 <- subset(merged1, select = -c(l, s, median, sd3pos, sd23pos, sd3neg, sd23neg, bmiz))
  bmiz <- NULL
  merged1$bmiz <- merged1$bmizfinal
  bmizfinal <- NULL
  merged1 <- subset(merged1, select = -c(bmizfinal)) # drops bmizfinal
  merged1$percentile <- round(pnorm(merged1$bmiz)*100,1)
  merged1$bmiz <-  round(merged1$bmiz, 0)
  if(Notes){
    print(paste0("Total records processed: ", nrow(merged1[!(is.na(merged1$bmiz)),])), quote = FALSE)
  }
  merged1
}

