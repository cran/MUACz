#' Generate both MUAC z-scores and BMI z-scores and their percentiles for age for children/adolescents given their age, sex, MUAC, and BMI values.
#'
#' @description Generates  both MUAC and BMI z-scores for age z-scores and percentiles based on LMS method for children and
#' adolescents aged 3 months to 19 years olds.
#'
#' @param Datafm A DataFrame with variables including, age (in months),
#' sex (1, 2 or "Male", "Female"), muac (numeric: in cm).
#'
#' @param age_range.muac MUAC age range in months: This can be "3-60" or "60-228".
#'
#' @param age_range.bmi BMI age range in months: This can be "0-24", "24-60" or "61-228".
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
#' @return A DataFrame with MUAC and BMI z-scores and their percentiles
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
#' <https://www.who.int/childgrowth/standards/bmi_for_age/en/>
#'
#' <https://www.who.int/growthref/who2007_bmi_for_age/en/>
#'
#' <https://www.who.int/childgrowth/standards/Technical_report.pdf>
#'
#' <https://www.who.int/childgrowth/standards/ac_for_age/en/>
#'
#' @examples
#' ## Example 1
#' ## creating a hypothetical dataset
#' dat1 <- data.frame(age = c(61, 73, 181, 217), sex = 1,
#'                      muac = c(13.0, 15.7, 34.1, 43.9),
#'                       bmi = c(12.1, 14.1, 27.1, 35.4))
#' ans1 <- muacz.bmiz(Datafm = dat1, age_range.muac = "60-228",
#'                  age_range.bmi = "61-228")
#' # ans1
#'
#' ## Example 1
#' ## creating a hypothetical dataset
#' dat2 <- data.frame(age = c(25, 36, 48, 60),
#'                   sex = 2,
#'                   muac = c(15, 17, 21.3, 14),
#'                   bmi = c(15.7, 16.8, 20.6, 12.7))
#' ans2 <- muacz.bmiz(Datafm = dat2, age_range.muac = "3-60",
#'           age_range.bmi = "24-60")
#' # ans2
#'
#' @seealso \code{\link{indivmuaczs}}, \code{\link{muaczs}}, \code{\link{bmizs}}, \code{\link{plotmuac}} and \code{\link{plotbmi}}.
#'
#' @export
#'
muacz.bmiz <- function(Datafm, age_range.muac = "60-228",
                       age_range.bmi = "61-228",
                       digits.zscore = 2,
                       digits.perc = 2,
                       Notes = FALSE){
  datf1 <- muaczs(Datafm, age_range = age_range.muac,
                  digits.zscore = digits.zscore,
                  digits.perc = digits.perc,
                  Notes = Notes)
  datf1$percentile.muac <- datf1$percentile
  datf2 <- bmizs(datf1, age_range = age_range.bmi,
                 digits.zscore = digits.zscore,
                 digits.perc = digits.perc,
                 Notes = Notes)
  datf2$percentile.bmi <- datf2$percentile
  datf2$percentile <- NULL
  datf2[, c(1,2,3,4,5,7,6,8)]
}

