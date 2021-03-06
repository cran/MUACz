% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/muac.R
\name{muaczs}
\alias{muaczs}
\title{Generate MUAC z-scores and percentiles for age for children and adolescents given their age, sex, and MUAC values.}
\usage{
muaczs(
  Datafm,
  age_range = "3-60",
  digits.zscore = 2,
  digits.perc = 2,
  Notes = FALSE
)
}
\arguments{
\item{Datafm}{A DataFrame with variables including pid (unique subject identification),
age (in months), sex (1, 2 or "Male", "Female"), muac (numeric: in cm).}

\item{age_range}{age range in months. Input has to be characters.
Options allowed are: "3-60" which is the default, or "60-228"}

\item{digits.zscore}{The number of digits for z-score variable}

\item{digits.perc}{The number of digits for percentile variable}

\item{Notes}{Is FALSE by default. If set to TRUE, 'notes' will be
printed on the console about the nature, range of variables allowed
and number of records processed.}
}
\value{
A DataFrame with MUAC z scores for age and percentiles.
}
\description{
Generates mid upper arm circumference (MUAC) for age z-scores and percentiles based on LMS-method for children and
adolescents aged 3 months to 19 years.
}
\examples{
## Example 1: younger age range is the default: 3 months - 5 years
# Creating a hypothetical dataset

dat1 <- data.frame(age = c(3, 5, 12, 18, 23, 24, 36, 48, 60, 24, 36, 48, 60),
        sex = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2),
        muac = c(15.6, 14.1, 15.8, 18.7, 12.9, 13,
        14.5, 16.1, 21.7, 12.7, 14.4, 16.2, 22.5))
# Run the function
ans1 <- muaczs(Datafm = dat1) # save the output
# ans1

 ## Example 2: For children 5-19 years old, specify their age range "61-228"
 # Creating a hypothetical dataset
 dat1 <- data.frame(age = c(60, 65, 90, 120, 220),
         sex = c("Male","Female","Male","Male","Female"),
         muac = c(20, 14.3, 15.4, 17.8, 25.1))

ans1 <- muaczs(Datafm = dat1, age_range = "60-228")
# ans1

}
\references{
Mramba L., Ngari M., Mwangome M., Muchai L., Bauni E., Walker A.S., Gibb D.M., Fegan G. and Berkley J.A. (2017)
\emph{A growth reference for mid upper arm circumference for age among school age children and adolescents, and validation for mortality:
growth curve construction and longitudinal cohort study}
BMJ 2017;358:j3423 <doi:10.1136/bmj.j3423>

<https://www.bmj.com/content/358/bmj.j3423>

<https://www.bmj.com/content/358/bmj.j3423/related#datasupp>

<https://www.who.int/childgrowth/standards/Technical_report.pdf>

<https://www.who.int/childgrowth/standards/ac_for_age/en/>
}
\seealso{
\code{\link{indivmuaczs}}, \code{\link{bmizs}}, \code{\link{muacz.bmiz}}, \code{\link{plotmuac}} and \code{\link{plotbmi}}.
}
