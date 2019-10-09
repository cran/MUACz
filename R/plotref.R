#' Plots individual's z-scores and percentiles on references curves.
#'
#' @description Plots individual's z-scores and percentiles on standardized reference curves for children and
#' adolescents aged 5 to 19 years to assess nutritional and health status,
#'  and  define risk of adverse health events for the given individual.
#'
#' @param age a numeric value (in years) between 5 and 19.
#'
#' @param sex preferably numeric (1 = Male, 2 = Female). Strings can also be used.
#'
#' @param muac a numeric value in cm between 5 and 50.
#'
#' @param graphtype requires the user to specify the type of the reference curves to be plotted.
#'   Arguments can be "z-scores" or "percentiles".
#'
#' @import ggplot2
#' @import dplyr
#'
#' @return  Plots z-scores or percentiles with a mark indicating where the individual person lies within the standardized reference curves.
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
#'  ## Plot individual's z-scores and percentiles on standardized reference curves.
#'  p1 <- plotref(age=13, sex=1, muac=25) # Boy's Z-score
#'  p2 <- plotref(age=13, sex=1, muac=25, graphtype="percentiles") # Boy's Percentile
#'  p3 <- plotref(age=12, sex=2, muac=22) # Girl's Z-score
#'  p4 <- plotref(age=12, sex=2, muac=22, graphtype="percentiles") # Girl's Percentile
#'
#' @seealso \code{\link{indivmuaczs}} and \code{\link{muaczs}}.
#'
#' @export
#'
plotref <- function(age=10, sex=1, muac, graphtype="z-scores") {
  Datafm <- data.frame(age = age, sex = sex, muac = muac)
  # stop if Data is not a dataframe
  stopifnot(inherits(Datafm, "data.frame"))
  # Age restriction:
  # set flags for bad data, but continue to process
  # Checks for age
  stopifnot(is.numeric(Datafm$age))
  if(any(Datafm$age < 5 | Datafm$age > 19)){
    stop("Age value is outside the range: 5 to 19 years.")
    }
  # Make sure muac is numeric
  stopifnot(is.numeric(Datafm$muac))
  if(any(Datafm$muac < 5 | Datafm$muac > 50 | is.na(Datafm$muac))) {
    stop("MUAC value is outside the range: 5cm to 50 cm.")
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
  perc <- zscores <- year <- NULL
  if(Datafm$sex == 1 & graphtype == "z-scores"){
    g1 <- ggplot2::ggplot(BoysZSNew, aes(x = year, y = muac,
                        group = zscores,
                        colour=zscores)) +
    geom_smooth(method = "gam",
                formula = y ~ s(x, bs = "cs")) +
    geom_text(data = BoysZSNew %>% filter(year == last(year)),
              aes(label = zscores,
                  x = year + 0.9,
                  y = muac, color = zscores)) +
    geom_point( aes(x = Datafm$age, y = Datafm$muac),
                size=2, colour="black", shape=10) +
    scale_y_continuous(breaks = seq(0, 50, 5),
                       "MUAC (cm)") +
    scale_x_continuous(breaks = seq(5, 19, 1),
                       expand = c(0.001, 0.9),
                       "Age (year)") +
    ggtitle("Boys Z scores")+
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      legend.text = element_text(face="bold"),
      legend.position = "none",
      legend.justification = c("right", "top"),
      legend.box.just = "right",
      text = element_text(face="bold"),
      plot.title = element_text(face="bold",
                  hjust=0.5, vjust = 2),
      axis.text=element_text(face="bold"))
  }
  if(Datafm$sex == 2 & graphtype == "z-scores"){
    g1 <- ggplot2::ggplot(GirlsZSNew,
                          aes(x = year,
                              y = muac,
                            group = zscores,
                            colour=zscores)) +
      geom_smooth(method = "gam",
                  formula = y ~ s(x, bs = "cs")) +
      geom_text(data = GirlsZSNew %>% filter(year == last(year)),
                aes(label = zscores,
                    x = year + 0.9,
                    y = muac, color = zscores)) +
      geom_point( aes(x = Datafm$age, y = Datafm$muac),
                  size=2, colour="black", shape=10) +
      scale_y_continuous(breaks = seq(0, 50, 5),
                         "MUAC (cm)") +
      scale_x_continuous(breaks = seq(5, 19, 1),
                         expand = c(0.001, 0.9),
                         "Age (year)") +
      ggtitle("Girls Z scores")+
      theme_bw() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.text = element_text(face="bold"),
        legend.position = "none",
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        text = element_text(face="bold"),
        plot.title = element_text(face="bold",
                    hjust=0.5, vjust = 2),
        axis.text=element_text(face="bold"))
  }
  if(Datafm$sex == 1 & graphtype == "percentiles"){
    g1 <- ggplot2::ggplot(BoysPercNew,
                          aes(x = year,
                              y = muac,
                      group = perc,
                      colour = perc)) +
      geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
      geom_text(data = BoysPercNew %>% filter(year == last(year)),
                aes(label = perc, x = year + 0.9,
                    y = muac, color = perc)) +
      geom_point( aes(x = Datafm$age, y = Datafm$muac),
                  size=2, colour="black", shape=10) +
      scale_y_continuous(breaks = seq(0, 50, 5),
                         "MUAC (cm)") +
      scale_x_continuous(breaks = seq(5, 19, 1),
                         expand = c(0.001, 0.9),
                         "Age (year)") +
      ggtitle("Boys Percentiles")+
      theme_bw() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.text = element_text(face="bold"),
        legend.position = "none",
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        text = element_text(face="bold"),
        plot.title = element_text(face="bold",hjust=0.5, vjust = 2),
        axis.text=element_text(face="bold"))
  }
  if(Datafm$sex == 2 & graphtype == "percentiles"){
    g1 <- ggplot2::ggplot(GirlsPercNew, aes(x = year, y = muac,
                                           group=perc,
                                           colour=perc)) +
      geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
      geom_text(data = GirlsPercNew %>% filter(year == last(year)),
                aes(label = perc, x = year + 0.9,
                    y = muac, color = perc)) +
      geom_point( aes(x = Datafm$age, y = Datafm$muac),
                  size=2, colour="black", shape=10) +
      scale_y_continuous(breaks = seq(0, 50, 5),
                         "MUAC (cm)") +
      scale_x_continuous(breaks = seq(5, 19, 1),
                         expand = c(0.001, 0.9),
                         "Age (year)") +
      ggtitle("Girls Percentiles")+
      theme_bw() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.text = element_text(face="bold"),
        legend.position = "none",
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        text = element_text(face="bold"),
        plot.title = element_text(face="bold",
                    hjust=0.5, vjust = 2),
        axis.text=element_text(face="bold"))
  }
  print(g1)
}
