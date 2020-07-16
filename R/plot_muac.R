#' Plots MUAC z-scores and percentiles on references curves.
#'
#' @description Plots individual MUAC z-scores and percentiles on standardized growth references
#'  using the LMS method for children and adolescents aged 3 months to 19 years.
#'
#' @param age a numeric value (in months) between 3 and 228 depending on the age_range.
#'
#' @param sex preferably numeric (1 = Male, 2 = Female).
#' Strings (such as "Male", "Female") can also be used.
#'
#' @param muac a numeric mid upper arm circumference value in cm.
#'
#' @param age_range age range in months. Input has to be characters.
#' Options allowed are: "3-60" which is the default, or "60-228".
#'
#' @param graphtype A character input is required: "z-scores" or "percentiles".
#'
#' @param size.label size of the label for calculated z-score or percentile
#'
#' @param size.score refers to the size of the text for the calculated z-scores
#' (-3,-2,-1,0,1,2,3) or the percentiles.
#'
#' @param lwd Line width allows you to specify a numeric value that control the
#' width of the line plots. By dafult, it is set to 1.
#'
#' @param line.color The color of the lines. It is set to "skyblue" by default.
#'
#' @param line.type Type of line such as: "solid", "dotted",
#' "dashed", "blank", "dotdash", "twodash", "longdash"
#'
#' @param shape of the individual point (marker)
#'
#' @param Notes Is FALSE by default. If set to TRUE, 'notes' will be printed on the
#' console about the nature, range of variables allowed and number of records processed.
#'
#' @import ggplot2
#' @import dplyr
#'
#' @return  Plots z-scores or percentiles with a mark indicating where the
#' individual person lies within the standardized reference curves.
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
#' g1 <- plotmuac(age = 48, sex = 2, muac = 16.2, line.color = "orange")
#'
#' g2 <- plotmuac(age = 48, sex = 2, muac = 16.2, graphtype = "percentiles",
#'               line.color = "orange")
#'
#' g3 <- plotmuac(age = c(24, 36, 48, 59), sex = 1, muac = c(13, 14.5, 16.1, 21.7))
#'
#' g4 <- plotmuac(age = c(24, 36, 48, 59), sex = 1, muac = c(13, 14.5, 16.1, 21.7),
#'               graphtype = "percentiles")
#'
#' g5 <- plotmuac(age = c(61, 73, 181, 217), sex = 1, muac = c(13, 15.7, 34.1, 43.9),
#'               age_range = "60-228")
#'
#' g6 <- plotmuac(age = c(61, 73, 181, 217), sex = 1, muac = c(13, 15.7, 34.1, 43.9),
#'               age_range = "60-228", graphtype = "percentiles")
#'
#'
#' @seealso \code{\link{indivmuaczs}}, \code{\link{muaczs}},  \code{\link{bmizs}}, \code{\link{muacz.bmiz}} and \code{\link{plotbmi}}.
#'
#' @export
#'
plotmuac <- function(age, sex, muac, age_range = "3-60",
                    graphtype = "z-scores", lwd = 1,
                    line.color = "skyblue",
                    line.type = "solid",
                    shape = 2,
                    size.label = 2, size.score = 4,
                    Notes = FALSE) {
  if(Notes){
    print(sprintf("Notes:"))
    print(sprintf("Age must be numeric in months"))
    print(sprintf("Sex should be numeric: 1 (male) or 2 (female) or can be character (Male, Female)"))
    print(sprintf("MUAC must be numeric in cm"))
    print(sprintf("Age_range in months: 3-60, 60-228"))
    print(sprintf("Multiple points on the same graph are possible only if data is from same age range and same sex"))
  }
  Datafm <- data.frame(age = age, sex = sex, muac = muac)
  # stop if Data is not a dataframe
  stopifnot(inherits(Datafm, "data.frame"))
  # Age restriction:
  # set flags for bad data, but continue to process
  stopifnot(is.numeric(Datafm$age))
  if(any(Datafm$age < 3 | Datafm$age > 228)){
    stop("Age value is outside the range: 3 months to 19 years.")
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
  Month <- perc <- zscores <- year <- NULL
  if(any(Datafm$age < 3 | Datafm$age > 60) & age_range == "3-60"){
    stop("Age range is not correct!")
  }
  if(any(Datafm$age < 60 | Datafm$age > 228) & age_range == "60-228"){
    stop("Age range is not correct!")
  }
  if(length(unique(Datafm$sex)) != 1){
    stop("Data for plotting reference growth curves cannot have both male and female!")
  }
  if(unique(Datafm$sex) == 1 & graphtype == "z-scores" & age_range == "3-60"){
    BoysZSACFA$zscores <- as.factor(BoysZSACFA$zscores)
    g1 <- ggplot2::ggplot() +
    geom_smooth(aes(x = BoysZSACFA$Month,
                    y = BoysZSACFA$muac,
                    group = BoysZSACFA$zscores),
                method = "gam", lwd = lwd,
                formula = y ~ s(x, bs = "cs"),
                se = FALSE, color = line.color,
                linetype = line.type) +
    geom_text(data = BoysZSACFA %>% filter(Month == last(Month)),
              aes(label = zscores,
                  x = Month + 1.9, color = "red",
                  y = muac), size = size.score) +
    geom_point(aes(x = Datafm$age, y = Datafm$muac),
                size=size.label, color="red",
                shape =  shape) +
    scale_y_continuous(breaks = seq(0, max(BoysZSACFA$muac)+2, 1),
                       "MUAC (cm)") +
    scale_x_continuous(breaks = seq(0, 60, 4),
                       expand = c(0.01, 0.9),
                       "Age (months)") +
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
      text = element_text(face = "bold"),
      plot.title = element_text(face = "bold",
                  hjust = 0.5, vjust = 2),
      axis.text = element_text(face = "bold"))
  }
  if(unique(Datafm$sex) == 2 & graphtype == "z-scores" & age_range == "3-60"){
    GirlsZSACFA$zscores <- as.factor(GirlsZSACFA$zscores)
    g1 <- ggplot2::ggplot() +
      geom_smooth(aes(x = GirlsZSACFA$Month,
                      y = GirlsZSACFA$muac,
                      group = GirlsZSACFA$zscores),
                  method = "gam", lwd = lwd,
                  formula = y ~ s(x, bs = "cs"),
                  se = FALSE, color = line.color,
                  linetype = line.type) +
      geom_text(data = GirlsZSACFA %>% filter(Month == last(Month)),
                aes(label = zscores,
                    x = Month + 1.9, color = "red",
                    y = muac), size = size.score) +
      geom_point( aes(x = Datafm$age, y = Datafm$muac),
                  size=size.label, color="red",
                  shape =  shape) +
      scale_y_continuous(breaks = seq(0, max(GirlsZSACFA$muac)+2, 1),
                         "MUAC (cm)") +
      scale_x_continuous(breaks = seq(0, 60, 4),
                         expand = c(0.01, 0.9),
                         "Age (months)") +
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
        text = element_text(face = "bold"),
        plot.title = element_text(face = "bold",
                    hjust = 0.5, vjust = 2),
        axis.text = element_text(face = "bold"))
  }
  if(unique(Datafm$sex) == 1 & graphtype == "percentiles" & age_range == "3-60"){
    BoysPercACFA$perc <- as.factor(BoysPercACFA$perc)
    g1 <- ggplot2::ggplot() +
      geom_smooth(aes(x = BoysPercACFA$Month,
                      y = BoysPercACFA$muac,
                      group = BoysPercACFA$perc),
                  method = "gam", lwd = lwd,
                  formula = y ~ s(x, bs = "cs"),
                  se = FALSE, color = line.color,
                  linetype = line.type) +
      geom_text(data = BoysPercACFA %>% filter(Month == last(Month)),
                aes(label = perc, x = Month + 2.9, color= "red",
                    y = muac), size = size.score) +
      geom_point( aes(x = Datafm$age, y = Datafm$muac),
                  size=size.label, colour="red",
                  shape =  shape) +
      scale_y_continuous(breaks = seq(0, max(BoysPercACFA$muac)+2, 1),
                         "MUAC (cm)") +
      scale_x_continuous(breaks = seq(0, 60, 4),
                         expand = c(0.02, 0.9),
                         "Age (months)") +
      ggtitle("Boys Percentiles")+
      theme_bw() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.text = element_text(face = "bold"),
        legend.position = "none",
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        text = element_text(face = "bold"),
        plot.title = element_text(face = "bold", hjust = 0.5, vjust = 2),
        axis.text = element_text(face = "bold"))
  }
  if(unique(Datafm$sex) == 2 & graphtype == "percentiles" & age_range == "3-60"){
    GirlsPercACFA$perc <- as.factor(GirlsPercACFA$perc)
    g1 <- ggplot2::ggplot() +
      geom_smooth(aes(x = GirlsPercACFA$Month,
                      y = GirlsPercACFA$muac,
                      group = GirlsPercACFA$perc),
                  method = "gam", lwd = lwd,
                  formula = y ~ s(x, bs = "cs"),
                  se = FALSE, color = line.color,
                  linetype = line.type) +
      geom_text(data = GirlsPercACFA %>% filter(Month == last(Month)),
                aes(label = perc, x = Month + 2.9, color = "red",
                    y = muac), size = size.score) +
      geom_point( aes(x = Datafm$age, y = Datafm$muac),
                  size = size.label, colour = "red",
                  shape = shape) +
      scale_y_continuous(breaks = seq(0, max(GirlsPercACFA$muac)+2, 1),
                         "MUAC (cm)") +
      scale_x_continuous(breaks = seq(0, 60, 4),
                         expand = c(0.02, 0.9),
                         "Age (months)") +
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
        text = element_text(face = "bold"),
        plot.title = element_text(face = "bold",
                    hjust = 0.5, vjust = 2),
        axis.text = element_text(face = "bold"))
  }
  if(unique(Datafm$sex) == 1 & graphtype == "z-scores" & age_range == "60-228"){
    BoysZSNew$zscores <- as.factor(BoysZSNew$zscores)
    g1 <- ggplot2::ggplot() +
      geom_smooth(aes(x = BoysZSNew$year,
                      y = BoysZSNew$muac,
                      group = BoysZSNew$zscores),
                  method = "gam", lwd = lwd,
                  formula = y ~ s(x, bs = "cs"),
                  se = FALSE, color = line.color,
                  linetype = line.type) +
      geom_text(data = BoysZSNew %>% filter(year == last(year)),
                aes(label = zscores,
                    x = year + 0.9, color = "red",
                    y = muac), size = size.score) +
      geom_point( aes(x = (Datafm$age)/12, y = Datafm$muac),
                  size=size.label, color = "red",
                  shape =  shape) +
      scale_y_continuous(breaks = seq(0, max(BoysZSNew$muac)+2, 2),
                         "MUAC (cm)") +
      scale_x_continuous(breaks = seq(5, 19, 1),
                         expand = c(0.001, 0.5),
                         "Age (years)") +
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
        text = element_text(face = "bold"),
        plot.title = element_text(face = "bold",
                                  hjust = 0.5, vjust = 2),
        axis.text = element_text(face = "bold"))
  }
  if(unique(Datafm$sex) == 2 & graphtype == "z-scores" & age_range == "60-228"){
    GirlsZSNew$zscores <- as.factor(GirlsZSNew$zscores)
    g1 <- ggplot2::ggplot() +
      geom_smooth(aes(x = GirlsZSNew$year,
                      y = GirlsZSNew$muac,
                      group = GirlsZSNew$zscores),
                  method = "gam", lwd = lwd,
                  formula = y ~ s(x, bs = "cs"),
                  se = FALSE, color = line.color,
                  linetype = line.type) +
      geom_text(data = GirlsZSNew %>% filter(year == last(year)),
                aes(label = zscores,
                    x = year + 0.9, color = "red",
                    y = muac), size = size.score) +
      geom_point( aes(x = (Datafm$age)/12, y = Datafm$muac),
                  size=size.label, color="red",
                  shape =  shape) +
      scale_y_continuous(breaks = seq(0, max(GirlsZSNew$muac)+2, 2),
                         "MUAC (cm)") +
      scale_x_continuous(breaks = seq(5, 19, 1),
                         expand = c(0.001, 0.5),
                         "Age (years)") +
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
        text = element_text(face = "bold"),
        plot.title = element_text(face = "bold",
                                  hjust = 0.5, vjust = 2),
        axis.text = element_text(face = "bold"))
  }
  if(unique(Datafm$sex) == 1 & graphtype == "percentiles" & age_range == "60-228"){
    BoysPercNew$perc <- as.factor(BoysPercNew$perc)
    g1 <- ggplot2::ggplot() +
      geom_smooth(aes(x = BoysPercNew$year,
                      y = BoysPercNew$muac,
                      group = BoysPercNew$perc),
                  method = "gam", lwd = lwd,
                  formula = y ~ s(x, bs = "cs"),
                  se = FALSE, color = line.color,
                  linetype = line.type) +
      geom_text(data = BoysPercNew %>% filter(year == last(year)),
                aes(label = perc, x = year + 0.9, color = "red",
                    y = muac), size = size.score) +
      geom_point( aes(x = (Datafm$age)/12, y = Datafm$muac),
                  size=size.label, colour = "red",
                  shape =  shape) +
      scale_y_continuous(breaks = seq(0, max(BoysPercNew$muac)+2, 2),
                         "MUAC (cm)") +
      scale_x_continuous(breaks = seq(5, 19, 1),
                         expand = c(0.001, 0.7),
                         "Age (years)") +
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
        text = element_text(face = "bold"),
        plot.title = element_text(face = "bold",hjust = 0.5, vjust = 2),
        axis.text = element_text(face = "bold"))
  }
  if(unique(Datafm$sex) == 2 & graphtype == "percentiles" & age_range == "60-228"){
    GirlsPercNew$perc <- as.factor(GirlsPercNew$perc)
    g1 <- ggplot2::ggplot() +
      geom_smooth(aes(x = GirlsPercNew$year,
                      y = GirlsPercNew$muac,
                      group = GirlsPercNew$perc),
                  method = "gam", lwd = lwd,
                  formula = y ~ s(x, bs = "cs"),
                  se = FALSE, color = line.color,
                  linetype = line.type) +
      geom_text(data = GirlsPercNew %>% filter(year == last(year)),
                aes(label = perc, x = year + 0.9, color = "red",
                    y = muac), size = size.score) +
      geom_point( aes(x = (Datafm$age)/12, y = Datafm$muac),
                  size = size.label, colour = "red",
                  shape =  shape) +
      scale_y_continuous(breaks = seq(0, max(GirlsPercNew$muac)+2, 2),
                         "MUAC (cm)") +
      scale_x_continuous(breaks = seq(5, 19, 1),
                         expand = c(0.001, 0.7),
                         "Age (years)") +
      ggtitle("Girls Percentiles")+
      theme_bw() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.text = element_text(face = "bold"),
        legend.position = "none",
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        text = element_text(face = "bold"),
        plot.title = element_text(face = "bold",
                                  hjust = 0.5, vjust = 2),
        axis.text = element_text(face = "bold"))
  }
  print(g1)
}

