#' Plots BMI z-scores and percentiles on references curves.
#'
#' @description Plots BMI z-scores and percentiles on standardized growth references
#'  using the LMS method for children and adolescents aged 0 to 19 years.
#'
#' @param age a numeric value (in months) from 0 to 228 depending on the age_range.
#'
#' @param sex preferably numeric (1 = Male, 2 = Female).
#' Strings (such as "Male", "Female") can also be used.
#'
#' @param age_range age range in months. Input has to be characters.
#' It allows "0-24" by default. Other inputs allowed are "24-60" or "61-228".
#'
#' @param bmi a numeric value in kilograms per meter squared (kg/m^2).
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
#' @param Notes Is FALSE by default. If set to TRUE, 'notes' will be printed on
#' the console about the nature, range of variables allowed and number of records processed.
#'
#' @import ggplot2
#' @import dplyr
#'
#' @return  Plots z-scores or percentiles with a mark indicating where the individual
#' person lies within the standardized reference curves.
#'
#' @references
#' <https://www.who.int/childgrowth/standards/bmi_for_age/en/>
#'
#' <https://www.who.int/growthref/who2007_bmi_for_age/en/>
#'
#' <https://www.who.int/childgrowth/standards/Technical_report.pdf>
#'
#' @examples
#' g1 <- plotbmi(age = 6, sex = 2, bmi = 18.5) # plots Z-scores
#'
#' g2 <- plotbmi(age = 6, sex = 2, bmi = 18.5, graphtype = "percentiles")
#'
#' g3 <- plotbmi(age = c(25, 36, 48, 60), sex = 2, bmi = c(15.7, 16.8, 20.6, 12.7),
#'       age_range = "24-60")
#'
#' g4 <- plotbmi(age = c(61, 73, 181, 217), sex = 1, bmi = c(12.1, 14.1, 27.1, 35.4),
#'       age_range = "61-228", graphtype = "percentiles")
#'
#' @seealso \code{\link{indivmuaczs}}, \code{\link{muaczs}}, \code{\link{bmizs}}, \code{\link{muacz.bmiz}} and \code{\link{plotmuac}}.
#'
#' @export
#'
plotbmi <- function(age, sex, bmi, lwd=1,
                       age_range = "0-24",
                       line.color = "skyblue",
                       graphtype = "z-scores",
                       size.label = 4, size.score = 5,
                       Notes = FALSE) {
  if(Notes){
    print(sprintf("Notes:"))
    print(sprintf("Age must be numeric in months"))
    print(sprintf("Sex should be numeric: 1 (male) or 2 (female) or can be character (Male, Female)"))
    print(sprintf("BMI must be numeric in Kg/M^2"))
    print(sprintf("Age_range in months: 0-24; 24-60, 61-228"))
    print(sprintf("Multiple points on the same graph are possible only if data is from same age range and same sex"))
  }
  Datafm <- data.frame(age = age, sex = sex, bmi = bmi)
  # stop if Data is not a dataframe
  stopifnot(inherits(Datafm, "data.frame"))
  # Age restriction:
  stopifnot(is.numeric(Datafm$age))
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
  Month <- perc <- zscores <- year <- NULL
  if(any(Datafm$age > 24) & age_range == "0-24"){
    stop("Age range is not correct!")
  }
  if(any(Datafm$age < 24 | Datafm$age > 60) & age_range == "24-60"){
    stop("Age range is not correct!")
  }
  if(any(Datafm$age < 61 | Datafm$age > 228) & age_range == "61-228"){
    stop("Age range is not correct!")
  }
   if(length(unique(Datafm$sex)) != 1){
     stop("Data for plotting reference growth curves cannot have both male and female!")
  }
  if(unique(Datafm$sex) == 1 & graphtype == "z-scores" & age_range =="0-24"){
    BMI_ZS_Boys <- BMI_ZS_Boys[BMI_ZS_Boys$agegrp==0,]
    BMI_ZS_Boys$zscores <- as.factor(BMI_ZS_Boys$zscores)
    g1 <- ggplot2::ggplot() +
    geom_smooth(aes(x = BMI_ZS_Boys$Month, y = BMI_ZS_Boys$bmi,
                    group = BMI_ZS_Boys$zscores, linetype = BMI_ZS_Boys$zscores),
                method = "gam", lwd=lwd,
                formula = y ~ s(x, bs = "cs"), se = FALSE, color = line.color) +
    geom_text(data = BMI_ZS_Boys %>% filter(Month == last(Month)),
              aes(label = zscores,
                  x = Month + 1.9, color = "red",
                  y = bmi), size = size.score) +
    geom_point( aes(x = Datafm$age, y = Datafm$bmi),
                size=size.label, color = "red",
                shape =  1:nrow(Datafm)) +
    scale_y_continuous(breaks = seq(0, max(BMI_ZS_Boys$bmi)+2, 1),
                       expression(bold(paste("Body Mass Index ",~ " (", kg/m^{2},")", )))) +
    scale_x_continuous(breaks = seq(0, 24, 3),
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
      text = element_text(face="bold"),
      plot.title = element_text(face="bold",
                  hjust=0.5, vjust = 2),
      axis.text=element_text(face="bold"))
  }
  if(unique(Datafm$sex) == 2 & graphtype == "z-scores" & age_range =="0-24"){
    BMI_ZS_Girls <- BMI_ZS_Girls[BMI_ZS_Girls$agegrp==0,]
    BMI_ZS_Girls$zscores <- as.factor(BMI_ZS_Girls$zscores)
    g1 <- ggplot2::ggplot() +
      geom_smooth(aes(x = BMI_ZS_Girls$Month,
                      y = BMI_ZS_Girls$bmi,
                      group = BMI_ZS_Girls$zscores, linetype = BMI_ZS_Girls$zscores),
                  method = "gam", lwd=lwd,
                  formula = y ~ s(x, bs = "cs"), se = FALSE, color = line.color) +
      geom_text(data = BMI_ZS_Girls %>% filter(Month == last(Month)),
                aes(label = zscores, color = "red",
                    x = Month + 1.9,
                    y = bmi), size = size.score) +
      geom_point( aes(x = Datafm$age, y = Datafm$bmi),
                  size=size.label, color="red",
                  shape =  1:nrow(Datafm)) +
      scale_y_continuous(breaks = seq(0, max(BMI_ZS_Girls$bmi)+2, 1),
                         expression(bold(paste("Body Mass Index ", ~" (", kg/m^{2},")", )))) +
      scale_x_continuous(breaks = seq(0, 24, 3),
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
        text = element_text(face="bold"),
        plot.title = element_text(face="bold",
                    hjust=0.5, vjust = 2),
        axis.text=element_text(face="bold"))
  }
  if(unique(Datafm$sex) == 1 & graphtype == "percentiles" & age_range =="0-24"){
    BMI_Per_Boys <- BMI_Per_Boys[BMI_Per_Boys$agegrp==0,]
    BMI_Per_Boys$perc <- as.factor(BMI_Per_Boys$perc)
    g1 <- ggplot2::ggplot() +
      geom_smooth(aes(x = BMI_Per_Boys$Month,
                      y = BMI_Per_Boys$bmi,
                      group = BMI_Per_Boys$perc,
                      linetype = BMI_Per_Boys$perc),
                  method = "gam", lwd=lwd,
                  formula = y ~ s(x, bs = "cs"),
                  se = FALSE, color = line.color) +
      geom_text(data = BMI_Per_Boys %>% filter(Month == last(Month)),
                aes(label = perc, x = Month + 2.9, color = "red",
                    y = bmi), size = size.score) +
      geom_point( aes(x = Datafm$age, y = Datafm$bmi),
                  size=size.label, colour="red",
                  shape =  1:nrow(Datafm)) +
      scale_y_continuous(breaks = seq(0, max(BMI_Per_Boys$bmi)+2, 1),
                         expression(bold(paste("Body Mass Index ", ~" (", kg/m^{2},")", )))) +
      scale_x_continuous(breaks = seq(0, 24, 3),
                         expand = c(0.01, 0.9),
                         "Age (months)") +
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
  if(unique(Datafm$sex) == 2 & graphtype == "percentiles" & age_range =="0-24"){
    BMI_Per_Girls <- BMI_Per_Girls[BMI_Per_Girls$agegrp==0,]
    BMI_Per_Girls$perc <- as.factor(BMI_Per_Girls$perc)
    g1 <- ggplot2::ggplot() +
      geom_smooth(aes(x = BMI_Per_Girls$Month,
                      y = BMI_Per_Girls$bmi,
                      group = BMI_Per_Girls$perc,
                      linetype = BMI_Per_Girls$perc),
                  method = "gam", lwd=lwd,
                  formula = y ~ s(x, bs = "cs"),
                  se = FALSE, color = line.color) +
      geom_text(data = BMI_Per_Girls %>% filter(Month == last(Month)),
                aes(label = perc, x = Month + 2.9, color = "red",
                    y = bmi), size = size.score) +
      geom_point(aes(x = Datafm$age, y = Datafm$bmi),
                  size=size.label, colour="red",
                  shape =  1:nrow(Datafm)) +
      scale_y_continuous(breaks = seq(0, max(BMI_Per_Girls$bmi)+2, 1),
                         expression(bold(paste("Body Mass Index ", ~" (", kg/m^{2},")", )))) +
      scale_x_continuous(breaks = seq(0, 24, 3),
                         expand = c(0.01, 0.9),
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
        text = element_text(face="bold"),
        plot.title = element_text(face="bold",
                    hjust=0.5, vjust = 2),
        axis.text=element_text(face="bold"))
  }

  if(unique(Datafm$sex) == 1 & graphtype == "z-scores" & age_range =="24-60"){
    BMI_ZS_Boys <- BMI_ZS_Boys[BMI_ZS_Boys$agegrp==1,]
    BMI_ZS_Boys$zscores <- as.factor(BMI_ZS_Boys$zscores)
    g1 <- ggplot2::ggplot() +
      geom_smooth(aes(x = BMI_ZS_Boys$Month,
                      y = BMI_ZS_Boys$bmi,
                      group = BMI_ZS_Boys$zscores,
                      linetype = BMI_ZS_Boys$zscores),
                  method = "gam", lwd=lwd,
                  formula = y ~ s(x, bs = "cs"),
                  se = FALSE, color = line.color) +
      geom_text(data = BMI_ZS_Boys %>% filter(Month == last(Month)),
                aes(label = zscores,
                    x = Month + 1.9, color = "red",
                    y = bmi), size = size.score) +
      geom_point( aes(x = Datafm$age, y = Datafm$bmi),
                  size=size.label, color="red",
                  shape =  1:nrow(Datafm)) +
      scale_y_continuous(breaks = seq(0, max(BMI_ZS_Boys$bmi)+2, 1),
                         expression(bold(paste("Body Mass Index ", ~" (", kg/m^{2},")", )))) +
      scale_x_continuous(breaks = seq(24, 60, 3),
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
        text = element_text(face="bold"),
        plot.title = element_text(face="bold",
                                  hjust=0.5, vjust = 2),
        axis.text=element_text(face="bold"))
  }
  if(unique(Datafm$sex) == 2 & graphtype == "z-scores" & age_range =="24-60"){
    BMI_ZS_Girls <- BMI_ZS_Girls[BMI_ZS_Girls$agegrp==1,]
    BMI_ZS_Girls$zscores <- as.factor(BMI_ZS_Girls$zscores)
    g1 <- ggplot2::ggplot() +
      geom_smooth(aes(x = BMI_ZS_Girls$Month,
                      y = BMI_ZS_Girls$bmi,
                      group = BMI_ZS_Girls$zscores,
                      linetype = BMI_ZS_Girls$zscores),
                  method = "gam", lwd=lwd,
                  formula = y ~ s(x, bs = "cs"),
                  se = FALSE, color = line.color) +
      geom_text(data = BMI_ZS_Girls %>% filter(Month == last(Month)),
                aes(label = zscores,
                    x = Month + 1.9, color = "red",
                    y = bmi), size = size.score) +
      geom_point( aes(x = Datafm$age, y = Datafm$bmi),
                  size=size.label, color="red",
                  shape =  1:nrow(Datafm)) +
      scale_y_continuous(breaks = seq(0, max(BMI_ZS_Girls$bmi)+2, 1),
                         expression(bold(paste("Body Mass Index ", ~" (", kg/m^{2},")", )))) +
      scale_x_continuous(breaks = seq(24, 60, 3),
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
        text = element_text(face="bold"),
        plot.title = element_text(face="bold",
                                  hjust=0.5, vjust = 2),
        axis.text=element_text(face="bold"))
  }
  if(unique(Datafm$sex) == 1 & graphtype == "percentiles" & age_range =="24-60"){
    BMI_Per_Boys <- BMI_Per_Boys[BMI_Per_Boys$agegrp==1,]
    BMI_Per_Boys$perc <- as.factor(BMI_Per_Boys$perc)
    g1 <- ggplot2::ggplot() +
      geom_smooth(aes(x = BMI_Per_Boys$Month,
                      y = BMI_Per_Boys$bmi,
                      group = BMI_Per_Boys$perc,
                      linetype = BMI_Per_Boys$perc),
                  method = "gam", lwd=lwd,
                  formula = y ~ s(x, bs = "cs"),
                  se = FALSE, color = line.color) +
      geom_text(data = BMI_Per_Boys %>% filter(Month == last(Month)),
                aes(label = perc, x = Month + 2.9, color = "red",
                    y = bmi), size = size.score) +
      geom_point( aes(x = Datafm$age, y = Datafm$bmi),
                  size=size.label, colour="red",
                  shape =  1:nrow(Datafm)) +
      scale_y_continuous(breaks = seq(0, max(BMI_Per_Boys$bmi)+2, 1),
                         expression(bold(paste("Body Mass Index ", ~" (", kg/m^{2},")", )))) +
      scale_x_continuous(breaks = seq(24, 60, 3),
                         expand = c(0.01, 0.9),
                         "Age (months)") +
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
  if(unique(Datafm$sex) == 2 & graphtype == "percentiles" & age_range =="24-60"){
    BMI_Per_Girls <- BMI_Per_Girls[BMI_Per_Girls$agegrp==1,]
    BMI_Per_Girls$perc <- as.factor(BMI_Per_Girls$perc)
    g1 <- ggplot() +
      geom_smooth(aes(x = BMI_Per_Girls$Month, y = BMI_Per_Girls$bmi,
                      group = BMI_Per_Girls$perc,
                      linetype = BMI_Per_Girls$perc),
                  method = "gam", lwd=lwd,
                  formula = y ~ s(x, bs = "cs"),
                  se = FALSE, color = line.color) +
      geom_text(data = BMI_Per_Girls %>% filter(Month == last(Month)),
                aes(label = perc, x = Month + 2.9, color = "red",
                    y = bmi), size = size.score) +
      geom_point( aes(x = Datafm$age, y = Datafm$bmi),
                  size=size.label, color = "red",
                  shape =  1:nrow(Datafm)) +
      scale_y_continuous(breaks = seq(0, max(BMI_Per_Girls$bmi)+2, 1),
                         expression(bold(paste("Body Mass Index ", ~" (", kg/m^{2},")", )))) +
      scale_x_continuous(breaks = seq(24, 60, 3),
                         expand = c(0.01, 0.9),
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
        text = element_text(face="bold"),
        plot.title = element_text(face="bold",
                                  hjust=0.5, vjust = 2),
        axis.text=element_text(face="bold"))
  }
if(unique(Datafm$sex) == 1 & graphtype == "z-scores" & age_range =="61-228"){
    BMI_ZS_Boys <- BMI_ZS_Boys[BMI_ZS_Boys$agegrp==2,]
    BMI_ZS_Boys$zscores <- as.factor(BMI_ZS_Boys$zscores)
  g1 <- ggplot2::ggplot() +
      geom_smooth(aes(x = BMI_ZS_Boys$year,
                      y = BMI_ZS_Boys$bmi,
                      group = BMI_ZS_Boys$zscores,
                      linetype = BMI_ZS_Boys$zscores),
                  method = "gam", lwd=lwd,
                  formula = y ~ s(x, bs = "cs"),
                  se = FALSE,
                  colour = line.color) +
      geom_text(data = BMI_ZS_Boys %>% filter(year == last(year)),
                aes(label = zscores, x = year + 0.9,
                    y = bmi, color = "red"), size = size.score) +
      geom_point(aes(x = (Datafm$age)/12, y = Datafm$bmi),
                  size = size.label, color = "red",
                  shape =  1:nrow(Datafm)) +
      scale_y_continuous(breaks = seq(0, max(BMI_ZS_Boys$bmi)+2, 2),
                         expression(bold(paste("Body Mass Index ", ~" (", kg/m^{2},")", )))) +
      scale_x_continuous(breaks = seq(5, 19, 1),
                         expand = c(0.001, 0.3), "Age (years)") +
      ggtitle("Boys Z scores")+
      theme_bw() +
      theme(
        legend.text = element_text(face="bold"),
        legend.position = "none",
        plot.title = element_text(face = "bold", hjust=0.5),
        axis.text = element_text(face="bold"),
        axis.title = element_text(face="bold"),
        legend.title = element_text(face="bold"),
        legend.background = element_rect(colour = "black"),
        panel.border = element_rect(colour="black", fill="NA",  linetype = "solid"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line("black",   linetype = "solid"),
        text = element_text(face="bold")
      )
  }
  if(unique(Datafm$sex) == 2 & graphtype == "z-scores" & age_range =="61-228"){
    BMI_ZS_Girls <- BMI_ZS_Girls[BMI_ZS_Girls$agegrp==2,]
    BMI_ZS_Girls$zscores <- as.factor(BMI_ZS_Girls$zscores)
    g1 <- ggplot2::ggplot() +
      geom_smooth(aes(x = BMI_ZS_Girls$year,
                      y = BMI_ZS_Girls$bmi,
                      group = BMI_ZS_Girls$zscores,
                      linetype = BMI_ZS_Girls$zscores),
                  method = "gam", lwd=lwd,
                  formula = y ~ s(x, bs = "cs"), se = FALSE,
                  colour = line.color) +
      geom_text(data = BMI_ZS_Girls %>% filter(year == last(year)),
                aes(label = zscores, x = year + 0.9,
                    y = bmi, color = "red"), size = size.score) +
      geom_point(aes(x = (Datafm$age)/12, y = Datafm$bmi),
                 size = size.label, color = "red",
                 shape =  1:nrow(Datafm)) +
      scale_y_continuous(breaks = seq(0, max(BMI_ZS_Girls$bmi)+2, 2),
                         expression(bold(paste("Body Mass Index ", ~" (", kg/m^{2},")", )))) +
      scale_x_continuous(breaks = seq(5, 19, 1),
                         expand = c(0.001, 0.3), "Age (years)") +
      ggtitle("Girls Z scores")+
      theme_bw() +
      theme(
        legend.text = element_text(face="bold"),
        legend.position = "none",
        plot.title = element_text(face = "bold", hjust=0.5),
        axis.text = element_text(face="bold"),
        axis.title = element_text(face="bold"),
        legend.title = element_text(face="bold"),
        legend.background = element_rect(colour = "black"),
        panel.border = element_rect(colour="black", fill="NA",  linetype = "solid"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line("black",   linetype = "solid"),
        text = element_text(face="bold")
      )
  }
  if(unique(Datafm$sex) == 1 & graphtype == "percentiles" & age_range =="61-228"){
    BMI_Per_Boys <- BMI_Per_Boys[BMI_Per_Boys$agegrp==2,]
    BMI_Per_Boys$perc <- as.factor(BMI_Per_Boys$perc)
    g1 <- ggplot2::ggplot() +
      geom_smooth(aes(x = BMI_Per_Boys$year,
                      y = BMI_Per_Boys$bmi,
                      group = BMI_Per_Boys$perc,
                      linetype = BMI_Per_Boys$perc),
                  method = "gam", lwd=lwd,
                  formula = y ~ s(x, bs = "cs"), se = FALSE,
                  colour = line.color) +
      geom_text(data = BMI_Per_Boys %>% filter(year == last(year)),
                aes(label = perc, x = year + 0.9,
                    y = bmi, color = "red"), size = size.score) +
      geom_point(aes(x = (Datafm$age)/12, y = Datafm$bmi),
                 size = size.label, color = "red",
                 shape =  1:nrow(Datafm)) +
      scale_y_continuous(breaks = seq(0, max(BMI_Per_Boys$bmi)+2, 2),
                         expression(bold(paste("Body Mass Index ", ~" (", kg/m^{2},")", )))) +
      scale_x_continuous(breaks = seq(5, 19, 1),
                         expand = c(0.001, 0.5), "Age (years)") +
      ggtitle("Boys Percentiles")+
      theme_bw() +
      theme(
        legend.text = element_text(face="bold"),
        legend.position = "none",
        plot.title = element_text(face = "bold", hjust=0.5),
        axis.text = element_text(face="bold"),
        axis.title = element_text(face="bold"),
        legend.title = element_text(face="bold"),
        legend.background = element_rect(colour = "black"),
        panel.border = element_rect(colour="black", fill="NA",  linetype = "solid"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line("black",   linetype = "solid"),
        text = element_text(face="bold")
      )
  }
  if(unique(Datafm$sex) == 2 & graphtype == "percentiles" & age_range =="61-228"){
    BMI_Per_Girls <- BMI_Per_Girls[BMI_Per_Girls$agegrp==2,]
    BMI_Per_Girls$perc <- as.factor(BMI_Per_Girls$perc)
    g1 <- ggplot2::ggplot() +
      geom_smooth(aes(x = BMI_Per_Girls$year,
                      y = BMI_Per_Girls$bmi,
                      group = BMI_Per_Girls$perc,
                      linetype = BMI_Per_Girls$perc),
                  method = "gam", lwd=lwd,
                  formula = y ~ s(x, bs = "cs"), se = FALSE,
                  colour = line.color) +
      geom_text(data = BMI_Per_Girls %>% filter(year == last(year)),
                aes(label = perc, x = year + 0.9,
                    y = bmi, color = "red"), size = size.score) +
      geom_point(aes(x = (Datafm$age)/12, y = Datafm$bmi),
                 size = size.label, color = "red",
                 shape =  1:nrow(Datafm)) +
      scale_y_continuous(breaks = seq(0, max(BMI_Per_Girls$bmi)+2, 2),
                         expression(bold(paste("Body Mass Index ", ~" (", kg/m^{2},")", )))) +
      scale_x_continuous(breaks = seq(5, 19, 1),
                         expand = c(0.001, 0.5), "Age (years)") +
      ggtitle("Girls Percentiles")+
      theme_bw() +
      theme(
        legend.text = element_text(face="bold"),
        legend.position = "none",
        plot.title = element_text(face = "bold", hjust=0.5),
        axis.text = element_text(face="bold"),
        axis.title = element_text(face="bold"),
        legend.title = element_text(face="bold"),
        legend.background = element_rect(colour = "black"),
        panel.border = element_rect(colour="black", fill="NA",  linetype = "solid"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line("black",   linetype = "solid"),
        text = element_text(face="bold")
      )
  }
  print(g1)
}

