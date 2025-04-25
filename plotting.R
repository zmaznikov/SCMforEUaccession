#' Plot both the actual values and the synthetic estimate for a single country
#' 
#' @param dataprepout A list, as output by the dataprep function in the "Synth"
#'   package, including the outcome variable for the entire period for the 
#'   donor sample units as a T x N matrix "Y0plot".
#' @param synthout A list, as output by the synth function in the "Synth" 
#'   package, including unit weights as an N x 1 vector "solution.w".
#' @param countryCode A unique regionno number, under which the country 
#'   is listed in the "world" data.frame.
#' @param useRgdpch A boolean value. If TRUE, GDP per capita is plotted, 
#'   if FALSE, GDP per worker is plotted.
#' @param ylim Numeric, used to set the limits of the y axis.
#' 
#' @return An instance of ggplot with the actual values 
#'   and the synthetic estimate.
plotPaths <- function(dataprepout, synthout, countryCode, useRgdpch = T, ylim = c(5000, 30000)) {
  if(useRgdpch) {
    bcomp <- data.frame(rownames_to_column(data.frame(dataprepout$Y0plot%*%synthout$solution.w), var = 'date'), world[which(world$regionno == countryCode & world$year >= startYear & world$year <= endYear),]$rgdpch)
    breaksY <- 10000
  } else {
    bcomp <- data.frame(rownames_to_column(data.frame(dataprepout$Y0plot%*%synthout$solution.w), var = 'date'), world[which(world$regionno == countryCode & world$year >= startYear & world$year <= endYear),]$rgdpwok)
    breaksY <- 20000
  }
  bcomp$date <- as.numeric(bcomp$date)
  colnames(bcomp) <- c("date", "synthetic", "true")
  
  bcompm <- melt(bcomp,  id = 'date')
  
  comp <- ggplot() + 
    geom_line(data = bcompm, aes(x=date, y = value, linetype = variable)) +
    scale_linetype_manual(values = c("f4", "solid"),
                          labels=c("Synthetic", "Actual")) +
    geom_vline(aes(xintercept = eventYear), linetype = 2)+ 
    coord_cartesian(xlim = c(1990, 2019), ylim = ylim) +
    scale_y_continuous(breaks=seq(ylim[1],ylim[2],breaksY))+
    theme(text = element_text(size=19),
          panel.border = element_rect(linetype = 1, fill = NA),
          panel.background = element_rect(fill = NA, colour = NA),
          axis.text = element_text(size=19),
          axis.text.y = element_text(angle = 90, hjust = 0.5),
          axis.title.x = element_blank()) +
    theme(legend.title=element_blank(),
          legend.position = c(0.95, 0.05),
          legend.justification = c(1, 0),
          legend.text=element_text(size=19)) +
    theme(plot.margin = unit(c(0.7,0.7,0.2,0.2), "cm"))
  
  return(comp)
}

#' Plot the gaps between the actual values and the synthetic estimate 
#' for a single country, including two confidence sets at different
#' significance levels
#' 
#' @param dataprepout A list, as output by the dataprep function in the "Synth"
#'   package, including the outcome variable for the entire period for the 
#'   donor sample units as a T x N matrix "Y0plot".
#' @param synthout A list, as output by the synth function in the "Synth" 
#'   package, including unit weights as an N x 1 vector "solution.w".
#' @param countryCode A unique regionno number, under which the country 
#'   is listed in the "world" data.frame.
#' @param vsignificance The significance level in decimal form
#' @param boundsdf A T x 3 data.frame with columns named "l" with lower bound,
#'   "u" for upper bound and "ci" - a column of ones. Contains the confidence
#'   set at the vsignificance significance level
#' @param bounds2df A T x 3 data.frame with columns named "l" with lower bound,
#'   "u" for upper bound and "ci" - a column of ones. Contains the confidence
#'   set at the 2*vsignificance significance level
#' @param useRgdpch A boolean value. If TRUE, GDP per capita is plotted, 
#'   if FALSE, GDP per worker is plotted.
#' @param ylab A String to be displayed as the y axis label.
#' 
#' @return An instance of ggplot with the gap between actual values 
#'   and the synthetic estimate, including two confidence sets.
plotGaps <- function(dataprepout, synthout, countryCode, vsignificance, boundsdf, bounds2df, useRgdpch = T, ylab = "") {
  if(useRgdpch) {
    bcomp <- data.frame(rownames_to_column(data.frame(dataprepout$Y0plot%*%synthout$solution.w), var = 'date'))
    bcomp$date <- as.numeric(bcomp$date)
    colnames(bcomp) <- c("date", "gap")
    bcomp$gap <- world[which(world$regionno == countryCode & world$year >= startYear & world$year <= endYear),]$rgdpch - bcomp$gap
    ylim <- c(-15000, 25000)
    breaksY <- 10000
  } else {
    bcomp <- data.frame(rownames_to_column(data.frame(dataprepout$Y0plot%*%synthout$solution.w), var = 'date'))
    bcomp$date <- as.numeric(bcomp$date)
    colnames(bcomp) <- c("date", "gap")
    bcomp$gap <- world[which(world$regionno == countryCode & world$year >= startYear & world$year <= endYear),]$rgdpwok - bcomp$gap
    ylim <- c(-30000, 50000)
    breaksY <- 20000
  }
  
  bcompm <- melt(bcomp,  id = 'date')
  
  comp <- ggplot() + 
    geom_line(data = bcompm, aes(x=date, y = value), linetype = "solid")+
    geom_vline(aes(xintercept = eventYear), linetype = 2)+ 
    coord_cartesian(xlim = c(1990, 2019), ylim = ylim) +
    scale_y_continuous(breaks=seq(ylim[1],ylim[2],breaksY))+
    labs(y = ylab) +
    theme(text = element_text(size=19),
          panel.border = element_rect(linetype = 1, fill = NA),
          panel.background = element_rect(fill = NA, colour = NA),
          axis.text = element_text(size=19),
          axis.text.y = element_text(angle = 90, hjust = 0.5),
          axis.title.x = element_blank()) +
    theme(legend.title=element_blank(),
          legend.position = c(0.05, 0.05),
          legend.justification = c(0, 0),
          legend.text=element_text(size=19)) +
    theme(plot.margin = unit(c(0.7,0.7,0.2,0.2), "cm"))
  
  comp$layers <- c(geom_hline(yintercept = 0, linetype = "f4"),
                   geom_ribbon(data = boundsdf, aes(x=seq(startYear, endYear, 1), ymin=l,ymax=u, fill = as.factor(ci)), alpha = 0.6, show.legend = T),
                   geom_ribbon(data = bounds2df, aes(x=seq(startYear, endYear, 1), ymin=l,ymax=u, fill = as.factor(ci)), alpha = 0.6, show.legend = T),
                   comp$layers)
  
  comp <- comp + 
    scale_fill_manual(values = c("grey69", "springgreen4"), labels = c(paste0((1-vsignificance)*100, "% confidence set"), paste0((1-2*vsignificance)*100, "% confidence set")))
  
  
  return(comp)
}

#' Plot a pie chart of the composition of a synthetic country
#' 
#' @param synthtables A list, as output by the synth.tab function in the "Synth"
#'   package, including the unit weights as an N x 3 data.frame "tab.w", where
#'   the "w.weights" column contains the unit weights and the "unit.names" 
#'   column contains the unit names.
#' @param maxSlices The maximal amount of detailed slices in the pie chart.
#'   Slices beyond these are aggregated.
#'   
#' @return An instance of ggplot with the pie chart with up to maxSlices named.
plotPie <- function(synthtables, maxSlices = 8) {
  funpie.data <- synthtables[["tab.w"]][1:2][which(synthtables[["tab.w"]][["w.weights"]] != 0), ]
  funpie.data <- funpie.data[order(-funpie.data$w.weights),]
  
  if(nrow(funpie.data) > maxSlices){
    funpie.data[maxSlices,]$w.weights <- signif(1 - sum(funpie.data[1:(maxSlices-1),]$w.weights), digits = 3)
    funpie.data[maxSlices,]$unit.names <- paste0("Others (", nrow(funpie.data) - maxSlices +1, ")")
    funpie.data <- funpie.data[1:maxSlices,]
  }
  
  funpie.data$label <- paste0(funpie.data$w.weights*100, "% ", funpie.data$unit.names)
  
  theme_update(plot.title = element_text(hjust = 0.5))
  
  pie <- funpie.data %>%
    mutate(unit.names = factor(x = unit.names,
                               levels = unit.names), # ideally you should specify the order explicitly
           label = factor(x = label,
                          levels = rev(label))) %>%
    ggplot(aes(x="", y=w.weights, fill=label)) +
    geom_bar(stat="identity", width=1, color="black") +
    coord_polar("y", start=0) +
    guides(fill = guide_legend(ncol=2,byrow=TRUE, reverse = TRUE)) +
    scale_fill_brewer(palette="Greens") +
    theme_void() +
    theme(legend.title = element_blank(),
          legend.key.width=unit(24,"pt"),
          legend.key.height=unit(24,"pt"),
          legend.position="bottom",
          legend.text = element_text(margin = margin(r = 24, unit = "pt"),
                                     size = 24,
                                     color = "black"))
  
  return(pie)
}

#' Plot the gaps between the actual values and the synthetic estimate 
#' for a single country, including random donor estimates, where the main
#' estimate and the one with lowest pre-accession MSE are highlighted
#' 
#' @param countryCode A unique regionno number, under which the country 
#'   is listed in the "world" data.frame.
#' @param countryISO3 A unique three capital letter String for the country,
#'   as defined in ISO 3166-1
#' @param randomizedSampleEstimates A T x (1 + R) data.frame with the estimates
#'   with random donor samples. The first column contains the main estimate.
#'   R is the number of random donor estimates.
#' @param losses A 1 x (1 + R) data.frame with the metric to be minimized
#'   (e.g. MSE) to determine the optimal estimate pre-accession, 
#'   starting with the main estimate's,
#'   where R is the number of random donor estimates.
#' @param useRgdpch A boolean value. If TRUE, GDP per capita is plotted, 
#'   if FALSE, GDP per worker is plotted.
#'   
#' @return A list of two ggplot instances: 
#'   one with the gaps between actual values and
#'   the random donor samples synthetic estimates (with the main and best fit
#'   estimates highlighted) and
#'   one with a boxplot of the last realizations of all gaps.
plotRDSParallel <- function(countryCode, countryISO3, randomizedSampleEstimates, losses, useRgdpch = T) {
  
  if(useRgdpch){
    btemp <- data.frame(world[which(world$regionno == countryCode & world$year >= startYear & world$year <= endYear),]$rgdpch - randomizedSampleEstimates)
    yLab <- "Gap in GDP per capita"
    realization <- data.frame(temp[which(temp$regionname==countryISO3),]$rgdpch)
    ylim <- c(-15000, 25000)
    breaksY <- 10000
  } else {
    btemp <- data.frame(world[which(world$regionno == countryCode & world$year >= startYear & world$year <= endYear),]$rgdpwok - randomizedSampleEstimates)
    yLab <- "Gap in GDP per worker"
    realization <- data.frame(temp[which(temp$regionname==countryISO3),]$rgdpwok)
    ylim <- c(-30000, 50000)
    breaksY <- 20000
  }
  rownames(realization) <- temp[which(temp$regionname==countryISO3),]$year
  btemp$index <- as.numeric(unlist(rownames(btemp)))
  
  btempRDS <- melt(btemp,  id = 'index', variable.name = 'series')
  l <- ggplot(btempRDS, aes(index, value, group = series))
  
  l$layers <- c(geom_vline(aes(xintercept = eventYear), linetype = 2), l$layers)
  
  left_pl <- l + theme(legend.position = "none") + 
    coord_cartesian(xlim = c(1990, 2019), ylim = ylim) + 
    geom_line(aes(color="Alternative random donor samples")) +
    geom_hline(aes(yintercept = 0), linetype = "f8") +
    geom_line(data = btempRDS[which(btempRDS$series == "w.weight"),], aes(color = "Main estimate"), size = 1) +
    geom_line(data = btempRDS[which(btempRDS$series == names(randomizedSampleEstimates)[apply(losses, MARGIN = 1, FUN = which.min)]),], aes(color = "Best pre-accession fit"), size = 1) +
    scale_y_continuous(c(0,breaks=seq(ylim[1],ylim[2],breaksY)))+
    theme(text = element_text(size=19),
          panel.border = element_rect(linetype = 1, fill = NA),
          panel.background = element_rect(fill = NA, colour = NA),
          axis.text = element_text(size=19),
          axis.text.y = element_text(angle = 90, hjust = 0.5),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    guides(linetype=guide_legend(keywidth = 3, keyheight = 1)) +
    scale_color_manual("",
                       values = c("Main estimate" = "black", "Best pre-accession fit" = "springgreen4", "Alternative random donor samples" = "grey"))+
    theme(legend.title=element_blank(),
          legend.position = c(0.05, 0.95),
          legend.justification = c(0, 1),
          legend.text=element_text(size=19)) +
    theme(plot.margin = unit(c(0.2,0.7,0.2,0.2), "cm"))
  
  
  btempBoxPlot <- melt(tail(btemp, n=1) ,  id = 'index', variable.name = 'series')
  r <- ggplot(btempBoxPlot, aes(index, value))
  right_pl <- r + 
    geom_boxplot() + 
    coord_cartesian(ylim = ylim) + 
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = NA, colour = NA)) +
    labs(x = "")+
    scale_x_continuous(labels = function(breaks) {rep_along(breaks, "")})
  
  bgrrdsplot <- list(left_pl, right_pl)
  return(bgrrdsplot)
}

#' Plot the gaps between the actual values and the synthetic estimate 
#' for a country and the corresponding placebo-in-space estimates for
#' the donor sample countries
#' 
#' @param realiz A T x (N + 1) data.frame with all gaps between actual values 
#'   and synthetic estimates, including the main estimate.
#' @param countryCode A unique regionno number, under which the country 
#'   is listed in the "world" data.frame.
#' @param useRgdpch A boolean value. If TRUE, GDP per capita is plotted, 
#'   if FALSE, GDP per worker is plotted.
#' @param ylim Numeric, used to set the limits of the y axis.
#' 
#' @return An instance of ggplot with the gaps between actual values and
#'   synthetic control estimates for a country and the placebo-in-space
#'   estimates for the donor sample countries.
plotDonorSampleSynth <- function(realiz, countryCode, useRgdpch = T, ylim = c(-15000, 30000)) {
  
  if(useRgdpch){
    yLab <- "Gap in GDP per capita"
  } else {
    yLab <- "Gap in GDP per worker"
  }
  realiz$index <- as.numeric(unlist(rownames(realiz)))
  
  meltedrealiz <- melt(realiz,  id = 'index', variable.name = 'series')
  l <- ggplot(meltedrealiz, aes(index, value, group = series))
  
  l$layers <- c(geom_vline(aes(xintercept = eventYear), linetype = 2), l$layers)
  
  plotAll <- l + theme(legend.position = "none") + 
    coord_cartesian(xlim = c(1990, 2019), ylim = ylim) + 
    geom_line(aes(color="Donor sample estimates")) +
    geom_hline(aes(yintercept = 0), linetype = "f8") +
    geom_line(data = meltedrealiz[which(meltedrealiz$series == countryCode),], aes(color = "Main estimate"), size = 1) +
    scale_y_continuous(breaks=seq(ylim[1],ylim[2],10000))+
    theme(text = element_text(size=19),
          panel.border = element_rect(linetype = 1, fill = NA),
          panel.background = element_rect(fill = NA, colour = NA),
          axis.text = element_text(size=19),
          axis.text.y = element_text(angle = 90, hjust = 0.5),
          axis.title.x = element_blank()) +
    labs(y = yLab)+
    guides(linetype=guide_legend(keywidth = 3, keyheight = 1)) +
    scale_color_manual("",
                       values = c("Main estimate" = "black", "Donor sample estimates" = "grey"))+
    theme(legend.title=element_blank(),
          legend.position = c(0.05, 0.95),
          legend.justification = c(0, 1),
          legend.text=element_text(size=19)) +
    theme(plot.margin = unit(c(0.2,0.7,0.2,0.2), "cm"))
  
  return(plotAll)
}

#' Plot the RMSPE ratios for the main estimate and all 
#' placebo-in-space estimates for the donor sample countries
#' 
#' @param rmspes An (N + 1) x 2 data.frame with the first one consisting of 
#'   ISO3 country codes of the countries from the donor sample and the target
#'   country, while the second one consists of the RMSPE ratios 
#' @param countryISO3 A unique three capital letter String for the country,
#'   as defined in ISO 3166-1
#'   
#' @return An instance of ggplot with all RMSPE ratios, 
#'   the main estimate highlighted
plotRatiosOfRMSPE <- function(rmspes, countryISO3) {
  p<-ggplot(data=rmspes, aes(x=reorder(country, rmspe), y=rmspe, fill = factor(ifelse(country == countryISO3, "Highlighted", "Normal")))) +
    geom_bar(stat="identity")+
    scale_fill_manual(values = c("black", "gray")) + 
    scale_y_continuous(breaks=c(0,5,10,15,20,25,30,35,40)) +
    theme(legend.position = "none",
          text = element_text(size=19),
          panel.border = element_rect(linetype = 1, fill = NA),
          panel.background = element_rect(fill = NA, colour = NA),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
  return(p + coord_flip())
}