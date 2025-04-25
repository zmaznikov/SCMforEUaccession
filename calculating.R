#' Add a data.frame to clipboard
#' 
#' @param x A data.frame to be added to clipboard.
#' @param row.names A boolean indicating whether row names should be included
#' @param col.names A boolean indicating whether column names should be included
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

#' Filter the IDs of countries to get these that
#' a) have some value for each predictor in the pre-treatment period and
#' b) have rgdpch values for the entire period
#'
#' @param sample A subset of data from "world", limited by the period
#'   to be analysed, for the countries in the donor pool and the target country
#' @param startYear A numeric marking the first year to be analysed.
#' @param eventYear A numeric marking the year of intervention in the analysis.
#' @param countryCode A unique regionno number, under which the country 
#'   is listed in the "world" data.frame.
#'
#' @return A list of the unique regionno numbers, under which the countries 
#'   fulfilling the criteria and the target countries are listed in the "world"
#'   data.frame.
filterCountries <- function(sample, startYear, eventYear, countryCode) {
  cident <- unique(sample$regionno)
  
  #numbers of countries with some agr value between startYear and (eventYear-1)
  aID <- unique(sample[which(sample$year>= startYear & sample$year <= (eventYear-1) & !(is.na(sample$agr))), ]$regionno)
  
  #numbers of countries with some inc value between startYear and (eventYear-1)
  incID <- unique(sample[which(sample$year>= startYear & sample$year <= (eventYear-1) & !(is.na(sample$rgdpwok))), ]$regionno)
  
  #numbers of countries with some ind value between startYear and (eventYear-1)
  indID <- unique(sample[which(sample$year>= startYear & sample$year <= (eventYear-1) & !(is.na(sample$ind))), ]$regionno)
  
  #numbers of countries with some inv value between startYear and (eventYear-1)
  invID <- unique(sample[which(sample$year>= startYear & sample$year <= (eventYear-1) & !(is.na(sample$ki))), ]$regionno)
  
  #numbers of countries with some sec value between startYear and (eventYear-1)
  secID <- unique(sample[which(sample$year>= startYear & sample$year <= (eventYear-1) & !(is.na(sample$sec))), ]$regionno)
  
  #numbers of countries with some ter value between startYear and (eventYear-1)
  terID <- unique(sample[which(sample$year>= startYear & sample$year <= (eventYear-1) & !(is.na(sample$ter))), ]$regionno)
  
  #numbers of countries with some pop value between startYear and (eventYear-1)
  popID <- unique(sample[which(sample$year>= startYear & sample$year <= (eventYear-1) & !(is.na(sample$popgr))), ]$regionno)
  
  #numbers of countries with some gdp value between startYear and (eventYear-1)
  gdpID <- unique(sample[which(sample$year>= startYear & sample$year <= (eventYear-1) & !(is.na(sample$rgdpch))), ]$regionno)
  
  #numbers of countries where the starting Year of rgdpch or a later one is missing
  gdpNA.ID <- unique(sample[which(sample$year == startYear & is.na(sample$rgdpch)), ]$regionno)
  
  cident0 <- cident[!cident %in% c(countryCode)]
  cident0 <- cident0[!cident0 %in% gdpNA.ID]
  
  #removed incID
  cID <- Reduce(intersect, list(cident0, aID, indID, invID, secID, terID, popID, gdpID))
  
  return(cID)
}

#' Filter the IDs of countries to get these that
#' a) have some value for each predictor in the pre-treatment period and
#' b) have rgdpwok values for the entire period
#'
#' @param sample A subset of data from "world", limited by the period
#'   to be analysed, for the countries in the donor pool and the target country
#' @param startYear A numeric marking the first year to be analysed.
#' @param eventYear A numeric marking the year of intervention in the analysis.
#' @param countryCode A unique regionno number, under which the country 
#'   is listed in the "world" data.frame.
#'
#' @return A list of the unique regionno numbers, under which the countries 
#'   fulfilling the criteria and the target countries are listed in the "world"
#'   data.frame.
filterCountriesWOK <- function(sample, startYear, eventYear, countryCode) {
  cident <- unique(sample$regionno)
  
  #numbers of countries with some agr value between startYear and (eventYear-1)
  aID <- unique(sample[which(sample$year>= startYear & sample$year <= (eventYear-1) & !(is.na(sample$agr))), ]$regionno)
  
  #numbers of countries with some ind value between startYear and (eventYear-1)
  indID <- unique(sample[which(sample$year>= startYear & sample$year <= (eventYear-1) & !(is.na(sample$ind))), ]$regionno)
  
  #numbers of countries with some inv value between startYear and (eventYear-1)
  invID <- unique(sample[which(sample$year>= startYear & sample$year <= (eventYear-1) & !(is.na(sample$ki))), ]$regionno)
  
  #numbers of countries with some sec value between startYear and (eventYear-1)
  secID <- unique(sample[which(sample$year>= startYear & sample$year <= (eventYear-1) & !(is.na(sample$sec))), ]$regionno)
  
  #numbers of countries with some ter value between startYear and (eventYear-1)
  terID <- unique(sample[which(sample$year>= startYear & sample$year <= (eventYear-1) & !(is.na(sample$ter))), ]$regionno)
  
  #numbers of countries with some pop value between startYear and (eventYear-1)
  popID <- unique(sample[which(sample$year>= startYear & sample$year <= (eventYear-1) & !(is.na(sample$popgr))), ]$regionno)
  
  #numbers of countries with some rgdpwok value between startYear and (eventYear-1)
  wokID <- unique(sample[which(sample$year>= startYear & sample$year <= (eventYear-1) & !(is.na(sample$rgdpwok))), ]$regionno)
  
  #numbers of countries where the starting Year of rgdpwok or a later one is missing
  wokNA.ID <- unique(sample[which(sample$year > startYear & is.na(sample$rgdpwok)), ]$regionno)
  
  cident0 <- cident[!cident %in% c(countryCode)]
  cident0 <- cident0[!cident0 %in% wokNA.ID]
  
  #removed incID
  cID <- Reduce(intersect, list(cident0, aID, indID, invID, secID, terID, popID, wokID))
  
  return(cID)
}

#' Calculate confidence sets at two significance levels
#'
#' @param synthAlli A list containing Ymat and weightsmat tables, as defined in SCM.CS
#'   (Article: Synthetic Control Method: Inference, Sensitivity Analysis and
#'   Confidence Sets with authors Sergio Firpo and Vitor Possebom),
#'   a T x N "synthest" data.frame with the placebo-in-space
#'   synthetic control estimates
#'   a 1 x (N + 1) "losses" data.frame with the metric to be minimized
#'   (MSE) to determine the optimal estimate pre-accession.
#' @param countryCode A unique regionno number, under which the country 
#'   is listed in the "world" data.frame.
#' @param treated "The column in matrix "Ymat" associated with the treated
#'   region. It must be a natural number less than or equal to the number of
#'   columns in matrix "Ymat"."
#' @param T0 "The row in matrix "Ymat" associated with the last pre-intervention
#'   period. It must be a natural number less than or equal to the number of
#'   rows in matrix "Ymat"."
#' @param phi "The sensitivity parameter defined either in step 6 or in step 7 of
#'   section 3 of Firpo and Possebom (2017). It has to be a positive real number.
#'   If you only want to construct the standard confidence interval under the
#'   assumption that each region is equally likely to receive treatment, set
#'   'phi' to zero."
#' @param v "The worst (best) case scenario vector defined in step 6 (step 7) of
#'   section 3 of Firpo and Possebom (2017). It is row vector whose length is
#'   equal to the number of observed regions, i.e., J+1. The elements of this
#'   vector must be equal to 0 or 1. If you only want to construct the standard
#'   confidence interval under the assumption that each region is equally likely
#'   to receive treatment, let 'v' be a zero vector."
#' @param precision "A natural number. A larger value for 'precision' makes the
#'   estimation of the confidence sets more precise, requiring more computing
#'   time. A value between 20 and 30 is reasonable."
#' @param type "A character vector equal to "linear", "constant" or "uniform". 
#'   Defines which confidence subset is implemented. The "uniform"
#'   confidence set was proposed by Ferman, Pinto and Possebom (2020) and
#'   contains all functions that are deviations from estimated treatment effect
#'   by an additive and constant factor and are not rejected by the placebo 
#'   test."
#' @param vsignificance "The significance level in decimal form."
#' @param plot "A logical value that determines whether a plot
#'   with the computed confidence set will be provided or not."
#'
#' @return A list of data.frames, including confidence sets at two significance
#'   levels and all gaps between actual values and placebo-in-space estimates
getBounds <- function(synthAlli, countryCode, treated, T0, phi = 0, v, precision = 30, type,
                      vsignificance, plot = F){
  bounds <- SCM.CS(synthAlli$Ymat, synthAlli$weightsmat, treated, T0, phi, v, precision, type,
                   vsignificance, plot)
  boundsdf <- data.frame(cbind(bounds$l, bounds$u))
  colnames(boundsdf) <- c("l", "u")
  boundsdf$ci <- 1
  
  bounds2 <- SCM.CS(synthAlli$Ymat, synthAlli$weightsmat, treated, T0, phi, v, precision, type,
                    2*vsignificance, plot)
  
  bounds2df <- data.frame(cbind(bounds2$l, bounds2$u))
  colnames(bounds2df) <- c("l", "u")
  bounds2df$ci <- 2
  
  realiz <- data.frame(synthAlli[["Ymat"]]-synthAlli[["synthest"]])
  rownames(realiz) <- seq(startYear, endYear, 1)
  colnames(realiz) <- union(cID, countryCode)
  
  return(list(boundsVsignificance = boundsdf, bounds2Vsignificance = bounds2df, allGapEstimates = realiz))
}

#' Calculate the average post-intervention effect of the main estimate
#'
#' @param dataprepout A list, as output by the dataprep function in the "Synth"
#'   package, including the outcome variable for the entire period for the 
#'   donor sample units as a T x N matrix "Y0plot".
#' @param synthout A list, as output by the synth function in the "Synth" 
#'   package, including unit weights as an N x 1 vector "solution.w".
#' @param countryCode A unique regionno number, under which the country 
#'   is listed in the "world" data.frame.
#' @param startYear A numeric marking the first year to be analysed.
#' @param eventYear A numeric marking the year of intervention in the analysis.
#' @param endYear A numeric marking the last year in the analysis.
#' @param lastSummaryYear A numeric marking the last year
#'   the effect should be averaged for.
#' @param useRgdpch A boolean value. If TRUE, GDP per capita is used, 
#'   if FALSE, GDP per worker is used.
#'
#' @return The average post-intervention effect of the main estimate.
summaryEffects <- function(dataprepout, synthout, countryCode, startYear, eventYear, endYear, lastSummaryYear, useRgdpch){
  if(useRgdpch){
    bcomp <- data.frame((world[which(world$regionno == countryCode & world$year >= startYear & world$year <= endYear),]$rgdpch - dataprepout$Y0plot%*%synthout$solution.w)/dataprepout$Y0plot%*%synthout$solution.w)
  } else {
    bcomp <- data.frame((world[which(world$regionno == countryCode & world$year >= startYear & world$year <= endYear),]$rgdpwok - dataprepout$Y0plot%*%synthout$solution.w)/dataprepout$Y0plot%*%synthout$solution.w)
  }
  bcomp2 <- bcomp[(eventYear-startYear+1):(lastSummaryYear-startYear+1),]
  return(mean(bcomp2))
}

#' Get summary statistics for the random donor sample estimates
#'
#' @param countryCode A unique regionno number, under which the country 
#'   is listed in the "world" data.frame.
#' @param randomizedSampleEstimates A list containing a T x R "RDSestimates" 
#'   data.frame with the estimates with random donor samples and 
#'   a 1 x R "losses" data.frame with the metric to be minimized
#'   (e.g. MSE) to determine the optimal estimate pre-accession,
#'   where R is the number of random donor estimates.
#' @param startYear A numeric marking the first year to be analysed.
#' @param eventYear A numeric marking the year of intervention in the analysis.
#' @param lastSummaryYear A numeric marking the last year
#'   the effects should be averaged for.
#' @param useRgdpch A boolean value. If TRUE, GDP per capita is used, 
#'   if FALSE, GDP per worker is used.
#'
#' @return A list containing the median, the mean, 
#'   the share of positive effects (in percent) of the average effects
#'   and the best fit effect.
summaryRDS <- function(countryCode, randomizedSampleEstimates, startYear, eventYear, lastSummaryYear, useRgdpch = T) {
  if(useRgdpch){
    btemp <- (data.frame(world[which(world$regionno == countryCode & world$year >= startYear & world$year <= endYear),]$rgdpch - randomizedSampleEstimates$RDSestimates)) / randomizedSampleEstimates$RDSestimates
  } else {
    btemp <- (data.frame(world[which(world$regionno == countryCode & world$year >= startYear & world$year <= endYear),]$rgdpwok - randomizedSampleEstimates$RDSestimates)) / randomizedSampleEstimates$RDSestimates
  }
  
  btemp <- btemp[(eventYear-startYear+1):(lastSummaryYear-startYear+1),]
  
  btempMeans <- colMeans(btemp)
  
  atemp <- table(sign(btempMeans))
  anum <- atemp[names(atemp)==1]/dim(randomizedSampleEstimates$RDSestimates)[2]

  return(list(median = median(btempMeans), average = mean(btempMeans), percentPositive = anum*100, bestFitEffect = btempMeans[apply(randomizedSampleEstimates$losses, MARGIN = 1, FUN = which.min)]))
  #return(list(effect = as.numeric(btemp[year,][1]), medianRDSEffect = median(as.matrix(btemp[year,][-1])), averageRDSEffect = mean(as.matrix(btemp[year,][-1])), positiveEffects = positiveEffects))
}

#' Estimate GDP per capita synthetic controls with random donor samples
#' (parallelized)
#'
#' @param countryCode A unique regionno number, under which the country 
#'   is listed in the "world" data.frame.
#' @param countryISO3 A unique three capital letter String for the country,
#'   as defined in ISO 3166-1
#' @param sampleSize The size of the samples to be randomly drawn.
#' @param world A data.frame with all values in different columns
#'   for all countries available.
#' @param startYear A numeric marking the first year to be analysed.
#' @param eventYear A numeric marking the year of intervention in the analysis.
#' @param endYear A numeric marking the last year in the analysis.
#' @param iterations R. The number of random donor samples to be estimated.
#' @param addToSeed A constant to be added to the iteration counter to get
#'   the seed for randomly drawing the countries for a donor sample.
#'
#' @return A list containing four data.frames:
#'   T x R "RDSestimates": the GDP per capita estimates
#'   with random donor samples
#'   N x R "RDSweights": the country weights for the synthetic estimates
#'   N x R "RDSamples": the country numbers in the random donor samples,
#'   as referred to in the regionno column of the "world" data.frame
#'   1 x R "losses": the metric to be minimized
#'   (MSE) to determine the optimal estimate pre-accession.
randomizeSampleParallel <- function(countryCode, countryISO3, sampleSize, world, startYear, eventYear, endYear, iterations = 99, addToSeed = 600) {
  
  # Setup parallel backend to use all but one of cores.
  n.cores <- detectCores()-1
  cl <- makeCluster(n.cores)
  registerDoParallel(cl)
  
  results <- foreach(j=1:iterations, .combine = rbind, .packages = "Synth") %dopar% {
    
    # Prepare the data in order to use the synthetic control estimator
    temp <- world[which(world$year >= startYear & world$year <= endYear & (! world$regionname %in% c("ROU", "BGR", "AUT", "BEL", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "SVK", "SVN", "ESP", "SWE", "GBR") | world$regionname %in% countryISO3)), ]
    
    cident <- unique(temp$regionno)
    
    #numbers of countries with some agr value between the starting year and the year befire the event year
    aID <- unique(temp[which(temp$year>= startYear & temp$year <= (eventYear-1) & !(is.na(temp$agr))), ]$regionno)
    
    #numbers of countries with some ind value between the starting year and the year befire the event year
    indID <- unique(temp[which(temp$year>= startYear & temp$year <= (eventYear-1) & !(is.na(temp$ind))), ]$regionno)
    
    #numbers of countries with some inv value between the starting year and the year befire the event year
    invID <- unique(temp[which(temp$year>= startYear & temp$year <= (eventYear-1) & !(is.na(temp$ki))), ]$regionno)
    
    #numbers of countries with some sec value between the starting year and the year befire the event year
    secID <- unique(temp[which(temp$year>= startYear & temp$year <= (eventYear-1) & !(is.na(temp$sec))), ]$regionno)
    
    #numbers of countries with some ter value between the starting year and the year befire the event year
    terID <- unique(temp[which(temp$year>= startYear & temp$year <= (eventYear-1) & !(is.na(temp$ter))), ]$regionno)
    
    #numbers of countries with some pop value between the starting year and the year befire the event year
    popID <- unique(temp[which(temp$year>= startYear & temp$year <= (eventYear-1) & !(is.na(temp$popgr))), ]$regionno)
    
    #numbers of countries with some gdp value between the starting year and the year befire the event year
    gdpID <- unique(temp[which(temp$year>= startYear & temp$year <= (eventYear-1) & !(is.na(temp$rgdpch))), ]$regionno)
    
    #numbers of countries where the starting Year of gdp is missing
    gdpNA.ID <- unique(temp[which(temp$year == startYear & is.na(temp$rgdpch)), ]$regionno)
    
    cident0 <- cident[!cident %in% c(countryCode)]
    cident0 <- cident0[!cident0 %in% gdpNA.ID]
    #removed incID
    cID <- Reduce(intersect, list(cident0, aID, indID, invID, secID, terID, popID, gdpID))
    
    
    set.seed(j+addToSeed)
    randomCID <- sample(cID, size = sampleSize)
    
    dataprep.out<-
      dataprep(
        foo = temp,
        predictors = c("rgdpch", "ki", "agr", "ind", "popgr", "ter", "sec"),
        predictors.op = "mean",
        dependent = "rgdpch",
        unit.variable = "regionno",
        time.variable = "year",
        treatment.identifier = countryCode,
        controls.identifier = randomCID,
        time.predictors.prior = c(startYear:(eventYear-1)),
        time.optimize.ssr = c(startYear:(eventYear-1)),
        unit.names.variable = "regionname",
        time.plot = startYear:endYear
      )
    # run synth
    synth.out <- synth(data.prep.obj = dataprep.out, optimxmethod = 'All')
    
    synth.tables <- synth.tab(
      dataprep.res = dataprep.out,
      synth.res = synth.out)
    
    return(c(dataprep.out$Y0plot%*%synth.out$solution.w, NA, synth.tables$tab.w$w.weights, NA, randomCID, NA, synth.out$loss.v))
  }
  ## Stop parallel backend ####
  stopCluster(cl)
  return(list(RDSestimates = t(results[, 1:(endYear-startYear+1)]), RDSweights = t(results[, (endYear-startYear+1+2):(endYear-startYear+1+2 + length(cID)-1)]), RDSamples = t(results[,(endYear-startYear+1+2 + length(cID)-1+2):(endYear-startYear+1+2 + length(cID)-1+2+ length(cID)-1)]), losses = t(results[, ncol(results)])))
}

#' Estimate GDP per worker synthetic controls with random donor samples
#' (parallelized)
#'
#' @param countryCode A unique regionno number, under which the country 
#'   is listed in the "world" data.frame.
#' @param countryISO3 A unique three capital letter String for the country,
#'   as defined in ISO 3166-1
#' @param sampleSize The size of the samples to be randomly drawn.
#' @param world A data.frame with all values in different columns
#'   for all countries available.
#' @param startYear A numeric marking the first year to be analysed.
#' @param eventYear A numeric marking the year of intervention in the analysis.
#' @param endYear A numeric marking the last year in the analysis.
#' @param iterations R. The number of random donor samples to be estimated.
#' @param addToSeed A constant to be added to the iteration counter to get
#'   the seed for randomly drawing the countries for a donor sample.
#'
#' @return A list containing four data.frames:
#'   T x R "RDSestimates": the GDP per worker estimates
#'   with random donor samples
#'   N x R "RDSweights": the country weights for the synthetic estimates
#'   N x R "RDSamples": the country numbers in the random donor samples,
#'   as referred to in the regionno column of the "world" data.frame
#'   1 x R "losses": the metric to be minimized
#'   (MSE) to determine the optimal estimate pre-accession.
randomizeSampleWOKParallel <- function(countryCode, countryISO3, sampleSize, world, startYear, eventYear, endYear, iterations = 99, addToSeed = 600) {
  
  # Setup parallel backend to use all but one of cores.
  n.cores <- detectCores()-1
  cl <- makeCluster(n.cores)
  registerDoParallel(cl)
  
  results <- foreach(j=1:iterations, .combine = rbind, .packages = "Synth") %dopar% {
    
    # Prepare the data in order to use the synthetic control estimator
    temp <- world[which(world$year >= startYear & world$year <= endYear & (! world$regionname %in% c("ROU", "BGR", "AUT", "BEL", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "SVK", "SVN", "ESP", "SWE", "GBR") | world$regionname %in% countryISO3)), ]
    
    cident <- unique(temp$regionno)
    
    #numbers of countries with some agr value between the starting year and the year befire the event year
    aID <- unique(temp[which(temp$year>= startYear & temp$year <= (eventYear-1) & !(is.na(temp$agr))), ]$regionno)
    
    #numbers of countries with some ind value between the starting year and the year befire the event year
    indID <- unique(temp[which(temp$year>= startYear & temp$year <= (eventYear-1) & !(is.na(temp$ind))), ]$regionno)
    
    #numbers of countries with some inv value between the starting year and the year befire the event year
    invID <- unique(temp[which(temp$year>= startYear & temp$year <= (eventYear-1) & !(is.na(temp$ki))), ]$regionno)
    
    #numbers of countries with some sec value between the starting year and the year befire the event year
    secID <- unique(temp[which(temp$year>= startYear & temp$year <= (eventYear-1) & !(is.na(temp$sec))), ]$regionno)
    
    #numbers of countries with some ter value between the starting year and the year befire the event year
    terID <- unique(temp[which(temp$year>= startYear & temp$year <= (eventYear-1) & !(is.na(temp$ter))), ]$regionno)
    
    #numbers of countries with some pop value between the starting year and the year befire the event year
    popID <- unique(temp[which(temp$year>= startYear & temp$year <= (eventYear-1) & !(is.na(temp$popgr))), ]$regionno)
    
    #numbers of countries with some rgdpwok value between the starting year and the year befire the event year
    wokID <- unique(temp[which(temp$year>= startYear & temp$year <= (eventYear-1) & !(is.na(temp$rgdpwok))), ]$regionno)
    
    #numbers of countries where the starting Year of rgdpwok is missing
    wokNA.ID <- unique(temp[which(temp$year > startYear & is.na(temp$rgdpwok)), ]$regionno)
    
    cident0 <- cident[!cident %in% c(countryCode)]
    cident0 <- cident0[!cident0 %in% wokNA.ID]
    
    cID <- Reduce(intersect, list(cident0, aID, indID, invID, secID, terID, popID, wokID))
    
    
    set.seed(j+addToSeed)
    randomCID <- sample(cID, size = sampleSize)
    
    dataprep.out<-
      dataprep(
        foo = temp,
        predictors = c("rgdpwok", "ki", "agr", "ind", "popgr", "ter", "sec"),
        predictors.op = "mean",
        dependent = "rgdpwok",
        unit.variable = "regionno",
        time.variable = "year",
        treatment.identifier = countryCode,
        controls.identifier = randomCID,
        time.predictors.prior = c(startYear:(eventYear-1)),
        time.optimize.ssr = c(startYear:(eventYear-1)),
        unit.names.variable = "regionname",
        time.plot = startYear:endYear
      )
    # run synth
    synth.out <- synth(data.prep.obj = dataprep.out, optimxmethod = 'All')
    
    synth.tables <- synth.tab(
      dataprep.res = dataprep.out,
      synth.res = synth.out)
    
    return(c(dataprep.out$Y0plot%*%synth.out$solution.w, NA, synth.tables$tab.w$w.weights, NA, randomCID, NA, synth.out$loss.v))
  }
  ## Stop parallel backend ####
  stopCluster(cl)
  return(list(RDSestimates = t(results[, 1:(endYear-startYear+1)]), RDSweights = t(results[, (endYear-startYear+1+2):(endYear-startYear+1+2 + length(cID)-1)]), RDSamples = t(results[,(endYear-startYear+1+2 + length(cID)-1+2):(endYear-startYear+1+2 + length(cID)-1+2+ length(cID)-1)]), losses = t(results[, ncol(results)])))
  
}

#' Calculate the RMSPE ratios for the main estimate and all 
#' placebo-in-space estimates for the donor sample countries
#'
#' @param synthAll A list of estimates, including placebo-in-space,
#'   as output by the "synthAll" function.
#' @param cID A vector of country codes, under which the donor sample countries 
#'   are listed in the "world" data.frame.
#' @param countryISO3 A unique three capital letter String for the country,
#'   as defined in ISO 3166-1
#' @param startYear A numeric marking the first year to be analysed.
#' @param eventYear A numeric marking the year of intervention in the analysis.
#' @param endYear A numeric marking the last year in the analysis.
#'
#' @return RMSPE ratios in descending order
getRatiosOfRMSPE <- function(synthAll, cID, countryISO3, startYear, eventYear, endYear) {
  rmspe <- NULL
  for (i in 1:dim(synthAll$Ymat)[2]) {
    rmspe[i] <- sqrt(mean((synthAll$Ymat[(eventYear-startYear+1):(endYear-startYear+1),i] - synthAll$synthest[(eventYear-startYear+1):(endYear-startYear+1),i])^2)/mean((synthAll$Ymat[1:(eventYear-startYear),i] - synthAll$synthest[1:(eventYear-startYear),i])^2))
  }
  rmspes <- data.frame(country = c(unique(world[which(world$regionno %in% cID), ]$regionname), countryISO3), rmspe = rmspe)
  return(rmspes[order(-rmspes$rmspe),])
}

#' Estimate the main synthetic control and placebo-in-space estimates
#'
#' @param temp A data.frame (subset of the "world" data.frame),
#'   restricted by startYear and endYear and cID and countryCode.
#' @param countryCode A unique regionno number, under which the target country 
#'   is listed in the "world" data.frame.
#' @param cID A vector of country codes, under which the donor sample countries 
#'   are listed in the "world" data.frame.
#' @param startYear A numeric marking the first year to be analysed.
#' @param eventYear A numeric marking the year of intervention in the analysis.
#' @param endYear A numeric marking the last year in the analysis.
#' @param useRgdpch A boolean value. If TRUE, GDP per capita is used, 
#'   if FALSE, GDP per worker is used.
#'
#' @return A list containing Ymat and weightsmat tables, as defined in SCM.CS
#'   (Article: Synthetic Control Method: Inference, Sensitivity Analysis and
#'   Confidence Sets with authors Sergio Firpo and Vitor Possebom),
#'   a T x N "synthest" data.frame with the placebo-in-space
#'   synthetic control estimates
#'   a 1 x (N + 1) "losses" data.frame with the metric to be minimized
#'   (MSE) to determine the optimal estimate pre-accession.
synthAll <- function(temp, countryCode, cID, startYear, eventYear, endYear, useRgdpch = T) {
  # Setup parallel backend to use all but one of cores.
  n.cores <- detectCores()-1
  cl <- makeCluster(n.cores)
  registerDoParallel(cl)
  
  if(useRgdpch) {
    preds <- c("rgdpch", "ki", "agr", "ind", "popgr", "ter", "sec")
    dep <- "rgdpch"
  } else {
    preds <- c("rgdpwok", "ki", "agr", "ind", "popgr", "ter", "sec")
    dep <- "rgdpwok"
  }
  
  dataprep.out<-
    dataprep(
      foo = temp,
      predictors = preds,
      predictors.op = "mean",
      dependent = dep,
      unit.variable = "regionno",
      time.variable = "year",
      treatment.identifier = countryCode,
      controls.identifier = cID,
      time.predictors.prior = c(startYear:(eventYear-1)),
      time.optimize.ssr = c(startYear:(eventYear-1)),
      unit.names.variable = "regionname",
      time.plot = startYear:endYear
    )
  # Estimating the synthetic control method
  synth.out <- synth(data.prep.obj = dataprep.out, method = "All")
  
  
  ## Estimate the SC weights for each state ####
  results <- foreach(j=union(cID, countryCode), .combine = rbind, .packages = "Synth") %dopar% {
    # Define the comparison regions.
    controlunits <- setdiff(union(cID, countryCode), j)
    # Prepare the data in order to use the synthetic control estimator
    if(useRgdpch) {
      preds <- c("rgdpch", "ki", "agr", "ind", "popgr", "ter", "sec")
      dep <- "rgdpch"
    } else {
      preds <- c("rgdpwok", "ki", "agr", "ind", "popgr", "ter", "sec")
      dep <- "rgdpwok"
    }
    
    dataprep.out<-
      dataprep(
        foo = temp,
        predictors = preds,
        predictors.op = "mean",
        dependent = dep,
        unit.variable = "regionno",
        time.variable = "year",
        treatment.identifier = j,
        controls.identifier = controlunits,
        time.predictors.prior = c(startYear:(eventYear-1)),
        time.optimize.ssr = c(startYear:(eventYear-1)),
        unit.names.variable = "regionname",
        time.plot = startYear:endYear
      )
    # Estimating the synthetic control method
    synth.out <- synth(data.prep.obj = dataprep.out, method = "All")
    Y <- dataprep.out$Y1plot
    weights <- synth.out$solution.w
    synthestimate <- dataprep.out$Y0plot%*%synth.out$solution.w
    lossV <- synth.out$loss.v
    return(c(Y, NA, weights, NA, synthestimate, NA, lossV))
  }
  ## Stop parallel backend ####
  stopCluster(cl)
  return(list(Ymat = t(results[, 1:(endYear-startYear+1)]), weightsmat = t(results[, (endYear-startYear+1+2):(endYear-startYear+1+2 + length(cID)-1)]), synthest = t(results[, (endYear-startYear+1+2 + length(cID)-1 +2):(ncol(results)-2)]), losses = t(results[, ncol(results)]), synthoutY1 = synth.out, dataprepoutY1 = dataprep.out))
}

###############################################################################
# Article: Synthetic Control Method: Inference, Sensitivity Analysis and
#          Confidence Sets
# Authors: Sergio Firpo and Vitor Possebom
# Code by: Vitor Possebom
# Function: Implement Confidence Sets and the Sensitivity Analysis Mechanism
#           for the Synthetic Control Estimator.
# Version: 08 - This function to implements the confidences Sets and the
#          Sensitivity Analysis Mechanism for the Synthetic Control Estimator
#          proposed by Firpo and Possebom (2018). We use the RMSPE test
#          statistic to compute confidence sets in this code. The user may want
#          to manually change the test statistic. Any questions, suggestions,
#          comments, critics can be sent to Vitor Possebom
#          (vitoraugusto.possebom@yale.edu). In particular, the user may want
#          to manually change the graphical parameters at the end of this code.
###############################################################################
SCM.CS <- function(Ymat, weightsmat, treated, T0, phi, v, precision, type,
                   significance, plot) {
  # 'Ymat' is a matrix that contains the realized outcome of interest. Each
  # column represents a region and each row represents a time period.
  if (is.matrix(Ymat) == FALSE) {
    stop("\n No matrix supplied in 'Ymat'.\n")
  }
  # 'weightsmat' is a matrix that contains the estimated weightsmat for the synthetic
  # control unit of each region. Each column represents a placebo region and
  # each row, a comparison unit. Pay attention that the order of the regions in
  # matrix 'weightsmat' must be identical to the order of the regions in matrix
  # 'Ymat'. 'weightsmat' is a matrix with J rows and J+1 columns, where J+1 is the
  # number of observed regions. In order to construct this matrix, one can
  # estimate a synthetic control unit using the command 'synth' (See Abadie,
  # Diamond and Hainmueller (2011).) and save the weightsmat found in the vector
  # solution.w for each region. Then, each vector solution.w will be a column
  # in matrix 'weightsmat'.
  if ((is.matrix(weightsmat) == FALSE) |
      ((dim(weightsmat)[1] != (dim(Ymat)[2] - 1)) == TRUE) |
      ((dim(weightsmat)[2] != dim(Ymat)[2]) == TRUE)) {
    stop("\n The supplied value for 'weightsmat' is invalid.\n")
  }
  # 'treated' is the column in matrix 'Ymat' associated with the treated
  # region. It must be a natural number less than or equal to the number of
  # columns in matrix 'Ymat'.
  if (((treated < 1) == TRUE) |
      ((treated > dim(Ymat)[2]) == TRUE) |
      ((treated != round(treated)) == TRUE)) {
    stop("\n The supplid value for 'treated' is invalid. \n")
  }
  # 'T0' is the row in matrix 'Ymat' associated with the last pre-intervention
  # period. It must be a natural number less than or equal to the number of
  # rows in matrix 'Ymat'.
  if (((T0 < 1) == TRUE) |
      ((T0 > dim(Ymat)[1]) == TRUE) |
      ((T0 != round(T0)) == TRUE)) {
    stop("\n The supplid value for 'T0' is invalid. \n")
  }
  # 'phi' is the sensitivity parameter defined either in step 6 or in step 7 of
  # section 3 of Firpo and Possebom (2017). It has to be a positive real number.
  # If you only want to construct the standard confidence interval under the
  # assumption that each region is equally likely to receive treatment, set
  # 'phi' to zero.
  if (phi < 0) {
    stop("\n The supplid value for 'phi' is invalid. \n")
  }
  # 'v' is the worst (best) case scenario vector defined in step 6 (step 7) of
  # section 3 of Firpo and Possebom (2017). It is row vector whose length is
  # equal to the number of observed regions, i.e., J+1. The elements of this
  # vector must be equal to 0 or 1. If you only want to construct the standard
  # confidence interval under the assumption that each region is equally likely
  # to receive treatment, let 'v' be a zero vector.
  if (((dim(v)[2] != dim(Ymat)[2]) == TRUE) |
      ((dim(v)[1] != 1) == TRUE) |
      (sum(as.numeric(v > 1)) != 0) |
      (sum(as.numeric(v < 0)) != 0)) {
    stop("\n The supplid value for 'v' is invalid. \n")
  }
  # 'precision' is a natural number. A larger value for 'precision' makes the
  # estimation of the confidence sets more precise, requiring more computing
  # time. A value between 20 and 30 is reasonable.
  if (((precision < 0) == TRUE) |
      ((precision != round(precision)) == TRUE)) {
    stop("\n The supplid value for 'precision' is invalid. \n")
  }
  # 'type' is a character vector equal to "linear", "constant" or "uniform".
  # 'type' defines which confidence subset is implemented. The "uniform"
  # confidence set was proposed by Ferman, Pinto and Possebom (2020) and
  # contains all functions that are deviations from estimated treatment effect
  # by an additive and constant factor and are not rejected by the placebo
  # test,
  if ((is.character(type) == FALSE) |
      (((type != "linear") == TRUE) & ((type != "constant") == TRUE) &
       ((type != "uniform") == TRUE))) {
    stop("\n The supplied value for 'type' is invalid. \n")
  }
  # 'significance' is the significance level in decimal form.
  if (((significance <= 0) == TRUE) |
      ((significance >= 1) == TRUE)) {
    stop("\n The supplied value for 'significance' is invalid. \n")
  }
  # 'plot' is a logical value that determines whether a plot with the
  # computed confidence set will be provided or not.
  if (is.logical(plot) == FALSE) {
    stop("\n The supplied value for 'plot' is invalid. \n")
  }
  # We define the initial parameter for the intervention effect.
  Y1 <- Ymat[, treated]
  Y0 <- Ymat[, setdiff(1:dim(Ymat)[2], treated)]
  gaps <- Y1 - Y0 %*% weightsmat[, treated]
  if (type == "constant") {
    param <- mean(gaps[(T0 + 1):dim(Ymat)[1], 1])
  } else if (type == "linear") {
    param <- gaps[dim(Ymat)[1], 1]/(dim(Ymat)[1] - T0)
  } else if (type == "uniform") {
    param <- 0
    postp <- rbind(matrix(0, T0, 1), matrix(1, dim(Ymat)[1] - T0, 1))
    nh0 <- rbind(
      matrix(0, T0, 1),
      matrix(gaps[(T0+1):dim(Ymat)[1], 1], dim(Ymat)[1] - T0, 1)
    )
  }
  s <- sign(param)
  ub <- param
  lb <- param
  # We start our loop whose iterations increase the precision of our
  # confidence sets.
  attemptu1 <- 1 # The loop has to start by not rejecting the null hypothesis.
  # If it starts by rejecting the null hypothesis, the confidence
  # set for the requested class of functions is empty.
  # This parameter will help us to guarantee that.
  attemptl1 <- 1 # The loop has to start by not rejecting the null hypothesis.
  # If it starts by rejecting the null hypothesis, the confidence
  # set for the requested class of functions is empty.
  # This parameter will help us to guarantee that.
  for (power in 0:precision) {
    step <- (1/2)^power
    reject.l <- 0
    reject.u <- 0
    # We start our loop whose interations try to find the upper bound
    # of our confidence set.
    while (reject.u == 0) {
      # Create the vector that represents the null hypothesis.
      if (type == "constant") {
        nh <- matrix(
          c(rep(0, T0), rep(ub, (dim(Ymat)[1] - T0))), dim(Ymat)[1], 1)
      } else if (type == "linear") {
        nh <- matrix(
          c(rep(0, T0),
            ub * seq(from = 1, to = (dim(Ymat)[1] - T0), by = 1)),
          dim(Ymat)[1], 1)
      } else if (type == "uniform") {
        nh <- nh0 + ub*postp
      }
      # Create a matrix to store the test statistics for each region.
      ts <- matrix(NA, 1, dim(Ymat)[2])
      # Estimate the gaps for each placebo unit.
      for (j in 1:dim(Ymat)[2]) {
        # Create the matrices of observed outcomes under the null.
        if (j == treated) {
          Y1 <- Ymat[, treated]
          Y0 <- Ymat[, setdiff(1:dim(Ymat)[2], treated)]
        } else {
          Y1 <- Ymat[, j] + nh
          Y0 <- Ymat[, setdiff(1:dim(Ymat)[2], j)]
          if (j < treated) {
            Y0[, (treated - 1)] <- Y0[, (treated - 1)] - nh
          } else if (j > treated) {
            Y0[, treated] <- Y0[, treated] - nh
          }
        }
        # Estimate the gaps.
        gaps <- Y1 - Y0 %*% weightsmat[, j] - nh
        # Estimate the test statistics (RMSPE).
        post <- (t(gaps[(T0 + 1):dim(Ymat)[1], 1]) %*%
                   gaps[(T0 + 1):dim(Ymat)[1], 1])/(dim(Ymat)[1] - T0)
        pre <- (t(gaps[1:T0, 1]) %*% gaps[1:T0, 1])/(T0)
        ts[1, j] <- post/pre
      }
      # Test the null hypothesis.
      rts <- matrix(NA, 1, dim(Ymat)[2])
      rts[1, ] <- rank(ts[1, ])
      prob <- exp(phi*v)/sum(exp(phi*v))
      indicator <- matrix(as.numeric(rts >= rts[1, treated]),dim(Ymat)[2],1)
      pvalue <- prob %*% indicator
      reject.u <- as.numeric(pvalue <= significance)
      if (reject.u == 0) {
        attemptu1 <- 0
        if (type == "uniform") {
          ub <- ub + step
        } else {
          ub <- param * (ub/param + s*step)
        }
      } else if (reject.u == 1) {
        if (attemptu1 == 1) {
          stop("\n The confidence set is empty for the requested class of functions. \n")
        }
        if (type == "uniform") {
          ub <- ub - step
        } else {
          ub <- param * (ub/param - s*step)
        }
      }
      if (type == "uniform") {
        if (abs(ub) > 1000000) {
          stop("\n Upper bound was not found. \n")
        }
      } else {
        if (abs(ub) > 100 * abs(param)) {
          print(paste(power, ub, param))
          stop("\n Upper bound was not found. \n")
        }
      }
    }
    # We start our loop whose interations try to find the lower bound
    # of our confidence set.
    while (reject.l == 0) {
      # Create the vector that represents the null hypothesis.
      if (type == "constant") {
        nh <- matrix(
          c(rep(0, T0), rep(lb, (dim(Ymat)[1] - T0))), dim(Ymat)[1], 1)
      } else if (type == "linear") {
        nh <- matrix(
          c(rep(0, T0),
            lb * seq(from = 1, to = (dim(Ymat)[1] - T0), by = 1)),
          dim(Ymat)[1], 1)
      } else if (type == "uniform") {
        nh <- nh0 + lb*postp
      }
      # Create a matrix to store the test statistics.
      ts <- matrix(NA, 1, dim(Ymat)[2])
      # Estimate the gaps for each placebo unit.
      for (j in 1:dim(Ymat)[2]) {
        # Create the matrices of observed outcomes under the null.
        if (j == treated) {
          Y1 <- Ymat[, treated]
          Y0 <- Ymat[, setdiff(1:dim(Ymat)[2], treated)]
        } else {
          Y1 <- Ymat[, j] + nh
          Y0 <- Ymat[, setdiff(1:dim(Ymat)[2], j)]
          if (j < treated) {
            Y0[, (treated - 1)] <- Y0[, (treated - 1)] - nh
          } else if (j > treated) {
            Y0[, treated] <- Y0[, treated] - nh
          }
        }
        # Estimate the gaps.
        gaps <- Y1 - Y0 %*% weightsmat[, j] - nh
        
        # Estimate the test statistics.
        post <- (t(gaps[(T0 + 1):dim(Ymat)[1], 1]) %*%
                   gaps[(T0 + 1):dim(Ymat)[1], 1])/(dim(Ymat)[1] - T0)
        pre <- (t(gaps[1:T0, 1]) %*% gaps[1:T0, 1])/(T0)
        ts[1, j] <- post/pre
      }
      # Test the null hypothesis.
      rts <- matrix(NA, 1, dim(Ymat)[2])
      rts[1, ] <- rank(ts[1, ])
      prob <- exp(phi*v)/sum(exp(phi*v))
      indicator <- matrix(as.numeric(rts >= rts[1, treated]),dim(Ymat)[2],1)
      pvalue <- prob %*% indicator
      reject.l <- as.numeric(pvalue <= significance)
      if (reject.l == 0) {
        attemptl1 <- 0
        if (type == "uniform") {
          lb <- lb - step
        } else {
          lb <- param * (lb/param - s*step)
        }
      } else if (reject.l == 1) {
        if (attemptl1 == 1) {
          stop("\n The confidence set is empty for the requested class of functions. \n")
        }
        if (type == "uniform") {
          lb <- lb + step
        } else {
          lb <- param * (lb/param + s*step)
        }
      }
      if (type == "uniform") {
        if (abs(lb) > 1000000) {
          stop("\n Lower bound was not found. \n")
        }
      } else {
        if (abs(ub) > 100 * abs(param)) {
          stop("\n Lower bound was not found. \n")
        }
      }
    }
  }
  # Draw the confidence interval graph.
  # Define the initial the intervention effect.
  Y1 <- Ymat[, treated]
  Y0 <- Ymat[, setdiff(1:dim(Ymat)[2], treated)]
  gaps <- Y1 - Y0 %*% weightsmat[, treated]
  # Define the upper bound of the confidence subset.
  if (type == "constant") {
    upper <- matrix(
      c(rep(NA, T0), rep(ub, (dim(Ymat)[1] - T0))), dim(Ymat)[1], 1)
  } else if (type == "linear") {
    upper <- matrix(
      c(rep(NA, T0),
        ub * seq(from = 1, to = (dim(Ymat)[1] - T0), by = 1)),
      dim(Ymat)[1], 1)
  } else if (type == "uniform") {
    upper <- nh0 + ub*postp
    upper <- matrix(
      c(rep(NA, T0), upper[(T0+1):dim(Ymat)[1], 1]),
      dim(Ymat)[1], 1
    )
  }
  # Define the lower bound of the confidence subset.
  if (type == "constant") {
    lower <- matrix(
      c(rep(NA, T0), rep(lb, (dim(Ymat)[1] - T0))), dim(Ymat)[1], 1)
  } else if (type == "linear") {
    lower <- matrix(
      c(rep(NA, T0),
        lb * seq(from = 1, to = (dim(Ymat)[1] - T0), by = 1)),
      dim(Ymat)[1], 1)
  } else if (type == "uniform") {
    lower <- nh0 + lb*postp
    lower <- matrix(
      c(rep(NA, T0), lower[(T0+1):dim(Ymat)[1], 1]),
      dim(Ymat)[1], 1
    )
  }
  # Plot all the information.
  if (plot == TRUE) {
    par(mar = c(3, 5, 1, 1))
    par(oma = c(0.1, 0.1, 0.1, 0.1))
    plot(as.matrix(1:dim(Ymat)[1], dim(Ymat)[1], 1), gaps,
         t = "n", col = "black", lwd = 2, cex = 4/5,
         ylab = "Gaps",
         xlab = NA, xaxs = "i", yaxs = "i", ylim = c(-50, 4))
    polygon(x = c(as.matrix(1:dim(Ymat)[1], dim(Ymat)[1], 1),
                  rev(as.matrix(1:dim(Ymat)[1], dim(Ymat)[1], 1))),
            y = c(upper, rev(lower)), col = 'grey90', border = NA)
    lines(as.matrix(1:dim(Ymat)[1], dim(Ymat)[1], 1), gaps,
          col = "black", lwd = 2, cex = 4/5)
    abline(v = T0 + 1, lty = "dotted")
    abline(h = 0, lty = "dotted")
  }
  # The function returns the upper and lower bounds of confidence subset.
  bounds <- list(u = upper, l = lower)
  return(bounds)
}
