if(!require(WDI)){
  install.packages("WDI")
  library(WDI)
}

if(!require(pwt10)){
  install.packages("pwt10")
  library(pwt10)
}

if(!require(Synth)){
  install.packages("Synth")
  library(Synth)
}

if(!require(pwt)){
  install.packages("pwt")
  library(pwt)
}

if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}

if(!require(tibble)){
  install.packages("tibble")
  library(tibble)
}

if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}

if(!require(reshape2)){
  install.packages("reshape2")
  library(reshape2)
}

if(!require(gridExtra)){
  install.packages("gridExtra")
  library(gridExtra)
}

if(!require(LowRankQP)){
  install.packages("LowRankQP")
  library(LowRankQP)
}

if(!require(doParallel)){
  install.packages("doParallel")
  library(doParallel)
}

if(!require(gtable)){
  install.packages("gtable")
  library(gtable)
}

if(!require(grid)){
  install.packages("grid")
  library(grid)
}

if(!require(rlang)){
  install.packages("rlang")
  library(rlang)
}

source("calculating.R")
source("plotting.R")

## Import data ####

# # alternatively
# load("world.RData")

data("pwt10.0")

data("pwt7.1")


AGR <- WDI(
  country = "all",
  indicator = "NV.AGR.TOTL.ZS",
  start = 1960,
  end = 2020,
  extra = TRUE,
  cache = NULL,
  latest = NULL,
  language = "en"
)
AGRnoAGG <- AGR[which(AGR$region != "Aggregates"), ][c("year", "iso3c", "NV.AGR.TOTL.ZS")]
AGRnoAGG <- AGRnoAGG[
  order( AGRnoAGG[,2], AGRnoAGG[,1] ),
]

IND <- WDI(
  country = "all",
  indicator = "NV.IND.TOTL.ZS",
  start = 1960,
  end = 2020,
  extra = TRUE,
  cache = NULL,
  latest = NULL,
  language = "en"
)
INDnoAGG <- IND[which(IND$region != "Aggregates"), ][c("year", "iso3c", "NV.IND.TOTL.ZS")]
INDnoAGG <- INDnoAGG[
  order( INDnoAGG[,2], INDnoAGG[,1] ),
]

SEC <- WDI(
  country = "all",
  indicator = "SE.SEC.ENRR",
  start = 1960,
  end = 2020,
  extra = TRUE,
  cache = NULL,
  latest = NULL,
  language = "en"
)
SECnoAGG <- SEC[which(SEC$region != "Aggregates"), ][c("year", "iso3c", "SE.SEC.ENRR")]
SECnoAGG <- SECnoAGG[
  order( SECnoAGG[,2], SECnoAGG[,1] ),
]

TER <- WDI(
  country = "all",
  indicator = "SE.TER.ENRR",
  start = 1960,
  end = 2020,
  extra = TRUE,
  cache = NULL,
  latest = NULL,
  language = "en"
)
TERnoAGG <- TER[which(TER$region != "Aggregates"), ][c("year", "iso3c", "SE.TER.ENRR")]
TERnoAGG <- TERnoAGG[
  order( TERnoAGG[,2], TERnoAGG[,1] ),
]

POPGR <- WDI(
  country = "all",
  indicator = "SP.POP.GROW",
  start = 1960,
  end = 2020,
  extra = TRUE,
  cache = NULL,
  latest = NULL,
  language = "en"
)
POPGRnoAGG <- POPGR[which(POPGR$region != "Aggregates"), ][c("year", "iso3c", "SP.POP.GROW")]
POPGRnoAGG <- POPGRnoAGG[
  order( POPGRnoAGG[,2], POPGRnoAGG[,1] ),
]


#~
RGDPCH <- data.frame(pwt10.0$year, pwt10.0$isocode, pwt10.0$rgdpe/pwt10.0$pop)
colnames(RGDPCH) <- c("year", "iso3c", "rgdpch")

#~
RGDPWOK <- data.frame(pwt10.0$year, pwt10.0$isocode, pwt10.0$rgdpe/pwt10.0$emp)
colnames(RGDPWOK) <- c("year", "iso3c", "rgdpwok")

#
KI <- data.frame(pwt7.1$year, pwt7.1$isocode, pwt7.1$ki, stringsAsFactors = FALSE)

#remove/ replace CH2 ZAR GER ROM TWN
colnames(KI) <- c("year", "iso3c", "ki")
KI$iso3c <- as.character(KI$iso3c)
KI <- KI[which(! KI$iso3c %in% c("CH2", "TWN")),]
KI["iso3c"][KI["iso3c"] == "GER"] <- "DEU"
KI["iso3c"][KI["iso3c"] == "ROM"] <- "ROU"
KI["iso3c"][KI["iso3c"] == "ZAR"] <- "COD"


temp <- NULL
temp <- merge(x = AGRnoAGG, y = INDnoAGG, by = c("iso3c", "year"), all = TRUE)
temp <- merge(x = temp, y = SECnoAGG, by = c("iso3c", "year"), all = TRUE)
temp <- merge(x = temp, y = TERnoAGG, by = c("iso3c", "year"), all = TRUE)
temp <- merge(x = temp, y = RGDPWOK, by = c("iso3c", "year"), all = TRUE)
temp <- merge(x = temp, y = KI, by = c("iso3c", "year"), all = TRUE)
temp <- merge(x = temp, y = POPGRnoAGG, by = c("iso3c", "year"), all = TRUE)
temp <- merge(x = temp, y = RGDPCH, by = c("iso3c", "year"), all = TRUE)
world <- temp
temp <- NULL
world$regionno <- match(world$iso3c, unique(world$iso3c))
world <- world[, c(11,  1:10)]
colnames(world) <-c("regionno", "regionname", "year", "agr", "ind", "sec", "ter", "rgdpwok", "ki", "popgr", "rgdpch")

# define the donor pool by ISO3 codes
sample0 <- c("CHL",	"ARG",	"BLR", "KAZ", "RUS",	"URY",	"PAN",	"BHS",	"EGY", "GEO", "CRI", "MYS", "KOR", "MEX",	"MUS",	"TTO",	"JPN", "PER", "PRY")

#set dpi for plot export
dpi.setting <- 450

## BULGARIA ####
# set the specific countryCode in world$regionno
countryCodeBGR <- 20
# set the starting year for the analysis
startYear <- 1990
# set the last year for the analysis
endYear <- 2019

### Bulgaria 2007 ####
# set the intervention year between startYear and endYear
eventYear <- 2007
# get the subset of data from world, limited by startYear, endYear for the countries in the donor pool and the target country
temp <- world[which(world$year >= startYear & world$year <= endYear & (world$regionname %in% sample0 | world$regionname == "BGR")), ]

## BGR GDP per capita 2007
# filter countries in temp to only include ones which fulfill the (availability) requirements of the dataprep function
cID <- filterCountries(temp, startYear, eventYear, countryCodeBGR)

# estimate the synthetic control counterfactual for the target country, repeat for each country in the donor pool for inference
synthAllBGR <- synthAll(temp, countryCodeBGR, cID, startYear = startYear, eventYear = eventYear, endYear = endYear, useRgdpch = T)

# get the synth tables
synthtablesBGR <- synth.tab(
  dataprep.res = synthAllBGR$dataprepoutY1,
  synth.res = synthAllBGR$synthoutY1
)

# # copy averaged actual, synthetic and sample mean predictor values to clipboard
# write.excel(synthtablesBGR$tab.pred)

# print synth tables
print(synthtablesBGR)

# plot path
bgr07path <- plotPaths(synthAllBGR$dataprepoutY1, synthAllBGR$synthoutY1, countryCodeBGR, useRgdpch = T, ylim = c(5000, 30000)) +
  labs(y ="GDP per capita") +
  guides(linetype=guide_legend(keywidth = 3, keyheight = 1)) +
  scale_linetype_manual(values = c("f4", "solid"),
                        labels=c("Synthetic Bulgaria", "Bulgaria"))
# save path plot
ggsave(plot = bgr07path, "bgr07path.pdf", path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

# plot pie chart
bgr07pie <- plotPie(synthtablesBGR)
# save pie chart
ggsave(plot = bgr07pie, "bgr07pie.pdf", path = "images/", width = 2500, height = 2500, units = "px", dpi = 332, bg = "white")

# load RDS
load("bgr07allParallel.RData")
# # alternatively estimate with random donor samples
# bgr07allParallel <- randomizeSampleParallel(countryCodeBGR, "BGR", length(cID), world, startYear = startYear, eventYear = eventYear, endYear = endYear, 500, addToSeed = 470)

# format the input data to include the main estimate first
randomizedSampleEstimatesInput <- data.frame(cbind(synthAllBGR$dataprepoutY1$Y0plot%*%synthAllBGR$synthoutY1$solution.w, bgr07allParallel$RDSestimates))
# format the losses (pre-intervention MSE) to include the main estimate first
bgr07RDSlosses <- data.frame(cbind(synthAllBGR$synthoutY1$loss.v, bgr07allParallel[["losses"]]))
# plot the random donor samples, highlighting the main estimate and the best fit estimate, Box plot of the point in each estimation included
bgr07rds <- plotRDSParallel(countryCode = countryCodeBGR, countryISO3 = "BGR", randomizedSampleEstimates = randomizedSampleEstimatesInput, losses = bgr07RDSlosses)
bgr07rdsplot <- grid.arrange(bgr07rds[[1]], bgr07rds[[2]], widths = c(7,1), top=textGrob("Bulgaria", gp=gpar(fontsize=30)))
# save the RDS plot
ggsave(plot = bgr07rdsplot, "bgr07rds.pdf", path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")


# calculate the summary statistics for the RDS estimates in the last year analyzed
bulgaria2019 <- summaryRDS(countryCodeBGR, bgr07allParallel, startYear = 1990, eventYear = 2007, lastSummaryYear = 2019)
# # copy the summary statistics table
# write.excel(bulgaria2019)

# calculate the summary statistics for the RDS estimates in 2013
bulgaria2013 <- summaryRDS(countryCodeBGR, bgr07allParallel, startYear = 1990, eventYear = 2007, lastSummaryYear = 2013)
# # copy the summary statistics table
# write.excel(bulgaria2013)

# Define the options of the function SCM.CS.
treated <- length(union(cID, countryCodeBGR))
T0 <- eventYear - startYear
precision <- 30
type <- "uniform"
phi <- 0
v <- matrix(0, 1, length(union(cID, countryCodeBGR)))
vsignificance <- 1/length(union(cID, countryCodeBGR))
plot <- FALSE

# calculate bounds and alternative gaps
boundsBGR07 <- getBounds(synthAllBGR, countryCodeBGR, treated, T0, phi, v, precision, type, vsignificance)

# plot the estimates for each country in the donor pool and the main estimate
bgr07all <- plotDonorSampleSynth(boundsBGR07$allGapEstimates, countryCode = countryCodeBGR, useRgdpch = T)
# save the plot
ggsave(plot = bgr07all, "bgr07all.pdf", path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

# calculate RMSPE ratios, plot and save them
rmspesBGR07 <- getRatiosOfRMSPE(synthAllBGR, cID, "BGR", startYear, eventYear, endYear)
bgr07rmspe <- plotRatiosOfRMSPE(rmspesBGR07, "BGR")
ggsave(plot = bgr07rmspe, "bgr07rmspe.pdf", path = "images/", width = 2160, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

# plot inference plots together
bgr07alter <- grid.arrange(arrangeGrob(bgr07all,
                                       bgr07rmspe,
                                       widths = c(16,9),
                                       nrow=1))

ggsave(plot = bgr07alter, "bgr07alter.pdf", path = "images/", width = (3840+2160), height = 2160, units = "px", dpi = dpi.setting, bg = "white")


# plot gaps
bgr07gapCS <- plotGaps(synthAllBGR$dataprepoutY1, synthAllBGR$synthoutY1, countryCodeBGR, vsignificance, boundsdf = boundsBGR07$boundsVsignificance, bounds2df = boundsBGR07$bounds2Vsignificance, ylab = "Gap in GDP per capita", T)
# save the gaps plot
ggsave(plot = bgr07gapCS, paste0("bgr07gapCS", vsignificance, ".pdf"), path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")



## BGR GDP per worker 2007
# filter countries in temp to only include ones which fulfill the (availability) requirements of the dataprep function
cID <- filterCountriesWOK(temp, startYear, eventYear, countryCodeBGR)

# estimate the synthetic control counterfactual for the target country, repeat for each country in the donor pool for inference
synthAllBGRwok <- synthAll(temp, countryCodeBGR, cID, startYear = startYear, eventYear = eventYear, endYear = endYear, F)

# get the synth tables
synthtablesBGRw <- synth.tab(
  dataprep.res = synthAllBGRwok$dataprepoutY1,
  synth.res = synthAllBGRwok$synthoutY1
)

# # copy averaged actual, synthetic and sample mean predictor values to clipboard
# write.excel(synthtablesBGRw$tab.pred)

# print synth tables
print(synthtablesBGRw)

# plot path
bgrwok07path <- plotPaths(synthAllBGRwok$dataprepoutY1, synthAllBGRwok$synthoutY1, countryCodeBGR, F, ylim = c(15000, 65000)) +
  labs(y ="GDP per worker") +
  guides(linetype=guide_legend(keywidth = 3, keyheight = 1)) +
  scale_linetype_manual(values = c("f4", "solid"),
                        labels=c("Synthetic Bulgaria", "Bulgaria"))
# save path plot
ggsave(plot = bgrwok07path, "bgrwok07path.pdf", path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

# plot pie chart
bgrwok07pie <- plotPie(synthtablesBGRw)
# save pie chart
ggsave(plot = bgrwok07pie, "bgrwok07pie.pdf", path = "images/", width = 2500, height = 2500, units = "px", dpi = 332, bg = "white")

# load RDS
load("bgr07wokallParallel.RData")
# # alternatively estimate with random donor samples
# bgr07wokallParallel <- randomizeSampleWOKParallel(countryCodeBGR, "BGR", length(cID), world, startYear = startYear, eventYear = eventYear, endYear = endYear, 500, addToSeed = 200166)
# format the input data to include the main estimate first
randomizedSampleEstimatesInput2 <- data.frame(cbind(synthAllBGRwok$dataprepoutY1$Y0plot%*%synthAllBGRwok$synthoutY1$solution.w, bgr07wokallParallel$RDSestimates))
# format the losses (pre-intervention MSE) to include the main estimate first
bgr07wokRDSlosses <- data.frame(cbind(synthAllBGRwok$synthoutY1$loss.v, bgr07wokallParallel[["losses"]]))
# plot the random donor samples, highlighting the main estimate and the best fit estimate, Box plot of the point in each estimation included
bgr07wokrds <- plotRDSParallel(countryCode = countryCodeBGR, countryISO3 = "BGR", randomizedSampleEstimates = randomizedSampleEstimatesInput2, losses = bgr07wokRDSlosses, useRgdpch = F)
bgr07wokrdsplot <- grid.arrange(bgr07wokrds[[1]], bgr07wokrds[[2]], widths = c(7,1), top=textGrob("Bulgaria", gp=gpar(fontsize=30)))
# save the RDS plot
ggsave(plot = bgr07wokrdsplot, "bgr07wokrds.pdf", path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

# calculate the summary statistics for the RDS estimates in the last year analyzed
bulgaria2019w <- summaryRDS(countryCodeBGR, bgr07wokallParallel, startYear = 1990, eventYear = 2007, lastSummaryYear = 2019, useRgdpch = F)
# # copy the summary statistics table
# write.excel(bulgaria2019w)

# calculate the summary statistics for the RDS estimates in 2013
bulgaria2013w <- summaryRDS(countryCodeBGR, bgr07wokallParallel, startYear = 1990, eventYear = 2007, lastSummaryYear = 2013, useRgdpch = F)
# # copy the summary statistics table
# write.excel(bulgaria2013w)

# Define the options of the function SCM.CS.
treated <- length(union(cID, countryCodeBGR))
T0 <- eventYear - startYear
precision <- 30
type <- "uniform"
phi <- 0
v <- matrix(0, 1, length(union(cID, countryCodeBGR)))
vsignificance <- 1/length(union(cID, countryCodeBGR))
plot <- FALSE

# calculate bounds and alternative gaps
boundsBGR07wok <- getBounds(synthAllBGRwok, countryCodeBGR, treated, T0, phi, v, precision, type, vsignificance)


# plot the estimates for each country in the donor pool and the main estimate
bgr07wokall <- plotDonorSampleSynth(boundsBGR07wok$allGapEstimates, countryCode = countryCodeBGR, useRgdpch = F)
# save the plot
ggsave(plot = bgr07wokall, "bgr07wokall.pdf", path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

# calculate RMSPE ratios, plot and save them
rmspesBGR07wok <- getRatiosOfRMSPE(synthAllBGRwok, cID, "BGR", startYear, eventYear, endYear)
bgr07wokrmspe <- plotRatiosOfRMSPE(rmspesBGR07wok, "BGR")
ggsave(plot = bgr07wokrmspe, "bgr07wokrmspe.pdf", path = "images/", width = 2160, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

# plot inference plots together
bgr07wokalter <- grid.arrange(arrangeGrob(bgr07wokall,
                                       bgr07wokrmspe,
                                       widths = c(16,9),
                                       nrow=1))

ggsave(plot = bgr07wokalter, "bgr07wokalter.pdf", path = "images/", width = (3840+2160), height = 2160, units = "px", dpi = dpi.setting, bg = "white")


# plot gaps
bgr07wokgapCS <- plotGaps(synthAllBGRwok$dataprepoutY1, synthAllBGRwok$synthoutY1, countryCodeBGR, vsignificance, boundsdf = boundsBGR07wok$boundsVsignificance, bounds2df = boundsBGR07wok$bounds2Vsignificance, ylab = "Gap in GDP per worker", F)
# save the gaps plot
ggsave(plot = bgr07wokgapCS, paste0("bgr07wokgapCS", vsignificance, ".pdf"), path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")


### Bulgaria 2001 ####
# set the intervention year between startYear and endYear
eventYear <- 2001
# get the subset of data from world, limited by startYear, endYear for the countries in the donor pool and the target country
temp <- world[which(world$year >= startYear & world$year <= endYear & (world$regionname %in% sample0 | world$regionname == "BGR")), ]

## BGR GDP per capita 2001
# filter countries in temp to only include ones which fulfill the (availability) requirements of the dataprep function
cID <- filterCountries(temp, startYear, eventYear, countryCodeBGR)

# estimate the synthetic control counterfactual for the target country, repeat for each country in the donor pool for inference
synthAllBGR01 <- synthAll(temp, countryCodeBGR, cID, startYear = startYear, eventYear = eventYear, endYear = endYear, useRgdpch = T)

# get the synth tables
synthtablesBGR01 <- synth.tab(
  dataprep.res = synthAllBGR01$dataprepoutY1,
  synth.res = synthAllBGR01$synthoutY1
)

# # copy averaged actual, synthetic and sample mean predictor values to clipboard
# write.excel(synthtablesBGR01$tab.pred)

# print synth tables
print(synthtablesBGR01)

# plot path
bgr01path <- plotPaths(synthAllBGR01$dataprepoutY1, synthAllBGR01$synthoutY1, countryCodeBGR, useRgdpch = T, ylim = c(5000, 30000)) +
  labs(y ="GDP per capita") +
  guides(linetype=guide_legend(keywidth = 3, keyheight = 1)) +
  scale_linetype_manual(values = c("f4", "solid"),
                        labels=c("Synthetic Bulgaria", "Bulgaria"))
# save path plot
ggsave(plot = bgr01path, "bgr01path.pdf", path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

# plot pie chart
bgr01pie <- plotPie(synthtablesBGR01)
# save pie chart
ggsave(plot = bgr01pie, "bgr01pie.pdf", path = "images/", width = 2500, height = 2500, units = "px", dpi = 332, bg = "white")

# load RDS
load("bgr01allParallel.RData")
# # alternatively estimate with random donor samples
# bgr01allParallel <- randomizeSampleParallel(countryCodeBGR, "BGR", length(cID), world, startYear = startYear, eventYear = eventYear, endYear = endYear, 500, addToSeed = 170734)
# format the input data to include the main estimate first
randomizedSampleEstimatesInput3 <- data.frame(cbind(synthAllBGR01$dataprepoutY1$Y0plot%*%synthAllBGR01$synthoutY1$solution.w, bgr01allParallel$RDSestimates))
# format the losses (pre-intervention MSE) to include the main estimate first
bgr01RDSlosses <- data.frame(cbind(synthAllBGR01$synthoutY1$loss.v, bgr01allParallel[["losses"]]))
# plot the random donor samples, highlighting the main estimate and the best fit estimate, Box plot of the point in each estimation included
bgr01rds <- plotRDSParallel(countryCode = countryCodeBGR, countryISO3 = "BGR", randomizedSampleEstimates = randomizedSampleEstimatesInput3, losses = bgr01RDSlosses)
bgr01rdsplot <- grid.arrange(bgr01rds[[1]], bgr01rds[[2]], widths = c(7,1), top=textGrob("Bulgaria", gp=gpar(fontsize=30)))

# save the RDS plot
ggsave(plot = bgr01rdsplot, "bgr01rds.pdf", path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

# calculate the summary statistics for the RDS estimates in the last year analyzed
bulgaria2019b <- summaryRDS(countryCodeBGR, bgr01allParallel, startYear = 1990, eventYear = 2001, lastSummaryYear = 2019, useRgdpch = T)
# # copy the summary statistics table
# write.excel(bulgaria2019b)

# calculate the summary statistics for the RDS estimates six years after the intervention
bulgaria2013b <- summaryRDS(countryCodeBGR, bgr01allParallel, startYear = 1990, eventYear = 2001, lastSummaryYear = 2013, useRgdpch = T)
# # copy the summary statistics table
# write.excel(bulgaria2013b)

# Define the options of the function SCM.CS.
treated <- length(union(cID, countryCodeBGR))
T0 <- eventYear - startYear
precision <- 30
type <- "uniform"
phi <- 0
v <- matrix(0, 1, length(union(cID, countryCodeBGR)))
vsignificance <- 1/length(union(cID, countryCodeBGR))
plot <- FALSE

# calculate bounds and alternative gaps
boundsBGR01 <- getBounds(synthAllBGR01, countryCodeBGR, treated, T0, phi, v, precision, type, vsignificance)

# plot the estimates for each country in the donor pool and the main estimate
bgr01all <- plotDonorSampleSynth(boundsBGR01$allGapEstimates, countryCode = countryCodeBGR, useRgdpch = T)
# save the plot
ggsave(plot = bgr01all, "bgr01all.pdf", path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

# calculate RMSPE ratios, plot and save them
rmspesBGR01 <- getRatiosOfRMSPE(synthAllBGR01, cID, "BGR", startYear, eventYear, endYear)
bgr01rmspe <- plotRatiosOfRMSPE(rmspesBGR01, "BGR")
ggsave(plot = bgr01rmspe, "bgr01rmspe.pdf", path = "images/", width = 2160, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

# plot inference plots together
bgr01alter <- grid.arrange(arrangeGrob(bgr01all,
                                       bgr01rmspe,
                                       widths = c(16,9),
                                       nrow=1))

ggsave(plot = bgr01alter, "bgr01alter.pdf", path = "images/", width = (3840+2160), height = 2160, units = "px", dpi = dpi.setting, bg = "white")


# plot gaps
bgr01gapCS <- plotGaps(synthAllBGR01$dataprepoutY1, synthAllBGR01$synthoutY1, countryCodeBGR, vsignificance, boundsdf = boundsBGR01$boundsVsignificance, bounds2df = boundsBGR01$bounds2Vsignificance, ylab = "Gap in GDP per capita", T)
# save the gaps plot
ggsave(plot = bgr01gapCS, paste0("bgr01gapCS", vsignificance, ".pdf"), path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")


##BGR GDP per worker 2001
# filter countries in temp to only include ones which fulfill the (availability) requirements of the dataprep function
cID <- filterCountriesWOK(temp, startYear, eventYear, countryCodeBGR)

# estimate the synthetic control counterfactual for the target country, repeat for each country in the donor pool for inference
synthAllBGR01wok <- synthAll(temp, countryCodeBGR, cID, startYear = startYear, eventYear = eventYear, endYear = endYear, F)

# get the synth tables
synthtablesBGRw01 <- synth.tab(
  dataprep.res = synthAllBGR01wok$dataprepoutY1,
  synth.res = synthAllBGR01wok$synthoutY1
)

# # copy averaged actual, synthetic and sample mean predictor values to clipboard
# write.excel(synthtablesBGRw01$tab.pred)

# print synth tables
print(synthtablesBGRw01)

# plot path
bgrwok01path <- plotPaths(synthAllBGR01wok$dataprepoutY1, synthAllBGR01wok$synthoutY1, countryCodeBGR, F, ylim = c(15000, 65000)) +
  labs(y ="GDP per worker") +
  guides(linetype=guide_legend(keywidth = 3, keyheight = 1)) +
  scale_linetype_manual(values = c("f4", "solid"),
                        labels=c("Synthetic Bulgaria", "Bulgaria"))
# save path plot
ggsave(plot = bgrwok01path, "bgrwok01path.pdf", path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

# plot pie chart
bgrwok01pie <- plotPie(synthtablesBGRw01)
# save pie chart
ggsave(plot = bgrwok01pie, "bgrwok01pie.pdf", path = "images/", width = 2500, height = 2500, units = "px", dpi = 332, bg = "white")


# load RDS
load("bgr01wokallParallel.RData")
# # alternatively estimate with random donor samples
# bgr01wokallParallel <- randomizeSampleWOKParallel(countryCodeBGR, "BGR", length(cID), world, startYear = startYear, eventYear = eventYear, endYear = endYear, 500, addToSeed = 111299)
# format the input data to include the main estimate first
randomizedSampleEstimatesInput4 <- data.frame(cbind(synthAllBGR01wok$dataprepoutY1$Y0plot%*%synthAllBGR01wok$synthoutY1$solution.w, bgr01wokallParallel$RDSestimates))
# format the losses (pre-intervention MSE) to include the main estimate first
bgr01wokRDSlosses <- data.frame(cbind(synthAllBGR01wok$synthoutY1$loss.v, bgr01wokallParallel[["losses"]]))
# plot the random donor samples, highlighting the main estimate and the best fit estimate, Box plot of the point in each estimation included
bgr01wokrds <- plotRDSParallel(countryCode = countryCodeBGR, countryISO3 = "BGR", randomizedSampleEstimates = randomizedSampleEstimatesInput4, losses = bgr01wokRDSlosses, useRgdpch = F)
bgr01wokrdsplot <- grid.arrange(bgr01wokrds[[1]], bgr01wokrds[[2]], widths = c(7,1), top=textGrob("Bulgaria", gp=gpar(fontsize=30)))
# save the RDS plot
ggsave(plot = bgr01wokrdsplot, "bgr01wokrds.pdf", path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")


# calculate the summary statistics for the RDS estimates in the last year analyzed
bulgaria2019bw <- summaryRDS(countryCodeBGR, bgr01wokallParallel, startYear = 1990, eventYear = 2001, lastSummaryYear = 2019, useRgdpch = F)
# # copy the summary statistics table
# write.excel(bulgaria2019bw)

# calculate the summary statistics for the RDS estimates in the last year analyzed
bulgaria2013bw <- summaryRDS(countryCodeBGR, bgr01wokallParallel, startYear = 1990, eventYear = 2001, lastSummaryYear = 2013, useRgdpch = F)
# # copy the summary statistics table
# write.excel(bulgaria2013bw)

# Define the options of the function SCM.CS.
treated <- length(union(cID, countryCodeBGR))
T0 <- eventYear - startYear
precision <- 30
type <- "uniform"
phi <- 0
v <- matrix(0, 1, length(union(cID, countryCodeBGR)))
vsignificance <- 1/length(union(cID, countryCodeBGR))
plot <- FALSE

# calculate bounds and alternative gaps
boundsBGR01WOK <- getBounds(synthAllBGR01wok, countryCodeBGR, treated, T0, phi, v, precision, type, vsignificance)

# plot the estimates for each country in the donor pool and the main estimate
bgr01wokall <- plotDonorSampleSynth(boundsBGR01WOK$allGapEstimates, countryCode = countryCodeBGR, useRgdpch = F)
# save the plot
ggsave(plot = bgr01wokall, "bgr01wokall.pdf", path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

# calculate RMSPE ratios, plot and save them
rmspesBGR01wok <- getRatiosOfRMSPE(synthAllBGR01wok, cID, "BGR", startYear, eventYear, endYear)
bgr01wokrmspe <- plotRatiosOfRMSPE(rmspesBGR01wok, "BGR")
ggsave(plot = bgr01wokrmspe, "bgr01wokrmspe.pdf", path = "images/", width = 2160, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

# plot inference plots together
bgr01wokalter <- grid.arrange(arrangeGrob(bgr01wokall,
                                          bgr01wokrmspe,
                                          widths = c(16,9),
                                          nrow=1))

ggsave(plot = bgr01wokalter, "bgr01wokalter.pdf", path = "images/", width = (3840+2160), height = 2160, units = "px", dpi = dpi.setting, bg = "white")


# plot gaps
bgr01wokgapCS <- plotGaps(synthAllBGR01wok$dataprepoutY1, synthAllBGR01wok$synthoutY1, countryCodeBGR, vsignificance, boundsdf = boundsBGR01WOK$boundsVsignificance, bounds2df = boundsBGR01WOK$bounds2Vsignificance, ylab = "Gap in GDP per worker", F)
# save the gaps plot
ggsave(plot = bgr01wokgapCS, paste0("bgr01wokgapCS", vsignificance, ".pdf"), path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")


## ROMANIA ####

# set the specific countryCode in world$regionno
countryCodeROU <- 166
# set the starting year for the analysis
startYear <- 1990
# set the last year for the analysis
endYear <- 2019

### Romania 2007 ####
# set the intervention year between startYear and endYear
eventYear <- 2007
# get the subset of data from world, limited by startYear, endYear for the countries in the donor pool and the target country
temp <- world[which(world$year >= startYear & world$year <= endYear & (world$regionname %in% sample0 | world$regionname == "ROU")), ]

## ROU GDP per capita 2007

# filter countries in temp to only include ones which fulfill the (availability) requirements of the dataprep function
cID <- filterCountries(temp, startYear = startYear, eventYear = eventYear, countryCodeROU)

# estimate the synthetic control counterfactual for the target country, repeat for each country in the donor pool for inference
synthAllROU <- synthAll(temp, countryCodeROU, cID, startYear = startYear, eventYear = eventYear, endYear = endYear, T)

# get the synth tables
synthtablesROU <- synth.tab(
  dataprep.res = synthAllROU$dataprepoutY1,
  synth.res = synthAllROU$synthoutY1
)

# # copy averaged actual, synthetic and sample mean predictor values to clipboard
# write.excel(synthtablesROU$tab.pred)

# print synth tables
print(synthtablesROU)


# plot path
rou07path <- plotPaths(synthAllROU$dataprepoutY1, synthAllROU$synthoutY1, countryCodeROU, T, ylim = c(5000, 30000)) +
  labs(y ="GDP per capita") +
  guides(linetype=guide_legend(keywidth = 3, keyheight = 1)) +
  scale_linetype_manual(values = c("f4", "solid"),
                        labels=c("Synthetic Romania", "Romania"))
# save path plot
ggsave(plot = rou07path, "rou07path.pdf", path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

# plot pie chart
rou07pie <- plotPie(synthtablesROU)
# save pie chart
ggsave(plot = rou07pie, "rou07pie.pdf", path = "images/", width = 2500, height = 2500, units = "px", dpi = 332, bg = "white")

# load RDS
load("rou07allParallel.RData")
# # alternatively estimate with random donor samples
# rou07allParallel <- randomizeSampleParallel(countryCodeROU, "ROU", length(cID), world, startYear = startYear, eventYear = eventYear, endYear = endYear, 500, addToSeed = 2100)
# format the input data to include the main estimate first
randomizedSampleEstimatesInput5 <- data.frame(cbind(synthAllROU$dataprepoutY1$Y0plot%*%synthAllROU$synthoutY1$solution.w, rou07allParallel$RDSestimates))
# format the losses (pre-intervention MSE) to include the main estimate first
rou07RDSlosses <- data.frame(cbind(synthAllROU$synthoutY1$loss.v, rou07allParallel[["losses"]]))
# plot the random donor samples, highlighting the main estimate and the best fit estimate, Box plot of the point in each estimation included
rou07rds <- plotRDSParallel(countryCode = countryCodeROU, countryISO3 = "ROU", randomizedSampleEstimates = randomizedSampleEstimatesInput5, losses = rou07RDSlosses)
rou07rdsplot <- grid.arrange(rou07rds[[1]], rou07rds[[2]], widths = c(7,1), top=textGrob("Romania", gp=gpar(fontsize=30)))
# save the RDS plot
ggsave(plot = rou07rdsplot, "rou07rds.pdf", path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

# calculate the summary statistics for the RDS estimates in the last year analyzed
romania2019 <- summaryRDS(countryCodeROU, rou07allParallel, startYear = 1990, eventYear = 2007, lastSummaryYear = 2019)
# # copy the summary statistics table
# write.excel(romania2019)

# calculate the summary statistics for the RDS estimates in 2013
romania2013 <- summaryRDS(countryCodeROU, rou07allParallel, startYear = 1990, eventYear = 2007, lastSummaryYear = 2013)
# # copy the summary statistics table
# write.excel(romania2013)

# Define the options of the function SCM.CS.
treated <- length(union(cID, countryCodeROU))
T0 <- eventYear - startYear
precision <- 30
type <- "uniform"
phi <- 0
v <- matrix(0, 1, length(union(cID, countryCodeROU)))
vsignificance <- 1/length(union(cID, countryCodeROU))
plot <- FALSE

# calculate bounds and alternative gaps
boundsROU07 <- getBounds(synthAllROU, countryCodeROU, treated, T0, phi, v, precision, type, vsignificance)

# plot the estimates for each country in the donor pool and the main estimate
rou07all <- plotDonorSampleSynth(boundsROU07$allGapEstimates, countryCode = countryCodeROU, useRgdpch = T)
# save the plot
ggsave(plot = rou07all, "rou07all.pdf", path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

# calculate RMSPE ratios, plot and save them
rmspesROU07 <- getRatiosOfRMSPE(synthAllROU, cID, "ROU", startYear, eventYear, endYear)
rou07rmspe <- plotRatiosOfRMSPE(rmspesROU07, "ROU")
ggsave(plot = rou07rmspe, "rou07rmspe.pdf", path = "images/", width = 2160, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

# plot inference plots together
rou07alter <- grid.arrange(arrangeGrob(rou07all,
                                       rou07rmspe,
                                       widths = c(16,9),
                                       nrow=1))

ggsave(plot = rou07alter, "rou07alter.pdf", path = "images/", width = (3840+2160), height = 2160, units = "px", dpi = dpi.setting, bg = "white")


# plot gaps
rou07gapCS <- plotGaps(synthAllROU$dataprepoutY1, synthAllROU$synthoutY1, countryCodeROU, vsignificance, boundsdf = boundsROU07$boundsVsignificance, bounds2df = boundsROU07$bounds2Vsignificance, ylab = "Gap in GDP per capita", T)
# plot the gaps with confidence sets
ggsave(plot = rou07gapCS, paste0("rou07gapCS", vsignificance, ".pdf"), path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")



## ROU GDP per worker 2007
# filter countries in temp to only include ones which fulfill the (availability) requirements of the dataprep function
cID <- filterCountriesWOK(temp, startYear = startYear, eventYear = eventYear, countryCodeROU)

# estimate the synthetic control counterfactual for the target country, repeat for each country in the donor pool for inference
synthAllROUwok <- synthAll(temp, countryCodeROU, cID, startYear = startYear, eventYear = eventYear, endYear = endYear, F)

# Get synth tables
synthtablesROUw <- synth.tab(
  dataprep.res = synthAllROUwok$dataprepoutY1,
  synth.res = synthAllROUwok$synthoutY1
) 

# # copy averaged actual, synthetic and sample mean predictor values to clipboard
# write.excel(synthtablesROUw$tab.pred)

# print synth tables
print(synthtablesROUw)

# plot path
rouwok07path <- plotPaths(synthAllROUwok$dataprepoutY1, synthAllROUwok$synthoutY1, countryCodeROU, F, ylim = c(15000, 65000)) +
  labs(y ="GDP per worker") +
  guides(linetype=guide_legend(keywidth = 3, keyheight = 1)) +
  scale_linetype_manual(values = c("f4", "solid"),
                        labels=c("Synthetic Romania", "Romania"))
# save path plot
ggsave(plot = rouwok07path, "rouwok07path.pdf", path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")



# plot pie chart
rouwok07pie <- plotPie(synthtablesROUw)
# save pie chart
ggsave(plot = rouwok07pie, "rouwok07pie.pdf", path = "images/", width = 2500, height = 2500, units = "px", dpi = 332, bg = "white")

# load RDS
load("rou07wokallParallel.RData")
# # alternatively estimate with random donor samples
# rou07wokallParallel <- randomizeSampleWOKParallel(countryCodeROU, "ROU", length(cID), world, startYear = startYear, eventYear = eventYear, endYear = endYear, 500, addToSeed = 260102)
# format the input data to include the main estimate first
randomizedSampleEstimatesInput6 <- data.frame(cbind(synthAllROUwok$dataprepoutY1$Y0plot%*%synthAllROUwok$synthoutY1$solution.w, rou07wokallParallel$RDSestimates))
# format the losses (pre-intervention MSE) to include the main estimate first
rou07wokRDSlosses <- data.frame(cbind(synthAllROUwok$synthoutY1$loss.v, rou07wokallParallel[["losses"]]))
# plot the random donor samples, highlighting the main estimate and the best fit estimate, Box plot of the point in each estimation included
rou07wokrds <- plotRDSParallel(countryCode = countryCodeROU, countryISO3 = "ROU", randomizedSampleEstimates = randomizedSampleEstimatesInput6, losses = rou07wokRDSlosses, useRgdpch = F)
rou07wokrdsplot <- grid.arrange(rou07wokrds[[1]], rou07wokrds[[2]], widths = c(7,1), top=textGrob("Romania", gp=gpar(fontsize=30)))
# save the RDS plot
ggsave(plot = rou07wokrdsplot, "rou07wokrds.pdf", path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")


# calculate the summary statistics for the RDS estimates in the last year analyzed
romania2019w <- summaryRDS(countryCodeROU, rou07wokallParallel, startYear = 1990, eventYear = 2007, lastSummaryYear = 2019, useRgdpch = F)
# # copy the summary statistics table
# write.excel(romania2019w)

# calculate the summary statistics for the RDS estimates in 2013
romania2013w <- summaryRDS(countryCodeROU, rou07wokallParallel, startYear = 1990, eventYear = 2007, lastSummaryYear = 2013, useRgdpch = F)
# # copy the summary statistics table
# write.excel(romania2013w)

# Define the options of the function SCM.CS.
treated <- length(union(cID, countryCodeROU))
T0 <- eventYear - startYear
precision <- 30
type <- "uniform"
phi <- 0
v <- matrix(0, 1, length(union(cID, countryCodeROU)))
vsignificance <- 1/length(union(cID, countryCodeROU))
plot <- FALSE

# calculate bounds and alternative gaps
boundsROU07WOK <- getBounds(synthAllROUwok, countryCodeROU, treated, T0, phi, v, precision, type, vsignificance)

# plot the estimates for each country in the donor pool and the main estimate
rou07wokall <- plotDonorSampleSynth(boundsROU07WOK$allGapEstimates, countryCode = countryCodeROU, useRgdpch = F)
# save the plot
ggsave(plot = rou07wokall, "rou07wokall.pdf", path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

# calculate RMSPE ratios, plot and save them
rmspesROU07wok <- getRatiosOfRMSPE(synthAllROUwok, cID, "ROU", startYear, eventYear, endYear)
rou07wokrmspe <- plotRatiosOfRMSPE(rmspesROU07wok, "ROU")
ggsave(plot = rou07wokrmspe, "rou07wokrmspe.pdf", path = "images/", width = 2160, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

# plot inference plots together
rou07wokalter <- grid.arrange(arrangeGrob(rou07wokall,
                                          rou07wokrmspe,
                                          widths = c(16,9),
                                          nrow=1))

ggsave(plot = rou07wokalter, "rou07wokalter.pdf", path = "images/", width = (3840+2160), height = 2160, units = "px", dpi = dpi.setting, bg = "white")

# plot gaps
rou07wokgapCS <- plotGaps(synthAllROUwok$dataprepoutY1, synthAllROUwok$synthoutY1, countryCodeROU, vsignificance, boundsdf = boundsROU07WOK$boundsVsignificance, bounds2df = boundsROU07WOK$bounds2Vsignificance, ylab = "Gap in GDP per worker", F)
# save gaps plot
ggsave(plot = rou07wokgapCS, paste0("rou07wokgapCS", vsignificance, ".pdf"), path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")


### Romania 2001 ####
# set the intervention year between startYear and endYear
eventYear <- 2001
# get the subset of data from world, limited by startYear, endYear for the countries in the donor pool and the target country
temp <- world[which(world$year >= startYear & world$year <= endYear & (world$regionname %in% sample0 | world$regionname == "ROU")), ]

## ROU GDP per capita 2001
# filter countries in temp to only include ones which fulfill the (availability) requirements of the dataprep function
cID <- filterCountries(temp, startYear = startYear, eventYear = eventYear, countryCodeROU)

# estimate the synthetic control counterfactual for the target country, repeat for each country in the donor pool for inference
synthAllROU01 <- synthAll(temp, countryCodeROU, cID, startYear = startYear, eventYear = eventYear, endYear = endYear, T)

# get the synth tables
synthtablesROU01 <- synth.tab(
  dataprep.res = synthAllROU01$dataprepoutY1,
  synth.res = synthAllROU01$synthoutY1
)

# # copy averaged actual, synthetic and sample mean predictor values to clipboard
# write.excel(synthtablesROU01$tab.pred)

# print synth tables
print(synthtablesROU01)

# plot path
rou01path <- plotPaths(synthAllROU01$dataprepoutY1, synthAllROU01$synthoutY1, countryCodeROU, T, ylim = c(5000, 30000)) +
  labs(y ="GDP per capita") +
  guides(linetype=guide_legend(keywidth = 3, keyheight = 1)) +
  scale_linetype_manual(values = c("f4", "solid"),
                        labels=c("Synthetic Romania", "Romania"))
# save path plot
ggsave(plot = rou01path, "rou01path.pdf", path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

# plot pie chart
rou01pie <- plotPie(synthtablesROU01)
# save pie chart
ggsave(plot = rou01pie, "rou01pie.pdf", path = "images/", width = 2500, height = 2500, units = "px", dpi = 332, bg = "white")

# load RDS
load("rou01allParallel.RData")
# # estimate random donor samples
# rou01allParallel <- randomizeSampleParallel(countryCodeROU, "ROU", length(cID), world, startYear = startYear, eventYear = eventYear, endYear = endYear, 500, addToSeed = 240901)
# format the input data to include the main estimate first
randomizedSampleEstimatesInput7 <- data.frame(cbind(synthAllROU01$dataprepoutY1$Y0plot%*%synthAllROU01$synthoutY1$solution.w, rou01allParallel$RDSestimates))
# format the losses (pre-intervention MSE) to include the main estimate first
rou01RDSlosses <- data.frame(cbind(synthAllROU01$synthoutY1$loss.v, rou01allParallel[["losses"]]))
# plot the random donor samples, highlighting the main estimate and the best fit estimate, Box plot of the point in each estimation included
rou01rds <- plotRDSParallel(countryCode = countryCodeROU, countryISO3 = "ROU", randomizedSampleEstimates = randomizedSampleEstimatesInput7, losses = rou01RDSlosses)
rou01rdsplot <- grid.arrange(rou01rds[[1]], rou01rds[[2]], widths = c(7,1), top=textGrob("Romania", gp=gpar(fontsize=30)))
# save the RDS plot
ggsave(plot = rou01rdsplot, "rou01rds.pdf", path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

# calculate the summary statistics for the RDS estimates in the last year analyzed
romania2019b <- summaryRDS(countryCodeROU, rou01allParallel, startYear = 1990, eventYear = 2001, lastSummaryYear = 2019, useRgdpch = T)
# # copy the summary statistics table
# write.excel(romania2019b)

# calculate the summary statistics for the RDS estimates in 2013
romania2013b <- summaryRDS(countryCodeROU, rou01allParallel, startYear = 1990, eventYear = 2001, lastSummaryYear = 2013, useRgdpch = T)
# # copy the summary statistics table
# write.excel(romania2013b)

# Define the options of the function SCM.CS.
treated <- length(union(cID, countryCodeROU))
T0 <- eventYear - startYear
precision <- 30
type <- "uniform"
phi <- 0
v <- matrix(0, 1, length(union(cID, countryCodeROU)))
vsignificance <- 1/length(union(cID, countryCodeROU))
plot <- FALSE

# calculate bounds and alternative gaps
boundsROU01 <- getBounds(synthAllROU01, countryCodeROU, treated, T0, phi, v, precision, type, vsignificance)

# plot the estimates for each country in the donor pool and the main estimate
rou01all <- plotDonorSampleSynth(boundsROU01$allGapEstimates, countryCode = countryCodeROU, useRgdpch = T)
# save the plot
ggsave(plot = rou01all, "rou01all.pdf", path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

# calculate RMSPE ratios, plot and save them
rmspesROU01 <- getRatiosOfRMSPE(synthAllROU01, cID, "ROU", startYear, eventYear, endYear)
rou01rmspe <- plotRatiosOfRMSPE(rmspesROU01, "ROU")
ggsave(plot = rou01rmspe, "rou01rmspe.pdf", path = "images/", width = 2160, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

# plot inference plots together
rou01alter <- grid.arrange(arrangeGrob(rou01all,
                                       rou01rmspe,
                                       widths = c(16,9),
                                       nrow=1))

ggsave(plot = rou01alter, "rou01alter.pdf", path = "images/", width = (3840+2160), height = 2160, units = "px", dpi = dpi.setting, bg = "white")

# plot gaps
rou01gapCS <- plotGaps(synthAllROU01$dataprepoutY1, synthAllROU01$synthoutY1, countryCodeROU, vsignificance, boundsdf = boundsROU01$boundsVsignificance, bounds2df = boundsROU01$bounds2Vsignificance, ylab = "Gap in GDP per capita", T)
# save the gaps plot
ggsave(plot = rou01gapCS, paste0("rou01gapCS", vsignificance, ".pdf"), path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")


## ROU GDP per worker 2001
# filter countries in temp to only include ones which fulfill the (availability) requirements of the dataprep function
cID <- filterCountriesWOK(temp, startYear = startYear, eventYear = eventYear, countryCodeROU)

# estimate the synthetic control counterfactual for the target country, repeat for each country in the donor pool for inference
synthAllROU01wok <- synthAll(temp, countryCodeROU, cID, startYear = startYear, eventYear = eventYear, endYear = endYear, F)

# Get synth tables
synthtablesROUw01 <- synth.tab(
  dataprep.res = synthAllROU01wok$dataprepoutY1,
  synth.res = synthAllROU01wok$synthoutY1
) 

# # copy averaged actual, synthetic and sample mean predictor values to clipboard
# write.excel(synthtablesROUw01$tab.pred)

# print synth tables:
print(synthtablesROUw01)

# plot path
rouwok01path <- plotPaths(synthAllROU01wok$dataprepoutY1, synthAllROU01wok$synthoutY1, countryCodeROU, F, ylim = c(15000, 65000)) +
  labs(y ="GDP per worker") +
  guides(linetype=guide_legend(keywidth = 3, keyheight = 1)) +
  scale_linetype_manual(values = c("f4", "solid"),
                        labels=c("Synthetic Romania", "Romania"))
# save path plot
ggsave(plot = rouwok01path, "rouwok01path.pdf", path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

# plot pie chart
rouwok01pie <- plotPie(synthtablesROUw01)
# save pie chart
ggsave(plot = rouwok01pie, "rouwok01pie.pdf", path = "images/", width = 2500, height = 2500, units = "px", dpi = 332, bg = "white")

# load RDS
load("rou01wokallParallel.RData")
# # estimate random donor samples
# rou01wokallParallel <- randomizeSampleWOKParallel(countryCodeROU, "ROU", length(cID), world, startYear = startYear, eventYear = eventYear, endYear = endYear, 500, addToSeed = 190601)
# format the input data to include the main estimate first
randomizedSampleEstimatesInput8 <- data.frame(cbind(synthAllROU01wok$dataprepoutY1$Y0plot%*%synthAllROU01wok$synthoutY1$solution.w, rou01wokallParallel$RDSestimates))
# format the losses (pre-intervention MSE) to include the main estimate first
rou01wokRDSlosses <- data.frame(cbind(synthAllROU01wok$synthoutY1$loss.v, rou01wokallParallel[["losses"]]))
# plot the random donor samples, highlighting the main estimate and the best fit estimate, Box plot of the point in each estimation included
rou01wokrds <- plotRDSParallel(countryCode = countryCodeROU, countryISO3 = "ROU", randomizedSampleEstimates = randomizedSampleEstimatesInput8, losses = rou01wokRDSlosses,useRgdpch = F)
rou01wokrdsplot <- grid.arrange(rou01wokrds[[1]], rou01wokrds[[2]], widths = c(7,1), top=textGrob("Romania", gp=gpar(fontsize=30)))
# save the RDS plot
ggsave(plot = rou01wokrdsplot, "rou01wokrds.pdf", path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")


# calculate the summary statistics for the RDS estimates in the last year analyzed
romania2019bw <- summaryRDS(countryCodeROU, rou01wokallParallel, startYear = 1990, eventYear = 2001, lastSummaryYear = 2019, useRgdpch = F)
# # copy the summary statistics table
# write.excel(romania2019bw)

# calculate the summary statistics for the RDS estimates in 2013
romania2013bw <- summaryRDS(countryCodeROU, rou01wokallParallel, startYear = 1990, eventYear = 2001, lastSummaryYear = 2013, useRgdpch = F)
# # copy the summary statistics table
# write.excel(romania2013bw)

# Define the options of the function SCM.CS.
treated <- length(union(cID, countryCodeROU))
T0 <- eventYear - startYear
precision <- 30
type <- "uniform"
phi <- 0
v <- matrix(0, 1, length(union(cID, countryCodeROU)))
vsignificance <- 1/length(union(cID, countryCodeROU))
plot <- FALSE

# calculate bounds and alternative gaps
boundsROU01WOK <- getBounds(synthAllROU01wok, countryCodeROU, treated, T0, phi, v, precision, type, vsignificance)

# plot the estimates for each country in the donor pool and the main estimate
rou01wokall <- plotDonorSampleSynth(boundsROU01WOK$allGapEstimates, countryCode = countryCodeROU, useRgdpch = F)
# save the plot
ggsave(plot = rou01wokall, "rou01wokall.pdf", path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

# calculate RMSPE ratios, plot and save them
rmspesROU01wok <- getRatiosOfRMSPE(synthAllROU01wok, cID, "ROU", startYear, eventYear, endYear)
rou01wokrmspe <- plotRatiosOfRMSPE(rmspesROU01wok, "ROU")
ggsave(plot = rou01wokrmspe, "rou01wokrmspe.pdf", path = "images/", width = 2160, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

# plot inference plots together
rou01wokalter <- grid.arrange(arrangeGrob(rou01wokall,
                                          rou01wokrmspe,
                                          widths = c(16,9),
                                          nrow=1))

ggsave(plot = rou01wokalter, "rou01wokalter.pdf", path = "images/", width = (3840+2160), height = 2160, units = "px", dpi = dpi.setting, bg = "white")


# plot gaps
rou01wokgapCS <- plotGaps(synthAllROU01wok$dataprepoutY1, synthAllROU01wok$synthoutY1, countryCodeROU, vsignificance, boundsdf = boundsROU01WOK$boundsVsignificance, bounds2df = boundsROU01WOK$bounds2Vsignificance, ylab = "Gap in GDP per worker", F)
# save the gaps plot
ggsave(plot = rou01wokgapCS, paste0("rou01wokgapCS", vsignificance, ".pdf"), path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

## EXTRA PLOTS ####
eventYear <- 2007
legend = gtable_filter(ggplotGrob(bgr07rds[[1]]), "guide-box") 
bgrrou07 <- grid.arrange(arrangeGrob(arrangeGrob(bgr07rds[[1]],
                                                 bgr07rds[[2]],
                                                 top=textGrob("Bulgaria", gp=gpar(fontsize=30)),
                                                 widths = c(7,1)),
                                     arrangeGrob(rou07rds[[1]]+ theme(legend.position="none"),
                                                 rou07rds[[2]],
                                                 top=textGrob("Romania", gp=gpar(fontsize=30)),
                                                 widths = c(7,1)),
                                     widths = c(8,8),
                                     left = textGrob("Gap in GDP per capita", rot = 90, vjust = 1, gp = gpar(fontsize = 19))),
                         nrow=1)

ggsave(plot = bgrrou07, "bgrrou07.pdf", path = "images/", width = 7680, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

legend = gtable_filter(ggplotGrob(bgr07wokrds[[1]]), "guide-box") 
bgrrou07wok <- grid.arrange(arrangeGrob(arrangeGrob(bgr07wokrds[[1]],
                                                 bgr07wokrds[[2]],
                                                 top=textGrob("Bulgaria", gp=gpar(fontsize=30)),
                                                 widths = c(7,1)),
                                     arrangeGrob(rou07wokrds[[1]]+ theme(legend.position="none"),
                                                 rou07wokrds[[2]],
                                                 top=textGrob("Romania", gp=gpar(fontsize=30)),
                                                 widths = c(7,1)),
                                     widths = c(8,8),
                                     left = textGrob("Gap in GDP per worker", rot = 90, vjust = 1, gp = gpar(fontsize = 19))),
                         nrow=1)

ggsave(plot = bgrrou07wok, "bgrrou07wok.pdf", path = "images/", width = 7680, height = 2160, units = "px", dpi = dpi.setting, bg = "white")


eventYear <- 2001

legend = gtable_filter(ggplotGrob(bgr01rds[[1]]), "guide-box") 
bgrrou01 <- grid.arrange(arrangeGrob(arrangeGrob(bgr01rds[[1]],
                                                 bgr01rds[[2]],
                                                 top=textGrob("Bulgaria", gp=gpar(fontsize=30)),
                                                 widths = c(7,1)),
                                     arrangeGrob(rou01rds[[1]]+ theme(legend.position="none"),
                                                 rou01rds[[2]],
                                                 top=textGrob("Romania", gp=gpar(fontsize=30)),
                                                 widths = c(7,1)),
                                     widths = c(8,8),
                                     left = textGrob("Gap in GDP per capita", rot = 90, vjust = 1, gp = gpar(fontsize = 19))),
                         nrow=1)

ggsave(plot = bgrrou01, "bgrrou01.pdf", path = "images/", width = 7680, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

legend = gtable_filter(ggplotGrob(bgr01wokrds[[1]]), "guide-box") 
bgrrou01wok <- grid.arrange(arrangeGrob(arrangeGrob(bgr01wokrds[[1]],
                                                    bgr01wokrds[[2]],
                                                    top=textGrob("Bulgaria", gp=gpar(fontsize=30)),
                                                    widths = c(7,1)),
                                        arrangeGrob(rou01wokrds[[1]]+ theme(legend.position="none"),
                                                    rou01wokrds[[2]],
                                                    top=textGrob("Romania", gp=gpar(fontsize=30)),
                                                    widths = c(7,1)),
                                        widths = c(8,8),
                                        left = textGrob("Gap in GDP per worker", rot = 90, vjust = 1, gp = gpar(fontsize = 19))),
                            nrow=1)

ggsave(plot = bgrrou01wok, "bgrrou01wok.pdf", path = "images/", width = 7680, height = 2160, units = "px", dpi = dpi.setting, bg = "white")



## CROATIA ####
countryCodeHRV <- 86
startYear <- 1995
endYear <- 2019

### Croatia 2013 ####
eventYear <- 2013
temp <- world[which(world$year >= startYear & world$year <= endYear & (world$regionname %in% sample0 | world$regionname == "HRV")), ]
## HRV GDP per capita 2013
# filter countries in temp to only include ones which fulfill the (availability) requirements of the dataprep function
cID <- filterCountries(temp, startYear, eventYear, countryCodeHRV)

# estimate the synthetic control counterfactual for the target country, repeat for each country in the donor pool for inference
synthAllHRV <- synthAll(temp, countryCodeHRV, cID, startYear = startYear, eventYear = eventYear, endYear = endYear, T)

# get synth tables
synthtablesHRV <- synth.tab(
  dataprep.res = synthAllHRV$dataprepoutY1,
  synth.res = synthAllHRV$synthoutY1
)

# # copy averaged actual, synthetic and sample mean predictor values to clipboard
# write.excel(synthtablesHRV$tab.pred)

# print synth tables
print(synthtablesHRV)

# plot path
hrv13path <- plotPaths(synthAllHRV$dataprepoutY1, synthAllHRV$synthoutY1, countryCodeHRV) +
  labs(y ="GDP per capita") +
  guides(linetype=guide_legend(keywidth = 3, keyheight = 1)) +
  scale_linetype_manual(values = c("f4", "solid"),
                        labels=c("Synthetic Croatia", "Croatia"))
# save path plot
ggsave(plot = hrv13path, "hrv13path.pdf", path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

# plot pie chart
hrv13pie <- plotPie(synthtablesHRV)
# save pie chart
ggsave(plot = hrv13pie, "hrv13pie.pdf", path = "images/", width = 2500, height = 2500, units = "px", dpi = 332, bg = "white")

#load RDS
load("hrv13allParallel.RData")
# # estimate random donor samples
# hrv13allParallel <- randomizeSampleParallel(countryCodeHRV, "HRV", length(cID), world, startYear = startYear, eventYear = eventYear, endYear = endYear, 500, addToSeed = 3500)
# format the input data to include the main estimate first
randomizedSampleEstimatesInput9 <- data.frame(cbind(synthAllHRV$dataprepoutY1$Y0plot%*%synthAllHRV$synthoutY1$solution.w, hrv13allParallel$RDSestimates))
# format the losses (pre-intervention MSE) to include the main estimate first
hrv13RDSlosses <- data.frame(cbind(synthAllHRV$synthoutY1$loss.v, hrv13allParallel[["losses"]]))
# plot the random donor samples, highlighting the main estimate and the best fit estimate, Box plot of the point in each estimation included
hrv13rds <- plotRDSParallel(countryCode = countryCodeHRV, countryISO3 = "HRV", randomizedSampleEstimates = randomizedSampleEstimatesInput9, losses = hrv13RDSlosses, useRgdpch = T)
hrv13rdsplot <- grid.arrange(hrv13rds[[1]], hrv13rds[[2]], widths = c(7,1), top=textGrob("Croatia", gp=gpar(fontsize=30)))
# save the RDS plot
ggsave(plot = hrv13rdsplot, "hrv13rds.pdf", path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

# summary
croatia2019 <- summaryRDS(countryCodeHRV, hrv13allParallel, startYear = 1995, eventYear = 2013, lastSummaryYear = 2019, useRgdpch = T)
# # copy the summary statistics table
# write.excel(croatia2019)

# Define the options of the function SCM.CS.
treated <- length(union(cID, countryCodeHRV))
T0 <- eventYear - startYear
precision <- 30
type <- "uniform"
phi <- 0
v <- matrix(0, 1, length(union(cID, countryCodeHRV)))
vsignificance <- 1/length(union(cID, countryCodeHRV))
plot <- FALSE

# calculate bounds and alternative gaps
boundsHRV13 <- getBounds(synthAllHRV, countryCodeHRV, treated, T0, phi, v, precision, type, vsignificance)

# plot the estimates for each country in the donor pool and the main estimate
hrv13all <- plotDonorSampleSynth(boundsHRV13$allGapEstimates, countryCode = countryCodeHRV, useRgdpch = T)
# save the plot
ggsave(plot = hrv13all, "hrv13all.pdf", path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

# calculate RMSPE ratios, plot and save them
rmspesHRV13 <- getRatiosOfRMSPE(synthAllHRV, cID, "HRV", startYear, eventYear, endYear)
hrv13rmspe <- plotRatiosOfRMSPE(rmspesHRV13, "HRV")
ggsave(plot = hrv13rmspe, "hrv13rmspe.pdf", path = "images/", width = 2160, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

# plot inference plots together
hrv13alter <- grid.arrange(arrangeGrob(hrv13all,
                                       hrv13rmspe,
                                       widths = c(16,9),
                                       nrow=1))

ggsave(plot = hrv13alter, "hrv13alter.pdf", path = "images/", width = (3840+2160), height = 2160, units = "px", dpi = dpi.setting, bg = "white")


# plot gaps
hrv13gapCS <- plotGaps(synthAllHRV$dataprepoutY1, synthAllHRV$synthoutY1, countryCodeHRV, vsignificance, boundsdf = boundsHRV13$boundsVsignificance, bounds2df = boundsHRV13$bounds2Vsignificance, ylab = "Gap in GDP per capita", T)
# save the gaps plot
ggsave(plot = hrv13gapCS, paste0("hrv13gapCS", vsignificance, ".pdf"), path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")


## HRV GDP per worker 2013
cID <- filterCountriesWOK(temp, startYear, eventYear, countryCodeHRV)

# estimate the synthetic control counterfactual for the target country, repeat for each country in the donor pool for inference
synthAllHRVwok <- synthAll(temp, countryCodeHRV, cID, startYear = startYear, eventYear = eventYear, endYear = endYear, F)

# Get synth tables
synthtablesHRVw <- synth.tab(
  dataprep.res = synthAllHRVwok$dataprepoutY1,
  synth.res = synthAllHRVwok$synthoutY1
) 

# # copy averaged actual, synthetic and sample mean predictor values to clipboard
# write.excel(synthtablesHRVw$tab.pred)

# print synth tables
print(synthtablesHRVw)

#Paths
hrvwok13path <- plotPaths(synthAllHRVwok$dataprepoutY1, synthAllHRVwok$synthoutY1, countryCodeHRV, F, ylim = c(15000, 65000)) +
  labs(y ="GDP per worker") +
  guides(linetype=guide_legend(keywidth = 3, keyheight = 1)) +
  scale_linetype_manual(values = c("f4", "solid"),
                        labels=c("Synthetic Croatia", "Croatia"))
ggsave(plot = hrvwok13path, "hrvwok13path.pdf", path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

# plot pie chart
hrvwok13pie <- plotPie(synthtablesHRVw)
# save pie chart
ggsave(plot = hrvwok13pie, "hrvwok13pie.pdf", path = "images/", width = 2500, height = 2500, units = "px", dpi = 332, bg = "white")

#load RDS
load("hrv13wokallParallel.RData")
# # alternatively estimate with random donor samples
# hrv13wokallParallel <- randomizeSampleWOKParallel(countryCodeHRV, "HRV", length(cID), world, startYear = startYear, eventYear = eventYear, endYear = endYear, 500, addToSeed = 290633)

# format the input data to include the main estimate first
randomizedSampleEstimatesInput10 <- data.frame(cbind(synthAllHRVwok$dataprepoutY1$Y0plot%*%synthAllHRVwok$synthoutY1$solution.w, hrv13wokallParallel$RDSestimates))
# format the losses (pre-intervention MSE) to include the main estimate first
hrv13wokRDSlosses <- data.frame(cbind(synthAllHRVwok$synthoutY1$loss.v, hrv13wokallParallel[["losses"]]))
# plot the random donor samples, highlighting the main estimate and the best fit estimate, Box plot of the point in each estimation included
hrv13wokrds <- plotRDSParallel(countryCode = countryCodeHRV, countryISO3 = "HRV", randomizedSampleEstimates = randomizedSampleEstimatesInput10, losses = hrv13wokRDSlosses, useRgdpch = F)
hrv13wokrdsplot <- grid.arrange(hrv13wokrds[[1]], hrv13wokrds[[2]], widths = c(7,1), top=textGrob("Croatia", gp=gpar(fontsize=30)))
# save the RDS plot
ggsave(plot = hrv13wokrdsplot, "hrv13wokrds.pdf", path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

# summary
croatia2019w <- summaryRDS(countryCodeHRV, hrv13wokallParallel, startYear = 1995, eventYear = 2013, lastSummaryYear = 2019, useRgdpch = F)
# # copy the summary statistics table
# write.excel(croatia2019w)

# Define the options of the function SCM.CS.
treated <- length(union(cID, countryCodeHRV))
T0 <- eventYear - startYear
precision <- 30
type <- "uniform"
phi <- 0
v <- matrix(0, 1, length(union(cID, countryCodeHRV)))
vsignificance <- 1/length(union(cID, countryCodeHRV))
plot <- FALSE

# calculate bounds and alternative gaps
boundsHRV13WOK <- getBounds(synthAllHRVwok, countryCodeHRV, treated, T0, phi, v, precision, type, vsignificance)

# plot the estimates for each country in the donor pool and the main estimate
hrv13wokall <- plotDonorSampleSynth(boundsHRV13WOK$allGapEstimates, countryCode = countryCodeHRV, useRgdpch = F)
# save the plot
ggsave(plot = hrv13wokall, "hrv13wokall.pdf", path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

# calculate RMSPE ratios, plot and save them
rmspesHRV13wok <- getRatiosOfRMSPE(synthAllHRVwok, cID, "HRV", startYear, eventYear, endYear)
hrv13wokrmspe <- plotRatiosOfRMSPE(rmspesHRV13wok, "HRV")
ggsave(plot = hrv13wokrmspe, "hrv13wokrmspe.pdf", path = "images/", width = 2160, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

# plot inference plots together
hrv13wokalter <- grid.arrange(arrangeGrob(hrv13wokall,
                                          hrv13wokrmspe,
                                          widths = c(16,9),
                                          nrow=1))

ggsave(plot = hrv13wokalter, "hrv13wokalter.pdf", path = "images/", width = (3840+2160), height = 2160, units = "px", dpi = dpi.setting, bg = "white")


# plot gaps
hrv13wokgapCS <- plotGaps(synthAllHRVwok$dataprepoutY1, synthAllHRVwok$synthoutY1, countryCodeHRV, vsignificance, boundsdf = boundsHRV13WOK$boundsVsignificance, bounds2df = boundsHRV13WOK$bounds2Vsignificance, ylab = "Gap in GDP per worker", F)
# save the gaps plot
ggsave(plot = hrv13wokgapCS, paste0("hrv13wokgapCS", vsignificance, ".pdf"), path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")


### Croatia 2007 ####
eventYear <- 2007

temp <- world[which(world$year >= startYear & world$year <= endYear & (world$regionname %in% sample0 | world$regionname == "HRV")), ]
## HRV GDP per capita 2007
cID <- filterCountries(temp, startYear, eventYear, countryCodeHRV)

# estimate the synthetic control counterfactual for the target country, repeat for each country in the donor pool for inference
synthAllHRV07 <- synthAll(temp, countryCodeHRV, cID, startYear = startYear, eventYear = eventYear, endYear = endYear, T)

# get the synth tables
synthtablesHRV07 <- synth.tab(
  dataprep.res = synthAllHRV07$dataprepoutY1,
  synth.res = synthAllHRV07$synthoutY1
)

# # copy averaged actual, synthetic and sample mean predictor values to clipboard
# write.excel(synthtablesHRV07$tab.pred)

# print synth tables:
print(synthtablesHRV07)

# plot path
hrv07path <- plotPaths(synthAllHRV07$dataprepoutY1, synthAllHRV07$synthoutY1, countryCodeHRV) +
  labs(y ="GDP per capita") +
  guides(linetype=guide_legend(keywidth = 3, keyheight = 1)) +
  scale_linetype_manual(values = c("f4", "solid"),
                        labels=c("Synthetic Croatia", "Croatia"))
# save path plot
ggsave(plot = hrv07path, "hrv07path.pdf", path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

# plot pie chart
hrv07pie <- plotPie(synthtablesHRV07)
# save pie chart
ggsave(plot = hrv07pie, "hrv07pie.pdf", path = "images/", width = 2500, height = 2500, units = "px", dpi = 332, bg = "white")

#load RDS
load("hrv07allParallel.RData")
# # estimate random donor samples
# hrv07allParallel <- randomizeSampleParallel(countryCodeHRV, "HRV", length(cID), world, startYear = startYear, eventYear = eventYear, endYear = endYear, 500, addToSeed = 101010)
# format the input data to include the main estimate first
randomizedSampleEstimatesInput11 <- data.frame(cbind(synthAllHRV07$dataprepoutY1$Y0plot%*%synthAllHRV07$synthoutY1$solution.w, hrv07allParallel$RDSestimates))
# format the losses (pre-intervention MSE) to include the main estimate first
hrv07RDSlosses <- data.frame(cbind(synthAllHRV07$synthoutY1$loss.v, hrv07allParallel[["losses"]]))
# plot the random donor samples, highlighting the main estimate and the best fit estimate, Box plot of the point in each estimation included
hrv07rds <- plotRDSParallel(countryCode = countryCodeHRV, countryISO3 = "HRV", randomizedSampleEstimates = randomizedSampleEstimatesInput11, losses = hrv07RDSlosses, useRgdpch = T)
hrv07rdsplot <- grid.arrange(hrv07rds[[1]], hrv07rds[[2]], widths = c(7,1), top=textGrob("Croatia", gp=gpar(fontsize=30)))
# save the RDS plot
ggsave(plot = hrv07rdsplot, "hrv07rds.pdf", path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

# summary
croatia2019b <- summaryRDS(countryCodeHRV, hrv07allParallel, startYear = 1995, eventYear = 2007, lastSummaryYear = 2019, useRgdpch = T)
# # copy the summary statistics table
# write.excel(croatia2019b)

# Define the options of the function SCM.CS.
treated <- length(union(cID, countryCodeHRV))
T0 <- eventYear - startYear
precision <- 30
type <- "uniform"
phi <- 0
v <- matrix(0, 1, length(union(cID, countryCodeHRV)))
vsignificance <- 1/length(union(cID, countryCodeHRV))
plot <- FALSE

# calculate bounds and alternative gaps
boundsHRV07 <- getBounds(synthAllHRV07, countryCodeHRV, treated, T0, phi, v, precision, type, vsignificance)

# plot the estimates for each country in the donor pool and the main estimate
hrv07all <- plotDonorSampleSynth(boundsHRV07$allGapEstimates, countryCode = countryCodeHRV, useRgdpch = T)
# save the plot
ggsave(plot = hrv07all, "hrv07all.pdf", path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

# calculate RMSPE ratios, plot and save them
rmspesHRV07 <- getRatiosOfRMSPE(synthAllHRV07, cID, "HRV", startYear, eventYear, endYear)
hrv07rmspe <- plotRatiosOfRMSPE(rmspesHRV07, "HRV")
ggsave(plot = hrv07rmspe, "hrv07rmspe.pdf", path = "images/", width = 2160, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

# plot inference plots together
hrv07alter <- grid.arrange(arrangeGrob(hrv07all,
                                       hrv07rmspe,
                                       widths = c(16,9),
                                       nrow=1))

ggsave(plot = hrv07alter, "hrv07alter.pdf", path = "images/", width = (3840+2160), height = 2160, units = "px", dpi = dpi.setting, bg = "white")


# plot gaps
hrv07gapCS <- plotGaps(synthAllHRV07$dataprepoutY1, synthAllHRV07$synthoutY1, countryCodeHRV, vsignificance, boundsdf = boundsHRV07$boundsVsignificance, bounds2df = boundsHRV07$bounds2Vsignificance, ylab = "Gap in GDP per capita", T)
# save the plot
ggsave(plot = hrv07gapCS, paste0("hrv07gapCS", vsignificance, ".pdf"), path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")


## HRV GDP per worker 2007
cID <- filterCountriesWOK(temp, startYear, eventYear, countryCodeHRV)

# estimate the synthetic control counterfactual for the target country, repeat for each country in the donor pool for inference
synthAllHRV07wok <- synthAll(temp, countryCodeHRV, cID, startYear = startYear, eventYear = eventYear, endYear = endYear, F)

# Get synth tables
synthtablesHRVw07 <- synth.tab(
  dataprep.res = synthAllHRV07wok$dataprepoutY1,
  synth.res = synthAllHRV07wok$synthoutY1
) 

# # copy averaged actual, synthetic and sample mean predictor values to clipboard
# write.excel(synthtablesHRVw07$tab.pred)

# prin synth tables
print(synthtablesHRVw07)

#Paths
hrvwok07path <- plotPaths(synthAllHRV07wok$dataprepoutY1, synthAllHRV07wok$synthoutY1, countryCodeHRV, F, ylim = c(15000, 65000)) +
  labs(y ="GDP per worker") +
  guides(linetype=guide_legend(keywidth = 3, keyheight = 1)) +
  scale_linetype_manual(values = c("f4", "solid"),
                        labels=c("Synthetic Croatia", "Croatia"))
ggsave(plot = hrvwok07path, "hrvwok07path.pdf", path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

# plot pie chart
hrvwok07pie <- plotPie(synthtablesHRVw07)
# save pie chart
ggsave(plot = hrvwok07pie, "hrvwok07pie.pdf", path = "images/", width = 2500, height = 2500, units = "px", dpi = 332, bg = "white")

# load RDS
load("hrv07wokallParallel.RData")
# # estimate random donor samples
# hrv07wokallParallel <- randomizeSampleWOKParallel(countryCodeHRV, "HRV", length(cID), world, startYear = startYear, eventYear = eventYear, endYear = endYear, 500, addToSeed = 350000)
# format the input data to include the main estimate first
randomizedSampleEstimatesInput12 <- data.frame(cbind(synthAllHRV07wok$dataprepoutY1$Y0plot%*%synthAllHRV07wok$synthoutY1$solution.w, hrv07wokallParallel$RDSestimates))
# format the losses (pre-intervention MSE) to include the main estimate first
hrv07wokRDSlosses <- data.frame(cbind(synthAllHRV07wok$synthoutY1$loss.v, hrv07wokallParallel[["losses"]]))
# plot the random donor samples, highlighting the main estimate and the best fit estimate, Box plot of the point in each estimation included
hrv07wokrds <- plotRDSParallel(countryCode = countryCodeHRV, countryISO3 = "HRV", randomizedSampleEstimates = randomizedSampleEstimatesInput12, losses = hrv07wokRDSlosses, useRgdpch = F)
hrv07wokrdsplot <- grid.arrange(hrv07wokrds[[1]], hrv07wokrds[[2]], widths = c(7,1), top=textGrob("Croatia", gp=gpar(fontsize=30)))
# save the RDS plot
ggsave(plot = hrv07wokrdsplot, "hrv07wokrds.pdf", path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

# summary
croatia2019bw <- summaryRDS(countryCodeHRV, hrv07wokallParallel, startYear = 1995, eventYear = 2007, lastSummaryYear = 2019, useRgdpch = F)
# # copy the summary statistics table
# write.excel(croatia2019bw)

# Define the options of the function SCM.CS.
treated <- length(union(cID, countryCodeHRV))
T0 <- eventYear - startYear
precision <- 30
type <- "uniform"
phi <- 0
v <- matrix(0, 1, length(union(cID, countryCodeHRV)))
vsignificance <- 1/length(union(cID, countryCodeHRV))
plot <- FALSE

# calculate bounds and alternative gaps
boundsHRV07WOK <- getBounds(synthAllHRV07wok, countryCodeHRV, treated, T0, phi, v, precision, type, vsignificance)

# plot the estimates for each country in the donor pool and the main estimate
hrv07wokall <- plotDonorSampleSynth(boundsHRV07WOK$allGapEstimates, countryCode = countryCodeHRV, useRgdpch = F)
# save the plot
ggsave(plot = hrv07wokall, "hrv07wokall.pdf", path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

# calculate RMSPE ratios, plot and save them
rmspesHRV07wok <- getRatiosOfRMSPE(synthAllHRV07wok, cID, "HRV", startYear, eventYear, endYear)
hrv07wokrmspe <- plotRatiosOfRMSPE(rmspesHRV07wok, "HRV")
ggsave(plot = hrv07wokrmspe, "hrv07wokrmspe.pdf", path = "images/", width = 2160, height = 2160, units = "px", dpi = dpi.setting, bg = "white")

# plot inference plots together
hrv07wokalter <- grid.arrange(arrangeGrob(hrv07wokall,
                                          hrv07wokrmspe,
                                          widths = c(16,9),
                                          nrow=1))

ggsave(plot = hrv07wokalter, "hrv07wokalter.pdf", path = "images/", width = (3840+2160), height = 2160, units = "px", dpi = dpi.setting, bg = "white")


# plot gaps
hrv07wokgapCS <- plotGaps(synthAllHRV07wok$dataprepoutY1, synthAllHRV07wok$synthoutY1, countryCodeHRV, vsignificance, boundsdf = boundsHRV07WOK$boundsVsignificance, bounds2df = boundsHRV07WOK$bounds2Vsignificance, ylab = "Gap in GDP per worker", F)
# save gaps plot
ggsave(plot = hrv07wokgapCS, paste0("hrv07wokgapCS", vsignificance, ".pdf"), path = "images/", width = 3840, height = 2160, units = "px", dpi = dpi.setting, bg = "white")


## Complete summary ####
summaryBulgaria <- cbind(summaryEffects(synthAllBGR$dataprepoutY1, synthAllBGR$synthoutY1,
                     countryCodeBGR, startYear = 1990, eventYear = 2007,
                     endYear = 2019, lastSummaryYear = 2019, useRgdpch = T),
      summaryEffects(synthAllBGR$dataprepoutY1, synthAllBGR$synthoutY1,
                     countryCodeBGR, startYear = 1990, eventYear = 2007,
                     endYear = 2019, lastSummaryYear = 2013, useRgdpch = T),
      summaryEffects(synthAllBGRwok$dataprepoutY1, synthAllBGRwok$synthoutY1,
                     countryCodeBGR, startYear = 1990, eventYear = 2007,
                     endYear = 2019, lastSummaryYear = 2019, useRgdpch = F),
      summaryEffects(synthAllBGRwok$dataprepoutY1, synthAllBGRwok$synthoutY1,
                     countryCodeBGR, startYear = 1990, eventYear = 2007,
                     endYear = 2019, lastSummaryYear = 2013, useRgdpch = F))

summaryRomania <- cbind(summaryEffects(synthAllROU$dataprepoutY1, synthAllROU$synthoutY1,
                     countryCodeROU, startYear = 1990, eventYear = 2007,
                     endYear = 2019, lastSummaryYear = 2019, useRgdpch = T),
      summaryEffects(synthAllROU$dataprepoutY1, synthAllROU$synthoutY1,
                     countryCodeROU, startYear = 1990, eventYear = 2007,
                     endYear = 2019, lastSummaryYear = 2013, useRgdpch = T),
      summaryEffects(synthAllROUwok$dataprepoutY1, synthAllROUwok$synthoutY1,
                     countryCodeROU, startYear = 1990, eventYear = 2007,
                     endYear = 2019, lastSummaryYear = 2019, useRgdpch = F),
      summaryEffects(synthAllROUwok$dataprepoutY1, synthAllROUwok$synthoutY1,
                     countryCodeROU, startYear = 1990, eventYear = 2007,
                     endYear = 2019, lastSummaryYear = 2013, useRgdpch = F))

summaryCroatia <- cbind(summaryEffects(synthAllHRV$dataprepoutY1, synthAllHRV$synthoutY1,
                     countryCodeHRV, startYear = 1995, eventYear = 2013,
                     endYear = 2019, lastSummaryYear = 2019, useRgdpch = T),
                     summaryEffects(synthAllHRV$dataprepoutY1, synthAllHRV$synthoutY1,
                                    countryCodeHRV, startYear = 1995, eventYear = 2013,
                                    endYear = 2019, lastSummaryYear = 2019, useRgdpch = T),
      summaryEffects(synthAllHRVwok$dataprepoutY1, synthAllHRVwok$synthoutY1,
                     countryCodeHRV, startYear = 1995, eventYear = 2013,
                     endYear = 2019, lastSummaryYear = 2019, useRgdpch = F),
      summaryEffects(synthAllHRVwok$dataprepoutY1, synthAllHRVwok$synthoutY1,
                     countryCodeHRV, startYear = 1995, eventYear = 2013,
                     endYear = 2019, lastSummaryYear = 2019, useRgdpch = F))

summaryMain <- data.frame(rbind(summaryBulgaria, summaryRomania, summaryCroatia))
write.excel(summaryMain)

summary7years <- data.frame(rbind(bulgaria2013, romania2013, croatia2019))
summary7yearsWOK <- data.frame(rbind(bulgaria2013w, romania2013w, croatia2019w))

summary13years <- data.frame(rbind(bulgaria2019, romania2019))
summary13yearsWOK <- data.frame(rbind(bulgaria2019w, romania2019w))

## Compare GDP per capita ####
tomeltbg <- data.frame(cbind(synthAllBGR$dataprepoutY1[["Y0plot"]], synthAllBGR$dataprepoutY1[["Y1plot"]]))
tomeltbg$index <- as.numeric(unlist(rownames(tomeltbg)))
meltedbg <- melt(tomeltbg,  id = 'index', variable.name = 'series')

tomeltro <- data.frame(cbind(synthAllROU$dataprepoutY1[["Y0plot"]], synthAllROU$dataprepoutY1[["Y1plot"]]))
tomeltro$index <- as.numeric(unlist(rownames(tomeltro)))
meltedro <- melt(tomeltro,  id = 'index', variable.name = 'series')

tomelthr <- data.frame(cbind(synthAllHRV$dataprepoutY1[["Y0plot"]], synthAllHRV$dataprepoutY1[["Y1plot"]]))
tomelthr$index <- as.numeric(unlist(rownames(tomelthr)))
meltedhr <- melt(tomelthr,  id = 'index', variable.name = 'series')

ggplot(meltedbg, aes(index, value, group = series)) + 
  geom_line(aes(color = "black")) +
  geom_line(data = meltedbg[which(meltedbg$series == "X20"),], aes(color = "grey"), size = 1) + 
  geom_line(data = meltedro[which(meltedro$series == "X166"),], aes(color = "yellow"), size = 1) + 
  geom_line(data = meltedhr[which(meltedhr$series == "X86"),], aes(color = "blue"), size = 1) + 
  coord_cartesian(ylim = c(0, 50000))+
  scale_y_continuous(breaks=seq(0,50000,20000))+
  theme(text = element_text(size=18),
        panel.border = element_rect(linetype = 1, fill = NA),
        panel.background = element_rect(fill = NA, colour = NA),
        axis.title.x = element_blank()) +
  labs(y ="") +
  guides(linetype=guide_legend(keywidth = 3, keyheight = 1)) +
  scale_color_manual("",
                     values = c("grey", "#171796", "#009B74", "#CE1126"),
                     labels=c("Donor sample countries", "Croatia", "Bulgaria", "Romania"))+
  theme(legend.title=element_blank(),
        legend.position = c(0.05, 0.95),
        legend.justification = c(0, 1))

ggsave("allSamples.pdf", path = "images/", width = 3840, height = 2160, units = "px", dpi = 332, bg = "white")

## Compare GDP per worker ####
tomeltbg <- data.frame(cbind(synthAllBGRwok$dataprepoutY1[["Y0plot"]], synthAllBGRwok$dataprepoutY1[["Y1plot"]]))
tomeltbg$index <- as.numeric(unlist(rownames(tomeltbg)))
meltedbg <- melt(tomeltbg,  id = 'index', variable.name = 'series')

tomeltro <- data.frame(cbind(synthAllROUwok$dataprepoutY1[["Y0plot"]], synthAllROUwok$dataprepoutY1[["Y1plot"]]))
tomeltro$index <- as.numeric(unlist(rownames(tomeltro)))
meltedro <- melt(tomeltro,  id = 'index', variable.name = 'series')

tomelthr <- data.frame(cbind(synthAllHRVwok$dataprepoutY1[["Y0plot"]], synthAllHRVwok$dataprepoutY1[["Y1plot"]]))
tomelthr$index <- as.numeric(unlist(rownames(tomelthr)))
meltedhr <- melt(tomelthr,  id = 'index', variable.name = 'series')

ggplot(meltedbg, aes(index, value, group = series)) + 
  geom_line(aes(color = "black")) +
  geom_line(data = meltedbg[which(meltedbg$series == "X20"),], aes(color = "grey"), size = 1) + 
  geom_line(data = meltedro[which(meltedro$series == "X166"),], aes(color = "yellow"), size = 1) + 
  geom_line(data = meltedhr[which(meltedhr$series == "X86"),], aes(color = "blue"), size = 1) + 
  coord_cartesian(ylim = c(0, 90000))+
  scale_y_continuous(breaks=seq(0,90000,20000))+
  theme(text = element_text(size=18),
        panel.border = element_rect(linetype = 1, fill = NA),
        panel.background = element_rect(fill = NA, colour = NA),
        axis.title.x = element_blank()) +
  labs(y ="") +
  guides(linetype=guide_legend(keywidth = 3, keyheight = 1)) +
  scale_color_manual("",
                     values = c("grey", "#171796", "#009B74", "#CE1126"),
                     labels=c("Donor sample countries", "Croatia", "Bulgaria", "Romania"))+
  theme(legend.title=element_blank(),
        legend.position = c(0.05, 0.95),
        legend.justification = c(0, 1))

ggsave("allSamplesWOK.pdf", path = "images/", width = 3840, height = 2160, units = "px", dpi = 332, bg = "white")
