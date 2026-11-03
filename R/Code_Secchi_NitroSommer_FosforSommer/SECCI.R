##### Modified Nicolas Dupont 30.10.2026 #####

##### Extract adn calculate ecological condition og coastal waters #####
##### For Secchi depth #####
##### based on the packaging WFD2ECA by Hanno Sandvik 2025 #####
##### https://github.com/NINAnor/WFD2ECA/blob/main/ ######
##### using code available at ecRxiv.com www.ecxiv.com #####

##### starting from code available at ecrxiv.com #####
##### section 7.1 for chlorophyll a #####

## HS = Hanno Sandsvik original code

## ND = Nicolas Dupont addition


## HS

library(foreign)
library(sf)
library(readxl)
library(raster)
library(httr)
library(jsonlite)
library(stringr)


## ND

indikator <- "SECCI" #
vannkategori <- "C"
account_year <- 2024

## HS   ## ND in comments in order to not reinstall the whole package and klassegrenser for the indicator in lakes each time we run the script

# if(!file.exists("../data/VF.RData")) {
#   download.file("https://zenodo.org/records/17702361/files/NINAnor/WFD2ECA-v2.1.zip",
#                 "../data/WFD2ECA.zip")
#   folder <- "../data/WFD2ECA"
#   unzip("../data/WFD2ECA.zip", exdir = folder)
#   while (length(list.files(folder)) == 1) {
#     folder <- paste0(folder, "/", list.files(folder))
#   }
#   whichDataFile <- c(
#     "fnr.xlsx",
#     "knr.xlsx",
#     "navnNVEl.csv",
#     "navnVL.csv",
#     "navnVM.csv",
#     "navnVN.csv",
#     "NVE.RData",
#     "Norge.map",
#     paste0("V-", vannkategori, ".csv"),
#     "VF.RData",
#     "VM-aktiv.xlsx",
#     "VM-param.xlsx"
#   )
#   dummy <- file.copy(from = paste0(folder, "/data/", whichDataFile),
#                      to   = "../data/",
#                      overwrite = FALSE)
#   dummy <- file.copy(from = paste0(folder,
#                                    "/klassegr/klassegrenser_",
#                                    indikator, 
#                                    ".xlsx"),
#                      to   = "../data/",
#                      overwrite = FALSE)
#   dummy <- file.copy(from = list.files(paste0(folder, "/R/"), full.names = TRUE),
#                      to   = ".",
#                      overwrite = FALSE)
#   dummy <- file.remove("../data/WFD2ECA.zip")
#   unlink("../data/WFD2ECA", recursive = TRUE)
# }

## HS

source("Funksjon.R")
for (filnavn in list.files(".", full.names = TRUE)) {
  if (substr(filnavn, nchar(filnavn), nchar(filnavn)) %=% "R") {
    source(filnavn)
  }
}

## HS

head(as.data.frame(read_xlsx("../data/klassegrenser_" %+% indikator %+% ".xlsx", col_types = "text")))

## HS

head(as.data.frame(read_xlsx("../data/VM-aktiv.xlsx", col_types = "text")))

## HS

DATA <- lesMaalinger(indikator,medium="VS") #modified ND -> added medium="VS"

## HS

{
  D <- DATA[1:12, ]
  for (i in 1:ncol(D)) D[, i] <- substr(D[, i], 1, 10)
  for (i in c(3, 4)) D[, i] <- D[, i] %+% "..."
  print(head(D))
  rm(D)
}

## HS

VL <- lesVannlokaliteter(vannkategori="C",API = TRUE) # modified ND -> added vannkategori="C"
#VL <- readRDS("VL_C_save.rds") # added ND saved copy of VL 30.10.2026 for coastal water bodies

## HS

head(VL)


## HS

V <- lesVannforekomster(vannkategori="C") # modified ND -> added vannkategori="C" 
# modified ND -> added a code line in lesVannforkomster function to fix error in downloaded database 

## HS

head(V)

## HS

## skipping the lake adjustement code

## HS modified ND

{
  xaxis <- seq(0, 30, 5)
  dig <- 0
  snudd <- TRUE
  tittel <- "Secchi depth" #added ND
  
  ml <- V[which(V$id %in% 
                  VL$id[which(VL$lokid %in% 
                                DATA$lokid[which(DATA$verdi %mellom% range(xaxis))])]), ]
  ml$maaling <- NA
  for (i in 1:nrow(ml)) {
    ml$maaling[i] <- mean(DATA$verdi[which(DATA$lokid %in% 
                                                  VL$lokid[which(VL$id == ml$id[i])])])
  }
  z <- 2
  fjern <- c()
  while (length(which(ml$maaling < xaxis[z])) == 0) {
    fjern <- c(fjern, z - 1)
    z <- z + 1
  }
  z <- length(xaxis) - 1
  while (length(which(ml$maaling > xaxis[z])) == 0) {
    fjern <- c(fjern, z + 1)
    z <- z - 1
  }
  if (length(fjern)) xaxis <- xaxis[-fjern]
  if (snudd) xaxis <- rev(xaxis)
  xlim <- range(xaxis)
  ml$maaling <- sapply(sapply(ml$maaling, max, xlim[1]), min, xlim[2])
  ml <- ml[order(ml$maaling, decreasing = !snudd), ]
  
  load("../data/Norge.map")
  par(mai = rep(0, 4))
  plot(Norge.kontur, xlim = c(3.5, 31.5), ylim = c(57.5, 71.5), 
       xaxs = "i", yaxs = "i", asp = 2, col = grey(0.84), border = NA)
  points(ml$long, ml$lat, pch = 20, cex = 0.6, col = if (snudd)
    farve(ml$maaling, xlim[2], xlim[1]) else 
      farve(ml$maaling, xlim[1], xlim[2]))
  text(6, 70, tittel, cex = 1.5, font = 2)
  for (i in seq(0, 0.999, 0.001)) {
    rect(24, 59+i*8, 26, 59+(i+0.001)*8, border = NA,
         col = if (snudd) farve(i, 1, 0) else farve(i, 0, 1))
  }
  rect(24, 59, 26, 67, lwd = 2)
  for (i in xaxis) {
    lines(c(24.0,23.6), rep(59 + 8 * (i-xlim[1]) / (xlim[2]-xlim[1]), 2), lwd=2)
  }
  text(23.6, 59 + 8 * (xaxis - xlim[1]) / (xlim[2] - xlim[1]), 
       erstatt(formatC((xaxis), dig, format = "f"), "-", "−"), pos = 2)
  if (length(unique(DATA$enhet)) %=% 1 & 
      unique(DATA$enhet) %!=% "3" & exists("Enheter")) {
    text(25, 58.5, Enheter[which(Enheter[, 1] == unique(DATA$enhet)), 2])
  }
}

## HS

klassegrenser <- hentKlassegrenser("../data/klassegrenser_" %+% indikator %+% ".xlsx")
if (nrow(klassegrenser) > 1) {
  m <- matrix(0, 8, 2, dimnames = list(
    c("Pessimum:", "Range of X_" %+% seq(0, 100, 20) %+% ":", "Optimum:"), 
    c("from", "to")))
  m[, 1] <- apply(klassegrenser, 2, min, na.rm = TRUE)
  m[, 2] <- apply(klassegrenser, 2, max, na.rm = TRUE)
  klassegrenser <- klassegrenser[which(!is.na(klassegrenser[, 1]))[1], ]
} else {
  m <- t(klassegrenser)
  dimnames(m) <- list(c("Pessim.", "X_0   =", "X_20  =", "X_40  =", "X_60  =",
                        "X_80  =", "X_100 =", "Optimum"), "")
}
print(m)

## HS

xlim <- c(0, 30)
xaxis <- seq(0, 30, 2.5)
yaxis <- seq(-0.2, 1.2, 0.2)
x <- seq(0, 100, le = 1000)
y <- nEQR(x, klassegrenser)
forklar <- "Original variable values"
if (unique(DATA$enhet) %!=% "3" & exists("Enheter")) {
  forklar <- forklar %+% " (" %+% 
    Enheter[which(Enheter[, 1] == unique(DATA$enhet)), 2] %+% ")"
}
par(mai = c(0.96, 0.84, 0.84, 0.12))
plot(x, y, ty = "l", main = tittel,
     xlim = xlim,         xaxs = "r", xaxt = "n", xlab = forklar,
     ylim = c(-0.2, 1.2), yaxs = "r", yaxt = "n", ylab = "nEQR values",
     cex.main = 1.5, cex.lab = 1.2)
rect(xlim[1]-144,-0.4, xlim[2]+144, 0.0, col = rgb(0.0, 0.0, 0.0, 0.1), border = NA)
rect(xlim[1]-144, 0.0, xlim[2]+144, 0.2, col = rgb(1.0, 0.0, 0.0, 0.2), border = NA)
rect(xlim[1]-144, 0.2, xlim[2]+144, 0.4, col = rgb(1.0, 0.5, 0.0, 0.2), border = NA)
rect(xlim[1]-144, 0.4, xlim[2]+144, 0.6, col = rgb(1.0, 1.0, 0.0, 0.2), border = NA)
rect(xlim[1]-144, 0.6, xlim[2]+144, 0.8, col = rgb(0.0, 1.0, 0.0, 0.2), border = NA)
rect(xlim[1]-144, 0.8, xlim[2]+144, 1.0, col = rgb(0.0, 0.0, 1.0, 0.2), border = NA)
rect(xlim[1]-144, 1.0, xlim[2]+144, 1.4, col = rgb(0.5, 0.0, 1.0, 0.1), border = NA)
lines(c(xlim[1] - 144,    klassegrenser[7]), c(+1, 1), lwd = 1.5, col = "blue")
lines(c(klassegrenser[7], klassegrenser[7]), c(-1, 1), lwd = 1.5, col = "blue")
lines(c(xlim[1] - 144,    xlim[2] + 144),    c(+0, 0), lwd = 1.5, col = "red")
#lines(c(klassegrenser[2], klassegrenser[2]), c(-1, 0), lwd = 1.5, col = "red")
lines(x, y, lwd = 3)
axis(1, xaxis, erstatt(xaxis, "-", "−"), cex.axis = 0.9)
axis(2, yaxis, c("−0.2", "0.0", 0.2, 0.4, 0.6, 0.8, "1.0", 1.2), cex.axis = 0.9)
text(xlim[2], seq(0.1, 0.9, 0.2), c("bad", "poor", "moderate", "good", "high"), 
     pos = 2, offset = 0)
rf <- klassegrenser[7] > klassegrenser[2]
text(klassegrenser[2], -0.15, expression(X[0]),   pos=ifelse(rf, 4, 2), offset=0.3)
text(klassegrenser[7], -0.15, expression(X[100]), pos=ifelse(rf, 2, 4), offset=0.3)

############################## added ND ########################################
## we need to pre select right data in DATA in order to calculate the right neQR for Secchi depth according to the instructions from Miljødirektoratet
## Code was generated with help of Microsoft Co-Pilot and tested by ND

dat <- DATA

## Prepare data

dat$dato <- as.Date(dat$tidpkt)
dat$year <- as.numeric(format(dat$dato, "%Y"))
dat$month <- as.numeric(format(dat$dato, "%m"))

dat$odyp <- as.numeric(dat$odyp)
dat$ndyp <- as.numeric(dat$ndyp)

## Add region ("reg") and water area ("id")

VLsave <- VL

VL$reg <- V$reg[match(VL$id, V$id)]

dat$reg <- VL$reg[match(dat$lokid, VL$lokid)]
dat$id  <- VL$id [match(dat$lokid, VL$lokid)]

## STEP 1: Filter valid secchi observations

secc <- dat[
  dat$operator == "=" & # enforcing the rule to keep all measurement that "=" equals to. (Inspection of data frame confirm that < and > cannot be trusted for an accurate value) 
    !is.na(dat$verdi) &
    !is.na(dat$reg) &
    dat$month %in% 6:8,]

## STEP 2a: Average secchi by location and date

secc_mean <- aggregate(
  verdi ~ id + lokid + dato + reg + year + month,
  data = secc,
  FUN = mean
)

secc_depth <- aggregate(
  ndyp ~ id + lokid + dato + reg + year + month,
  data = secc,
  FUN = function(x) c(
    ndyp_max = max(x),
    n_depths = length(x))
)

secc_depth$antall_dyp <- secc_depth$ndyp[, "n_depths"]
secc_depth$ndyp <- secc_depth$ndyp[, "ndyp_max"]

secc_summary <- merge(
  secc_mean,
  secc_depth,
  by = c("id", "lokid", "dato", "reg", "year", "month")
)

## STEP 2b: Keep area-years with near-complete seasonal coverage

month_check <- aggregate(
  month ~ lokid + reg + year,
  data = secc_summary,
  FUN = function(x) sort(unique(x))
)

obs_check <- aggregate(
  month ~ lokid + reg + year,
  data = secc_summary,
  FUN = length
)

names(obs_check)[4] <- "nobs"

month_check$nobs <- obs_check$nobs

# month_check$valid <- sapply(  # function to keep years with minimum 2 monthly measurements (not required by Miljødirektoratet)
#   seq_len(nrow(month_check)),
#   function(i) {
#     
#     r <- month_check$reg[i]
#     months <- month_check$month[[i]]
#     nobs <- month_check$nobs[i]
#     
#     ## Define required months by region
#     
#     if (r %in% c("H", "G", "B")) {
#       required <- 6:8
#     } else if (r %in% c("S", "N", "M")) {
#       required <- 6:8
#     } else {
#       return(FALSE)
#     }
# 
#     missing_months <- setdiff(required, months)
# 
#     ## complete coverage
#     if (length(missing_months) == 0)
#       return(TRUE)
#     
#     if (length(missing_months) == 1)
#       return(TRUE)
#     
#     # if (length(missing_months) == 2)
#     #   return(TRUE)
# 
#     # ## allow one missing month
#     # if (length(missing_months) == 1) {
#     # 
#     #   missing_month <- missing_months
#     # 
#     #   return(
#     #     missing_month != min(required) &&
#     #       missing_month != max(required) &&
#     #       nobs >= length(required)
#     #   )
#     #   
#     # }
#     
#     ## More than one month missing    
#     FALSE
#   }
# )


month_check$valid <- sapply(  # function for keeping any year with at leat one measurement.
  month_check$month,
  function(months) any(months %in% 6:8)
)

## Keep only valid area-years
valid_loc_year <- month_check[
  month_check$valid,
  c("lokid", "year")
]

## select three consecutive years 2022 - 2024
complete_lokid <- aggregate(
  year ~ lokid,
  data = valid_loc_year,
  FUN = function(x) {
#     all(c(account_year-2, account_year-1, account_year) %in% unique(x)) # if we want to select the three consecutive last years
    sum(unique(x) %in% (account_year-2):account_year) >= 2  # if we want to select only water bodies with 2 years in the last three
  }
)

complete_lokid <- complete_lokid[
  complete_lokid$year,
  "lokid"
]

valid_loc_year <- valid_loc_year[
  valid_loc_year$lokid %in% complete_lokid,
]

## Keep all observations from valid area-years
secc_summary <- merge(
  secc_summary,
  valid_loc_year,
  by = c("lokid", "year")
)

## STEP 3: Reassemble metadata

secc_2224 <- secc[
  secc$year >= account_year-2 &
    secc$year <= account_year,
]

metadata <- secc_2224[
  
  !duplicated(paste(secc_2224$lokid, secc_2224$dato)),
  
]


## Merge depth averaged secchi by date and locations values back selected for 2022 to 2024
keep <- merge(
  metadata,
  secc_summary,
  by = c("lokid", "dato")
)

## Replace original secchi value with aggregated value
keep$verdi <- keep$verdi.y

## Keep original upper depth from metadata
# keep$odyp <- keep$odyp.x

## Keep deepest depth from secc_summary
keep$ndyp <- keep$ndyp.y

## Remove duplicate columns created by merge
keep <- keep[
  ,
  !names(keep) %in% c(
    "verdi.x",
    "verdi.y",
    # "odyp.x",
    # "odyp.y",
    "ndyp.x",
    "ndyp.y",
    "year.y",
    "month.y",
    "reg.y",
    "id.y"
  )
]

## Rename retained metadata columns
names(keep)[names(keep) == "year.x"] <- "year"
names(keep)[names(keep) == "month.x"] <- "month"
names(keep)[names(keep) == "reg.x"] <- "reg"
names(keep)[names(keep) == "id.x"] <- "id"

## STEP 4: Calculate monthly location means

locality_month <- aggregate(
  verdi ~ id + lokid + reg + year + month,
  data = keep,
  FUN = mean
)

## STEP 5: Calculate station-specific mean (2022-2024)

#keep_2224 <- keep[locality_month$year >= 2022 & locality_month$year <= 2024,]

SECCI_mean <- aggregate(
  verdi ~ id + lokid,
  data = locality_month,
  mean
)

names(SECCI_mean)[3] <- "verdi_mean"


## Keep one metadata record per station
meta_station <- keep[!duplicated(keep$lokid),]

## Merge metadata and p90 values
summary_mean <- merge(
  meta_station,
  SECCI_mean,
  by = c("id","lokid")
)

## Replace original chlorophyll value with aggregated value
summary_mean$verdi <- summary_mean$verdi_mean

## Remove duplicate columns created by merge
summary_mean <- summary_mean[,!names(summary_mean)=="verdi_mean"]

## Result

head(summary_mean)


## HS  modified ND

utmating <- WFD2ECA_ND(
  DATA = summary_mean,
  vannforekomster = V,
  vannlokaliteter = VL,
  rapportenhet = c("norge"), ## added ND
  rapportaar = c(account_year), ## added ND
  utEQR=T,
  parameter = indikator,
  vannkategori = vannkategori,
  utMaaling = TRUE,
  iterasjoner = 10000, ## added ND we increase simulation to 10000 (from 1000)
  SEED = 479001600,
  maalingPer = 0, ## added ND modifed from 50 -> 0; forces the model to be calculated
  maalingTyp = 0, ## added ND modifed from 50 -> 0; forces the model to be calculated
  maalingTot = 0, ## added ND modifed from 200 -> 0; forces the model to be calculated
  vis = TRUE,
  fraMaaned=6, 
  tilMaaned=8,
  EQR="nEQR"
)

## HS

{
  ut <- utmating
  attributes(ut) <- attributes(ut)[1:3]
  # to simplify the output, most attributes are removed
  str(ut)
  rm(ut)
}

## HS

{
  cat("Range of values prior to truncation (nEQR): " %+% 
        format(round(min(utmating$Norge, na.rm=TRUE), 4), nsm=4, sci=FALSE) %+%
        " to " %+%
        format(round(max(utmating$Norge, na.rm=TRUE), 4), nsm=4, sci=FALSE) %+% "\n")
  tomrom <- ifelse(nchar(format(round(min(utmating$Norge, na.rm=TRUE), 4), 
                                nsm=4, sci=FALSE)) %=% 7, " ", "")
  utmating <- trunker(utmating)
  cat("Range of values _after_  truncation (nEQR): " %+% tomrom %+%
        format(round(min(utmating$Norge, na.rm=TRUE), 4), nsm=4, sci=FALSE) %+%
        " to " %+%
        format(round(max(utmating$Norge, na.rm=TRUE), 4), nsm=4, sci=FALSE) %+% "\n")
}

## HS

{
  kvantiler <- c(0.025, 0.25, 0.5, 0.75, 0.975)
  print(round(quantile(utmating$Norge[  "Norge", , -1],probs=kvantiler),4))
  print(paste("mean nEQR value:", round(mean(utmating$Norge[  "Norge", , -1]),2),sep=" ")) ## ADDED ND
  print(paste("sd nEQR value:", round(sd(utmating$Norge[  "Norge", , -1]),2),sep=" ")) ## ADDED ND
  #  print(round(t(apply(utmating$Norge[  "Norge", , -1], 1, quantile, kvantiler)), 4))
  #  print(round(t(apply(utmating$landsdel[, "2024", -1], 1, quantile, kvantiler)), 4)) # ADDED ND we only want NORGE
}

