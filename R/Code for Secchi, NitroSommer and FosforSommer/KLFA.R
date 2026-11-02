##### Modified Nicolas Dupont 30.10.2026 #####

##### Extract and calculate ecological condition og coastal waters #####
##### For chlorophyll a #####
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

indikator <- "KLFA" 
vannkategori <- "C"
account_year <- 2024

## HS

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
# VL <- readRDS("VL_C_save.rds") # added ND saved copy of VL 30.10.2026 for coastal water bodies

## HS

head(VL)


## HS

V <- lesVannforekomster(vannkategori="C") # modified ND -> added vannkategori="C" 
                                          # modified ND -> added a code line in lesVannforkomster to fix error in downloaded database 

## HS

head(V)

## HS

## skipping the lake adjustement code

## HS modified ND

{
  xaxis <- seq(0, 30, 5)
  dig <- 0
  snudd <- TRUE
  tittel <- "Chlorophyll a" #added ND
  
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
xaxis <- seq(0, 30, 5)
yaxis <- seq(-0.2, 1.2, 0.2)
x <- seq(0, 100, le = 1000)
y <- mEQR(x, klassegrenser)
forklar <- "Original variable values"
if (unique(DATA$enhet) %!=% "3" & exists("Enheter")) {
  forklar <- forklar %+% " (" %+% 
    Enheter[which(Enheter[, 1] == unique(DATA$enhet)), 2] %+% ")"
}
par(mai = c(0.96, 0.84, 0.84, 0.12))
plot(x, y, ty = "l", main = tittel,
     xlim = xlim,         xaxs = "r", xaxt = "n", xlab = forklar,
     ylim = c(-0.2, 1.2), yaxs = "r", yaxt = "n", ylab = "mEQR values",
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
## we need to pre select right data in DATA in order to calculate the right mEQR for chlorophyll a according to the instruction from Miljødirektoratet
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

## STEP 1: Filter valid chlorophyll observations

chl <- dat[
  !is.na(dat$odyp) &
    !is.na(dat$ndyp) &
    #    dat$odyp == dat$ndyp & #relaxing the rule about the sample depth has to be equal for both øverst and nederst
    dat$ndyp <= 10 &
    #    dat$operator != "<" & # relaxing the rule to remove all depth that are below a limit of detection. 
    !is.na(dat$verdi) &
    !is.na(dat$reg) &
    (
      (dat$reg %in% c("H", "G", "B") & dat$month %in% 3:9) |
        (dat$reg %in% c("S", "N", "M") & dat$month %in% 2:10)
    ),
]

## shifting "operator" to "="

chl$operator <- "="

## STEP 2a: Average chlorophyll by location and date

chl_mean <- aggregate(
  verdi ~ id + lokid + dato + reg + year + month,
  data = chl,
  FUN = mean
)

chl_depth <- aggregate(
  ndyp ~ id + lokid + dato + reg + year + month,
  data = chl,
  FUN = function(x) c(
    ndyp_max = max(x),
    n_depths = length(x))
)

chl_depth$antall_dyp <- chl_depth$ndyp[, "n_depths"]
chl_depth$ndyp <- chl_depth$ndyp[, "ndyp_max"]

chl_depth <- chl_depth[-which(chl_depth[,"ndyp"]<10 | chl_depth$antall_dyp<3),] # removing profile that are not at least 10 m deep or three depth profile

chl_summary <- merge(
  chl_mean,
  chl_depth,
  by = c("id", "lokid", "dato", "reg", "year", "month")
)

## STEP 2b: Keep location with near-complete seasonal coverage

month_check <- aggregate(
  month ~ lokid + reg + year,
  data = chl_summary,
  FUN = function(x) sort(unique(x))
)

obs_check <- aggregate(
  month ~ lokid + reg + year,
  data = chl_summary,
  FUN = length
)

names(obs_check)[4] <- "nobs"

month_check$nobs <- obs_check$nobs

month_check$valid <- sapply(
  seq_len(nrow(month_check)),
  function(i) {
    
    r <- month_check$reg[i]
    months <- month_check$month[[i]]
    nobs <- month_check$nobs[i]
    
    ## Define required months by region
    
    if (r %in% c("H", "G", "B")) {
      required <- 3:9
    } else if (r %in% c("S", "N", "M")) {
      required <- 2:10
    } else {
      return(FALSE)
    }
    
    missing_months <- setdiff(required, months)
    
    ## complete coverage
    if (length(missing_months) == 0)
      return(TRUE)
    
    ## allow one missing month
    if (length(missing_months) == 1) {
      
      missing_month <- missing_months
      
      return(
        missing_month != min(required) &&
          missing_month != max(required) &&
          nobs >= length(required)  # if removed allows for one montlhy measurement missing (except first and last month)
      )
      
    }
    
    ## More than one month missing    
    FALSE
  }
)


## Keep only valid location-years
valid_loc_year <- month_check[
  month_check$valid,
  c("lokid", "year")
]

## select three consecutive years 2022 - 2024
complete_lokid <- aggregate(
  year ~ lokid,
  data = valid_loc_year,
  FUN = function(x) {
    # all(c(account_year-2, account_year-1, account_year) %in% unique(x)) # if we want to select the three consecutive last years
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
chl_summary <- merge(
  chl_summary,
  valid_loc_year,
  by = c("lokid", "year")
)

## STEP 3: Reassemble metadata

chl_2224 <- chl[
  chl$year >= account_year-2 &
    chl$year <= account_year,
]

metadata <- chl_2224[
  
  !duplicated(paste(chl_2224$lokid, chl_2224$dato)),
  
]


## Merge depth averaged chlorophyll values back
keep <- merge(
  metadata,
  chl_summary,
  by = c("lokid", "dato")
)

## Replace original chlorophyll value with aggregated value
keep$verdi <- keep$verdi.y

## Keep original upper depth from metadata
keep$odyp <- keep$odyp.x

## Keep deepest depth from chl_summary
keep$ndyp <- keep$ndyp.y

## Remove duplicate columns created by merge
keep <- keep[
  ,
  !names(keep) %in% c(
    "verdi.x",
    "verdi.y",
    "odyp.x",
    "odyp.y",
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

## STEP 5: Calculate station-specific 90th percentile (2022-2024)

#keep_2224 <- keep[locality_month$year >= 2022 & locality_month$year <= 2024,]

p90 <- aggregate(
  verdi ~ id + lokid,
  data = locality_month,
  FUN = function(x)
    quantile(x, probs = 0.90, na.rm = TRUE)
)

names(p90)[3] <- "verdi_p90"


## Keep one metadata record per station
meta_station <- keep[!duplicated(keep$lokid),]

## Merge metadata and p90 values
summary90 <- merge(
  meta_station,
  p90,
  by = c("id","lokid")
)

## Replace original chlorophyll value with aggregated value
summary90$verdi <- summary90$verdi_p90

## Remove duplicate columns created by merge
summary90 <- summary90[,!names(summary90)=="verdi_p90"]

## Result

head(summary90)


################################################################################


## HS

utmating <- WFD2ECA_ND(
  DATA = summary90,
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
  maalingPer = 0, ## added ND modifed from 50 -> newvalue; forces the model to be calculated
  maalingTyp = 0, ## added ND modifed from 50 -> newvalue; forces the model to be calculated
  maalingTot = 0, ## added ND modifed from 200 -> newvalue; forces the model to be calculated
  vis = TRUE
  
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
  cat("Range of values prior to truncation (mEQR): " %+% 
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

## HS modified ND

{
  kvantiler <- c(0.025, 0.25, 0.5, 0.75, 0.975)
  print(round(quantile(utmating$Norge[  "Norge", , -1],probs=kvantiler),4))
  print(paste("mean nEQR value:", round(mean(utmating$Norge[  "Norge", , -1]),2),sep=" ")) ## ADDED ND
  print(paste("sd nEQR value:", round(sd(utmating$Norge[  "Norge", , -1]),2),sep=" ")) ## ADDED ND
#  print(round(t(apply(utmating$Norge[  "Norge", , -1], 1, quantile, kvantiler)), 4))
#  print(round(t(apply(utmating$landsdel[, "2024", -1], 1, quantile, kvantiler)), 4)) # ADDED ND we only want NORGE
}

