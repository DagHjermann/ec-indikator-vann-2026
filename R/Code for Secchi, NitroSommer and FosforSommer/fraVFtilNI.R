### fraVFtilNI
# Funksjoner til WFD2ECA
# ved Hanno Sandvik
# november 2025
# se https://github.com/NINAnor/WFD2ECA
###



# Gjennomfører (mesteparten av) dataflyten fra vannforskriften til naturindeks
# (basert på funksjonen WFD2ECA, men med andre standardinnstillinger enn denne)
fraVFtilNI  <- function(
  DATA,
  vannforekomster,
  vannlokaliteter,
  parameter,
  vannkategori,
  filKlasser = NULL,
  rapportaar = c(1990, 2000, 2010, 2014, 2019, 2024),
  rapportenhet = c("kommune", "fylke", "landsdel", "norge"),
  adminAar = 2010,
  kommHist = "data/knr.xlsx",
  fylkHist = "data/fnr.xlsx",
  paramFil = "data/VM-param.xlsx",
  aktivFil = "data/VM-aktiv.xlsx",
  rapportperiode = 10,
  vedMaalefeil = "dato",
  maksSkjevhet = 3,
  utMaaling = FALSE,
  utEQR = FALSE,
  utEstimat = FALSE,
  bareInkluder = NULL,
  ikkeInkluder = NULL,
  maalingPer = 25,
  maalingTot = 100,
  maalingTyp = 25,
  maalingInt = 25,
  EQR = "asymptotisk",
  ignorerVariabel = NULL,
  fastVariabel = NULL,
  trend = NA,
  anadrom = FALSE,
  aktVekting = TRUE,
  aktivitetsvekt = 2,
  antallvekt = 0.5,
  tidsvekt = 0.9,
  arealvekt = 2,
  logit = TRUE,
  DeltaAIC = 2,
  interaksjon = TRUE,
  ekstrapolering = "kjente",
  beggeEnder = TRUE,
  iterasjoner = 10000,
  lokalModell = TRUE,
  SEED = NULL,
  bredde = NULL,
  vis = TRUE,
  tell = TRUE,
  ...
) WFD2ECA (
    DATA            =            DATA,
    vannforekomster = vannforekomster,
    vannlokaliteter = vannlokaliteter,
    parameter       =       parameter,
    vannkategori    =    vannkategori,
    filKlasser      =      filKlasser,
    rapportaar      =      rapportaar,
    rapportenhet    =    rapportenhet,
    adminAar        =        adminAar,
    kommHist        =        kommHist,
    fylkHist        =        fylkHist,
    paramFil        =        paramFil,
    aktivFil        =        aktivFil,
    rapportperiode  =  rapportperiode,
    vedMaalefeil    =    vedMaalefeil,
    maksSkjevhet    =    maksSkjevhet,
    utMaaling       =       utMaaling,
    utEQR           =           utEQR,
    utEstimat       =       utEstimat,
    bareInkluder    =    bareInkluder,
    ikkeInkluder    =    ikkeInkluder,
    maalingPer      =      maalingPer,
    maalingTot      =      maalingTot,
    maalingTyp      =      maalingTyp,
    maalingInt      =      maalingInt,
    EQR             =             EQR,
    ignorerVariabel = ignorerVariabel,
    fastVariabel    =    fastVariabel,
    trend           =           trend,
    anadrom         =         anadrom,
    aktVekting      =      aktVekting,
    aktivitetsvekt  =  aktivitetsvekt,
    antallvekt      =      antallvekt,
    tidsvekt        =        tidsvekt,
    arealvekt       =       arealvekt,
    logit           =           logit,
    DeltaAIC        =        DeltaAIC,
    interaksjon     =     interaksjon,
    ekstrapolering  =  ekstrapolering,
    beggeEnder      =      beggeEnder,
    iterasjoner     =     iterasjoner,
    lokalModel      =     lokalModell,
    SEED            =            SEED,
    bredde          =          bredde,
    vis             =             vis,
    tell            =            tell,
    ...
)



# Oppdatering av et datasett som er lasta ned fra NI-basen
oppdaterNImedVF <- function(NIdata,   # datasett som er lasta ned fra NI-basen
                            nyeData,  # data som de forrige skal erstattes med
                            avrunding = 5,
                            enhet = "kommune",
                            kvartiler = FALSE) {
  
  NIbackup <- NIdata
  nyeData  <- round(nyeData[[enhet]], avrunding)
  feil     <- c()
  sistnavn <- ""
  if ("maalt" %in% names(attributes(nyeData))) {
    maalt <- attr(nyeData, "maalt")
    dimnames(maalt) <- dimnames(nyeData)[-3]
  } else {
    maalt <- matrix(F, dim(nyeData)[1], dim(nyeData)[2],
                    dimnames = dimnames(nyeData)[-3])
  }
  if (kvartiler) {
    kv <-  apply(nyeData[, , -1], 1:2, quantile, c(0.25, 0.50, 0.75), na.rm = TRUE)
  }

  for (i in 1:nrow(NIdata[[1]])) {
    aar <- NIdata[[1]]$yearName[i]
    if (sistnavn == "") {
      sistnavn <- NIdata[[1]]$areaName[i]
    }
    if (NIdata[[1]]$areaName[i] %in% dimnames(nyeData)$kommune) { # kommune kjent?
      if (NIdata[[1]]$yearId[i]) { # er det en årsverdi (ikke referanseverdi)?
        if (aar %in% dimnames(nyeData)$aar) { # fins det data for dette året?
          if (any(is.na(nyeData[NIdata[[1]]$areaName[i], aar, -1]))) {
            feil <- c(feil, "Det manglet verdier for " %+% 
                        NIdata[[1]]$areaName[i] %+%
                        " i " %+% aar %+% "!\n")
            NIdata <- NIcalc::setIndicatorValues(NIdata,
                                                 areaId = NIdata[[1]]$areaId[i],
                                                 years = as.numeric(aar),
                                                 est = NA, lower = NA, upper = NA,
                                                 datatype = NA)
          } else { # kun kjente dataverdier
            w <- which(nyeData[NIdata[[1]]$areaName[i], aar, ] < 0)
            if (length(w)) {
              # negative verdier settes til null!
              nyeData[NIdata[[1]]$areaName[i], aar, w] <- 0
            }
            # er den spesifikke verdien basert på målinger eller modellering?
            typ <- if (maalt[NIdata[[1]]$areaName[i], aar]) 2 else 3
            # oppdatering med de simulerte dataene:
            if (kvartiler) {
              NIdata <- NIcalc::setIndicatorValues(
                NIdata,
                areaId = NIdata[[1]]$areaId[i],
                years = as.numeric(aar),
                est   = kv[2, NIdata[[1]]$areaName[i], aar],
                lower = kv[1, NIdata[[1]]$areaName[i], aar], 
                upper = kv[3, NIdata[[1]]$areaName[i], aar],
                datatype = typ,
                unitOfMeasurement = "nEQR"
              )
            } else {
              NIdata <- NIcalc::setIndicatorValues(
                NIdata,
                areaId = NIdata[[1]]$areaId[i],
                years = as.numeric(aar),
                distribution = NIcalc::makeDistribution(
                  nyeData[NIdata[[1]]$areaName[i], aar, -1]
                ),
                datatype = typ,
                unitOfMeasurement = "nEQR"
              )
            }
          }
        } else { # feil årstall
          NIdata <- NIcalc::setIndicatorValues(NIdata,
                                               areaId = NIdata[[1]]$areaId[i],
                                               years = as.numeric(aar),
                                               est = NA, lower = NA, upper = NA,
                                               datatype = NA)
        }
      } else { # referanseverdi
        NIdata <- NIcalc::setIndicatorValues(NIdata,
                                             areaId = NIdata[[1]]$areaId[i],
                                             years = "Referanseverdi",
                                             est = 1, lower = 1, upper = 1,
                                             datatype = 1,
                                             unitOfMeasurement = "nEQR")
      }
    } else { # ukjent kommune!
      feil <- c(feil, "Arealnavnet \"" %+% NIdata[[1]]$areaName[i] %+% 
                  "\" ble ikke funnet i datafila!\n")
    }
    if (i == nrow(NIdata[[1]]) || NIdata[[1]]$areaName[i + 1] != sistnavn) {
      cat(floor(i * 100 / nrow(NIdata[[1]])) %+% 
            " % er unnagjort (sist: " %+% sistnavn %+% 
            ").                        \r")
      sistnavn <- NIdata[[1]]$areaName[i+1]
    }
  } # i
  cat("\n")
  
  if (length(feil)) {
    feil <- unique(feil)
    skriv("Følgende feilmelding", ifelse(length(feil) > 1, "er", ""), 
          "ble samla opp underveis:", linjer.over = 1)
    for (i in 1:length(feil)) {
      skriv(feil[i], pre = "* ")
    }
    cat("\n")
  } else {
    skriv("Det hele skjedde uten feilmeldinger.", linjer.under = 1)
  }
  
  # Nå kjøres noen tester om resultatet er som forventa
  OK <- TRUE
  txt <- "Variabelklassen er "
  if (class(NIdata)      %!=% class(NIbackup)) {
    txt <- txt %+% "IKKE "
    OK <- FALSE
  }
  skriv(txt, "slik den skal være.")
  txt <- "Listeelementenes navn er "
  if (names(NIdata)      %!=% names(NIbackup)) {
    txt <- txt %+% "IKKE "
    OK <- FALSE
  }
  skriv(txt, "slik de skal være.")
  txt <- "Kolonnenavnene er "
  if (names(NIdata[[1]]) %!=% names(NIbackup[[1]])) {
    txt <- txt %+% "IKKE "
    OK <- FALSE
  }
  skriv(txt, "slik de skal være.")
  txt <- "De første seks kolonnene er "
  if (NIdata[[1]][, 1:6] %!=% NIbackup[[1]][, 1:6]) {
    txt <- txt %+% "IKKE "
    OK <- FALSE
  }
  skriv(txt, "slik de skal være.")
  if (OK) {
    skriv("Da er så langt alt OK. Noen flere tester følger ...")
  } else {
    skriv("\nDet er funnet avvik!\n\nNoen flere tester følger ...")
  }
  skriv("Nåværende  indikatorverdier i naturindeksbasen:")
  print(summary(NIbackup[[1]]$verdi))
  skriv("Oppdaterte indikatorverdier i naturindeksbasen:")
  print(summary(NIdata  [[1]]$verdi))
  skriv("Simulerte  indikatorverdier fra vannmiljø:")
  print(summary(as.vector(c(nyeData[, , 1], 
                            rep(1, length(unique(NIdata[[1]]$areaId)))))))
  for (i in 7:9) {
    skriv("Oppsummering av ", colnames(NIdata[[1]])[i], ":")
    print(summary(NIdata[[1]][, i], useNA = "always"))
  }
  for (i in 10:12) {
    skriv("Oppsummering av ", colnames(NIdata[[1]])[i], ":")
    print(table(NIdata[[1]][, i], useNA = "always"))
  }
  skriv("Til slutt sammenlignes tallverdiene for en tilfeldig stikkprøve av kommuner.")
  for (i in sample(unique(NIdata[[1]]$areaId), 10)) {
    knavn <- NIdata[[1]]$areaName[which(NIdata[[1]]$areaId   == i)][1]
    skriv(knavn, ":")
    J <- dimnames(nyeData)$aar
    m <- matrix(0, 2, length(J), dimnames = list(c("VF", "NI"), J))
    m[1, ] <- apply(nyeData[knavn, , ], 1, mean)
    for (j in J) {
      m[2, j] <- NIdata[[1]]$verdi[which(NIdata[[1]]$areaId   == i &
                                         NIdata[[1]]$yearName == j)]
    }
    print(round(m, 3))
  }
  
  invisible(NIdata)
}








