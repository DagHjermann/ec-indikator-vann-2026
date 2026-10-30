### lesVannlokaliteter
# Funksjoner til WFD2ECA
# ved Hanno Sandvik
# oktober 2025
# se https://github.com/NINAnor/WFD2ECA
###



lesVannlokaliteter <- function(vannkategori = c("L", "R"),
                               filsti = "../data",
                               kolonnenavn = "navnVL.csv",
                               API = TRUE,
                               ...) {
  
  # Funksjonen leser inn vannlokaliteter fra vannmiljø-databasen
  
  # Kolonner som datarammen VL trenger for å fungere:
  nyeKolonner <- c(
    "lokid",
    "lokkod",
    "loknam",
    "sjonr",
    "id",
    "kat",
    "X",
    "Y"
  )
  nodvendig <- c(1, 2, 5, 6)
  
  OK <- TRUE
  vannkategori <- toupper(vannkategori) %A% c("L", "R", "C")
  if (length(vannkategori) %=% 0) {
    OK <- FALSE
    skriv("Parameteren \"vannkategori\" må være minst én av bokstavene \"L\", ",
          "\"R\" og \"C\"!", pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
  }
  
  # Innlesing av "tolkningstabellen": 
  # Hvilke kolonner i vannlokalitetstabellen svarer til hvilke kolonner i VL
  if (OK) {
    if (nchar(filsti)) {
      if (substr(filsti, nchar(filsti), nchar(filsti)) %!=% "/" &
          substr(filsti, nchar(filsti), nchar(filsti)) %!=% "\\") {
        filsti <- filsti %+% "/"
      }
    }
    navnVL <- try(read.table(filsti %+% kolonnenavn, 
                             header = TRUE, sep = ";", quote = "", 
                             na.strings = "", strip.white = TRUE, comment.char = "", 
                             stringsAsFactors = FALSE, fileEncoding="latin1"))
    if (inherits(navnVL, "try-error")) {
      OK <- FALSE
      skriv("Dette skjedde en feil under innlesing av fila \"", filsti %+%
              kolonnenavn, ". Sjekk om fila fins, at det er oppgitt korrekt ",
            "navn på den, og at den er formatert som semikolondelt tabell.",
            pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
    }
  }
  if (API) {
    
    # Innlesing av vannlokaliteter via API
    navnVL <- navnVL[, c("api", "nytt")]
    VL <- list()
    kontroll <- list(...)
    baseURL  <- kontroll$baseURL
    ENDpoint <- kontroll$ENDpoint
    APIkey   <- kontroll$APIkey
    if (is.null(ENDpoint)) ENDpoint <- "/GetWaterLocations"
    if (is.null(APIkey))   APIkey   <- "4!_55ddgfde905+_!24!;vv"
    if (is.null(baseURL))  baseURL  <- 
      "https://vannmiljoapi.miljodirektoratet.no/api/Public"
    URL <- baseURL %+% ENDpoint
    headers = c("Content-Type" = "application/json; charset=UTF-8",
                "vannmiljoWebAPIKey" = APIkey)
    body <- 
      '{"BoundingBox":{"xmin":-120000,"ymin":6000000,"xmax":1200000,"ymax":8000000}}'
    respons <- POST(URL, add_headers(.headers = headers), body = body)
    if (status_code(respons) != 200) {
      OK <- FALSE
      skriv("Det lyktes ikke å hente data fra vannmiljø-databasen. ",
            "Statuskoden var ", status_code(respons), ".",
            pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
    } else {
      JSONdata <- fromJSON(content(respons, "text"), flatten = TRUE)
      VL   <- as.data.frame(JSONdata)
      VL[] <- lapply(VL, as.character)
      VL   <- VL[VL$Result.WaterCategory %in% vannkategori, ]
      if (nrow(VL) < 1) {
        OK <- FALSE
        skriv("Datasettet var tomt, uvisst av hvilken grunn.",
              pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
      }
    }
  } else {
    
    # Innlesing av vannlokaliteter fra fil
    navnVL <- navnVL[, c("eksport", "nytt")]
    VL <- list()
    for (i in vannkategori) {
      VL[[i]] <- try(as.data.frame(read_xlsx(filsti %+% "VL-" %+% i %+% ".xlsx", 
                                             col_types = "text")))
      if (inherits(VL[[i]], "try-error")) {
        OK <- FALSE
        skriv("Dette skjedde en feil under innlesing av fila \"", filsti,
              "VL-", i, ".xlsx", ". Sjekk om fila fins, og at det er oppgitt ",
              "korrekt navn på den.",
              pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
      }
    }
    if (OK) {
      VL. <- VL[[1]]
      if (length(vannkategori) > 1) {
        for (k in 2:length(vannkategori)) {
          VL. <- rbind(VL., VL[[k]])
        }
      }
      VL <- VL.
    }
  }

  # Så "oversettes" kolonnenavnene
  if (OK) {
    if (all(colnames(VL)  %in% navnVL[, 1])) {
      if (all(nyeKolonner %in% navnVL[, 2])) {
        for (i in 1:ncol(VL)) {
          w <- which(navnVL[, 1] == colnames(VL)[i])
          if (length(w)) {
            colnames(VL)[i] <- navnVL$nytt[w]
          }
        }
        if (all(nyeKolonner[nodvendig] %in% colnames(VL))) {
          for (i in nyeKolonner %-% colnames(VL)) {
            VL[, i] <- NA
          }
          VL <- VL[, nyeKolonner]
        } else {
          OK <- FALSE
          skriv("Kolonnenavnene i den innleste datafila fra \"vannmiljø\" er ikke ",
                "som forventa!", pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
        }
      } else {
        OK <- FALSE
        skriv("Kolonnenavnene i \"", kolonnenavn, "\" er ikke som forventa!",
              pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
      }
    } else {
      OK <- FALSE
      skriv("Kolonnenavnene i den innleste datafila fra \"vannmiljø\" er ikke ",
            "som forventa!", pre = "FEIL: ", linjer.over = 1, linjer.under = 1)
    }
  }
  if (OK) {
    if (any(is.na(VL$lokid))) {
      w <- which(is.na(VL$lokid))
      VL$lokid[w] <- substr(VL$lokkod[w], 5, nchar(VL$lokkod[w]))
    }
    VL$lokid <- as.numeric(VL$lokid)
    VL$sjonr <- as.numeric(VL$sjonr)
    VL$X     <- as.numeric(VL$X)
    VL$Y     <- as.numeric(VL$Y)
    skriv("Innlesing av ", nrow(VL), " vannlokaliteter var vellykka.", 
          linjer.over = 1)
  }
  return(VL)
}

