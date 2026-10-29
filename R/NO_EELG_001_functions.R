
# file specifications for Vannmiljø export file ---- 

vm_file_specifications <- cols(
  Vannlokalitet_kode = col_character(),
  Vannlokalitetsnavn = col_character(),
  Betegnelse = col_character(),
  Type = col_character(),
  Aktivitet_id = col_character(),
  Aktivitet_navn = col_character(),
  Oppdragsgiver = col_character(),
  Oppdragstaker = col_character(),
  Parameter_id = col_character(),
  Parameter_navn = col_character(),
  Parameter_casnr = col_logical(),
  Medium_id = col_character(),
  Medium_navn = col_character(),
  LatinskNavn_id = col_double(),
  VitenskapligNavn = col_character(),
  Provetakmetode_id = col_character(),
  Analysemetode_id = col_character(),
  Tid_provetak = col_date(format = ""),
  Ovre_dyp = col_double(),
  Nedre_dyp = col_double(),
  DybdeEnhet = col_character(),
  Filtrert_Prove = col_character(),
  UnntasKlassifisering = col_logical(),
  Operator = col_character(),
  Verdi = col_double(),
  Listenavn = col_character(),
  Enhet = col_character(),
  Provenr = col_logical(),
  Deteksjonsgrense = col_logical(),
  Kvantifiseringsgrense = col_logical(),
  Opprinnelse = col_character(),
  Ant_verdier = col_double(),
  Kommentar = col_character(),
  Arkiv = col_character(),
  Produktbeskrivelse = col_logical(),
  `UTM33 Ost (X)` = col_double(),
  `UTM33 Nord (Y)` = col_double(),
  year = col_double()
)

# index functions ----

get_depth_score <- function(depth, okoregion, vanntype) {
  
  case_when(
    
    # = = = = = 
    # Nordsjøen sør, type 2
    # = = = = = 
    
    okoregion == "Nordsjøen sør" &
      vanntype == 2 &
      depth > 8 ~ 5,
    
    okoregion == "Nordsjøen sør" &
      vanntype == 2 &
      depth > 6 ~ 4,
    
    okoregion == "Nordsjøen sør" &
      vanntype == 2 &
      depth > 5 ~ 3,
    
    okoregion == "Nordsjøen sør" &
      vanntype == 2 &
      depth > 3 ~ 2,
    
    okoregion == "Nordsjøen sør" &
      vanntype == 2 &
      depth > 2 ~ 1,
    
    okoregion == "Nordsjøen sør" &
      vanntype == 2 &
      depth >= 0 ~ 0,
    
    # = = = = = 
    # Nordsjøen sør, type 3
    # = = = = = 
    
    okoregion == "Nordsjøen sør" &
      vanntype == 3 &
      depth > 8 ~ 5,
    
    okoregion == "Nordsjøen sør" &
      vanntype == 3 &
      depth > 6 ~ 4,
    
    okoregion == "Nordsjøen sør" &
      vanntype == 3 &
      depth > 5 ~ 3,
    
    okoregion == "Nordsjøen sør" &
      vanntype == 3 &
      depth > 3 ~ 2,
    
    okoregion == "Nordsjøen sør" &
      vanntype == 3 &
      depth > 2 ~ 1,
    
    okoregion == "Nordsjøen sør" &
      vanntype == 3 &
      depth >= 0 ~ 0,
    
    # = = = = = 
    # Nordsjøen nord, type 3
    # = = = = = 
    
    okoregion == "Nordsjøen nord" &
      vanntype == 3 &
      depth > 7 ~ 5,
    
    okoregion == "Nordsjøen nord" &
      vanntype == 3 &
      depth > 6 ~ 4,
    
    okoregion == "Nordsjøen nord" &
      vanntype == 3 &
      depth > 4 ~ 3,
    
    okoregion == "Nordsjøen nord" &
      vanntype == 3 &
      depth > 3 ~ 2,
    
    okoregion == "Nordsjøen nord" &
      vanntype == 3 &
      depth > 1 ~ 1,
    
    okoregion == "Nordsjøen nord" &
      vanntype == 3 &
      depth >= 0 ~ 0,
    
    # = = = = = 
    # Norskehavet sør, type 3
    # NOTE: copied from Nordsjøen nord, as was done in report 
    # Økokyst Norskehavet sør 2024 (see table 5 in the report)
    # = = = = = 
    
    okoregion == "Norskehavet sør" &
      vanntype == 3 &
      depth > 7 ~ 5,
    
    okoregion == "Norskehavet sør" &
      vanntype == 3 &
      depth > 6 ~ 4,
    
    okoregion == "Norskehavet sør" &
      vanntype == 3 &
      depth > 4 ~ 3,
    
    okoregion == "Norskehavet sør" &
      vanntype == 3 &
      depth > 3 ~ 2,
    
    okoregion == "Norskehavet sør" &
      vanntype == 3 &
      depth > 1 ~ 1,
    
    okoregion == "Norskehavet sør" &
      vanntype == 3 &
      depth >= 0 ~ 0,
    
    TRUE ~ NA_real_
    
  )
}


if (FALSE){
  # test
  n <- 15
  eelgrass_wide$depth_limit[1:n]
  eelgrass_wide$okoregion[1:n]
  eelgrass_wide$vanntype[1:n]
  get_depth_score(
    eelgrass_wide$depth_limit[1:n],
    eelgrass_wide$okoregion[1:n],
    eelgrass_wide$vanntype[1:n]
  )
  
}


get_density_score <- function(x) {
  # 4 = tett eng
  # 3 = flekkvis tett eng
  # 2 = spredte planter
  # 1 = enkeltfunn
  case_when(
    x %in% 1:4 ~ x,
    TRUE ~ NA_real_
  )
  
}



get_epiphyte_score <- function(x) {
  # 1 = lite / ingen
  # 2 = spredt
  # 3 = vanlig
  # 4 = dominerende
  case_when(
    x %in% 0:1 ~ 4,
    x %in% 2 ~ 3,
    x %in% 3 ~ 2,
    x %in% 4 ~ 1,
    TRUE ~ NA_real_
  )
  
}


