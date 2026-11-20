### paths_ND
# Stier til kode- og datamappe for Secchi/NTOTs/PTOTs/KLFA
# Lagt til 2026 i forbindelse med flytting av koden inn i ec-indikator-vann-2026.
#
# Den opprinnelige koden (Hanno Sandvik / Nicolas Dupont) forutsatte at
# working directory var kodemappa, og at data lå i "../data". I dette
# prosjektet ligger koden i R/Code_Secchi_NitroSommer_FosforSommer og data i
# data/data_Secchi_NitroSommer_FosforSommer. Funksjonene under bygger
# absolutte stier ut fra prosjektrota (her: mappa med .Rproj-fila), slik at
# skriptene kan kjøres uavhengig av working directory.
###


library(here)


# Mappa med kodefilene. kodesti() gir mappa, kodesti("Funksjon.R") en fil i den.
kodesti <- function(...) {
  here::here("R", "Code_Secchi_NitroSommer_FosforSommer", ...)
}


# Mappa med datafilene (klassegrenser_*.xlsx, VM-*.xlsx, V-C.csv, Norge.map,
# VF.RData, NVE.RData osv.). Brukes også som "filsti"-argument til lesMaalinger(),
# lesVannlokaliteter() og lesVannforekomster().
datasti <- function(...) {
  here::here("data", "data_Secchi_NitroSommer_FosforSommer", ...)
}
