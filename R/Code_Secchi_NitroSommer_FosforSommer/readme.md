R code fro calculation of chlorophyll a (KLFA), Secchi depth (SECCI), Nitrogen concentration in summer (NTOTs) and Phosphorus concentration in summer (PTOTs)





The code is based on the packaging by Hanno Sandsvik (2025)

https://github.com/NINAnor/WFD2ECA





**Environment**



The codes are located in the "Code" folder



The Data ( .RData files .xlsx files .csv files .map files) are located in the "data" folder



Folder indicator

|

|\_\_Code

|

|\_\_data



**Running the scripts**



By running the first section of the code (KLFA.R, SECCI.R, NTOTs.R, PTOTs.R) 



\# if(!file.exists("../data/VF.RData")) {

\#   download.file("https://zenodo.org/records/17702361/files/NINAnor/WFD2ECA-v2.1.zip",

\#                 "../data/WFD2ECA.zip")

\#   folder <- "../data/WFD2ECA"

\#   unzip("../data/WFD2ECA.zip", exdir = folder)

\#   while (length(list.files(folder)) == 1) {

\#     folder <- paste0(folder, "/", list.files(folder))

\#   }

\#   whichDataFile <- c(

\#     "fnr.xlsx",

\#     "knr.xlsx",

\#     "navnNVEl.csv",

\#     "navnVL.csv",

\#     "navnVM.csv",

\#     "navnVN.csv",

\#     "NVE.RData",

\#     "Norge.map",

\#     paste0("V-", vannkategori, ".csv"),

\#     "VF.RData",

\#     "VM-aktiv.xlsx",

\#     "VM-param.xlsx"

\#   )

\#   dummy <- file.copy(from = paste0(folder, "/data/", whichDataFile),

\#                      to   = "../data/",

\#                      overwrite = FALSE)

\#   dummy <- file.copy(from = paste0(folder,

\#                                    "/klassegr/klassegrenser\_",

\#                                    indikator, 

\#                                    ".xlsx"),

\#                      to   = "../data/",

\#                      overwrite = FALSE)

\#   dummy <- file.copy(from = list.files(paste0(folder, "/R/"), full.names = TRUE),

\#                      to   = ".",

\#                      overwrite = FALSE)

\#   dummy <- file.remove("../data/WFD2ECA.zip")

\#   unlink("../data/WFD2ECA", recursive = TRUE)

\# }



from the Code folder, the above code will download the R files and data files in the Sandsvik repository and save them respectively the Code in the data folders. 



\--NOTE--



Some of the downloaded codes will need to be replaced by the given codes in this repository



* WFD2ECA\_ND.R
* mEQR.R
* lesVannforekomster.R



The code may also download klassegrenser excel file (excel file with the threshold value) but for Lakes water body, and should be replaced by the klassegrense file in this repository



* klassegrenser\_KLFA.xlsx
* klassegrenser\_SECCI.xlsx
* klassegrenser\_NTOTs.xlsx
* klassegrenser\_PTOTs.xlsx



If you want to run the script for another water type (river or coastal), you may run the above code once to download all files and then replace the klassegrenser file from lakes with the correct klassegrenser and the correct R files. The code should after be put as comments to not reinstall codes and overwrite the klassegrenser files each time.



\-- End of NOTE--



You may now run the KLFA.R / SECCI.R / NTOTs.R and PTOTs.R codes



* You can adapt the account year (last year of the three years period to evaluate the indicator value)

