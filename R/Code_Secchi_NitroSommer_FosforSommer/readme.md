R code fro calculation of chlorophyll a (KLFA), Secchi depth (SECCI), Nitrogen concentration in summer (NTOTs) and Phosphorus concentration in summer (PTOTs)





The code is based on the packaging by Hanno Sandsvik (2025)

https://github.com/NINAnor/WFD2ECA





**Environment**



The code is located in the folder `R/Code_Secchi_NitroSommer_FosforSommer`



The data (.RData, .xlsx, .csv and .map files) are located in the folder `data/data_Secchi_NitroSommer_FosforSommer`



```
ec-indikator-vann-2026
|
|__ R
|   |__ Code_Secchi_NitroSommer_FosforSommer
|
|__ data
    |__ data_Secchi_NitroSommer_FosforSommer
```

**Paths**

All paths are built from the project root with `here::here()`, through the two helper
functions defined in `paths_ND.R`:

* `kodesti(...)` points into `R/Code_Secchi_NitroSommer_FosforSommer`
* `datasti(...)` points into `data/data_Secchi_NitroSommer_FosforSommer`

`paths_ND.R` is sourced at the top of each master script, so the scripts can be run
from any working directory. The working directory no longer has to be set to the code
folder, and no relative `"../data"` paths are used.



**Running the scripts**



By running the first section of the code (KLFA.R, SECCI.R, NTOTs.R, PTOTs.R) 



\# if(!file.exists(datasti("VF.RData"))) {

\#   download.file("https://zenodo.org/records/17702361/files/NINAnor/WFD2ECA-v2.1.zip",

\#                 datasti("WFD2ECA.zip"))

\#   folder <- datasti("WFD2ECA")

\#   unzip(datasti("WFD2ECA.zip"), exdir = folder)

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

\#                      to   = datasti(),

\#                      overwrite = FALSE)

\#   dummy <- file.copy(from = paste0(folder,

\#                                    "/klassegr/klassegrenser\_",

\#                                    indikator, 

\#                                    ".xlsx"),

\#                      to   = datasti(),

\#                      overwrite = FALSE)

\#   dummy <- file.copy(from = list.files(paste0(folder, "/R/"), full.names = TRUE),

\#                      to   = kodesti(),

\#                      overwrite = FALSE)

\#   dummy <- file.remove(datasti("WFD2ECA.zip"))

\#   unlink(datasti("WFD2ECA"), recursive = TRUE)

\# }



the above code will download the R files and data files from the Sandvik repository and save them in the code and data folders respectively. 



\--NOTE--



Some of the downloaded codes will need to be replaced by the given codes in this repository



* WFD2ECA_ND.R
* mEQR.R
* lesVannforekomster.R
* lesMaalinger.R, lesVannlokaliteter.R, lesInnsjodatabasen.R, hentKlassegrenser.R (only the file paths, see **Paths** above)

Note that `paths_ND.R` is not part of the Sandvik package and must not be deleted.



The code may also download klassegrenser excel file (excel file with the threshold value) but for Lakes water body, and should be replaced by the klassegrense file in this repository



* klassegrenser\_KLFA.xlsx
* klassegrenser\_SECCI.xlsx
* klassegrenser\_NTOTs.xlsx
* klassegrenser\_PTOTs.xlsx



If you want to run the script for another water type (river or coastal), you may run the above code once to download all files and then replace the klassegrenser file from lakes with the correct klassegrenser and the correct R files. The code should after be put as comments to not reinstall codes and overwrite the klassegrenser files each time.



\-- End of NOTE--



**Master scripts**

KLFA.R, SECCI.R, NTOTs.R and PTOTs.R are the four master scripts - one per indicator.
Each one is run on its own (interactively, section by section) and loads all the other
.R files in the code folder as function definitions. The other .R files are not meant to
be run directly.

You may now run the KLFA.R / SECCI.R / NTOTs.R and PTOTs.R codes



* You can adapt the account year (last year of the three years period to evaluate the indicator value)

