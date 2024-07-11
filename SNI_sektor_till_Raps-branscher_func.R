## Ladda in paket.
if (!require("tidyverse")) install.packages("tidyverse")

SNI_sektor_till_Raps <- function(df = stativ_tot) {
  # Funktion som lägger till en ny kolumn med Raps-branscher (3-siffrig kod) till en befintlig data frame (som måste innehålla kolumnerna 'AstSNI2007' och 'InstKod10').
  
  # Felhantering.
  if (!all(c("AstSNI2007", "InstKod10") %in% colnames(df))) {
    stop("Error: Data framen som matas in måste innehålla kolumnerna 'AstSNI2007' and 'InstKod10'.")
  }
  
  # Uppdaterar och returnerar en data frame.
  df <- df %>%
    mutate(rapsBranschKod = case_when( # lägg till kolumnen 'rapsBranschKod' vars värde fås utifrån vilket villkor som blivit uppfyllt nedan.
      substr(AstSNI2007,1,2)=='01' & substr(InstKod10,1,2) %in% c('11','12','14') ~ '001',
      substr(AstSNI2007,1,2)=='02' & substr(InstKod10,1,2) %in% c('11','12','14') ~ '002',
      substr(AstSNI2007,1,2)=='03' & substr(InstKod10,1,2) %in% c('11','12','14') ~ '003',
      substr(AstSNI2007,1,2) %in% c('05','06','07','08','09') & substr(InstKod10,1,2) %in% c('11','12','14') ~ '004',
      substr(AstSNI2007,1,2) %in% c('10','11','12') & substr(InstKod10,1,2) %in% c('11','12','14') ~ '005',
      substr(AstSNI2007,1,2) %in% c('13','14','15') & substr(InstKod10,1,2) %in% c('11','12','14') ~ '006',
      substr(AstSNI2007,1,3)=='161' & substr(InstKod10,1,2) %in% c('11','12','14') ~ '007',
      substr(AstSNI2007,1,3)=='162' & substr(InstKod10,1,2) %in% c('11','12','14') ~ '008',
      substr(AstSNI2007,1,3)=='171' & substr(InstKod10,1,2) %in% c('11','12','14') ~ '009',
      substr(AstSNI2007,1,3)=='172' & substr(InstKod10,1,2) %in% c('11','12','14') ~ '010',
      substr(AstSNI2007,1,2) %in% c('18','58') & substr(InstKod10,1,2) %in% c('11','12','14') ~ '011',
      substr(AstSNI2007,1,2)=='19' & substr(InstKod10,1,2) %in% c('11','12','14') ~ '012',
      substr(AstSNI2007,1,2)=='21' & substr(InstKod10,1,2) %in% c('11','12','14') ~ '013',
      substr(AstSNI2007,1,2)=='20' & substr(InstKod10,1,2) %in% c('11','12','14') ~ '014',
      substr(AstSNI2007,1,2)=='22' & substr(InstKod10,1,2) %in% c('11','12','14') ~ '015',
      substr(AstSNI2007,1,2)=='23' & substr(InstKod10,1,2) %in% c('11','12','14') ~ '016',
      substr(AstSNI2007,1,2)=='24' & substr(InstKod10,1,2) %in% c('11','12','14') ~ '017',
      substr(AstSNI2007,1,2)=='25' & substr(InstKod10,1,2) %in% c('11','12','14') ~ '018',
      substr(AstSNI2007,1,2)=='28' & substr(InstKod10,1,2) %in% c('11','12','14') ~ '019',
      substr(AstSNI2007,1,3) %in% c('261','262','263','264') & substr(InstKod10,1,2) %in% c('11','12','14')  ~ '020',
      substr(AstSNI2007,1,2)=='27' & substr(InstKod10,1,2) %in% c('11','12','14') ~ '021',
      substr(AstSNI2007,1,3) %in% c('265','266','267','268') & substr(InstKod10,1,2) %in% c('11','12','14') ~ '022',
      substr(AstSNI2007,1,2) %in% c('29','30') & substr(InstKod10,1,2) %in% c('11','12','14') ~ '023',
      substr(AstSNI2007,1,2) %in% c('31','32','33') & substr(InstKod10,1,2) %in% c('11','12','14') ~ '024',
      substr(AstSNI2007,1,2)=='35' & substr(InstKod10,1,2) %in% c('11','12','14') ~ '025',
      substr(AstSNI2007,1,2) %in% c('36','37','38','39') & substr(InstKod10,1,2) %in% c('11','12','14')  ~ '026',
      substr(AstSNI2007,1,2) %in% c('41','42','43') & substr(InstKod10,1,2) %in% c('11','12','14') ~ '027',
      substr(AstSNI2007,1,2)=='45' & substr(InstKod10,1,2) %in% c('11','12','14') ~ '028',
      substr(AstSNI2007,1,2) %in% c('46','47') & substr(InstKod10,1,2) %in% c('11','12','14') ~ '029',
      substr(AstSNI2007,1,2) %in% c('55','56') & substr(InstKod10,1,2) %in% c('11','12','14') ~ '030',
      substr(AstSNI2007,1,3) %in% c('491','492') & substr(InstKod10,1,2) %in% c('11','12','14') ~ '031',
      substr(AstSNI2007,1,3) %in% c('493','494','495') & substr(InstKod10,1,2) %in% c('11','12','14') ~ '032',
      substr(AstSNI2007,1,2)=='50' & substr(InstKod10,1,2) %in% c('11','12','14') ~ '033',
      substr(AstSNI2007,1,2)=='51' & substr(InstKod10,1,2) %in% c('11','12','14') ~ '034',
      substr(AstSNI2007,1,2)=='52' & substr(InstKod10,1,2) %in% c('11','12','14') ~ '035',
      substr(AstSNI2007,1,2)=='53' & substr(InstKod10,1,2) %in% c('11','12','14') ~ '036',
      substr(AstSNI2007,1,2)=='61' & substr(InstKod10,1,2) %in% c('11','12','14') ~ '037',
      substr(AstSNI2007,1,2) %in% c('64','65','66') & substr(InstKod10,1,2) %in% c('11','12','14') ~ '038',
      substr(AstSNI2007,1,3) %in% c('681','682') & substr(InstKod10,1,2) %in% c('11','12','14') ~ '039',
      AstSNI2007 %in% c('68310','68320')  & substr(InstKod10,1,2) %in% c('11','12','14') ~ '040',
      substr(AstSNI2007,1,2) %in% c('62','63','69','70','71','72','73','74','75','77','78','79','80','81','82') & substr(InstKod10,1,2) %in% c('11','12','14')  ~ '041',
      substr(AstSNI2007,1,2) %in% c('72') & (substr(InstKod10,1,4) %in% c('1311','1312','1314') | substr(InstKod10,1,1)=='2') ~ '041',
      substr(AstSNI2007,1,2) %in% c('85') & substr(InstKod10,1,2) %in% c('11','12','14') ~ '042',
      substr(AstSNI2007,1,2) %in% c('86','87','88') & substr(InstKod10,1,2) %in% c('11','12','14') ~ '043',
      substr(AstSNI2007,1,2) %in% c('59','60','90','91','92','93','94','95','96','97')  & substr(InstKod10,1,2) %in% c('11','12','14') ~ '044',
      substr(InstKod10,1,2)=='15' ~ '045',
      substr(AstSNI2007,1,2) %in% c('84') & substr(InstKod10,1,2) %in% c('13','21','22') ~ '046',
      ((substr(AstSNI2007,1,2) >= '01' & substr(AstSNI2007,1,2) <= '71') | (substr(AstSNI2007,1,2) >= '73' & substr(AstSNI2007,1,2) <= '83')) & substr(InstKod10,1,2) %in% c('13','21','22') ~ '047',
      substr(AstSNI2007,1,2)=='72' & substr(InstKod10,1,4) %in% c('1313') ~ '047',
      substr(AstSNI2007,1,2) %in% c('89','90','91','92','93','94','95','96','97') & substr(InstKod10,1,2) %in% c('13','21','22') ~ '047',
      substr(AstSNI2007,1,2) %in% c('85') & substr(InstKod10,1,2) %in% c('13','21','22') ~ '048',
      substr(AstSNI2007,1,2) %in% c('86','87','88') & substr(InstKod10,1,2) %in% c('13','21','22') ~ '049',
      substr(AstSNI2007,1,2) %in% c('98','99') ~ '099',
      TRUE ~ "999" # Övriga hamnar här.
    ))
}

