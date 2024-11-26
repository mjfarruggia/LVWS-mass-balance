library(dataRetrieval)
library(tidyverse)
Q_LOCH_FULL <- readNWISdv(siteNumbers = lv_no, parameterCd = "00060",
                           startDate = '1983-10-01', endDate = '2024-09-30')

Q_LOCH_FULL <- renameNWISColumns(Q_LOCH_FULL)

Q_LOCH_FULL <- Q_LOCH_FULL %>%
  filter(Flow_cd=="A") %>% #Exclude provisional data
  rename(Q_cfs = Flow,
         date= Date) %>%
  mutate(Q_m3s = Q_cfs * 0.02831) %>%
  select(date, Q_cfs, Q_m3s)

write_csv(Q_LOCH_FULL, "data/export/LochO_19831001-20231009.csv")
