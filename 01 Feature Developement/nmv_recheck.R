StartDate <- as.Date('2022-06-01')
EndDate <- as.Date('2022-06-30')

source('get_period_data.R')

Check <- TotalSummary %>%
  filter(TikingonCateReport == 'FRESH',
         CustomerGroupRegion == 'SOUTH') %>%
  # group_by(Cate2) %>%
  summarise(Nmv = sum(SkuNmv, na.rm = TRUE),
            ProductValue = sum(SkuValue, na.rm = TRUE),
            Cogs = sum(Cogs*NmvQty, na.rm = TRUE),
            FM = ProductValue - Cogs,
            FM_percent_PV = FM/ProductValue,
            FM_percent_NMV = FM/Nmv)
