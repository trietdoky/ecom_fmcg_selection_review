TotalSummary <- TotalSummaryData %>%
  filter(OrderDate >= as.Date('2022-04-01'),
         OrderDate < as.Date('2022-07-01'),
         (NmvQty > 0) |
           (ShrinkageQty > 0) |
           (QtyInventorySalable > 0))


ShrinkageBySku <- TotalSummary %>%
  # filter(CustomerGroupRegion == ReviewRegion,
  #        TikingonCateReport %in% ReviewCateReport) %>%
  mutate(SellingValue = SkuValue,
         BuyingValue = Cogs*NmvQty,
         Month = month(OrderDate)) %>%
  mutate_if(is.numeric, na.fill, 0) %>%
  # CASH HOLDING COST
  group_by(Sku, ProductName, Cate2Id, Cate2, TikingonCateReport, Month) %>%
  summarize(SellingValue = sum(SellingValue),
            BuyingValue = sum(BuyingValue),
            ShrinkageValue = sum(ShrinkageValue),
            SkuNmv = sum(SkuNmv),
            CashHoldingCost = mean(QtyInventorySalable*Cogs*WACC)) %>%
  ungroup()

TotalShrinkage <- ShrinkageBySku %>%
  group_by(Month, TikingonCateReport) %>%
  summarise(ShrinkageValue = sum(ShrinkageValue)) %>%
  ungroup()

export(TotalShrinkage, file = 'Shrinkage456.xlsx')



ReviewPeriod <- "ShortTerm"

ReviewRegion <- "SOUTH"
ReviewCateReport <- c("FRESH")

StartDate <- as.Date(floor_date(Today, unit = "weeks") - days(7))
EndDate <- as.Date(floor_date(Today, unit = "weeks") - days(1))
source("get_period_data.R")

ShrinkageByWarehouse <- TotalSummary %>%
  filter(CustomerGroupRegion == ReviewRegion,
         TikingonCateReport %in% ReviewCateReport) %>%
  group_by(WarehouseName, Cate2) %>%
  summarise(Nmv = sum(SkuNmv, na.rm = T),
            ShrinkageValue = sum(ShrinkageValue, na.rm = T)) %>%
  ungroup()

ShrinkageAbsolute <- ShrinkageByWarehouse %>%
  select(Cate2, WarehouseName, ShrinkageValue) %>%
  spread(key = "WarehouseName", value = "ShrinkageValue") %>%
  mutate(tmp = rowSums(across(c(-Cate2)))) %>%
  arrange(desc(tmp)) %>%
  select(-tmp)

ShrinkagePercentage <- ShrinkageByWarehouse %>%
  group_by(WarehouseName) %>%
  mutate(TotalNmv = sum(Nmv)) %>%
  ungroup() %>%
  mutate(ShrinkagePercentage = ShrinkageValue/TotalNmv) %>%
  select(Cate2, WarehouseName, ShrinkagePercentage) %>%
  spread(key = "WarehouseName", value = "ShrinkagePercentage") %>%
  mutate(tmp = rowSums(across(c(-Cate2)))) %>%
  arrange(desc(tmp)) %>%
  select(-tmp)

FormatSummaryTable(ShrinkageAbsolute)
FormatSummaryTable(ShrinkagePercentage, percentageColNames = colnames(ShrinkagePercentage)[-1])
