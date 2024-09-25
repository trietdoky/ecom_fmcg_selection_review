### Identify the ABCD trend of Ngon Selection

# MAY
StartDate <- as.Date("2022-05-01")
EndDate <- as.Date("2022-05-31")
source("get_period_data.R")

May_PerformerRankAllCates <- TotalSummary %>%
  group_by(Sku, ProductName, TikingonCateReport, CustomerGroupRegion, Cate2, Cate3) %>%
  summarize(SkuNmv = sum(SkuNmv, na.rm = T),
            NmvQty = sum(NmvQty),
            AvgStock = ceiling(mean(QtyInventorySalable)),
            TotalCustomer = sum(TotalCustomer),
            ReturnCustomer = max(ReturnCustomer), # returning customer within 6 months
            ShrinkageQty = sum(ShrinkageQty),
            ShrinkageValue = sum(ShrinkageValue, na.rm = T),
            AvgSalesPeriod = SkuNmv/(1+as.numeric(difftime(EndDate,StartDate, units = "day"))),
            AvgRating = max(AvgRating),
            TotalApprovedReview = max(TotalApprovedReview),
            LtThreeStarsCount = min(LtThreeStarsCount),
            ShrinkageRate = ShrinkageQty/NmvQty,
            DoC = max(DoC)) %>%
  ungroup() %>%
  mutate(NmvContribution = SkuNmv/sum(SkuNmv, na.rm = T)) %>%
  ABCClassify(SkuNmv, abcRange = c(80, 95)) %>%
  mutate(abcclass = ifelse(NmvContribution == 0, 'D', abcclass)) %>%
  arrange(accpercentage) %>%
  mutate(Year = "2022",
         Month = "05",
         MonthYear = "May'22")

# JUNE
StartDate <- as.Date("2022-06-01")
EndDate <- as.Date("2022-06-30")
source("get_period_data.R")

Jun_PerformerRankAllCates <- TotalSummary %>%
  group_by(Sku, ProductName, TikingonCateReport, CustomerGroupRegion, Cate2, Cate3) %>%
  summarize(SkuNmv = sum(SkuNmv, na.rm = T),
            NmvQty = sum(NmvQty),
            AvgStock = ceiling(mean(QtyInventorySalable)),
            TotalCustomer = sum(TotalCustomer),
            ReturnCustomer = max(ReturnCustomer), # returning customer within 6 months
            ShrinkageQty = sum(ShrinkageQty),
            ShrinkageValue = sum(ShrinkageValue, na.rm = T),
            AvgSalesPeriod = SkuNmv/(1+as.numeric(difftime(EndDate,StartDate, units = "day"))),
            AvgRating = max(AvgRating),
            TotalApprovedReview = max(TotalApprovedReview),
            LtThreeStarsCount = min(LtThreeStarsCount),
            ShrinkageRate = ShrinkageQty/NmvQty,
            DoC = max(DoC)) %>%
  ungroup() %>%
  mutate(NmvContribution = SkuNmv/sum(SkuNmv, na.rm = T)) %>%
  ABCClassify(SkuNmv, abcRange = c(80, 95)) %>%
  mutate(abcclass = ifelse(NmvContribution == 0, 'D', abcclass)) %>%
  arrange(accpercentage) %>%
  mutate(Year = "2022",
         Month = "06",
         MonthYear = "Jun'22")


# JUL
StartDate <- as.Date("2022-07-01")
EndDate <- as.Date("2022-07-31")
source("get_period_data.R")

Jul_PerformerRankAllCates <- TotalSummary %>%
  group_by(Sku, ProductName, TikingonCateReport, CustomerGroupRegion, Cate2, Cate3) %>%
  summarize(SkuNmv = sum(SkuNmv, na.rm = T),
            NmvQty = sum(NmvQty),
            AvgStock = ceiling(mean(QtyInventorySalable)),
            TotalCustomer = sum(TotalCustomer),
            ReturnCustomer = max(ReturnCustomer), # returning customer within 6 months
            ShrinkageQty = sum(ShrinkageQty),
            ShrinkageValue = sum(ShrinkageValue, na.rm = T),
            AvgSalesPeriod = SkuNmv/(1+as.numeric(difftime(EndDate,StartDate, units = "day"))),
            AvgRating = max(AvgRating),
            TotalApprovedReview = max(TotalApprovedReview),
            LtThreeStarsCount = min(LtThreeStarsCount),
            ShrinkageRate = ShrinkageQty/NmvQty,
            DoC = max(DoC)) %>%
  ungroup() %>%
  mutate(NmvContribution = SkuNmv/sum(SkuNmv, na.rm = T)) %>%
  ABCClassify(SkuNmv, abcRange = c(80, 95)) %>%
  mutate(abcclass = ifelse(NmvContribution == 0, 'D', abcclass)) %>%
  arrange(accpercentage) %>%
  mutate(Year = "2022",
         Month = "07",
         MonthYear = "Jul'22")


# AUG MTD
StartDate <- as.Date("2022-08-01")
EndDate <- as.Date("2022-08-31")
source("get_period_data.R")

Aug_PerformerRankAllCates <- TotalSummary %>%
  group_by(Sku, ProductName, TikingonCateReport, CustomerGroupRegion, Cate2, Cate3) %>%
  summarize(SkuNmv = sum(SkuNmv, na.rm = T),
            NmvQty = sum(NmvQty),
            AvgStock = ceiling(mean(QtyInventorySalable)),
            TotalCustomer = sum(TotalCustomer),
            ReturnCustomer = max(ReturnCustomer), # returning customer within 6 months
            ShrinkageQty = sum(ShrinkageQty),
            ShrinkageValue = sum(ShrinkageValue, na.rm = T),
            AvgSalesPeriod = SkuNmv/(1+as.numeric(difftime(EndDate,StartDate, units = "day"))),
            AvgRating = max(AvgRating),
            TotalApprovedReview = max(TotalApprovedReview),
            LtThreeStarsCount = min(LtThreeStarsCount),
            ShrinkageRate = ShrinkageQty/NmvQty,
            DoC = max(DoC)) %>%
  ungroup() %>%
  mutate(NmvContribution = SkuNmv/sum(SkuNmv, na.rm = T)) %>%
  ABCClassify(SkuNmv, abcRange = c(80, 95)) %>%
  mutate(abcclass = ifelse(NmvContribution == 0, 'D', abcclass)) %>%
  arrange(accpercentage) %>%
  mutate(Year = "2022",
         Month = "08",
         MonthYear = "Aug'22")

# MERGE
merge <- May_PerformerRankAllCates %>%
  bind_rows(Jun_PerformerRankAllCates) %>%
  bind_rows(Jul_PerformerRankAllCates) %>%
  bind_rows(Aug_PerformerRankAllCates)

analysis <- merge %>%
  group_by(Year, Month, MonthYear, TikingonCateReport, CustomerGroupRegion) %>%
  summarise(TotalSku_byCateReport_byRegion = n_distinct(Sku),
            A_Count = n_distinct(Sku[abcclass == "A"]),
            B_Count = n_distinct(Sku[abcclass == "B"]),
            C_Count = n_distinct(Sku[abcclass == "C"]),
            D_Count = n_distinct(Sku[abcclass == "D"])) %>%
  ungroup() %>%
  arrange(CustomerGroupRegion, TikingonCateReport, Year, Month) %>%
  select(-Year, - Month) %>%
  filter(CustomerGroupRegion != "TRANSIT")

export(analysis, file = "delete_after_use_analysis_for_onsite.xlsx")
