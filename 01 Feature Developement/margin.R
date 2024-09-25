ReviewPeriod <- "MidTerm"

StartDate <- DatePeriod(ReviewPeriod)
EndDate <- Sys.Date() - days(1)
source("get_period_data.R")

SkuMargin <- TotalSummary %>%
  # filter(CustomerGroupRegion == ReviewRegion,
  #        TikingonCateReport %in% ReviewCateReport) %>%
  mutate(SellingValue = SkuValue,
         BuyingValue = Cogs*NmvQty) %>%
  mutate_if(is.numeric, na.fill, 0) %>%
  # CASH HOLDING COST
  group_by(Sku, ProductName, Cate2Id, Cate2, TikingonCateReport) %>%
  summarize(SellingValue = sum(SellingValue),
            BuyingValue = sum(BuyingValue),
            ShrinkageValue = sum(ShrinkageValue),
            SkuNmv = sum(SkuNmv),
            CashHoldingCost = mean(QtyInventorySalable*Cogs*WACC)) %>%
  ungroup()

TotalMargin <- SkuMargin %>%
  summarise(SellingValue = sum(SellingValue),
            BuyingValue = sum(BuyingValue),
            CashHoldingCost = sum(CashHoldingCost),
            ShrinkageValue = sum(ShrinkageValue),
            FrontMargin = (SellingValue - BuyingValue)/SellingValue,
            RefinedFrontMargin = (SellingValue - BuyingValue - CashHoldingCost - ShrinkageValue)/SellingValue,
            Nmv = sum(SkuNmv)) %>%
  mutate(TikingonCateReport = 'Total Ngon') %>%
  select(TikingonCateReport, Nmv, FrontMargin, RefinedFrontMargin)

NgonSubcateMargin <- SkuMargin %>%
  group_by(TikingonCateReport) %>%
  summarise(SellingValue = sum(SellingValue),
            BuyingValue = sum(BuyingValue),
            CashHoldingCost = sum(CashHoldingCost),
            ShrinkageValue = sum(ShrinkageValue),
            FrontMargin = (SellingValue - BuyingValue)/SellingValue,
            RefinedFrontMargin = (SellingValue - BuyingValue - CashHoldingCost - ShrinkageValue)/SellingValue,
            Nmv = sum(SkuNmv)) %>%
  ungroup() %>%
  select(TikingonCateReport, Nmv, FrontMargin, RefinedFrontMargin) %>%
  bind_rows(TotalMargin)
