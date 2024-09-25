# ASSUMING SKU LIST GOT
# AVAILABLE INFORMATION

# ReviewRegion <- "HCM"
# ReviewCateReport <- c("FRESH")
# StartDate
# EndDate
# ShrinkageData
# CogsData
# InvData
# RatingData
# CleanSalesOrder
# CleanSelection
# TotalSalesSummary

# Stock in SGN3 and VLN must be investigated

TotalSummary <- TotalSummaryData %>%
  filter(OrderDate >= StartDate,
         OrderDate <= EndDate,
         (NmvQty > 0) |
         (ShrinkageQty > 0) |
         (QtyInventorySalable > 0)) %>%
  distinct()

#
# TotalSummaryLastCycle <- TotalSummaryData %>%
#   filter(OrderDate >= DatePeriod(ReviewPeriod, Origin =  StartDate),
#          OrderDate <= DatePeriod(ReviewPeriod, Origin =  EndDate))


