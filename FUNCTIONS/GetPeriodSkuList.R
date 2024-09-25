GetPeriodSkuList <- function(StartPeriodDate, EndPeriodDate, SalesData, ShrkData, InvData) {
  # SALES ORDER

  SalesSku <- SalesData %>%
    filter(OrderDate >= StartPeriodDate,
           OrderDate <= EndPeriodDate) %>%
    distinct(Sku)

  # SHRINKAGE

  ShrinkageSku <- ShrkData %>%
    filter(ShrinkageDate >= StartPeriodDate,
           ShrinkageDate <= EndPeriodDate) %>%
    distinct(Sku)

  # INV

  InventorySku <- InvData %>%
    filter(DateKey >= StartPeriodDate,
           DateKey <= EndPeriodDate) %>%
    filter(QtyInventorySalable > 0) %>%
    distinct(Sku)

  # SELECTION CREATED

  # SelectionSku <- SelectionData %>%
  #   filter(CreatedAt >= StartPeriodDate,
  #          CreatedAt <= EndPeriodDate) %>%
  #   distinct(Sku)

  # ACTIVE SKU

  TotalPeriodSkuList <- union_all(SalesSku, ShrinkageSku) %>%
    union_all(InventorySku) %>%
    # union_all(SelectionSku) %>%
    distinct(Sku)

  return(TotalPeriodSkuList)
}


