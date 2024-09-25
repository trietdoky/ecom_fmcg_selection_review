source("rdata_load.R")

# CLEAN DATA
ShrinkageData <- RawShrinkageData %>%
  select(sku, product_qty, date_done, dest_wh) %>%
  separate(dest_wh, into = 'warehouse', sep = '-', extra = 'drop') %>%
  rename(
    "shrinkage_qty" = "product_qty",
    "shrinkage_date" = "date_done",
    "warehouse_name" = "warehouse"
  ) %>%
  mutate(warehouse_name = toupper(warehouse_name),
         warehouse_name = case_when(
           warehouse_name == "HN" ~ "MFLBI",
           warehouse_name == "SGN" ~ "MFTBI",
           TRUE ~ warehouse_name
         )) %>%
  group_by(shrinkage_date, sku, warehouse_name) %>%
  summarize(shrinkage_qty = sum(shrinkage_qty)) %>%
  ungroup() %>%
  clean_names("upper_camel")

CogsData <- NewestCogs %>%
  rename("warehouse_name" = "warehouse") %>%
  mutate(warehouse_name = toupper(warehouse_name),
         warehouse_name = case_when(
           warehouse_name == "HN" ~ "MFLBI",
           warehouse_name == "SGN" ~ "MFTBI",
           TRUE ~ warehouse_name
         )) %>%
  clean_names("upper_camel")  %>%
  group_by(Sku, Cogs, DateKey, WarehouseName) %>%
  summarise(QtyInventorySalable = sum(QtyInventorySalable)) %>%
  ungroup()

InvData <- NewestCogs %>%
  rename("warehouse_name" = "warehouse") %>%
  mutate(warehouse_name = toupper(warehouse_name),
         warehouse_name = case_when(
           warehouse_name == "HN" ~ "MFLBI",
           warehouse_name == "SGN" ~ "MFTBI",
           TRUE ~ warehouse_name
         )) %>%
  clean_names("upper_camel") %>%
  group_by(Sku, Cogs, DateKey, WarehouseName) %>%
  summarise(QtyInventorySalable = sum(QtyInventorySalable)) %>%
  ungroup()


RatingData <- RawSumRating %>%
  clean_names("upper_camel")

CleanSalesOrder <- RawSalesOrder %>%
  mutate(warehouse_name = toupper(warehouse_name),
         warehouse_name = case_when(
           warehouse_name == "HN" ~ "MFLBI",
           warehouse_name == "SGN" ~ "MFTBI",
           TRUE ~ warehouse_name
         )) %>%
  clean_names("upper_camel")

CleanSelection <- SelectionNgon %>%
  clean_names("upper_camel")



TotalSummaryData <- GetPeriodSkuList(as.Date(Sys.Date() - days(186)), Today, CleanSalesOrder, ShrinkageData, InvData) %>%
  left_join(CleanSalesOrder %>% select(Sku, WarehouseName, ProductQty, ProductPrice, Nmv,
                                       starts_with("Order"), starts_with("Customer"),
                                       -OrderModel, -OrderGeneralStatus),
            by = "Sku", suffix=c("",".d")) %>%
  select(-ends_with(".d")) %>% # REMOVE DUPPLICATED COLUMNS
  group_by(Sku, CustomerId) %>%
  mutate(ReturnCustomerCount = n(),
         OrderDate = as.Date(OrderDate)) %>%
  ungroup() %>%
  group_by(OrderDate, Sku, WarehouseName,
           CustomerGroupRegion) %>%
  summarize(SkuValue = sum(ProductQty*ProductPrice),
            SkuNmv = sum(Nmv),
            NmvQty = sum(ProductQty),
            TotalCustomer = n_distinct(CustomerId) - sum(is.na(CustomerId)),
            ReturnCustomer = sum(ReturnCustomerCount > 1)) %>%
  ungroup() %>%
  full_join(ShrinkageData, by = c("Sku", "WarehouseName", "OrderDate" = "ShrinkageDate")) %>%
  full_join(CogsData, by = c("Sku", "WarehouseName", "OrderDate" = "DateKey")) %>%
  filter(!is.na(OrderDate)) %>% # REMOVE EMPTY ENTRIES
  left_join(CleanSelection %>% select(Sku, ProductName, Cate2Id, Cate2, Cate3Id, Cate3, TikingonCateReport, IsNewProduct), by = "Sku") %>%
  filter(!is.na(ProductName)) %>% # FILTER OUT GIFTS
  left_join(RatingData, by = "Sku") %>%
  mutate(ShrinkageQty = ifelse(is.na(ShrinkageQty), 0, ShrinkageQty),
         ShrinkageValue = ShrinkageQty*Cogs) %>%
  mutate_at(.vars = c("SkuNmv", "SkuValue", "NmvQty",
                      "TotalCustomer", "ReturnCustomer",
                      "ShrinkageQty", "ShrinkageValue"), na.fill, 0)

TotalSummaryData <- TotalSummaryData %>%
  mutate(CustomerGroupRegion = case_when(
    WarehouseName %in% ActiveCentralWarehouse ~ 'CENTRAL',
    WarehouseName %in% ActiveNorthWarehouse ~ 'NORTH',
    WarehouseName %in% ActiveSouthWarehouse ~ 'SOUTH',
    TRUE ~ 'TRANSIT'
  ))


DocCalculationData <- TotalSummaryData %>%
  filter(OrderDate <= Today,
         OrderDate > Today - days(30)) %>%
  group_by(Sku, CustomerGroupRegion) %>%
  mutate(RR30 = sum(NmvQty)/30,
         StockToday = ifelse(OrderDate == Today, QtyInventorySalable,as.numeric(NA)),
         StockToday = max(StockToday, na.rm = TRUE),
         DoC = ifelse(RR30 <= 0.032, NA, StockToday/RR30)) %>%
  ungroup() %>%
  distinct(Sku, CustomerGroupRegion, DoC) %>%
  filter(DoC > 0)

TotalSummaryData <- TotalSummaryData %>%
  left_join(DocCalculationData, by = c("Sku", "CustomerGroupRegion")) %>%
  distinct()


















