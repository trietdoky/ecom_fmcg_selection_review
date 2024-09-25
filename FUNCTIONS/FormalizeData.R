FormalizeData <- function(data) {
  result <- data %>%
    mutate(across(matches(c("Sku", "SupplierId", "Order_Code", "Original_Order_Code", "Original_Code", "SellerId", "CustomerId")), as.character),
           across(matches(c("NetWeight", "Cogs", "SbdStockQuantity", "Nmv", "Cmv")), as.numeric))
  return(result)
}