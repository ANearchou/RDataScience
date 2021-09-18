
library(data.table)

path <- paste0(examples_path, "/Brazil_ECommerce")

order_payments <- fread(paste0(path, "/Input/olist_order_payments_dataset.csv"))
orders <- fread(paste0(path, "/Input/olist_orders_dataset.csv"))
order_items <- fread(paste0(path, "/Input/olist_order_items_dataset.csv"))



transactions <- order_items[,.(sales = sum(price),
                              freight_value = sum(freight_value),
                              quantity = .N),
                           by = .(order_id, product_id, seller_id)]
productTable <- fread(paste0(path, "/Input/olist_products_dataset.csv"))
productTable <- unique(productTable, by = "product_id")
productTable[,Weight := product_weight_g/1000]

categories_eng <- fread(paste0(path, "/Input/product_category_name_translation.csv"))
productTable <- categories_eng[productTable, on = .(product_category_name)] 
setnames(productTable, "product_category_name_english", "Category")
productTable[is.na(Category), Category := "MISSING"]
productTable[,Category := gsub("_", " ", Category)]
productTable[,c("product_category_name", "product_name_lenght", "product_description_lenght", 
                "product_photos_qty", "product_weight_g", "product_length_cm", "product_height_cm", "product_width_cm") := NULL]


transactions <- orders[,.(order_id, salesdate = fasttime::fastPOSIXct(order_purchase_timestamp))][
  transactions, on = .(order_id)
]

sellers <- fread(paste0(path, "/Input/olist_sellers_dataset.csv"))
sellers[,seller_city := gsub("_", " ", seller_city)]




#############################################################################################
transactions <- productTable[transactions, on = .(product_id)]
transactions <- sellers[,.(seller_id, seller_city)][transactions, on = .(seller_id)]
transactions[,Volume := quantity*Weight]
transactions[,period := format(as.Date(lubridate::floor_date(salesdate, "months")), "%Y-%m-01")]


product_lvl <- transactions[,.(
  sales = sum(sales),
  freight_value = sum(freight_value),
  total_sales = sum(sales) + sum(freight_value),
  quantity = sum(quantity),
  volume = sum(Volume),
  baskets = uniqueN(order_id)
), by = .(product_id, period)][order(product_id, period)]

category_lvl <- transactions[,.(
  sales = sum(sales),
  freight_value = sum(freight_value),
  total_sales = sum(sales) + sum(freight_value),
  quantity = sum(quantity),
  volume = sum(Volume),
  baskets = uniqueN(order_id)
), by = .(Category, period)][order(Category, period)]

sellers_city_lvl <- transactions[,.(
  sales = sum(sales),
  freight_value = sum(freight_value),
  total_sales = sum(sales) + sum(freight_value),
  quantity = sum(quantity),
  volume = sum(Volume),
  baskets = uniqueN(order_id)
), by = .(seller_city, period)][order(seller_city, period)]


seller_lvl <- transactions[,.(
  sales = sum(sales),
  freight_value = sum(freight_value),
  total_sales = sum(sales) + sum(freight_value),
  quantity = sum(quantity),
  volume = sum(Volume),
  baskets = uniqueN(order_id)
), by = .(seller_id, period)][order(seller_id, period)]


fwrite(product_lvl, paste0(path, "/Output/product_lvl.csv"))
fwrite(category_lvl, paste0(path, "/Output/category_lvl.csv"))
fwrite(sellers_city_lvl, paste0(path, "/Output/sellers_city_lvl.csv"))
fwrite(seller_lvl, paste0(path, "/Output/seller_lvl.csv"))