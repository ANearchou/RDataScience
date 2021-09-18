
##################################################################################
########### DATA MANIPULATION 

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

###############################################################################################
###############################################################################################


###############################################################################################
############## ASSOCIATION RULES

total_bsk <- uniqueN(transactions[,order_id])
target <- unique(transactions[,.(Target = product_id, basket = order_id)])
cross <- unique(transactions[,.(Cross = product_id, basket = order_id)])

target_baskets <- target[,.(target_baskets = uniqueN(basket)), by = .(Target)]
cross_baskets <- cross[,.(cross_baskets = uniqueN(basket)), by = .(Cross)]

combination <- merge.data.table(
  target, cross, 
  by = "basket",
  all = TRUE
)
combination_baskets <- combination[,.(combination_baskets = uniqueN(basket)), by = .(Target, Cross)]
combination_baskets <- combination_baskets[Target != Cross]

combination_baskets <- cross_baskets[combination_baskets, on = .(Cross), nomatch = 0]
combination_baskets <- target_baskets[combination_baskets, on = .(Target), nomatch = 0]
combination_baskets[,total_baskets := total_bsk]

combination_baskets[,Lift := (combination_baskets*total_baskets)/(target_baskets*cross_baskets)]
combination_baskets <- combination_baskets[order(Target, -Lift)]

combination_baskets[,wtd_lift := Lift * (0.25*target_baskets + 0.25*cross_baskets + 0.5*combination_baskets)]

fwrite(
  combination_baskets,
  paste0(examples_path, "/Brazil_ECommerce/Association_Rules/Output/Association_Rules.csv")
)
###############################################################################################
###############################################################################################
