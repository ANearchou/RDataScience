con <- RMySQL::dbConnect(
  RMySQL::MySQL(),
  dbname = "company",
  host = "courses.csrrinzqubik.us-east-1.rds.amazonaws.com",
  port = 3306,
  user = "student",
  password = "datacamp"
)

library(DBI)
library(data.table)

dbListTables(con)

employees <- data.table(
  dbReadTable(con, "employees")
)
products <- data.table(
  dbReadTable(con, "products")
)
sales <- data.table(
  dbReadTable(con, "sales")
)


sql <- 'SELECT sales.*, 
          products.name as product_name, 
          products.id as product_id,
          products.contract,
          employees.name as employee_name, 
          employees.id as employee_id,
          employees.started_at
  
      FROM (sales LEFT JOIN products AS products ON sales.product_id = products.id)
      LEFT JOIN employees as employees ON sales.employee_id = employees.id
      ORDER BY sales.id'
res <- dbSendQuery(con, sql)
full_table_sql <- data.table(
  dbFetch(res)
)

full_table_sql[,`:=`(
  employee_id = NULL, product_id = NULL
)]

full_table_r <- employees[,.(employee_id = id, employee_name = name, started_at)][
  products[,.(product_id = id, product_name = name, contract)][
    sales, on = .(product_id)
  ], on = .(employee_id)
]
full_table_r <- full_table_r[,names(full_table_sql), with = FALSE]


all(full_table_r == full_table_sql) # TRUE
