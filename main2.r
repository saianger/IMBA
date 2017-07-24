###########################################################################################################
#
# Kaggle Instacart competition
# Fabien Vavrand, June 2017
# Simple xgboost starter, score 0.3791 on LB
# Products selection is based on product by product binary classification, with a global threshold (0.21)
#
###########################################################################################################

library(data.table)
library(dplyr)
library(tidyr)


# Load Data ---------------------------------------------------------------
path <- "."

aisles <- fread(file.path(path, "aisles.csv"))
departments <- fread(file.path(path, "departments.csv"))
orderp <- fread(file.path(path, "order_products__prior.csv"))
ordert <- fread(file.path(path, "order_products__train.csv"))
orders <- fread(file.path(path, "orders.csv"))
products <- fread(file.path(path, "products.csv"))


# Reshape data ------------------------------------------------------------
aisles$aisle <- as.factor(aisles$aisle)
departments$department <- as.factor(departments$department)
orders$eval_set <- as.factor(orders$eval_set)
products$product_name <- as.factor(products$product_name)

products <- products %>% 
  inner_join(aisles) %>% inner_join(departments) %>% 
  select(-aisle_id, -department_id)
rm(aisles, departments)

ordert$user_id <- orders$user_id[match(ordert$order_id, orders$order_id)]

orders_products <- orders %>% inner_join(orderp, by = "order_id")

rm(orderp)
gc()

# Products ----------------------------------------------------------------
prd <- orders_products %>%
  arrange(user_id, order_number, product_id) %>%
  group_by(user_id, product_id) %>%
  mutate(product_time = n()) %>%
  ungroup() %>%
  group_by(product_id) %>%
  summarise(
    prod_orders = n(),
    prod_reorders = sum(reordered)
    #prod_first_orders = sum(product_time == 1),
    #prod_second_orders = sum(product_time == 2)
  )

#prd$prod_reorder_probability <- prd$prod_second_orders / prd$prod_first_orders
#prd$prod_reorder_times <- 1 + prd$prod_reorders / prd$prod_first_orders
prd$prod_reorder_ratio <- prd$prod_reorders / prd$prod_orders

prd <- prd %>% select(-prod_reorders)

rm(products)
gc()

# Users -------------------------------------------------------------------
users <- orders %>%
  filter(eval_set == "prior") %>%
  group_by(user_id) %>%
  summarise(
    user_orders = max(order_number),
    user_period = sum(days_since_prior_order, na.rm = T),
    user_mean_days_since_prior = mean(days_since_prior_order, na.rm = T)
  )

us <- orders_products %>%
  group_by(user_id) %>%
  summarise(
    user_total_products = n(),
    user_reorder_ratio = sum(reordered == 1) / sum(order_number > 1),
    user_distinct_products = n_distinct(product_id)
  )

users <- users %>% inner_join(us)
users$user_average_basket <- users$user_total_products / users$user_orders

us <- orders %>%
  filter(eval_set != "prior") %>%
  select(user_id, order_id, eval_set,
         time_since_last_order = days_since_prior_order)

users <- users %>% inner_join(us)

rm(us)
gc()


# Database ----------------------------------------------------------------
data <- orders_products %>%
  group_by(user_id, product_id) %>% 
  summarise(
    up_orders = n(),
    up_first_order = min(order_number),
    up_last_order = max(order_number),
    up_average_cart_position = mean(add_to_cart_order))


#orders_products <- orders_products[orders_products$user_id %in% c(1,2,5),]

# get avg product purchase day gap ######
orders_products$days_since_prior_order[is.na(orders_products$days_since_prior_order)] <-0
orders_products <- orders_products[order(orders_products$user_id,orders_products$order_number),]
orders_products_cum <- unique(orders_products[,c('user_id','order_number','days_since_prior_order')]) %>% group_by(user_id) %>% mutate(cumsum_days = cumsum(days_since_prior_order))
orders_products_cum_full <- merge(orders_products,orders_products_cum[,-3])
orders_products_cum_full <- orders_products_cum_full[order(orders_products_cum_full$user_id,orders_products_cum_full$product_id,orders_products_cum_full$order_number),] %>% group_by(user_id) %>% mutate(max_period = max(cumsum_days))
orders_products_cum_full.shift <- orders_products_cum_full[-nrow(orders_products_cum_full),c('user_id','product_id','cumsum_days')] 
orders_products_cum_full.shift <- rbind(orders_products_cum_full[1,c('user_id','product_id','cumsum_days')],orders_products_cum_full.shift)
colnames(orders_products_cum_full.shift) <- c('user_id_s','product_id_s','cumsum_days_s')
# get product day gap that only purchased once
orders_products_cum_full.once <- orders_products_cum_full %>% group_by(user_id,product_id) %>% mutate(count_n = n()) %>% filter(count_n == 1)
orders_products_cum_full.once.full <- orders_products_cum_full.once %>% inner_join(orders[orders$eval_set %in% c('train','test'),c('user_id','days_since_prior_order')], by = "user_id")
orders_products_cum_full.once.full$avg_prod_gap <- ((orders_products_cum_full.once.full$max_period - orders_products_cum_full.once.full$days_since_prior_order.x) + orders_products_cum_full.once.full$days_since_prior_order.y) * 2
prod.day.gap.f.once <- orders_products_cum_full.once.full[,c('user_id','product_id','avg_prod_gap')]
# get product day gap that purchased more than once
orders_products_cum_full.comb <- cbind(orders_products_cum_full,orders_products_cum_full.shift)
orders_products_cum_full.comb$avg_prod_gap <- orders_products_cum_full.comb$cumsum_days - orders_products_cum_full.comb$cumsum_days_s
orders_products_cum_full.comb <- orders_products_cum_full.comb[-1,]
orders_products_cum_full.comb[which(orders_products_cum_full.comb$user_id != orders_products_cum_full.comb$user_id_s | orders_products_cum_full.comb$product_id !=orders_products_cum_full.comb$product_id_s),c('avg_prod_gap')] <- NA
orders_products_cum_full.comb <- orders_products_cum_full.comb[which(!is.na(orders_products_cum_full.comb$avg_prod_gap)),]
prod.day.gap.f <- orders_products_cum_full.comb %>% group_by(user_id,product_id) %>% summarize(median_prod_day_gap = round(mean(avg_prod_gap)))
# combine both
prod.day.gap.f <- rbind(prod.day.gap.f,prod.day.gap.f.once)


# get the add to cart trend for last product purchase ######
z.score.margin <- 2.1
cart.order.down <- function(x){
  if(x>z.score.margin) {return(1)}
  else {return(0)}
}
cart.stat <- orders_products_cum_full %>% group_by(user_id,product_id) %>% summarise(mean_cart_order=mean(add_to_cart_order),sd_cart_order=sd(add_to_cart_order))
cart.stat$sd_cart_order[which(is.na(cart.stat$sd_cart_order))] <- 0
orders_products_cum_full.product.last.cart <- orders_products_cum_full %>% group_by(user_id,product_id) %>% filter(order_number == max(order_number)) %>% select(user_id,product_id,add_to_cart_order)
cart.stat.full <- merge(orders_products_cum_full.product.last.cart,cart.stat)
cart.stat.full$z_score <- (cart.stat.full$add_to_cart_order - cart.stat.full$mean_cart_order)/cart.stat.full$sd_cart_order
cart.stat.full$z_score[is.na(cart.stat.full$z_score)] <- 0
cart.stat.full$cart_order_down <- sapply(cart.stat.full$z_score,cart.order.down)
cart.stat.full <- cart.stat.full[,c('user_id','product_id','cart_order_down')]



rm(orders_products, orders)

data <- data %>% 
  inner_join(prd, by = "product_id") %>%
  inner_join(users, by = "user_id") %>%
  inner_join(prod.day.gap.f, by = c("user_id","product_id")) %>%
  inner_join(cart.stat.full, by = c("user_id","product_id"))

data$up_order_rate <- data$up_orders / data$user_orders
data$up_orders_since_last_order <- data$user_orders - data$up_last_order
#data$up_order_rate_since_first_order <- data$up_orders / (data$user_orders - data$up_first_order + 1)

data <- data %>% 
  left_join(ordert %>% select(user_id, product_id, reordered), 
            by = c("user_id", "product_id"))

rm(ordert, prd, users)
gc()


# Train / Test datasets ---------------------------------------------------
train <- as.data.frame(data[data$eval_set == "train",])
train$eval_set <- NULL
train$user_id <- NULL
train$product_id <- NULL
train$order_id <- NULL
train$reordered[is.na(train$reordered)] <- 0
train$reordered[!is.na(train$reordered)] <- 1

test <- as.data.frame(data[data$eval_set == "test",])
test$eval_set <- NULL
test$user_id <- NULL
test$reordered <- NULL

rm(data)
gc()


# Model -------------------------------------------------------------------
library(xgboost)

params <- list(
  "objective"           = "reg:logistic",
  "eval_metric"         = "logloss",
  "eta"                 = 0.1,
  "max_depth"           = 6,
  "min_child_weight"    = 10,
  "gamma"               = 0.70,
  "subsample"           = 0.76,
  "colsample_bytree"    = 0.95,
  "alpha"               = 2e-05,
  "lambda"              = 10
)

subtrain <- train %>% sample_frac(0.1)
X <- xgb.DMatrix(as.matrix(subtrain %>% select(-reordered)), label = subtrain$reordered)
model <- xgboost(data = X, params = params, nrounds = 80)

importance <- xgb.importance(colnames(X), model = model)
xgb.ggplot.importance(importance)

rm(X, importance, subtrain)
gc()


# Apply model -------------------------------------------------------------
X <- xgb.DMatrix(as.matrix(test %>% select(-order_id, -product_id)))
test$reordered <- predict(model, X)

test$reordered <- (test$reordered > 0.21) * 1

submission <- test %>%
  filter(reordered == 1) %>%
  group_by(order_id) %>%
  summarise(
    products = paste(product_id, collapse = " ")
  )

missing <- data.frame(
  order_id = unique(test$order_id[!test$order_id %in% submission$order_id]),
  products = "None"
)

submission <- submission %>% bind_rows(missing) %>% arrange(order_id)
write.csv(submission, file = "submit.csv", row.names = F)
