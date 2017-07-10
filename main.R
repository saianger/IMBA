aisles <- read.csv('aisles.csv')
head(aisles,5)
dept <- read.csv('departments.csv')
head(dept,5)
opp <- read.csv('order_products__prior.csv') 
head(opp,30)
opt <- read.csv('order_products__train.csv')
head(opt,30)
orders <- read.csv('orders.csv')
head(orders,5)
products <- read.csv('products.csv')
head(products,5)



data <- orders %>%
  left_join(rbind(opt,opp),by="order_id") %>%
  left_join(products,by="product_id") %>%
  left_join(aisles,by="aisle_id") %>%
  left_join(dept,by="department_id")

data.train <- data[data$eval_set == 'train',]
data.prior <- data[data$eval_set == 'prior',]
data.test <- data[data$eval_set == 'test',]

#data.prior.sample <- data.prior[sample(1:nrow(orders),3000),]
data.prior$days_since_prior_order[is.na(data.prior$days_since_prior_order)] <-0
data.prior$aisle_id <- as.factor(data.prior$aisle_id)
data.prior$department_id <- as.factor(data.prior$department_id)
head(data.prior)


library(randomForest)
model <- randomForest(as.factor(product_id) ~ order_dow + order_hour_of_day + days_since_prior_order+add_to_cart_order+reordered+aisle_id + department_id,data=data.prior.sample,importance=TRUE)

model <- randomForest(as.factor(product_id) ~ order_dow + order_hour_of_day + days_since_prior_order+add_to_cart_order+reordered+aisle_id + department_id,data=data.prior,importance=TRUE)





set.seed(1)
orders.sample <- orders[sample(1:nrow(orders),3000),]
order.prior.sample <- orders.sample[orders.sample$eval_set == 'prior',]
opp.full.sample <- merge(order.prior.sample, opp)
opp.full.sample <- merge(opp.full.sample, products)
opp.full.sample <- merge(opp.full.sample,aisles)
opp.full.sample <- merge(opp.full.sample,dept)
