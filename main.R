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


day.part <- function(x) {
  if(x >= 6 & x < 12)
  {return('M')}
  else if(x >= 12 & x < 18)
  {return('A')}
  else if(x >=18 & x < 23)
  {return('E')}
  else
  {return('O')}
}

is.weekend <- function(x){
  if(x > 0 & x < 6 ) {return(0)}
  else {return(1)}
}

# get all the orders for train users
train.users <- data.frame(unique(orders[orders$eval_set == 'train',c('user_id')]))
colnames(train.users) <- c('user_id')
train.prior <- merge(train.users,data[data$eval_set == 'prior',])
save(train.prior,file='train_prior.rda')
train.prior.sample <- train.prior[train.prior$user_id %in% c(1,2,5),]
save(train.prior.sample,file = 'train_prior_sample.rda')
train.prior.sample.final <- train.prior.sample %>% group_by(user_id,product_id) %>% summarise(prod_count=n())
train.prior.sample$day_part <- sapply(train.prior.sample$order_hour_of_day,day.part)
train.prior.sample$weekend <- sapply(train.prior.sample$order_dow, is.weekend)
train.prior.sample$days_since_prior_order[is.na(train.prior.sample$days_since_prior_order)] <-0
a <- train.prior.sample %>% group_by(user_id,product_id) %>% order_by(order_number,cumsum(days_since_prior_order))

# proposed training set columns
# product_prob, aisle, dept, weekend, daypart, median_day_gap_between_order_for_this_product, num_of_total_product_purchased_by_customer

# get latest train orders




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
