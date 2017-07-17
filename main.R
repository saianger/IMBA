library("dplyr")
library("modeest")
library(data.table)

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

z.score.margin <- 2.1
cart.order.down <- function(x){
  if(x>z.score.margin) {return(1)}
  else {return(0)}
}

# get all the orders for train users
train.users <- data.frame(unique(orders[orders$eval_set == 'train',c('user_id')]))
colnames(train.users) <- c('user_id')
train.prior <- merge(train.users,data[data$eval_set == 'prior',])
train.train <- merge(train.users,data[data$eval_set == 'train',])
#save(train.prior,file='train_prior.rda')
train.prior.sample <- train.prior[train.prior$user_id %in% c(1,2,5),]
train.train.sample <- train.train[train.train$user_id %in% c(1,2,5),]
# enable full data to create feature matrix
train.prior.sample <- train.prior
train.train.sample <- train.train
#save(train.prior.sample,file = 'train_prior_sample.rda')
#train.prior.sample.final <- train.prior.sample %>% group_by(user_id,product_id) %>% summarise(prod_count=n())
train.prior.sample$day_part <- sapply(train.prior.sample$order_hour_of_day,day.part)
train.prior.sample$weekend <- sapply(train.prior.sample$order_dow, is.weekend)
train.prior.sample$days_since_prior_order[is.na(train.prior.sample$days_since_prior_order)] <-0
train.prior.sample <- train.prior.sample[order(train.prior.sample$user_id,train.prior.sample$order_number),]
a <- unique(train.prior.sample[,c('user_id','order_number','days_since_prior_order')]) %>% group_by(user_id) %>% mutate(cumsum_days = cumsum(days_since_prior_order))
b<- merge(train.prior.sample,a[,-3])
c <- b %>% group_by(product_id,user_id) %>% mutate(day_gap = lag(cumsum_days), order_by=order_number)
d <- b[order(b$user_id,b$product_id,b$order_number),]
d.shift <- d[-nrow(d),c('user_id','product_id','cumsum_days')]
d.shift <- rbind(d[1,c('user_id','product_id','cumsum_days')],d.shift)
colnames(d.shift) <- c('user_id_s','product_id_s','cumsum_days_s')
d.comb <- cbind(d,d.shift)
d.comb$avg_prod_gap <- d.comb$cumsum_days - d.comb$cumsum_days_s
d.comb[which(d.comb$user_id != d.comb$user_id_s | d.comb$product_id !=d.comb$product_id_s),c('avg_prod_gap')] <- NA
d.comb <- d.comb[which(d.comb$avg_prod_gap != 0 & !is.na(d.comb$avg_prod_gap)),]
prod.day.gap.f <- d.comb %>% group_by(user_id,product_id) %>% summarize(median_prod_day_gap = round(median(avg_prod_gap)))
#f <- d %>% group_by(user_id,product_id,day_part) %>% summarise(day_part_count = n()) %>% arrange(user_id, product_id,desc(day_part_count))
# get the mode for day_part
f <- d %>% group_by(user_id,product_id,day_part) %>% summarise(day_part_count = n())  %>% group_by(user_id, product_id) %>% filter(day_part_count == max(day_part_count)) %>% filter(row_number(day_part_count) == 1)
# get the mode for weekend
g <- d %>% group_by(user_id,product_id,weekend) %>% summarise(weekend_count = n())  %>% group_by(user_id, product_id) %>% filter(weekend_count == max(weekend_count))

# get the probability of purchasing a product
prod.count.f <- d %>% group_by(product_id,user_id)  %>% summarise(prod_count=n())
user.order.count.f <- d %>% group_by(user_id,order_id) %>% summarise(order_count1=n()) %>% group_by(user_id) %>% summarise(order_count=n()) 
prod.prob.f <- merge(prod.count.f,user.order.count.f)
prod.prob.f$prod_prob <- prod.prob.f$prod_count/prod.prob.f$order_count
prod.prob.f$prod_prob_rank <- round(prod.prob.f$prod_prob * 10)

# get the num_of_median_total_product_purchased_by_customer_each_order
prod.total.median.f <- d %>% group_by(user_id,order_id) %>% summarise(num_prod = n()) %>% group_by(user_id) %>%summarize(median_num_prod = round(median(num_prod)))

# get the add to cart trend for last product purchase
cart.stat <- d %>% group_by(user_id,product_id) %>% summarise(mean_cart_order=mean(add_to_cart_order),sd_cart_order=sd(add_to_cart_order))
cart.stat <- cart.stat[which(!is.na(cart.stat$sd_cart_order)),]
d.product.last.cart <- d %>% group_by(user_id,product_id) %>% filter(order_number == max(order_number)) %>% select(user_id,product_id,add_to_cart_order)
cart.stat.full <- merge(d.product.last.cart,cart.stat)
cart.stat.full$z_score <- (cart.stat.full$add_to_cart_order - cart.stat.full$mean_cart_order)/cart.stat.full$sd_cart_order
cart.stat.full$z_score[is.na(cart.stat.full$z_score)] <- 0
cart.stat.full$cart_order_down <- sapply(cart.stat.full$z_score,cart.order.down)
cart.stat.full <- cart.stat.full[,c('user_id','product_id','cart_order_down')]


# creating feature matrix
feature.matrix.f <- merge(prod.day.gap.f,prod.prob.f)
feature.matrix.f <- merge(feature.matrix.f,f)
feature.matrix.f <- merge(feature.matrix.f,g)
#feature.matrix.f <- merge(feature.matrix.f,prod.day.gap.f)
feature.matrix.f <- merge(feature.matrix.f,prod.total.median.f)
feature.matrix.f <- merge(feature.matrix.f,products)
feature.matrix.f <- merge(feature.matrix.f,cart.stat.full)
# get train target variables
train.train.sample$bought <- 1
#train.train.sample.dedup <- unique(train.train.sample[,c('user_id')])
train.train.sample.dedup<- train.train.sample[!duplicated(train.train.sample[,c('user_id')]),]
train.train.sample.dedup$day_part_recent <- sapply(train.train.sample.dedup$order_hour_of_day,day.part)
train.train.sample.dedup$weekend_recent <- sapply(train.train.sample.dedup$order_dow, is.weekend)
feature.matrix.f <- merge(feature.matrix.f,train.train.sample[,c('user_id','product_id','bought')],all.x = TRUE)
feature.matrix.f <- merge(feature.matrix.f,train.train.sample.dedup[,c('user_id','day_part_recent','weekend_recent','days_since_prior_order')])
feature.matrix.f$bought[which(is.na(feature.matrix.f$bought))] <- 0
feature.matrix.f.model <- feature.matrix.f[,c('product_id','median_prod_day_gap','prod_prob_rank','day_part','weekend','median_num_prod','aisle_id','department_id','cart_order_down','day_part_recent','weekend_recent','days_since_prior_order','bought')]
str(feature.matrix.f.model)
feature.matrix.f.model$product_id <- as.factor(feature.matrix.f.model$product_id)
feature.matrix.f.model$aisle_id <- as.factor(feature.matrix.f.model$aisle_id)
feature.matrix.f.model$department_id <- as.factor(feature.matrix.f.model$department_id)
feature.matrix.f.model$day_part <- as.factor(feature.matrix.f.model$day_part)
feature.matrix.f.model$day_part_recent <- as.factor(feature.matrix.f.model$day_part_recent)
feature.matrix.f.model$bought <- as.factor(feature.matrix.f.model$bought)
feature.matrix.f.model <- feature.matrix.f.model[,c('bought',  'median_prod_day_gap' , 'prod_prob_rank','day_part','weekend','median_num_prod' , 'aisle_id' ,'department_id' ,'cart_order_down', 'day_part_recent','weekend_recent','days_since_prior_order')]
save(feature.matrix.f.model,file="featurematrix_train.rda")
load(file="featurematrix.rda")


########### Test data set generation ##################
# get all the orders for train users
train.users <- data.frame(unique(orders[orders$eval_set == 'test',c('user_id')]))
colnames(train.users) <- c('user_id')
train.prior <- merge(train.users,data[data$eval_set == 'prior',])
train.train <- merge(train.users,data[data$eval_set == 'test',])
#save(train.prior,file='train_prior.rda')
#train.prior.sample <- train.prior[train.prior$user_id %in% c(1,2,5),]
#train.train.sample <- train.train[train.train$user_id %in% c(1,2,5),]
# enable full data to create feature matrix
train.prior.sample <- train.prior
train.train.sample <- train.train
#save(train.prior.sample,file = 'train_prior_sample.rda')
#train.prior.sample.final <- train.prior.sample %>% group_by(user_id,product_id) %>% summarise(prod_count=n())
train.prior.sample$day_part <- sapply(train.prior.sample$order_hour_of_day,day.part)
train.prior.sample$weekend <- sapply(train.prior.sample$order_dow, is.weekend)
train.prior.sample$days_since_prior_order[is.na(train.prior.sample$days_since_prior_order)] <-0
train.prior.sample <- train.prior.sample[order(train.prior.sample$user_id,train.prior.sample$order_number),]
a <- unique(train.prior.sample[,c('user_id','order_number','days_since_prior_order')]) %>% group_by(user_id) %>% mutate(cumsum_days = cumsum(days_since_prior_order))
b<- merge(train.prior.sample,a[,-3])
c <- b %>% group_by(product_id,user_id) %>% mutate(day_gap = lag(cumsum_days), order_by=order_number)
d <- b[order(b$user_id,b$product_id,b$order_number),]
d.shift <- d[-nrow(d),c('user_id','product_id','cumsum_days')]
d.shift <- rbind(d[1,c('user_id','product_id','cumsum_days')],d.shift)
colnames(d.shift) <- c('user_id_s','product_id_s','cumsum_days_s')
d.comb <- cbind(d,d.shift)
d.comb$avg_prod_gap <- d.comb$cumsum_days - d.comb$cumsum_days_s
d.comb[which(d.comb$user_id != d.comb$user_id_s | d.comb$product_id !=d.comb$product_id_s),c('avg_prod_gap')] <- NA
d.comb <- d.comb[which(d.comb$avg_prod_gap != 0 & !is.na(d.comb$avg_prod_gap)),]
prod.day.gap.f <- d.comb %>% group_by(user_id,product_id) %>% summarize(median_prod_day_gap = round(median(avg_prod_gap)))
#f <- d %>% group_by(user_id,product_id,day_part) %>% summarise(day_part_count = n()) %>% arrange(user_id, product_id,desc(day_part_count))
# get the mode for day_part
f <- d %>% group_by(user_id,product_id,day_part) %>% summarise(day_part_count = n())  %>% group_by(user_id, product_id) %>% filter(day_part_count == max(day_part_count)) %>% filter(row_number(day_part_count) == 1)
# get the mode for weekend
g <- d %>% group_by(user_id,product_id,weekend) %>% summarise(weekend_count = n())  %>% group_by(user_id, product_id) %>% filter(weekend_count == max(weekend_count))

# get the probability of purchasing a product
prod.count.f <- d %>% group_by(product_id,user_id)  %>% summarise(prod_count=n())
user.order.count.f <- d %>% group_by(user_id,order_id) %>% summarise(order_count1=n()) %>% group_by(user_id) %>% summarise(order_count=n()) 
prod.prob.f <- merge(prod.count.f,user.order.count.f)
prod.prob.f$prod_prob <- prod.prob.f$prod_count/prod.prob.f$order_count
prod.prob.f$prod_prob_rank <- round(prod.prob.f$prod_prob * 10)

# get the num_of_median_total_product_purchased_by_customer_each_order
prod.total.median.f <- d %>% group_by(user_id,order_id) %>% summarise(num_prod = n()) %>% group_by(user_id) %>%summarize(median_num_prod = round(median(num_prod)))

# get the add to cart trend for last product purchase
cart.stat <- d %>% group_by(user_id,product_id) %>% summarise(mean_cart_order=mean(add_to_cart_order),sd_cart_order=sd(add_to_cart_order))
cart.stat <- cart.stat[which(!is.na(cart.stat$sd_cart_order)),]
d.product.last.cart <- d %>% group_by(user_id,product_id) %>% filter(order_number == max(order_number)) %>% select(user_id,product_id,add_to_cart_order)
cart.stat.full <- merge(d.product.last.cart,cart.stat)
cart.stat.full$z_score <- (cart.stat.full$add_to_cart_order - cart.stat.full$mean_cart_order)/cart.stat.full$sd_cart_order
cart.stat.full$z_score[is.na(cart.stat.full$z_score)] <- 0
cart.stat.full$cart_order_down <- sapply(cart.stat.full$z_score,cart.order.down)
cart.stat.full <- cart.stat.full[,c('user_id','product_id','cart_order_down')]


# creating feature matrix
feature.matrix.f <- merge(prod.day.gap.f,prod.prob.f)
feature.matrix.f <- merge(feature.matrix.f,f)
feature.matrix.f <- merge(feature.matrix.f,g)
#feature.matrix.f <- merge(feature.matrix.f,prod.day.gap.f)
feature.matrix.f <- merge(feature.matrix.f,prod.total.median.f)
feature.matrix.f <- merge(feature.matrix.f,products)
feature.matrix.f <- merge(feature.matrix.f,cart.stat.full)
# get train target variables
#train.train.sample$bought <- 1
#train.train.sample.dedup <- unique(train.train.sample[,c('user_id')])
train.train.sample.dedup<- train.train.sample[!duplicated(train.train.sample[,c('user_id')]),]
train.train.sample.dedup$day_part_recent <- sapply(train.train.sample.dedup$order_hour_of_day,day.part)
train.train.sample.dedup$weekend_recent <- sapply(train.train.sample.dedup$order_dow, is.weekend)
#feature.matrix.f <- merge(feature.matrix.f,train.train.sample[,c('user_id','product_id','bought')],all.x = TRUE)
feature.matrix.f <- merge(feature.matrix.f,train.train.sample.dedup[,c('user_id','day_part_recent','weekend_recent','days_since_prior_order')])
#feature.matrix.f$bought[which(is.na(feature.matrix.f$bought))] <- 0
feature.matrix.f.model.test <- feature.matrix.f[,c('user_id','product_id','median_prod_day_gap','prod_prob_rank','day_part','weekend','median_num_prod','aisle_id','department_id','cart_order_down','day_part_recent','weekend_recent','days_since_prior_order')]
str(feature.matrix.f.model.test)
#feature.matrix.f.model.test$product_id <- as.factor(feature.matrix.f.model$product_id)
feature.matrix.f.model.test$aisle_id <- as.factor(feature.matrix.f.model.test$aisle_id)
feature.matrix.f.model.test$department_id <- as.factor(feature.matrix.f.model.test$department_id)
feature.matrix.f.model.test$day_part <- as.factor(feature.matrix.f.model.test$day_part)
feature.matrix.f.model.test$day_part_recent <- as.factor(feature.matrix.f.model.test$day_part_recent)
feature.matrix.f.model.test <- feature.matrix.f.model.test[,c('user_id','product_id',  'median_prod_day_gap' , 'prod_prob_rank','day_part','weekend','median_num_prod' , 'aisle_id' ,'department_id' ,'cart_order_down', 'day_part_recent','weekend_recent','days_since_prior_order')]
save(feature.matrix.f.model.test,file="featurematrix_test.rda")
load(file="featurematrix_test.rda")

########## End of test data set generation ###########

library(randomForest)
library(reshape2)
for(i in as.vector(unique(feature.matrix.f.model$department_id))){
  temp.train <- feature.matrix.f.model[feature.matrix.f.model$department_id == i,]
  temp.test <- feature.matrix.f.model.test[feature.matrix.f.model.test$department_id == i,]
  temp.test$aisle_id <- factor(temp.test$aisle_id)
  temp.train$aisle_id <- factor(temp.train$aisle_id)
  model <- randomForest(bought ~ median_prod_day_gap + prod_prob_rank+day_part+weekend+median_num_prod +aisle_id +cart_order_down+ day_part_recent+weekend_recent+days_since_prior_order,data=temp.train,importance=TRUE)
  test.predict <- predict(model,temp.test)
  #head(temp.train)
  break
}

final.result <- cbind(temp.test[,c('user_id','product_id')],test.predict)
final.result <- final.result[which(final.result$test.predict == 1),]
final.result <- final.result[,c('user_id','product_id')]
final.result <- merge(final.result,train.train[,c('user_id','order_id')])
test <- aggregate(product_id ~ order_id, data = final.result[,c('order_id','product_id')], paste, collapse = " ")
write.csv(test,file="final.csv",row.names = FALSE)

library(randomForest)
model <- randomForest(bought ~ median_prod_day_gap + prod_prob_rank+day_part+weekend+median_num_prod +aisle_id +cart_order_down+ day_part_recent+weekend_recent+days_since_prior_order,data=data.dept,importance=TRUE)
require(xgboost)
bstSparse <- xgboost(data = feature.matrix.f.model[,c('median_prod_day_gap' , 'prod_prob_rank','day_part','weekend','median_num_prod' , 'aisle_id' ,'department_id' ,'cart_order_down', 'day_part_recent','weekend_recent','days_since_prior_order')], label = feature.matrix.f.model$bought, max_depth = 15, objective = "binary:logistic")










# proposed training set columns
# product_prob, aisle, dept, weekend, daypart, median_day_gap_between_order_for_this_product, num_of_median_total_product_purchased_by_customer_each_order

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
model <- randomForest(bought ~ order_dow + order_hour_of_day + days_since_prior_order+add_to_cart_order+reordered+aisle_id + department_id,data=feature.matrix.f,importance=TRUE)

model <- randomForest(as.factor(product_id) ~ order_dow + order_hour_of_day + days_since_prior_order+add_to_cart_order+reordered+aisle_id + department_id,data=data.prior,importance=TRUE)





set.seed(1)
orders.sample <- orders[sample(1:nrow(orders),3000),]
order.prior.sample <- orders.sample[orders.sample$eval_set == 'prior',]
opp.full.sample <- merge(order.prior.sample, opp)
opp.full.sample <- merge(opp.full.sample, products)
opp.full.sample <- merge(opp.full.sample,aisles)
opp.full.sample <- merge(opp.full.sample,dept)
