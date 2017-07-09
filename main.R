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

set.seed(1)
orders.sample <- orders[sample(1:nrow(orders),3000),]
order.prior.sample <- orders.sample[orders.sample$eval_set == 'prior',]
opp.full.sample <- merge(order.prior.sample, opp)
