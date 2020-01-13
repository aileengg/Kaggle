library(dplyr)
library(ggplot2)
pokemon <- read.csv('/Users/_____/Downloads/pokemon.csv')
head(pokemon)


n_use <- data.frame(pokemon$name, pokemon$speed,
                    pokemon$height_m, pokemon$weight_kg,
                   pokemon$hp, pokemon$attack, pokemon$defense, pokemon$type1)

length(n_use$pokemon.name)

n_use$pokemon.height_m

n_use <- n_use[order(- n_use$pokemon.height_m),]

library(tidyr)
n_use <- n_use %>% drop_na()


n_use %>% head(30) %>%
ggplot(aes(pokemon.name, pokemon.height_m)) + geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1)) + ylab("height")

## k-nearest neighbour

n_use$pokemon.type1 = factor(n_use$pokemon.type1)
library(dplyr)
n_use2 <- n_use[-1] 

library(caTools)
set.seed(123)
split = sample.split(n_use2$pokemon.type1, SplitRatio = 0.75)
training_set = subset(n_use2, split== TRUE)
test_set = subset(n_use2, split==FALSE)

# feature scaling
training_set[-7]=scale(training_set[-7])
test_set[-7] = scale(test_set[-7])

# fitting model
library(class)
type_pred = knn(train = training_set[, -7],
             test = test_set[, -7],
             cl = training_set[, 7],
             k = 5,
             prob = TRUE)

# Making the Confusion Matrix
confusion_matrix = table(test_set[, 7], type_pred)
confusion_matrix


###################################
## For logistic regression
n_nd <- n_use[-1]

n_nd[-7] = scale(n_nd[-7])

logistic <- glm(n_use$pokemon.type1 ~ n_use$pokemon.height_m + n_use$pokemon.weight_kg +
                n_use$pokemon.hp + n_use$pokemon.attack +
                  n_use$pokemon.defense, data = n_use, family="binomial")

summary(logistic)


#### linear model 
library(MASS)

f_log <- stepAIC(logistic, direction=c("both"), trace=FALSE)
summary(f_log)

model1 <- lm(n_nd$pokemon.attack ~ ., data= n_nd)
summary(model1)

f_model1 <- stepAIC(model1, direction=c("both"), trace=FALSE)
summary(f_model1)

library(dplyr)
#n_linear <- n_nd %>% select( -c(pokemon.type1))
n_linear <- n_nd[-7]
cor(n_linear)

library(GGally)

ggcorr(n_linear)

model2 <- lm(n_linear$pokemon.attack ~., data= n_linear)
summary(model2)

f_model2 <- stepAIC(model2, direction=c("both"), trace=FALSE)
summary(f_model2)

bm <- glm(n_nd$pokemon.type1 ~ ., data=n_nd, family="binomial")
summary(bm)


## random forest 
library(randomForest)
set.seed(123)
classifier = randomForest(x = training_set[-7],
                          y = training_set$pokemon.type1,
                          ntree = 300)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-7])

# Making the Confusion Matrix
cm = table(test_set[, 7], y_pred)

plot(classifier)

### grouping 

group_type <- n_use2 %>% 
  
  
  
  
## for k-means clustering
pokemon2 <- n_use[-1]

library(tidyr)
pokemon2 <- pokemon2 %>% drop_na()
pokemon2 <- pokemon2[-7]  

set.seed(6)
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(pokemon2, i)$withinss)
plot(1:10,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')

# Fitting K-Means to the dataset
set.seed(29)
kmeans = kmeans(x = pokemon2, centers = 5)
y_kmeans = kmeans$cluster

### Hierarchal CLustering 
library(dplyr)
poke3 <- pokemon2 %>% select(pokemon.weight_kg,pokemon.defense)
# Using the dendrogram to find the optimal number of clusters
dendrogram = hclust(d = dist(poke3, method = 'euclidean'), method = 'ward.D')
plot(dendrogram,
     main = paste('Dendrogram'),
     ylab = 'Euclidean distances')

# Fitting Hierarchical Clustering to the dataset
hc = hclust(d = dist(poke3, method = 'euclidean'), method = 'ward.D')
y_hc = cutree(hc, 2)

# Visualising the clusters
library(cluster)
clusplot(poke3,
         y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels= 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of pokemon'),
         xlab = 'Weight in Kg',
         ylab = 'Defense Score')

### SVM between type1 and defense
library(e1071)
g_training <- tibble(training_set$pokemon.defense, training_set$pokemon.hp,
                       training_set$pokemon.type1)
names(g_training)[1] = 'defense'
names(g_training)[2] = 'hp'
names(g_training)[3] = 'type'

g_test <- tibble(test_set$pokemon.defense, test_set$pokemon.hp,
                 test_set$pokemon.type1)
names(g_test)[1] = 'defense'
names(g_test)[2] = 'hp'
names(g_test)[3] = 'type'

classifier2 = svm(formula = g_training$type ~.,
                 data = g_training,
                 type = 'C-classification',
                 kernel = 'linear')

# Predicting the Test set results
ypred2 = predict(classifier2, newdata = g_test[-3])

# Making the Confusion Matrix
 cm = table(g_test$type, ypred2)


### optimizing equation, maximizing hp based on defense and attack, then inner join to get type1.
max_m <- tibble(n_use2$pokemon.defense, n_use2$pokemon.attack, n_use2$pokemon.hp) 
max(max_m$`n_use2$pokemon.defense`)  #230
max(max_m$`n_use2$pokemon.attack`)  #185
max(max_m$`n_use2$pokemon.hp`)  #255

md <- lm(n_use2$pokemon.hp ~ -1 + n_use$pokemon.defense + n_use2$pokemon.attack)
summary(md)

library(lpSolve)

objective <- function(X1,X2) X1*n_use2$pokemon.defense + X2*n_use2$pokemon.attack
const <- matrix(c(0.29259, 0.55518), nrow=2, byrow = TRUE)
constraint <- c(0, 0)
direction <- c(">=", ">=")

lp(direction="max", objective, const, direction, constraint)

#install.packages("lpSolveAPI")
library(lpSolveAPI)
lprec <- make.lp(0,2)
lp.control(lprec, sense="max")
set.objfn(lprec, c(0.29259, 0.5551)) 
add.constraint(lprec, c(1,1),">=", 0)
add.constraint(lprec, c(1,0), "<=",230)
add.constraint(lprec, c(0,1), "<=",185)
lprec
solve(lprec)
get.objective(lprec)
get.variables(lprec)
 
 
opt <- as.data.frame(pokemon[2:20])

opt$classification <- pokemon$classfication

m_l <- glm(opt$classification ~., data=opt, family="binomial")
summary(m_l)
