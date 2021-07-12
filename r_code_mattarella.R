## ----setup, include=FALSE, echo=FALSE, warning=FALSE,error=FALSE,fig.align = "center"----------------------------------
knitr::opts_chunk$set(echo = FALSE
                      #,warning=FALSE,
                      # message=FALSE
                      )
extrafont::loadfonts()
# remotes::install_github("easystats/easystats")
library(tidyverse)
library(sandwich)
library(readr)
library(corrplot)
library(easystats)
library(hrbrthemes)
library(Hmisc)
library(GoodmanKruskal)
library(ggraph) 
library(glmnet)
library(caret)
library(ggpubr)
library(olsrr)
library(GGally)
library(mltools)
library(data.table)
library(multcomp)
library(car)
library(MASS)
library(lmtest)
library(doParallel)
source('data/funct/unregister_dopar.R')
df <- read_csv("data/student.csv")


## ----bank--------------------------------------------------------------------------------------------------------------
df$sex <- as.factor(df$sex)
df$address<- as.factor(df$address)
df$famsize<- as.factor(df$famsize)
df$Pstatus <- as.factor(df$Pstatus)
df$Mjob<- as.factor(df$Mjob)
df$Fjob<- as.factor(df$Fjob)
df$reason<- as.factor(df$reason)
df$guardian<- as.factor(df$guardian)
df$schoolsup<- as.factor(df$schoolsup)
df$famsup<- as.factor(df$famsup)
df$paid<- as.factor(df$paid)
df$activities<- as.factor(df$activities)
df$nursery<- as.factor(df$nursery)
df$higher <- as.factor(df$higher)
df$internet <- as.factor(df$internet)
df$romantic <- as.factor(df$romantic)
df$school <- NULL
G1 <- df$G1
G2 <- df$G2

describe_distribution(df) # numeric variables


## ----pressure, echo=F, error=FALSE, fig.align='center', warning=FALSE,fig.width=7--------------------------------------
a <- ggplot(df,
       aes(x = Fjob,
           fill = famsup)) +
  geom_bar(position = "stack")

b <- ggplot(df,
       aes(x = Mjob,
           fill = famsup)) +
  geom_bar(position = "stack")

c <- ggplot(df,
       aes(x = famsup,
           fill = paid)) +
  geom_bar(position = "fill") +
            labs(y = "Proportion")
d <- ggplot(df,
       aes(x = activities ,
           fill = address)) +
  geom_bar(position = "fill") +
            labs(y = "Proportion")
s <- ggarrange(a,b,c,d,
          labels = c("A", "B", "C","D"),
          ncol = 2, nrow = 2)

annotate_figure(s,
                top = text_grob("", color = "black", face = "bold", size = 14),
                fig.lab = "Figure 1", fig.lab.face = "bold")


## ----fig, fig.align = "center", fig.width=9----------------------------------------------------------------------------

annotate_figure(ggarrange(ggplot(data=df, aes(x=G3, group=Mjob, fill=Mjob)) +
    geom_density(adjust=1.5, alpha=.4) +
    theme_ipsum(base_family = 'Helvetica')
    ,ggplot(data=df, aes(x=G3, group=Fjob, fill=Fjob)) +
    geom_density(adjust=1.5, alpha=.4) +
    theme_ipsum(base_family = 'Helvetica')
    ,ggplot(data=df, aes(x=G3, group=address, fill=address)) +
    geom_density(adjust=1.5, alpha=.4) +
    theme_ipsum(base_family = 'Helvetica')
    ,ggplot(data=df, aes(x=G3, group=internet, fill=internet)) +
    geom_density(adjust=1.5, alpha=.4) +
    theme_ipsum(base_family = 'Helvetica'),
    labels = c("A", "B", "C","D")),
                top = text_grob(" ", color = "black", face = "bold", size = 14),
                fig.lab = "Figure 2", fig.lab.face = "bold")


## ----fig.align='center'------------------------------------------------------------------------------------------------
ggplot(data = df) +
  geom_count(mapping = aes(x = G3, y = failures))+
  theme_ipsum(base_family = 'Helvetica')


## ----------------------------------------------------------------------------------------------------------------------
df <- df %>% 
            mutate(y = round((G1+G2+G3)/3,1), .keep = 'unused')

BoxCoxTrans(df$y)
df$y <- df$y^1.3


## ----fig.align='center'------------------------------------------------------------------------------------------------
ggdensity(df, x = "y", fill = "lightblue", title = "General Grade") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")


## ----------------------------------------------------------------------------------------------------------------------
leveneTest(y~Mjob,data = df)


## ----warning=FALSE,echo=FALSE,message=FALSE----------------------------------------------------------------------------
mjob <- aov(y~Mjob,data = df)
posthoc = glht(mjob, linfct = mcp(Mjob = "Tukey"))
summary(corrected <- posthoc, test = adjusted(type = "bonferroni"))


## ----fig.width=4-------------------------------------------------------------------------------------------------------
leveneTest(y~Fjob,data = df)
fjob <- aov(y~Fjob,data = df)
posthoc2 = glht(fjob, linfct = mcp(Fjob = "Tukey"))
summary(corr <- posthoc2,test = adjusted(type = "bonferroni"))


## ----------------------------------------------------------------------------------------------------------------------
reas <- aov(y~reason,data = df)
posthoc3 = glht(reas, linfct = mcp(reason = "Tukey"))
summary(posthoc3,test = adjusted(type = "bonferroni"))

inter <- aov(y~internet,data = df)
summary(inter)

addr <- aov(y~address,data = df)
summary(addr)



## ----include=FALSE-----------------------------------------------------------------------------------------------------
corrplo <- df %>%
  correlation() %>%
  summary() 

library(Hmisc)
library(corrplot)
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
    )
}
r <- append (corrplo$Parameter, 'y')
r <- r[-c(11)]

res2<-rcorr(as.matrix(df[r]))
flattenCorrMatrix(res2$r, res2$P)


## ----fig.align='center'------------------------------------------------------------------------------------------------
# Insignificant correlations are leaved blank
corrplot::corrplot(res2$r, type="upper", 
         p.mat = res2$P, insig = "blank",diag = F, tl.col = 'black')


## ----fig.align='center'------------------------------------------------------------------------------------------------
df[r] %>%
  correlation(partial = T) %>%
  plot()


## ----------------------------------------------------------------------------------------------------------------------
multiple = c("Fjob", "Mjob", "reason")

binary = c("sex",'address','higher',"famsize", "Pstatus", "schoolsup", "famsup", "paid", "activities", "internet", "romantic")

for (col in binary) {
  df[col] <- as.numeric(unlist(df[col]))
}
df[binary] <- ifelse(df[binary] == 1,0,1)

one_hot_enc <- as.data.frame(one_hot(as.data.table(df[multiple])))

enc_df <- df[,!(names(df) %in% multiple)]
enc_df <- enc_df[,!(names(enc_df) %in% c('guardian','nursery'))]

one_hot_enc <- one_hot_enc[,!(names(one_hot_enc) %in% c('Fjob_other','Mjob_other','reason_other'))] # drop the baselines cat, the ones less interpretable
one_hot_enc_alter <- one_hot_enc[,!(names(one_hot_enc) %in% c('Fjob_health','Fjob_services','Fjob_at_home','Mjob_at_home','Mjob_services','reason_course','reason_home'))] # drop the baselines cat, the ones less interpretable

data <- cbind(one_hot_enc,enc_df)
data_altern <- cbind(one_hot_enc_alter,enc_df)
data_altern$Walc <- NULL
describe_distribution(data)


## ----traintest---------------------------------------------------------------------------------------------------------
set.seed(30)
split_train_test <- createDataPartition(y = data$y, p=0.8, list = F)
train <- data[split_train_test,]
test <-  data[-split_train_test,]
dim(test)
dim(train)


## ----fig.align='center'------------------------------------------------------------------------------------------------
model <- lm(y ~., train)
check_model(model)


## ----------------------------------------------------------------------------------------------------------------------
check_normality(model)
check_heteroscedasticity(model)
check_autocorrelation(model)
check_collinearity(model)


## ----------------------------------------------------------------------------------------------------------------------
plot(model, which = 4)


## ----------------------------------------------------------------------------------------------------------------------
lmtest::coeftest(model, vcov. = vcovHC, type = "HC1")


## ----------------------------------------------------------------------------------------------------------------------
performance(model)


## ----------------------------------------------------------------------------------------------------------------------
m_stepwise <- lm(y ~., train)
m_stepwise <- select_parameters(m_stepwise) 
coeftest(m_stepwise,vcov. = vcovHC, type = "HC1")


## ----------------------------------------------------------------------------------------------------------------------
check_model(m_stepwise)


## ----------------------------------------------------------------------------------------------------------------------
check_normality(m_stepwise)
check_heteroscedasticity(m_stepwise)
check_autocorrelation(m_stepwise)
check_collinearity(m_stepwise)


## ----------------------------------------------------------------------------------------------------------------------
x = as.matrix(train[,-36])
y = train$y


## ----------------------------------------------------------------------------------------------------------------------
m_cvlasso=cv.glmnet(x,y)
plot(m_cvlasso)


## ----------------------------------------------------------------------------------------------------------------------
coef <- coef(m_cvlasso, s = m_cvlasso$lambda.min)
coefname <- coef@Dimnames[[1]][-1]
coef <- coefname[coef@i]
coef


## ----------------------------------------------------------------------------------------------------------------------
fmla <- as.formula(paste("y ~ ", paste(coef, collapse = "+")))
lasso <- lm(fmla, data=train)
check_model(lasso)


## ----------------------------------------------------------------------------------------------------------------------
check_normality(lasso)
check_heteroscedasticity(lasso)
check_autocorrelation(lasso)
check_collinearity(lasso)


## ----------------------------------------------------------------------------------------------------------------------

coeftest(lasso, vcov. = vcovHC, type='HC1')


## ----------------------------------------------------------------------------------------------------------------------
compare_performance(model,lasso,m_stepwise,rank = T)


## ---- fig.align='center'-----------------------------------------------------------------------------------------------
plot(compare_performance(model,lasso,m_stepwise,rank = T))


## ----------------------------------------------------------------------------------------------------------------------
robust <- rlm(fmla,data=train, psi = psi.bisquare) # more penalizing than standard huber one


## ----------------------------------------------------------------------------------------------------------------------
hweights <- data.frame(resid = robust$resid, weight = robust$w)
hweights2 <- hweights[order(robust$w),]
hweights2[1:10,]


## ----Table comparison, error=FALSE, message=FALSE, warning=FALSE, paged.print=TRUE-------------------------------------
rob_se_pan <- list(sqrt(diag(vcovHC(model, type = "HC1"))),
                   sqrt(diag(vcovHC(m_stepwise, type = "HC1"))),
                   sqrt(diag(vcovHC(lasso, type = "HC1"))),
                   sqrt(diag(vcovHC(robust, type = "HC1")))
                   )

# stargazer::stargazer(model, m_stepwise, lasso, robust,
#                      type = 'text',
#                      digits = 2,
#                      dep.var.labels.include = F,
#                      omit.table.layout = "n",
#                      header = F, 
#                      column.labels = c('Baseline OLS', 'Stepwise', 'Lasso', "Robust"),
#                      se = rob_se_pan)



## ----fig.align='center'------------------------------------------------------------------------------------------------

cntr <- caret::trainControl(method = 'cv',
                            number = 10,
                            search = 'grid')

#### Comment the code to obtain the algo just because I saved it as an Rds file and load it automatically, 
#### it's faster this way ####

# tree <- caret::train(y~.,
#                      data = train,
#                      method = "rpart",
#                      trControl = cntr,
#                      tuneLength = 50)

#saveRDS(tree, "rpar_model.rds")

tree <- readRDS("rpar_model.rds")
plot(tree)
print(round(tree$bestTune[[1]],5))


## ----------------------------------------------------------------------------------------------------------------------
train_pred = predict(tree, newdata = train)
test_pred = predict(tree, newdata = test)

print(paste0('Training RMSE: ', (rmse(train$y,train_pred)),'  ',
             'Test RMSE: ', (rmse(test$y, test_pred)),'  ',
             'Training MAE: ', (mae(train$y,train_pred)),'  ',
             'Test MAE: ', (mae(test$y, test_pred))
             )
      )

TREE <- c(rmse(test$y, test_pred),mae(test$y, test_pred),R2(train$y, train_pred))


## ----------------------------------------------------------------------------------------------------------------------
library(rattle)

fancyRpartPlot(tree$finalModel)


## ---- warning=FALSE, error=FALSE---------------------------------------------------------------------------------------

# cl <- makePSOCKcluster(7)
# registerDoParallel(cl)


cntr <- trainControl(method = 'boot_all',
                              number = 50)

tunegrid <- expand.grid(.mtry=rnorm(9,mean=sqrt(length(train))+3,sd=3.5))

#### Comment the code to obtain the algo just because I saved it as an Rds file and load it automatically, 
#### it's faster this way ####

# rf <- caret::train(y ~ .,
#                        data = train, 
#                        method = "rf",
#                        trControl = cntr, 
#                        tuneGrid= tunegrid,
#                        allowParallel = T)
# saveRDS(rf, "rf_model.rds")

rf <- readRDS("rf_model.rds")

#stopCluster(cl)


## ----------------------------------------------------------------------------------------------------------------------
train_pred = predict(rf, newdata = train)
test_pred = predict(rf, newdata = test)

print(paste0('Training RMSE: ', (rmse(train$y,train_pred)),'  ',
             'Test RMSE: ', (rmse(test$y, test_pred)),'  ',
             'Training MAE: ', (mae(train$y,train_pred)),'  ',
             'Test MAE: ', (mae(test$y, test_pred))
             )
      )
RF <- c(rmse(test$y, test_pred),mae(test$y, test_pred),R2(train$y, train_pred))


## ----------------------------------------------------------------------------------------------------------------------
plot(varImp(rf))


## ----------------------------------------------------------------------------------------------------------------------
test_pred = predict(model, newdata = test)
train_pred = predict(model, newdata = train)
OLS <- c(rmse(test$y, test_pred),mae(test$y, test_pred),R2(train$y, train_pred))
test_pred = predict(m_stepwise, newdata = test)
train_pred = predict(m_stepwise, newdata = train)
Stepwise <- c(rmse(test$y, test_pred),mae(test$y, test_pred),R2(train$y, train_pred))
test_pred = predict(lasso, newdata = test)
train_pred = predict(lasso, newdata = train)
LASSO <- c(rmse(test$y, test_pred),mae(test$y, test_pred),R2(train$y, train_pred))
test_pred = predict(robust, newdata = test)
train_pred = predict(robust, newdata = train)
Robust <- c(rmse(test$y, test_pred),mae(test$y, test_pred),R2(train$y, train_pred))

metrics <- data.frame(OLS,Stepwise,LASSO,Robust,TREE,RF)
row.names(metrics) <- c("RMSE","MAE","R2")
metrics


## ---- include=FALSE, message=FALSE-------------------------------------------------------------------------------------
library(grid)
library(gridExtra)
library(cluster)
library(factoextra)
library(png)
library(dendextend)

df <- read_csv("data/student.csv")
df$sex <- as.factor(df$sex)
df$address<- as.factor(df$address)
df$famsize<- as.factor(df$famsize)
df$Pstatus <- as.factor(df$Pstatus)
df$Mjob<- as.factor(df$Mjob)
df$Fjob<- as.factor(df$Fjob)
df$reason<- as.factor(df$reason)
df$guardian<- as.factor(df$guardian)
df$schoolsup<- as.factor(df$schoolsup)
df$famsup<- as.factor(df$famsup)
df$paid<- as.factor(df$paid)
df$activities<- as.factor(df$activities)
df$nursery<- as.factor(df$nursery)
df$higher <- as.factor(df$higher)
df$internet <- as.factor(df$internet)
df$romantic <- as.factor(df$romantic)
df$school <- NULL
G1 <- df$G1
G2 <- df$G2


## ----------------------------------------------------------------------------------------------------------------------
y <-cut(df$G3, seq(0,20,4), labels=c("F","D","C","B","A"),include.lowest=T)


## ----------------------------------------------------------------------------------------------------------------------
dfnum <- df %>%
  correlation() %>%
  summary() 

dfnums <- append(dfnum$Parameter,'G3')
dfnum <- as.data.frame(scale(df[dfnums]))
df_votefree <- dfnum[,-c(14,15,16)]
describe_distribution(dfnum[,-c(14,15,16)])


## ---- fig.width=7, fig.height=4.5--------------------------------------------------------------------------------------
set.seed(42)
sample <- createDataPartition(y = y, p=0.95, list = F)
df_sample <-  dfnum[-sample,]
y_sample <- y[-sample]
distance <- get_dist(df_sample)
d_1 <- fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"),show_labels = F)
df_sample_2 <-  df_votefree[-sample,]
y_sample <- y[-sample]
distance <- get_dist(df_sample_2)
d_2 <- fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"),show_labels = F)

annotate_figure(ggarrange(d_1,d_2,labels = c("G1,G2,G3 included","G1,G2,G3 excluded")),
                top = text_grob("", color = "black", face = "bold", size = 14),
                fig.lab = "Distant Matirces", fig.lab.face = "bold")


## ----------------------------------------------------------------------------------------------------------------------
k2 <- kmeans(df_sample, centers = 2, nstart = 25)


## ----------------------------------------------------------------------------------------------------------------------
fviz_cluster(k2, data = df_sample_2)


## ----fig.align='center',fig.width=7------------------------------------------------------------------------------------
k3 <- kmeans(df_sample_2, centers = 3, nstart = 25)
k4 <- kmeans(df_sample_2, centers = 4, nstart = 25)
k5 <- kmeans(df_sample_2, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = df_sample_2) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df_sample_2) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df_sample_2) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df_sample_2) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)


## ----fig.align='center'------------------------------------------------------------------------------------------------
set.seed(42)

k <- 15 

wss<-sapply(1:k ,function(k){kmeans(df_votefree,k,nstart=50,iter.max=15)$tot.withinss})

plot(1:k, wss, type="b", pch=18, xlab="Number of clusters K", ylab="Total within-clusters sum of squares", col="orange") 


## ----fig.align='center'------------------------------------------------------------------------------------------------
fviz_nbclust(df_votefree, kmeans, method = "silhouette")


## ----------------------------------------------------------------------------------------------------------------------
#kmeans2 <- kmeans(df_votefree,2)
ris2 <- eclust(df_votefree,"kmeans",k=2)
avg_s_2 <- fviz_silhouette(ris2) + labs(title= "K = 2",
                                        subtitle= " Avg Silhoutte width:")


## ----------------------------------------------------------------------------------------------------------------------
avg_s_2


## ----------------------------------------------------------------------------------------------------------------------
final <- kmeans(df_votefree, 2, nstart = 25)


## ----------------------------------------------------------------------------------------------------------------------
df_votefree %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

