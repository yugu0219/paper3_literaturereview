###############random forest##############
library(ranger);library(data.table)
# Read the data
d1 <- fread("C:/Users/gu021/OneDrive - Wageningen University & Research/1 WUR_work/2 meta/paper1_lr_origional.csv")
length(unique(d1$title))
# Rename columns
cols_old <- colnames(d1)
cols_new <- tolower(unlist(tstrsplit(cols_old,'\\[',keep=1)))
setnames(d1,cols_old,cols_new)
d1[,alfe:= al+fe]
d1[,title2 := .GRP,by='title']
d1[,logq:= log(qmax)]
d1[,logk:= log(klnew)]
d1[,logq_k:= log(qmax/klnew)]
d2 <- d1[-c(68:73),][title2!=7][,c("ph_h2o","som","clay","alfe", "logq_k")]

d4 <- d1[-c(68:73),][title2!=7][,c("ph_h2o","som","clay","alfe","soil_type_fao2", "logq")]
cols_soilty <- c('soil_type_fao2')
d4[,c(cols_soilty) := lapply(.SD,as.factor),.SDcols = cols_soilty][]
d5 <- mltools::one_hot(d4, cols = c("soil_type_fao2"))
d6 <- d1[-c(68:73),][title2!=7][,c("ph_h2o","som","clay","alfe", "logq_k")]


set.seed(123)
mtry <- seq(0.1, 2.5, 0.3)
num.trees <- c(100, 200, 500, 1000)
hypergrid <- list()
for(i in 1:length(mtry)){
  hypergrid[[i]] <- data.table(
    mtry = mtry[i],
    num.trees = num.trees
  )
}


hypergrid <- rbindlist(hypergrid)

test <- as.data.table(na.omit(d2))

train.index <- sample(1:nrow(test), 0.7 * nrow(test))
val.index <- 1:nrow(test)
val.index <- val.index[!val.index %in% train.index]

iris.train <- test[train.index,]
iris.val <- test[val.index,]

hypergrid$model <- paste0("model" , 1:nrow(hypergrid))
#iris.train$Species <- NULL

#fwrite(hypergrid, file = "dev/hypergrid.csv")
#fwrite(iris.train, file = "dev/train.csv")
#fwrite(iris.val, file = "dev/val.csv")

for(i in 1:nrow(hypergrid)){
  print(i)
  #select rel
  rel.hypergrid <- hypergrid[i,]
  
  #create folder
  dir.create(paste0("dev/", rel.hypergrid$model))
  
  #train model using hyperparameters
  iris.rf <- ranger(formula = logq_k~ ., data = iris.train,
                    num.trees = rel.hypergrid$num.trees,
                    mtry = rel.hypergrid$mtry)
  
  #predict on iris.val
  prediction <- predict(iris.rf, iris.val)[[1]]
  
  #extract metrics
  metrics <- data.table(model = rel.hypergrid$model,
                        RMSE = caret::RMSE(prediction, iris.val$logq_k))
  
  fwrite(metrics, file = paste0("dev/", rel.hypergrid$model, "/RMSE.csv"))
}
ranger(formula = logq_k ~ ., data = iris.train,
       num.trees = 500,
       mtry = 2)

##read train
iris.train <- fread("dev/train.csv")
iris.val <- fread("dev/val.csv")
hypergrid <- fread(file = "dev/hypergrid.csv")


filelist <- list.files("dev/", recursive = TRUE, pattern = ".csv", full.names = TRUE)
filelist <- filelist[grepl("RMSE", filelist)]
filelist <- lapply(filelist, fread)
filelist <- rbindlist(filelist)

final <- merge(hypergrid, filelist, by = "model")
final <- dplyr::arrange(final, final$RMSE)

final[RMSE == max(RMSE)]

best <- ranger(formula = logq_k ~ ., data = iris.train,
               num.trees = as.numeric(final[RMSE == max(RMSE)][,3]),
               mtry = as.numeric(final[RMSE == max(RMSE)][,2]),
               importance = 'permutation')
iris.val$predicted <- predict(best, iris.val)[[1]]
plot(iris.val$logq_k, iris.val$predicted)
summary(lm(logq_k~ predicted,iris.val))


dt.label <- data.table(
  R2 = c(caret::R2(iris.val$logq_k, iris.val$predicted)),
  RMSE = c(caret::RMSE(iris.val$logq_k, iris.val$predicted)),
  type = c( "test"))

dt.label[, RMSE := round(RMSE, 2)]
dt.label[, R2 := round(R2, 2)]
dt.label[, label := paste("R2 =", R2, "\nRMSE = ", RMSE)]

ggplot(data=iris.val,aes(x=logq_k,y= predicted))+
  geom_point(aes(logq_k,predicted))+
  geom_smooth(method = 'lm', se = FALSE, linetype = "solid") +
  geom_abline(slope = 1,intercept = 0,linetype=2)+
  ggtitle("log(Qmax/KL)~ph+SOM+clay+[Al+Fe]ox")+
  geom_text(data = iris.val, aes(x = 5, y = 12), label = dt.label$label, 
            inherit.aes = FALSE, col = "#e31a1c",family="A", size = 4,face = "bold") +
  xlim(3,15)+ylim(3,15)+
  labs(x="Measured Log(Qmax/KL)",y="Predicted Log(Qmax/kl)")+
  theme_bw()


# Create the explainer
explainer.best <- DALEX::explain(best, 
                                 data = iris.train[,-5], 
                                 y = iris.train$logq_k, label = "model_train")
explainer.test <- DALEX::explain(model, 
                                 data = as.data.frame(dt.test[, .SD, .SDcols = !c(target)]), 
                                 y = dt.test$psi, label = "model_test")



explainer.all <- explain(model, 
                         data = as.data.frame(dt[, .SD, .SDcols = !c(target)]), 
                         y = dt$psi, label = "model_all")
#--------------
performance1 <- model_performance(explainer.train)
performance2 <- model_performance(explainer.test)
performance3<- model_performance(explainer.all)

library(caret)
#
trainpred <- predict(model, newdata = dt.train)
dt.train[, predicted := trainpred]
defaultSummary(data.frame(obs = dt.train$psi, pred = trainpred))
#
trainpred <- predict(model, newdata = dt.test)
dt.test[, predicted := trainpred]
defaultSummary(data.frame(obs = dt.test$psi, pred = trainpred))


