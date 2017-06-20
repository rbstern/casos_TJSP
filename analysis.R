library(glmnet)
library(slam)
library(tm)

load("data-raw/d_final.rda")
covariaveis_adicionais=data.frame(magistrado=d_final$magistrado,comarca=d_final$comarca,tempo=d_final$tempo)
rm(d_final)
gc()

load("ementas_tm.rda")

ementas_tm=removeSparseTerms(ementas_tm,sparse = 0.99)
ementas_tm=ementas_tm>0
nomes_covariaveis_texto=colnames(ementas_tm)
gc()

ementas_tm <-  sparseMatrix(i=ementas_tm$i, j=ementas_tm$j, x=ementas_tm$v,
                              dims=c(ementas_tm$nrow, ementas_tm$ncol))


# criar dummies para juizes e comarcas
X_comarca <- sparse.model.matrix(~covariaveis_adicionais[,c("comarca")]-1)
X_magistrado <- sparse.model.matrix(~covariaveis_adicionais[,c("magistrado")]-1)
covariaveis=cbind(ementas_tm,X_comarca,X_magistrado)

nomes_covariaveis_adicionais=c(colnames(X_comarca),colnames(X_magistrado))

nomes_covariaveis=c(nomes_covariaveis_texto,nomes_covariaveis_adicionais)

fitLasso = cv.glmnet(covariaveis, covariaveis_adicionais$tempo, alpha=1) # validacao cruzada
plot(fitLasso) # plota lambda vs risco estimada
lambdaOtimo = fitLasso$lambda.min # retorna melhor lambda
coeffs=coefficients(fitLasso,s = lambdaOtimo) # melhor lambda

covariaveis_modelo=nomes_covariaveis
estimativas_covariaveis_modelo=as.matrix(coeffs)[-1]
ordemCovariaveis=order(abs(estimativas_covariaveis_modelo),decreasing = TRUE)
covariaveis_ordem_importancia=covariaveis_modelo[ordemCovariaveis]
palavras_ordem_importancia=covariaveis_modelo[
  ordemCovariaveis[ordemCovariaveis<=length(nomes_covariaveis_texto)]
  ]
palavras_ordem_importancia[1:10]


sort(abs(estimativas_covariaveis_modelo),decreasing = TRUE)[1:10]
plot(order(abs(estimativas_covariaveis_modelo),decreasing = TRUE))
abline(h=length(nomes_covariaveis_texto))


tapply(covariaveis_adicionais$tempo,as.matrix(covariaveis[,which(nomes_covariaveis=="laud")]),mean)
tapply(covariaveis_adicionais$tempo,as.matrix(covariaveis[,which(nomes_covariaveis=="laud")]),sd)


tapply(covariaveis_adicionais$tempo,as.matrix(covariaveis[,grep("ão Preto",nomes_covariaveis)]),mean)


### XGBoost

library(xgboost)
bst <- xgboost(data = covariaveis, 
               label = covariaveis_adicionais$tempo,
               nthread = 3, 
               nround = 1000,
               objective="reg:linear",
               verbose=FALSE,lambda=0.01)
importance_matrix=xgb.importance(nomes_covariaveis, model = bst)[1:75,]
xgb.plot.importance(importance_matrix)


randomIndex=sample(1:nrow(covariaveis),nrow(covariaveis))
nTrein=round(nrow(covariaveis)*0.5)
train=randomIndex[1:nTrein]
test=randomIndex[-c(1:nTrein)]


# all covariates
bst <- xgboost(data = covariaveis[train,], 
               label = covariaveis_adicionais$tempo[train],
               nthread = 3, 
               nround = 10000,
               objective="reg:linear",
               verbose=FALSE,lambda=0.01)
predicoes_todas_covariaveis=predict(bst,covariaveis[test,])
plot(predicoes_todas_covariaveis,covariaveis_adicionais$tempo[test],pch=18,cex=0.3)
abline(a=0,b=1,lwd=3,col=2)
mean(abs(predicoes_todas_covariaveis-covariaveis_adicionais$tempo[test]))
196.7667
sd(abs(predicoes_todas_covariaveis-covariaveis_adicionais$tempo[test]))/length(test)
0.02359313

# only text
bst <- xgboost(data = covariaveis[train,1:length(nomes_covariaveis_texto)], 
               label = covariaveis_adicionais$tempo[train],
               nthread = 3, 
               nround = 10000,
               objective="reg:linear",
               verbose=FALSE,lambda=0.01)
predicoes_covariaveis_texto=predict(bst,covariaveis[test,1:length(nomes_covariaveis_texto)])
plot(predicoes_covariaveis_texto,covariaveis_adicionais$tempo[test],pch=18,cex=0.3)
abline(a=0,b=1,lwd=3,col=2)
mean(abs(predicoes_covariaveis_texto-covariaveis_adicionais$tempo[test]))
209.2898
sd(abs(predicoes_todas_covariaveis-covariaveis_adicionais$tempo[test]))/length(test)
0.02359313


# only juizes e comarcas
bst <- xgboost(data = covariaveis[train,-c(1:length(nomes_covariaveis_texto))], 
               label = covariaveis_adicionais$tempo[train],
               nthread = 3, 
               nround = 10000,
               objective="reg:linear",
               verbose=FALSE,lambda=0.01)
predicoes_covariaveis_Semtexto=predict(bst,covariaveis[test,-c(1:length(nomes_covariaveis_texto))])
plot(predicoes_covariaveis_texto,covariaveis_adicionais$tempo[test],pch=18,cex=0.3)
abline(a=0,b=1,lwd=3,col=2)
mean(abs(predicoes_covariaveis_Semtexto-covariaveis_adicionais$tempo[test]))
242.5776
sd(abs(predicoes_covariaveis_Semtexto-covariaveis_adicionais$tempo[test]))/length(test)
0.0281097

##### lasso split
# tudo
fitLasso = cv.glmnet(covariaveis[train,], covariaveis_adicionais$tempo[train], alpha=1) # validacao cruzada
plot(fitLasso) # plota lambda vs risco estimada
lambdaOtimo = fitLasso$lambda.min # retorna melhor lambda
predicoes_todas_covariaveis_lasso=predict(fitLasso,covariaveis[test,],s = lambdaOtimo) # melhor lambda
plot(predicoes_todas_covariaveis_lasso,covariaveis_adicionais$tempo[test],pch=18,cex=0.3)
abline(a=0,b=1,lwd=3,col=2)
mean(abs(predicoes_todas_covariaveis_lasso-covariaveis_adicionais$tempo[test]))
201.775
sd(abs(predicoes_todas_covariaveis_lasso-covariaveis_adicionais$tempo[test]))/length(test)
0.02177344

# soh texto
fitLasso = cv.glmnet(covariaveis[train,1:length(nomes_covariaveis_texto)], covariaveis_adicionais$tempo[train], alpha=1) # validacao cruzada
plot(fitLasso) # plota lambda vs risco estimada
lambdaOtimo = fitLasso$lambda.min # retorna melhor lambda
predicoes_covariaveis_texto_lasso=predict(fitLasso,covariaveis[test,1:length(nomes_covariaveis_texto)],s = lambdaOtimo) # melhor lambda
plot(predicoes_covariaveis_texto_lasso,covariaveis_adicionais$tempo[test],pch=18,cex=0.3)
abline(a=0,b=1,lwd=3,col=2)
mean(abs(predicoes_covariaveis_texto_lasso-covariaveis_adicionais$tempo[test]))
210.7557
sd(abs(predicoes_covariaveis_texto_lasso-covariaveis_adicionais$tempo[test]))/length(test)
0.02220379

# soh juizes e comarcas
fitLasso = cv.glmnet(covariaveis[train,-c(1:length(nomes_covariaveis_texto))], covariaveis_adicionais$tempo[train], alpha=1) # validacao cruzada
plot(fitLasso) # plota lambda vs risco estimada
lambdaOtimo = fitLasso$lambda.min # retorna melhor lambda
predicoes_covariaveis_Semtexto_lasso=predict(fitLasso,covariaveis[test,-c(1:length(nomes_covariaveis_texto))],s = lambdaOtimo) # melhor lambda
plot(predicoes_covariaveis_Semtexto_lasso,covariaveis_adicionais$tempo[test],pch=18,cex=0.3)
abline(a=0,b=1,lwd=3,col=2)
mean(abs(predicoes_covariaveis_Semtexto_lasso-covariaveis_adicionais$tempo[test]))
243.0841
sd(abs(predicoes_covariaveis_Semtexto_lasso-covariaveis_adicionais$tempo[test]))/length(test)
0.02733699
