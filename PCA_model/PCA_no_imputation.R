#install.packages("corrr")
library('corrr')
#install.packages("ggcorrplot")
library(ggcorrplot)
#install.packages("FactoMineR")
library("FactoMineR")
#install.packages("factoextra")
library("factoextra")
#install.packages("onnx")
#library('onnx')

library(psych)

getwd()

par(ask=TRUE)
set.seed(1234) # make results reproducible
# colors = c("cyan1", "cyan2", "cyan3", "cyan4", "gold1", "gold2", "gold3", "gold4", "red", "yellow", "blue")

chess2019_full = read.csv("base_data/CHES2019_incl_party_id_without_imputation_all_items_filled.csv")
voter_table = read.csv("base_data/EVS_without_imputation_all_items_filled.csv")


# reduce data - delete NAN-rows
chess2019 <- na.omit(chess2019_full[,2:7])

# scale data
data_normalized <- scale(chess2019)
head(data_normalized)



##################

ncomp <- 2

pca_rotated <- psych::principal(data_normalized, rotate="varimax", nfactors=ncomp, scores=TRUE)
print(pca_rotated$scores[1:5,])  # Scores returned by principal()
pca_rotated$scores
prediction_voters <- predict.psych(pca_rotated, data = voter_table)

#write.csv(cbind(chess2019_full, pca_rotated$scores), 'CHES2019_incl_party_id.csv')
#write.csv(cbind(voter_table, prediction_voters), 'result_application_PCA_to_voter_data.csv')

summary(pca_rotated)
unclass(loadings(pca_rotated))
pca_rotated$weights



#export as onnx model
#onnx_model <- convert_to_onnx(model = pca_rotated)
#save_onnx(onnx_model, file = "model.onnx")
######################










# apply PCA
#set.seed(111)
#data.pca <- princomp(data_normalized)
#summary(data.pca)

# check components
#data.pca$loadings[, 1:2]


#pca_object <- data.pca

# visualize PCs
#fviz_eig(pca_object, addlabels = TRUE)

# biplot of attributes
#fviz_pca_var(pca_object, col.var = "black")

# contribution
#fviz_cos2(pca_object, choice = "var", axes = 1:2)

# biplot including contribution
#fviz_pca_var(pca_object, col.var = "cos2",
#             gradient.cols = c("black", "orange", "green"),
#             repel = TRUE)




