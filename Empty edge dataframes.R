#Jessie Bolin Honours
#Empty lists / dataframes for use when mapping EAC edge
#Use this with source()
# 2018-11-10


#Create lists for future outputs
all.months <- list()
inverseissue <- list()
inverselist <- list()

#Dataframe for data, variance of PC1, direction of loadings, and flexible loading direction
all.stuff <- data.frame(Date = character(), Variance = numeric(), Direction = character(), Direction.flex = character())

#If loadings are not the same sign, write them to this dataframe
#This is dataframe of method 'failures'
inverse <- data.frame(date = numeric(), PC1_sst = numeric(), PC1_v = numeric(), PC1_speed = numeric(),
                      PC2_sst = numeric(), PC2_v = numeric(), PC2_speed = numeric(), PC3_sst = numeric(),
                      PC3_v = numeric(), PC3_speed = numeric(), variance1 = numeric(),
                      variance2 = numeric(), variance3 = numeric())

#Stick loadings, all dates, and variances for all 3 PC's into this dataframe.
#This will have PCA information for every day since 2001, during whale migration months
df <- data.frame(date = numeric(), PC1_sst = numeric(), PC1_v = numeric(), PC1_speed = numeric(),
                 PC2_sst = numeric(), PC2_v = numeric(), PC2_speed = numeric(), PC3_sst = numeric(),
                 PC3_v = numeric(), PC3_speed = numeric(), PC1_var =numeric(), PC2_var = numeric(), PC3_var = numeric())
