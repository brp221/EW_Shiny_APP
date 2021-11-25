library(remotes)
#install_github("yonghah/esri2sf")
library(esri2sf)
library(ggplot2)

avg_watt_btype <- energy_building_metadata %>% group_by(BUILDINGTY)%>% summarise(WattHours = mean(WattHours))


#energy_building_metadata <- filter_year_built()
sum_watts_date_range <- energy_building_metadata %>% group_by(BUILDINGID)%>% summarise(WattHours = mean(WattHours))
# A basic scatterplot with color depending on Species
ggplot(sum_watts_date_range, aes(x=YEARBUILT, y=WattHours)) + 
  geom_point(size=6) 



energy_building_metadata <- energy_building_metadata %>% filter(YEARBUILT < 1950, YEARBUILT > 1800)

energy_building_metadata <- filter_year_built()
#sum_watts_date_range <- energy_building_metadata %>% group_by(BUILDINGID)%>% summarise(WattHours = mean(WattHours))
# A basic scatterplot with color depending on Species
ggplot(energy_building_metadata, aes(x=YEARBUILT, y=WattHours)) + 
  geom_point(size=6) 


# Notched Boxplot of Tooth Growth Against 2 Crossed Factors
# boxes colored for ease of interpretation
boxplot(len~supp*dose, data=ToothGrowth, notch=TRUE,
        col=(c("gold","darkgreen")),
        main="Tooth Growth", xlab="Suppliment and Dose")

# Convert the variable dose from a numeric to a factor variable
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
head(ToothGrowth)
# Box plot with dot plot
# Basic box plot
p <- ggplot(ToothGrowth, aes(x=dose, y=len)) + 
  geom_boxplot()
p
# Rotate the box plot
p + coord_flip()
# Notched box plot
ggplot(ToothGrowth, aes(x=dose, y=len)) + 
  geom_boxplot(notch=TRUE)
# Change outlier, color, shape and size
ggplot(ToothGrowth, aes(x=dose, y=len)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)
p + geom_dotplot(binaxis='y', stackdir='center', dotsize=1)
# Box plot with jittered points
# 0.2 : degree of jitter in x direction
p + geom_jitter(shape=16, position=position_jitter(0.2))


p <- ggplot(ToothGrowth, aes(x=dose, y=len)) + 
  geom_boxplot()
p
# Rotate the box plot
p + coord_flip()
# Notched box plot
ggplot(energy_building_metadata, aes(x=BUILDINGTY, y=WattHours)) + 
  geom_boxplot(notch=TRUE)
# Change outlier, color, shape and size
ggplot(ToothGrowth, aes(x=dose, y=len)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)
p + geom_dotplot(binaxis='y', stackdir='center', dotsize=1)
# Box plot with jittered points
# 0.2 : degree of jitter in x direction
p + geom_jitter(shape=16, position=position_jitter(0.2))
