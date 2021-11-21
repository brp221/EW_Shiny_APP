library(remotes)
#install_github("yonghah/esri2sf")
library(esri2sf)
library(ggplot2)

# 3: Using RColorBrewer
ggplot(mtcars, aes(x=as.factor(cyl), fill=as.factor(cyl) )) + 
  geom_bar( ) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position="none")


avg_watt_btype <- energy_building_metadata %>% group_by(BUILDINGTY)%>% summarise(WattHours = mean(WattHours))

# 3: Using RColorBrewer
ggplot(energy_building_metadata, aes(x=BUILDINGTY, y = WattHours,  fill=BUILDINGTY )) + 
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position="none")