#################################################################################################################################
#
#   - Subject : The evolution of automotive field regarding energy consomption
#
#   - Authors : Adrien Marty, Maxime Grac
#
#   - Master ISS INSA Toulouse
#
#     / ! \  Do not forget to set your worspace directory to the source file location / ! \
#################################################################################################################################


# ---------------------------- Imports ---------------------------------------
library(plyr)
library(ggplot2)
library(dplyr)

# ---------------------------- Reading csv ---------------------------------------
dst = read.csv("cardataset/data.csv")

`%ni%` = Negate(`%in%`)

# ---------------------------- Computing of the consumption with metric system ---------------------------------------
dst$Vehicle.Size=factor(dst$Vehicle.Size, levels=c('Compact','Midsize','Large'), ordered=TRUE)
dst$City.l_100 = 100* 3.785411784 / (1.609344 *dst$city.mpg)
dst$Highway.l_100 = 100* 3.785411784 / (1.609344 *dst$highway.MPG)
dst$Mixt.l_100 = (dst$City.l_100 + dst$Highway.l_100)/2


# ---------------------------- dataset with cars with less than 250 HP ---------------------------------------
non_sportive_cars = subset(dst,dst$Engine.HP < 250)


# ---------------------------- mean consumption  ---------------------------------------
conso_moyenne= ddply(non_sportive_cars, .(Year), summarize,  Rate1=mean(City.l_100), Rate2=mean(Highway.l_100))
ggplot(conso_moyenne) + 
  ggtitle("Evolution of the gaz consumption over the years") +
  ylab("Consumption (L/100km)") + xlab("Years") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(stat = "identity", position = "identity", color="#0045F4",aes(x=Year,y=Rate1)) + 
  geom_smooth(se=FALSE, method = "loess", span = 0.5,color ="#0045F4",aes(x=Year,y=Rate1)) +
  geom_point(stat = "identity", position = "identity",color ="#00A19A", aes(x=Year,y=Rate2)) +
  geom_smooth(se=FALSE, method = "loess", span = 0.5, color ="#00A19A", aes(x=Year,y=Rate2)) 
   
  
# ---------------------------- consumption of automatic vs manual ---------------------------------------
conso_gear= ddply(subset(dst,(dst$Transmission.Type=="MANUAL" | dst$Transmission.Type=="AUTOMATIC") & dst$Engine.HP < 250), 
                  .(Year,Transmission.Type), summarize, conso_moyenne=mean((City.l_100+Highway.l_100)/2))
ggplot(conso_gear) + 
  ggtitle("Consumption of automatic gear boxes regarding manual ones") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Consumption (L/100km)") + xlab("Years") +
  labs(fill = "Gearbox type") + 
  theme(axis.text.x = element_text(angle = 90)) +
  geom_histogram(stat = "identity", position = "identity",  aes(x=factor(Year),y=conso_moyenne, fill=factor(Transmission.Type))) +
  scale_fill_brewer(palette = "Set1") 


# ---------------------------- consumption per maker ---------------------------------------
factor_marque = c("Volkswagen","Audi","BMW","Mercedes-Benz","Porsche","Bentley")
selected_type = c("Sedan","Coupe","4dr SUV")
voiture_rec = subset(dst, dst$Year>=2014 & dst$Vehicle.Style %in% selected_type & dst$Make %in% factor_marque)
ggplot(voiture_rec) + 
  ggtitle("Comparaison between makers regarding the consumption") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Consumption (L/100km)") + xlab("Vehicle Style") +
  labs(fill = "Makers") +
  geom_boxplot(position="dodge",aes(x = Vehicle.Style , y = Mixt.l_100 , fill=factor(Make, factor_marque, ordered=TRUE)))
no_cyl=c(NA,0)


# ---------------------------- mean number of cylenders ---------------------------------------
cylinders = ddply(subset(non_sportive_cars,non_sportive_cars$Engine.Cylinders %ni% no_cyl),.(Year,Engine.Cylinders),summarise, Nombre = length(Engine.Cylinders))
cylinders_tot= ddply(cylinders,.(Year) ,summarise, Tot_pon = sum(Nombre*Engine.Cylinders), tot=sum(Nombre))
cylinders = ddply(cylinders_tot,.(Year), summarise, moy = Tot_pon/tot )

ggplot(cylinders) +
  ggtitle("Evolution of the number of cylinders over the years") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Number of cylinders") + xlab("Years") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_histogram(stat = "identity",  aes(x=Year,y=moy))


# ---------------------------- fuel types ---------------------------------------
fuel_type_nmbr = ddply(subset(dst,dst$Engine.Fuel.Type != ""),.(Year,Engine.Fuel.Type),summarise, Nombre = length(Engine.Fuel.Type))
fuel_type_tot= ddply(fuel_type_nmbr,.(Year) ,summarise, Tot = sum(Nombre))
fuel_type_nmbr = merge(fuel_type_nmbr,fuel_type_tot, by = "Year")
fuel_type_nmbr = ddply(fuel_type_nmbr,.(Year, Engine.Fuel.Type),summarise, prop = Nombre/Tot)

ggplot(fuel_type_nmbr) + 
  ggtitle("The evolution of energy sources for cars over the years") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Consumption (L/100km)") + xlab("Year") +
  labs(fill = "Energy sources") +
  geom_bar(stat = "identity",  aes(x=factor(Year),y=(prop), fill=factor(Engine.Fuel.Type))) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_brewer(palette = "RdYlBu")
 

 

