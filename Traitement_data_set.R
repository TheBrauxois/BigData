library(plyr)
library(ggplot2)
library(dplyr)
dst = read.csv("~/INSA/5\ ISS/BigData/BigData/cardataset/data.csv")

dst$Vehicle.Size=factor(dst$Vehicle.Size, levels=c('Compact','Midsize','Large'), ordered=TRUE)
dst$City.l_100 = 100* 3.785411784 / (1.609344 *dst$city.mpg)
dst$Highway.l_100 = 100* 3.785411784 / (1.609344 *dst$highway.MPG)
dst$Mixt.l_100 = (dst$City.l_100 + dst$Highway.l_100)/2

#aggregate(dst[, ], list(dst$Year), mean)
non_sportive_cars = subset(dst,dst$Engine.HP < 250)

conso_moyenne= ddply(non_sportive_cars, .(Year), summarize,  Rate1=mean(City.l_100), Rate2=mean(Highway.l_100))
ggplot(conso_moyenne) + 
  ggtitle("Evolution of the gaz consumption over the years") +
  geom_point(stat = "identity", position = "identity", color="#0045F4",aes(x=Year,y=Rate1,fill="City")) + 
  geom_smooth(se=FALSE, method = "loess", span = 0.5,color ="#0045F4",aes(x=Year,y=Rate1)) +
  geom_point(stat = "identity", position = "identity",color ="#00A19A", aes(x=Year,y=Rate2)) +
  geom_smooth(se=FALSE, method = "loess", span = 0.5, color ="#00A19A", aes(x=Year,y=Rate2,fill="Highway")) +
  ylab("Consumption (L/100km)") + xlab("Years") 
  

prix_marque= ddply(dst, .(Make), summarize, price=mean(MSRP), conso_moyenne=mean((City.l_100+Highway.l_100)/2))
#ggplot(prix_marque) + 
 # geom_histogram(stat = "identity", position = "identity",  aes(x=factor(price),y=conso_moyenne)) 

conso_gear= ddply(subset(dst,(dst$Transmission.Type=="MANUAL" | dst$Transmission.Type=="AUTOMATIC") & dst$Engine.HP < 250), 
                  .(Year,Transmission.Type), summarize, conso_moyenne=mean((City.l_100+Highway.l_100)/2))
ggplot(conso_gear) + 
  geom_histogram(stat = "identity", position = "identity",  aes(x=factor(Year),y=conso_moyenne, fill=factor(Transmission.Type))) 



#HP_PRICE= ddply(subset(dst,dst$MSRP>2500), .(Make), summarize, HP_moyenne=mean(Engine.HP),prix_moyen=mean(MSRP))

#ggplot(HP_PRICE) + 
 # geom_line(stat = "identity", position = "dodge",  aes(x=HP_moyenne,y=prix_moyen))


factor_marque = c("Volkswagen","Audi","BMW","Mercedes-Benz","Porsche","Bentley")
selected_type = c("Sedan","Coupe","4dr SUV")

voiture_rec = subset(dst, dst$Year>=2014 & dst$Vehicle.Style %in% selected_type & dst$Make %in% factor_marque)
#prix_marque_type = ddply(voiture_rec, .(Make,Vehicle.Style),summarize, prix=mean(MSRP))
ggplot(voiture_rec) + 
  geom_boxplot(position="dodge",aes(x = Vehicle.Style , y = Mixt.l_100 , fill=factor(Make, factor_marque, ordered=TRUE)))


#fuel_type_nmbr = ddply(dst,.(Year,Engine.Fuel.Type),summarise, Nombre = length(Engine.Fuel.Type),Perc = as.double(Nombre)/as.double(sum(Nombre)))

fuel_type_nmbr = ddply(dst,.(Year,Engine.Fuel.Type),summarise, Nombre = length(Engine.Fuel.Type))
#fuelpct=group_by(fuel_type_nmbr, Engine.Fuel.Type, Year) %>% summarise(Pct = Nombre/sum(fuel_type_nmbr$Nombre) * 100)
ggplot(fuel_type_nmbr) + 
  barplot(stat = "identity",  aes(x=factor(Year),y=(Nombre), fill=factor(Engine.Fuel.Type))) 
 

`%ni%` = Negate(`%in%`)
#transmission_nmbr = ddply(dst,.(Year,Transmission.Type),summarise, Nombre = (length(Transmission.Type)/length(Year))) 
#ggplot(transmission_nmbr) + 
 # geom_histogram(stat = "identity", position = "dodge",  aes(x=factor(Year),y=Nombre, fill=factor(Transmission.Type))) 
not_fuel = c("","Electric","natural gas")
fuel_consumption_per_gas = ddply(subset(non_sportive_cars,non_sportive_cars$Engine.Fuel.Type %ni% not_fuel),.(Engine.Fuel.Type),summarise, Consumption = mean(Mixt.l_100))
ggplot(fuel_consumption_per_gas) +
  geom_histogram(stat = "identity",  aes(x=factor(Engine.Fuel.Type),y=(Consumption)))
 

