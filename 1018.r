install.packages("openxlsx")
install.packages("sf")
library(openxlsx)
library(sf)


df=read.xlsx("D:/R_Data/R_Script/행정경계데이터셋/국내지진목록.xlsx",sheet=1,startRow=4,colNames =FALSE)
head(df)

idx=grep("^북한",df$X8)

df[idx,'X8']

df=df[-idx,]

df

df[,6]=gsub("N","",df[,6])
df[,7]=gsub("E","",df[,7])

df[,6]=as.numeric(df[,6])
df[,7]=as.numeric(df[,7])


#행정 경계지도와 지진분포 출력
#shapefile 읽어오기
map=st_read("D:/R_Data/R_Script/행정경계데이터셋/Z_NGII_N3A_G0010000.shp")

#WGS84 좌표계로 변환
map=st_transform(map,crs=4326)

#포인트 데이터를 sf객체로 변환(포인트 출력)
df_sf =df%>%
  st_as_sf(coords=c("X7","X6"),crs=4326)

ggplot()+
  geom_sf(data=map,fill="white",alpha=0.5,color="black")+
  geom_sf(data=df_sf,aes(size=X3),shape=21,fill="red",alpha=0.3,color="black")+
  theme(legend.position = "none")+
  labs(title = "지진분포",x="경도",y="위도")