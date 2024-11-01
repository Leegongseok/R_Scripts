titles=html_nodes(html,"")%>%
  html_text()
titles

titles2=html_nodes(html,"")%>%
  html_text()
titles2

api=""

# api_key=""
# returnType="xml"
# 
# numofRows=10
# pageNo=1
# itemcode="PM10"
# dataGubun="HOUR"
# searchCondition="MONTH"
# 
# url=paste0(api,"?servicekey",api_key,
#            "&returnType",retyrnType
#            "&numofRows",numofRows,
#            "&pageNo",pageNo,
#            "& itemcode",itemcode,
#            "&dataGubun",dataGubun,
#            "&searchCondition",searchCondition
#            
#            )
url="https://apis.data.go.kr/B552584/ArpltnStatsSvc/getCtprvnMesureLIst?serviceKey=0DfIN2fYX4MUHuXtQfkbfi5QdOS%2FI5pedtZqZn8Bap6SKBvEeqFNpIeEpMaEGAPm3XYuKGH15NJKYGCzrXLx8A%3D%3D&returnType=xml&numOfRows=10&pageNo=1&itemCode=PM10&dataGubun=HOUR&searchCondition=MONTH"
url2=""

install.packages("XML")
install.packages("httr")
install.packages("xml2")
library(httr)
library(XML)
library(xml2)
response=GET(url)
content=content(response,"text")

content
xmlfile=xmlParse(content,asText = TRUE)
xmlfile
df=xmlToDataFrame(getNodeSet(xmlFile,"//items/item"))
df
content=content(response,"text")

xmlFile=xmlParse(content,asText=TRUE)
print(xmlFile)

#미세먼지 시간별 농도 그래프
library(ggplot2)
ggplot(data=df,aes(x=dataTime,y=seoul))+
  geom_bar(stat = "identity",fill="orange")+
  theme(axis.text.x = element_text(angle=90))+
  labs(title = "시간대별 서울지역의 미세먼지 농도 변화",x="측정일시",y="미세먼지농도(pM10)")

#지역별 미세먼지 농도의 지도 분포
#미세먼지 농도에 대한 데이터프레임 확인


df

#df에서 필요한 데이터만 추출
#제공되는 미세먼지 데이터 중에서 마지막 시간의 데이터가 1행이고 지역이 연속적이지 않기 때문에 아래와 같은 데이터 추출이 필요함
pm=df[1,c(1:16,19)]
pm

#지역별 미세먼지 데이터프레임의 행과 열을 바꾸기

pm.region=t(pm)
pm.region

#행렬데이터를 데이터프레임으로 변환
df.region=as.data.frame(pm.region)
df.region

#1로 설정된 컬럼이름을 PM10 컬럼명으로 변경
colnames(df.region)="PM10"
df.region



