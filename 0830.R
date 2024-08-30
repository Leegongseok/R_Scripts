#인스톨된 패키지
pkg=installed.packages()
pkg
#패키지 설치된 경로
.libPaths()
#데이터셋종류 확인
data(package="datasets")

#직사각형의 면적을 구하는 함수 정의 
getRectangleArea =function(w,h){
  area=w*h
  return(area)
}

w=20
h=35
#함수호출
getRectangleArea(w,h)

func2<-function(a,b){
  c<-a*(b+1)
  return(c)
}
 func2(2,3)
 func2(3,2)
 
 #3장 데이터 구조
 #벡터:선형으로 1개이상의 숫자 또는 문자열들을 저장가능
 #인덱스는 1부터 시작
 #여러개의 데이터를 저장할 때 c()함수 사용
 
 score=80
 score
 
 score=c(95,78,92)
 score #print(score)
 
 score[2]=88
 score[4]=100
 score
 
name=c("장발장","팡틴","코제트")
name

name[4]="자베르"

name

#연속적인 값들을 벡터에 저장
#sequence함수사용
x1=seq(10,100,by=10)
x1
#1씩 증가하는 값 저장
x2=1:10
seq(1,10,by=1)
x2
#1씩 감소하는 값 저장
x3=10:1
seq(1,10,by=1)
x3
#저장되는 값의 길이(개수)를 지정해서 동일하게 분배된 값을 저장
x4=seq(0,10,length.out=5)
x4

#반복(repeat)함수:rep(),times,each 속성
x5=c(1,2,3)
y1=rep(x5,times=3)
y1

x6=c(1,2,3)
y2=rep(x6,each=3)
y2

#연산자

#산술연산자

10^3

a=5
b=5

r1=a^b
r1

x=c(10,20,30,40)
y=c(1,6,11,16)
w=c(100,200)

x+5
x+y
#벡터 개수가 다를 때는 개수가 작은 쪽의 인덱스를 반복해서 연산한다
x+w

#비교연산자:비교연산자의 결과는 논리값 TRUE,  false로 결과가 나타남
a=7
b=10

a>b

x=c(10,20,30)
x<=10
#벡터에 있는 요소의 값들중에 하나라도 true가있으면 true
any(x<=10)
#벡터에 있는 요소의 값들중에 하나라도 false가있으면 false
all(x<=10)

#벡터에서 20 이상인 요소를 반환
x[x>=20]

#논리연산자:논리값 true,false로 결과를 반환

x=c(TRUE,TRUE,FALSE,FALSE)
y=c(TRUE,FALSE,TRUE,FALSE)

x|y
x&y
#둘이 같은값을 하면 FALSE,둘이 다르면 TRUE
xor(x,y)
!x

#NA(NOT Available(결측치):누락된경우)
#NULL(변수를 선언하고 초기화하지 않은 경우)
#INF(Infinity:0이아닌 숫자를 0으로 나누었을떄 )
#NAN(Not a Number:0을 0으로 나누었을떄)

Z=NULL
Z
is.null(Z)
k
is.null(k)

y=c(1,2,3,NA,5)
y

Z=5/0
Z

T=0/0
T

#요인(factor)
#벡터에 저장된 동일한 값들을 요인으로 찾아내서 대표값들을 분류해서 반환+원래 벡터값들

gender=c("여","남","남","여","여","남")
gender

gender.factor=factor(gender)
gender.factor