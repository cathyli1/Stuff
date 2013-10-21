#1
w<-sapply(1:100, function(x) {if(x-x%/%3*3==0&x-x%/%5*5==0) "FizzBuzz"
                          else 
                           if (x-x%/%3*3==0&(x-x%/%5*5!=0)) "Fizz"
                          else
                                if ((x-x%/%3*3!=0)&x-x%/%5*5==0) "Buzz"
                          else x})
#2
x<-runif(1000,min=0,max=2*pi)
y<-runif(1000,min=0,max=1)
u<-y*cos(x)
v<-y*sin(x)
plot(u,v,cex=.5)
plot(sqrt(u^2+v^2))

#3
sente<-"Hello, my name is Bob. I am a statistician. I like statistics very much."
sente_split<-unlist(strsplit(sente,split=""))
for(i in 1:72)
{write(sente_split[i],file=paste("out_",i,".txt",sep=""))}
a=""
for(i in 1:72)
{
  if(class(try(read.table(paste("out_",i,".txt",sep="")),silent=TRUE))=="try-error")
    {b<-" "}
  else
  {b<-as.matrix(read.table(paste("out_",i,".txt",sep="")))}
    a<-paste(a,b,sep="")
}
a