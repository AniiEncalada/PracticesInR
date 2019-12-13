x<-3
my_func <- function(y){
 x<-5
 y+5
}
print(x)
compute_s_n <- function(n){
  val<-0
  for (i in 1:n) {
    val <- val + i^2
  }
  val
}
compute_s_n(10)
s_n <- vector('numeric', 25)
for(i in 1:25){
  s_n[i]<-compute_s_n(i)
}
print(s_n)
s_n <- map_dbl(1:25, compute_s_n)
print(s_n)
