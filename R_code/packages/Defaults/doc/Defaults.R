### R code from vignette source 'Defaults.Snw'

###################################################
### code chunk number 1: a
###################################################
hello<-"visible";.goodbye<-"hidden"
ls()

library(Defaults)
setDefaults('ls',all.names=TRUE)

ls()

ls(all.names=FALSE)
unDefaults(ls)
ls()


###################################################
### code chunk number 2: b
###################################################
getDefaults(ls)
getDefaults()
unsetDefaults(ls,confirm=FALSE)


###################################################
### code chunk number 3: c
###################################################
fun <- function(x=5,y=5) {
importDefaults("fun")
x * y
}
fun()
fun(x=1:5)


###################################################
### code chunk number 4: d
###################################################
setDefaults(fun,x=8,y=2)
fun()
fun(9)
fun(y=0.5)
unsetDefaults(fun,confirm=FALSE)
fun()


