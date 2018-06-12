val first = List(1,2,3,4,213)
val second = List(1,2,3,4,213)
val third = List(1,2,3,4,213)


for(x <- first; y<-second; z <- third; if z > 4 ) yield x::y::z::Nil

first flatMap(x => second.flatMap(y => third.filter(_ > 4).map(z =>x::y::z::Nil )))

