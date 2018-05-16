
var intList = 1000 :: 2 :: 70 :: 433 :: Nil
println(intList)

def isort(xs: List[Int]): List[Int] = xs match {
  case List() => List()
  case y :: ys => insert(y, isort(ys))
}

def insert(x:Int, xs:List[Int]):List[Int] = xs match {
  case List() => x::Nil
  case y::ys => if(y >= x) x::xs else y::insert(x, ys)
}

print(isort(intList))


