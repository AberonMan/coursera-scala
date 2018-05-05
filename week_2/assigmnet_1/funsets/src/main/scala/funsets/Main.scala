package funsets

object Main extends App {
  import funsets.FunSets._


  var ss1 = singletonSet(3)

  var ss2 = singletonSet(2)


  var ss3 = singletonSet(1)

  val superSet = union(union(ss1,ss2), ss3)


  printSet(superSet)

  printSet(map(superSet, (x:Int) => x *x ))
}
