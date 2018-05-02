def notTailFactorial(number: Int): Int = {
  if (number == 0) 1 else number * notTailFactorial(number - 1)
}
notTailFactorial(0)
notTailFactorial(3)
notTailFactorial(2)
notTailFactorial(15)


tailFunction(0)
tailFunction(3)
tailFunction(2)
tailFunction(15)


def tailFunction(numeric: Int):Int={

  def accFactorialFun(acc:Int, number:Int): Int ={
    if (number == 0) acc
    else accFactorialFun(acc*number, number-1)
  }

  accFactorialFun(1, numeric)
}
