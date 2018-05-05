object exercise {

  def sdfds(f:Int =>Int ) =  y=>( y + x/y)

  def product(f: Int => Int)(a:Int, b:Int): Int  = unitFunction(f)(a,b)((x:Int,y:Int) =>x*y, 1)]





  def factorial(number : Int): Int = if(number == 0) 1 else product(x =>x)(1,  number)

  def unitFunction(f:Int =>Int)(a:Int, b:Int)(combine:(Int, Int) => Int, unitValue:Int):Int =
    if (a > b) unitValue
    else combine(f(a), unitFunction(f)(a+1,b)(combine, unitValue))

  factorial(0)
  factorial(1)
  factorial(2)
  factorial(3)
  factorial(5)
  unitFunction(x=>x)(1,5)((a,b) =>a*b,1)
}