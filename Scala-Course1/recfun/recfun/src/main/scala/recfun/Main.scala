package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  def pascal(c: Int, r: Int): Int =
  {
  
    if(c == 0 || c == r) 1
    else pascal(c-1, r-1) + pascal(c, r-1)
  }                                            

  
  def balance(chars: List[Char]): Boolean =
  {
    def helper(c : List[Char], left : Int, right : Int):  Boolean =
    {
      if(c.isEmpty) right == left
      else if(right > left) false
      else helper(c.tail, left + {if(c.head == '(') 1 else 0}, right + {if(c.head == ')') 1 else 0})
    }
    
    helper(chars, 0, 0)
  }  
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
  {
  	
  	if(money == 0)  return 1
  	if (coins.isEmpty || money < 0) return 0
  	var count : Int = 0
  	var temp : Int = 0
  	while(temp <= money) {
  		count += countChange(money-temp, coins.tail)
  		temp += coins.head
  	}
  	count
  } 
  }
