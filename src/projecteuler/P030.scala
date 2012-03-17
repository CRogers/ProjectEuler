package projecteuler

object P030 {
	
	def main(args: Array[String]) = {
		
		var total = 0
		
		for(i <- 4 to Int.MaxValue) {
			val sum = Euler.getDigits(i).map(pow5).sum
			if(sum == i) {
				total += i
				println(total)
			}
		}
	}
	
	def pow5(i: Int): Int = {
		val i2 = i*i
		i2*i2*i
	}
	
}