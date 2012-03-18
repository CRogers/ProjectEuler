package projecteuler
import scala.collection.mutable._

object P035 {

	def main(args: Array[String]) = {
		
		val primes = PrimeCalculator.load("primes-1e6.bin", 1000000)
		println(primes.size)
		val primesHash = new HashSet[Int]
		
		for(p <- primes)
			primesHash.add(p)
		
		val circularPrimes = new ArrayBuffer[Int]
		
		var i = 0
		while(primes(i) < 1e6.toInt) {
				
			val digits = Euler.getDigits(primes(i))
			val rots = Euler.rotations(digits).map(Euler.makeInt(_))
			val allPrime = rots.foldLeft(true)((res, a) => res && primesHash.contains(a))
			
			if(allPrime) {
				circularPrimes.append(primes(i))
				println(primes(i))
			}
		
			i += 1
		}
		
		println(circularPrimes.toList)	
		println(circularPrimes.size)	
	}
}