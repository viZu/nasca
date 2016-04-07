def isPrime(n: Int): Boolean = {
  if (n % 2 == 0) n == 2
  else if (n % 3 == 0) n == 3
  else {
    var step = 4
    val m: Double = Math.sqrt(n) + 1
    var i = 5
    var valid = true
    while (i < m && valid) {
      if (n % i == 0) {
        valid = false
      }
      step = 6 - step
      i = i + step
    }
    valid
  }
}

def nthPrime(n: Int): Int = {
  var candidate: Int = 2
  var count: Int = 0

  while (count < n) {
    if (isPrime(candidate)) {
      //println(candidate)
      count = count + 1
    }
    candidate = candidate + 1
  }

  candidate - 1
}

isPrime(17)
val t1 = System.currentTimeMillis()
nthPrime(188717)
println(System.currentTimeMillis() - t1)