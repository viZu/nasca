var i = 4868929

val ints = i.toString.toCharArray.map(_ - '0')

ints.sum

var sum = 0

while (i > 0) {
  sum = sum + i % 10
  i = i / 10
}

sum