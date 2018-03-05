
m <- as.matrix(
  data.frame(
    x1 = c(1, 11, 21),
    x2 = c(2, 12, 22),
    x3 = c(3, 13, 13)
  )
)
a <- 0
for (i in 1:3) {
  for (j in 1:3) {
    a <- a + m[i, i]
  }
}
