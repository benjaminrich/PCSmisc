require(PCSmisc)


sex.mapping <- mapping(c("Male", "Female"), c(1, 0))
sex.inverse.mapping <- attr(sex.mapping, "inverse")
sex.mapping("Male")
sex.mapping("Female")
sex.mapping(c("Female", "Female", "Male", "Female"))
class(sex.mapping(c("Female", "Female", "Male", "Female")))
sex.mapping("Unknown")
sex.inverse.mapping(0)
sex.inverse.mapping(1)
sex.inverse.mapping(c(1, 0, 0, 1, 0))
class(sex.inverse.mapping(c(1, 0, 0, 1, 0)))
sex.inverse.mapping(5)

m1 <- mapping(c("E", "H", "F"), c("A", "B", "E"))
m1.inv <- attr(m1, "inverse")
set.seed(1232)
x <- sample(LETTERS[1:8], 1000, replace=TRUE)
table(x, m1(x), useNA="always")
table(x, m1.inv(x), useNA="always")
table(x, m1.inv(m1(x)), useNA="always")
table(x, m1(m1.inv(x)), useNA="always")


m2 <- mapping(c("E", "H", "E", "F"), c("A", "B", "B", "E"))
m2.inv <- attr(m2, "inverse")
table(x, m2(x), useNA="always")
table(x, m2.inv(x), useNA="always")
table(x, m2.inv(m2(x)), useNA="always")
table(x, m2(m2.inv(x)), useNA="always")

