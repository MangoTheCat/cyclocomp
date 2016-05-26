
context("cyclocomp_q")

test_that("cyclocomp_q works the same as cyclocomp + quote", {

  
  expect_equal(
    cyclocomp_q(if (TRUE) "foo" else if(FALSE) "bar" else "baz"), 
    cyclocomp(quote(if (TRUE) "foo" else if(FALSE) "bar" else "baz")))

  expect_equal(
    cyclocomp_q(while(condition && another_condition) if(something) do_something else break), 
    cyclocomp(quote(while(condition && another_condition) if(something) do_something else break)))

})

