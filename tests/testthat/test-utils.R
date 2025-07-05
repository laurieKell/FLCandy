# Test utility functions

test_that("from_logits works correctly", {
  # Test basic functionality
  expect_equal(from_logits(0), 0.6, tolerance = 1e-6)
  expect_equal(from_logits(1), 0.7998 / (1 + exp(-1)) + 0.2001, tolerance = 1e-6)
  
  # Test vector input
  input <- c(0, 1, 2)
  result <- from_logits(input)
  expect_length(result, 3)
  expect_true(all(result >= 0.2001 & result <= 1.0))
  
  # Test error handling
  expect_error(from_logits("not numeric"), "logit_h must be numeric")
})

test_that("to_logits works correctly", {
  # Test basic functionality
  expect_equal(to_logits(0.6), 0, tolerance = 1e-6)
  
  # Test vector input
  input <- c(0.3, 0.5, 0.8)
  result <- to_logits(input)
  expect_length(result, 3)
  
  # Test error handling
  expect_error(to_logits("not numeric"), "h must be numeric")
  expect_error(to_logits(0.1), "h must be between 0.2001 and 1.0")
  expect_error(to_logits(1.1), "h must be between 0.2001 and 1.0")
})

test_that("logit transformations are reversible", {
  # Test that to_logits and from_logits are inverse functions
  original <- c(0.3, 0.5, 0.8)
  transformed <- to_logits(original)
  back_transformed <- from_logits(transformed)
  
  expect_equal(original, back_transformed, tolerance = 1e-6)
}) 