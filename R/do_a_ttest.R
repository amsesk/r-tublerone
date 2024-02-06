#' @title do_a_ttest
#'
#' @description Performs a paired t-test, omitting NA values, on two groups from a column of a tibble.
#'
#' @param variable The column on which to perform the t-test.
#' @param our_data The tibble that contains the variable data and a `Treatment` variable with two groups.
#' @param group1 The first group.
#' @param group2 The second group.
#'
#' @return double
#'
#' @examples
#' our_data = tibble(variable = c(1,2,3,3,2,7,7,7,8,9), Treatment = c("T1", "T1", "T1", "T1", "T1", "T2", "T2", "T2", "T2", "T2"))
#' do_a_ttest("variable", our_data, "T1", "T2")
#'
#' @export

do_a_ttest = function(variable, our_data, group1, group2) {
  the_t_test = t.test(
    x = our_data %>%
      filter(Treatment == group1) %>%
      pull(variable),
    y = our_data %>%
      filter(Treatment == group2) %>%
      pull(variable),
    na.omit = TRUE,
    paired = TRUE,
    alternative = "two.sided"
  )

  return(the_t_test$p.value)
}
