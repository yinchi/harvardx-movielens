Line 49 of 88-validation.Rmd reads:

    the_ecdf <- ecdf(predicted_ratings_FINAL_VALIDATION - validation$rating)
 
 It should instead be:
 
    the_ecdf <- ecdf(abs(predicted_ratings_FINAL_VALIDATION - validation$rating))

The error also affects the final plot in Section 4 in both the PDF and HTML versions of the report.
However, I have left all errors as is to preserve the project as submitted.

The final calculation in Section 4, the proportion of predictions within a half-star of the
actual rating, is unaffected by this error.
