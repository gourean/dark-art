
# Reproduction Script for CSV Special Character Handling in R

# 1. Create a dummy CSV with special characters in headers
csv_content <- "Group (A/B),Value %,Test-Name,Space Name\nA,10,Test1,Space1\nB,20,Test2,Space2"
csv_file <- "test_special_chars.csv"
writeLines(csv_content, csv_file)

# 2. Read with check.names = FALSE (Proposed fix)
df_fixed <- read.csv(csv_file, stringsAsFactors = TRUE, check.names = FALSE)
print("Column names with check.names = FALSE:")
print(names(df_fixed))

# 3. Test Formula Construction for Faceting
p_split <- "Group (A/B)"

# A. Unsafe (Current App Code)
print("Testing Unsafe Formula:")
tryCatch({
  f_unsafe <- as.formula(paste("~", p_split))
  print(f_unsafe)
}, error = function(e) {
  print(paste("Unsafe formula failed as expected:", e$message))
})

# B. Safe (Proposed Fix)
print("Testing Safe Formula:")
tryCatch({
  f_safe <- as.formula(paste0("~ `", p_split, "`"))
  print(f_safe)
}, error = function(e) {
  print(paste("Safe formula failed:", e$message))
})


# 6. Clean up
file.remove(csv_file)
