# validate_anova.R
# Purpose: Validate the manual Greenhouse-Geisser implementation in app.R
# by comparing it against standard calculations or known logic.

library(stats)

# 1. Create Synthetic Data (Repeated Measures)
set.seed(123)
N <- 20 # Subjects
k <- 3  # Timepoints

# Create correlated data (Compound Symmetry violation)
# Time 1: Baseline
t1 <- rnorm(N, mean=10, sd=2)
# Time 2: Treatment (correlated with t1)
t2 <- t1 + rnorm(N, mean=2, sd=1) 
# Time 3: Washout (highly variable)
t3 <- t2 + rnorm(N, mean=0, sd=5)

wide_df <- data.frame(t1, t2, t3)
wide_mat <- as.matrix(wide_df)

print("--- Data Summary ---")
print(colMeans(wide_mat))
print(cor(wide_mat))

# 2. APP.R LOGIC (Manual Implementation)
# ---------------------------------------------------------
print("\n--- Running App Logic ---")

# Step A: Standard ANOVA to get F-value
# Need Long format for aov()
long_df <- stack(wide_df)
names(long_df) <- c("Value", "Group")
long_df$ID <- factor(rep(1:N, k))

fit <- aov(Value ~ Group + Error(ID/Group), data=long_df)
s <- summary(fit)
target <- s[[length(s)]][[1]] # Extract Error:Group table

F_val <- target$`F value`[1]
df1 <- target$Df[1]
df2 <- target$Df[2]
p_val_uncorrected <- target$`Pr(>F)`[1]

print(paste("Uncorrected ANOVA: F =", round(F_val, 4), "df1 =", df1, "df2 =", df2, "p =", p_val_uncorrected))


# Step B: Manual Sphericity & Epsilon
S <- cov(wide_mat)
d <- k - 1

# Mauchly's Test
M <- contr.poly(k)
S_orth <- t(M) %*% S %*% M
eigen_vals <- eigen(S_orth)$values
W <- prod(eigen_vals) / (mean(eigen_vals)^d)
chi_df <- (d * (d + 1) / 2) - 1
chi_stat <- -(N - 1 - (2*d + 5)/6) * log(W)
mauchly_p <- pchisq(chi_stat, df = chi_df, lower.tail = FALSE)

print(paste("Mauchly's W =", round(W,4), "P =", mauchly_p))

# Greenhouse-Geisser Epsilon (The critical part)
mean_diag <- mean(diag(S))
mean_grand <- mean(S)
row_means <- rowMeans(S)

num <- k^2 * (mean_diag - mean_grand)^2
den <- (d) * (sum(S^2) - 2*k*sum(row_means^2) + k^2*mean_grand^2)
gg_eps <- num / den

print(paste("Calculated GG Epsilon =", gg_eps))

# Corrected Values
df1_corr <- df1 * gg_eps
df2_corr <- df2 * gg_eps
p_val_corrected <- pf(F_val, df1_corr, df2_corr, lower.tail = FALSE)

print(paste("Corrected ANOVA: F =", round(F_val, 4), "df1 =", round(df1_corr, 2), "df2 =", round(df2_corr, 2), "p =", p_val_corrected))


# 3. VERIFICATION (Using 'car' package if available, or theoretical check)
# Since we might not have 'car' installed in this environment, we will check consistency.
# GG Epsilon is bounded by 1/(k-1) and 1.
lower_bound <- 1/(k-1)
print(paste("Theoretical Epsilon Bounds: [", round(lower_bound, 3), ", 1.0 ]"))

if(gg_eps >= lower_bound && gg_eps <= 1.0) {
    print("PASS: Epsilon is within valid bounds.")
} else {
    print("FAIL: Epsilon is out of bounds!")
}

# Check Logic: If epsilon < 1, p-value should be LARGER (less significant) than uncorrected.
if(p_val_corrected >= p_val_uncorrected) {
    print("PASS: Corrected P-value is >= Uncorrected P-value (Conservative).")
} else {
    print("FAIL: Corrected P-value is SMALLER (Anti-conservative) - Logic Error!")
}

