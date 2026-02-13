attach(VAR)
> names(VAR) <- str_trim(names(VAR))
> names(VAR) <- gsub(" ", "_", names(VAR))
> VAR$Demonetization <- ifelse(VAR$DATE >= as.Date("2016-11-01") & VAR$DATE <= as.Date("2016-12-01"), 1, 0)
> VAR$COVID <- ifelse(VAR$DATE >= as.Date("2020-03-01") & VAR$DATE <= as.Date("2022-04-01"), 1, 0)
> 
> VAR$Demonetization
> VAR$COVID
> logCiC<- log(`Currency in Circulation`)
> logUPI<- log1p(UPI)
> adf.test(logCiC)
> d.logCiC<- diff(logCiC)
> adf.test(d.logCiC)
> adf.test(logUPI)
> d.logUPI<- diff(logUPI)
> adf.test(d.logUPI)
> adf.test(IIP)
> adf.test(CPI)
> adf.test(`REPO RATE`)
> d.reporate<- diff(`REPO RATE`)
> adf.test(d.reporate)
> d.repo<-diff(d.reporate)
> adf.test(d.repo)
> names(VAR)
> var_data <- dplyr::select(VAR_data,
+                           d.logCiC, d.logUPI, CPI, IIP, d.repo, Demonetization, COVID)
> head(var_data)
> VAR_data <- VAR_data %>%
+     mutate(
+         UPI = ifelse(UPI <= 0 | is.na(UPI), 1, UPI),
+         logUPI = log(UPI),
+         d.logUPI = c(NA, diff(logUPI))
+     )
> VAR_data$d.logUPI[!is.finite(VAR_data$d.logUPI)] <- NA
> VAR_data$d.logCiC[!is.finite(VAR_data$d.logCiC)] <- NA
> VAR_data$d.repo[!is.finite(VAR_data$d.repo)] <- NA
> var_data <- VAR_data %>%
+     dplyr::select(d.logCiC, d.logUPI, CPI, IIP, d.repo, Demonetization, COVID)
> 
> var_data <- na.omit(var_data)
> head(var_data)
> summary(var_data)
> lag_selection <- VARselect(var_data[, c("d.logCiC", "d.logUPI", "CPI", "IIP", "d.repo")],
+                            lag.max = 6, type = "const")
> lag_selection
> var_model <- vars::VAR(var_data[, c("d.logCiC", "d.logUPI", "CPI", "IIP", "d.repo")],
                       p = 2, type = "const",  exogen = var_data[, c("Demonetization", "COVID")])
> summary(var_model)
> roots(var_model, modulus = TRUE)
> causality(var_model, cause = "d.logUPI")
> causality(var_model, cause = "d.logCiC")
> irf_upi_cic <- irf(var_model, impulse = "d.logUPI", response = "d.logCiC", n.ahead = 10, boot = TRUE)
> plot(irf_upi_cic, main = "CIC Response to UPI Shock")
> 
> 
> irf_cic_upi <- irf(var_model, impulse = "d.logCiC", response = "d.logUPI", n.ahead = 10, boot = TRUE)
> plot(irf_cic_upi, main = "UPI Response to CIC Shock")
> 
> fevd_results <- fevd(var_model, n.ahead = 10)
> plot(fevd_results)
> 
> stargazer(var_model$varresult, type = "text", title = "VAR Model Results")
