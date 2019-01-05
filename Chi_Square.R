chisq.test(claim1$BasePolicy,claim1$FraudFound_P)
#p-value = 0.001705 - Month #p-value = 2.196e-06 - Make #p-value = 4.057e-05 -AccidentArea#p-value = 3.003e-05 - MonthClaimed
#p-value = 0.0002399- Sex#p-value < 2.2e-16 - Fault#PolicyType -p-value < 2.2e-16#VehicleCategory - p-value < 2.2e-16
#VehiclePrice - p-value = 2.984e-13#Deductible - p-value = 1.297e-15#Days_Policy_Accident - p-value = 0.02084
#PastNumberOfClaims - p-value = 1.434e-11#AgeOfVehicle - p-value = 0.002613#AgeOfPolicyHolder - p-value = 6.151e-05
#PoliceReportFiled - p-value = 0.05951#AgentType - p-value = 0.006597#NumberOfSuppliments - p-value = 0.0004114
#AddressChange_Claim - p-value < 2.2e-16#Year - p-value = 0.008321#BasePolicy - p-value < 2.2e-16

# independent 2-group t-test
t.test(claim1$RepNumber~claim1$FraudFound_P) # where y is numeric and x is a binary factor

#data:  claim1$Age by claim1$FraudFound_P
t = 3.5984, df = 1035.7, p-value = 0.0003353
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  0.769112 2.613924
sample estimates:
  mean in group 0 mean in group 1 
39.95696        38.26544 

data:  claim1$RepNumber by claim1$FraudFound_P
t = 0.95067, df = 1046.8, p-value = 0.342
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  -0.1557984  0.4486398
sample estimates:
  mean in group 0 mean in group 1 
8.492033        8.345612 
