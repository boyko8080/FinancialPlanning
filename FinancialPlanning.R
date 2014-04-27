#$$$# Retirement Planning Tool#$$$#
###################################

## Planning Starts As Of Today ##
## Prior wealth growth and wage growth is not considered
## Prior accomplishments are only expressed in the model in the form of starting capital

#Infraltion Rate
## not yet used

#Starting Wage
Wage0 = 75000
#Starting retirement savings
Retirement_Accnt0 = 25000

#Wage Growth Rate - annual
#Wage growth due to simple increases dictated by HR, counteract inflation
Wage_GrowthRate = .02

#Starting age
Age0 = 28

# When are you planning to Retire? 
# Could be the legal retirement age, could be earlier or later
# Retirement Age
RetirementAge0 = 67

#Career Growth - perturbations applied to wage vector based on expectation of promotions due
#to increase in responsibilities, changes in career, and then eventual decline in pre-retirement years
Career_Vector=c(1:RetirementAge0)
Career_Vector=Career_Vector*0
Career_Vector[30] = 15000 ## Promotion to managerial level at 30 (VP or DIR)
##Career_Vector[35] = 25000 ## Change of Functional Role or Industry to be aligned with business/sales/marketing
Career_Vector[40] = 15000 ## Promottion to executive level within organization (DIR or VP)
Career_Vector[45] = 25000 ## Promottion to executive level within organization (MD or SVP)
##Career_Vector[50] = 150000 ## Promottion to C-level within organization
##Career_Vector[55] = -150000 ## Change of career to advisory/consultant role
Career_Vector[60] = -40000 ## decrease participation in business, possible shift to education/non-profit, etc
plot(Career_Vector,type="s")

# Savings Rate 
# This is how much you are saving each year, as % of base salary (wage)
# Model will max pre-tax savings 
# So if savings < max pre-tax allowance then all savings will be directed to pre-tax accnt
# The rest will be directed to taxable accnt

SavingsRate0 = 0.06

# Savings growth rate - as you get older you save more
SavingsGrowthRate = 0.05

# You can only save so much, even when you are older...
# At most you'll afford to save
SavingsMaxRate = 0.3


#This is how many years left to save
SavingYears=RetirementAge0-Age0

Pre_Retirement_Age_Vector = c(1:SavingYears)
Pre_Retirement_Age_Vector
Pre_Retirement_Age_Vector[1]

## Start off with initial Savings Rate
## then growth the savings rate up to SavingsMaxRate
## increasing the SavingsRate by SavingsGrowthRate each year
SavingsRate_Vector = c(1:SavingYears)
SavingsRate_Vector[1] = SavingsRate0
for (i in 2:SavingYears){
  SavingsRate_Vector[i] = min(SavingsRate_Vector[i-1]*(1 + SavingsGrowthRate),SavingsMaxRate)
}
SavingsRate_Vector

## Start off with fixed growth expectation
Pre_Retirement_Growth_Vector = c(1:SavingYears)
for (i in 1:SavingYears){
Pre_Retirement_Growth_Vector[i] = .06
}
Pre_Retirement_Growth_Vector

##Start Off With Simple expected wage
## then add career growth into the equation
Expected_Wage_Vector = c(1:SavingYears)
Expected_Wage_Vector[1] = Wage0+Career_Vector[Age0]

for (i in 2:SavingYears){
  Expected_Wage_Vector[i] = Expected_Wage_Vector[i-1]*(1+Wage_GrowthRate)+Career_Vector[Age0+i-1]
}
Expected_Wage_Vector

# Initialize Retirement account
Retirement_Accnt_Vector = c(1:SavingYears)
## SaveContribution for debugging and analysis
Contribution_Vector = c(1:SavingYears)
Contribution_Vector[1] = Wage0*SavingsRate_Vector[1]

Retirement_Accnt_Vector = c(1:SavingYears)
Retirement_Accnt_Vector[1] = Retirement_Accnt0
for (i in 2:SavingYears){
  ## Growth Component = previous account value times the growthrate
  Retirement_Accnt_Vector[i] = Retirement_Accnt_Vector[i-1]*(1+Pre_Retirement_Growth_Vector[i])
  ## Additional Contribution Component = Future Wage Expectation time the savings rate
  Retirement_Accnt_Vector[i] = Retirement_Accnt_Vector[i] + Expected_Wage_Vector[i]*SavingsRate_Vector[i]
  Contribution_Vector[i] = Expected_Wage_Vector[i]*SavingsRate_Vector[i]
}

Contribution_Vector
Retirement_Accnt_Vector


plot(Age0:(RetirementAge0-1),Retirement_Accnt_Vector,type="l")
plot(Age0:(RetirementAge0-1),Expected_Wage_Vector,type="s")
plot(Age0:(RetirementAge0-1),Contribution_Vector,type="s")

Results_Mat = cbind(Contribution_Vector,Expected_Wage_Vector,Retirement_Accnt_Vector)
Line_Names_Vector=c("Contribution","Expected_Wage","Retirement_Accnt")
colnames(Results_Mat) <- Line_Names_Vector
matplot(Age0:(RetirementAge0-1),Results_Mat, type="l", lty=1, lwd=2,col = c("black", "red","green"), main = "Retirement Account, Wage, and Savings Growth" )
legend("topleft",Line_Names_Vector, col = c("black", "red", "green"), lwd = 1,title="legend", bg = "gray90")


matplot(Age0:(Age0+9),Results_Mat[1:10,2], type="l", lty=1, lwd=2,col = c("black", "red","green"), main = "Wage Growth - First 10 Years" )
legend("topleft",Line_Names_Vector[2], col = c("black", "red", "green"), lwd = 1,title="legend", bg = "gray90")

matplot((Age0+10):(Age0+19),Results_Mat[11:20,2], type="l", lty=1, lwd=2,col = c("black", "red","green"), main = "Wage Growth - Years 11 - 20 From Now" )
legend("topleft",Line_Names_Vector[2], col = c("black", "red", "green"), lwd = 1,title="legend", bg = "gray90")



########
#### To Do:
####
#### 1. Convert fixed rate to a portfolio and simulate expected return
#### 1.1 model rebalancing of portfolio from mostly equities in earlier years to mostly bonds in latter pre-retirement years
#### 2. Add tax rate consideration and split savings into pre-tax and post-tax components
#### 2.1 Add an easy option to force maxing out of pre-tax savings
#### 2.2 Add an option for business owners: individual 401K has higher pre-tax allowance than 401K
#### 3. Add  retirement component that illustrates withdrawal from retirement age until age 100
#### 3.1 Improve charts and presentation of out put, calibrate for export to PDF
#### 4. Convert portfolio from risky to mostly risk free during retirment then to perpetuity after age 90
#### 5. Add social security component and instructions on how to retrieve
#### 6. Add real estate considerations 
#### 7. Add budget component that will help direct funds to retirement and real estate components
#### 
########