module NetIncome where

rate = 0.09

studentLoan :: Double -> Double -> Double
studentLoan threshold salary = if salary <= threshold
                                then 0
                                else (salary - threshold) * rate



pointX = 12570
pointY = 50270
rate2 = 0.08
rate3 = 0.02

nationalInsuranceContri :: Double -> Double
nationalInsuranceContri grossAnnualIncome = if grossAnnualIncome <= pointX
                                            then 0
                                            else if grossAnnualIncome <= pointY
                                            then (grossAnnualIncome-pointX) * rate2
                                            else(pointY-pointX) * rate2 + (grossAnnualIncome-pointY) * rate3



personalAllowance = 12570
basic = 37700
allowanceReductionPoint = 100000
additional = 125140
basicRate = 0.2
higherRate = 0.4
additionalRate = 0.45

incomeTax :: Double -> Double
incomeTax salary= if salary <= personalAllowance 
                    then 0
                  else if salary <= allowanceReductionPoint
                     then 
                     if (salary - personalAllowance) <= basic
                     then (salary - personalAllowance) * basicRate
                     else (basic * basicRate)+((salary - personalAllowance - basic) * higherRate)

                  else if salary < additional
                    then
                    let personalAllow = personalAllowance - ((salary - allowanceReductionPoint)/2)
                    in
                        if(salary - personalAllow) <= basic
                        then(salary - personalAllow) * basicRate
                        else(basic * basicRate) + ((salary - personalAllow - basic) * higherRate)

                  else 
                    (basic * basicRate)+((additional - basic) * higherRate)+((salary - additional) * additionalRate)



finalIncome repaymentThreshold income = income - studentLoan repaymentThreshold income - nationalInsuranceContri income - incomeTax income