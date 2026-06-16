import NetIncome
import Test.HUnit

runTestForOutputText :: Test -> IO String
runTestForOutputText theTest = do
    (_, testOutputAccumulator) <- runTestText putTextToShowS theTest
    let testOutput = testOutputAccumulator ""
    return testOutput
    
main :: IO ()
main = do
    testOutput <- runTestForOutputText allTests 
    writeFile "testOutput.txt" testOutput
    putStrLn testOutput
    return ()


allTests :: Test
allTests = TestList
    [

     -- test cases for studentLoan before August 2023
        TestCase (assertEqual "calculating student loan repayment when income is below the threshold" 0 (studentLoan 28470 27000)),
        TestCase (assertEqual "calculating student loan repayment when income is aboove the threshold" 2837.7 (studentLoan 28470 60000)),
        TestCase (assertEqual "calculating student loan repayment when income is equal to the threshold" 0 (studentLoan 28470 28470)),

     -- test cases for studentLoan after August 2023
        TestCase (assertEqual "calculating student loan repayment when income is below the threshold" 0 (studentLoan 25000 21000)),
        TestCase (assertEqual "calculating student loan repayment when income is above the threshold" 3150 (studentLoan 25000 60000)),
        TestCase (assertEqual "calculating student loan repayment when income is equal the threshold" 0 (studentLoan 25000 25000)),

     -- test cases for National Insurance Contributions
        TestCase (assertEqual "calculating National Insurance Contributions when gross annual income between £0 to £12,570" 0 (nationalInsuranceContri 12000)),
        TestCase (assertEqual "calculating National Insurance Contributions when gross annual income between £12,571 to £50,270" 1874.40 (nationalInsuranceContri 36000)),
        TestCase (assertEqual "calculating National Insurance Contributions when gross annual income above £50,270" 3810.6 (nationalInsuranceContri 90000)),

    -- test cases for Income Tax
        TestCase (assertEqual "calculating Income Tax when salary is less than personal allowance " 0 (incomeTax 12000)),
        TestCase (assertEqual "calculating Income Tax when taxable income is Basic rate " 3486 (incomeTax 30000)),
        TestCase (assertEqual "calculating Income Tax when taxable income is Higher rate " 11432 (incomeTax 60000)),
        TestCase (assertEqual "calculating Income Tax when gross annual income is between £100000 and 125140 " 39432 (incomeTax 120000)),
        TestCase (assertEqual "calculating Income Tax when gross annual income is above £125140 " 67203 (incomeTax 180000))
    ]