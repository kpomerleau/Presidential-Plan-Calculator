

setwd("C:/Users/kep/Dropbox (Personal)/Cruz")

rm(list=ls()) 

#Load Functions

  source("avgfunctions.R")
  
####################################Calculations################################################

#functions for each tax law:

  #Current Law:

    CurrentLawTaxBurden<-function(income1, income2, children, married, hoh){
  
      stateincometax<-0 #Set to Zero
      
      #Step 7: Federal Taxable Income
      
      taxableincome<-FedTaxableIncome(income1, income2, children, married, hoh, stateincometax)
      
      #Step 8: Federal Income Tax
      
      federalincometaxBC<-FedIncomeTax(taxableincome,married,hoh)
      
      #Step 9: Child Tax Credit
      
      ctc<-FedCTC(income1, income2, children,married)
      
      #Earned Income Tax Credit
      
      eitc<-FedEITC(income1,income2,children,married)
      
      #Step 10: Federal Income Tax after credits
      
      federalincometax<-federalincometaxBC-ctc-eitc
      
      #Step 12: Employee Payroll Taxes
      
      employeepayrolltax1<-FedEmployeePayroll(income1)
      employeepayrolltax2<-FedEmployeePayroll(income2)
      employeepayrolltax<-employeepayrolltax1+employeepayrolltax2
      
      #Medicare Surtax
      
      medsurtax<-MedSurtax(income1,income2,married)
      
      #AMT
      
      amt<-AMT(income1, income2, married)
      
        federalincometax<-max(amt,federalincometax)
      
      #Tax Bill
      
      taxburden<-federalincometax+employeepayrolltax+medsurtax
      
      #Step 13: Employer Payroll Taxes
      
      employerpayrolltax1<-FedEmployerPayroll(income1)
      employerpayrolltax2<-FedEmployerPayroll(income2)
      employerpayrolltax<-employerpayrolltax1+employerpayrolltax2
      
      #Step 14: Total Tax Wedge
      
      taxwedge<-federalincometax+employeepayrolltax+employerpayrolltax+medsurtax
      
      income<-income1+income2
      
      return(c(income,
               taxableincome,
               federalincometaxBC,
               ctc,
               eitc,
               federalincometax,
               employeepayrolltax+medsurtax,
               taxburden,
               employerpayrolltax,
               taxwedge)
             )
      
    }

  #Ted Cruz Tax Plan:
    
    CruzTaxBurden<-function(income1, income2, children, married, hoh){
  
      stateincometax<-0 #Set to Zero
      
      #adjust income for cruz:
      
      income1<-income1+FedEmployerPayroll(income1)
      income2<-income2+FedEmployerPayroll(income2)
      
      #apply VAT to income
      
      employerpayrolltax1<-income1*fedtax$vatrate[1]
      employerpayrolltax2<-income2*fedtax$vatrate[1]
      
      income1<-income1-employerpayrolltax1
      income2<-income2-employerpayrolltax2
      
      employerpayrolltax<-employerpayrolltax1+employerpayrolltax2
      
      #Step 7: Federal Taxable Income
      
      taxableincome<-FedTaxableIncome(income1, income2, children, married, hoh, stateincometax)
      
      #Step 8: Federal Income Tax
      
      federalincometaxBC<-FedIncomeTax(taxableincome,married,hoh)
      
      #Step 9: Child Tax Credit
      
      ctc<-FedCTC(income1, income2, children,married)
      
      #Earned Income Tax Credit
      
      eitc<-FedEITC(income1,income2,children,married)
      
      #Step 10: Federal Income Tax after credits
      
      federalincometax<-federalincometaxBC-ctc-eitc
      
      #Step 12: Employee Payroll Taxes
      
        #No employee payroll tax
        
          employeepayrolltax<-0
      
      #Medicare Surtax
      
      medsurtax<-MedSurtax(income1,income2,married)
      
      #Tax Bill
      
      taxburden<-federalincometax+employeepayrolltax+medsurtax
          
      #Step 14: Total Tax Wedge
      
      taxwedge<-federalincometax+employeepayrolltax+employerpayrolltax+medsurtax
      
      income<-income1+income2
      
      return(c(income,
               taxableincome,
               federalincometaxBC,
               ctc,
               eitc,
               federalincometax,
               employeepayrolltax+medsurtax,
               taxburden,
               employerpayrolltax,
               taxwedge)
      )
      
    }

  #Rubio Tax Plan

    RubioTaxBurden<-function(income1, income2, children, married, hoh){
      
      stateincometax<-0 #Set to Zero
      
      #Step 7: Federal Taxable Income
      
      taxableincome<-FedTaxableIncome(income1, income2, children, married, hoh, stateincometax)
      
      #Step 8: Federal Income Tax
      
      federalincometaxBC<-FedIncomeTax(taxableincome,married,hoh)
      
      #Step 9: Child Tax Credit
      
      ctc<-FedCTC(income1, income2, children,married)
      
      #New Rubio Child Tax Credit
      
      rubioctc<-RubioCTC(income1,income2,children,married)
      
        ctc<-ctc+rubioctc
      
      #Earned Income Tax Credit
      
      eitc<-FedEITC(income1,income2,children,married)
      
      #Step 10: Federal Income Tax after credits
      
      federalincometax<-federalincometaxBC-ctc-eitc
      
      #Step 12: Employee Payroll Taxes
      
      employeepayrolltax1<-FedEmployeePayroll(income1)
      employeepayrolltax2<-FedEmployeePayroll(income2)
      employeepayrolltax<-employeepayrolltax1+employeepayrolltax2
      
      #Medicare Surtax
      
      medsurtax<-MedSurtax(income1,income2,married)
      
      #AMT
      
      amt<-AMT(income1, income2, married)
      
      federalincometax<-max(amt,federalincometax)
      
      #apply the per-person credit
      
      federalincometax<-federalincometax-(2000*(1+married+children))
      
      #Tax Bill
      
      taxburden<-federalincometax+employeepayrolltax+medsurtax
      
      #Step 13: Employer Payroll Taxes
      
      employerpayrolltax1<-FedEmployerPayroll(income1)
      employerpayrolltax2<-FedEmployerPayroll(income2)
      employerpayrolltax<-employerpayrolltax1+employerpayrolltax2
      
      #Step 14: Total Tax Wedge
      
      taxwedge<-federalincometax+employeepayrolltax+employerpayrolltax+medsurtax
      
      income<-income1+income2
      
      return(c(income,
               taxableincome,
               federalincometaxBC,
               ctc,
               eitc,
               federalincometax,
               employeepayrolltax+medsurtax,
               taxburden,
               employerpayrolltax,
               taxwedge)
      )
      
    }

  #Trump Tax Plan

    TrumpTaxBurden<-function(income1, income2, children, married, hoh){
  
        stateincometax<-0 #Set to Zero
        
        #Step 7: Federal Taxable Income
        
        taxableincome<-FedTaxableIncome(income1, income2, children, married, hoh, stateincometax)
        
        #Step 8: Federal Income Tax
        
        federalincometaxBC<-FedIncomeTax(taxableincome,married,hoh)
        
        #Step 9: Child Tax Credit
        
        ctc<-FedCTC(income1, income2, children,married)
        
        #Earned Income Tax Credit
        
        eitc<-FedEITC(income1,income2,children,married)
        
        #Step 10: Federal Income Tax after credits
        
        federalincometax<-federalincometaxBC-ctc-eitc
        
        #Step 12: Employee Payroll Taxes
        
        employeepayrolltax1<-FedEmployeePayroll(income1)
        employeepayrolltax2<-FedEmployeePayroll(income2)
        employeepayrolltax<-employeepayrolltax1+employeepayrolltax2
        
        #Medicare Surtax
        
        medsurtax<-MedSurtax(income1,income2,married)
        
        #Tax Bill
        
        taxburden<-federalincometax+employeepayrolltax+medsurtax
        
        #Step 13: Employer Payroll Taxes
        
        employerpayrolltax1<-FedEmployerPayroll(income1)
        employerpayrolltax2<-FedEmployerPayroll(income2)
        employerpayrolltax<-employerpayrolltax1+employerpayrolltax2
        
        #Step 14: Total Tax Wedge
        
        taxwedge<-federalincometax+employeepayrolltax+employerpayrolltax+medsurtax
        
        income<-income1+income2
        
        return(c(income,
                 taxableincome,
                 federalincometaxBC,
                 ctc,
                 eitc,
                 federalincometax,
                 employeepayrolltax+medsurtax,
                 taxburden,
                 employerpayrolltax,
                 taxwedge)
        )
        
      }

  #Clinton Tax Plan

    ClintonTaxBurden<-function(income1, income2, children, married, hoh){
      
      stateincometax<-0 #Set to Zero
      
      #Step 7: Federal Taxable Income
      
      taxableincome<-FedTaxableIncome(income1, income2, children, married, hoh, stateincometax)
      
      #Step 8: Federal Income Tax
      
      federalincometaxBC<-FedIncomeTax(taxableincome,married,hoh)
      
      #Step 9: Child Tax Credit
      
      ctc<-FedCTC(income1, income2, children,married)
      
      #Earned Income Tax Credit
      
      eitc<-FedEITC(income1,income2,children,married)
      
      #Step 10: Federal Income Tax after credits
      
      federalincometax<-federalincometaxBC-ctc-eitc
      
      #Step 12: Employee Payroll Taxes
      
      employeepayrolltax1<-FedEmployeePayroll(income1)
      employeepayrolltax2<-FedEmployeePayroll(income2)
      employeepayrolltax<-employeepayrolltax1+employeepayrolltax2
      
      #Medicare Surtax
      
      medsurtax<-MedSurtax(income1,income2,married)
      
      #AMT
      
      amt<-AMT(income1, income2, married)
        
      federalincometax<-max(amt,federalincometax)
      
      #Buffett Rule
      
        if(income1+income2 >= 1000000){
        
          buffett<-((income1+income2)*0.3)*min((((income1+income2)-1000000)/1000000),1)
        
        } else {
          
          buffett <- 0
          
        }
      
          if(federalincometax+employeepayrolltax < buffett){
            
            federalincometax<-federalincometax+(buffett-employeepayrolltax)
            
          }
      
      #Tax Bill
      
      taxburden<-federalincometax+employeepayrolltax+medsurtax
      
      #Step 13: Employer Payroll Taxes
      
      employerpayrolltax1<-FedEmployerPayroll(income1)
      employerpayrolltax2<-FedEmployerPayroll(income2)
      employerpayrolltax<-employerpayrolltax1+employerpayrolltax2
      
      #Step 14: Total Tax Wedge
      
      taxwedge<-federalincometax+employeepayrolltax+employerpayrolltax+medsurtax
      
      income<-income1+income2
      
      return(c(income,
               taxableincome,
               federalincometaxBC,
               ctc,
               eitc,
               federalincometax,
               employeepayrolltax+medsurtax,
               taxburden,
               employerpayrolltax,
               taxwedge)
      )
      
    }

  #Sanders Tax Plan

    SandersTaxBurden<-function(income1, income2, children, married, hoh){
      
      stateincometax<-0 #Set to Zero
      
      #Need to adjust wages downward for new employer-side payroll taxes
      
      #new healthcare and paid leave employer-side tax tax:
      
        healthcare1<-income1*(fedtax$healthcarepayrolltax[1]+fedtax$employerleave[1])
        healthcare2<-income2*(fedtax$healthcarepayrolltax[1]+fedtax$employerleave[1])
        
      #new SS tax       

        newss1<-SandersSStax(income1)
        newss2<-SandersSStax(income2)

      #income adjustments

        income1<-income1-healthcare1-newss1
        income2<-income2-healthcare2-newss2
      
      taxableincome<-FedTaxableIncome(income1, income2, children, married, hoh, stateincometax)
      
      #Step 8: Federal Income Tax and surtax
      
      surtax<-SandersSurtax(income1,income2,married,hoh)
      
      federalincometaxBC<-FedIncomeTax(taxableincome,married,hoh)+surtax
      
      #Step 9: Child Tax Credit
      
      ctc<-FedCTC(income1, income2, children,married)
      
      #Earned Income Tax Credit
      
      eitc<-FedEITC(income1,income2,children,married)
      
      #Step 10: Federal Income Tax after credits
            
      federalincometax<-federalincometaxBC-ctc-eitc
      
      #Step 12: Employee Payroll Taxes
      
      employeepayrolltax1<-FedEmployeePayroll(income1)
      employeepayrolltax2<-FedEmployeePayroll(income2)
      employeepayrolltax<-employeepayrolltax1+employeepayrolltax2
      
      #Medicare Surtax
      
      medsurtax<-MedSurtax(income1,income2,married)
            
      #Tax Bill
      
      taxburden<-federalincometax+employeepayrolltax+medsurtax
      
      #Step 13: Employer Payroll Taxes
      
      employerpayrolltax1<-FedEmployerPayroll(income1)+healthcare1+newss1
      employerpayrolltax2<-FedEmployerPayroll(income2)+healthcare2+newss2
      
      employerpayrolltax<-employerpayrolltax1+employerpayrolltax2
      
      #Step 14: Total Tax Wedge
      
      taxwedge<-federalincometax+employeepayrolltax+employerpayrolltax+medsurtax
      
      income<-income1+income2
      
      return(c(income,
               taxableincome,
               federalincometaxBC,
               ctc,
               eitc,
               federalincometax,
               employeepayrolltax+medsurtax,
               taxburden,
               employerpayrolltax,
               taxwedge)
      )
      
    }

  #Trump New Tax Plan
    
    TrumpNewTaxBurden<-function(income1, income2, children, married, hoh){
      
      stateincometax<-min(0,100000*(married+1)) #Set to Zero
      
      #Step 7: Federal Taxable Income
      
      taxableincome<-FedTaxableIncome(income1, income2, children, married, hoh, stateincometax)
      
      #Step 8: Federal Income Tax
      
      federalincometaxBC<-FedIncomeTax(taxableincome,married,hoh)
      
      #Step 9: Child Tax Credit
      
      ctc<-FedCTC(income1, income2, children,married)
      
      #Earned Income Tax Credit
      
      eitc<-FedEITC(income1,income2,children,married)
      
      #Step 10: Federal Income Tax after credits
      
      federalincometax<-federalincometaxBC-ctc-eitc
      
      #Step 12: Employee Payroll Taxes
      
      employeepayrolltax1<-FedEmployeePayroll(income1)
      employeepayrolltax2<-FedEmployeePayroll(income2)
      employeepayrolltax<-employeepayrolltax1+employeepayrolltax2
      
      #Medicare Surtax
      
      medsurtax<-MedSurtax(income1,income2,married)
      
      #Tax Bill
      
      taxburden<-federalincometax+employeepayrolltax+medsurtax
      
      #Step 13: Employer Payroll Taxes
      
      employerpayrolltax1<-FedEmployerPayroll(income1)
      employerpayrolltax2<-FedEmployerPayroll(income2)
      employerpayrolltax<-employerpayrolltax1+employerpayrolltax2
      
      #Step 14: Total Tax Wedge
      
      taxwedge<-federalincometax+employeepayrolltax+employerpayrolltax+medsurtax
      
      income<-income1+income2
      
      return(c(income,
               taxableincome,
               federalincometaxBC,
               ctc,
               eitc,
               federalincometax,
               employeepayrolltax+medsurtax,
               taxburden,
               employerpayrolltax,
               taxwedge)
      )
      
    }
    
##############Taxpayer Parameters###########

children<-0
married<-1

  #Are you head of household? If you are single and have any children, yes, otherwise no:
                         
    if(married == 0 & children > 0){
      
      hoh <- 1
      
    } else {
      
      hoh <- 0
      
    }

income1<-100000
income2<-500000

####Current Law Calculations

fedtax<-read.csv("fedtax.csv", header = TRUE, fill = TRUE, sep = ",")

currentlaw<-data.frame(CurrentLawTaxBurden(income1,income2, children, married, hoh))
rownames(currentlaw)<-c('Income','Taxable Income', 'Federal Income Tax', 'Child Tax Credit','EITC', 'Federal Income Tax After Credits', 'Employee Payroll Tax', 'Tax Burden','Employer Payroll Tax','Tax Wedge')
colnames(currentlaw)<-c('Amount')

#Cruz Calculations

fedtax<-read.csv("cruz.csv", header = TRUE, fill = TRUE, sep = ",")

cruzlaw<-data.frame(CruzTaxBurden(income1, income2, children, married, hoh))
rownames(cruzlaw)<-c('Income','Taxable Income', 'Federal Income Tax', 'Child Tax Credit','EITC', 'Federal Income Tax After Credits', 'Employee Payroll Tax','Tax Burden', 'Employer Payroll Tax', 'Tax Wedge')
colnames(cruzlaw)<-c('Amount')

#Rubio Calculations

fedtax<-read.csv("rubio.csv", header = TRUE, fill = TRUE, sep = ",")

rubiolaw<-data.frame(RubioTaxBurden(income1, income2, children, married, hoh))

rownames(rubiolaw)<-c('Income','Taxable Income', 'Federal Income Tax', 'Child Tax Credit','EITC', 'Federal Income Tax After Credits', 'Employee Payroll Tax','Tax Burden', 'Employer Payroll Tax', 'Tax Wedge')
rownames(rubiolaw)<-c('Income','Taxable Income', 'Federal Income Tax', 'Child Tax Credit','EITC', 'Federal Income Tax After Credits', 'Employee Payroll Tax','Tax Burden', 'Employer Payroll Tax', 'Tax Wedge')
colnames(rubiolaw)<-c('Amount')

#Trump Calculations

fedtax<-read.csv("trump.csv", header = TRUE, fill = TRUE, sep = ",")

trumplaw<-data.frame(TrumpTaxBurden(income1, income2, children, married, hoh))

rownames(trumplaw)<-c('Income','Taxable Income', 'Federal Income Tax', 'Child Tax Credit','EITC', 'Federal Income Tax After Credits', 'Employee Payroll Tax','Tax Burden', 'Employer Payroll Tax', 'Tax Wedge')
rownames(trumplaw)<-c('Income','Taxable Income', 'Federal Income Tax', 'Child Tax Credit','EITC', 'Federal Income Tax After Credits', 'Employee Payroll Tax','Tax Burden', 'Employer Payroll Tax', 'Tax Wedge')
colnames(trumplaw)<-c('Amount')

#clinton

fedtax<-read.csv("clinton.csv", header = TRUE, fill = TRUE, sep = ",")

clintonlaw<-data.frame(ClintonTaxBurden(income1, income2, children, married, hoh))

rownames(clintonlaw)<-c('Income','Taxable Income', 'Federal Income Tax', 'Child Tax Credit','EITC', 'Federal Income Tax After Credits', 'Employee Payroll Tax','Tax Burden', 'Employer Payroll Tax', 'Tax Wedge')
rownames(clintonlaw)<-c('Income','Taxable Income', 'Federal Income Tax', 'Child Tax Credit','EITC', 'Federal Income Tax After Credits', 'Employee Payroll Tax','Tax Burden', 'Employer Payroll Tax', 'Tax Wedge')
colnames(clintonlaw)<-c('Amount')

#Sanders

fedtax<-read.csv("sanders.csv", header = TRUE, fill = TRUE, sep = ",")

sanderslaw<-data.frame(SandersTaxBurden(income1, income2, children, married, hoh))

rownames(sanderslaw)<-c('Income','Taxable Income', 'Federal Income Tax', 'Child Tax Credit','EITC', 'Federal Income Tax After Credits', 'Employee Payroll Tax','Tax Burden', 'Employer Payroll Tax', 'Tax Wedge')
rownames(sanderslaw)<-c('Income','Taxable Income', 'Federal Income Tax', 'Child Tax Credit','EITC', 'Federal Income Tax After Credits', 'Employee Payroll Tax','Tax Burden', 'Employer Payroll Tax', 'Tax Wedge')
colnames(sanderslaw)<-c('Amount')

#Trump's New Tax Plan

fedtax<-read.csv("trumpnew.csv", header = TRUE, fill = TRUE, sep = ",")

trumpnewlaw<-data.frame(TrumpNewTaxBurden(income1, income2, children, married, hoh))

rownames(trumpnewlaw)<-c('Income','Taxable Income', 'Federal Income Tax', 'Child Tax Credit','EITC', 'Federal Income Tax After Credits', 'Employee Payroll Tax','Tax Burden', 'Employer Payroll Tax', 'Tax Wedge')
rownames(trumpnewlaw)<-c('Income','Taxable Income', 'Federal Income Tax', 'Child Tax Credit','EITC', 'Federal Income Tax After Credits', 'Employee Payroll Tax','Tax Burden', 'Employer Payroll Tax', 'Tax Wedge')
colnames(trumpnewlaw)<-c('Amount')


#final output:

final<-cbind(currentlaw,clintonlaw[,1],cruzlaw[,1],rubiolaw[,1],sanderslaw[,1],trumplaw[,1], trumpnewlaw[,1])

colnames(final)<-c('current law','clinton','cruz','rubio','sanders','trump','trumpnew')
