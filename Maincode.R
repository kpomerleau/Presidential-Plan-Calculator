

setwd("C:/Users/kep/Dropbox (Personal)/Cruz")

rm(list=ls()) 

#Load Functions

  source("avgfunctions.R")
  
####################################Calculations################################################

#functions for each tax law:

  #Current Law:

    CurrentLawTaxBurden<-function(income1, income2, children, children5, married, hoh, itemizeddeductions){
  
      #Step 7: Federal Taxable Income
      
      taxableincome<-FedTaxableIncome(income1, income2, children+children5, married, hoh, itemizeddeductions)
      
      #Step 8: Federal Income Tax
      
      federalincometaxBC<-FedIncomeTax(taxableincome,married,hoh)
      
      #Step 9: Child Tax Credit
      
      ctc<-FedCTC(income1, income2, children+children5,married)
      
      #Earned Income Tax Credit
      
      eitc<-FedEITC(income1,income2,children+children5,married)
      
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

  #Clinton Tax Plan

    ClintonTaxBurden<-function(income1, income2, children, children5, married, hoh, itemizeddeductions){
      
      #Step 7: Federal Taxable Income
      
      taxableincome<-FedTaxableIncome(income1, income2, children+children5, married, hoh, itemizeddeductions)
      
      #Step 8: Federal Income Tax
      
        #28% cap on itemized deductions
      
          Limited<-itemizeddeductions*(1-.19) + min((income1+income2)*0.11,30000)
          
          LimitedIncome<-taxableincome + Limited - fedtax[5,3+married+(hoh*2)]
          
          #Add the limited deductions back to taxable income
          
          if (LimitedIncome > 0){
            
            Limit<-min(LimitedIncome,Limited)
            
          } else {
            
            Limit<-0
            
          }
            
            taxableincome <- taxableincome + Limit
      
      federalincometaxBC<-FedIncomeTax(taxableincome,married,hoh)
      
          #deduct the 28% deductions again
      
      federalincometaxBC<-federalincometaxBC #- (Limit * 0.28)
      
      taxableincome<-taxableincome-Limit
      
      surtax<- ClintonSurtax(income1,income2)
      
      #Step 9: Child Tax Credit
      
      ctc<-FedCTC(income1, income2, children,married)
      
        #new Clinton CTC
      
          ctc<-ctc + ClintonCTC(income1, income2, children5, married)
      
      #Earned Income Tax Credit
      
      eitc<-FedEITC(income1,income2,children+children5,married)
      
      #Step 10: Federal Income Tax after credits
      
      federalincometax<-federalincometaxBC+surtax-ctc-eitc
      
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
      
          if(federalincometax+employeepayrolltax < buffett & income1+income2 > 2000000){
            
            federalincometax<-federalincometax+(buffett-employeepayrolltax)
            
          }
      
      #Tax Bill
      
      taxburden<-federalincometax+employeepayrolltax+medsurtax+surtax
      
      #Step 13: Employer Payroll Taxes
      
      employerpayrolltax1<-FedEmployerPayroll(income1)
      employerpayrolltax2<-FedEmployerPayroll(income2)
      employerpayrolltax<-employerpayrolltax1+employerpayrolltax2
      
      #Step 14: Total Tax Wedge
      
      taxwedge<-federalincometax+employeepayrolltax+employerpayrolltax+medsurtax+surtax
      
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
    
    TrumpNewTaxBurden<-function(income1, income2, children, children5, married, hoh, itemizeddeductions, childcare){
      
      #Cap on itemized deductions
      
      itemizeddeductions<-min(itemizeddeductions,100000*(married+1)) #Set to Zero
      
      #Step 7: Federal Taxable Income
      
      taxableincome<-FedTaxableIncome(income1, income2, children, married, hoh, itemizeddeductions)
      
        #reduce taxable income for childcare
      
        if( (income1+income2) > (250000*(married+1) )) {
          
          childcare <- max(0,( 1-((income1+income2) - (250000 *(married+1))/(50000*(married+1))))*childcare)
          
        } else {
          
          childcare <- childcare
          
        }
      
        #Max childcare deduction at 12k
      
          childcare <- min(18000,childcare)
      
          taxableincome<-max(taxableincome - childcare,0)
      
      #Step 8: Federal Income Tax
      
      federalincometaxBC<-FedIncomeTax(taxableincome,married,hoh)
      
      #Step 9: Child Tax Credit
      
      ctc<-FedCTC(income1, income2, children+children5,married)
      
      #Earned Income Tax Credit
      
      #eitc<-FedEITC(income1,income2,children+children5,married)
      
      eitc<-TrumpEITC(income1, income2, childcare)
      
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

children<-2
children5<-0
married<-1
childcare<-15600
itemizeddeductions<-0

  #Are you head of household? If you are single and have any children, yes, otherwise no:
                         
    if(married == 0 & children+children5 > 0){
      
      hoh <- 1
      
    } else {
      
      hoh <- 0
      
    }

income1<-15600
income2<-15600


####Current Law Calculations

fedtax<-read.csv("fedtax.csv", header = TRUE, fill = TRUE, sep = ",")

currentlaw<-data.frame(CurrentLawTaxBurden(income1, income2, children, children5, married, hoh, itemizeddeductions))
rownames(currentlaw)<-c('Income','Taxable Income', 'Federal Income Tax', 'Child Tax Credit','EITC', 'Federal Income Tax After Credits', 'Employee Payroll Tax', 'Tax Burden','Employer Payroll Tax','Tax Wedge')
colnames(currentlaw)<-c('Amount')

#clinton

fedtax<-read.csv("clinton.csv", header = TRUE, fill = TRUE, sep = ",")

clintonlaw<-data.frame(ClintonTaxBurden(income1, income2, children, children5, married, hoh, itemizeddeductions))

rownames(clintonlaw)<-c('Income','Taxable Income', 'Federal Income Tax', 'Child Tax Credit','EITC', 'Federal Income Tax After Credits', 'Employee Payroll Tax','Tax Burden', 'Employer Payroll Tax', 'Tax Wedge')
rownames(clintonlaw)<-c('Income','Taxable Income', 'Federal Income Tax', 'Child Tax Credit','EITC', 'Federal Income Tax After Credits', 'Employee Payroll Tax','Tax Burden', 'Employer Payroll Tax', 'Tax Wedge')
colnames(clintonlaw)<-c('Amount')

#Trump's New Tax Plan

fedtax<-read.csv("trumpnew.csv", header = TRUE, fill = TRUE, sep = ",")

trumpnewlaw<-data.frame(TrumpNewTaxBurden(income1, income2, children, children5, married, hoh, itemizeddeductions, childcare))

rownames(trumpnewlaw)<-c('Income','Taxable Income', 'Federal Income Tax', 'Child Tax Credit','EITC', 'Federal Income Tax After Credits', 'Employee Payroll Tax','Tax Burden', 'Employer Payroll Tax', 'Tax Wedge')
rownames(trumpnewlaw)<-c('Income','Taxable Income', 'Federal Income Tax', 'Child Tax Credit','EITC', 'Federal Income Tax After Credits', 'Employee Payroll Tax','Tax Burden', 'Employer Payroll Tax', 'Tax Wedge')
colnames(trumpnewlaw)<-c('Amount')


#final output:

final<-cbind(currentlaw,clintonlaw[,1], trumpnewlaw[,1])

colnames(final)<-c('current law','clinton','trumpnew')
