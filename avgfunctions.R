#This file stores all the functions necessary to calculate Marriage Penalty

#NOTE: These are slightly different than the tax wedge calculations. (This incorporates IRS rounding)

#############Federal Tax Functions###############################

  #federal taxable income
  
    FedTaxableIncome<-function(income1, income2, children, married, hoh, itemizeddeductions){
    
      income<-income1+income2
      
      #Personal Exemptions
      
        if(income>fedtax$pep_pease_threshold[1+married+(hoh*2)]){
          
            personalexemption<-max(0,(1-( ceiling(((income-fedtax$pep_pease_threshold[1+married+(hoh*2)])/2500))*(fedtax$pep_phaseout[1])))*(fedtax$personal_exemption[1]*(1+children+married))) #PeP Turned off here
          
          } else {
          
            personalexemption<-fedtax$personal_exemption[1]*(1+children+married)
          
          }
        
        #Calculating STANDARD/ITEMIZED DEDUCTION
        
          if(itemizeddeductions>fedtax$standard_deduction[1+married+(hoh*2)]){
          
            if(income>fedtax$pep_pease_threshold[1+married+(hoh*2)]){
            
              deduction<-max(itemizeddeductions-((income-fedtax$pep_pease_threshold[1+married+(hoh*2)])*.03),itemizeddeductions*0.2)
            
            } else {
            
              deduction<-itemizeddeductions
            
            }
          
          } else {
          
          deduction<-fedtax$standard_deduction[1+married+(hoh*2)]
          
          }
        
          taxableincome<-max(0,income-deduction-personalexemption)
        
          return(taxableincome)
        
      }

  #federal income tax
  
    FedIncomeTax<-function(taxableincome,married,hoh){
    
    #Pre-Credit Federal Income Tax Bill
    
    x<-1 #An index that counts through the tax brackets
    
    federalincometax<-0
    
    #To make this a little easier, I don't call on the variable names in the dataset. single:3,married:4,hoh:5
    
    while(TRUE){  
      
      if( taxableincome < fedtax[x+1,3+married+(hoh*2)] & x < sum(!is.na(fedtax$incometaxrate))){
        
        federalincometax <- federalincometax + ( ( taxableincome - fedtax[x,3+married+(hoh*2)] ) * fedtax$incometaxrate[x] )
        
        break
        
      } else {
        
        federalincometax <- federalincometax + fedtax$incometaxrate[x] * ( fedtax[x+1,3+married+(hoh*2)] - fedtax[x,3+married+(hoh*2)] )
        
        x<-x+1
        
      } 
      
      if( x == sum(!is.na(fedtax$incometaxrate)) ) {
        
        federalincometax <- federalincometax + fedtax$incometaxrate[x] * ( taxableincome - fedtax[x,3+married+(hoh*2)] )
        
        break
        
      }
      
    }
    
    return(federalincometax)
    
  }
  
  #Clinton income tax, with 28% cap on deductions
    
    ClintonFedIncomeTax<-function(taxableincome,married,hoh){
      
      #Pre-Credit Federal Income Tax Bill
      
      x<-1 #An index that counts through the tax brackets
      
      federalincometax<-0
      
      #To make this a little easier, I don't call on the variable names in the dataset. single:3,married:4,hoh:5
      
      while(TRUE){  
        
        if( taxableincome < fedtax[x+1,3+married+(hoh*2)] & x < sum(!is.na(fedtax$incometaxrate))){
          
          federalincometax <- federalincometax + ( ( taxableincome - fedtax[x,3+married+(hoh*2)] ) * fedtax$incometaxrate[x] )
          
          break
          
        } else {
          
          federalincometax <- federalincometax + fedtax$incometaxrate[x] * ( fedtax[x+1,3+married+(hoh*2)] - fedtax[x,3+married+(hoh*2)] )
          
          x<-x+1
          
        } 
        
        if( x == sum(!is.na(fedtax$incometaxrate)) ) {
          
          federalincometax <- federalincometax + fedtax$incometaxrate[x] * ( taxableincome - fedtax[x,3+married+(hoh*2)] )
          
          break
          
        }
        
      }
      
      return(federalincometax)
      
    }
    
  #federal EITC
  
    FedEITC<-function(income1,income2,children,married){
    
      income<-income1+income2
      
    c<-min(children,3)
    
    if(married == 0){
      
      if(income < fedtax$eitc_threshold[1+c]) {
        
        eitc<-income*(fedtax$eitc_max[1+c]/fedtax$eitc_threshold[1+c])
        
      } else if (income >= fedtax$eitc_threshold[1+c] & income <= fedtax$eitc_phaseout_single[1+c]) {
        
        eitc<-fedtax$eitc_max[1+c]
        
      } else if (income > fedtax$eitc_phaseout_single[1+c]) {
        
        eitc<-max(0,fedtax$eitc_max[1+c]+((fedtax$eitc_phaseout_single[1+c]-income)*(fedtax$eitc_max[1+c]/(fedtax$eitc_maxincome_single[1+c]-fedtax$eitc_phaseout_single[1+c]))))
        
      }
      
    } else if (married == 1) {
      
      if(income < fedtax$eitc_threshold[1+c]) {
        
        eitc<-income*(fedtax$eitc_max[1+c]/fedtax$eitc_threshold[1+c])
        
      } else if (income >= fedtax$eitc_threshold[1+c] & income <= fedtax$eitc_phaseout_married[1+c]) {
        
        eitc<-fedtax$eitc_max[1+c]
        
      } else if (income > fedtax$eitc_phaseout_married[1+c]) {
        
        eitc<-max(0,fedtax$eitc_max[1+c]+((fedtax$eitc_phaseout_married[1+c]-income)*(fedtax$eitc_max[1+c]/(fedtax$eitc_maxincome_married[1+c]-fedtax$eitc_phaseout_married[1+c]))))
        
      }
      
    }
    
    return(eitc)
    
  }
  
  #federal Child Tax Credit
  
    FedCTC<-function(income1, income2, children,married){
      
      income<-income1+income2
    
    #Child Tax Credit
    
    if(children>0) {
      
      c<-children
      
      if (married == 0) {
        
        if(income <= fedtax$ctcphasein[1]){
          
          ctc<-0
          
        } else if(income <= fedtax$ctcphaseout_single[1]){
          
          ctc<-min(fedtax$ctccredit[1]*c,((income-fedtax$ctcphasein[1])*fedtax$ctcphaseinrate[1]))
          
        } else if(income > fedtax$ctcphaseout_single[1]){
          
          ctc<-max(0,(fedtax$ctccredit[1]*c)-( (ceiling((income-fedtax$ctcphaseout_single[1])*(1/1000))*1000) * fedtax$ctcphaseoutrate[1]) )
          
        }
        
      } else if (married == 1) {
        
        if(income <= fedtax$ctcphasein[1]) {
          
          ctc<-0
        
        } else if(income <= fedtax$ctcphaseout_married[1]) {
          
          ctc<-min(fedtax$ctccredit[1]*c,((income-fedtax$ctcphasein[1])*fedtax$ctcphaseinrate[1]))
                                                       
        } else if(income > fedtax$ctcphaseout_married[1]) {
          
          ctc<-max(0,(fedtax$ctccredit[1]*c)- (ceiling((income-fedtax$ctcphaseout_married[1])*(1/1000))*1000) *fedtax$ctcphaseoutrate[1])
        
        }
        
      }
      
    } else {
      
      ctc<-0
    
    }
    
    return(ctc)
    
  }

    RubioCTC<-function(income1,income2,children,married){
  
      income <- income1+income2
      
      #Child Tax Credit
      
      if(children>0) {
        
        rubioctc<-min((income * .153), 2500*children)
        
      } else {
        
        rubioctc<-0
        
      }
      
      #Phaseout of RubioCTC over $150,000 ($300,000 Married filing jointly)
      
      if(income > 150000*(2+married)){
        
        rubioctc<-max((rubioctc*(1-(income-150000*(1+married))/(50000*(1+married)))),0)
          
          
          
        
      }
      
      
      return(rubioctc)
      
    }
    
    ClintonCTC<-function(income1, income2, children5, married){
      
      income<-income1+income2
      
      if(children5>0) {
        
        c<-children5
        
        if (married == 0) {
          
          if(income <= 0){ 'No phase-in for new CTC'
            
            ctc<-0
            
          } else if(income <= fedtax$ctcphaseout_single[1]){
            
            ctc<-min(fedtax$ctccredit[1]*c*2,((income)*0.45))
            
          } else if(income > fedtax$ctcphaseout_single[1]){
            
            ctc<-max(0,(fedtax$ctccredit[1]*c*2)-( (ceiling((income-fedtax$ctcphaseout_single[1])*(1/1000))*1000) * fedtax$ctcphaseoutrate[1]) )
            
          }
          
        } else if (married == 1) {
          
          if(income <= 0) { 'No phase in threshold for new credit'
            
            ctc<-0
            
          } else if(income <= fedtax$ctcphaseout_married[1]) {
            
            ctc<-min(fedtax$ctccredit[1]*c*2,((income)*.45))
            
          } else if(income > fedtax$ctcphaseout_married[1]) {
            
            ctc<-max(0,(fedtax$ctccredit[1]*c*2)- (ceiling((income-fedtax$ctcphaseout_married[1])*(1/1000))*1000) *fedtax$ctcphaseoutrate[1])
            
          }
          
        }
        
      } else {
        
        ctc<-0
        
      }
      
      return(ctc)
      
    }
  
  #Federal Employee Payroll Taxes
  
    FedEmployeePayroll<-function(income){
    
    #Employee payroll taxes
    
    x<-1 #An index that counts through the payroll tax brackets
    
    employeepayrolltax<-0
    
    while(TRUE){  
      
      if( income < fedtax$emppayrollbracket[x+1] & x < sum(!is.na( fedtax$emppayrollbracket ) ) ){
        
        employeepayrolltax <- employeepayrolltax + ( ( income - fedtax$emppayrollbracket[x] ) * fedtax$emppayrollrate[x] )
        
        break
        
      } else {
        
        employeepayrolltax <- employeepayrolltax + fedtax$emppayrollrate[x] * ( fedtax$emppayrollbracket[x+1] - fedtax$emppayrollbracket[x])
        
        x<-x+1
        
      } 
      
      if( x == sum(!is.na(fedtax$emppayrollbracket))){
        
        employeepayrolltax <- employeepayrolltax + fedtax$emppayrollrate[x] * ( income - fedtax$emppayrollbracket[x] )
        
        break
        
      }
      
    }
    
    return(employeepayrolltax)
    
  }
  
  #Federal Employer Payroll Taxes
  
    FedEmployerPayroll<-function(income){
    
    x<-1 #An index that counts through the payroll tax brackets
    
    employerpayrolltax<-0
    
    while(TRUE){  
      
      if( income < fedtax$emplrayrollbracket[x+1] & x < sum(!is.na( fedtax$emplrayrollbracket ) ) ){
        
        employerpayrolltax <- employerpayrolltax + ( ( income - fedtax$emplrayrollbracket[x] ) * fedtax$emplrpayrollrate[x] )
        
        break
        
      } else {
        
        employerpayrolltax <- employerpayrolltax + fedtax$emplrpayrollrate[x] * ( fedtax$emplrayrollbracket[x+1] - fedtax$emplrayrollbracket[x] )
        
        x<-x+1
        
      } 
      
      if( x == sum(!is.na(fedtax$emplrayrollbracket))){
        
        employerpayrolltax <- employerpayrolltax + fedtax$emplrpayrollrate[x] * ( income - fedtax$emplrayrollbracket[x] )
        
        break
        
      }
      
    }
    
    return(employerpayrolltax)
    
  }

  #Medicare Surtax

    MedSurtax<-function(income1,income2,married){
      
      income<-income1+income2
        
      x<-1 #An index that counts through the tax brackets
      
      medsurtax<-0
      
      #To make this a little easier, I don't call on the variable names in the dataset. single:3,married:4,hoh:5
      
      while(TRUE){  
     
        if(married == 0){
          
          if( income < fedtax$medsurtaxsinglebracket[x+1] & x < sum(!is.na(fedtax$medsurtaxsinglebracket))){
            
            medsurtax <- medsurtax + ( ( income - fedtax$medsurtaxsinglebracket[x] ) * fedtax$medsurtaxrate[x] )
            
            break
            
          } else {
            
            medsurtax <- medsurtax + fedtax$medsurtaxrate[x] * ( fedtax$medsurtaxsinglebracket[x+1] - fedtax$medsurtaxsinglebracket[x] )
            
            x<-x+1
            
          } 
          
          if( x == sum(!is.na(fedtax$medsurtaxsinglebracket)) ) {
            
            medsurtax <- medsurtax + fedtax$medsurtaxrate[x] * ( income - fedtax$medsurtaxsinglebracket[x] )
            
            break
            
          }
          
        } else 
          
        if(married == 1){
          
          if( income < fedtax$medsurtaxmarriedbracket[x+1] & x < sum(!is.na(fedtax$medsurtaxmarriedbracket))){
            
            medsurtax <- medsurtax + ( ( income - fedtax$medsurtaxmarriedbracket[x] ) * fedtax$medsurtaxrate[x] )
            
            break
            
          } else {
            
            medsurtax <- medsurtax + fedtax$medsurtaxrate[x] * ( fedtax$medsurtaxmarriedbracket[x+1] - fedtax$medsurtaxmarriedbracket[x] )
            
            x<-x+1
            
          } 
          
          if( x == sum(!is.na(fedtax$medsurtaxmarriedbracket)) ) {
            
            medsurtax <- medsurtax + fedtax$medsurtaxrate[x] * ( income - fedtax$medsurtaxmarriedbracket[x] )
            
            break
            
          }
          
        }      
      
      }
      
      return(medsurtax)   
      
    }
  
  #Alternative Minimum Tax

    AMT<-function(income1, income2, married){
      
      income<-income1+income2
      
      if(married == 1){
        
        amti <- income - max(0,(fedtax$amtexemptionmarried[1] - max(0,(income - fedtax$amtphaseoutmarried[1])*.25)))
        
      } else {
        
        amti<- income - max(0,(fedtax$amtexemptionsingle[1] - max (0,(income - fedtax$amtphaseoutsingle[1])*.25)))
        
      }
      
      x<-1
      
      amt<-0
        
      while(TRUE){
        
        if( amti < fedtax$amtbracket[x+1] & x < sum(!is.na(fedtax$amtbracket))){
          
          amt <- amt + ( ( amti - fedtax$amtbracket[x] ) * fedtax$amtrate[x] )
          
          break
          
        } else {
          
          amt <- amt + fedtax$amtrate[x] * ( fedtax$amtbracket[x+1] - fedtax$amtbracket[x] )
          
          x<-x+1
          
        } 
        
        if( x == sum(!is.na(fedtax$amtbracket)) ) {
          
          amt <- amt + fedtax$amtrate[x] * ( amti - fedtax$amtbracket[x] )
          
          break
          
        }         
        
      }  
      
      return(amt)
      
    }

  #Sanders Surtax

    SandersSurtax<-function(income1, income2, married, hoh){
      
      income<-income1+income2
      
      x<-1 #An index that counts through the tax brackets
      
      surtax<-0
      
      #To make this a little easier, I don't call on the variable names in the dataset. single:3,married:4,hoh:5
      
      while(TRUE){  
        
        if( income < fedtax[x+1,36+married+(hoh*2)] & x < sum(!is.na(fedtax$surtaxrate))){
          
          surtax <- surtax + ( ( income - fedtax[x,36+married+(hoh*2)] ) * fedtax$surtaxrate[x] )
          
          break
          
        } else {
          
          surtax <- surtax + fedtax$surtaxrate[x] * ( fedtax[x+1,36+married+(hoh*2)] - fedtax[x,36+married+(hoh*2)] )
          
          x<-x+1
          
        } 
        
        if( x == sum(!is.na(fedtax$surtaxrate)) ) {
          
          surtax <- surtax + fedtax$surtaxrate[x] * ( income - fedtax[x,36+married+(hoh*2)] )
          
          break
          
        }
        
      }
      
      return(surtax)
      
    }
    
  #Clinton Surtax
    
    ClintonSurtax<-function(income1, income2){
      
      totalincome <- income1 + income2
      
      if(totalincome < 5000000){
        
        surtax <- 0
        
      } else {
        
        surtax <- (totalincome - 5000000) * 0.04
        
      }
      
      return(surtax)
      
    }
      
  #Sanders Health Insurance Employer-side Payroll Tax

    SandersSStax<-function(income){
      
      x<-1 #An index that counts through the payroll tax brackets
      
      SStax<-0
      
      while(TRUE){  
        
        if( income < fedtax$sspayrollbracket[x+1] & x < sum(!is.na( fedtax$sspayrollbracket ) ) ){
          
          SStax <- SStax + ( ( income - fedtax$sspayrollbracket[x] ) * fedtax$sspayrollrate[x] )
          
          break
          
        } else {
          
          SStax <- SStax + fedtax$sspayrollrate[x] * ( fedtax$sspayrollbracket[x+1] - fedtax$sspayrollbracket[x] )
          
          x<-x+1
          
        } 
        
        if( x == sum(!is.na(fedtax$sspayrollbracket))){
          
          SStax <- SStax + fedtax$sspayrollrate[x] * ( income - fedtax$sspayrollbracket[x] )
          
          break
          
        }
        
      }
      
      return(SStax)
      
    }


