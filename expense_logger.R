
#Expense Logger

expense_log <- function(){
library(dplyr)
library(mailR)
library(xtable)
library(stringr)
setwd('/Users/mac_warrior/Documents/R_scripts')
year <- readline('Enter year: ')
month <- readline('Enter month: ') 

tenants <- c('Apoorv','Sanatan','Sharad','Vinayak')

#Calculations based on number of users
total_tenants <- length(tenants)

#Enter Expenses Module
expenses <- c()
for (i in 1:total_tenants){  
  readname <- paste0('Enter expense for ',tenants[i],': ')
  expense <- as.numeric(readline(readname))
  expenses <- rbind(expenses,expense)
}
expenses <- as.vector(expenses)
expense_df <- data.frame(tenants,expenses)

total_expenses <- sum(expense_df[,2])
expense_per_head <- total_expenses/length(tenants)

bank <- c()
for (i in 1:total_tenants){  
  my_expense <- expense_df[i,2]
  type <- ifelse(my_expense < expense_per_head,'debit',
              ifelse(my_expense > expense_per_head,'credit','No pain, No Gain'))
  amount <- ifelse(my_expense < expense_per_head,expense_per_head-my_expense,
              ifelse(my_expense > expense_per_head,my_expense - expense_per_head,0))

banking <- data.frame(type,amount)
bank <- rbind(bank,banking)
}

expense_df <- cbind(expense_df,bank)

num_credit <- nrow(expense_df[expense_df$type == 'credit',])
num_debit <- total_tenants - num_credit - nrow(expense_df[expense_df$type == 'No pain, No Gain',])
total_credit_amount <- sum(expense_df$amount[expense_df$type == 'credit'])

expense_df$trans <- ifelse(expense_df$type == 'credit',expense_df$amount/total_credit_amount,0)

tenants <- sort(rep(expense_df[expense_df$type=='debit',1],num_credit))
payment_to <- rep(expense_df[expense_df$type=='credit',1],num_credit)

payment <- data.frame(tenants,payment_to)
payment <-  left_join(payment,expense_df[,c(1,4)],by=NULL)
payment <-  left_join(payment,expense_df[,c(1,5)],by=c("payment_to"='tenants'))

payment$final_payment <- payment$amount * payment$trans

#Output Generation and Sending Mail
string_credit = ''
for (i in 1:length(unique(payment_to))){
  string_credit <- paste0(string_credit,' & ',unique(payment_to)[i])
}
string_credit <- str_sub(string_credit,4,str_length(string_credit))

cat("\014")

msg1.1 <- data.frame('Type' = 'TotalExpenses_for_the_month' ,'Amount' = total_expenses)
msg1.2 <- data.frame('Type' = 'Per_head_expenditure_is','Amount' = expense_per_head)
msg <- rbind(msg1.1,msg1.2)

msg0 <- (paste('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0
Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
               <html xmlns="http://www.w3.org/1999/xhtml">
               <head>
               <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
               <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
               </head>
               <body>', print(xtable(msg), type = 'html'), ',</body>
               </html>'))


msg_t1 <- expense_df[,c(1,2)]

msg1 <- (paste('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0
Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
               <html xmlns="http://www.w3.org/1999/xhtml">
               <head>
               <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
               <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
               </head>
               <body>', print(xtable(msg_t1), type = 'html'), ',</body>
               </html>'))

msg1.3 <- data.frame('Note' = paste0(string_credit,' paid more than the individual expense.'))

msg2 <- (paste('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0
Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
               <html xmlns="http://www.w3.org/1999/xhtml">
               <head>
               <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
               <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
               </head>
               <body>', print(xtable(msg1.3), type = 'html'), ',</body>
               </html>'))

msg4 <- data.frame("Transfers table" = 'Please find the detailed transfer amount below:')

msg3 <- (paste('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0
Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
               <html xmlns="http://www.w3.org/1999/xhtml">
               <head>
               <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
               <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
               </head>
               <body>', print(xtable(msg4), type = 'html'), ',</body>
               </html>'))

msg_t2 <- (unique(payment[,c(1,2,5)]))

msgfinal <- (paste('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0
Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
               <html xmlns="http://www.w3.org/1999/xhtml">
               <head>
               <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
               <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
               </head>
               <body>', print(xtable(msg_t2), type = 'html'), ',</body>
               </html>'))


send.mail(from = "apoorv7491@gmail.com",
          to = c("apoorv7491@gmail.com",'vinayak010694@gmail.com','sanatansingh1@gmail.com','sharad647@gmail.com'),
          #           replyTo = c("Reply to someone else <someone.else@gmail.com>")
          subject = paste0("Expense Summary for the month of ",month),
          body = paste0(msg0,msg1,msg2,msg3,msgfinal),
          html = T,
          smtp = list(host.name = "smtp.gmail.com", port = 465, user.name = "apoorv7491", passwd = "aa_apoorv7491", ssl = TRUE),
          authenticate = TRUE,
          send = TRUE)
}