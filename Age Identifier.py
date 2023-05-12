from datetime import date
import datetime


# Takes input from user
date_input = input("Enter your birth date: ")

# Format that we will use to take input 
format = '%d/%m/%Y'

# Get today's date from datetime function
today = date.today()

## Try except block to handle incorrect date
try:
    # setting the input's format as required
    datetime = datetime.datetime.strptime(date_input, format)
    # now the birthdate and today's date is in same format(i.e.25-02-2022)
    birth_date = datetime.date()
    # comparing today and birthday because they are in same format now 
    if(birth_date <= today): 

        # subtract today's date from birth date to get number of days
        year = today - birth_date
        # divide the number of days with 365 to get Year
        print('Your Age is: ', year / 365)
        # Check if birth day and month is equal to today's day and month to check if it is birthday
        if((birth_date.day, birth_date.month) == (today.day, today.month)):
            print("Happy Birthday")
        elif((birth_date.day,birth_date.month,birth_date.year) == (today.day, today.month,today.year)):
            month=today-birth_date
            print("Age is:", month / 12)
    else:
        print("Please enter the correct year ")
except:
    print("Incorrect date format")
