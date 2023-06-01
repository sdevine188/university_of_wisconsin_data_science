#!/usr/bin/python3.6
import sys
line = sys.stdin.readline()
while line:
  print(line.strip())
  line = sys.stdin.readline()


line = "10 1"
line.split()


#//////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////


# activity 1 - first.py

#!/usr/bin/python3.6
def factorial(val):
        factorial = 1
        if(val < 0):
                return(-1)
        else:
                for i in list(range(1, (val + 1))):
#                        print(i)
                        factorial = factorial * i
                        
        return(factorial)
        
factorial(val = 4)


#//////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////


# activity 1 - second.py

#!/usr/bin/python3.6
import sys

line = sys.stdin.readline()
val_list = []
for val in line.split():
        val_int = int(val)
        val_list.append(val_int)
val_list_length = len(val_list)
print(sum(val_list) / val_list_length)


#//////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////


# activity 1- third.py
line = "24 5"


#!/usr/bin/python3.6
import sys

# read input
line = sys.stdin.readline()

# convert input into a list of numeric values
val_list = []
for val in line.split():
        val_int = int(val)
        val_list.append(val_int)
        
# sort val_list
val_list.sort()

# get prime_list
not_prime_list = []

# loop through values between two input values, and find which are primes
for possible_prime in list(range(val_list[0] + 1, (val_list[1]))):
#        print(possible_prime)
        if(possible_prime == 2):
                continue
        
        if(possible_prime > 2):
                for possible_divisor in list(range(2, possible_prime)):
        #                print(possible_divisor)
        #                print(possible_prime % possible_divisor)
        #                
                        if(possible_prime != possible_divisor):
        #                        print(possible_divisor)
                                
                                if((possible_prime % possible_divisor) == 0):
        #                                print(possible_prime)
                                        not_prime_list.append(possible_prime)
                        
# get unique values from not_prime_list
not_prime_list = list(set(not_prime_list))

# get primes by removing values in not_prime_list from val_list
prime_list = []
for possible_prime in list(range(val_list[0] + 1, (val_list[1]))):

        if(possible_prime not in not_prime_list):
                prime_list.append(possible_prime)      
                
# concatenate prime_list using specific character pattern
concatenate_character = ":"
prime_list_concatenated = ""
for prime in prime_list:
#        print(prime)
        if(concatenate_character == ":"):
                prime_list_concatenated = prime_list_concatenated + str(prime) + ":"
                concatenate_character = "!"
                continue
        if(concatenate_character == "!"):
                prime_list_concatenated = prime_list_concatenated + str(prime) + "!"
                concatenate_character = "&"
                continue
        if(concatenate_character == "&"):
                prime_list_concatenated = prime_list_concatenated + str(prime) + "&" 
                concatenate_character = ":"
                continue

# remove final concatenation_character from prime_list_concatenated 
prime_list_concatenated = prime_list_concatenated[:-1]

print(prime_list_concatenated)

                

# print prime_list_concatenated
print(prime_list_concatenated)



