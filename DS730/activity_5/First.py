#!/usr/bin/python3.6

# to run in hortonworks:
# chmod +x First.py
# ./First.py 

def isPrime(value):
        
        # get possible_prime
        possible_prime = value
        
        # get prime_check_list
        prime_check_list = []

        # return 0 if possible_prime is <= 2
        if(possible_prime <= 2):
                return False

        # loop through possible_divisors 
        # and add a flag to prime_check_list if possible_prime is found to be not prime
        if(possible_prime > 2):
                for possible_divisor in list(range(2, possible_prime)):
                        if(possible_prime != possible_divisor):
                                if((possible_prime % possible_divisor) == 0):
                                        prime_check_list.append(1)
              
        # check if possible_prime was found to be not prime
        # return True if prime, false if not prime
        if(sum(prime_check_list) == 0):
                return True
        else:
                return False

     
def printPrime(first, second):
        
        # get val_list
        val_list = [first, second]

        # get prime_list        
        prime_list = []

        # loop through possible_primes, adding them to prime_list as appropriate
        for possible_prime in list(range(val_list[0] + 1, (val_list[1]))):
                if(isPrime(possible_prime) == True):
                        prime_list.append(possible_prime)
           
        # if there are primes, get prime_list_concatenated and print it; else return "No primes"           
        if(len(prime_list) > 0):        
                prime_list_concatenated = ""
                for prime in prime_list:
                        prime_list_concatenated = prime_list_concatenated + str(prime) + ":"
                
                # remove final concatenation_character from prime_list_concatenated 
                prime_list_concatenated = prime_list_concatenated[:-1]
        
                # print prime_list_concatenated 
                print(prime_list_concatenated)
        else: 
                print("No primes")


def main():
        
        # get first_input_number, ensuring it is >= 0
        first_input_number = input("Please input the first number: ")
        first_input_number = int(first_input_number)
        while(first_input_number < 0):
                first_input_number = input("Sorry, all input numbers must be greater than or equal to zero. Please input the first number: ")
                first_input_number = int(first_input_number)
                
        # get second_input_number, ensuring it is >= 0
        second_input_number = input("Please input the second number: ")
        second_input_number = int(second_input_number)
        while(second_input_number < 0):
                second_input_number = input("Sorry, all input numbers must be greater than or equal to zero. Please input the second number: ")
                second_input_number = int(second_input_number)

#        print("first_input_number is " + str(first_input_number) + "; second_input_number is " + str(second_input_number))
        
        # get val_list combining first_input_number and second_input_number in ascending order
        val_list = [first_input_number, second_input_number]
        val_list.sort()
        
        # call printPrime using first_input_number and second_input_number
        printPrime(val_list[0], val_list[1])


if __name__ == "__main__":
        main()
