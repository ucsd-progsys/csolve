#include <stdio.h>
int main (int argc, char const *argv[])
{
        // set an integer number here
        int number = 23945;
        // if the number is odd (1 = TRUE)
        if(odd(number)==1)
                printf("%d is odd\n",number);
        else
                printf("%d is even\n",number);
        return 0;
}

// returns 0 if the given number becomes 0, so the given number is odd
// returns even(number - 1) elsewhere
int odd(int number){
        if (number==0)
                return 0;
        else
                return even(number-1);
}

// returns 0 if the given number becomes 0, so the given number is even
// returns odd(number - 1) elsewhere
int even(int number){
        if(number==0)
                return 1;
        else
                return odd(number-1);
