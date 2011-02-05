#include <stdio.h>
int main (int argc, char const *argv[]){
        int number = 23945;
        if(odd(number)==1)
                printf("%d is odd\n",number);
        else
                printf("%d is even\n",number);
        return 0;
}

int odd(int number){
        if (number==0)
                return 0;
        else
                return even(number-1);
}

int even(int number){
        if(number==0)
                return 1;
        else
                return odd(number-1);
}
