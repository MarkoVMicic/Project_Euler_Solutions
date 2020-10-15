// Problem link: https://projecteuler.net/problem=2

/*
Problem statement:
    
    By considering the terms in the Fibonacci sequence whose values do not 
    exceed four million, find the sum of the even-valued terms.

*/

#include <stdio.h>
#define MAX_VALUE 4000000

long long SumEvenFibonacci(long);

int main(int argc, char **argv)
{
    long long answer = SumEvenFibonacci(MAX_VALUE);

    printf("Answer: %lli\n", answer);
    return(0);
}

long long SumEvenFibonacci(long max_value)
{
    long long answer = 0;
    long long f_t = 1;
    long long f_t_minus_1 = 1;
    long long temp;
    for(int i = 0; f_t < max_value; i++)
    {
        temp = f_t + f_t_minus_1;
        f_t_minus_1 = f_t;
        f_t = temp;
        if(i % 3 == 0)
        {
            answer += temp;
        }
    }
    return answer;
}