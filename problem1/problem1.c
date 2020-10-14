// problem link: https://projecteuler.net/problem=1

/*
Problem statement: 

    If we list all the natural numbers below 10 that are multiples of 3 or 5, 
    we get 3, 5, 6 and 9. The sum of these multiples is 23.

    Find the sum of all the multiples of 3 or 5 below 1000.
*/

#include <stdio.h>
#define MAX_VALUE 10000000

long long NaiveSum(void);
long long FastSum(void);
long long SumDivisibleBy(long long n);

int main(int argv, char **argc)
{
    long long naive_answer = NaiveSum();
    long long fast_answer = FastSum();
    
    printf("Naive Answer: %lli\n", naive_answer);
    printf("Fast Answer: %lli\n", fast_answer);
    return 0;
}

long long NaiveSum(void)
{
    long long answer = 0;
    for(int i = 1; i < MAX_VALUE; i++)
    {
        if((i % 3 == 0) || (i % 5 == 0))
        {
            answer += i;
        }
    }
    return answer;
}

long long FastSum(void)
{
    return SumDivisibleBy(3) + SumDivisibleBy(5) - SumDivisibleBy(15);
}

long long SumDivisibleBy(long long n)
{
    /*
        Consider some integer m such that mn <= MAX_VALUE, but (m+1)n > 
        MAX_VALUE. 

        Then we wish to compute the following: 
        n + 2n + 3n + ... + mn = n(1 + 2 + ... + m)
                               = ∑_1^m i
                               = m(m+1)/2
        We can compute m as simply the integer division MAX_VALUE/n. 
    */
    // If we wish to include MAX_VALUE, then we remove the -1 
    long long m = (MAX_VALUE - 1) / n; 
    return (n * ((m*(m+1))/2));
}