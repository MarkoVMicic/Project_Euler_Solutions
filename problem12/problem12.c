#include <stdio.h>
#include <stdlib.h>
#include <math.h>

long int find_num_factors(long int triangle_number)
{
    long int num_factors = 0;
    long int max_factor = sqrt(triangle_number);
    for(long int i = 1; i < max_factor; i++)
    {


        if(triangle_number % i == 0)
        {
            num_factors += 2;
        }
    }
    return num_factors;
}

int main(int argc, char **argv)
{
    char* end;
    long int min_num_factors;
    long int i = 1;
    long int triangle_number;
    long int num_factors;

    min_num_factors = strtol(argv[1], &end, 10);

    printf("Finding triangle number with factors > %ld\n", min_num_factors);   
    while(i)
    {
        triangle_number = (i*(i+1))/2;
        num_factors = find_num_factors(triangle_number);
        if(num_factors > min_num_factors)
        {
            break;
        }
        if(i % 50 == 0)
        {
            printf("Processed %ld numbers; current triangle number is %ld\n", i, triangle_number);
        }
        i++;
    }
    printf("Number is: %ld\n", triangle_number);
    printf("It has %ld factors. \n", num_factors);
}