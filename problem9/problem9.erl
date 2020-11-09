% Problem link: https://projecteuler.net/problem=9


% Problem Statement: 

% A Pythagorean triplet is a set of three natural numbers, a < b < c, for 
% which,

% a2 + b2 = c2
% For example, 32 + 42 = 9 + 16 = 25 = 52.

% There exists exactly one Pythagorean triplet for which a + b + c = 1000.
% Find the product abc.

-module(problem9).
-export([main/0]).

main()
->
    % We want M > N > 0
    M = 2,
    N = 1,
    {A, B, C} = pythagorean_triplets(M, N),
    A*B*C
.

% if we go beyond 1000 then something has gone wrong. 
pythagorean_triplets( M, _ ) when M > 1000
->
    false
;
%  Make sure M > N
pythagorean_triplets( M, N ) when M =< N
->
    pythagorean_triplets( M+1, 1 )
;
pythagorean_triplets( M, N )
->
    % Comment at end of file explains these formulae. It's trivial to check 
    % that A^2 + B^2 = C^2 here though. 
    A = M*M - N*N,
    B = 2*M*N,
    C = M*M + N*N,
    % Since pythagorean triplets can be constructed by multiplying other 
    % pythagorean triplets by some integer factor, we check if the sum of our 
    % current pythagorean triplets evenly divides 1000. 
    case 1000 rem (A+B+C) of
        0 -> {1000 div (A+B+C)*A, 1000 div (A+B+C)*B, 1000 div (A+B+C)*C};
        _ -> pythagorean_triplets( M, N+1 )
    end
.


% Consider C^2 = A^2 + B^2 where A,B,C are pairwise coprime integers.
% Since A, B are coprime (aka gcd(A,B) = 1), it must be the case that at least 
% one of A and B is odd. Without loss of generality, assume that A is odd. 


% Then
%     C^2 - A^2    = B^2
%     (C-A)(C+A)   = B^2
%     (C-A)(C+A)/B = B
%     (C+A)/B      = B/(C-A)

% since a,b,c are integers, we know that (C+A)/B is rational. Since it is 
% rational, it can be expressed as the ratio of two integers, which here we 
% call M/N. We assume here that M/N is written in its lowest form. This 
% implies gcd(M,N) = 1 (i.e. they are coprime). 
% Then note that by the last equation, M/N = B/(C-A) so that N/M = (C-A)/B.

% Thus we have two equations:
%     (C+A)/B = M/N
%     (C-A)/B = N/M
% which we can simplify a bit
%     C/B + A/B  = M/N
%     C/B - A/B  = N/M
% And we can solve for C/B and A/B. The second equation shows us:
%     C/B = N/M + A/B
% which subbed into the first equation gives us
%     N/M + 2A/B = M/N
%     2A/B       = M/N - N/M
%     A/B        = 1/2(M/N - N/M)
% and putting this into the second equation yields
%     C/B - 2(M/N - N/M)  = N/M
%     C/B - M/2N + N/2M   = N/M
%     C/B                 = M/2N + N/2M
%     C/B                 = 1/2(M/N + N/M)
% simplifying both gives
%     C/B = (M^2 + N^2)/(2MN)
%     A/B = (M^2 - N^2)/(2MN)
% We show quickly that only 1 of M and N is odd:
% • Since M and N are coprime, they can't both be even. 
% • If M and N were both odd, then M^2  - N^2 mod 4 = 0 which we show below:
%   Suppose a number mod 4 is represented as 
%       4I + J, for integer I and J = [0,1,2,3]. 
% Then clearly J = 1 and J = 3 correspond to odd numbers. 
% Then 
%     (4I + 1)^2 mod 4 = 16I^2 + 8I + 1 mod 4
%                      = (16I^2 + 8I) mod 4 + 1 mod 4
%                      = 4(4I^2 + 2I) mod 4 + 1 mod 4
%                      = 0 mod 4 + 1 mod 4 
%                      = 1 mod 4
% and
%     (4I + 3)^2 mod 4 = 16I^2 + 24I + 9 mod 4
%                      = 4(4I^2 + 6I) mod 4 + 9 mod 4
%                      = 0 mod 4 + 1 mod 4 
%                      = 1 mod 4
% So the square of an odd number is congruent to 1 modulo 4, thus 
%   M^2 - N^2 mod 4 = 1 mod 4 - 1 mod 4 = 0 mod 4. 
% This means that M^2 - N^2 is a multiple of 4. 
% Now we examine A/B = (M^2 - N^2)/(2MN). Note that 2MN cannot be a multiple 
% of 4 since A and B are coprime. Thus since the minimum possible even factor 
% of the numerator M^2 - N^2 is 4 (as we showed above), and the minimum even 
% multple of the denominator 2MN is 2 (as evidenced by the coefficient of 2), 
% this implies that A must be even.
% But we already defined A to be odd, so clearly M and N cannot both be odd. 
% Thus since M, N cannot both be even and cannot both be odd, we've shown that 
% exactly 1 is even and the other is odd. 
% Since one is even and the other is odd, that implies that 
% (M^2 - N^2) and (M^2 + N^2) are both even, and (2MN) is odd. 
% As the numerators of the two equations are even while the denominators are 
% odd, we see in both C/B = (M^2 + N^2)/(2MN) and A/B = (M^2 - N^2)/(2MN), we 
% have written the fractions in reduced form, we can simply equate numerators 
% with numerators and denominators with denominators. Thus
%     A = M^2 - N^2
%     B = 2MN
%     C = M^2 + N^2
% where gcd(M,N) = 1 and only 1 is odd while the other is even.
% This is how we can generate pythagorean triplets. 
% We note that if both are even, we simply generate pythagorean triplets that 
% are some multiple of other pythagorean triplets. We can filter these out for 
% efficiency if necessary. 
