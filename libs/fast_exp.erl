-module( fast_exp ).
-export( [ exp/2,
           power/3 ]).

% -------------------------------------------------------------
% Square-Multiply Algorithm for fast exponentiation of integers
% -------------------------------------------------------------
exp( Base,  Exp ) when Exp > 3 -> lexp( Base, split_exp(Exp, []), Base );
exp( Base,  3 )                -> Base*Base*Base;
exp( Base,  2 )                -> Base*Base;
exp( Base,  1 )                -> Base;
exp( _Base, 0 )                -> 1.

 

lexp( Base, [sqr|L], Acc ) -> lexp( Base, L, Acc*Acc );
lexp( Base, [_|L], Acc )   -> lexp( Base, L, Acc*Base );
lexp( _, _, Acc )          -> Acc.

 

                        % -----------------
power(Base, Exp, Mod)   %  B^E = C (mod M)
->                      % -----------------
    case Exp of
        0   -> 1;
        _
        ->
            case ( Exp rem 2 == 0 ) of
                 true   -> exp( power(Base, Exp div 2, Mod), 2 ) rem Mod;
                 false  -> Base*power( Base, Exp-1, Mod ) rem Mod
            end
    end
.

% --------------------------------------------------
% Square when Exp is even, Multiply when Exp is odd.
% --------------------------------------------------
split_exp( Exp, L ) when Exp < 2    -> L;
split_exp( Exp, L )
->
    case ( Exp rem 2 == 0 ) of
         true   -> split_exp( Exp div 2, [sqr|L] );
         false  -> split_exp( Exp -   1, [mul|L] )
    end
.
% -----------------------------------------------------------------------------
% NOTE: Laws of exponents tell us that 
%           A^X • A^Y = A^(X+Y)
%           (A^X)^Y   = A^(XY)
%       which tells us 
%           - when we multiply, we remove one from Exp
%           - when we square, we divide exp by 2.
%       Also since we square when Exp is even, it's guaranteed to be evenly 
%       divisible
% -----------------------------------------------------------------------------
