# Baillie-PSW Primality Test
A basic Haskell implementation of the Baillie-PSW test for probable primes.

The test combines a strong base-2 Fermat probable prime test with a strong Lucas probable prime test, which, interestingly,
is a deterministic primality test for numbers up to 2^64. This means that for numbers less than this upper bound, there is no
known composite number that will fool the test! The test exploits the absence of overlap between the base-2 strong Fermat pseudoprimes
and the Lucas pseudoprimes; a composite that fools one of these tests almost surely will not fool the other. Additionally, it's fast!
Big props to Robert Baillie, Carl Pomerance, John Selfridge, and Samuel Wagstaff for all the impressive number theoretic work.

Additional background for those interested:
* http://mpqs.free.fr/LucasPseudoprimes.pdf
* https://math.dartmouth.edu/~carlp/PDF/paper25.pdf
* https://math.dartmouth.edu/~carlp/dopo.pdf
* https://en.wikipedia.org/wiki/Baillie%E2%80%93PSW_primality_test
