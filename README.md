# Programming Assignment - TDA352

__Cryptlib__: A library that provides a number of mathematical functions commonly used in cryptography,  i.e.  Euler's Phi Function (Totient), the Extended Euclidean Algorithm, modular inverse etc.

__Fiat-Shamir-Attack__:  Eavesdropped on a number of Fiat-Shamir protocol runs. Found that the same nonce was used twice! Due to the special soundness property, the secret key used in the protocol can be retrieved. :)

__CBC-Attack__: Intercepted a message that was encrypted using cipher-block chaining and decrypt it under the premise: knowing the plain-text value of the first block. ^^

__RSA-Attack__: The same message is encrypted using RSA to three different recipients. The enablers of the attack are (1) all recipients have the same public key (e = 3) and (2) the recipients have different modulus (N1, N2, N3) that are coprime.

__ElGamal-Attack__: Two ElGamal encryptions of the same message can look completely different. However, this also makes the strength of the encryption depend on the random number generation, which is weak here.
