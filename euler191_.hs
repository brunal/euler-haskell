-- late twice+ -> no prize
-- absent 3j de suite -> no prize
--periode : 30j (=N)
--combien de chaines avec un prize ?

--total : 3^N chaines
--late twice+ : on ne garde que les chaines sans L (=2^N chaines) ou avec un L (=2^(N-1)*1*N chaines)    conduisent a un prix *si pas trop d'absence*
--  total 2^(N-1)*(2+N)
--absent 3j de suite : celle ou absent 3 premiers jours + pas absent + osef du reste : 1^3*2*3^(N-4)   = 2*3^(N-4)
--  second jour: pas absent le 1er ou le 5eme ->      2*1^3*2*3^(N-5)      = 4*3^(N-5)
--  etc.
--  comme celui-ci pour N-2 jours :                  (N-2)*4*3*(N-5)
--  dernier jour : 3^(N-4)*2*1^3     = 2*3^(N-4)
--  total 4*3^(N-5)*(3+N-2) = 3^(N-5)*(1+N)  chaines conduisent a la perte du prix pour absence

RESTREINDRE CES DERNIERS CALCULS AUX CHAINES AVEC 0/1 retards seulement
ou bien ne pas partir de celles-ci ?

seconde version :

late twice+ : on ne garde que les chaines sans L (=2^N chaines) ou avec un L (=2^(N-1)*1*N chaines)    conduisent a un prix *si pas trop d'absence*
-- total 2^(N-1)*(2+N)

parmi celles-ci, retrancher celles avec 3+ jours consecutifs de retard
commence le premier jour : 1^3, puis ce qu'on veut (avec 0 ou 1 retard) : 2^(N-3) + 2^(N-4)*(N-3)
commence le k-ieme jour (k de 0 a 29) :
    - si pas de retard : 1^k*1^3*2^(N-k-3)
    - si retard apres : 1^k*1^3*2^(N-k-4)*(N-k-3)
    - si retard avant : 1^k                             ARF mais il peut y avoir du retard (pas trop long) avant le retard "final"...



--total : 2^(N-1)*(2+N) - 3^(N-5)*(1+N) si N>=5
--        2^(N-1)*(2+N) - 4*3^(N-4)     sinon