viveEnLaMansion(tiaAgatha).
viveEnLaMansion(mayordomo).
viveEnLaMansion(charles).

odia(tiaAgatha, Persona):-
    viveEnLaMansion(Persona),
    Persona \= mayordomo.

odia(charles, Persona):-
    viveEnLaMansion(Persona),
    not(odia(tiaAgatha, Persona)).

odia(mayordomo, Persona):-
    odia(tiaAgatha, Persona).

masRicoQue(Persona, tiaAgatha):-
    viveEnLaMansion(Persona),
    not(odia(mayordomo, Persona)).

mata(Asesino, Victima):-
    odia(Asesino, Victima),
    not(masRicoQue(Asesino, Victima)),
    viveEnLaMansion(Asesino).

/* 1)
mata(Asesino,tiaAgatha).
Asesino = tiaAgatha.

2)
odia(_,milhouse).
false.

odia(charles, Persona).
Persona = mayordomo ;
false.

odia(Persona, tiaAgatha).
Persona = tiaAgatha ;
Persona = mayordomo.

odia(Quien,OdiaA).
Quien = OdiaA, OdiaA = tiaAgatha ;
Quien = tiaAgatha,
OdiaA = charles ;
Quien = charles,
OdiaA = mayordomo ;
Quien = mayordomo,
OdiaA = tiaAgatha ;
Quien = mayordomo,
OdiaA = charles.

odia(mayordomo,_).
true ;
true.
 */
