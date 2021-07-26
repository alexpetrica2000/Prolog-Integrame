:- ensure_loaded('checker.pl').

test_mode(detailed).

% Considerăm următoarele reprezentări:
%
% O integramă este reprezentată prin structura (compusul)
% integ(H, W, Lista, Vocab), unde:
% H este înălțimea integramei
% W este lățimea integramei
% Lista este o listă de tupluri (Poz, Valoare), unde
%   Poz este un tuplu (R, C) conținând rândul și coloana (0-based)
%   Valoare este una dintre:
%     x - dacă celula este neagră (nu poate fi completată cu litere)
%     o literă, dacă celula este completată cu o literă
%     o listă de întrebări, reprezentate ca tupluri (Text, Dir, ID), cu
%       Text - un srting, textul întrebării
%       Dir - una dintre valorile j sau d, indicând direcția întrebării
%       ID - un identificator numeric al întrebării
% Vocab este o listă de stringuri reprezentând cuvinte disponibile
% pentru a rezolva întrebarea.
%
% În ieșirea predicatului intrebări, o întrebare este reprezentată ca
% ((R, C), Text, Dir, ID), unde
% R este rândul căsuței cu întrebarea (0-based)
% C este coloana căsuței cu întrebarea (0-based)
% Text este textul întrebării (un string)
% Dir este j sau d, reprezentând direcția în care trebuie să fie plasat
% răspunsul (jos sau dreapta)
% ID este un identificator numeric al întrebării.

% Puteți vizualiza integramele cu:
% integrama(0, W), print_integrama(W).
% integrama(1, W), print_integrama(W).
% integrama(2, W), print_integrama(W).
% integrama(3, W), print_integrama(W).
%
% Testați cu
% vmtest.
% Testați teste individuale (vedeți predicatul tt din checker.pl) cu
% vmtest(Test).
% de exemplu cu vmtest(intrebari).


% intrebari/2
% intrebari(integ(+H, +W, +Lista, +Vocab), -Lista_intrebari)
% Este adevărat atunci când Lista_intrebari este o lista de tupluri
% ((R, C), Text, Dir, ID), fiecare tuplu corespunzând unei întrebări din
% integramă (rândul, coloana, textul întrebării, direcția (j/d),
% identificatorul).
% BONUS: intrebari are o singură soluție (o singură listă) pentru o
% anumită integramă.
intrebari_helper(_,[],[]).
intrebari_helper(Val, [H|T], [(Val,H)|Rest]) :- intrebari_helper(Val,T,Rest).
intrebari(integ(_,_,[],_),[]).
intrebari(integ(_,_,[(H,Valoare) | T],_), QList) :- is_list(Valoare),!,intrebari_helper(H,Valoare,Rezultat),intrebari(integ(_,_,T,_), QList2), append(Rezultat, QList2, QList).
intrebari(integ(_,_,[_ | T],_),QList) :- intrebari(integ(_,_,T,_), QList).
% id_intrebare/2
% id_intrebare(+Integ, ?Intrebare, ?Q_ID)
% Este adevărat dacă în integrama reprezentată ca integ(...), Intrebare
% este un text iar Q_ID este un identificator care corespund aceleași
% întrebări.
id_intrebare_helper([((_,_),Intrebare,_,ID)| _], Intrebare, ID).
id_intrebare_helper([_|T], Intrebare, ID) :- id_intrebare_helper(T,Intrebare,ID).
id_intrebare(Integrama, Intrebare, ID) :- intrebari(Integrama, Lista), id_intrebare_helper(Lista, Intrebare, ID).

% completare/3
% completare(+Integ, +Sol, -Integrama)
% Predicatul produce Integrama, o structură de forma integ(...),
% pornind de la Integ, în care au fost completate celule conform cu
% soluția Sol.
% Soluția este reprezentată ca o listă de perechi (Întrebare, Răspuns),
% unde Întrebarea este textul unei întrebări, iar Răspuns este un cuvând
% de completat; ambele sunt stringuri.
% De exemplu, o soluție parțială pentru integrama 0 poate fi:
% [('Din care plouă', 'NOR'), ('Al doilea număr', 'DOI')]
% BONUS: lungime_spatiu are o singură soluție pentru o anumită
% întrebare.
% Puteți testa manual predicatul cu o interogare de forma:
% integrama(0, W), solutie(0, Sol), completare(W, Sol, W2),
%   print_integrama(W2).
addCharJ(_,_,[],[]).
addCharJ(Lista,(R,C), [_|T], Rest) :- member(((R,C),_), Lista), R1 is R + 1, C1 is C,  addCharJ(Lista,(R1,C1),T, Rest).
addCharJ(Lista,(R,C),[H|T],[((R,C),H) | Rest]):- R1 is R + 1, C1 is C, addCharJ(Lista,(R1,C1), T, Rest).

addCharD(_,_,[],[]).
addCharD(Lista,(R,C), [_|T], Rest) :-  member(((R,C),_), Lista),R1 is R, C1 is C + 1, addCharD(Lista,(R1,C1),T, Rest).
addCharD(Lista,(R,C),[H|T],[((R,C),H) | Rest]):- R1 is R, C1 is C + 1, addCharD(Lista,(R1,C1), T, Rest).

changes(ListaPrim,(R,C),j, Lista, Rezultat) :- R1 is R + 1, C1 is C, addCharJ(ListaPrim,(R1,C1),Lista,Rezultat).
changes(ListaPrim,(R,C),d, Lista, Rezultat) :- R1 is R, C1 is C + 1, addCharD(ListaPrim,(R1,C1),Lista, Rezultat).

find_sol(ListaPrim,[((R,C), Text, Dir, _)|_],(Text, SolText), CharList) :- atom_chars(SolText,L), changes(ListaPrim,(R,C),Dir,L,CharList),!.
find_sol(ListaPrim,[_|T],(Text,SolText), CharList) :- find_sol(ListaPrim,T,(Text,SolText),CharList).

completare_helper(ListaPrim,_, [], ListaPrim).
completare_helper(ListaPrim,Lista,[H|T],Rez) :- find_sol(ListaPrim,Lista, H, Rez2), append(ListaPrim, Rez2,Rez3), completare_helper(Rez3, Lista, T, Rez).

completare(integ(H,W,Lista,Vocab), SolList, integ(H,W,NewLista,Vocab)) :- intrebari(integ(H,W,Lista,Vocab),QList), completare_helper(Lista,QList,SolList,NewLista).


% lungime_spatiu/3
% lungime_spatiu(integ(+H, +W, +Lista, +Vocab), +Intrebare, -Lungime)
% Returnează lungimea spațiului asociat întrebării date.
% Întrebarea este indicată prin textul ei. De exemplu:
% lungime_spatiu pentru integrama 0 și întrebarea 'Al doilea număr'
% trebuie să lege Lungime la 3.
% BONUS: lungime_spatiu are o singură soluție pentru o anumită
% întrebare.
% Puteți testa manual predicatul cu o interogare de forma:
% integrama(0, W), id_intrebare(W, Text, 3), lungime_spatiu(W, Text, X).

find_poz([((R, C), Text, _, _) | _], Text, (R,C)).
find_poz([_ | T], Text, (R,C)) :- find_poz(T,Text,(R,C)).

find_dir([((_, _), Text, Dir, _) | _], Text, Dir).
find_dir([_ | T], Text, Dir) :- find_dir(T,Text,Dir).


countJ(H,_,_, _, (H,_), 0).
countJ(_,_,_, [((R,C), Valoare) | _], (R,C), 0) :- is_list(Valoare),!.
countJ(_,_,_, [((R,C), x) | _], (R,C), 0).
countJ(H,W,Lista, [_ | T], (R,C), Len) :- countJ(H,W,Lista, T, (R,C), Len),!.
countJ(H,W, Lista, [], (R,C), Len) :- R1 is R + 1, C1 is C, countJ(H,W,Lista,Lista,(R1,C1), Ln), Len is Ln + 1, !. 

countD(_,W,_, _, (_,W), 0).
countD(_,_,_, [((R,C), Valoare) | _], (R,C), 0) :- is_list(Valoare), !.
countD(_,_,_, [((R,C), x) | _], (R,C), 0).
countD(H,W,Lista, [_ | T], (R,C), Len) :- countD(H,W,Lista, T, (R,C), Len), !.
countD(H,W,Lista, [], (R,C), Len) :- R1 is R, C1 is C + 1, countD(H,W,Lista, Lista, (R1,C1), Ln), Len is Ln + 1, !.


lungime_spatiu_helper(H,W,Lista, (R,C), j, Lungime) :- R1 is R + 1, C1 is C, countJ(H,W,Lista, Lista, (R1,C1), Lungime).
lungime_spatiu_helper(H,W,Lista, (R,C), d, Lungime) :- R1 is R, C1 is C + 1, countD(H,W,Lista, Lista, (R1,C1), Lungime).

lungime_spatiu(integ(H,W,Lista,Vocab), Intrebare, Lungime) :- intrebari(integ(H,W,Lista,Vocab), QList), find_poz(QList, Intrebare, Poz), find_dir(QList, Intrebare, Dir),
 lungime_spatiu_helper(H,W,Lista, Poz, Dir, Lungime).

% intersectie/5
% intersectie(integ(+H, +W, +Lista, +Voc), +I1, -Poz1, +I2, -Poz2)
% Pentru o integramă și două întrebări date prin textul lor (I1 și I2),
% al căror răspunsuri se intersectează, întoarce în Poz1 indicele din
% răspunsul la I1 la care este intersecția, și în Poz2 indicele din
% răspunsul la I2 la care este intersecția. Indecșii incep de la 0.
%
% De exemplu, în integrama 0:
%  █       █       2↓      3↓      █
%  █       0↓,1→   -       -       █
%  4→      -       -       -       █
%  5→      -       -       -       █
%  █       █       █       █       █
%
%  Întrebările 'Primii 3 din artă' și 'Afirmativ' (3, respectiv 1) se
%  intersectează la pozițiile 0, respectiv 2 (va fi litera A, de la
%  ART, respectiv DA).

matchDJ(L1,L2,H,W,(R1,C1),(R2,C2), Poz1, Poz2) :- Poz1 is C2 - C1, Poz2 is R1 - R2, Poz1 >=0, Poz2 >= 0, Poz1 < H, Poz2 < W, Poz1 < L1, Poz2 < L2, PP1 is Poz1 + C1, PP2 is Poz2 + R2, (R1, PP1) = (PP2, C2).
matchJD(L1,L2,H,W,(R1,C1),(R2,C2), Poz1, Poz2) :- Poz2 is C1 - C2, Poz1 is R2 - R1, Poz1 >= 0, Poz2 >=0, Poz1 < W, Poz2 < H, Poz1 < L1, Poz2 < L2, PP1 is Poz1 + R1, PP2 is Poz2 + C2, (PP1, C1) = (R2, PP2).           

intersectie_helper(L1,L2,H,W,(R1,C1), d, (R2,C2), j, Poz1, Poz2) :- R11 is R1, C11 is C1 + 1, R22 is R2 + 1, C22 is C2, matchDJ(L1,L2,H,W,(R11, C11), (R22,C22), Poz1, Poz2).
intersectie_helper(L1,L2,H,W,(R1,C1), j, (R2,C2), d, Poz1, Poz2) :- R11 is R1 + 1, C11 is C1, R22 is R2, C22 is C2 + 1, matchJD(L1,L2,H,W,(R11,C11),(R22,C22), Poz1, Poz2).

intersectie(integ(H,W,Lista,Vocab), I1, Poz1, I2, Poz2) :- intrebari(integ(H,W,Lista,Vocab), QList), find_poz(QList, I1, P1),find_dir(QList, I1, Dir1),
 find_poz(QList, I2, P2),find_dir(QList, I2, Dir2),lungime_spatiu(integ(H,W,Lista,Vocab), I1, L1), lungime_spatiu(integ(H,W,Lista,Vocab), I2, L2),
  intersectie_helper(L1,L2,H,W,P1, Dir1, P2, Dir2, Poz1, Poz2).

% solutii_posibile/2
% solutii_posibile(integ(+H, +W, +Lista, +Vocabular), -Solutii)
% Formează o listă Solutii, conținând perechi de forma
% (Întrebare, Cuvinte), unde
% Întrebare este textul unei întrebări din integramă, iar Cuvinte este o
% listă de cuvinte sunt din Vocabular și au lungimea corectă pentru a fi
% răspuns la întrebare. Solutii conține câte o pereche pentru fiecare
% întrebare din integramă.
% Cuvintele sunt reprezentate ca liste de stringuri, fiecare string
% având lungime 1 (o singură literă).
% De exemplu, pentru integrama 0, Solutii conține 6 perechi, două dintre
% ele fiind:
% ('Afirmativ', [['D', 'A'], ['N', 'U']])
% ('Din care plouă',
% [['N','O','R'],['A','R','T'],['U','I','T'],['D','O','I']])

formlist(_, [], []).
formlist(Len, [H | T], [H2 | T2]) :- string_length(H,Len2), Len2 =:= Len, atom_chars(H, H2), formlist(Len,T,T2),!.
formlist(Len, [_ | T], Rez) :- formlist(Len,T,Rez).

solutii_posibile_helper(_, [], []).
solutii_posibile_helper(integ(H,W,Lista, Vocab), [(_, Text, _, _) | T], [(Text, C) | Z]) :-  lungime_spatiu(integ(H,W,Lista, Vocab), Text, Len), formlist(Len,Vocab,C), solutii_posibile_helper(integ(H,W,Lista, Vocab),T,Z).

solutii_posibile(integ(H, W, Lista, Vocab), Solutii) :- intrebari(integ(H,W,Lista, Vocab), QList), solutii_posibile_helper(integ(H, W, Lista, Vocab), QList, Solutii).

% rezolvare/2
% rezolvare(+Integ, -Solutie)
% Rezolvare produce în Solutie soluția integramei Integ. Soluția este
% reprezentată ca o listă de perechi de stringuri, fiecare pereche
% conținând textul unei întrebări și cuvântul (ca string) care este
% răspunsul la întrebare.
rezolvare(_, _) :- false.
