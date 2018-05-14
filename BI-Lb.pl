% Lb Bracket induction

:- op(400, xfx, fs).    % over
:- op(400, xfx, bs).    % under
:- op(400, xfx, pr).    % product
:- op(300, fy, ab).     % antibracket
:- op(300, fy, br).     % bracket

tt(N) :- time(t(N)).
t(N) :- prs(N).

prs(N) :- fetch(N, Str, A),
  lookup(Str, Zone, Pros),
  p(Zone, A), nl,
  pppros(Pros),
  fail.

prs(_).

pppros([Onset, W, Offset]) :- !,
  pppros1(Onset, W, Offset).

pppros([Onset, W, Offset|Config]) :- pppros1(Onset, W, Offset),
  write(' '),
  pppros(Config).

pppros1(Onset, W, Offset) :- ppon(Onset), write(W), ppoff(Offset).

ppoff([]).
ppoff([0|Set]) :- write(']'), ppoff(Set).

ppon([]).
ppon([0|Set]) :- write('['), ppon(Set).

% id
p([[], P, []], P) :- primitive(P).
 
% /L
p(Zone, D) :-
  nappend3([Delta1, [F2, C fs B, []|GammaG1G2], Delta2], Zone),
  append(Gamma, [G1G2], GammaG1G2),
  append(Gamma, [G1], Minor),
  p(Minor, B),
  append(Delta1, [F2, C, G2|Delta2], Major),
  p(Major, D),
  append(G1, G2, G1G2).

% /R
p(Delta, C fs B) :-
  append(Delta, [[], B, []], Antec),
  p(Antec, C).

% \L
p(Zone, D) :-
  nappend3([Delta1, [F2F1|Gamma], [[], A bs C, G2|Delta2]], Zone),
  p([F1|Gamma], A),
  append(Delta1, [F2, C, G2|Delta2], Major),
  p(Major, D),
  append(F2, F1, F2F1).

% \R
p(Delta, A bs C) :-
  p([[], A, []|Delta], C).

% *L
p(Zone, D) :- append3(Delta1, [F, A pr B, G|Delta2], Zone),
  append(Delta1, [F, A, [], [], B, G|Delta2], Antec),
  p(Antec, D).

% *R
p(Delta1Delta2, A pr B) :-
  append3(Delta1, Delta2, Delta1Delta2),
  p(Delta1, A),
  p(Delta2, B).
  
% []-1L
p(Zone, B) :-
  append3(Delta1, [F0, ab A, [0|G]|Delta2], Zone),
  append(Delta1, [F, A, G|Delta2], Antec),
  p(Antec, B),
  append(F, [0], F0).

% []-1R
p([F|OmegaG], ab B) :-
  append(Omega, [G], OmegaG),
  append(Omega, G0, OmegaG0),
  p([[0|F]|OmegaG0], B),
  append(G, [0], G0).

% <>L
p(Zone, B) :-
  append3(Delta1, [F, br A, G|Delta2], Zone),
  append(Delta1, [F0, A, [0|G]|Delta2], Antec),
  p(Antec, B),
  append(F, [0], F0).

% <>R
p([[0|F]|OmegaG0], br B) :-
  append(Omega, [G0], OmegaG0),
  append(Omega, [G], OmegaG),
  p([F|OmegaG], B),
  append(G, [0], G0).
  
% append3(Ls1, Ls2, Ls) means that the concatenation of Ls1
% and Ls2, of lengths multiples of three, is Ls
append3([], [], []).
  
append3([], [X, Y, Z|Ls2], [X, Y, Z|Ls]) :-
  append3([], Ls2, Ls).

append3([X, Y, Z|Ls1], Ls2, [X, Y, Z|Ls]) :- 
  append3(Ls1, Ls2, Ls).

% nappend(?Ls, ?L) means that L is the concatenation of the
% list of lists Ls.
nappend([], []).

nappend([[]|Ls], L) :-
  nappend(Ls, L).

nappend([[H|L1]|Ls], [H|L]) :- 
  nappend([L1|Ls], L).

% nappend3(?Ls, ?L) means that L is the concatenation of the
% list of lists of length multiple of three Ls.
nappend3([], []).

nappend3([[]|Ls], L) :-
  nappend3(Ls, L).

nappend3([[H, I, J|L1]|Ls], [H, I, J|L]) :- 
  nappend3([L1|Ls], L).

primitive(n(_)).
primitive(s(_)).
primitive(cn(_)).

% lookup(+Ws, -Config, -Pros) means that list of words Ws has result
% of lexical lookup the uninstantiated bracketing Configuration Config, 
% and that the corresponding uninstantiated bracketing prosodic form is Pros
lookup([], [], []).

lookup(WWsWs2, [On, A, Off|Config], [On, W|Pros]) :- 
  append([W|Ws], Ws2, WWsWs2),
  lex([W|Ws], A, _),
  lookup(Ws2, Config, Pros2),
  append(Ws, [Off|Pros2], Pros).

lex(['John'], n(t(s(m))), john).
lex([man], cn(s(m)), man).
lex([loves], (br n(t(s(_)))bs s(f))fs n(_), love).
lex(['Mary'], n(t(s(m))), mary).
lex([that], ab ab (cn(N)bs cn(N))fs(br n(t(N))bs s(f)), that).
lex([that], ab ab (cn(N)bs cn(N))fs(s(f) fs n(t(N))), that).

fetch(N, Str, A) :- str(N, Str, A),
  nl, nl, write(N), write(' '), write(Str), write(' '), write(A), nl.

str(1, ['John', loves, 'Mary'], s(f)).
str(2, [man, that, loves, 'Mary'], cn(s(m))).
str(3, [man, that, 'Mary', loves], cn(s(m))).
