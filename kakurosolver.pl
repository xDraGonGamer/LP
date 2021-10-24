:- [codigo_comum].

%  ist199333 Tiago Santos


%  combinacoes_soma(N, Els, Soma, Combs)
%  Combs eh a lista ordenada cujos elementos sao listas de N inteiros de
%  Els cuja soma eh Soma.

combinacoes_soma(N, Els, Soma, Combs):-
    findall(X, (combinacao(N, Els, X), sum_list(X, Soma)), Combs).


%  permutacoes_soma(N, Els, Soma, Perms)
%  Perms eh a lista ordenada cujos elementos sao listas (e as suas
%  permutacoes)de N inteiros de Els cuja soma eh Soma.

permutacoes_soma(N, Els, Soma, Perms):-
    combinacoes_soma(N, Els, Soma, Combs),
    findall(X1, (member(X, Combs), permutation(X, X1)), Perms1),
    sort(Perms1, Perms).


%  Construtor do tipo abstrato de dados espaco.
%  Recebe um inteiro maior ou igual a 0, Sum, e uma lista e retorna o
%  espaco correspondente.

constroi_espaco(Soma, Pos, espaco(Soma, Pos)):-
    Soma >= 0, is_list(Pos). %verificacao dos argumentos

primeiro([P | _], P). %retorna em P o primeiro elemento de uma lista

eh_posicao(Pos):- %verifica se o seu argumento e uma posicao valida
    is_list(Pos), primeiro(Pos, X), last(Pos, Y),
    X >= 0, Y >= 0.

linha(Pos, X):- %retorna em X a linha de uma posicao
    last(Pos, X).

coluna(Pos, X):- %retorna em X a coluna de uma posicao
    primeiro(Pos, X).

eh_posicao_vazia(Pos):- %verifica se Pos e uma posicao vazia
    eh_posicao(Pos), coluna(Pos, X), linha(Pos, Y),
    X == 0, Y == 0.


%  espaco_fila(Fila, Esp, H_V)
%  Fila e uma fila do puzzle
%  H_V e o atomo h de horizontal ou v de vertical
%  Esp e um espaco da fila Fila

espaco_fila([P | R], Esp, H_V):-
    eh_posicao(P), \+ eh_posicao_vazia(P), %verifica se P e uma posicao nao vazia
    (   H_V == h     %coloca em X
    ->  linha(P, X)  %o valor que devera
    ;   coluna(P, X) %ser a soma da fila
    ),
    obtem_variaveis(R, [], Variaveis), length(Variaveis, Y), %obtem as variaveis desde P ate a proxima lista
    (   Y =\= 0
    ->  constroi_espaco(X, Variaveis, Esp)
    );
    espaco_fila(R, Esp, H_V).

%  obtem_variaveis(Lst1, Aux, Variaveis)
%  Lst1 e uma lista
%  Aux e uma variavel auxiliar
%  Variaveis e a lista que contem todas as variaveis seguidas nao
%  interrompidas

obtem_variaveis([P | _], Variaveis, Variaveis):-
    nonvar(P). %caso P nao seja uma variavel, para e retorna Aux

obtem_variaveis([], Variaveis, Variaveis):- !. %caso chegue ao fim, retorna Aux

obtem_variaveis([P | R], Aux, Variaveis):-
    var(P), append(Aux, [P], Aux1), %se P e var, adiciona P a Aux
    obtem_variaveis(R, Aux1, Variaveis). %chamada recursiva


%  espacos_fila(H_V, Fila, Espacos)
%  H_V e o atomo h de horizontal ou v de vertical
%  Fila e uma fila do puzzle
%  Espacos e a lista de todos os espacos de Fila, da esquerda para a
%  direita

espacos_fila(H_V, Fila, []):-
    \+ bagof(X, espaco_fila(Fila, X, H_V), _), !.

espacos_fila(H_V, Fila, Espacos):-
    bagof(X, espaco_fila(Fila, X, H_V), Espacos).


%  espacos_puzzle(Puzzle, Espacos)
%  Puzzle- Representacao interna do puzzle
%  Espacos- lista de espacos do puzzle

espacos_puzzle([], _):- !.

espacos_puzzle(Puzzle, Espacos):-
    mat_transposta(Puzzle, Verticais),
    espacos_puzzle_aux(Puzzle, h, [], Esp1),
    espacos_puzzle_aux(Verticais, v, [], Esp2),
    append(Esp1, Esp2, Espacos).

%  espacos_puzzle_aux(Puzzle, H_V, Aux, Espacos)
%  Puzzle- puzzle
%  H_V- h ou v, indicando linhas ou colunas
%  Aux- Variavel auxiliar
%  Espacos- espacos do puzzle referentes a linhas ou colunas

espacos_puzzle_aux([], _, Res, Res):- !. %base da recursao

espacos_puzzle_aux([P | R], H_V, Aux, Espacos):-
    espacos_fila(H_V, P, X), %todos os espacos da fila
    append(Aux, X, NAux),
    espacos_puzzle_aux(R, H_V, NAux, Espacos). %funcao mas com o puzzle sem a fila considerada anteriormente

%  pertence(El, Lst)
%  El- elemento
%  Lst- lista
%  Retorna true se El esta em Lst e false caso contrario

pertence(_, []):- %nenhum elemento pertence a lista vazia
    !, fail.
pertence(N, [P | R]):-
    N == P, !; %se pertence, para o retrocesso e retorna true
    pertence(N, R).


%  intersecao(Lst1, Lst2, Res)
%  Lst1, Lst2- sao listas
%  Res- lista com os elementos que pertencem a ambas as listas

intersecao(Lst1, Lst2, Res):-
    intersecao(Lst1, Lst2, [], Res). %chamada a funcao auxiliar

intersecao([], _, Intersecao, Intersecao):- !. %base da recursao

intersecao([P | R], Lst2, Intersecao, Res):-
    pertence(P, Lst2)-> %se P e elemento de Lst2
    append(Intersecao, [P], NewIntersecao), %adiciona P a lista
    intersecao(R, Lst2, NewIntersecao, Res); %chamadas
    intersecao(R, Lst2, Intersecao, Res).    %recursivas

%  espaco_tem_variaveis_em_comum(Esp1, Esp2)
%  Esp1, Esp2- sao espacos
%  Retorna true se ambos os espacos contem variaveis em comum

espaco_tem_variaveis_em_comum(espaco(_, Vars), espaco(_, Vars2)):-
    intersecao(Vars, Vars2, Res), Res \== []. %se a intersecao for vazia, nao teem variaveis em comum

%  espacos_com_posicoes_comuns(Espacos, Esp, Esps_com)
%  Espacos- lista de espacos
%  Esp- espaco
%  Esp_com- lista de espacos de Espacos com variaveis em comum com Esp
%  excluindo Esp

espacos_com_posicoes_comuns(Espacos, Esp, Esp_com):-
    bagof(X, (member(X, Espacos), espaco_tem_variaveis_em_comum(X, Esp), X \== Esp), Esp_com). %retorna todos os espacos com variaveis em comum

%  permutacoes_soma_espacos(Espacos, Perms_soma)
%  Espacos- lista de espacos
%  Perms_soma- lista de listas de 2 elementos em que o primeiro
%  elemento e um espaco de Espacos e o segundo e a lista ordenada
%  de permutacoes cuja soma e igual a soma do espaco

permutacoes_soma_espacos(Espacos, Perms_soma):-
    !, permutacoes_soma_espaco(Espacos, [], Perms_soma). %chamada auxiliar

permutacoes_soma_espaco([], Aux, Aux):- !. %base da recursao

permutacoes_soma_espaco([espaco(Soma, Vars) | R], Aux, Perms_soma):-
    Els = [1, 2, 3, 4, 5, 6, 7, 8, 9],
    length(Vars, Length),
    !, permutacoes_soma(Length, Els, Soma, X), %todas as permutacoes que satisfazem o espaco
    P = [espaco(Soma, Vars), X],
    append(Aux, [P], NAux),
    permutacoes_soma_espaco(R, NAux, Perms_soma). %chamada recursiva

%  permutacao_possiveo_espaco(Perm, Esp, Espacos, Perms_soma)
%  Perm- Permutacao possivel para o espaco
%  Esp- Espaco
%  Espacos- lista de espacos
%  Perms_soma- lista de listas de 2 elementos em que o primeiro
%  elemento e um espaco de Espacos e o segundo e a lista ordenada
%  de permutacoes cuja soma e igual a soma do espaco

permutacao_possivel_espaco(Perm, espaco(Soma, Vars), Espacos, Perms_soma):-
    espacos_com_posicoes_comuns(Espacos, espaco(Soma, Vars), Esp_com), %retorna os espacos com variaveis em comum
    length(Vars, Length),
    permutacao_possivel_espaco_filtro(Esp_com, Perms_soma, [], Filtrado), %filtra a lista Perms_soma de modo a que so tenha espacos com variaveis em c    omum
    permutacoes_soma(Length, [1, 2, 3, 4, 5, 6, 7, 8, 9], Soma, Perms_de_esp), %retorna todas as permutacoes possiveis de Esp
    %retorna todas as permutacoes que satisfazem o pre    dicado permutacao_possivel_espaco_aux
    member(Y, Perms_de_esp), permutacao_possivel_espaco_aux(Y, Y, Filtrado, Perm).

%  permutacao_possivel_espaco_filtro(Espacos, Lst, Aux, Res)
%  Espacos- Lista de espacos
%  Lst- Lista de espacos e as suas permutacoes possiveis
%  Aux- Variavel auxiliar a funcao
%  Res- Lista correspondente aos elementos de Lst que sao espacos de
%       Espacos

permutacao_possivel_espaco_filtro(_, [], Aux, Aux):- !. %base da recursao

permutacao_possivel_espaco_filtro(Espacos, [[Esp, X] | R2], Aux, Res):-
    (   member(Esp, Espacos) %se Esp for um espaco de Espacos
    ->  append(Aux, [[Esp, X]], NAux) %junta o elemento de Lst a lista auxiliar
    ;   NAux = Aux %caso contrario mantem
    ),
    permutacao_possivel_espaco_filtro(Espacos, R2, NAux, Res). %chamada recursiva

%  permutacao_possivel_espaco_aux(Lst, Els, Perms_soma, perm)
%  Lst- Permutacao a ser avaliada
%  Els- Elementos iterados de Lst
%  Perms_soma- Lista de espacos e as suas permutacoes
%  Perm- Permutacao que verifica que o seu elemento indice i e membro
%  das permutacoes do espaco indice i

permutacao_possivel_espaco_aux(Lst, [], _, Lst):- !. %base da recursao

permutacao_possivel_espaco_aux(Lst, [P | R], [[_, X] | Esps], Perm):-
    append(X, Numeros), %junta todos os elementos das permutacoes de um espaco numa so lista
    member(P, Numeros)-> permutacao_possivel_espaco_aux(Lst, R, Esps, Perm) %verifica se e membro, caso for a funcao continua
    ; fail.

%  PermutacoesPossiveisEspaco(Espacos, Perms_soma, Esp, Perms_poss)
%  Espacos- Lista de espacos
%  Perms_soma- Lista de listas de 2 elementos em que o primeiro
%  elemento e um espaco de Espacos e o segundo e a lista ordenada
%  de permutacoes cuja soma e igual a soma do espaco
%  Esp- Espaco
%  Perms_poss- Lista de 2 elementos em que o primeiro e a lista de
%  variaveis de Esp e o segundo e a lista ordenada de permutacoes
%  possiveis para o espaco Esp

permutacoes_possiveis_espaco(Espacos, Perms_soma, espaco(Soma, Vars), Perms_poss):-
    bagof(Perm, permutacao_possivel_espaco(Perm, espaco(Soma, Vars), Espacos, Perms_soma), Aux),
    append([Vars], [Aux], Perms_poss).

%  permutacoes_possiveis_espacos(Espacos, Perms_poss_esps)
%  Espacos- Lista de espacos
%  Perms_poss_esps- Lista de permutacoes possiveis de todos os espacos

permutacoes_possiveis_espacos(Espacos, Perms_poss_esps):-
    permutacoes_soma_espacos(Espacos, Perms_soma),
    bagof(X, permutacoes_possiveis_espacos_aux(Espacos, Perms_soma, X), Perms_poss_esps).

permutacoes_possiveis_espacos_aux(Espacos, Perms_soma, X):- %funcao auxiliar
    member(Y, Espacos), permutacoes_possiveis_espaco(Espacos, Perms_soma, Y, X).


%  numeros_comuns(Lst_Perms, Numeros_comuns)
%  Lst_Perms- Lista de permutacoes
%  Numeros_comuns- Lista de pares (pos, numero)

numeros_comuns(Lst_Perms, Numeros_comuns):-
    numeros_comuns(Lst_Perms, 1, [], Numeros_comuns). %chamada auxiliar

numeros_comuns([[] | _], _, Aux, Aux):- !.

numeros_comuns([[P | R] | R1], Index, Aux, Numeros_comuns):-
    (   numeros_comuns_aux(P, Index, R1) %caso P esteja em todas as listas
    ->  append(Aux, [(Index, P)], NAux)  %adiciona o par
    ;   NAux = Aux                       %caso nao pertenca atualiza a variavel NAux para a Aux anterior
    ),
    NIndex is Index + 1,                 %atualiza o Index atual
    numeros_comuns([R | R1], NIndex, NAux, Numeros_comuns), !. %chamada recursiva

%  numeros_comuns_aux(El, Index, Listas)
%  El- Elemento a avaliar
%  Index- Posicao a avaliar
%  Listas- Lista de Listas

numeros_comuns_aux(_, _, []):- !. %caso ja tenha percorrido todas as listas, base da recursao

numeros_comuns_aux(El, Index, [L | R]):-
    \+ nth1(Index, L, El), !, fail;    %se nao pertencer, para e retorna falso
    numeros_comuns_aux(El, Index, R).  %chamada recursiva


%  atribui_comuns(Perms_Possiveis)
%  Perms_Possiveis- Lista de permutacoes possiveis
%  A funcao atualiza a lista Perms_Possiveis, substituindo cada espaco
%  numeros comuns.

atribui_comuns([]):- !.

atribui_comuns([P | R]):-
    atribui_comuns_aux(P), atribui_comuns(R).

atribui_comuns_aux([Vars, Permutacoes]):-
    numeros_comuns(Permutacoes, Pares), %ve os pares validos
    atribui_comuns_aux_aux(Vars, Pares). %chama outra auxiliar

atribui_comuns_aux_aux(_, []):- !. %quando acaba a lista para

atribui_comuns_aux_aux(Vars, [(Index, Val) | R]):-
    nth1(Index, Vars, Val), %atribui o valor a variavel
    atribui_comuns_aux_aux(Vars, R). %chamada recursiva


%  retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis)
%  Perms_Possiveis- Lista de permutacoes possiveis
%  Novas_Perms_Possiveis- Perms_Possiveis mas sem as permutacoes
%  impossiveis

retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis):-
    !, retira_impossiveis(Perms_Possiveis, [], Novas_Perms_Possiveis).

retira_impossiveis([], Aux, Aux):- !.

retira_impossiveis([[Posicoes, Perms] | R], Aux, Novas_Perms_Possiveis):-
    retira_numeros_com_pos(Posicoes, 1, [], Pares), %(pos, val), em que posicao e a posicao na qual val se encontra se val e um numero
    retira_impossiveis_aux(Perms, Pares, [], Novas_Perms), %retira as permutacoes que nao satisfacam os pares
    append(Aux, [[Posicoes, Novas_Perms]], NAux),
    retira_impossiveis(R, NAux, Novas_Perms_Possiveis). %chamada recursiva


retira_impossiveis_aux([], _, Aux, Aux):- !.

retira_impossiveis_aux([Perm | R], Pares, Aux, Novas_Perms):-
    (   \+ satisfaz_pares(Perm, Pares) -> retira_impossiveis_aux(R, Pares, Aux, Novas_Perms) %caso nao satisfaca
        ; append(Aux, [Perm], NAux), retira_impossiveis_aux(R, Pares, NAux, Novas_Perms) %caso satisfaca, faz o append
    ).


%  satisfaz_pares(Perm, Pares):-
%  Perm- Permutacao
%  Pares- Lista de pares (pos, val)
%  Retorna true caso em cada posicao pos de Perm esteja o elemento val

satisfaz_pares(_, []):- !.

satisfaz_pares(Perm, [(Pos, Valor) | R]):-
    nth1(Pos, Perm, Valor), satisfaz_pares(Perm, R).


%  retira_numeros_com_pos(Lst, Index, Aux, Pares)
%  Lst- Lista de elementos
%  Index- Index a comecar
%  Aux- Lista auxiliar, inicialmente vazia
%  Pares- Lista de pares (pos, val) em que pos e a posicao de val na
%         lista Lst

retira_numeros_com_pos([], _, Aux, Aux):- !.

retira_numeros_com_pos([P | R], Index, Aux, Pares):-
    \+number(P)-> NIndex is Index + 1, retira_numeros_com_pos(R, NIndex, Aux, Pares); %no caso de nao ser numero
    append(Aux, [(Index, P)], NPares), NIndex is Index + 1, %caso seja numero, faz append do par
    retira_numeros_com_pos(R, NIndex, NPares, Pares).       %e chamada recursiva


%  simplifica(Perms_Possiveis, Novas_Perms_Possiveis)
%  Perms_Possiveis- Lista de permutacoes possiveis
%  Novas_Perms_Possiveis- Resultado de simplificar Perms_Possiveis

simplifica(Perms_Possiveis, Novas_Perms_Possiveis):-
    !, simplifica_aux(Perms_Possiveis, [], Novas_Perms_Possiveis), !.

simplifica_aux(Perms_Possiveis, Perms_Possiveis, Perms_Possiveis):- !.

simplifica_aux(Perms_Possiveis, _, Novas_Perms_Possiveis):-
    atribui_comuns(Perms_Possiveis),
    retira_impossiveis(Perms_Possiveis, NPerms_Possiveis),
    simplifica_aux(NPerms_Possiveis, Perms_Possiveis, Novas_Perms_Possiveis).



%  inicializa(Puzzle, Perms_Possiveis)
%  Puzzle- Puzzle de jogo
%  Perms_Possiveis- Lista de permutacoes simplificada para Puzzle

inicializa(Puzzle, Perms_Possiveis):-
    espacos_puzzle(Puzzle, Espacos),
    permutacoes_possiveis_espacos(Espacos, Perms_poss_esps),
    simplifica(Perms_poss_esps, Perms_Possiveis).


%  escolhe_menos_alternativas(Perms_Possiveis, Escolha)
%  Perms_Possiveis- Lista de permutacoes possiveis
%  Escolha- Elemento de Perms_Possiveis escolhido

escolhe_menos_alternativas([[Posicoes, Perms] | R], Escolha):-
    length(Perms, X),
    (   X < 2 -> escolhe_menos_alternativas(R, Escolha)
    ;Escolha = [Posicoes, Perms]
    ).


%  experimenta_perm(Escolha, Perms_Possiveis, Novas_Perms_Possiveis)
%  Escolha- Escolha (elemento de Perms_Possiveis)
%  Perms_Possiveis- Lista de permutacoes possiveis
%  Novas_Perms_Possiveis- Resultado de substitui em Perms_Possiveis o
%  elemento Escolha pelo elemento [Esp, [Perm]]


experimenta_perm([Esp, Lst_Perms], Perms_Possiveis, Novas_Perms_Possiveis):-
    nth0(0, Escolha, Esp), nth0(1, Escolha, Lst_Perms),
    member(Perm, Lst_Perms),
    Esp = Perm,
    substitui(Escolha, [Esp, [Perm]], Perms_Possiveis, Novas_Perms_Possiveis).


substitui(_, _, [], []).
substitui(A, B, [A|R], [B|R2]) :- substitui(A, B, R, R2).
substitui(A, B, [P|R], [P|R2]) :- P \= A, substitui(A, B, R, R2).


%  resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis)
%  Perms_Possiveis- Lista de permutacoes possiveis
%  Novas_Perms_Possiveis- Resultado de aplicar o algoritmo descrito na
%  seccao 2.2 a Perms_Possiveis

resolve_aux(Perms_Possiveis, Perms_Possiveis):-
    \+ escolhe_menos_alternativas(Perms_Possiveis, _).

resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis):-
    escolhe_menos_alternativas(Perms_Possiveis, Escolha),
    experimenta_perm(Escolha, Perms_Possiveis, NPerms_Possiveis),
    simplifica(NPerms_Possiveis, Simplificado),
    resolve_aux(Simplificado, Novas_Perms_Possiveis).


%  resolve(Puz)
%  Puz- Puzzle
%  A funcao resolve o Puzzle Puz

resolve(Puz):-
    inicializa(Puz, Perms_Possiveis),
    resolve_aux(Perms_Possiveis, _), !.
