% Numero - 106157 Nome - Leonor Costa Figueira
:- set_prolog_flag(answer_write_options,[max_depth(0)]). %para listas completas
:- ['dados.pl'], ['keywords.pl'].  %ficheiros a importar

%predicados auxiliares: -------------------------------------------------------


%predicado que elimina elementos repetidos de uma lista
eliminar_rep(Lst, Lst_sem_rep) :-
    eliminar_rep(Lst, Lst_sem_rep, []).

eliminar_rep([], Lst_sem_rep, Lst_sem_rep) :- !.

eliminar_rep([N|R], Lst_sem_rep, Lst_aux) :-
    \+ member(N, Lst_aux),
    append([N], Lst_aux, N_lstAux),
    eliminar_rep(R, Lst_sem_rep, N_lstAux).

eliminar_rep([N|R], Lst_sem_rep, Lst_aux) :-
    member(N, Lst_aux),
    eliminar_rep(R, Lst_sem_rep, Lst_aux).


%predicado que determina o numero de elementos de uma lista
nr_elementos(Lst, Nr_elementos) :-
    nr_elementos(Lst, Nr_elementos, 0).

nr_elementos([], Nr_elementos, Nr_elementos) :- !.

nr_elementos([_|R], Nr_elementos, Aux) :-
    New_aux is Aux + 1,
    nr_elementos(R, Nr_elementos, New_aux).


%predicados associados ao facto de algumas disciplinas serem semestrais:
perSemestral(p1, Periodo) :- Periodo = p1; Periodo = p1_2.
perSemestral(p2, Periodo) :- Periodo = p2; Periodo = p1_2.
perSemestral(p3, Periodo) :- Periodo = p3; Periodo = p3_4.
perSemestral(p4, Periodo) :- Periodo = p4; Periodo = p3_4.


%predicado que verifica se uma determinada disciplina e' do primeiro semestre
verifica_semestre1(NomeDisciplina, Curso) :-
    evento(ID,NomeDisciplina,_,_,_),
    turno(ID,Curso,_,_),
    !, 
    %aqui e' necessario um ID de apenas um 
    %evento da determinada disciplina e curso
    (perSemestral(p1, Periodo); 
    perSemestral(p2, Periodo)), 
    horario(ID,_,_,_,_,Periodo). 


%predicado que verifica se uma determinada disciplina e' do segundo semestre
verifica_semestre2(NomeDisciplina, Curso) :-
    evento(ID,NomeDisciplina,_,_,_),
    turno(ID,Curso,_,_),
    !,
    %aqui e' necessario um ID de apenas um 
    %evento da determinada disciplina e curso
    (perSemestral(p3, Periodo); 
    perSemestral(p4, Periodo)), 
    horario(ID,_,_,_,_,Periodo).


%predicido que determina o minimo entre dois numeros
min(A, B, A) :- A =< B.
min(A, B, B) :- A > B.


%predicido que determina o minimo entre dois numeros
max(A, B, A) :- A >= B.
max(A, B, B) :- A < B.


%predicado que recebe uma lista com tuplos, cada tuplo com uma hora de inicio
%de evento e hora final de evento e determina o numero de horas que se
%encontram no slot, este com hora inicio e hora final.
aux_numHorasOcupadas(Lst_HorasEventos, HoraInicio, HoraFim, HorasTotal) :-
    aux_numHorasOcupadas(Lst_HorasEventos, HoraInicio, HoraFim, HorasTotal, 0).

aux_numHorasOcupadas([],_,_, HorasTotal, HorasTotal) :- !.

aux_numHorasOcupadas([(HoraInicioEvento, HoraFimEvento)|R], 
                    HoraInicio, HoraFim, HorasTotal, SomaHoras) :-
    ocupaSlot(HoraInicio, HoraFim, HoraInicioEvento, HoraFimEvento, Horas),
    !, %para nao ir para o outro caso de aux_numHorasOcupadas
    New_SomaHoras is SomaHoras + Horas,
    aux_numHorasOcupadas(R, HoraInicio, HoraFim, HorasTotal, New_SomaHoras).

aux_numHorasOcupadas([(HoraInicioEvento, HoraFimEvento)|R], HoraInicio, 
                    HoraFim, HorasTotal, SomaHoras) :-
    %este caso foi adicionado na hipotese de o evento nao ocorrer entre
    %as horas do slot, uma vez que neste caso nao se quer adicionar horas
    \+ ocupaSlot(HoraInicio, HoraFim, HoraInicioEvento, HoraFimEvento,_),
    aux_numHorasOcupadas(R, HoraInicio, HoraFim, HorasTotal, SomaHoras).


%predicado que devolve dias uteis da semana
diaSemana(Semana) :- Semana = segunda-feira.
diaSemana(Semana) :- Semana = terca-feira.
diaSemana(Semana) :- Semana = quarta-feira.
diaSemana(Semana) :- Semana = quinta-feira.
diaSemana(Semana) :- Semana = sexta-feira.


%------------------------------------------------------------------------------


%predicado que devolve os IDs dos eventos sem sala
eventosSemSalas(Eventos) :-
    setof(ID, NomeDisciplina^Tipologia^NumAlunos^evento(ID, 
        NomeDisciplina, Tipologia, NumAlunos, semSala), Eventos).


%predicado que devolve os IDs dos eventos 
%sem sala num determinado dia da semana
eventosSemSalasDiaSemana(DiaDaSemana, Eventos) :-
    findall(ID, (horario(ID, DiaDaSemana,_,_,_,_), 
            evento(ID,_,_,_, semSala)), Lst_res),
    sort(Lst_res, Eventos).
    

%predicado que devolve os IDs dos eventos 
%sem sala no(s) determinado(s) periodo(s)
eventosSemSalasPeriodo(Periodos, Eventos) :- 
    eventosSemSalasPeriodo(Periodos, Eventos, []).

eventosSemSalasPeriodo([], Eventos, Eventos) :- !.

eventosSemSalasPeriodo([N|R], Eventos, Lst_Aux) :-
    setof(ID, Periodo^NomeDisciplina^Tipologia^NumAlunos^DiaSemana^HoraInicio
                ^HoraFim^Duracao^(perSemestral(N, Periodo), 
                evento(ID, NomeDisciplina, Tipologia, NumAlunos, semSala), 
                horario(ID, DiaSemana, HoraInicio, HoraFim, Duracao, Periodo)),
                Lst_ID),
                !, %pois se der aqui verdadeiro, e' 
                %desnecessario testar o proximo caso
    append(Lst_Aux, Lst_ID, N_lstAux),
    eliminar_rep(N_lstAux, N_lstSemRep),
    sort(N_lstSemRep, N_lstOrd),
    eventosSemSalasPeriodo(R, Eventos, N_lstOrd).

eventosSemSalasPeriodo([N|R], Eventos, Lst_Aux) :-
    \+ (perSemestral(N, Periodo), evento(ID,_,_,_, semSala), 
        horario(ID,_,_,_,_, Periodo)),
    eventosSemSalasPeriodo(R, Eventos, Lst_Aux).


%predicado que seleciona os eventos que ocorreu no determinado periodo
organizaEventos(ListaEventos, Periodo, EventosNoPeriodo) :-
    organizaEventos(ListaEventos, Periodo, EventosNoPeriodo, []).

organizaEventos([],_, EventosNoPeriodo, EventosNoPeriodo) :- !.

organizaEventos([N|R], Periodo, EventosNoPeriodo, Lst_Aux) :-
    perSemestral(Periodo, Per),
    horario(N,_,_,_,_,Per),
    \+ member(N, Lst_Aux), %para nao ter elementos repetidos
    !, %para nao ir para o outro caso de organizaEventos
    append([N], Lst_Aux, Lst_Res),
    sort(Lst_Res, Lst_ordenada),
    organizaEventos(R, Periodo, EventosNoPeriodo, Lst_ordenada).

organizaEventos([N|R], Periodo, EventosNoPeriodo, Lst_Aux) :- 
    \+ (perSemestral(Periodo, Per),
    horario(N,_,_,_,_,Per)),
    organizaEventos(R, Periodo, EventosNoPeriodo, Lst_Aux).


%predicado que devolve uma lista de todos os IDs dos eventos
%cuja duracao seja inferior 'a duracao passada no argumento
eventosMenoresQue(Duracao, ListaEventosMenoresQue) :-
    setof(ID, DiaSemana^HoraInicio^HoraFim^Periodo^Duracao_evento^
            (horario(ID,DiaSemana,HoraInicio,HoraFim,Duracao_evento,Periodo),
            Duracao_evento =< Duracao), ListaEventosMenoresQue).


%predicado que verifica se o evento identificado pelo determinado ID
%tem uma duracao inferior 'a duracao passada no argumento
eventosMenoresQueBool(ID, Duracao) :-
    horario(ID,_,_,_,Duracao_evento,_),
    Duracao_evento =< Duracao.


%predicado que devolve uma lista com todas as disciplinas do determinado curso
procuraDisciplinas(Curso, ListaDisciplinas) :-
    setof(NomeDisciplina, Tipologia^NumAlunos^Sala^Ano^NomeTurma^ID^
        (evento(ID,NomeDisciplina,Tipologia,NumAlunos,Sala),
        turno(ID,Curso,Ano,NomeTurma)), ListaDisciplinas).


%predicado que recebe uma lista de disciplinas e distribui as disciplinas 
%em duas listas: a primeira lista com as disciplinas do primeiro semestre
%e a segunda com as disciplinas do segundo semestre
organizaDisciplinas(ListaDisciplinas, Curso, Semestres) :-
    organizaDisciplinas(ListaDisciplinas, Curso, Semestres, [[],[]]).

organizaDisciplinas([],_,Semestres, Semestres) :- !.

organizaDisciplinas([N|R], Curso, Semestres, [Semestre1, Semestre2]) :-
    verifica_semestre1(N, Curso),
    !, %pois se der aqui verdadeiro, e' desnecessario testar o proximo caso
    \+ member(N, Semestre1), %para nao ter elementos repetidos 
    append([N], Semestre1, Lst_Res),
    sort(Lst_Res, Lst_ordenada),
    organizaDisciplinas(R, Curso, Semestres, [Lst_ordenada, Semestre2]).

organizaDisciplinas([N|R], Curso, Semestres, [Semestre1, Semestre2]) :-
    verifica_semestre2(N, Curso),
    \+ member(N, Semestre2), %para nao ter elementos repetidos
    append([N], Semestre2, Lst_Res),
    sort(Lst_Res, Lst_ordenada),
    organizaDisciplinas(R, Curso, Semestres, [Semestre1, Lst_ordenada]).


%predicado que determina o numero de horas total dos eventos associados
%ao determinado curso, no determinado ano e periodo
horasCurso(Periodo, Curso, Ano, TotalHoras) :-
    setof(ID, NomeTurma^DiaSemana^HoraInicio^HoraFim^Duracao^Per^
        (perSemestral(Periodo, Per),
        turno(ID, Curso, Ano, NomeTurma),
        horario(ID, DiaSemana, HoraInicio, HoraFim, 
                Duracao, Per)), Lst_eventos),
        !, %pois se der aqui verdadeiro, e' 
        %desnecessario testar o proximo caso
    horasCurso(Periodo, Curso, Ano, TotalHoras, 0, Lst_eventos).
    %foi criada uma lista dos determinados eventos; ao utilizar a duracao 
    %de cada evento, o numero de horas do mesmo e' contado apenas uma vez,
    %deste modo o numero de horas do evento nao e' contado multiplas vezes,
    %caso os varios turnos partilharem o mesmo evento

horasCurso(Periodo, Curso, Ano, TotalHoras) :-
    %caso nao existam eventos no determinado curso, ano e periodo
    \+ (perSemestral(Periodo, Per), 
        turno(ID, Curso, Ano,_),
        horario(ID,_,_,_,_, Per)),
    horasCurso(Periodo, Curso, Ano, TotalHoras, 0, []).

horasCurso(_,_,_,TotalHoras, TotalHoras, []) :- !.

horasCurso(Periodo, Curso, Ano, TotalHoras, Horas, [N|R]) :-
    horario(N,_,_,_,Duracao,_),
    N_horas is Horas + Duracao,
    horasCurso(Periodo, Curso, Ano, TotalHoras, N_horas, R).


%devolve uma lista de tuplos na forma (Ano, Periodo, NumHoras), em que
%NumHoras e' o total de horas associado ao determinado curso, ano e periodo 
evolucaoHorasCurso(Curso, Evolucao) :-
    evolucaoHorasCurso(Curso, Evolucao, [], [(1,p1),(1,p2),(1,p3),(1,p4),
                                        (2,p1),(2,p2),(2,p3),(2,p4),(3,p1),
                                        (3,p2),(3,p3),(3,p4)]).

evolucaoHorasCurso(_, Evolucao, Evolucao, []) :- !.

%aqui cada elemento da lista dos tuplos sera' um tuplo com o Ano e Periodo
evolucaoHorasCurso(Curso, Evolucao, Lst_Aux, [(Ano, Periodo)|R]) :-
    horasCurso(Periodo, Curso, Ano, TotalHoras),
    append(Lst_Aux, [(Ano, Periodo, TotalHoras)], N_lstAux),
    evolucaoHorasCurso(Curso, Evolucao, N_lstAux, R).


%predicado que determinado o numero de horas sobrepostas entre o evento com 
%inicio em HoraInicioEvento e fim em HoraFimEvento, e o determinado slot
%com inicio en HoraInicioDada e fim em HoraFimDada
ocupaSlot(HoraInicioDada, HoraFimDada, HoraInicioEvento, HoraFimEvento, Horas) :-
    max(HoraInicioDada, HoraInicioEvento, Max),
    min(HoraFimDada, HoraFimEvento, Min),
    Min - Max > 0,
    Horas is Min - Max.


%predicado que determina o numero de horas ocupadas num tipo
%especifico de salas, num determinado periodo e dia de semana,
%num intervalo de tempo definido entre HoraInicio e HoraFim
numHorasOcupadas(Periodo, TipoSala, DiaSemana, HoraInicio, HoraFim, SomaHoras) :-
    salas(TipoSala, Sala),
    numHorasOcupadas(Periodo, TipoSala, DiaSemana, HoraInicio, 
                    HoraFim, SomaHoras, 0, Sala). 
                    %aqui Sala e' uma lista de salas

numHorasOcupadas(_,_,_,_,_, SomaHoras, SomaHoras, []) :- !.

%aqui vai-se iterando sobre as salas, indo-se 
%somando as horas ocupadas em cada sala
numHorasOcupadas(Periodo, TipoSala, DiaSemana, HoraInicio, 
                HoraFim, SomaHoras, Horas_Aux, [N|R]) :-
    findall((HoraInicioEvento, HoraFimEvento), (perSemestral(Periodo, Per), 
            evento(ID,_,_,_,N), horario(ID, DiaSemana, HoraInicioEvento, 
                                        HoraFimEvento,_, Per)), 
                                        Lst_HorasEventos),
    aux_numHorasOcupadas(Lst_HorasEventos, HoraInicio, HoraFim, HorasTotal),
    New_Horas_Aux is Horas_Aux + HorasTotal,
    numHorasOcupadas(Periodo, TipoSala, DiaSemana, HoraInicio, 
                    HoraFim, SomaHoras, New_Horas_Aux, R).


%predicado que devolve o numero de horas possiveis de serem ocupadas
%por um tipo especifico de salas, num intervalo de tempo definido 
%entre HoraInicio e HoraFim
ocupacaoMax(TipoSala, HoraInicio, HoraFim, Max) :-
    salas(TipoSala, Sala),
    nr_elementos(Sala, Nr_salas),
    Max is (HoraFim - HoraInicio) * Nr_salas.


%predicado que determina a percentagem da relacao entre SomaHoras, numero,
%de horas ocupadas, e Max, numero de horas possiveis de serem ocupadas
percentagem(SomaHoras, Max, Percentagem) :-
    Percentagem is (SomaHoras / Max) * 100.


%predicado que devolve uma lista de tuplos do tipo:
%casosCriticos(DiaSemana, TipoSala, Percentagem), com o dia da semana, 
%o tipo de sala e a percentagem de ocupacao, em que a percentagem e' 
%superior ao valor de Threshold passado no argumento, no intervalo de
%tempo entre HoraInicio e HoraFim
ocupacaoCritica(HoraInicio, HoraFim, Threshold, Resultados) :-
    ocupacaoCritica(HoraInicio, HoraFim, Threshold, 
                    Resultados, [], [p1, p2, p3, p4]).

ocupacaoCritica(_,_,_, Resultados, Resultados, []) :- !.

ocupacaoCritica(HoraInicio, HoraFim, Threshold, Resultados, Lst_Aux, [N|R]) :-
    findall((casosCriticos(DiaSemana, TipoSala, Per_Arr)), 
            (diaSemana(DiaSemana),
            numHorasOcupadas(N,TipoSala, DiaSemana, 
                            HoraInicio, HoraFim, SomaHoras),
            ocupacaoMax(TipoSala, HoraInicio, HoraFim, Max), 
            percentagem(SomaHoras, Max, Percentagem), 
            Threshold < Percentagem, 
            ceiling(Percentagem, Per_Arr)), Lst_res),
    append(Lst_Aux, Lst_res, N_lstAux),
    sort(N_lstAux, N_lstAuxOrd),
    ocupacaoCritica(HoraInicio, HoraFim, Threshold, Resultados, N_lstAuxOrd, R).


%definicao das restricoes possiveis:
cab1(A, [_,_,_,A,_,_,_,_]).
cab2(A, [_,_,_,_,A,_,_,_]).
honra(A, B, [_,_,_,A,_,B,_,_]).
honra(A, B, [_,_,B,_,A,_,_,_]).
lado(A, B, [A,B,_,_,_,_,_,_]).
lado(A, B, [_,A,B,_,_,_,_,_]).
lado(A, B, [_,_,_,_,_,A,B,_]).
lado(A, B, [_,_,_,_,_,_,A,B]).
lado(A, B, [B,A,_,_,_,_,_,_]).
lado(A, B, [_,B,A,_,_,_,_,_]).
lado(A, B, [_,_,_,_,_,B,A,_]).
lado(A, B, [_,_,_,_,_,_,B,A]).
naoLado(A, B, Lst) :- \+ lado(A, B, Lst).
frente(A, B, [A,_,_,_,_,B,_,_]).
frente(A, B, [_,A,_,_,_,_,B,_]).
frente(A, B, [_,_,A,_,_,_,_,B]).
frente(A, B, [B,_,_,_,_,A,_,_]).
frente(A, B, [_,B,_,_,_,_,A,_]).
frente(A, B, [_,_,B,_,_,_,_,A]).
naoFrente(A, B, Lst) :- \+ frente(A, B, Lst).


%predicado que verifica se o posicionamento atual dos
%lugares da mesa cumpre todas as restricoes
testarCond([],_) :- !.

testarCond([N|R], Mesas) :-
    N =.. Lst, %passar um predicado N para uma lista; exemplo: 
                %frente(A, B), Lst = [frente, A, B]
    append(Lst, [Mesas], New_lst), %New_lst = [frente,A,B,[X,Y,Z,W,A,B,C,D]]
    Restricao =.. New_lst, %Restricao = frente(A,B,[X,Y,Z,W,A,B,C,D])
    Restricao, %frente(A,B,[X,Y,Z,W,A,B,C,D])  
    testarCond(R, Mesas).


%predicado que recebe uma lista de elementos e outra com restricoes, cujo
%objetivo e' determinar um posicionamento dos lugares dos elementos numa mesa,
%de forma a respeitar todas as resticoes 
ocupacaoMesa([],ListaRestricoes,[[X,Y,Z],[W,A],[B,C,D]]) :-
    testarCond(ListaRestricoes, [X,Y,Z,W,A,B,C,D]).
    %apenas no caso terminal e' verificado se o atual
    %posicionamento respeita todas as restricoes


%cada vez que todos os elementos sao sentados recursivamente nos 
%lugares da mesa e' testado se o posicionamento atual respeita todas 
%as restricoes; o prolog testa todas as possibilidades, e no final
%restam apenas a(s) solucao(oes) que respeitaram todas as restricoes
%(neste caso seria apenas uma solucao uma vez que assim o diz no enunciado)
ocupacaoMesa([N|R], ListaRestricoes, [[X,Y,Z],[W,A],[B,C,D]]) :-
    var(X),
    X = N,
    ocupacaoMesa(R, ListaRestricoes, [[X,Y,Z],[W,A],[B,C,D]]).

ocupacaoMesa([N|R], ListaRestricoes, [[X,Y,Z],[W,A],[B,C,D]]) :-
    var(Y),
    Y = N,
    ocupacaoMesa(R, ListaRestricoes, [[X,Y,Z],[W,A],[B,C,D]]).

ocupacaoMesa([N|R], ListaRestricoes, [[X,Y,Z],[W,A],[B,C,D]]) :-
    var(Z),
    Z = N,
    ocupacaoMesa(R, ListaRestricoes, [[X,Y,Z],[W,A],[B,C,D]]).

ocupacaoMesa([N|R], ListaRestricoes, [[X,Y,Z],[W,A],[B,C,D]]) :-
    var(W),
    W = N,
    ocupacaoMesa(R, ListaRestricoes, [[X,Y,Z],[W,A],[B,C,D]]).

ocupacaoMesa([N|R], ListaRestricoes, [[X,Y,Z],[W,A],[B,C,D]]) :-
    var(A),
    A = N,
    ocupacaoMesa(R, ListaRestricoes, [[X,Y,Z],[W,A],[B,C,D]]).

ocupacaoMesa([N|R], ListaRestricoes, [[X,Y,Z],[W,A],[B,C,D]]) :-
    var(B),
    B = N,
    ocupacaoMesa(R, ListaRestricoes, [[X,Y,Z],[W,A],[B,C,D]]).

ocupacaoMesa([N|R], ListaRestricoes, [[X,Y,Z],[W,A],[B,C,D]]) :-
    var(C),
    C = N,
    ocupacaoMesa(R, ListaRestricoes, [[X,Y,Z],[W,A],[B,C,D]]).

ocupacaoMesa([N|R], ListaRestricoes, [[X,Y,Z],[W,A],[B,C,D]]) :-
    var(D),
    D = N,
    ocupacaoMesa(R, ListaRestricoes, [[X,Y,Z],[W,A],[B,C,D]]).