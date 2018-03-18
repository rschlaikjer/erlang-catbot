-module(levenshtein).
-export([distance/2]).

distance(S,T) ->
    ld(S,T).

ld([],T) ->
    length(T);
ld(S,[]) ->
    length(S);
ld([X|S],[X|T]) ->
    ld(S,T);
ld([_SH|ST]=S,[_TH|TT]=T) ->
    1 + lists:min([ld(S,TT),ld(ST,T),ld(ST,TT)]).
