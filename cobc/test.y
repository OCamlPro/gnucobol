
%token IS "IS"
%token A "A"
%token C "C"
%token B "B"


%nonassoc SHIFT_PREFER
%nonassoc  IS

%%
start:
    sentences_list

_is:
|IS

a:
    A _c



b:
  _is B

_c:
    %prec SHIFT_PREFER
| _is C

sentence:
    a 
    |b


sentences_list:
    sentence
    |sentences_list sentence
    
