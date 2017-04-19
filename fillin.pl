:- ensure_loaded(library(clpfd)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  File      : fillin.pl                                                   %%
%%  Author    : Lan Yang                                                    %%
%%  Student Id: 746569                                                      %%
%%  User name : lany4                                                       %%
%%  Purpose   : An implementation of fillin puzzle game . This program will %%
%%              be given a puzzle and a wordlist which is used to slove the %%  
%%              puzzle. The output of this program is the solution of this  %%
%%              game.                                                       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This is the Stater given on the LMS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main(PuzzleFile, WordlistFile, SolutionFile) :-
	read_file(PuzzleFile, Puzzle),
	read_file(WordlistFile, Wordlist),
	valid_puzzle(Puzzle),
	samelength(Puzzle, Sloved),
   	maplist(samelength, Puzzle, Sloved),
	solve_puzzle(Puzzle, Wordlist, Solved),
	print_puzzle(SolutionFile, Solved).

read_file(Filename, Content) :-
	open(Filename, read, Stream),
	read_lines(Stream, Content),
	close(Stream).

read_lines(Stream, Content) :-
	read_line(Stream, Line, Last),
	(   Last = true
	->  (   Line = []
	    ->  Content = []
	    ;   Content = [Line]
	    )
	;  Content = [Line|Content1],
	    read_lines(Stream, Content1)
	).

read_line(Stream, Line, Last) :-
	get_char(Stream, Char),
	(   Char = end_of_file
	->  Line = [],
	    Last = true
	; Char = '\n'
	->  Line = [],
	    Last = false
	;   Line = [Char|Line1],
	    read_line(Stream, Line1, Last)
	).

print_puzzle(SolutionFile, Puzzle) :-
	open(SolutionFile, write, Stream),
	maplist(print_row(Stream), Puzzle),
	close(Stream).

print_row(Stream, Row) :-
	maplist(put_puzzle_char(Stream), Row),
	nl(Stream).

put_puzzle_char(Stream, Char) :-
	(   var(Char)
	->  put_char(Stream, '_')
	;   put_char(Stream, Char)
	).

valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
	maplist(samelength(Row), Rows).


samelength([], []).
samelength([_|L1], [_|L2]) :-
	same_length(L1, L2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This is the first part to solve the puzzle.
%% This part is aim to assign each '_' in the slot a variable, and also keep 
%% the original '#' or fixed letter in it.
%% transferPuzzle is aim to initial a list which has same structure as the 
%% given puzzle. resetPuzzle is aim to compare a list and a slot in original 
%% puzzle, then replce all the variables if it is not '_' in the original
%% puzzle. Then, convertPuzzle use maplist to make the replce happen in the 
%% whole slots in the puzzle and get a puzzle we already processed.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

convertPuzzle(Puzzle,NewPuzzle) :-
	transferPuzzle(Puzzle,TransPuzzle),
	maplist(resetPuzzle,TransPuzzle,Puzzle,NewPuzzle).

transferPuzzle([],[]).
transferPuzzle([Head1|Tail1],[Head2|Tail2]) :-
	samelength([Head1|Tail1],[Head2|Tail2]),transferPuzzle(Tail1,Tail2).

resetPuzzle([],[],[]).
resetPuzzle([Head1|Tail1],[Head2|Tail2],[Head3|Tail3]) :-
	(Head2 = '_' -> 
		Head3 = Head1;
		Head3 = Head2),resetPuzzle(Tail1,Tail2,Tail3).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This is the second part to solve the puzzle.
%% In this part, we will transfer a puzzle into a list. It is not just simply  
%% put each line into to list. First, (split_List) split each row if there is 
%%  a '#', for eample, if the row is ['_','_','#','_','_'], it will become
%% [['_','_'],['_','_']], Then, we use maplist to make it happen in whole 
%% puzzle. After using maplist, each row, will be in a list, so the result 
%% will become list of list of list, so we use unite to make it become list
%% of list(transferRow). Then, we will transpose the puzzle, do that again 
%% to each column (transferColumn). Given this, when we transfer the puzzle,
%% we just convert the puzzle to the variables(First part), then splitList
%% puzzle, make it become slots for both row and column and append them 
%% together(transferPuzzleToList,Second Part). As we talk in disscussion 
%% board, the length of slots are all bigger than 1, and all the slots I get  
%% is not a empty list. So we can delete the slots whose length is 1. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

transferPuzzleToList(NewPuzzle, Result):-
    transferRow(NewPuzzle,Acc1),
    transferColumn(NewPuzzle,Acc2),
    append(Acc1,Acc2,Acc3),
    delete(Acc3,[_],Result).

splitList([],_,Acc,Result) :-
	(length(Acc,0) -> 
		Result = [];
		Result = [Acc]).
splitList([Head|Tail],Tag,Acc,Result) :-
	((nonvar(Head),Head = Tag) -> 
		(length(Acc,0) ->
			splitList(Tail,Tag,[],Result);
			Result = [Acc|Acc2],splitList(Tail,Tag,[],Acc2));
		append(Acc,[Head],NewList),splitList(Tail,Tag,NewList,Result)).
	
split_list(NewPuzzle,Row) :- splitList(NewPuzzle,'#',[],Row).

unite(List,Acc,Result):-
	(length(List,0) -> Result = Acc;
	(List = [Head|Tail]),append(Acc,Head,Acc1),unite(Tail,Acc1,Result)).

transferRow(NewPuzzle,Result) :-
	maplist(split_list,NewPuzzle,Acc),unite(Acc,[],Result).

transferColumn(NewPuzzle,Result) :-
	transpose(NewPuzzle,NewPuzzleV),
	transferRow(NewPuzzleV,Result).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This is the third part to solve the puzzle .
%% After we processed puzzle and get all the Slots, we need to get possible 
%% answer for each slot. possibleAnswer is used to compare to words, if them 
%% have same length, and second one has the pre-filled letter which first one
%% has, we can says second one is the possible answer for first one. Then ,we
%% compare this slot to the whole Wordlist(possibleAnswerList), if it is the
%% possible, we add it to a accumulator, and when we finish the recursion, the
%% word in accumulator is the list of possible answer we need to find.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

possibleAnswer([],[]).
possibleAnswer([HeadS|TailS],[HeadW|TailW]) :-
	samelength([HeadS|TailS],[HeadW|TailW]),
	(var(HeadS),possibleAnswer(TailS,TailW);
	 HeadS = HeadW,possibleAnswer(TailS,TailW)).

possibleAnswerList(_Slot,[],Acc,Result) :- Acc = Result.
possibleAnswerList(Slot,[HeadW|TailW],Acc,Result) :-
    (possibleAnswer(Slot,HeadW) -> 
    	append(Acc,[HeadW],Acc1),possibleAnswerList(Slot,TailW,Acc1,Result);
        possibleAnswerList(Slot,TailW,Acc,Result)).

getPossibleAnswer(Slot,Wordlist,Result) :- 
	possibleAnswerList(Slot,Wordlist,[],Result).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This is the fourth part to solve the puzzle .
%% After we can get the possible answer for each slot, we need to find one 
%% slot used to be fill in. To solve the puzzle as soon as possible, we can 
%% choose the one with fewest possible answers(nextFilled), and this slot will 
%% be filled in. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nextFilled([],Wordlist,_,Result,Slot,SlotAnswer) :- 
	Slot = Result, getPossibleAnswer(Slot,Wordlist,SlotAnswer).
nextFilled([HeadS|TailS],Wordlist,Num,Result,Slot,SlotAnswer) :- 
	getPossibleAnswer(HeadS,Wordlist,Acc), 
	(length(Acc,Len),Len < Num -> 
		nextFilled(TailS,Wordlist,Len,HeadS,Slot,SlotAnswer);
		nextFilled(TailS,Wordlist,Num,Result,Slot,SlotAnswer)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% if the length is less than previous one, we updat Result which is used to
%% record the slot with fewest possible answers. Afther we finish iteartion,
%% the Slot in result is what we are looking for.(nextFilled)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getNextFilled([HeadS|TailS],Wordlist,Slot,SlotAnswer) :-
	nextFilled([HeadS|TailS],Wordlist,9999,HeadS,Slot,SlotAnswer).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% This is the last part befor we solve the puzzle.
%% We just fill the slot we find before. we can use member to macth the slot 
%% and it possible answers. After linking a slot with a word, we will delete
%% the slot form all the slot and delete the word from WordList, in this step, 
%% we use select(Elem,List1,List2)(Is true when List1, with Elem removed, 
%% results in List2). Then we recursively fill rest slot with rest wordlist.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fill([], _RestList).
fill(Slots, Wordlist) :-
    getNextFilled(Slots,Wordlist,Slot,SlotAnswer),
    member(Slot, SlotAnswer),
    select(Slot, Slots, RestSlots),
    select(Slot, Wordlist, RestWordList),
    fill(RestSlots, RestWordList).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Finally, we slove the puzzle. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve_puzzle(Puzzle, WordList, Sloved):-
    convertPuzzle(Puzzle, Sloved),
    transferPuzzleToList(Sloved, Slots),
    fill(Slots, WordList).





