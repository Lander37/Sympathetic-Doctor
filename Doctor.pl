/*Define the Symptoms for each Diagnosis*/
fever([sweating_alot, feeling_any_aches, feeling_cold, losing_appetite, feeling_weak]).
flu([feeling_any_aches, feeling_weak, sneezing, coughing, having_sore_throat]).
rash([bleeding, feeling_itchiness, having_any_swelling, having_blisters, seeing_any_redness_on_your_skin]).
serious_injury([bleeding, having_any_swelling, having_trouble_breathing, feeling_any_confusion, feeling_any_nausea]).
minor_injury([bleeding, seeing_any_redness_on_your_skin, feeling_any_aches, having_any_swelling, feeling_any_nausea]).

/*Define all the Different Diagnosis Possible*/
allDiagnosis([a_fever, the_flu, some_rashes, a_serious_injury, a_minor_injury, no_sickness]).

/*Define Different Pain and Mood Levels*/
pain_level([none, unbearable, alot, manageable, mild]).
mood_level([calm, tired, stressed, angry, weepy]).

/*Define Gestures for Unbearable, Alot Pain OR Angry, Weepy Mood*/
calmingGesture([relaxed_tone, attentive_look, calm_demeanour, look_of_assurance, reassuring_voice]).

/*Define Gestures for Manageable Pain OR Stressed Mood*/
normalGesture([concerned_look, mellow_tone, faint_smile, monotone_voice, hint_of_concern]).

/*Define Gestures for Other Pain Levels OR Moods*/
relaxedGesture([optimistic_tone, cheerful_tone, beaming_grin, smiling_face, enthusiastic_tone]).

/*This Predicate Initializes the Global List of Available Symptoms to Ask About (in allSymptoms).
  Fever Symptoms are unified with the variable A, Flu Symptoms are unified with the variable B and so on...*/
initSymptoms(A, B, C, D, E) :-
	fever(A), flu(B), rash(C), serious_injury(D), minor_injury(E),
	flatten([A,B,C,D,E], F),
	sort(F, Var),
	nb_setval(allSymptoms, Var).

/*This Predicate Initializes the Global Counter Variables*/
initCounters :-
	nb_setval(feverCounter, 0), nb_setval(fluCounter, 0), nb_setval(rashCounter, 0),
	nb_setval(seriousCounter, 0), nb_setval(minorCounter, 0).

/*These Predicates Increment the Respective Counter Variables*/
addFever :- nb_getval(feverCounter, C), CNew is C + 1, nb_setval(feverCounter, CNew).
addFlu :- nb_getval(fluCounter, C), CNew is C + 1, nb_setval(fluCounter, CNew).
addRash :- nb_getval(rashCounter, C), CNew is C + 1, nb_setval(rashCounter, CNew).
addSerious :- nb_getval(seriousCounter, C), CNew is C + 1, nb_setval(seriousCounter, CNew).
addMinor :- nb_getval(minorCounter, C), CNew is C + 1, nb_setval(minorCounter, CNew).

/*These Predicates Unify Pain and Mood Levels with Var*/
painLevel(Index, Var) :- pain_level(L), nth0(Index, L, Var).
moodLevel(Var) :- mood_level(L), random_member(Var, L).

/*This Predicate Selects the Gestures to Use based on Patient's Pain and Mood. Unifies the List with Var*/
gestureSet(Var, Mood, Pain) :-
	((Pain = unbearable; Pain = alot; Mood = angry; Mood = weepy) -> calmingGesture(Var)
	;
	(Pain = manageable; Mood = stressed) -> normalGesture(Var)
	;
	relaxedGesture(Var)).

/*This Predicate Formats Input for Display. Unifies it with Output*/
reformat(Input, Output) :- atomic_list_concat(L, '_', Input), atomic_list_concat(L, ' ', Output).

/*This Predicate Removes C from the List of Symptoms that can be Asked About*/
removeSymptom(C) :-
	nb_getval(allSymptoms, List),
	delete(List, C, NewList),
	nb_setval(allSymptoms, NewList).

/*This Predicate Checks if Element is a symptom of all the Different Diagnosis. Then Increments the Relevant Counters*/
adjustCounters(Element, Z, Y, X, W, V) :-
	(member(Element, Z) -> append([], [1], A); append([], [0], A)),
	(member(Element, Y) -> append(A, [1], B); append(A, [0], B)),
	(member(Element, X) -> append(B, [1], C); append(B, [0], C)),
	(member(Element, W) -> append(C, [1], D); append(C, [0], D)),
	(member(Element, V) -> append(D, [1], Final); append(D, [0], Final)),

	(nth0(0, Final, 1) -> addFever; true),
	(nth0(1, Final, 1) -> addFlu; true),
	(nth0(2, Final, 1) -> addRash; true),
	(nth0(3, Final, 1) -> addSerious; true),
	(nth0(4, Final, 1) -> addMinor; true).

/*This Predicate Chooses a Random Symptom to Ask Question About*/
ask(Z, Y, X, W, V) :-
	nb_getval(allSymptoms, All),
	random_member(A, All),
	removeSymptom(A),
	reformat(A, B),
	write("Have you been "), write(B), write("?"), nl,
	write("Answer: (yes./no.)"), nl, nl,
	read(C), nl,
	(C = yes -> adjustCounters(A, Z, Y, X, W, V); true).

/*This Predicate Checks the Diagnosis for the Patient. Unifies it with Var*/
getDiagnosis(Var) :-
	allDiagnosis(List),
	nb_getval(feverCounter, FeverValue), nb_getval(fluCounter, FluValue), nb_getval(rashCounter, RashValue),
	nb_getval(seriousCounter, SeriousValue), nb_getval(minorCounter, MinorValue),

	(FeverValue >= 3 -> nth0(0, List, Var);
	FluValue >= 3 -> nth0(1, List, Var);
	RashValue >= 3 -> nth0(2, List, Var);
	SeriousValue >= 3 -> nth0(3, List, Var);
	MinorValue >= 3 -> nth0(4, List, Var);
	nth0(5, List, Var)).

/*This Predicate Begins the Consultation with Doctor*/
start :-
	initCounters,
	initSymptoms(FeverList, FluList, RashList, SeriousList, MinorList),

	/*Get Pain Level from User. Unifies it with the variable Pain*/
	write("Hi! I'm the Doctor that will see you today. Are you currently feeling any pain at all?"), nl,
	write("Answer: (yes/no)"), nl, nl,
	read(P0), nl,
	(P0 = no -> painLevel(0, Pain)
	;
	write("Is the Pain Unbearable?"), nl,
	write("Answer: (yes./no.)"), nl, nl,
	read(P1), nl,
	P1 = yes -> painLevel(1, Pain)
	;
	write("Are you in Alot of Pain?"), nl,
	write("Answer: (yes./no.)"), nl, nl,
	read(P2), nl,
	P2 = yes -> painLevel(2, Pain)
	;
	write("Is the Pain Manageable?"), nl,
	write("Answer: (yes./no.)"), nl, nl,
	read(P3), nl,
	P3 = yes -> painLevel(3, Pain)
	;
	painLevel(4, Pain)),

	/*Get Random Mood Level and unifies it with the variable Mood*/
	moodLevel(Mood),
	write("(Doctor observes that patient is feeling "), write(Mood), write(".)"), nl, nl,

	/*Get Gestures to use based on patient's pain and mood. Unifies the List with the variable Gestures*/
	gestureSet(Gestures, Mood, Pain),

	/**********Start Asking Patient Questions for Diagnosis**********/
	repeat,

	/*Choose a Gesture to Use*/
	random_member(T0, Gestures),
	reformat(T0, G0),
	write("(Doctor asks with a "), write(G0), write(".)"), nl, nl,

	/*Ask Question to Patient and Increment Relevant Counter Values based on Answer*/
	ask(FeverList, FluList, RashList, SeriousList, MinorList),

	/*Check Counter Values*/
	nb_getval(feverCounter, FeverValue), nb_getval(fluCounter, FluValue), nb_getval(rashCounter, RashValue),
	nb_getval(seriousCounter, SeriousValue), nb_getval(minorCounter, MinorValue), nb_getval(allSymptoms, AllSymp),

	/*Check if Diagnosis can be Reached. If True, End Question Asking. If False, Backtrack.*/
	((FeverValue >= 3; FluValue >= 3; RashValue >= 3; SeriousValue >= 3; MinorValue >= 3; AllSymp = []), !; fail),

	/*Get the Diagnosis for the Patient and Inform the Patient*/
	getDiagnosis(S0),
	reformat(S0, S1),
	write("Thanks for answering those questions. After Examination, I believe you have "), write(S1), write("."), nl,
	(S0 \= 'no_sickness' -> write("Please proceed to wait outside the Dispensary for your medication."); true).



