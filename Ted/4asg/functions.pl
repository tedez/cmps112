not( X ) :- X, !, fail.
not( _ ).


convert_to_degrees( degmin( Deg, Mins ), ReturnDeg ) :-
    ReturnDeg is Deg + Mins / 60.

calculate_distance( X1, Y1, X2, Y2, ReturnDist ) :-
    ReturnDist is sqrt((X2 - X1)**2 + (Y2 - Y1)**2).

distance( Airport1, Airport2, DistanceMiles ) :-
    % GET LAT AND LON FOR INPUT AIRPORTS
    airport( Airport1, _, Latitude1, Longitude1 ),
    airport( Airport2, _, Latitude2, Longitude2 ),
    % CONVERT LAT AND LONS TO SOLELY DEGREES 
    convert_to_degrees( Latitude1, Latdegrees1 ),
    convert_to_degrees( Latitude2, Latdegrees2 ),
    convert_to_degrees( Longitude1, Longdegrees1 ),
    convert_to_degrees( Longitude2, Longdegrees2 ),
    % CALCULATE DISTANCE BEWTWEEN AIRPORTS 
    calculate_distance( Latdegrees1, Longdegrees1, Latdegrees2,
     Longdegrees2, DistanceDegrees ),
    % CONVERT DEGREES TO MILES 
    LatGap is 69,
    DistanceMiles is LatGap * DistanceDegrees.

convert_to_hours( time( Hours, Mins ), Hoursonly ) :-
    Hoursonly is Hours + Mins / 60.

miles_to_hours( Miles, Hours ) :-
    Hours is Miles / 500.

print_2digits( Digits ) :-
    Digits < 10, print( 0 ), print( Digits ).

print_2digits( Digits ) :-
    Digits >= 10, print( Digits ).

print_time( Hoursonly ) :-
    Minsonly is floor( Hoursonly * 60 ),
    Hours is Minsonly // 60,
    Mins is Minsonly mod 60,
    print_2digits( Hours ),
    print( ':' ),
    print_2digits( Mins ).

/*
 * * Find paths from a departure airport to a destination airport.
 * * The path with the shortest distance is chosen.  
 * * The list returned contains lists of the airports with their 
 * * departure/arrival times.
 * */
% FACT IF THE START AND END LOCATIONS ARE THE SAME
findpath( End, End, _, [End], _ ).
% FOR DIRECT FLIGHTS
findpath( Curr, End, Visited, [[Curr, DepTime, ArrTime] | List],
          DepTimeInHM ) :-
    flight( Curr, End, DepTimeInHM ),
    not( member( End, Visited ) ),
    convert_to_hours( DepTimeInHM, DepTime ),
    distance( Curr, End, DistanceMi ),
    miles_to_hours( DistanceMi, DeltaTime ),
    ArrTime is DepTime + DeltaTime,
    ArrTime < 24.0, 

    findpath( End, End, [End | Visited], List, _).
% FOR CONNECTING FLIGHTS
findpath( Curr, End, Visited, [[Curr, DepTime, ArrTime] | List],
          DepTimeInHM ) :-
    flight( Curr, Next, DepTimeInHM ),
    not( member( Next, Visited ) ),
    convert_to_hours( DepTimeInHM, DepTime ),
    distance( Curr, Next, DistanceMi ),
    miles_to_hours( DistanceMi, DeltaTime ),
    ArrTime is DepTime + DeltaTime,
    ArrTime < 24.0, 

    flight( Next, _, NextDepTimeInHM ),
    convert_to_hours( NextDepTimeInHM, NextDepTime ),
    TimeDiff is NextDepTime - ArrTime - 0.5,
    TimeDiff >= 0, 
    findpath( Next, End, [Next | Visited], List, NextDepTimeInHM ).

/*
 * * Write the given list using a certain form given departs/arrives
 * * paired with times.
 * */
writepath( [] ) :- nl.
writepath( [[Dep, DDTime, DATime], Arr | []] ) :-
    airport( Dep, Depart_name, _, _),
    airport( Arr, Arrive_name, _, _),
    write( '     ' ), write( 'depart  ' ),
    write( Dep ), write( '  ' ),
    write( Depart_name ),
    print_time( DDTime ), nl,

    write( '     ' ), write( 'arrive  ' ),
    write( Arr ), write( '  ' ),
    write( Arrive_name ),
    print_time( DATime ), nl,
    !, true.
writepath( [[Dep, DDTime, DATime], [Arr, ADTime, AATime] | Rest] ) :-
    airport( Dep, Depart_name, _, _),
    airport( Arr, Arrive_name, _, _),
    write( '     ' ), write( 'depart  ' ),
    write( Dep ), write( '  ' ),
    write( Depart_name ),
    print_time( DDTime ), nl,

    write( '     ' ), write( 'arrive  ' ),
    write( Arr ), write( '  ' ),
    write( Arrive_name ),
    print_time( DATime ), nl,
    !, writepath( [[Arr, ADTime, AATime] | Rest] ).

fly( Depart, Depart ) :-
    write( 'Error: the departure and the destination are the same.' ),
    nl,
    !, fail.
% MAIN TEST CASE...
fly( Depart, Arrive ) :-
    airport( Depart, _, _, _ ),
    airport( Arrive, _, _, _ ),
    % TWO INPUT AIRPORTS ARE IN DATABASE.PL
    % SO WE ATTEMPT OT FIND A PATH BETWEEN THEM
    findpath( Depart, Arrive, [Depart], List, _ ),
    !, nl,
    % IF WE HIT THIS POINT, WE HAVE FOUND A PATH & NEED TO 
    % WRITE IT...FINDPATH DID NOT FAIL
    writepath( List ),
    true.
fly( Depart, Arrive ) :-
    airport( Depart, _, _, _ ),
    airport( Arrive, _, _, _ ),
    write( 'Error: your flight is not possible in the twilight zone.' ),
    !, fail.
fly( _, _) :-
    write( 'Error: nonexistent airport(s).' ), nl,
    !, fail.
