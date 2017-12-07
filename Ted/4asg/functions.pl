not( X ) :- X, !, fail.
not( _ ).

constants( List ) :-
   Pi is pi,
   E is e,
   Epsilon is epsilon,
   List = [Pi, E, Epsilon].

mathfns( X, List ) :-
   S is sin( X ),
   C is cos( X ),
   Q is sqrt( X ),
   List = [S, C, Q].

haversine_radians( LatA, LonA, LatB, LonB, Distance ) :-
   Dlon is LonB - LonA,
   Dlat is LatB - LatA,
   A is sin( Dlat / 2 ) ** 2
      + cos( LatA ) * cos( LatB ) * sin( Dlon / 2 ) ** 2,
   Dist is 2 * atan2( sqrt( A ), sqrt( 1 - A )),
   % returns distance between airports in meters... 
   Distance is Dist * 3961.

convert_to_rads( degmin( Deg, Mins ), ReturnDeg ) :-
    ReturnDeg is ( ( Deg + Mins / 60 ) * ( pi / 180 ) ).

get_distance_a_to_b( AirpA, AirpB, Distance ) :-
    % GET LAT AND LON FOR INPUT AIRPORTS
    airport( AirpA, _, LatA, LonA ),
    airport( AirpB, _, LatB, LonB ),
    % CONVERT LAT AND LONS TO SOLELY DEGREES 
    convert_to_rads( LatA, LatRadsA ),
    convert_to_rads( LatB, LatRadsB ),
    convert_to_rads( LonA, LonRadsA ),
    convert_to_rads( LonB, LonRadsB ),
    % CALCULATE DISTANCE BEWTWEEN AIRPORTS 
    haversine_radians( LatRadsA, LonRadsA, LatRadsB,
     LonRadsB, Distance ).

convert_to_hours( time( Hours, Mins ), ReturnConvertedHours ) :-
    ReturnConvertedHours is Hours + Mins / 60.

format_time( Digits ) :-
    Digits < 10, print( 0 ), print( Digits ).

format_time( Digits ) :-
    Digits >= 10, print( Digits ).

print_time( Hoursonly ) :-
    Minsonly is floor( Hoursonly * 60 ),
    Hours is Minsonly // 60,
    Mins is Minsonly mod 60,
    format_time( Hours ),
    print( ':' ),
    format_time( Mins ).

% Convert miles to hours by dividing by the 
% planes' constant rate of travel -- 500 mph
mph_to_hours( Miles, ReturnConvertedHours ) :- 
    ReturnConvertedHours is Miles / 500.


attempt_path_route( DestAP, DestAP, _, [DestAP], _ ).
% CASE WHERE WE CAN FIND A DIRECT PATH FROM CURRENT 
% AIRPORT TO DESTINATION AIRPORT 
attempt_path_route( Location, DestAP, Visited, [[Location, StartTime, EndTime] | List], StartTimeUnmolested ) :-
    flight( Location, DestAP, StartTimeUnmolested ),
    not( member( DestAP, Visited ) ),
    convert_to_hours( StartTimeUnmolested, StartTime ),
    get_distance_a_to_b( Location, DestAP, Distance ),
    mph_to_hours( Distance, Delta ),
    EndTime is StartTime + Delta,
    EndTime < 24.0, 
    attempt_path_route( DestAP, DestAP, [DestAP | Visited], List, _).
% FOR CONNECTING FLIGHTS
attempt_path_route( Location, DestAP, Visited, [[Location, StartTime, EndTime] | List], StartTimeUnmolested ) :-
    flight( Location, Connection, StartTimeUnmolested ),
    not( member( Connection, Visited ) ),
    convert_to_hours( StartTimeUnmolested, StartTime ),
    get_distance_a_to_b( Location, Connection, Distance ),
    mph_to_hours( Distance, Delta ),
    EndTime is StartTime + Delta,
    EndTime < 24.0, 

    flight( Connection, _, ConnectingTime ),
    convert_to_hours( ConnectingTime, NextDepTime ),
    TimeDiff is NextDepTime - EndTime - 0.5,
    TimeDiff >= 0, 
    attempt_path_route( Connection, DestAP, [Connection | Visited], List, ConnectingTime ).

path_output( [] ) :- nl.
path_output( [[DepartAP, StartTime, ArrivalTime], DestAP | []] ) :-
    airport( DepartAP, Depart, _, _),
    airport( DestAP, Dest, _, _),
    format('\tdepart\t%s\t%s', [DepartAP, Depart]),
    print_time( StartTime ), nl,
    format('\tarrive\t%s\t%s', [DestAP, Dest]),
    print_time( ArrivalTime ), nl,
    !, true.
path_output( [[DepartAP, StartTime, ArrivalTime], [DestAP, DepTime, ArrTime] | Cdr] ) :-
    airport( DepartAP, Depart, _, _),
    airport( DestAP, Dest, _, _),
    format('\tdepart\t%s\t%s', [DepartAP, Depart]),
    print_time( StartTime ), nl,
    format('\tarrive\t%s\t%s', [DestAP, Dest]),
    print_time( ArrivalTime ), nl,
    !, path_output( [[DestAP, DepTime, ArrTime] | Cdr] ).

fly( Depart, Depart ) :-
    write( 'Error: Departing Airport == Destination Airport.' ),
    nl,
    !, fail.
% MAIN TEST CASE...
fly( Depart, Arrive ) :-
    % CHECK THE INPUT AIRPORTS ARE IN OUR DATABASE
    airport( Depart, _, _, _ ),
    airport( Arrive, _, _, _ ),
    % TWO INPUT AIRPORTS ARE IN DATABASE.PL
    % SO WE ATTEMPT OT FIND A PATH BETWEEN THEM
    attempt_path_route( Depart, Arrive, [Depart], List, _ ),
    !, nl,
    % IF WE HIT THIS POINT, WE HAVE FOUND A PATH & NEED TO 
    % WRITE IT...FINDPATH DID NOT FAIL
    path_output( List ),
    true.
fly( Depart, Arrive ) :-
    airport( Depart, _, _, _ ),
    airport( Arrive, _, _, _ ),
    write( 'Error: Flight not possible.' ),
    !, fail.
fly( _, _) :-
    write( 'Error: At least one non-existent airport.' ), nl,
    !, fail.