%%---10--|----20---|----30---|----40---|----50---|
-doc "Adds two number together".
-doc #{since => "1.0", author => "Joe"}.
-doc #{since => "2.0"}.
add(One, Two) -> One + Two.


-doc #{equiv => add(One, Two, [])}.
-spec add(One :: number(), Two :: number()) -> number().
add(One, Two) -> add(One, Two, []).
