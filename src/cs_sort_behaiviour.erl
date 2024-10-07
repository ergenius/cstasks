%% I created this module to demonstrate how to create a custom behaiviour.
%% To be sincere I love defining new behaiviours despite I'm not much in love with OOP :)
-module(cs_sort_behaiviour).
-author("Madalin Grigore-Enescu").

-callback sort(Job :: tuple()) -> 
    {ok, Job :: tuple()} | {error, ErrorDetails :: atom()}.