with
AWS.Response,
Ada.Containers.Indefinite_Ordered_Maps,
Ada.Strings.Less_Case_Insensitive,
Interfaces;

Package BitTorrent is

Private
--     use type AWS.Response.Callback;
   Package Callback_Map is new Ada.Containers.Indefinite_Ordered_Maps(
       "<"          => Ada.Strings.Less_Case_Insensitive,
       "="          => AWS.Response."=",
       Key_Type     => String,
       Element_Type => AWS.Response.Callback
     );

End BitTorrent;
