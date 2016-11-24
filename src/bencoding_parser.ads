private with
Ada.Containers.Indefinite_Ordered_Maps;

with
Ada.Streams;

Package BEncoding_Parser with Preelaborate, Remote_Types, Elaborate_Body is

   Type Bencode_Type is
     ( BT_String, BT_Integer, BT_List, BT_Dictionary );



   Type Element(<>) is private; --    with Read => Read, Write => Write;

   procedure Read(Stream : not null access Ada.Streams.Root_Stream_Type'Class;
                  Item   : out  Element);

   procedure Write(Stream : not null access Ada.Streams.Root_Stream_Type'Class;
                   Item   : in  Element);

   For Element'Read  Use Read;
   For Element'Write Use Write;

   Function Get_Type	( Item : Element ) return Bencode_Type;
   Function Image	( Item : Element ) return String;
   Function Value	( Item : String  ) return Element;

   Parsing_Error :   Exception;

private

   Type Element_Data(Element_Type : Bencode_Type; Length : Natural);


   Type Element is not null access all Element_Data;



--     procedure Read(
--                    Stream : not null access Ada.Streams.Root_Stream_Type'Class;
--                    Item   : out  Element);
--
--     procedure Write(
--                     Stream : not null access Ada.Streams.Root_Stream_Type'Class;
--                     Item   : in  Element);

   Package Element_Map_Pkg is new Ada.Containers.Indefinite_Ordered_Maps(
         Key_Type     => String,
         Element_Type => Element,
         "<"          => "<",
         "="          => "="
       );

   -- The list-type can be implemented as a simple array.
   Type Element_List    is Array(Positive range <>) of Element;


   function  Get_Value( Item : Element ) return String;
   function  Get_Value( Item : Element ) return Integer;
   function  Get_Value( Item : Element ) return Element_List;
   function  Get_Value( Item : Element ) return Element_Map_Pkg.Map;
   function  Get_Value( Item : Element; Entre : String ) return Element;


   function  Create( Item : String  ) return Element;
   function  Create( Item : Integer ) return Element;
   function  Create( Item : Element_List ) return Element;
   function  Create( Item : Element_Map_Pkg.Map ) return Element;
   function  Create( Key : String; Entre : Element ) return Element;


   Procedure Append( Item : in out Element; Key : String; Entre : Element );
   Function  Exists( Item : Element; Key : String ) return Boolean;
   Function  Length( Item : Element ) return Natural;
end BEncoding_Parser;
