Pragma Ada_2012;

With Ada.Streams;
Package BEncoding is --with Remote_Types, Elaborate_Body is

   -- The "Integer" in Bencodings should be the largest possible.
   subtype Integer is Standard.Long_Long_Integer;

   Type Bencode_Type is
     ( BT_String, BT_Integer, BT_List, BT_Dictionary );

   Type Element(<>) is private;


--     For Element'Input  Use Parse;
--     For Element'Output Use Put;



   Function Get_Type ( E : Element ) return Bencode_Type;

   subtype String_Element is Element
   with Dynamic_Predicate => Get_Type(String_Element) = BT_String;

   subtype Integer_Element is Element
   with Dynamic_Predicate => Get_Type(Integer_Element) = BT_String;

   subtype List_Element is Element
   with Dynamic_Predicate => Get_Type(List_Element) = BT_List;

   subtype Dictionary_Element is Element
   with Dynamic_Predicate => Get_Type(Dictionary_Element) = BT_Dictionary;




   ----------------------------
   --  Dictionary Functions  --
   ----------------------------

   --   Function  "&"( Left, Right : Dictionary_Element ) return Dictionary_Element;


   Function  "/"( Left : Dictionary_Element; Right : String ) return Element;

   -- STRING & ELEMENT returns a dictionary containing the association of
   -- LEFT => RIGHT.
   Function  "&"( Left : String; Right : Element ) return Dictionary_Element;

   -- Append inserts the association KEY => ITEM onto the given dictionary.
   Procedure Append(Dict : Dictionary_Element; Key : String; Item : Element);

   -- Checks for the existance of a key.
   Function Exists( Left : Dictionary_Element; Right : String ) return Boolean;

   ----------------------
   --  List Functions  --
   ----------------------

   -- Returns the list given on the left-side with the right appended.
   Function "+"( Left: List_Element; Right : Element ) return List_Element;

   -- Returns a single-element list containing the given element.
   Function "+"( Right : Element ) return List_Element;

   ----------------------------
   --  Conversion Functions  --
   ----------------------------

   -- Create a string-element from the given string.
   Function "+"( Item : String  ) return String_Element;

   -- Create an integer-element from the given integer.
   Function "+"( Item : Integer ) return Integer_Element;

   -- Return the value of the given element.
   Function "-"( Item : Integer_Element ) return Integer;
   Function "-"( Item : String_Element  ) return String;

   Function Image( Item : Element ) return String;
   Function Value( Item : String  ) return Element;

   ------------------
   --  Exceptions  --
   ------------------

   Parsing_Error : Exception;

Private

   -- Element_Data defines the actual record-structure of the Element.
   Type Element_Data( Element_Type : Bencode_Type; Length : Natural );

   -- Element is a pointer to the deffered-type Element_Data; the reason for
   -- this circuitous method is to allow the instantiation of a map between
   -- String and Element to be instantiated, thus simplifying the implementation
   -- of the dictionary element-type.
   Type Element is not null access all Element_Data
     with Read => Read, Write => Write;
  -- with Storage_Size => 0;

--        procedure Put (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
--                       Item   : Element);
--        Function Parse (Stream : not null access Ada.Streams.Root_Stream_Type'Class) return  Element;
--
--     For Element'Input  Use Parse;
--     For Element'Output Use Put;


   -- The list-type can be implemented as a simple array.
   Type Element_List    is Array(Positive range <>) of Element;


  procedure Read(
        Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        Item   : out  Element);

  procedure Write(
        Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        Item   : in  Element);

End BEncoding;
