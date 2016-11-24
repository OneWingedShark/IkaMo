Package BEncoding_Parser.Subtypes with Elaborate_Body is
   subtype String_Element is Element
   with Dynamic_Predicate => Get_Type(String_Element) = BT_String;

   subtype Integer_Element is Element
   with Dynamic_Predicate => Get_Type(Integer_Element) = BT_String;

   subtype List_Element is Element
   with Dynamic_Predicate => Get_Type(List_Element) = BT_List;

   subtype Dictionary_Element is Element
   with Dynamic_Predicate => Get_Type(Dictionary_Element) = BT_Dictionary;

   ----------------------------
   --  CONVERSION FUNCTIONS  --
   ----------------------------

   -- Create an atom of the specified type.
   Function "+"( Item : String  ) return String_Element;
   Function "+"( Item : Integer ) return Integer_Element;

   -- Return the value of the given element.
   Function "-"( Item : Integer_Element ) return Integer;
   Function "-"( Item : String_Element  ) return String;

   ----------------------------
   --  Dictionary Functions  --
   ----------------------------

   --   Function  "&"( Left, Right : Dictionary_Element ) return Dictionary_Element;

   -- Returns the element indicated by RIGHT of the dictionary LEFT.
   Function  "/"( Left : Dictionary_Element; Right : String ) return Element;

   -- STRING & ELEMENT returns a dictionary containing the association of
   -- LEFT => RIGHT.
   Function  "&"( Left : String; Right : Element ) return Dictionary_Element;

   -- Append inserts the association KEY => ITEM onto the given dictionary.
   Procedure Append (Dict : in out Dictionary_Element; Key : String; Item : Element);

   -- Checks for the existance of a key.
   Function Exists( Left : Dictionary_Element; Right : String ) return Boolean;

   -- Returns the length of the given element; 1 in the case of Integer.
   Function Length( Item : Element ) return Natural;

private
--     Function "+"( Item : String  ) return String_Element  renames Create;
--     Function "+"( Item : Integer ) return Integer_Element renames create;
--     Function "-"( Item : Integer_Element ) return Integer is ( Get_Value(Item) );
--     Function "-"( Item : String_Element  ) return String  is ( Get_Value(Item) );

End BEncoding_Parser.Subtypes;
