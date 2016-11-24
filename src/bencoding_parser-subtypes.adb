Package body BEncoding_Parser.Subtypes is
   Function "+"( Item : String  ) return String_Element  is
      ( String_Element'(Create(Item)) );
   Function "+"( Item : Integer ) return Integer_Element is
      ( Integer_Element'(Create(Item)) );
   Function "-"( Item : Integer_Element ) return Integer is ( Get_Value(Item) );
   Function "-"( Item : String_Element  ) return String  is ( Get_Value(Item) );

   Function  "&"( Left, Right : Dictionary_Element ) return Dictionary_Element is
      Function Make_Dictionary return Element_Map_Pkg.Map is
         Package Map renames Element_Map_Pkg;
      begin
         Return Result : Map.Map do
            declare
               Procedure Include_Items( Position : Map.Cursor ) is
               begin
                  Result.Include( Map.Key(Position), Map.Element(Position) );
               end Include_Items;
               L : Map.Map renames Get_Value( Left );
               R : Map.Map renames Get_Value( Right );
            begin
               L.Iterate ( Include_Items'Access );
               R.Iterate ( Include_Items'Access );
            end;
         end return;
      End Make_Dictionary;
   begin
      Return Result : Dictionary_Element := Create( Make_Dictionary );
   end "&";



   -- Returns the element indicated by RIGHT of the dictionary LEFT.
   Function  "/"( Left : Dictionary_Element; Right : String ) return Element is
                 (Get_Value(Left,Right));

   -- STRING & ELEMENT returns a dictionary containing the association of
   -- LEFT => RIGHT.
   Function  "&"( Left : String; Right : Element ) return Dictionary_Element is
      ( Create(Left, Right) );

   -- Append inserts the association KEY => ITEM onto the given dictionary.
   Procedure Append(Dict : in out Dictionary_Element; Key : String; Item : Element) is
   begin
      BEncoding_Parser.Append(Dict, Key, Item);
   end Append;


   -- Checks for the existance of a key.
   Function Exists( Left : Dictionary_Element; Right : String ) return Boolean is
      ( BEncoding_Parser.Exists(Left,Right) );

   Function Length( Item : Element ) return Natural
     renames BEncoding_Parser.Length;

End BEncoding_Parser.Subtypes;
