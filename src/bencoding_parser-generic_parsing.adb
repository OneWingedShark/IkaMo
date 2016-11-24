
Separate(BEncoding_Parser)
Package body Generic_Parsing is

   subtype Type_Character is Character
   with Static_Predicate => Type_Character in '0'..'9'|'i'|'d'|'l';

   --------------------------------
   --  GENERAL HELPER FUNCTIONS  --
   --------------------------------

   -- Check_Type is here to have a common point for generating
   -- the exception and error message when the tag is invalid.
   Function Check_Type( Ch : Character ) return Type_Character is
   begin
      return Result : constant Type_Character:= Ch;
   exception
      when Constraint_Error => -- | Assert_Failure =>
         raise Parsing_Error with "Invalid type-tag.";
   end Check_Type;

   Procedure Consume( Working : aliased in out T ) with Inline;
   Function  Current( Working : aliased in out T ) return Character with Inline;
   Function  Is_End ( Working : aliased in out T; Terminal : character:= 'e' ) return Boolean is
     ( Current(Working) = Terminal );

   Function  Current( Working : aliased in out T ) return Character is
     ( Current(Working'Access) );

   Function Get_Type( Working : aliased in out T ) return Type_Character is
      Ch : Character renames Current(Working'access);
   begin
      return Result : constant Type_Character:= Check_Type(Ch) do
         if Result not in '0'..'9' then
            Consume(Working);
         end if;
      end return;
   end Get_Type;

   Procedure Consume(Working : aliased in out T)  is
   begin
      Clear(Working'access);
   end Consume;

   Function Consume(Working : aliased in out T) return character is
      Result : Constant Character:= Current(Working'Access);
   begin
      Consume(Working);
      Return Result;
   end Consume;


   Function Consume(Working : aliased in out T; Terminal : Character) return String is
   begin
      if Is_End(Working, Terminal) then
         Consume(working);
         return "";
      else
         declare
            C : Character renames Consume(Working);
         begin
            Clear(Working'access);
            return Result : constant String :=  C & Consume(Working, Terminal);
         end;
      end if;
   end;

   Function As_Array(List : Element_Vector_Pkg.Vector) return Element_List is
      Use Element_Vector_Pkg;
      Position : Cursor:= List.First;
      Function To_Array(Items : Vector) return Element_List is
         Function Item return Element is
         begin
            return Result : Element:= Element_Vector_Pkg.Element(Position) do
               Next( Position );
            end return;
         end Item;

      begin
         return Result : Element_List:= (1..Natural(List.Length) => Item );
      end To_Array;
   begin
      Return Result : Element_List:= To_Array(List);
   end As_Array;

   Function Parse_List(Working : aliased in out T) return Element_Vector_Pkg.Vector;


   -----------------------
   --  ELEMENT PARSING  --
   -----------------------

   Function Parse_Integer(Working : aliased in out T) return Integer_Element is
   begin
      Return Result : Integer_Element := new Element_Data'(
                                                           Element_Type  => BT_Integer,
                                                           Length        => 0,
                                                           Integer_Value => 0
                                                          )
      do
         declare
            Text   : String renames Consume( Working, 'e' );
         begin
            Result.Integer_Value:= Integer'Value( Text );
         end;
      end return;
   end Parse_Integer;

   Function Parse_String(Working : aliased in out T) return String_Element is
      Length : constant string:= Consume(Working, ':');
   begin
      Return Result : String_Element := new Element_Data'(
                                                          Element_Type => BT_String,
                                                          Length       => Natural'Value(Length),
                                                          String_Value => (others => ASCII.NUL)
                                                         )
      do
         for Item of Result.String_Value loop
            Item:= Consume(Working);
         end loop;
      end return;
   end Parse_String;

   Function Parse_List(Working : aliased in out T) return List_Element is
      Use Element_Vector_Pkg;
      List : constant Vector:= Parse_List( Working );

   begin
      Return Result : List_Element := new Element_Data'(
                                                        Element_Type  => BT_List,
                                                        Length        => Natural(List.Length),
                                                        List_Value    => As_Array(List)
                                                       );
   end Parse_List;

   Function Parse_List(Working : aliased in out T) return Element_Vector_Pkg.Vector is
      Use Element_Vector_Pkg;
   begin
      Return Result : Vector do
         loop
            declare
               Item : constant Element:= Parse(Working);
            begin
               Result.Append( Item );
            end;
            exit when Is_End(Working);
         end loop;
         Consume(Working);
      end return;
   end Parse_List;

   Function Parse_Dictionary(Working : aliased in out T) return Dictionary_Element is
   begin
      Return Result : Dictionary_Element := new Element_Data'(
                                                              Element_Type  => BT_Dictionary,
                                                              Length        => 0,
                                                              Dictionary    => <>
                                                             )
      do
         loop
            -- Exit at the front, because dictionaries can be empty.
            exit when Is_End(Working);
            declare
               Key  : constant String_Element:= Parse_String(Working);
               Item : constant Element:= Parse(Working);
            begin
               Result.Dictionary.Include(-Key, Item);
            end;
         end loop;
         Consume(Working);
      end return;
   end Parse_Dictionary;

   ---------------------------
   --  MAIN PARSE FUNCTION  --
   ---------------------------

   Function Parse( Working : aliased in out T ) return Element is
      Ch : constant Type_Character := Get_Type(Working);
   begin
      case Ch is
         when '0'..'9'	=> return Parse_String(Working);
         when 'i'	=> return Parse_Integer(Working);
         when 'd'	=> return Parse_Dictionary(Working);
         when 'l'	=> return Parse_List(Working);
      end case;
   end Parse;

end Generic_Parsing;
