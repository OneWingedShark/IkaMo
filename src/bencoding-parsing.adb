with
Ada.Strings.Maps;


separate(BEncoding)

Package body Parsing is
   Package Maps renames Ada.Strings.Maps;

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
      when Constraint_Error | Assert_Failure =>
         raise Parsing_Error
           with "Invalid type-tag.";
   end Check_Type;



   Procedure Consume(Working : in out Unbounded_String) with Inline;
   Function  Is_End (Working : in out Unbounded_String) return Boolean is
     ( Ada.Strings.Unbounded.Element(Working, 1) = 'e' );

   Function Get_Type(Working : in out Unbounded_String) return Type_Character is
      Ch : Character renames Ada.Strings.Unbounded.Element(Working, 1);
   begin
      return Result : constant Type_Character:= Check_Type(Ch) do
         if Result not in '0'..'9' then
            Consume(Working);
         end if;
      end return;
   end Get_Type;

   Function Parse_String(Working : in out Unbounded_String) return String_Element is
      Function Get_Length return Natural is
         Delimeter : constant Natural := Index(Working, ":") - 1;
      begin
         Return Result : Natural := Natural'Value( Slice(Working,1,Delimeter) ) do
            Delete (Working, 1, Delimeter);
            Consume(Working);
         end return;
      end Get_Length;

      Length : constant Natural := Get_Length;
   begin
      Return Result : String_Element := new Element_Data'(
                                                          Element_Type => BT_String,
                                                          Length       => Length,
                                                          String_Value => Slice(Working, 1, Length)
                                                         )
      do
         Delete(Working, 1, Length);
      end return;
   end Parse_String;

   Function Parse_Integer(Working : in out Unbounded_String) return Integer_Element is
      Use Ada.Strings.Maps;
   begin
      Return Result : Integer_Element := new Element_Data'(
                                                           Element_Type  => BT_Integer,
                                                           Length        => 0,
                                                           Integer_Value => 0
                                                          )
      do
         declare
            Ending : constant Positive:= Index(Working, To_Set('e'))-1;
            Text   : constant String:=   Slice(Working,1,Ending);
         begin
            Delete(Working, 1, Ending);
            Consume(Working);
            Result.Integer_Value:= Integer'Value( Text );
         end;
      end return;
   end Parse_Integer;

   Function Parse_Dictionary(Working : in out Unbounded_String) return Dictionary_Element is
   begin
      Return Result : Integer_Element := new Element_Data'(
                                                           Element_Type  => BT_Dictionary,
                                                           Length        => 0,
                                                           Dictionary    => <>
                                                          )
      do
         loop
            declare
               Key  : constant String_Element:= Parse_String(Working);
               Item : constant Element:= Parse(Working);
            begin
               Result.Dictionary.Include(-Key, Item);
            end;
            exit when Is_End(Working);
         end loop;
         Consume(Working);
      end return;
   end Parse_Dictionary;


   Function Parse_List(Working : in out Unbounded_String) return Element_Vector_Pkg.Vector is
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


   Function Parse_List( Working : in out Unbounded_String ) return List_Element is
      Use Element_Vector_Pkg;
      List : constant Vector:= Parse_List( Working );

   begin
      Return Result : List_Element := new Element_Data'(
                                                        Element_Type  => BT_List,
                                                        Length        => Natural(List.Length),
                                                        List_Value    => As_Array(List)
                                                       );
   end Parse_List;

   Procedure Consume(Working : in out Unbounded_String) is
   begin
      Ada.Strings.Unbounded.Delete(Working,1,1);
   end Consume;


   Function Parse(Working : in out Unbounded_String) return Element is
   begin
      case Get_Type(Working) is
         when '0'..'9'	=> return Parse_String(Working);
         when 'i'	=> return Parse_Integer(Working);
         when 'd'	=> return Parse_Dictionary(Working);
         when 'l'	=> return Parse_List(Working);
      end case;
   end Parse;

   ----------------------
   --  STREAM PARSING  --
   ----------------------

   Function Parse( Working : not null access Root_Stream_Type'Class;
                   Prolog  : access character
                 ) return Element;

   Function  Is_End (Working  : not null access Root_Stream_Type'Class;
                     Ch       : out Character) return Boolean is
   begin
      Ch := Character'Input( Working );
      Return Ch in 'e';
   end;

   Function Consume( Working  : not null access Root_Stream_Type'Class;
                     Terminal : Ada.Strings.Maps.Character_Set;
                     Mapping  : Ada.Strings.Maps.Character_Mapping:= Maps.Identity;
                     Prolog   : access constant character:= null
                   ) return Unbounded_String is
   begin
      Return Result : Unbounded_String do
         if Prolog /= null then
            Append( Result, Prolog.All );
         end if;

         loop
            declare
               Item : constant Character := Character'Input(Working);
            begin
               exit when Maps.Is_In( Item, Terminal );
               Append( Result, Item );
            end;
         end loop;
      end return;
   end Consume;

   Function Consume( Working  : not null access Root_Stream_Type'Class;
                     Terminal : Character;
                     Prolog   : access constant character:= null
                   ) return Unbounded_String is
     ( Consume(Working  => Working,
               Terminal => Maps.To_Set( Terminal ),
               Prolog   => Prolog
              )
     );


   Function Get_Type(Working : not null access Root_Stream_Type'Class; Ch : out Character) return Type_Character is
   begin
      Ch:= Character'Input(Working);
      return Result : constant Type_Character:= Check_Type(Ch);
   end Get_Type;

   Function Parse_String( Working : not null access Root_Stream_Type'Class; Ch : aliased Character ) return String_Element is
      Temp   : constant Unbounded_String:= Consume( Working, ':', Ch'Access );
      Length : constant Natural:= Natural'Value(To_String(Temp));
   begin
      Return Result : String_Element := new Element_Data'(
                                                          Element_Type => BT_String,
                                                          Length       => Length,
                                                          String_Value => (others => ASCII.NUL)
                                                         )
      do
         for Item of Result.String_Value loop
             Item:= Character'Input( Working );
         end loop;
      end return;
   end Parse_String;

   Function Parse_String( Working : not null access Root_Stream_Type'Class) return String_Element is
      ch : aliased character:= '0';
   begin
      return Parse_String(Working, ch);
   end Parse_String;


   Function Parse_Integer(Working : not null access Root_Stream_Type'Class) return Integer_Element is
      Temp : constant Unbounded_String:= Consume(Working, 'e');
   begin
      Return Result : Integer_Element := new Element_Data'(
                                                           Element_Type  => BT_Integer,
                                                           Length        => 0,
                                                           Integer_Value => 0
                                                          )
      do
         declare
            Text   : String renames To_String( Temp );
         begin
            Result.Integer_Value:= Integer'Value( Text );
         end;
      end return;
   end Parse_Integer;

   Function Parse_List(Working : not null access Root_Stream_Type'Class) return Element_Vector_Pkg.Vector is
      Use Element_Vector_Pkg;
      Ch : Character:= ASCII.NUL;
   begin
      Return Result : Vector do
         loop
            declare
               Function Prolog return access character is
                  (if Ch = ASCII.NUL then Null else New Character'(Ch));
               Item : constant Element:= Parse( Working, Prolog );
            begin
               Result.Append( Item );
            end;
            exit when Is_End(Working, Ch);
         end loop;
      end return;
   end Parse_List;

   Function Parse_List(Working : not null access Root_Stream_Type'Class) return List_Element is
      Use Element_Vector_Pkg;
      List : constant Vector:= Parse_List( Working );

   begin
      Return Result : List_Element := new Element_Data'(
                                                        Element_Type  => BT_List,
                                                        Length        => Natural(List.Length),
                                                        List_Value    => As_Array(List)
                                                       );
   end Parse_List;

   Function Parse_Dictionary(Working : not null access Root_Stream_Type'Class) return Dictionary_Element is
      Ch : access Character:= null;
   begin
      Return Result : Integer_Element := new Element_Data'(
                                                           Element_Type  => BT_Dictionary,
                                                           Length        => 0,
                                                           Dictionary    => <>
                                                          )
      do
         loop
            Ch:= (if ch /= null then ch
                  else New Character'( Character'Input(Working) ));
            declare
               Key  : constant String_Element:= Parse_String(Working, Ch.all);
               Item : constant Element:= Parse(Working);
            begin
               Result.Dictionary.Include(-Key, Item);
            end;
            exit when Is_End(Working, Ch.all);
         end loop;
      end return;
   end Parse_Dictionary;


   Function Parse( Working : not null access Root_Stream_Type'Class; Prolog : access character ) return Element is
      Ch  : aliased Character;
      Tag : Type_Character;
   begin
      if Prolog = Null then
         Tag:= Get_Type(Working, Ch);
      else
         Tag:= Prolog.All;
         Ch := Prolog.All;
      end if;

      case Tag is
         when '0'..'9'	=> return Parse_String(Working, Ch);
         when 'i'	=> return Parse_Integer(Working);
         when 'd'	=> return Parse_Dictionary(Working);
         when 'l'	=> return Parse_List(Working);
      end case;
   end Parse;

   Function Parse( Working : not null access Root_Stream_Type'Class ) return Element is
     ( Parse(Working, Prolog => Null));

End Parsing;
