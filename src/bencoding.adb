With
Ada.Streams,
Ada.Strings.Maps,
Ada.Strings.Unbounded,
Ada.Containers.Indefinite_Ordered_Maps,
Ada.Containers.Indefinite_Vectors,
Ada.Characters.Handling,
System.Assertions;

use
System.Assertions;
--
--  with
--  Ada.Text_IO;

Package Body BEncoding is

   Package Element_Map_Pkg is new Ada.Containers.Indefinite_Ordered_Maps(
         Key_Type     => String,
         Element_Type => Element,
         "<"          => "<",
         "="          => "="
       );
--
--
--

   Type Element_Data(Element_Type : Bencode_Type; Length : Natural) is record
      case Element_Type is
         when BT_String     => String_Value  : String(1..Length);
         when BT_Integer    => Integer_Value : Integer;
         when BT_List       => List_Value    : Element_List(1..Length);
         when BT_Dictionary => Dictionary    : Element_Map_Pkg.Map;
      end case;
   end record;


   -- Put and Parse headers must be put here; possibly a bug. (Language? GNAT?)
   procedure Put (Stream  : not null access Ada.Streams.Root_Stream_Type'Class;
                  Item    : Element_Data);
   Function Parse (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
                   return  Element_Data;

   For Element_Data'Input  use Parse;
   For Element_Data'Output use Put;

   Function Exists(Left : Dictionary_Element; Right : String) return Boolean is
      use Element_Map_Pkg;
   begin
      Return Left.Dictionary.Find(Right) /= Element_Map_Pkg.No_Element;
   end Exists;

   Function  "/"( Left : Dictionary_Element; Right : String ) return Element is
      ( Left.Dictionary(Right) );

   Function  "&"( Left : String; Right : Element ) return Dictionary_Element is
   begin
      Return Result : Dictionary_Element:= new Element_Data'(
               Element_Type => BT_Dictionary,
               Length => 0,
               others => <>
      ) do
         Result.Dictionary.Insert( Key => Left, New_Item => Right );
      end return;
   end "&";

   Procedure Append(Dict : Dictionary_Element; Key : String; Item : Element) is
   begin
      Dict.Dictionary.Include(Key => Key, New_Item => Item);
   end Append;



   Function "+"( Left: List_Element; Right : Element ) return List_Element is
   begin
      Return Result : List_Element:= New Element_Data'(
               Element_Type => BT_List,
               Length       => Left.List_Value'Length + 1,
               List_Value   => Left.List_Value & Right
      );
   end "+";


   Function "+"( Right : Element ) return List_Element is
   begin
      Return Result : List_Element:= New Element_Data'(
               Element_Type => BT_List,
               Length       => 1,
               List_Value   => (1 => Right)
      );
   end "+";


   Function "+"( Item : String  ) return String_Element is
     ( new Element_Data'(
                         Element_Type => BT_String,
                         Length       => Item'Length,
                         String_Value => Item
                        )
     );

   Function "+"( Item : Integer ) return Integer_Element is
     ( new Element_Data'(
                         Element_Type  => BT_Integer,
                         Length        => 0,
                         Integer_Value => Item
                        )
     );

   Function "-"( Item : Integer_Element ) return Integer is
     ( Item.Integer_Value );

   Function "-"( Item : String_Element  ) return String is
     ( Item.String_Value );

   Function Image( Item : Element ) return String is
      use Ada.Characters.Handling;

      Function Image( I : Integer ) return String is
         Text : String renames Integer'Image( I );
      begin
         -- Remove the leading blank in the case of a non-negative number.
         if I >= 0 then
            Return Text(Positive'Succ(Text'First)..Text'Last);
         else
            Return Text;
         end if;
      end Image;

      Function Tag( C : Character; Data : String ) return String is
        ( C & Data & 'e' );

      Function Encode( I : Integer ) return String is
        ( Tag('i', Image(I)) );

      Function Encode( S : String ) return String is
        ( Image(S'Length) & ':' & S );


      Function Encode_List( L : Element_List ) return String is
        (case L'Length is
            when 0 => "",
            when 1 => Image(L(L'First)),
            when others => Image(L(L'First)) & Encode_List(L(1+L'First..L'Last))
        );

      Function Encode_Dict( D : Element_Map_Pkg.Map ) return String is
         use Ada.Strings.Unbounded;
         Package Maps renames Element_Map_Pkg;
         Working : Ada.Strings.Unbounded.Unbounded_String;
         procedure Process(Position : Maps.Cursor) is
         begin
            Working := Working &
                       Encode(Maps.Key(Position)) &
                       Image (Maps.Element(Position));
         end Process;
      begin
         D.Iterate( Process'Access );
         return To_String( Working );
      end Encode_Dict;

      Function Encode( L : Element_List ) return String is
         ( Tag('l', Encode_List(L)) );

      Function Encode( D : Element_Map_Pkg.Map ) return String is
         ( Tag('d', Encode_Dict(D)) );
   begin
      case Item.Element_Type is
         when BT_String     => return Encode( Item.String_Value  );
         when BT_Integer    => return Encode( Item.Integer_Value );
         when BT_List       => return Encode( Item.List_Value    );
         when BT_Dictionary => return Encode( Item.Dictionary    );
      end case;
   end Image;


   Package Parsing is
      Use Ada.Strings.Unbounded, Ada.Streams;
      Function Parse( Working : in out Unbounded_String ) return Element;
      Function Parse( Working : not null access Root_Stream_Type'Class ) return Element;
   Private

      Package Element_Vector_Pkg is new Ada.Containers.Indefinite_Vectors(
                 Index_Type   => Positive,
                 Element_Type => Element
               );

      Function Parse_Integer	( Working : in out Unbounded_String )	return Integer_Element;
      Function Parse_String	( Working : in out Unbounded_String )	return String_Element;
      Function Parse_List	( Working : in out Unbounded_String )	return List_Element;
      Function Parse_Dictionary	( Working : in out Unbounded_String )	return Dictionary_Element;

      Function Parse_Integer	( Working : not null access Root_Stream_Type'Class )	return Integer_Element;
      Function Parse_String	( Working : not null access Root_Stream_Type'Class )	return String_Element;
      Function Parse_List	( Working : not null access Root_Stream_Type'Class )	return List_Element;
      Function Parse_Dictionary	( Working : not null access Root_Stream_Type'Class )	return Dictionary_Element;
   End Parsing;

   Package body Parsing is separate;




   Function Value( Item : String ) return Element is
      Use Ada.Strings.Unbounded, Ada.Strings.Maps, Ada.Strings;
--
--        subtype Type_Character is Character
--        with Static_Predicate => Type_Character in '0'..'9'|'i'|'d'|'l';
--
--
      Working : Unbounded_String := Trim( To_Unbounded_String(Item), Left );
--
--
--        Package Element_Vector_Pkg is new Ada.Containers.Indefinite_Vectors(
--                   Index_Type   => Positive,
--                   Element_Type => Element
--                 );
--
--        Procedure Consume with Inline;
--        Function  Is_End return Boolean is
--           ( Unbounded.Element(Working, 1) = 'e' );
--
--
--        Function Get_Type return Type_Character is
--        begin
--           return Result : constant Type_Character:= Unbounded.Element(Working, 1) do
--              if Result not in '0'..'9' then
--                 Consume;
--              end if;
--           end return;
--        exception
--           when Constraint_Error | Assert_Failure =>
--              raise Parsing_Error
--                with "Incalid type-tag.";
--        end Get_Type;
--
--
--        Procedure Consume is
--        begin
--           Unbounded.Delete(Working,1,1);
--        end Consume;
--
--        ------------------------------------
--        --  Headers of Parsing Functions  --
--        ------------------------------------
--
--        Function Parse		return Element;
--        Function Parse_Integer	return Integer_Element;
--        Function Parse_String	return String_Element;
--        Function Parse_List	return List_Element;
--        Function Parse_Dictionary	return Dictionary_Element;
--
--
--        Function Parse_List return Element_Vector_Pkg.Vector is
--           Use Element_Vector_Pkg;
--        begin
--           Return Result : Vector do
--              loop
--                 declare
--                    Item : constant Element:= Parse;
--                 begin
--                    Result.Append( Item );
--                 end;
--                 exit when Is_End;
--              end loop;
--              Consume;
--           end return;
--        end Parse_List;
--
--
--        Function Parse_List return List_Element is
--           Use Element_Vector_Pkg;
--           List : constant Vector:= Parse_List;
--
--           Position : Cursor:= List.First;
--           Function To_Array(Items : Vector) return Element_List is
--              Function Item return Element is
--              begin
--                 return Result : Element:= Element_Vector_Pkg.Element(Position) do
--                    Next( Position );
--                 end return;
--              end Item;
--
--           begin
--              return Result : Element_List:= (1..Natural(List.Length) => Item );
--           end To_Array;
--
--        begin
--           Return Result : List_Element := new Element_Data'(
--               Element_Type  => BT_List,
--               Length        => Natural(List.Length),
--               List_Value    => To_Array(List)
--              );
--        end Parse_List;
--
--        Function Parse_Dictionary return Dictionary_Element is
--        begin
--           Return Result : Integer_Element := new Element_Data'(
--               Element_Type  => BT_Dictionary,
--               Length        => 0,
--               Dictionary    => <>
--              )
--           do
--              loop
--                 declare
--                    Key  : constant String_Element:= Parse_String;
--                    Item : constant Element:= Parse;
--                 begin
--                    Result.Dictionary.Include(-Key, Item);
--                 end;
--                 exit when Is_End;
--              end loop;
--              Consume;
--           end return;
--        end Parse_Dictionary;
--
--
--        Function Parse_Integer return Integer_Element is
--        begin
--           Return Result : Integer_Element := new Element_Data'(
--               Element_Type  => BT_Integer,
--               Length        => 0,
--               Integer_Value => 0
--              )
--           do
--              declare
--                 Ending : constant Positive:= Index(Working, To_Set('e'))-1;
--                 Text   : constant String:= Slice(Working,1,Ending);
--              begin
--                 Delete(Working, 1, Ending);
--                 Consume;
--                 Result.Integer_Value:= Integer'Value( Text );
--              end;
--           end return;
--        end Parse_Integer;
--
--        Function Parse_String return String_Element is
--           Function Get_Length return Natural is
--              Delimeter : constant Natural := Index(Working, ":") - 1;
--           begin
--              Return Result : Natural := Natural'Value( Slice(Working,1,Delimeter) ) do
--                 Delete(Working, 1, Delimeter);
--                 Consume;
--              end return;
--           end Get_Length;
--
--           Length : constant Natural := Get_Length;
--        begin
--           Return Result : String_Element := new Element_Data'(
--               Element_Type => BT_String,
--               Length       => Length,
--               String_Value => Slice(Working, 1, Length)
--              )
--           do
--              Delete(Working, 1, Length);
--           end return;
--        end Parse_String;
--
--
--
--        Function Parse return Element is
--        begin
--           case Get_Type is
--           when '0'..'9'	=> return Parse_String;
--           when 'i'	=> return Parse_Integer;
--           when 'd'	=> return Parse_Dictionary;
--           when 'l'	=> return Parse_List;
--           end case;
--        end Parse;

   begin
      return Result : Element := Parsing.Parse( Working );
   end Value;




   procedure Put (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
                  Item   : Element_Data) is
      Use Ada.Strings.Unbounded;
      Working : Unbounded_String;
   begin
      null;
   end Put;


   Function Parse (Stream : not null access Ada.Streams.Root_Stream_Type'Class) return  Element_Data is
   begin
      return Parsing.Parse( Stream ).All;
   End Parse;

   procedure Put (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
                  Item   : Element) is
   begin
      Element_Data'Output(Stream, Item.All);
   End Put;

   Function Parse (Stream : not null access Ada.Streams.Root_Stream_Type'Class) return  Element
     renames Parsing.Parse;
     --(New Element_Data'( Element_Data'Input(Stream) ));




   Function Get_Type ( E : Element ) return Bencode_Type is
      ( E.Element_Type ); -- Could this be in the Spec?


   procedure Read(
        Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        Item   : out  Element) is
   begin
      Item:= New Element_Data'(Element_Data'Input( Stream ));
   end Read;


  procedure Write(
        Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        Item   : in  Element) is
   begin
      Element_Data'Output( Stream, Item.all );
   end Write;

End BEncoding;
