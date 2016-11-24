with
Ada.Containers.Indefinite_Ordered_Maps,
Ada.Containers.Indefinite_Vectors;


package body BEncoding_Parser is

   Type Element_Data(Element_Type : Bencode_Type; Length : Natural) is record
      case Element_Type is
         when BT_String     => String_Value  : String(1..Length);
         when BT_Integer    => Integer_Value : Integer;
         when BT_List       => List_Value    : Element_List(1..Length);
         when BT_Dictionary => Dictionary    : Element_Map_Pkg.Map;
      end case;
   end record;


   --------------------------------------------
   --  BASIC OPERATIONS FOR GENERIC PARSING  --
   --------------------------------------------

   Generic
      Type T(<>) is limited private;

      -- Next is assumed to be a destructive read.
      with Function Next( Stream : not null access T ) return Character;
   Package Atomic_Primitives is

      -- Because Next is a destructive-read, we provide
      -- a single-character buffer which, when Current is
      -- called upon it either fills it via Next or, if it
      -- already contains a character, returns its contents.
      --
      -- Clear empties the buffer, and thus a subsequent
      -- call to Current will fill it again, but not until
      -- it is called.
      Function  Current( Stream : not null access T ) return Character;
      Procedure Clear  ( Stream : not null access T );
   End Atomic_Primitives;

   Package body Atomic_Primitives is
      Data : character:= ASCII.NUL;
      Open : Boolean:=   False;

      Procedure Clear( Stream : not null access T ) is
      begin
         Open:= False;
      end Clear;

      Function  Current( Stream : not null access T ) return Character is
      begin
         if not Open then
            Open:= True;
            Data:= Next(Stream);
         end if;

         return Data;
      end Current;
   End Atomic_Primitives;


   ----------------------------
   --  CONSTRAINED SUBTYPES  --
   ----------------------------

   subtype String_Element is Element
   with Predicate => String_Element.Element_Type = BT_String;

   subtype Integer_Element is Element
   with Predicate => Integer_Element.Element_Type = BT_Integer;

   subtype List_Element is Element
   with Predicate => List_Element.Element_Type = BT_List;

   subtype Dictionary_Element is Element
   with Predicate => Dictionary_Element.Element_Type = BT_Dictionary;

   -----------------------
   --  HELPER FUNCTIONS --
   -----------------------

   Function "-"( Item : String_Element  ) return String is
     ( Item.String_Value )
     with Inline;

   Function Get_Type ( Item : Element ) return Bencode_Type is
     ( Item.Element_Type );

   function Get_Value( Item : Element ) return String is
     ( Item.String_Value );

   function Get_Value( Item : Element ) return Integer is
     ( Item.Integer_Value );

   function Get_Value( Item : Element ) return Element_List is
     ( Item.List_Value );

   function Get_Value( Item : Element ) return Element_Map_Pkg.Map is
     ( Item.Dictionary );

   function Get_Value( Item : Element ) return Element_Data is
     ( Item.All );

   function Get_Value( Item : Element; Entre : String ) return Element is
     ( Item.Dictionary(Entre) );

   function Create( Item : String ) return Element is
     ( New Element_Data'(Element_Type  => BT_String,
                         Length        => Item'Length,
                         String_Value  => Item)
     );

   function Create( Item : Integer ) return Element is
     ( New Element_Data'(Element_Type   => BT_Integer,
                         Length         => 0,
                         Integer_Value  => Item)
     );

   function Create( Item : Element_List ) return Element is
     ( New Element_Data'(Element_Type   => BT_List,
                         Length         => Item'Length,
                         List_Value     => Item)
     );

   function Create( Item : Element_Map_Pkg.Map ) return Element is
     ( New Element_Data'(Element_Type   => BT_Dictionary,
                         Length         => 0,
                         Dictionary     => Item)
     );
   function Create( Key : String; Entre : Element ) return Element is
   begin
      Return Result : Element:= New Element_Data'(BT_Dictionary,0,others => <>) do
         Append( Result, Key, Entre );
      end return;
   end;

   Procedure Append( Item : in out Element; Key : String; Entre : Element ) is
   begin
      Item.Dictionary.Insert( Key, Entre );
   end;

   Function Exists( Item : Element; Key : String ) return Boolean is
     ( Element_Map_Pkg."/="(Item.Dictionary.Find(Key), Element_Map_Pkg.No_Element) );

   Function  Length( Item : Element ) return Natural is
     (case Item.Element_Type is
         when BT_Integer    => 1,
         when BT_String     => Item.String_Value'Length,
         when BT_List       => Item.List_Value'Length,
         when BT_Dictionary => Natural(Item.Dictionary.Length)
     );


   -----------------------
   --  GENERIC PARSING  --
   -----------------------

   Generic
      with package Primitives is new Atomic_Primitives(<>);
   Package Generic_Parsing is
      Use primitives;

      Function Parse ( Working : aliased in out T ) return Element;

   Private

      Package Element_Vector_Pkg is new Ada.Containers.Indefinite_Vectors(
                 Index_Type   => Positive,
                 Element_Type => Element
                                                                         );

      Function Parse_String	(Working : aliased in out T) return     String_Element;
      Function Parse_Integer	(Working : aliased in out T) return    Integer_Element;
      Function Parse_List	(Working : aliased in out T) return       List_Element;
      Function Parse_Dictionary	(Working : aliased in out T) return Dictionary_Element;
   End Generic_Parsing;

   Package body Generic_Parsing is separate;


   -------------------------------
   --  IMAGE & VALUE FUNCTIONS  --
   -------------------------------

   Function Value( Item : String ) return Element is
      type VString is record
         Index : Natural := Item'First;
      end record;

      Function Get_Next( Stream : not null access VString ) return Character is
      begin
         Return Result : constant character := Item(Stream.Index) do
            Stream.Index:= 1 + Stream.Index;
         end return;
      end Get_Next;

      Package String_Atomics is new Atomic_Primitives( VString, Get_Next );
      Package String_Parsing is new Generic_Parsing  ( String_Atomics    );

      Cursor : aliased VString;
   begin
      Return String_Parsing.Parse( Cursor );
   end Value;


   Function Image( Item : Element ) return String is

      ----------------------
      --  ENCODE HEADERS  --
      ----------------------

      Function Encode( I : Integer		) return String;
      Function Encode( S : String		) return String;
      Function Encode( L : Element_List		) return String;
      Function Encode( D : Element_Map_Pkg.Map	) return String;

      ------------------------
      --  HELPER FUNCTIONS  --
      ------------------------

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

      Function Process_List( L : Element_List ) return String is
        (case L'Length is
            when 0 => "",
            when 1 => Image(L(L'First)),
            when others => Image(L(L'First)) & Process_List(L(1+L'First..L'Last))
        );

      Function Process_Dict( D : Element_Map_Pkg.Map ) return String is

         Function "&"(Left, Right : String) return not null access String is
            ( New String'( Left & Right ) );

         Function "&"(Left, Right : not null access string) return not null access string is
            ( Left.all & Right.all );

         Package Maps renames Element_Map_Pkg;
         Working : not null access string:= new String'(2..1 => <>);

         procedure Process(Position : Maps.Cursor) is
         begin
            Working := Working &
                        (Encode(Maps.Key(Position)) &
                         Image (Maps.Element(Position))
                       );
         end Process;
      begin
         D.Iterate( Process'Access );
         return Working.all;
      end Process_Dict;

      --------------------
      -- ENCODE BODIES  --
      --------------------

      Function Encode( I : Integer ) return String is
        ( Tag('i', Image(I)) );

      Function Encode( S : String ) return String is
        ( Image(S'Length) & ':' & S );

      Function Encode( L : Element_List ) return String is
         ( Tag('l', Process_List(L)) );

      Function Encode( D : Element_Map_Pkg.Map ) return String is
         ( Tag('d', Process_Dict(D)) );
   begin
      case Item.Element_Type is
         when BT_String     => return Encode( Item.String_Value  );
         when BT_Integer    => return Encode( Item.Integer_Value );
         when BT_List       => return Encode( Item.List_Value    );
         when BT_Dictionary => return Encode( Item.Dictionary    );
      end case;
   end Image;


   ----------------------------
   --  READ, WRITE, AND etc  --
   ----------------------------

   Function Get_Next( Stream : not null access Ada.Streams.Root_Stream_Type'Class ) return Character is
     ( Character'Input(Stream) );

   Package Stream_Atomics is new Atomic_Primitives( Ada.Streams.Root_Stream_Type'Class, Get_Next );
   Package Stream_Parsing is new Generic_Parsing( Stream_Atomics );

   procedure Read(
        Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        Item   : out  Element) is
   begin
      Item := Stream_Parsing.Parse( Stream.all );
   end Read;

  procedure Write(
        Stream : not null access Ada.Streams.Root_Stream_Type'Class;
        Item   : in  Element) is
   begin
      String'Write( Stream, Image( Item ) );
   end;

end BEncoding_Parser;
