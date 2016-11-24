with BEncoding_Parser.Subtypes;
use  BEncoding_Parser.Subtypes;

Package BitTorrent.MetaInfo is
   use all type
     BEncoding_Parser.Subtypes.String_Element,
     BEncoding_Parser.Subtypes.Dictionary_Element;

   Type Info ( Data : not null access BEncoding_Parser.Subtypes.Dictionary_Element ) is tagged private
     with Implicit_Dereference => Data;

   Function Make( Data : BEncoding_Parser.Subtypes.Dictionary_Element ) return Info;

   Subtype MetaInfo is BEncoding_Parser.Subtypes.Dictionary_Element
     with Dynamic_Predicate => Exists(MetaInfo, "info");

   Function Get_Aannounce( Item : MetaInfo ) return String is
      ( -BEncoding_Parser.Subtypes.String_Element'(Item / "announce") );

   Function Get_Info( Item : MetaInfo ) return Info is
      (Make(Item / "info"));


   Function "/"( Left : Info; Right : String ) return BEncoding_Parser.Element is
     ( BEncoding_Parser.Subtypes."/"(BEncoding_Parser.Subtypes.Dictionary_Element'(Left), Right) );

   Function Get_Name( Item : Info ) return String is
     ( -BEncoding_Parser.Subtypes.String_Element'(Item.data.All / "name") );

Private
   Function "-"( Item : Info ) return BEncoding_Parser.Subtypes.Dictionary_Element is
     ( Item.Data.All );

   Type Info ( Data : not null access BEncoding_Parser.Subtypes.Dictionary_Element ) is tagged null record
     with Type_Invariant =>       Exists(-Info, "name")         and
                                  Exists(-Info, "piece length") and
                                  Exists(-Info, "pieces")       and
     (Exists(-Info, "length") xor Exists(-Info, "files"))
   ;

   Function Make( Data : BEncoding_Parser.Subtypes.Dictionary_Element ) return Info is
      (Data => New Dictionary_Element'(Data / "info"));

End BitTorrent.MetaInfo;
