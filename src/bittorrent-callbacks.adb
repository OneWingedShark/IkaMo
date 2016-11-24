with
AWS.MIME,
Ada.Containers.Vectors,
Ada.Containers.Indefinite_Holders;

Package body BitTorrent.Callbacks is

   Function Announce	(Request : AWS.Status.Data) return AWS.Response.Data is
   begin
      Return Result : AWS.Response.Data := AWS.Response.Build(
--              Status_Code   => ,
--              Cache_Control => ,
--              Encoding      => ,
            Content_Type  => AWS.MIME.Text_Plain,
            Message_Body  => "bencoded dictionary"
         );
   End Announce;

   Function Scrape	(Request : AWS.Status.Data) return AWS.Response.Data is
   begin
      Return Result : AWS.Response.Data do
         Result:= AWS.Response.Build(
--              Status_Code   => ,
--              Cache_Control => ,
--              Encoding      => ,
            Content_Type  => AWS.MIME.Text_Plain,
            Message_Body  => "bencoded dictionary"
         );
      end return;
   End Scrape;


   Package SString is new Ada.Containers.Indefinite_Holders(String);
   Package SString_List is new Ada.Containers.Vectors
     (Positive, SString.Holder, SString."=");

   Function Registered_Names return SString_List.Vector is
      use SString_List, SString;
   begin
      Return Result : Vector do
         declare
            Procedure Process(C: Callback_Map.Cursor) is
            begin
               Result.Append( To_Holder(Callback_Map.Key(C)) );
            end Process;
         begin
            Callbacks.Iterate(Process'Access);
         end;
      end return;
   End Registered_Names;

   Function Registered_Names (Separator : String := "") return String is
      Function Names( List    : in out SString_List.Vector;
                      Divider : String:= Separator ) return String is
         Length : Constant Natural:= Natural( List.Length );
      begin
         case Length is
            when 0 => return "";
            when 1 => return SString.Element(List.Last_Element);
            when others =>
               declare
                  Text : constant String:= SString.Element( List.Last_Element );
               begin
                  List.Delete_Last;
                  return Names(List, Divider) & Divider & Text;
               end;
         end case;
      end Names;

      Working : SString_List.Vector:= Registered_Names;
   begin
      return Names( Working, Separator );
   end Registered_Names;



End BitTorrent.Callbacks;
