--  Pragma Assertion_Policy( DISABLE );
                        --Ignore );
with
--  BEncoding,
BEncoding_Parser.Subtypes,
BitTorrent.MetaInfo,
BitTorrent.Tracker_HTTP_Protocol,
BitTorrent.Tracker,

Ada.Streams.Stream_IO,
Ada.Text_IO.Text_Streams,
Ada.Exceptions.Traceback,


AWS,
AWS.Client,
AWS.Response,
AWS.Server,
AWS.Status,
AWS.Parameters.Set,
AWS.Utils.Streams,

Ada.Text_IO;

use
Ada.Text_IO,
BitTorrent.Tracker_HTTP_Protocol;


with
AWS.Communication.Client,
AWS.Containers.Tables;

Procedure IkaMo is

   package BEncoding renames BEncoding_Parser;

   Function Get_Stream(F : in out Ada.Text_IO.File_Type) return not null access Ada.Streams.Root_Stream_Type'Class is
      Use Ada.Text_IO;
   begin
      Open(File => F, Mode => In_File, Name => "gimp-2.8.14-setup-1.exe.torrent" );
      Return Ada.Text_IO.Text_Streams.Stream( F );
   end;

   Input_File   : Ada.Text_IO.File_Type;
   Input_Stream : not null access Ada.Streams.Root_Stream_Type'Class:=
     Get_Stream(Input_file);

--      Output : Access Ada.Streams.Root_Stream_Type:=
--          Ada.Text_IO.Text_Streams.Stream( Standard_Output );

   A : constant string := "d5:Helloi23e3:icei4ee";
   K : BEncoding.Element :=
               BEncoding.Element'Input( Input_Stream );
--                   BEncoding_Parser.Value( A );


   function CB2 (Request : AWS.Status.Data) return AWS.Response.Data is
      --pragma Unreferenced (Request);

      Procedure Process (Name, Value : String) is
      begin
         Ada.Text_IO.Put_Line(Name & ':' & ASCII.HT & Value);
      end Process;

      Procedure Iteration is
        new AWS.Containers.Tables.Generic_Iterate_Names(Process);

      Params : AWS.Parameters.List renames AWS.Status.Parameters(Request);
   begin
      Ada.Text_IO.Put_Line( "Beginning Iterating." );
      Iteration( Table => AWS.Containers.Tables.Table_Type(Params), Separator => "&" );
      Ada.Text_IO.Put_Line( "Finished Iterating." );
      return AWS.Response.Build ("text/html", "---- Catcher's Mitt ----"
                                 & ASCII.CR & ASCII.LF &
                                   ""
                                );
   end Cb2;
--
--
--     function CB (Request : AWS.Status.Data) return AWS.Response.Data is
--        --pragma Unreferenced (Request);
--
--        Procedure Process (Name, Value : String) is
--        begin
--           Ada.Text_IO.Put_Line(Name & ':' & ASCII.HT & Value);
--        end Process;
--
--        Procedure Iteration is
--          new AWS.Containers.Tables.Generic_Iterate_Names(Process);
--
--        Params : AWS.Parameters.List renames AWS.Status.Parameters(Request);
--     begin
--        Ada.Text_IO.Put_Line( "Iterating." );
--        Iteration( Table => AWS.Containers.Tables.Table_Type(Params), Separator => "&" );
--
--
--
--        return AWS.Response.Build ("text/html", "Hello world!");
--     end CB;

   TheServer : AWS.Server.HTTP;
   ch : Character;


   Function Port return String  is ( "6881" );
   Function Port return Natural is ( Natural'Value(Port) );

   use all type BEncoding.Element;
Begin

   Ada.Text_IO.Put_Line( "Type:  " & BEncoding.Get_Type(K)'img );
   --Ada.Text_IO.Put_Line( "Image: " & BEncoding.Image(K) );

   declare
      --      Use BitTorrent.MetaInfo;
      use BEncoding_Parser.Subtypes;
      H : BEncoding_Parser.Subtypes.Dictionary_Element:= K;
      --J : MetaInfo:= Make( H );
      I : Dictionary_Element:= H / "info";
--      Len : Integer_Element := I / "piece length";
      Ann : String_Element  := H / "announce";
   begin
      null;
--        Ada.Text_IO.Put_Line( "Length:" & Integer'Image(-Len) );
      Ada.Text_IO.Put_Line( "Announce: " & (-Ann) );

--        Ada.Text_IO.Put_Line( "info: " & BEncoding_Parser.Image(I) );
--        Ada.Text_IO.Put_Line( "Name: " & Get_Name(I) );
   end;


--     BEncoding_Parser.Element'Output( Output, K );

-- AWS Items
   AWS.Server.Start (TheServer, "Rosettacode",
                     Callback => CB2'Unrestricted_Access, Port => Port);
--     AWS.Server.Start (TheServer, "Rosettacode",
--                       Callback => BitTorrent.Tracker.Handler'Unrestricted_Access, Port => 8080);



   --  SEE:   AWS.Status.Set.
--     AWS.Parameters.Set.

   declare
      Use aws.Parameters.Set;
      L : AWS.Parameters.List;

      Peer_Name  : constant String_20:= "IKAMO-DEBUGGING-----";
      Test_Value : constant String:=    "12345678901234567890";

      use BitTorrent.Tracker_HTTP_Protocol;
      use aws.Utils.Streams;

      Info : BEncoding_Parser.Subtypes.Dictionary_Element renames K;
      K : aliased SHA1;
      use BEncoding_Parser.Subtypes;

      Tracker  : constant String:= "http://tracker.minglong.org:8080";
      Announce : constant String:= "/announce";

      Response : AWS.Response.Data;
   begin
      null;
      String'Write( K'Access, Image( Dictionary_Element(Info) / "info" ) );
      L:= --Create_Request( K.Value, Test_Value, "80" );
        Create_Request(
           Info_Hash  => K.Value,
           Peer_ID    => Peer_Name,
           Port       => Port,
           Uploaded   => "0",
           Downloaded => "0"
                      );
      -- tracker.publicbt.com:80
      Ada.Text_IO.Put_Line(Tracker & Announce & L.URI_Format );

Response:=
      AWS.Client.Get(
                     URL                => Tracker & Announce & L.URI_Format
                    --"http://www.googleapis.com/customsearch/v1?key=INSERT_YOUR_API_KEY"
        --&cx=017576662512468239146:omuauf_lfve&q=lectures"
--                       ,
--                       User               => ,
--                       Pwd                => ,
--                       Proxy              => ,
--                       Proxy_User         => ,
--                       Proxy_Pwd          => ,
--                       Timeouts           => ,
--                       Data_Range         => ,
--                       Follow_Redirection => False,
--                       Certificate        => ,
--                       Headers            =>
                    );

      Ada.Text_IO.Put_Line( "Code:" & AWS.Response.Status_Code(Response)'Img );
      Ada.Text_IO.Put_Line( "----------------------------------------------" );
        Ada.Text_IO.Put_Line(AWS.Response.Message_Body( Response ));
      Ada.Text_IO.Put_Line( "----------------------------------------------" );

    Ada.Text_IO.Put_Line("Parameter List" & ASCII.HT & L.URI_Format);
   end;

   Ada.Text_IO.Put_Line ("Press any key to quit.");
   Ada.Text_IO.Get_Immediate (ch);
   AWS.Server.Shutdown (TheServer);


Ada.Text_IO.Put_Line( "Done." );
End IkaMo;
