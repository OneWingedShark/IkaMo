With
BitTorrent.Callbacks,
AWS.MIME,
AWS.URL;

Package Body Bittorrent.Tracker is
   Use AWS;

   function Enclose( Text, Start, Stop : String ) return String is
     ( Start & Text & Stop );
   function Enclose( Text : String; Start, Stop : Character ) return String is
     ( Start & Text & Stop );
   function Quote(S : String) return String is ( Enclose(S, '"', '"') );
   function Start_Tag( S : String ) return String is ( Enclose(S, '<', '>') );
   function Stop_Tag ( S : String ) return String is ( Enclose(S, "</", ">") );
   function HTML_Tag(Name, Inner_HTML : String; Self_Closing : Boolean:= False)
                     return String is
     (if Self_Closing then Enclose(Name&' '&Inner_HTML, "<", " />")
      else Start_Tag(Name) & Inner_HTML & Stop_Tag(Name)
     );


   Function Handler(Request : AWS.Status.Data) return AWS.Response.Data is
      Handlers : Callback_Map.Map renames BitTorrent.Callbacks.Callbacks;

      URL : constant AWS.URL.Object:= AWS.Status.URI(Request);
      Path : constant String:= AWS.URL.Path(URL);
      File : constant String:= AWS.URL.File(URL);
      Handle : BitTorrent.Callback_Map.Cursor:= Handlers.Find( File );
      BR : constant string:= "<br />";

      Use AWS.Response, Callback_Map;
   begin
      return Result : Data do
         if Handle = No_Element then
            declare
               function List return string is ( Callbacks.Registered_Names(BR) );
            begin
               Result:= AWS.Response.Build
                 (MIME.Text_HTML, "<h1>Invalid URI</h1>"& BR &
                    "<tt>"& AWS.Status.URL(Request) &"</tt>" & BR &
                    "Path: " & Path & BR &
                    "File: " & File & BR &
                    "<hr />" & "Acceptable URLs:" & HTML_Tag( "PRE", List )
                 );
            end;
         else
            Result := Element(Handle)(Request);
         end if;
      end return;
   End Handler;

End Bittorrent.Tracker;
