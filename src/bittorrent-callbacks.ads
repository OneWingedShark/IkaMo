with
AWS.Status;

private with
BitTorrent.Get_Callbacks;

Private Package BitTorrent.Callbacks is


   Function Announce	(Request : AWS.Status.Data) return AWS.Response.Data;
   Function Scrape	(Request : AWS.Status.Data) return AWS.Response.Data;


   Callbacks : constant Callback_Map.Map;
   Function Registered_Names(Separator : String := "") return String;
private
   Callbacks : constant Callback_Map.Map:= BitTorrent.Get_Callbacks;
End BitTorrent.Callbacks;
