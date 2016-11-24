with BitTorrent.Callbacks;

Function BitTorrent.Get_Callbacks return Callback_Map.Map is
Begin
   Return Result : Callback_Map.Map do
      Result.Include( "announce",	BitTorrent.Callbacks.Announce'access	);
      Result.Include( "scrape",		BitTorrent.Callbacks.Scrape'access	);
   end return;
End BitTorrent.Get_Callbacks;
