Package body BitTorrent.Tracker_HTTP_Protocol is
   Function Create_Request (
                            Info_Hash: GNAT.SHA1.Message_Digest;
                            Peer_ID : String_20;
                            Port : String;
                            Uploaded : String := "0";
                            Downloaded : String := "0"
                           ) return AWS.Parameters.List is
      Use AWS.Parameters.Set;
   begin
      Return Result : AWS.Parameters.List do
         Add( Result, "info_hash", Info_Hash, False );
         Add( Result, "peer_id", Peer_ID, False );
         Add( Result, "port", Port, False );
         Add( Result, "uploaded", Uploaded, False );
         Add( Result, "downloaded", Downloaded, False );
      end return;
   end Create_Request;


End BitTorrent.Tracker_HTTP_Protocol;
