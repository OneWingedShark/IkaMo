with
GNAT.SHA1,
AWS.Parameters.Set;

Package BitTorrent.Tracker_HTTP_Protocol is

   subtype String_20 is String(1..20);

   Function Create_Request (
                            Info_Hash : GNAT.SHA1.Message_Digest;
                            Peer_ID : String_20;
                            Port : String;
                            Uploaded : String := "0";
                            Downloaded : String := "0"
                           )
                               return AWS.Parameters.List;

End BitTorrent.Tracker_HTTP_Protocol;
