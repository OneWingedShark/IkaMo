Package Body Bittorrent.Tracker_Response is
   Function Make_Response(
                          Interval         : Positive;
                          Complete,
                          Incomplete       : Natural;
                          Peers            : String;
                          Tracker_ID       : access String:= null;
                          Minimum_Interval : access Integer:= null;
                          Warning_Message  : access String:= null
                         ) return Response is
      Result : Dictionary_Element;
   begin
      return Result;
   end Make_Response;


   Function Make_Response(
                          Interval         : Positive;
                          Complete,
                          Incomplete       : Natural;
                          Peers            : Dictionary_Element;
                          Tracker_ID       : access String:= null;
                          Minimum_Interval : access Integer:= null;
                          Warning_Message  : access String:= null
                         ) return Response is
      Result : Dictionary_Element;
   begin
      return Result;
   end Make_Response;


End Bittorrent.Tracker_Response;
