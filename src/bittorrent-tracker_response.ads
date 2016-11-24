with
BEncoding_Parser.Subtypes;

Use
BEncoding_Parser.Subtypes;

Private Package Bittorrent.Tracker_Response with Elaborate_Body is

   subtype Error is Dictionary_Element
   with Dynamic_Predicate =>     Exists(Error, "failure reason")
                             and Keys(Error) = 1;

   Function Make_Error( Failure_Reason : String ) return Error;

   subtype Response is Dictionary_Element
   with Dynamic_Predicate =>     Exists(Response, "interval")
                             and Exists(Response, "tracker id")
                             and Exists(Response, "complete")
                             and Exists(Response, "incomplete")
                             and Exists(Response, "peers");

   Function Make_Response(
                          Interval         : Positive;
                          Complete,
                          Incomplete       : Natural;
                          Peers            : String;
                          Tracker_ID       : access String:=  null;
                          Minimum_Interval : access Integer:= null;
                          Warning_Message  : access String:=  null
                         ) return Response;

   Function Make_Response(
                          Interval         : Positive;
                          Complete,
                          Incomplete       : Natural;
                          Peers            : Dictionary_Element;
                          Tracker_ID       : access String:=  null;
                          Minimum_Interval : access Integer:= null;
                          Warning_Message  : access String:=  null
                         ) return Response;

private

   Function Make_Error( Failure_Reason : String ) return Error is
     ( "failure reason" & (+Failure_Reason) );


End Bittorrent.Tracker_Response;
