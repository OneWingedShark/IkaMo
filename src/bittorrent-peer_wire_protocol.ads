with ada.Exceptions.Traceback;
with system;
Package BitTorrent.Peer_Wire_Protocol is

   type Handshake(<>) is private;


   Type Message_ID is (
               Choke,
               Unchoke,
               Interested,
               Uninterested,
               Have,
               Bitfield,
               Request,
               Piece,
               Cancel
              ) with Size => 8, Object_Size => 8;

Private
   use Interfaces;

   Default_Protocol : constant String(1..19):= "BitTorrent protocol";

   type Handshake( Length : Natural ) is record
      Protocol : String(1..Length);
      Reserved : String(1..8);
      InfoHash,
      Peer_ID  : String(1..20);
   end record
     with Type_Invariant =>
       Interfaces.Unsigned_8'Val(Handshake.Length) = Interfaces.Unsigned_8(Handshake.Length);

   subtype Defautlt_Handshake is Handshake(Length => 19)
   with Dynamic_Predicate => Defautlt_Handshake.Protocol = Default_Protocol
                             or else raise Program_Error with "Invalid protocol";




   Type Bytes is array(Positive range <>) of Unsigned_8;

   type PWP_Message(Length : Natural; ID : Message_ID) is record
      case Length is
         when 0 => null;
         when others =>
            case ID is
               when Choke | Unchoke |
                    Interested | Uninterested	=> null;
               when Have			=> Have_Index   : Unsigned_32;
               when Bitfield			=> Bitfield_Payload : Bytes(1..Length);
               when Request | Cancel | Piece	=> Piece_Index,
                                                   Block_Offset : Unsigned_32;
                  case ID is
                     when Request | Cancel	=> Block_Length : Unsigned_32;
                     when Piece			=> Block_Data   : Unsigned_32;
                     when others		=> null;
                  end case;
            end case;
      end case;
   end record;


End BitTorrent.Peer_Wire_Protocol;
