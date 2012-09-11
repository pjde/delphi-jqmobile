unit uWebSocket;
{
  Delphi JQuery Mobile Components

  Copyright (c) 2012, PJ Design Engineering P/L

  JQuery, JQuery Mobile (c) 2010-2012 The JQuery Foundation
  ICS (Internet Component Suite) (c) 1999-2010 François PIETTE
  uSHA1.pas written by Dave Barton (davebarton@bigfoot.com)

  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:
     * Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.
     * Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.
     * Neither the name of the copyright holder nor the
       names of its contributors may be used to endorse or promote products
       derived from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL MIKKO KOPPANEN BE LIABLE FOR ANY
  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
}

interface

uses
 Classes, OverbyteIcsWSocket, OverbyteIcsWSocketS;

const
 // web socket decoding stages
  rsNone              = 0;
  rsHeader            = 1;
  rsExtraLength       = 2;
  rsMask              = 3;
  rsPayload           = 4;
  // web socket op codes
  opContinuation      = $0;
  opText              = $1;
  opBinary            = $2;
  // 3 - 7 reseerved for further non-control frames
  opClose             = $8;
  opPing              = $9;
  opPong              = $a;
  // b - f reserved for further control frames

type
  TWSClient = class;
  TWSServer = class;

  TWSMsgEvent = procedure (Sender : TObject; Client : TWSClient; aMsg : TMemoryStream) of object;
  TWSTextEvent = procedure (Sender : TObject; Client : TWSClient; aText : AnsiString) of object;
  TWSCloseEvent = procedure (Sender : TObject; Client : TWSClient; aCode : integer; aText : AnsiString) of object;
  TWSClientEvent = procedure (Sender : TObject; Client : TWSClient) of object;
  TWSMonEvent = procedure (Sender : TObject; aMsg : string) of object;

  TWSClient = class (TWSocketClient)
  private
    FVersion : string;
    FOrigin : string;
    FClientKey, FServerKey : AnsiString;
    FProtocol : string;
    FUpgraded : boolean;
    FHandShake : boolean;
    FRxStage : integer;
    FNeed : integer;
    FPayloadLength : integer;
    FOpCode : integer;
    FFinal : Boolean;
    FMasked : Boolean;
    FMask : array [0..3] of byte;
    FRxStream : TMemoryStream;
    FRxPacket : TMemoryStream;
    FOwner : TWSServer;
  public
    ID : string;
    Data : TObject;
    constructor Create (anOwner : TComponent); override;
    destructor Destroy; override;
    procedure Mon (aStr : string);
    procedure DataAvailable (Sender: TObject; ErrCode: Word);
    procedure SendString (Str : AnsiString);
//    procedure SendResponse (aResponse : TJQMResponse);
    procedure SendPing (Str : AnsiString);
    procedure SendPong (Str : AnsiString);
    procedure SendClose (Code : integer; aReason : AnsiString = '');
  end;

  TWSServer = class (TComponent)
  private
    FPort : integer;
    FAutoPong : boolean;
    FServer : TWSocketServer;
    FOnText : TWSMsgEvent;
    FOnPing, FOnPong : TWSTextEvent;
    FOnClose : TWSCloseEvent;
    FOnMon : TWSMonEvent;
    FOnNewClient, FOnFinishClient : TWSClientEvent;
    procedure ClientDisconnected (Sender: TObject; Client: TWSocketClient; Error: Word);
    procedure ClientCreated (Sender: TObject; Client: TWSocketClient);
    procedure ClientConnected (Sender: TObject; Client: TWSocketClient; Error: Word);
  public
    constructor Create (anOwner : TComponent); override;
    destructor Destroy; override;
    procedure Mon (aStr : string);
    procedure Start;
    procedure Stop;
    property Server : TWSocketServer read FServer;
  published
    property Port : integer read FPort write FPort;
    property AutoPong : boolean read FAutoPong write FAutoPong;
    property OnText : TWSMsgEvent read FOnText write FOnText;
    property OnPing : TWSTextEvent read FOnPing write FOnPing;
    property OnPong : TWSTextEvent read FOnPong write FOnPong;
    property OnMon : TWSMonEvent read FOnMon write FOnMon;
    property OnClose : TWSCloseEvent read FOnClose write FOnClose;
    property OnNewClient : TWSClientEvent read FOnNewClient write FOnNewClient;
    property OnFinishClient : TWSClientEvent read FOnFinishClient write FOnFinishClient;
  end;

implementation

uses
  uSHA1, SysUtils;

const
  SpecGUID = '258EAFA5-E914-47DA-95CA-C5AB0DC85B11';
  CRLF     = #13#10;

function Base64Encode (Input : AnsiString) : AnsiString;
var
  Final : AnsiString;
  Count : Integer;
  Len   : Integer;
const
  Base64Out: array [0..64] of AnsiChar =
    ('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
     'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
     'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
     'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
     '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '/', '=');
begin
  Final := '';
  Count := 1;
  Len := Length (Input);
  while Count <= Len do
    begin
      Final := Final + Base64Out[(Byte (Input[Count]) and $FC) shr 2];
      if (Count + 1) <= Len then
        begin
          Final := Final + Base64Out[((Byte (Input[Count]) and $03) shl 4) +
                                     ((Byte (Input[Count + 1]) and $F0) shr 4)];
          if (Count+2) <= Len then
            begin
              Final := Final + Base64Out[((Byte (Input[Count + 1]) and $0F) shl 2) +
                                         ((Byte (Input[Count + 2]) and $C0) shr 6)];
              Final := Final + Base64Out[(Byte (Input[Count + 2]) and $3F)];
            end
          else
            begin
              Final := Final + Base64Out[(Byte (Input[Count + 1]) and $0F) shl 2];
              Final := Final + '=';
            end
        end
      else
        begin
          Final := Final + Base64Out[(Byte (Input[Count]) and $03) shl 4];
          Final := Final + '==';
        end;
      Count := Count + 3;
    end;
  Result := Final;
end;

function GetKey (aKey : AnsiString) : AnsiString;  // get server key from client's key
var
  Tmp, Hash : AnsiString;
  Context: TSHA1Context;
  Digest: TSHA1Digest;
  i : integer;
begin
  Tmp := aKey + SpecGUID;
  SHA1Init (Context);
  SHA1Update (Context, @Tmp[1], length (Tmp));
  SHA1Final (Context, Digest);
  Hash := '';
  for i := 0 to 19 do Hash := Hash + AnsiChar (Digest[i]);
  Result := Base64Encode (Hash);
end;

{ TWSClient - Implements the Web Sockets RFC Protocol }
constructor TWSClient.Create (anOwner: TComponent);
var
  i : integer;
begin
  inherited;
  ID := '';
  FVersion := '';
  FOrigin := '';
  FClientKey := '';
  FServerKey := '';
  FProtocol := '';
  FHandShake := true;
  FUpgraded := false;
  FRxStage := rsNone;
  FMasked := false;
  for i := 0 to 3 do FMask[i] := 0;
  FRxStream := TMemoryStream.Create;
  FRxPacket := TMemoryStream.Create;
  //FOnMon := nil;
  OnDataAvailable := DataAvailable;
end;

procedure TWSClient.DataAvailable (Sender: TObject; ErrCode: Word);
var
  p : int64;
  x, y : integer;
  tmp : AnsiString;
  tmp2 : string;
  Lines : TStringList;
  Response : string;
  b : byte;

  procedure DoOutput (opCode : integer);
  var
   aStr : AnsiString;
  begin
    if FOpCode in [opPing, opPong, opClose] then
      begin
        FRxPacket.Seek (0, soFromBeginning);
        SetLength (aStr, FRxPacket.Size);
        FRxPacket.Read (aStr[1], FRxPacket.Size);
      end
    else
      aStr := '';
    case FOpCode of
      opText   :
        if Assigned (FOwner.FOnText) then FOwner.FOnText (Sender, Self, FRxPacket);
      opBinary :
        begin
          SendClose (1002, 'Binary not supported.'); // check
          CloseDelayed;
        end;
      opPing   :
        begin
          if FOwner.FAutoPong then SendPong (aStr);
          if Assigned (FOwner.FOnPing) then FOwner.FOnPing (Sender, Self, aStr);
        end;
      opPong   :
        if Assigned (FOwner.FOnPong) then FOwner.FOnPong (Sender, Self, aStr);
      opClose  :
        if length (aStr) >= 2 then
          begin
            y := ord (aStr[2]) + $100 * ord (aStr[1]);
            aStr := Copy (aStr, 3, Length (aStr) - 2);
            Mon ('Close Code ' + IntToStr (y) + ' Reason ' + string (aStr));
            if Assigned (FOwner.FOnClose) then FOwner.FOnClose (Sender, Self, y, aStr);
            CloseDelayed;
          end;
      end;
    FRxPacket.Clear;
  end;

begin
  tmp := ReceiveStrA;
  if length (tmp) = 0 then exit;
  if not FUpgraded then
    begin
      Lines := TStringList.Create;
      x := Pos (CRLF, string (tmp));
      while x > 0 do
        begin
          Lines.Add (Copy (string (tmp), 1, x - 1));
          tmp := AnsiString (Copy (tmp, x + 2, length (tmp) - x));
          x := Pos (CRLF, string (tmp));
        end;
      if Length (tmp) > 0 then Lines.Add (string (tmp));
      for x := 0 to Lines.Count - 1 do
        begin
          y := Pos (': ', Lines[x]);
          if y > 0 then
            begin
              tmp := AnsiString (Copy (Lines[x], 1, y - 1));
              tmp2 := Copy (Lines[x], y + 2, length (Lines[x]) - y);
              if tmp = 'Sec-WebSocket-Key' then
                begin
                  FClientKey := AnsiString (tmp2);
                  FServerKey := GetKey (FClientKey);
                end
              else if tmp = 'Sec-WebSocket-Version' then
                FVersion := tmp2
              else if tmp = 'Sec-WebSocket-Origin' then
                FOrigin := tmp2
              else if tmp = 'Sec-WebSocket-Protocol' then
                FProtocol := tmp2;
            end;
        end;
      Lines.Free;
      if (length (FServerKey) > 0) then
        begin
          Response :=
            'HTTP/1.1 101 Switching Protocols' + CRLF +
            'Upgrade: websocket' + CRLF +
            'Connection: Upgrade' + CRLF +
            'Sec-WebSocket-Accept: ' + string (FServerKey) + CRLF +
            'Sec-WebSocket-Origin: ' + FOrigin + CRLF +
            'Sec-WebSocket-Protocol: klingon' + CRLF + CRLF;
          SendStr (Response);
          FUpgraded := true;
          FRxStage := rsHeader; // expect new massage
          FNeed := 2;       // mimimum needed is 2 bytes
        end;
    end
  else
    begin
      p := FRxStream.Position;
      FRxStream.Seek (0, soFromEnd);
      FRxStream.Write (tmp[1], length (tmp));
      FRxStream.Seek (p, soFromBeginning);
      while (FRxStream.Size - FRxStream.Position) >= FNeed do
        begin
          case FRxStage of
            rsHeader :
              begin
                // need to implement continuation
                FRxStream.Read (b, 1);
                FOpCode := b and $0f;
                FFinal := (b and $80) > 0;
                // ignore rsvs for now
                FRxStream.Read (b, 1);
                FMasked := (b and $80) > 0;
                FPayLoadLength := b and $7f;
                if FPayLoadLength = 0 then
                  begin
                    if FMasked then
                      begin
                        FNeed := 4;
                        FRxStage := rsMask;
                      end
                    else
                      begin
                        DoOutput (FOpCode);
                        FNeed := 2;
                        FRxStage := rsHeader;
                      end;
                  end
                else if FPayloadLength <= 125 then // final length
                  begin
                    if FMasked then
                      begin
                        FNeed := 4;
                        FRxStage := rsMask;
                      end
                    else
                      begin
                        FNeed := FPayLoadLength;
                        FRxStage := rsPayload;
                      end;
                  end
                else if FPayLoadLength = 126 then
                  begin
                    FRxStage := rsExtraLength;
                    FNeed := 2;
                  end
                else if FPayLoadLength = 127 then
                  begin
                    FRxStage := rsExtraLength;
                    FNeed := 8;
                  end
              end;
            rsExtraLength :
              begin
                FPayLoadLength := 0;
                for x := 1 to FNeed do
                  begin
                    FRxStream.Read (b, 1);
                    FPayLoadLength := (FPayLoadLength * $100) + b;
                  end;
                if FMasked then
                  begin
                    FNeed := 4;
                    FRxStage := rsMask;
                  end
                else
                  begin
                    FRxStage := rsPayload;
                    FNeed := FPayLoadLength;
                  end;
              end;
            rsMask :
              begin
                FRxStream.Read (FMask, 4);
                if FPayLoadLength = 0 then
                  begin
                    DoOutput (FOpCode);
                    FRxStage := rsHeader;
                    FNeed := 2;
                  end
                else
                  begin
                    FRxStage := rsPayload;
                    FNeed := FPayLoadLength;
                  end;
              end;
            rsPayload :
              begin
                SetLength (Tmp, FPayLoadLength);
                FRxStream.Read (Tmp[1], length (Tmp));
                if FOpCode <> opContinuation then FRxPacket.Clear;
                FRxPacket.Seek (0, soFromEnd);
                for x := 1 to length (Tmp) do
                  begin
                    b := ord (Tmp[x]) xor FMask[(x - 1) mod 4];
                    FRxPacket.Write (b, 1);
                  end;
                if FFinal then DoOutput (FOpCode);
                FRxStage := rsHeader;
                FNeed := 2;
              end;
            else FRxStream.Read (b, 1);   // avoid infinite loop
          end;    // case
        end;   // while
      if FRxStream.Position = FRxStream.Size then FRxStream.Clear;
    end;
end;

destructor TWSClient.Destroy;
begin
  FRxStream.Free;
  FRxPacket.Free;
  inherited;
end;

procedure TWSClient.Mon (aStr: string);
begin
  if Assigned (FOwner.FOnMon) then FOwner.FOnMon (Self, aStr);
end;

procedure TWSClient.SendClose (Code : integer; aReason : AnsiString);
var
  i : integer;
  aMask : array [0..3] of byte;
  aMsg, aRes : AnsiString;
begin
  Code := Code mod $10000;
  for i := 0 to 3 do aMask[i] := Random ($ff);
  aRes := AnsiChar ($80 + opClose) + AnsiChar ($82);
  for i := 0 to 3 do aRes := aRes + AnsiChar (aMask[i]);
  aMsg := AnsiChar (Code mod $100) + AnsiChar (Code div $100) + aReason;
  for i := 1 to length (aMsg) do
    aRes := aRes + AnsiChar (ord (aMsg[i]) xor aMask[(i - 1) mod 4]);
  SendStr (aRes);
end;

procedure TWSClient.SendPing (Str: AnsiString);
var
  i, len : integer;
  aMask : array [0..3] of byte;
  aRes : AnsiString;
begin
  len := Length (Str);
  if len > 125 then exit;
  for i := 0 to 3 do aMask[i] := Random ($ff);
  aRes := AnsiChar ($80 + opPing) + AnsiChar ($80 + len);
  for i := 0 to 3 do aRes := aRes + AnsiChar (aMask[i]);
  for i := 1 to len do aRes := aRes + AnsiChar (ord (Str[i]) xor aMask[(i - 1) mod 4]);
  SendStr (aRes);
end;

procedure TWSClient.SendPong (Str: AnsiString);
var
  i, len : integer;
  aMask : array [0..3] of byte;
  aRes : AnsiString;
begin
  len := Length (Str);
  if len > 125 then exit;
  for i := 0 to 3 do aMask[i] := Random ($ff);
  aRes := AnsiChar ($80 + opPong) + AnsiChar ($80 + len);
  for i := 0 to 3 do aRes := aRes + AnsiChar (aMask[i]);
  for i := 1 to len do aRes := aRes + AnsiChar (ord (Str[i]) xor aMask[(i - 1) mod 4]);
  SendStr (aRes);
end;

procedure TWSClient.SendString (Str: AnsiString);
var                            // 64K maximum
  i : integer;
  len, lenmask, lendiv : uint64;
  aMask : array [0..3] of byte;
  aRes : AnsiString;
begin
  for i := 0 to 3 do aMask[i] := Random ($ff);
  len := Length (Str);
  if len > $ffff then
    begin
      aRes := AnsiChar ($81) + AnsiChar ($ff);
      lenmask := $ff00000000000000;
      lendiv := $100000000000000;
      for i := 7 downto 0 do
        begin
          aRes := aRes + AnsiChar ((len and lenmask) div lendiv);
          lenmask := lenmask div $100;
          lendiv := lendiv div $100;
        end;
    end
  else if len > 125 then
    begin
      aRes := AnsiChar ($81) + AnsiChar ($fe);
      aRes := aRes + AnsiChar (len div $100) + AnsiChar (len mod $100);
    end
  else
    aRes := AnsiChar ($81) + AnsiChar ($80 + len);
  for i := 0 to 3 do aRes := aRes + AnsiChar (aMask[i]);
  for i := 1 to len do aRes := aRes + AnsiChar (ord (Str[i]) xor aMask[(i - 1) mod 4]);
  SendStr (aRes);
end;

{ TWSServer }
procedure TWSServer.ClientConnected (Sender: TObject; Client: TWSocketClient;
  Error: Word);
begin
  Mon ('WebSocket Client Connected.');
end;

procedure TWSServer.ClientCreated (Sender: TObject; Client: TWSocketClient);
begin
  TWSClient (Client).FOwner := Self;
  if Assigned (FOnNewClient) then FOnNewClient (Self, TWSClient (Client));
end;

procedure TWSServer.ClientDisconnected (Sender: TObject; Client: TWSocketClient;
  Error: Word);
begin
  Mon ('WebSocket Client Disconnected.');
  if Assigned (FOnFinishClient) then FOnFinishClient (Self, TWSClient (Client));
end;

constructor TWSServer.Create (anOwner: TComponent);
begin
  inherited;
  FAutoPong := true;
  FPort := 10200;
  FServer := TWSocketServer.Create (anOwner);
  FServer.OnClientConnect := ClientConnected;
  FServer.OnClientDisconnect := ClientDisconnected;
  FServer.OnClientCreate := ClientCreated;
end;

destructor TWSServer.Destroy;
begin
  Stop;
  FServer.Free;
  inherited;
end;

procedure TWSServer.Mon (aStr: string);
begin
  if Assigned (FOnMon) then FOnMon (Self, aStr);
end;

procedure TWSServer.Start;
begin
  FServer.Addr := '0.0.0.0';
  FServer.Banner := '';
  FServer.Port := IntToStr (FPort);
  FServer.Proto := 'tcp';
  FServer.ClientClass := TWSClient;
  try
    FServer.Listen;
    Mon ('WebSockets Listening.');
  except
    Mon ('Error WebSockets Listening.');
  end;
end;

procedure TWSServer.Stop;
var
  i : integer;
begin
  for i := 0 to FServer.ClientCount - 1 do
    try
      FServer.Client[i].Close;
    except
    end;
  try
    FServer.Close;
  except
  end;
end;

end.
