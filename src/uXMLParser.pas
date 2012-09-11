unit uXMLParser;

{
  On-the-fly XML parser for Delphi JQuery Mobile Components

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
uses Classes;

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

type
  { Token types returned by parser }
  TTokens = (toElement, toComment, toText);
  { XML tag types }
  TTagTypes = (ttOpening, ttClosing, ttEmpty);
  TStages = (stIdle, stTag, stElement, stName, stValue, stDTD, stNonElement, stInstruction, stParams, stComment, stIgnore, stError);
  TTokenEvent = procedure (Sender : TObject; Token : TTokens; TokenName : AnsiString;
                              Tag : TTagTypes; Params : TList) of object;
  TTokenValueEvent = procedure (Sender : TObject; aName, aValue : AnsiString) of object;
  TTokenErrorEvent = procedure (Sender: TObject; ErrorMsg : AnsiString) of object;

  TParam = class (TObject)
    Name : Ansistring;
    Value : Ansistring;
    function IntName : integer;
    function IntValue : integer;
    function BoolValue : boolean;
    function CharValue : AnsiChar;
    constructor Create;
  end;

  TXMLParser = class (TObject)
  private
    FTagType : TTagTypes;
    FStage : TStages;
    FParam : TParam;
    FText : AnsiString;
    FOnToken : TTokenEvent;
    FOnError : TTokenErrorEvent;
    FOnValue : TTokenValueEvent;
  protected
    Params : TList;
    SkipSpaces : boolean;
    Quoted : boolean;
    TextValue : boolean;
    Buffer : TMemoryStream;
    TokenString : AnsiString;
    TokenStack : TStringList;
    function GetChar : AnsiChar;
    procedure DoParsing;
    procedure ClearParams;
    procedure DoToken (Token : TTokens; var TokenName : AnsiString; Tag : TTagTypes; Attributes : TList);
    procedure DoError (ErrorMsg : AnsiString);
    procedure DoValue (aName, aValue : AnsiString);
  public
    Owner : TObject;
    Tag, Tag2 : integer; // general puspose integers
    constructor Create (anOwner : TObject);
    destructor Destroy; override;
    procedure Parse (Data : AnsiString); overload;
    procedure Parse (Data : TStream); overload;
    function Depth : integer;
    procedure Reset;
    property OnToken : TTokenEvent read FOnToken write FOnToken;
    property OnError : TTokenErrorEvent read FOnError write FOnError;
    property OnValue : TTokenValueEvent read FOnValue write FOnValue;
  end;

const
  tgt : array [TTagTypes] of string = ('Opening', 'Closing', 'Empty');
  tkt : array [TTokens] of string = ('Element', 'Comment', 'Text');


implementation
uses SysUtils;
{$REGION 'TParam' }
{ TParam }
function TParam.BoolValue: boolean;
begin
  Result := not ((CharValue = '0') or (CharValue = 'F'));
end;

function TParam.CharValue: AnsiChar;
begin
  if Length (Value) = 0 then
    Result := #0
  else
    Result := UpCase (Value[1]);
end;

constructor TParam.Create;
begin
  Name := '';
  Value := '';
end;

function TParam.IntName: integer;
begin
 Result := StrToIntDef (string (Name), -1);
end;

function TParam.IntValue: integer;
begin
  if UpperCase (string (Value)) = 'TRUE' then
    Result := 1
  else if UpperCase (string (Value)) = 'FALSE' then
    Result := 0
  else
    Result := StrToIntDef (string (Value), -1);
end;
{$ENDREGION}
{$REGION 'TXMLParser' }
{ TXMLParser }
procedure TXMLParser.ClearParams;
var
  i : integer;
begin
  for i := 0 to Params.Count - 1 do
    TParam (Params[i]).Free;
  FParam:= nil;
  Params.Clear;
end;

constructor TXMLParser.Create (anOwner: TObject);
begin
  inherited Create;
  Params := TList.Create;
  Owner := anOwner;
  FStage := stIdle;
  SkipSpaces := false;
  FTagType := ttOpening;
  FParam := nil;
  TextValue := false;
  FText := '';
  Buffer := TMemoryStream.Create;
  TokenStack := TStringList.Create;
  FOnToken := nil;
  FOnError := nil;
  FOnValue := nil;
  Tag := 0;
  Tag2 := 0;
end;

function TXMLParser.Depth: integer;
begin
  Result := TokenStack.Count;
end;

destructor TXMLParser.Destroy;
begin
  ClearParams;
  Params.Free;
  TokenStack.Free;
  Buffer.Free;
  inherited;
end;

procedure TXMLParser.DoError (ErrorMsg: AnsiString);
begin
  if Assigned (FOnError) then FOnError (Self, ErrorMsg);
end;

procedure TXMLParser.DoParsing;
const
  st : array[TStages] of AnsiString =
    ('Idle', 'Tag', 'Element', 'Name', 'Value', 'DTD', 'NonElement', 'Instruction', 'Params', 'Comment', 'Ignore', 'Error');

var
  delims : AnsiString;
  aChar : AnsiChar;
  CanLoad : boolean;

  function Delim (delims_ : AnsiString; chr_ : AnsiChar) : boolean;
  begin
    Result := Pos (chr_, delims_) > 0;
  end;

begin
  delims := '<';
  aChar := GetChar;
  while aChar <> #0 do
    begin
      case FStage of
        stIdle   : delims := '<';
        stIgnore : delims := '<>';
        stTag    : delims := '!? '#9'/<>';
        stName   : delims := ' =/<>';
        stValue  : delims := ' '#9'/<>';
        else       delims := '<';
      end;

      CanLoad := (not Delim (delims, aChar)) and (aChar <> #0);
      while CanLoad do
        begin
          if (aChar = #13) and (quoted) then
            begin
              DoError('Unterminated string');
              quoted := false; // error condition
            end;
         case FStage of
            stIdle : TokenString := TokenString + aChar;
            stTag :
              if CharInSet (aChar, ['?', '!']) then
                FStage := stIgnore
              else
                TokenString := TokenString + aChar;
            stName :
              begin
                if FParam = nil then
                  begin
                    FParam := TParam.Create;
                    Params.Add (FParam);
                  end;
                FParam.Name := FParam.Name + aChar;
              end;
            stValue :
              if aChar = '"' then
                begin
                  if quoted then
                    begin
                      CanLoad := false;
                      quoted := false;
                    end
                  else
                    quoted := true;
                  aChar := #0;
                end
              else
                if FParam <> nil then FParam.Value := FParam.Value + aChar;
            end; // case
          if CanLoad then
            begin
              aChar := GetChar;
              CanLoad := (not Delim (delims, aChar)) and (aChar <> #0);
            end;
        end; // while CanLoad

      // delimiters encountered
      if aChar = #0 then
        begin
           // do nothing
        end
      else if (FStage = stValue) and quoted then
        FParam.Value := FParam.Value + aChar
      else if aChar = '<' then
        begin
          ClearParams;
          TokenString := AnsiString (Trim (string (TokenString)));
          if length (TokenString) > 0 then DoToken (toText, TokenString, ttEmpty, Params);
          FStage := stTag;
          TokenString := '';
          FParam := nil;
          FTagType := ttOpening;
        end
      else if aChar = '>' then
        begin
          if length (TokenString) > 0 then DoToken (toElement, TokenString, FTagType, Params);
          ClearParams;
          FStage := stIdle;
        end
      else if aChar = '/' then
        begin
          case FStage of
            stTag :
              if length (TokenString) > 0 then
                begin
                  FTagType := ttEmpty;
                  FStage := stIgnore;
                end
              else
                FTagType := ttClosing;
            else
              begin
                FTagType := ttEmpty;
                FStage := stIgnore;
              end;
            end; // case
        end
      else
        begin
          case FStage of
            stIgnore :
              if aChar = '<' then
                begin
                  FStage := stName;
                  TokenString := '';
                  quoted := false;
                  ClearParams;
                  FParam := nil;
                end;
            stName :
              if aChar = '=' then
                begin
                  quoted := false;
                  FStage := stValue;
                end;
            stValue :
              if (aChar = ' ') and quoted then
                FParam.Value := FParam.Value + aChar
              else
                begin
                  FStage := stName;
                  FParam := nil;
                end;
            stTag : FStage := stName;
            end; // case
        end; // else
      aChar := GetChar;
    end;
  Buffer.Clear;
end;

procedure TXMLParser.DoToken (Token: TTokens; var TokenName: AnsiString;
  Tag: TTagTypes; Attributes: TList);
begin
  if Token = toElement then
    case Tag of
      ttOpening :
        begin
          FText := '';
          TextValue := false;
          TokenStack.Add (string (TokenName));
        end;
      ttClosing :
        if TokenStack.Count = 0 then
          begin
            DoError ('Orphaned Closing Tag ' + TokenName);
            TextValue := false;
          end
        else
          begin
            if CompareText (TokenStack[TokenStack.Count - 1], string (TokenName)) <> 0 then
              DoError (AnsiString ('Unmatched Closing Tag ' + string (TokenName) + ' vs ' + TokenStack[TokenStack.Count - 1]))
            else if TextValue then
              DoValue (TokenName, FText);
            FText := '';
            TokenStack.Delete (TokenStack.Count - 1);
            TextValue := false;
          end;
        ttEmpty : TextValue := false;
      end // case
  else if (Token = toText) and (TokenStack.Count > 0) then
    begin
      FText := TokenName;
      TextValue := true;
    end;
  try
    if Assigned (FOnToken) then
      FOnToken (Self, Token, TokenName, Tag, Attributes);
  except

  end;
  ClearParams;
  TokenName := '';
end;

procedure TXMLParser.DoValue (aName, aValue: AnsiString);
begin
  if Assigned (FOnValue) then
    FOnValue (Self, aName, aValue);
end;

function TXMLParser.GetChar: AnsiChar;
begin
  Result := #10;  //lf
  while Result = #10 do
    begin
      if Buffer.Position < Buffer.Size then
        Buffer.Read(Result, 1)
      else
        Result := #0;
    end;
end;

procedure TXMLParser.Parse (Data: AnsiString);
var
  loc : cardinal;
begin
  loc := Buffer.Position;
  Buffer.Seek (0, soFromEnd);
  Buffer.Write (Data[1], length (Data));
  Buffer.Seek (loc, soFromBeginning);
  DoParsing;
end;

procedure TXMLParser.Parse (Data: TStream);
var
  loc : cardinal;
begin
  loc := Buffer.Position;
  Buffer.Seek (0, soFromEnd);
  Data.Seek (0, soFromBeginning);
  Buffer.CopyFrom (Data, Data.Size);
  Buffer.Seek (loc, soFromBeginning);
  DoParsing;
end;

procedure TXMLParser.Reset;
begin
  Buffer.Clear;
  FStage := stIdle;
  FTagType := ttOpening;
  FParam := nil;
  TokenString := '';
  ClearParams;
end;
{$ENDREGION}
end.
