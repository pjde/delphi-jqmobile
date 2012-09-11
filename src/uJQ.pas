unit uJQ;

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

uses Classes, Messages, Graphics, OverbyteIcsHttpSrv, OverbyteIcsWSocketS,
    uWebSocket, uXMLParser;

(* To do
  Websockets

  TJQMWidgets
    Not yet implemented

  TJQMCustom

  TJQMLabel

  TJQMSwitch
    Need to increase width on long labels

  TJQMImage
    Events to be added

  TJQMMap
    Not yet implemented

  TJQMSlider

  TJQMRadioBtn

  TJQMKeypad

  TJQMRadioGroup
    Events to be finialised

  TJQMCheckGroup

  TJQMCheckBox

  TJQMButton

  TJQMListViewItem

  TJQMListView
    Collapsible to be finialised
    Pictures / icons etc to be added

  TJQMHeader
    Icons etc to be added

  TJQMFooter

  TJQMMenu

  TJQMPage
    Mulitple page support to be added
    Dialog pages to be fixed
*)

const
  JQSID               = 'JQSID';
  DefSessionLifeTime  = 0.5;    // half day
  ft : array [boolean] of string = ('false', 'true');
  CRLF                = #13#10;

type
  TJQM = class;
  TJQMPage = class;
  TJQMSession = class;
  TJQMResponse = class;

  TColor = integer;

  TJQMFontWeights = (fwNormal, fwBold, fwBolder, fwLighter);
  TJQMFontSizes = (fsXXSmall, fsXSmall, fsSmall, fsMedium, fsLarge, fsXLarge, fsXXLarge, fxSmaller, fxLarger);
  TJQMFontStyles = (fsNormal, fsItalic, fsOblique);
  TJQMJustifications = (juLeft, juCentre, juRight);
  TJQMBtnIcons = (biNone, biHome, biDelete, biPlus, biUPArrow, biDownArrow, biCheck, biGear, biGrid, biStar, biCustom, biRightArrow, biLeftArrow, biMinus, biRefresh, biForward, biBack, biAlert, biInfo, biSearch);
  TJQMIconPos = (ipLeft, ipRight, ipTop, ipBottom, ipNoText);

  TJQMGetPageEvent = procedure (Sender : TObject; Session : TJQMSession; Levels : TStringList; var aPage : TJQMPage) of object;
  TJQMClickEvent = procedure (Sender : TObject; Session : TJQMSession; Name : string; Params : TStringList; Response : TJQMResponse) of object;

  TJQMStateChangeEvent = procedure (Sender : TObject; Session : TJQMSession; Name : string; Index : integer; Value : boolean; Params : TStringList; Response : TJQMResponse) of object;
  TJQMValueChangeEvent = procedure (Sender : TObject; Session : TJQMSession; Name : string; Index : integer; Value : integer; Params : TStringList; Response : TJQMResponse) of object;
  TJQMTextChangeEvent = procedure  (Sender : TObject; Session : TJQMSession; Name : string; Index : integer; Value : string;  Params : TStringList; Response : TJQMResponse) of object;

  TJQMTimerEvent = procedure (Sender : TObject; Session : TJQMSession; Params : TStringList; Response : TJQMResponse) of object;
  TJQMSessionEvent = procedure (Sender : TObject; Session : TJQMSession) of object;
  TJQMMonEvent = procedure (Sender : TObject; Str : string) of object;

  TJQMResponseItem = class
    aType, aName, aValue : string;
    anIndex : integer;
  end;

  TJQMResponse = class
  private
    Items : TList;
    procedure AddItem (aType, aName, aValue : string); overload;
    procedure AddItem (aType, aName : string; anIndex : integer; aValue : string); overload;
  public
    function ToXml : string;
    function Empty : boolean;
    procedure Clear;
    procedure ChangeState (aName : string; aState : boolean); overload;
    procedure ChangeState (aName : string; anIndex : integer; aState : boolean); overload;
    procedure ChangeValue (aName : string; aValue : integer); overload;
    procedure ChangeValue (aName : string; anIndex : integer; aValue : integer); overload;
    procedure ChangeText (aName : string; aCaption : string); overload;
    procedure ChangeText (aName : string; anIndex : integer; aCaption : string); overload;
    procedure ChangePage (aName : string); overload;
    procedure ChangePage (aName, Options : string); overload;
    constructor Create;
    destructor Destroy; override;
  end;

  TJQMStyle = class
    Family : string;
    Colour : TColor;
    Weight : TJQMFontWeights;
    Size : TJQMFontSizes;
    Style : TJQMFontStyles;
    Justification : TJQMJustifications;
    procedure Assign (anOther : TJQMStyle); virtual;
    function FamilyParam : string;
    function ColourParam : string;
    function WeightParam : string;
    function SizeParam : string;
    function StyleParam : string;
    function JustificationParam : string;
    function Params : string;
    constructor Create;
    destructor Destroy; override;
  end;

  TJQMItem = class
    Theme : Char;
    Caption, Name : string;
    procedure Assign (anOther : TJQMItem); virtual;
    function Render : string; virtual;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TJQMItemClass = class of TJQMItem;

  TJQMCustomItem = class (TJQMItem)
    RenderString : string;
    procedure Assign (anOther : TJQMItem); override;
    function Render : string; override;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TJQMLabel = class (TJQMItem)
    Padding : integer;
    Style : TJQMStyle;
    Items : TStringList;
    procedure Assign (anOther : TJQMItem); override;
    function Render : string; override;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TJQMSwitch = class (TJQMItem)
    Labels : array [boolean] of string;
    procedure Assign (anOther : TJQMItem); override;
    function Render : string; override;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TJQMImage = class (TJQMItem)
    Source : string;
    Scale : integer;
//    Width, Height : integer;
    function Render : string; override;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TJQMMap = class (TJQMItem)
    function Render : string; override;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TJQMSlider = class (TJQMItem)
    Mini : Boolean;
    FillHL : Boolean;
    Step, Min, Max, Value : integer;
    procedure Assign (anOther : TJQMItem); override;
    function Render : string; override;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TJQMKeypad = class (TJQMItem)
    Pass : boolean;
    Style : TJQMStyle;
    TitleTheme : Char;
    OKTheme : Char;
    procedure Assign (anOther : TJQMItem); override;
    function Render : string; override;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TJQMRadioGroup = class (TJQMItem)
    Items : TStringList;
    Horizontal : boolean;
    procedure Add (aCaption : string; Checked : boolean);
    procedure Assign (anOther : TJQMItem); override;
    function Render : string; override;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TJQMCheckGroup = class (TJQMItem)
    Mini : Boolean;
    Items : TStringList;
    Horizontal : boolean;
    procedure Add (aCaption : string; Checked : boolean);
    procedure Assign (anOther : TJQMItem); override;
    function Render : string; override;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TJQMButton = class (TJQMItem)
    HRef : string;
    HRefAsDialog : Boolean;
    Corners : Boolean;
    Icon : TJQMBtnIcons;
    IconPos : TJQMIconPos;
    InLined, Mini, Shadow, IconShadow : Boolean;
    procedure Assign (anOther : TJQMItem); override;
    function Render : string; override;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TJQMListViewItem = class (TJQMItem)
    Divider : Boolean;
    HRef : string;
    ImageURL : string;
    HRefAsDialog : Boolean;
    procedure Assign (anOther : TJQMItem); override;
    function Render : string; override;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TJQMListView = class (TJQMItem)
    Items : TList;
    DividerTheme : char;
    Numbered : Boolean;
    Collapsible : Boolean;
    procedure Clear;
    function Add : TJQMListViewItem; overload;
    function Add (aCaption, aRef : string; aDivider : boolean) : TJQMListViewItem; overload;
    procedure Assign (anOther : TJQMItem); override;
    function Render : string; override;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TJQMHeader = class (TJQMItem)
    function Render : string; override;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TJQMFooter = class (TJQMItem)
    function Render : string; override;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TJQMMenu = class (TJQMItem)
    Items : TStringList;
    procedure Assign (anOther : TJQMItem); override;
    function Render : string; override;
    function Add (aString : string) : integer;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TJQMPage = class (TJQMItem)
  private
    FSessionInit : string;
    FOwner : TJQM;
    FParams : TStringList;
    function HTMLHead : string;
  public
    FItems : TList;
    FileName : string;
    procedure Assign (anOther : TJQMItem); override;
    function Render : string; override;
    procedure AddParam (aName, aValue : string);
    procedure AddItem (anItem : TJQMItem);
    function AddList : TJQMListView;
    function AddMenu (aName : string) : TJQMMenu;
    function AddSwitch (aName : string) : TJQMSwitch;
    function AddRadioGroup (aName : string) : TJQMRadioGroup;
    function AddCheckGroup (aName : string) : TJQMCheckGroup;
    function AddBtn (aName, aCaption : string) : TJQMButton;
    function AddSlider (aName, aCaption : string; aMin, aMax : integer) : TJQMSlider;
    function AddHeader (aName, aCaption : string) : TJQMHeader;
    function AddFooter (aName, aCaption : string) : TJQMFooter;
    function AddLabel (aName, aCaption : string) : TJQMLabel; overload;
    function AddLabel (aName : string) : TJQMLabel; overload;
    function AddCustom (HTML : string) : TJQMCustomItem;
    function AddImage (aName, anImage : string) : TJQMImage;
    function AddKeyPad (aName, aCaption : string; aPass : boolean) : TJQMKeypad;
    procedure Clear;
    constructor Create; overload; override;
    destructor Destroy; override;
  end;

  TJQMSession = class
  private
    FOwner : TJQM;
  public
    ID : string;
    Pin : string;
    UserAgent : string;
    Created : TDateTime;
    Host : string;
    Data : pointer;
    UsingWS : boolean; // using websockets
    function WebSocket : TWSClient;
    constructor Create;
    destructor Destroy; override;
  end;

  TJQM = class (TComponent)
  private
    FHTTPServer : THttpServer;
    FWSServer : TWSServer;
    FJSFolder : string;
    FHTTPPort, FWSPort : integer;
    FSessions : TList;
    FTimer : THandle;
    FSessionLifeTime : TDateTime;
    FOnStopped: TNotifyEvent;
    FOnStarted: TNotifyEvent;
    FOnStartError: TNotifyEvent;
    FOnShowPage : TJQMClickEvent;
    FOnGetPage: TJQMGetPageEvent;
    FOnTapped: TJQMClickEvent;
    FOnTapHeld: TJQMClickEvent;
    FOnSwiped : TJQMClickEvent;

    FOnStateChanged : TJQMStateChangeEvent;
    FOnValueChanged: TJQMValueChangeEvent;
    FOnTextChanged : TJQMTextChangeEvent;

    FOnTimer : TJQMTimerEvent;
    FOnMon : TJQMMonEvent;
    FOnSessionDestroy : TJQMSessionEvent;
    FOnSessionCreate : TJQMSessionEvent;
    FOnSessionChanged : TJQMSessionEvent;
    function NewSessionID : string;
    function GetSession (byID : string) : TJQMSession;

    procedure ServerStarted (Sender: TObject);
    procedure ServerStopped (Sender: TObject);
    procedure GetDocument (Sender, Client: TObject; var Flags: THttpGetFlag);
    procedure PostDocument (Sender, Client: TObject; var Flags: THttpGetFlag);

    procedure WSNewClient (Sender: TObject; Client: TWSClient);
    procedure WSFinishClient (Sender: TObject; Client: TWSClient);
    procedure WSMon (Sender : TObject; Str : string);
    procedure WSText (Sender : TObject; Client: TWSClient; Msg: TMemoryStream);
    procedure WSToken (Sender : TObject; Token : TTokens; TokenName : AnsiString;
                              Tag : TTagTypes; Params : TList);
    procedure WSError (Sender: TObject; ErrorMsg : AnsiString);
    procedure TimerProc (var aMsg : TMessage);
  public
    AdditionalStyles, AdditionalScripts : TStringList;
    constructor Create (anOwner : TComponent); override;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    procedure Mon (aStr : string);
    function GetWS (byID : string) : TWSClient;
    property Sessions : TList read FSessions;
    property WSServer : TWSServer read FWSServer;
  published
    property HTTPPort : integer read FHTTPPort write FHTTPPort;
    property WSPort : integer read FWSPort write FWSPort;
    property SessionLifeTime : TDateTime read FSessionLifeTime write FSessionLifeTime;
    property JSFolder : string read FJSFolder write FJSFolder;
    property OnStarted : TNotifyEvent read FOnStarted write FOnStarted;
    property OnStartError : TNotifyEvent read FOnStartError write FOnStartError;
    property OnStopped : TNotifyEvent read FOnStopped write FOnStopped;
    property OnGetPage : TJQMGetPageEvent read FOnGetPage write FOnGetPage;
    property OnShowPage : TJQMClickEvent read FOnShowPage write FOnShowPage;
    property OnTapped : TJQMClickEvent read FOnTapped write FOnTapped;
    property OnTapHeld : TJQMClickEvent read FOnTapHeld write FOnTapHeld;
    property OnStateChanged : TJQMStateChangeEvent read FOnStateChanged write FOnStateChanged;
    property OnValueChanged : TJQMValueChangeEvent read FOnValueChanged write FOnValueChanged;
    property OnTextChanged : TJQMTextChangeEvent read FOnTextChanged write FOnTextChanged;
    property OnSwiped : TJQMClickEvent read FOnSwiped write FOnSwiped;
    property OnTimer : TJQMTimerEvent read FOnTimer write FOnTimer;
    property OnSessionCreate : TJQMSessionEvent read FOnSessionCreate write FOnSessionCreate;
    property OnSessionDestroy : TJQMSessionEvent read FOnSessionDestroy write FOnSessionDestroy;
    property OnSessionChanged :  TJQMSessionEvent read FOnSessionChanged write FOnSessionChanged;
    property OnMon : TJQMMonEvent read FOnMon write FOnMon;
  end;

procedure Register;

implementation

uses SysUtils, Windows;

var
  PageID : integer = 0;

procedure Register;
begin
  RegisterComponents ('JQMobile', [TJQM]);
end;

function DisplayStr (aStr : string) : string;
var
  i : integer;
begin
  Result := '';
  for i := 1 to length (aStr) do
    if CharInSet (astr[i], [' '..'z']) then
      Result := Result + aStr[i]
    else
      Result := Result + '<' + IntToStr (ord (aStr[i])) + '>';
end;

function HTMLColour (aColour : TColor) : string;
begin
  Result := '#' +
            IntToHex (aColour and $ff, 2) +
            IntToHex ((aColour and $ff00) shr 8, 2) +
            IntToHex ((aColour and $ff0000) shr 16, 2);
end;

{ TJQM }

constructor TJQM.Create (anOwner : TComponent);
begin
  inherited;
  FHTTPPort := 80;
  FWSPort := 8002;
  FJSFolder := 'wwwroot';
  AdditionalStyles := TStringList.Create;
  AdditionalScripts := TStringList.Create;
  FSessions := TList.Create;
  FSessionLifeTime := DefSessionLifeTime;
  FTimer := AllocateHWnd (TimerProc);
  FHTTPServer := THttpServer.Create (nil);
  FHTTPServer.OnServerStarted := ServerStarted;
  FHTTPServer.OnServerStopped := ServerStopped;
  FHTTPServer.OnGetDocument := GetDocument;
  FHTTPServer.OnPostDocument := PostDocument;
  FWSServer := TWSServer.Create (nil);
  FWSServer.AutoPong := true;
  FWSServer.OnNewClient := WSNewClient;
  FWSServer.OnFinishClient := WSFinishClient;
  FWSServer.OnMon := WSMon;
  FWSServer.OnText := WSText;
end;

destructor TJQM.Destroy;
var
  i :  integer;
begin
  DeAllocateHWnd (FTimer);
  FHTTPServer.Stop;
  FWSServer.Stop;
  AdditionalStyles.Free;
  AdditionalScripts.Free;
  for i := 0 to FSessions.Count - 1 do
    begin
      if Assigned (FOnSessionDestroy) then FOnSessionDestroy (Self, TJQMSession (FSessions[i]));
      if Assigned (FOnSessionChanged) then FOnSessionChanged (Self, TJQMSession (FSessions[i]));
      TJQMSession (FSessions[i]).Free;
    end;
  FSessions.Free;
  FHTTPServer.Free;
  FWSServer.Free;
  inherited;
end;

procedure TJQM.GetDocument (Sender, Client: TObject; var Flags: THttpGetFlag);
var
  aClient  : THttpConnection;
  x, y : integer;
  b : boolean;
  Ext, Tmp, Cookie, Tmp2, Tmp3 : string;
  ResponseCookie : string;
  Params : TStringList;
  aPage : TJQMPage;
  aSession : TJQMSession;
  Levels : TStringList;
  aResponse : TJQMResponse;
begin
  if Flags = hg401 then exit;
  aClient := THttpConnection (Client);
  Tmp := aClient.Path;
//  Mon ('Get ' + Tmp);
  // Session Control
  aSession := nil;
  ResponseCookie := '';
  if GetCookieValue (aClient.RequestCookies, JQSID, Cookie) then
    aSession := GetSession (Cookie)
  else
    Cookie := '';
  if aSession = nil  then
    begin
      aSession := TJQMSession.Create;
      aSession.FOwner := Self;
      if Cookie = '' then
        begin
          aSession.ID := NewSessionID;
          if FSessionLifeTime > 0 then
            ResponseCookie := MakeCookie (JQSID, aSession.ID, Now + FSessionLifeTime, '/')
          else
            ResponseCookie := MakeCookie (JQSID, aSession.ID, 0, '/');
        end
      else
        aSession.ID := Cookie;
      aSession.UserAgent := aClient.RequestUserAgent;
      aSession.Host := aClient.GetPeerAddr;
      FSessions.Add (aSession);
      if Assigned (FOnSessionCreate) then FOnSessionCreate (aClient, aSession);
      if Assigned (FOnSessionChanged) then FOnSessionChanged (Self, aSession);
      if (FSessions.Count = 1) and (FSessionLifeTime > 0) then SetTimer (FTimer, 1, 60000, nil);
    end;

  Ext := UpperCase (ExtractFileExt (Tmp));
  // CGI (AJAX) requests
  if (Ext = '.CGI') then
    begin
      Tmp := UpperCase (Copy (Tmp, 1, Length (Tmp) - Length (Ext)));  // remobe ext
//      Mon ('CGI request ' + Tmp + ' Params ' + aClient.Params);
      if not ExtractURLEncodedValue (aClient.Params, 'Name', Cookie) then
        Cookie := '';
      Params := TStringList.Create;
      Tmp2 := aClient.Params;
      x := Pos ('&', Tmp2);
      while (x > 0) do
        begin
          Params.Add (URLDecode (Copy (Tmp2, 1, x - 1)));
          Tmp2 := Copy (Tmp2, x + 1, length (Tmp2) - x);
          x := Pos ('&', Tmp2);
        end;
      Params.Add (URLDecode (Tmp2));
      aResponse := TJQMResponse.Create;
      if (Tmp = '/PAGE') and Assigned (FOnShowPage) then
        FOnShowPage (Self, aSession, Cookie, Params, aResponse)
      else if (Tmp = '/TAPPED') and Assigned (FOnTapped) then
        FOnTapped (Self, aSession, Cookie, Params, aResponse)
      else if (Tmp = '/TAPHELD') and Assigned (FOnTapHeld) then
        FOnTapHeld (Self, aSession, Cookie, Params, aResponse)
      else if (Tmp = '/SWIPED') and Assigned (FOnSwiped) then
        FOnSwiped (Self, aSession, Cookie, Params, aResponse)
      else if (Tmp = '/STATECHANGED') and Assigned (FOnStateChanged) then
        begin
          if ExtractURLEncodedValue (aClient.Params, 'Value', Tmp2) then
            b := CompareText (Tmp2, 'on') = 0
          else
            b := false;
          if ExtractURLEncodedValue (aClient.Params, 'Index', Tmp2) then
            y := StrToIntDef (Tmp2, 0)
          else
            y := 0;
          FOnStateChanged (Self, aSession, Cookie, y, b, Params, aResponse)
        end
      else if (Tmp = '/VALUECHANGED') and Assigned (FOnValueChanged) then
        begin
          if ExtractURLEncodedValue (aClient.Params, 'Value', Tmp2) then
            x := StrToIntDef (Tmp2, 0)
          else
            x := 0;
          if ExtractURLEncodedValue (aClient.Params, 'Index', Tmp2) then
            y := StrToIntDef (Tmp2, 0)
          else
            y := 0;
          FOnValueChanged (Self, aSession, Cookie, y, x, Params, aResponse);
        end
      else if (Tmp = '/TEXTCHANGED') and Assigned (FOnTextChanged) then
        begin
          if not ExtractURLEncodedValue (aClient.Params, 'Value', Tmp2) then
            Tmp2 := '';
          if ExtractURLEncodedValue (aClient.Params, 'Index', Tmp3) then
            y := StrToIntDef (Tmp3, 0)
          else
            y := 0;
          FOnTextChanged (Self, aSession, Cookie, y, Tmp2, Params, aResponse)
        end
      else if (Tmp = '/TIMER') and Assigned (FOnTimer) then
        FOnTimer (Self, aSession, Params, aResponse);
      Params.Free;
      aClient.AnswerString (Flags, '', '', '', aResponse.ToXml);
      aResponse.Free;
    end
  else if (Ext = '') or (Ext = '.HTML') then
    begin
      Tmp := Copy (Tmp, 1, Length (Tmp) - Length (Ext));  // remove ext
      Levels := TStringList.Create;
      if Length (Tmp) = 0 then
        Levels.Add('')
      else
        begin
          x := Pos ('/', Tmp);
          while (x > 0) do
            begin
              Levels.Add (URLDecode (Copy (Tmp, 1, x - 1)));
              Tmp := Copy (Tmp, x + 1, length (Tmp) - x);
              x := Pos ('/', Tmp);
            end;
          Levels.Add (URLDecode (Tmp));
        end;
      aPage := nil;
      if Assigned (FOnGetPage) then FOnGetPage (Self, aSession, Levels, aPage);
      if aPage = nil then
        Flags := hg404
      else
        begin
          aPage.FOwner := Self;
          aPage.FSessionInit := aClient.GetXAddr + ':' + IntToStr (FWSPort);
          Tmp := aPage.Render;
          if Length (Tmp) > 0 then
            aClient.AnswerString (Flags, '', '', ResponseCookie, Tmp)
          else
            Flags := hg404;
          aPage.Free;
        end;
      Levels.Free;
    end;
  if Flags = hgSendDoc then
    begin
      if FileExists (aClient.Document) then
        begin
          Flags := hgWillSendMySelf;
          aClient.SendDocument (httpSendDoc, ResponseCookie);
        end
      else
        Flags := hg404;
    end;
end;

procedure TJQM.PostDocument (Sender, Client: TObject;
  var Flags: THttpGetFlag);
var
  aClient  : THttpConnection;
  Tmp : string;
begin
  if Flags = hg401 then exit;
  aClient := THttpConnection (Client);
  Tmp := aClient.Path;
  Mon ('POST ' + Tmp);
end;

function TJQM.GetSession (byID: string): TJQMSession;
var
  i : integer;
begin
  for i := 0 to FSessions.Count - 1 do
    begin
      Result := FSessions[i];
      if Result.ID = byID then exit;
    end;
  Result := nil;
end;

function TJQM.GetWS (byID: string): TWSClient;
var
  i : integer;
begin
  for i := 0 to FWSServer.Server.ClientCount - 1 do
    begin
      Result := TWSClient (FWSServer.Server.Client[i]);
      if Result.ID = byID then exit;
    end;
  Result := nil;
end;

procedure TJQM.Mon (aStr: string);
begin
  if Assigned (FOnMon) then FOnMon (Self, aStr);
end;

function TJQM.NewSessionID: string;
var
  i : integer;
  x : double;
  p : PByte;
begin
  x := Now;
  p := @x;
  Result := IntToHex (Random (maxint), 8);
  for i := 1 to sizeof (x) - 4 do
    begin
      Result := Result + IntToHex (p^, 2);
      inc (p);
    end;
end;

procedure TJQM.ServerStarted (Sender: TObject);
begin
  if Assigned (FOnStarted) then FOnStarted (Self);
end;

procedure TJQM.ServerStopped (Sender: TObject);
begin
  if Assigned (FOnStopped) then FOnStopped (Self);
end;

procedure TJQM.Start;
begin
  Mon ('Starting Servers');
  FWSServer.Port := FWSPort;
  FWSServer.Start;
  FHTTPServer.DocDir := FJSFolder;
  FHTTPServer.Port := IntToStr (FHTTPPort);
  try
    FHTTPServer.Start;
  except
    Mon ('Error Starting HTTP Server.');
    if Assigned (FOnStartError) then FOnStartError (Self);
  end;
end;

procedure TJQM.Stop;
begin
  FWSServer.Stop;
  FHTTPServer.Stop;
end;

procedure TJQM.TimerProc (var aMsg: TMessage);
var
  i : integer;
  aSession : TJQMSession;
  n : TDateTime;
begin
  if aMsg.Msg <> WM_TIMER then exit;
  case aMsg.WParam of
    1 :
      begin
        n := Now;
        for i := FSessions.Count - 1 downto 0 do
          begin
            aSession := FSessions[i];
            if aSession.Created + FSessionLifeTime < n  then
              begin
                mon ('Deleting Expired Session.');
                if Assigned (FOnSessionDestroy) then FOnSessionDestroy (Self, aSession);
                if Assigned (FOnSessionChanged) then FOnSessionChanged (Self, aSession);
                aSession.Free;
                FSessions.Delete (i);
              end;
          end;
        if FSessions.Count = 0 then KillTimer (FTimer, 1);
      end;
  end;
end;

procedure TJQM.WSNewClient (Sender: TObject; Client: TWSClient);
begin
  Client.Data := TXMLParser.Create (Client);
  with TXMLParser (Client.Data) do
    begin
      OnToken := WSToken;
      OnError := WSError;
    end;
end;

procedure TJQM.WSFinishClient (Sender: TObject; Client: TWSClient);
var
  aSession : TJQMSession;
begin
  if Assigned (Client.Data) then Client.Data.Free;
  aSession := GetSession (Client.ID);
  if aSession <> nil then
    begin
      aSession.UsingWS := false;
      if Assigned (FOnSessionChanged) then FOnSessionChanged (Self, aSession);
    end;
end;

procedure TJQM.WSError (Sender: TObject; ErrorMsg: AnsiString);
begin
//  Mon ('WS Error ' + string (ErrorMsg));
end;

procedure TJQM.WSMon (Sender: TObject; Str: string);
begin
//  Mon ('WS ' + Str);
end;

procedure TJQM.WSText (Sender: TObject; Client: TWSClient; Msg: TMemoryStream);
begin
  if Assigned (Client.Data) then
    TXMLParser (Client.Data).Parse (Msg);
end;

procedure TJQM.WSToken (Sender: TObject; Token: TTokens; TokenName: AnsiString;
  Tag: TTagTypes; Params: TList);
var
  i, y : integer;
  s, v : string;
  aParam : TParam;
  aClient : TWSClient;
  aSession : TJQMSession;
  aList : TStringList;
  aResponse : TJQMResponse;
begin
  aClient := nil;
  if Sender is TXMLParser then
    aClient := TWSClient (TXMLParser (Sender).Owner);
  TokenName := AnsiString (UpperCase (string (TokenName)));
//  Mon ('WS Token ' + string (TokenName));
  for i := 0 to Params.Count - 1 do
    begin
      aParam := Params[i];
      aParam.Name := AnsiString (UpperCase (string (aParam.Name)));
//      Mon (string (aParam.Name) + ' = ' + string (aParam.Value));
    end;
  if aClient = nil then exit;
  if TokenName = 'SESSION' then
    begin
      for i := 0 to Params.Count - 1 do
        begin
          aParam := Params[i];
          if aParam.Name = 'ID' then
            aClient.ID := string (aParam.Value);
        end;
      aSession := GetSession (aClient.ID);
      if aSession <> nil then
        begin
          aSession.UsingWS := true;
          if Assigned (FOnSessionChanged) then FOnSessionChanged (Self, aSession);
        end;
//      Mon ('FID ' + aClient.FID);
    end
  else if (TokenName = 'TAPPED') or (TokenName = 'TAPHELD') or
          (TokenName = 'SWIPED') or (TokenName = 'PAGE') or
          (TokenName = 'STATECHANGED') or (TokenName = 'VALUECHANGED') or
          (TokenName = 'TEXTCHANGED') then
    begin
      s := '';
      v := '0';
      y := -1;
      aList := TStringList.Create;
      for i := 0 to Params.Count - 1 do
        begin
          aParam := Params[i];
          if aParam.Name = 'NAME' then
            s := string (aParam.Value)
          else
            aList.Add (string (aParam.Name) + '=' + string (aParam.Value));
          if aParam.Name = 'VALUE' then
            v := string (aParam.Value)
          else if aParam.Name = 'INDEX' then
            y := StrToIntDef (string (aParam.Value), -1);
        end;
      aSession := GetSession (aClient.ID);
      aResponse := TJQMResponse.Create;
      if (TokenName = 'TAPPED') and Assigned (FOnTapped) then
        FOnTapped (Self, aSession, s, aList, aResponse)
      else if (TokenName = 'TAPHELD') and Assigned (FOnTapHeld) then
        FOnTapHeld (Self, aSession, s, aList, aResponse)
      else if (TokenName = 'SWIPED') and Assigned (FOnSwiped) then
        FOnSwiped (Self, aSession, s, aList, aResponse)
      else if (TokenName = 'PAGE') and Assigned (FOnShowPage) then
        FOnShowPage (Self, aSession, s, aList, aResponse)
      else if (TokenName = 'STATECHANGED') and Assigned (FOnStateChanged) then
        FOnStateChanged (Self, aSession, s, y, CompareText (v, 'on') = 0, aList, aResponse)
      else if (TokenName = 'VALUECHANGED') and Assigned (FOnValueChanged) then
        FOnValueChanged (Self, aSession, s, y, StrToIntDef (v, 0), aList, aResponse)
      else if (TokenName = 'TEXTCHANGED') and Assigned (FOnTextChanged) then
        FOnTextChanged (Self, aSession, s, y, v, aList, aResponse);
      if not aResponse.Empty then
        aClient.SendString (AnsiString (aResponse.ToXml));
      aResponse.Free;
      aList.Free;
    end;
end;

{ TJQMPage }

function TJQMPage.AddBtn (aName, aCaption: string): TJQMButton;
begin
  Result := TJQMButton.Create;
  Result.Name := aName;
  Result.Caption := aCaption;
  AddItem (Result);
end;

function TJQMPage.AddCheckGroup (aName: string): TJQMCheckGroup;
begin
  Result := TJQMCheckGroup.Create;
  Result.Name := aName;
  AddItem (Result);
end;

function TJQMPage.AddCustom (HTML: string): TJQMCustomItem;
begin
  Result := TJQMCustomItem.Create;
  Result.RenderString := HTML;
  AddItem (Result);
end;

function TJQMPage.AddFooter (aName, acaption: string): TJQMFooter;
begin
  Result := TJQMFooter.Create;
  Result.Name := aName;
  Result.Caption := aCaption;
  AddItem (Result);
end;

function TJQMPage.AddHeader (aName, aCaption: string): TJQMHeader;
begin
  Result := TJQMHeader.Create;
  Result.Name := aName;
  Result.Caption := aCaption;
  AddItem (Result);
end;

function TJQMPage.AddSlider (aName, aCaption: string; aMin,
  aMax: integer): TJQMSlider;
begin
  Result := TJQMSlider.Create;
  Result.Name := aName;
  Result.Caption := aCaption;
  Result.Min := aMin;
  Result.Max := aMax;
  Result.Value := (aMax + aMin) div 2;
  AddItem (Result);
end;

function TJQMPage.AddSwitch (aName: string): TJQMSwitch;
begin
  Result := TJQMSwitch.Create;
  Result.Name := aName;
  AddItem (Result);
end;

procedure TJQMPage.Assign (anOther: TJQMItem);
var
  i : integer;
  anItem, bItem : TJQMItem;
  aPage : TJQMPage;
begin
  inherited;
  for i := 0 to FItems.Count - 1 do
    TJQMItem (FItems[i]).Free;
  FItems.Clear;
  if not (anOther is TJQMPage) then exit;
  aPage := TJQMPage (anOther);
  FileName := aPage.FileName;
  FParams.Assign (aPage.FParams);
  for i := 0 to aPage.FItems.Count - 1 do
    begin
      anItem := aPage.FItems[i];
      bItem := TJQMItemClass (anItem.ClassType).Create;
      bItem.Assign (anItem);
      FItems.Add (anItem);
    end;
end;

function TJQMPage.AddImage(aName, anImage: string): TJQMImage;
begin
  Result := TJQMImage.Create;
  Result.Name := aName;
  Result.Source := anImage;
  Result.Scale := 100;
  AddItem (Result);
end;

procedure TJQMPage.AddItem (anItem: TJQMItem);
var
  i : integer;
  bItem : TJQMItem;
begin
  if (anItem is TJQMHeader) and (FItems.Count > 0) then // ensure all headers are at top of page
    begin
      for i := 0 to FItems.Count - 1 do
        begin
          bItem := FItems[i];
          if not (bItem Is TJQMHeader) then
            begin
              FItems.Insert (i, anItem);
              exit;
            end;
        end;
    end;
  FItems.Add (anItem);
end;

function TJQMPage.AddKeyPad (aName, aCaption: string;
  aPass: boolean): TJQMKeyPad;
begin
  Result := TJQMKeyPad.Create;
  Result.Name := aName;
  Result.Caption := aCaption;
  Result.Pass := aPass;
  AddItem (Result);
end;

function TJQMPage.AddLabel (aName: string): TJQMLabel;
begin
  Result := TJQMLabel.Create;
  Result.Name := aName;
  AddItem (Result);
end;

function TJQMPage.AddList: TJQMListView;
begin
  Result := TJQMListView.Create;
  AddItem (Result);
end;

function TJQMPage.AddMenu (aName : string) : TJQMMenu;
begin
  Result := TJQMMenu.Create;
  Result.Name := aName;
  AddItem (Result);
end;

function TJQMPage.AddLabel (aName, aCaption: string): TJQMLabel;
begin
  Result := TJQMLabel.Create;
  Result.Name := aName;
  Result.Items.Add (aCaption);
  AddItem (Result);
end;

procedure TJQMPage.AddParam (aName, aValue: string);
begin
  FParams.Add (aName + '=' + aValue);
end;

function TJQMPage.AddRadioGroup (aName: string): TJQMRadioGroup;
begin
  Result := TJQMRadioGroup.Create;
  Result.Name := aName;
  AddItem (Result);
end;

procedure TJQMPage.Clear;
var
  i : integer;
begin
  FParams.Clear;
  for i := 0 to FItems.Count - 1 do
    TJQMItem (FItems[i]).Free;
  FItems.Clear;
end;

constructor TJQMPage.Create;
begin
  inherited;
  FOwner := nil;
  Caption := '';
  FileName := '';
  FSessionInit := '';
  FItems := TList.Create;
  FParams := TStringList.Create;
end;

destructor TJQMPage.Destroy;
begin
  Clear;
  FItems.Free;
  FParams.Free;
  inherited;
end;

function TJQMPage.Render: string;
var
  i, x : integer;
  n,v :string;
  anItem : TJQMItem;
  aFile : TFileStream;
  aStr : TStringStream;
begin
  if Length (FileName) > 0 then // copy static page
    begin
      if FileExists (FileName) then
        try
          aFile := TFileStream.Create (FileName, fmOpenRead);
          aStr := TStringStream.Create;
          aStr.CopyFrom (aFile, 0);
          Result := aStr.DataString;
          aStr.Free;
          aFile.Free;
        except
          Result := '';
        end;
    end
  else   // generate from list
    begin
      Result := '';
      PageID := PageID + 1;
      if PageID > 5000 then PageID := 0;
      if (FOwner <> nil) then   // default page
        begin
          Result :=
            '<!DOCTYPE HTML>'#10 +
            '<html>'#10 +
            HTMLHead +
              #9'<body onload="SessionInit (''' + FSessionInit + ''')">'#10;
        end;
      Result := Result +
        format (#9#9'<div id="page%d" data-role="page">'#10, [PageID]);
      if (FOwner <> nil) then   // default page
        begin
          for i := 0 to FParams.Count - 1 do
            begin
              x := Pos ('=', FParams[i]);
              n := Copy (FParams[i], 1, x - 1);
              v := Copy (FParams[i], x + 1, length (FParams[i]) - x);
              Result := Result + format (#9#9#9'<input name="%s" type="hidden" value="%s">'#10, [n, v]);
            end;
        end;
      for i := 0 to FItems.Count - 1 do
        begin
          anItem := FItems[i];
          Result := Result + anItem.Render;
        end;
      Result := Result +
        #9#9'</div> <!-- End of Page --->'#10; // end of page
      if (FOwner <> nil) then   // default page
        begin
          Result := Result +
            #9#9'<script language="javascript" type="text/javascript">'#10 +
              #9#9#9'$().ready (function ()'#10 +
              #9#9#9'{'#10 +
                #9#9#9#9'$("[data-role=page]").live ("pageshow", function (event, ui)'#10 +
                #9#9#9#9'{'#10 +
                  #9#9#9#9#9'PageSetup (this);'#10 +
                #9#9#9#9'});'#10 +
              #9#9#9'});'#10 +
            #9#9'</script>'#10;
          Result := Result +
            #9'</body>'#10 +
          '</html>'#10;
        end;
    end;
end;

function TJQMPage.HTMLHead : string;
var
  i : integer;
begin
  Result :=
    #9'<head>'#10 +
	    #9#9'<meta charset="utf-8">'#10 +
	    #9#9'<meta name="viewport" content="width=device-width, initial-scale=1">'#10 +
      #9#9'<title>' + Caption + '</title>'#10 +
      #9#9'<link rel="stylesheet" href="/jquery.mobile-1.1.0.min.css" />'#10 +
      #9#9'<link rel="stylesheet" href="/jqm-default.css"/>'#10;
  if FOwner <> nil then
    for i := 0 to FOwner.AdditionalStyles.Count - 1 do
      Result := Result +
        #9#9'<link rel="stylesheet" href="' + FOwner.AdditionalStyles[i] + '"/>'#10;
  Result := Result +
      #9#9'<script src="/jquery-1.7.1.min.js"></script>'#10 +
      #9#9'<script src="/misc.logic.js"></script>'#10;
  if FOwner <> nil then
    for i := 0 to FOwner.AdditionalScripts.Count - 1 do
      Result := Result +
        #9#9'<script src="' + FOwner.AdditionalScripts[i] + '"></script>'#10;
  Result := Result +
      #9#9'<script src="/jquery.mobile-1.1.0.min.js"></script>'#10 +
      #9#9'<script language="javascript" type="text/javascript"> <!--- Session Initialisation  --->'#10 +
//        #9#9#9'SessionInit ("' + FSessionInit + '");'#10 +
      #9#9'</script>'#10 +
    #9'</head>'#10;
end;

{ TJQMItem }

procedure TJQMItem.Assign (anOther: TJQMItem);
begin
  Theme := anOther.Theme;
  Caption := anOther.Caption;
  Name := anOther.Name;
end;

constructor TJQMItem.Create;
begin
  Caption := '';
  Name := '';
  Theme := 'a';
end;

destructor TJQMItem.Destroy;
begin
  //                  s
  inherited;
end;

function TJQMItem.Render: string;
begin
  Result := '';
end;

{ TJQMListViewItem }

procedure TJQMListViewItem.Assign (anOther: TJQMItem);
var
  anItem : TJQMListViewItem;
begin
  inherited;
  if not (anOther is TJQMListViewItem) then exit;
  anItem := TJQMListViewItem (anOther);
  Divider := anItem.Divider;
  HRef := anItem.HRef;
  HRefAsDialog := anItem.HRefAsDialog;
end;

constructor TJQMListViewItem.Create;
begin
  inherited;
  Divider := false;
  HRef := '';
  HRefAsDialog := false;
  ImageURL := '';
end;

destructor TJQMListViewItem.Destroy;
begin
  inherited;
end;

function TJQMListViewItem.Render: string;
var
  Tmp : string;
begin
  if length (ImageURL) > 0 then
    Tmp := format ('<img src="%s">', [ImageURL])
  else
    Tmp := '';
  if Divider then
    Result := format (#9#9#9#9'<li data-role="list-divider">%s</li>'#10, [Caption])
  else
    begin
      if HRefAsDialog then
        Result := format (#9#9#9#9'<li><a href="%s" data-rel="dialog">%s%s</a></li>'#10, [HRef, Tmp, Caption])
      else
        Result := format (#9#9#9#9'<li><a href="%s">%s%s</a></li>'#10, [HRef, Tmp, Caption]);
    end;
end;

{ TJQMListView }

function TJQMListView.Add : TJQMListViewItem;
begin
  Result := TJQMListViewItem.Create;
  Items.Add (Result);
end;

function TJQMListView.Add (aCaption, aRef: string;
  aDivider: boolean): TJQMListViewItem;
begin
  Result := TJQMListViewItem.Create;
  Result.Divider := aDivider;
  Result.HRef := aRef;
  Result.Caption := aCaption;
  Items.Add (Result);
end;

procedure TJQMListView.Assign (anOther: TJQMItem);
var
  i : integer;
  anItem, bItem : TJQMListViewItem;
  aView : TJQMListView;
begin
  inherited;
  if not (anOther is TJQMListView) then exit;
  aView := TJQMListView (anOther);
  DividerTheme := aView.DividerTheme;
  Numbered := aView.Numbered;
  Collapsible := aView.Collapsible;
  Clear;
  for i := 0 to aView.Items.Count - 1 do
    begin
      anItem := aView.Items[i];
      bItem := aView.Add;
      bItem.Assign (anItem);
    end;
end;

procedure TJQMListView.Clear;
var
  i : integer;
begin
  for i := 0 to Items.Count - 1 do
    TJQMListViewItem (Items[i]).Free;
  Items.Clear
end;

constructor TJQMListView.Create;
begin
  inherited;
  Theme := 'c';
  DividerTheme := 'b';
  Numbered := false;
  Collapsible := false;
  Items := TList.Create;
end;

destructor TJQMListView.Destroy;
begin
  Clear;
  Items.Free;
  inherited;
end;

function TJQMListView.Render: string;
var
  i : integer;
  elem : string;
  flag : boolean;
begin
  Result := '';
  flag := false;
  if Numbered then elem := 'ol' else elem := 'ul';

  if Collapsible then
    begin
      for i := 0 to Items.Count - 1 do
        with TJQMListViewItem (Items[i]) do
          begin
            if Divider then
              begin
                if flag then
                  Result := Result + #9#9#9'</div> <!--- End of Collapsible xx--->'#10;

                  Result := Result + format (#9#9#9'<div data-role="collapsible" data-theme="%s" data-content-theme="%s" >'#10,
                              [Self.Theme, Theme]);
                  Result := Result + #9#9#9'<h3>' + Caption + '</h3>'#10;
                flag := true;
              end
            else
              begin
                if HRefAsDialog then
                  Result := Result + format (#9#9#9'<ul data-role="listview" href="%s" data-rel="dialog">%s</ul>'#10, [HRef, Caption])
                else
                  Result := Result + format (#9#9#9'<ul data-role="listview" href="%s">%s</ul>'#10, [HRef, Caption]);
              end;
          end;
      if flag then
        Result := Result + #9#9#9'</div> <!--- End of Collapsible --->'#10;
    end
  else
    begin
      Result := Result + format (#9#9#9'<%s data-role="listview" id="%s" data-theme="%s">'#10,
        [elem, Name, Theme]);
      for i := 0 to Items.Count - 1 do
        Result := Result + TJQMListViewItem (Items[i]).Render;
      Result := Result + format (#9#9#9'</%s> <!--- End of ListView --->'#10,
        [elem]);
    end;
   //      	<div data-role="collapsible" data-collapsed="true" data-theme="b" data-content-theme="d">
end;

{ TJQMHeader }

constructor TJQMHeader.Create;
begin
  inherited;
end;

destructor TJQMHeader.Destroy;
begin
//
  inherited;
end;

function TJQMHeader.Render: string;
begin
  Result :=
    format (#9#9#9'<div id="%s" reftype="header" data-role="header" data-theme="%s">', [Name, Theme]) +
	  format ('<h1>%s</h1>', [Caption]) + '</div> <!--- End of Header --->'#10;
end;

{ TJQMButton }

procedure TJQMButton.Assign (anOther: TJQMItem);
var
  aBtn : TJQMButton;
begin
  inherited;
  if not (anOther is TJQMButton) then exit;
  aBtn := TJQMButton (anOther);
  HRef := aBtn.HRef;
  HRefAsDialog := aBtn.HRefAsDialog;
  Corners := aBtn.Corners;
  Icon := aBtn.Icon;
  IconPos := aBtn.IconPos;
  InLined := aBtn.InLined;
  Mini := aBtn.Mini;
  Shadow := aBtn.Shadow;
  IconShadow := aBtn.IconShadow;
end;

constructor TJQMButton.Create;
begin
  inherited;
  HRef := '';
  HRefAsDialog := false;
  Corners := true;
  Shadow := true;
  Inlined := false;
  IconPos := ipLeft;
  Icon := biNone;
  IconShadow := true;
  Mini := false;
  Theme := 'c';
end;

destructor TJQMButton.Destroy;
begin
  inherited;
end;

function TJQMButton.Render: string;
var
  options : string;
begin
  options := '';
  if not Corners then options := options + ' data-corners="false"';
  case Icon of
    biNone       :;
    biHome       : options := options + ' data-icon="home"';
    biDelete     : options := options + ' data-icon="delete"';
    biPlus       : options := options + ' data-icon="plus"';
    biUPArrow    : options := options + ' data-icon="arrow-u"';
    biDownArrow  : options := options + ' data-icon="arrow-d"';
    biCheck      : options := options + ' data-icon="checl"';
    biGear       : options := options + ' data-icon="gear"';
    biGrid       : options := options + ' data-icon="grid"';
    biStar       : options := options + ' data-icon="star"';
    biCustom     : options := options + ' data-icon="custom"';
    biRightArrow : options := options + ' data-icon="arrow-r"';
    biLeftArrow  : options := options + ' data-icon="arrow-l"';
    biMinus      : options := options + ' data-icon="minus"';
    biRefresh    : options := options + ' data-icon="refresh"';
    biForward    : options := options + ' data-icon="forward"';
    biBack       : options := options + ' data-icon="back"';
    biAlert      : options := options + ' data-icon="alert"';
    biInfo       : options := options + ' data-icon="info"';
    biSearch     : options := options + ' data-icon="search"';
  end;
  if Icon <> biNone then
    begin
      case IconPos of
        ipLeft   : options := options + ' data-iconpos="left"';
        ipRight  : options := options + ' data-iconpos="right"';
        ipTop    : options := options + ' data-iconpos="top"';
        ipBottom : options := options + ' data-iconpos="bottom"';
        ipNoText : options := options + ' data-iconpos="notext"';
      end;
      if not IconShadow then options := options + ' data-iconshadow="false"';
    end;
  if Inlined    then options := options + ' data-inline="true"';
  if Mini       then options := options + ' data-mini="true"';
  if not Shadow then options := options + ' data-shadow="false"';
  options := options + format (' data-theme="%s"', [Theme]);
  if length (HRef) > 0 then
    begin
      if HRefAsDialog then
        Result := format (#9#9#9'<a href="%s" reftype="btn" data-role="button" data-rel="dialog"%s>%s</a> <!--- End of Button --->'#10,
                      [HRef, options, Caption])
      else
        Result := format (#9#9#9'<a href="%s" reftype="btn" data-role="button"%s>%s</a> <!--- End of Button --->'#10,
                      [HRef, options, Caption]);
    end
  else
    Result := format (#9#9#9'<a id="%s" reftype="btn" data-role="button"%s>%s</a> <!--- End of Button --->'#10,
                      [Name, options, Caption])
end;
{ TJQMFooter }

constructor TJQMFooter.Create;
begin
  inherited;
end;

destructor TJQMFooter.Destroy;
begin
  inherited;
end;

function TJQMFooter.Render: string;
begin
  Result :=
    format (#9#9#9'<div id="%s" reftype="footer" data-role="footer" data-theme="%s">', [Name, Theme]) +
	  format ('<h1>%s</h1>', [Caption]) + '</div> <!--- End of Footer --->'#10;
end;

{ TJQMSession }

constructor TJQMSession.Create;
begin
  FOwner := nil;
  ID := '';
  UserAgent := '';
  Host := '';
  Data := nil;
  UsingWS := false;
  Created := Now;
end;

destructor TJQMSession.Destroy;
begin
  inherited;
end;

function TJQMSession.WebSocket: TWSClient;
begin
  Result := nil;
  if FOwner = nil then exit;
  if not UsingWS then exit;
  Result := FOwner.GetWS (ID);
end;

{ TJQMSlider }

procedure TJQMSlider.Assign (anOther: TJQMItem);
var
  aSlider : TJQMSlider;
begin
  inherited;
  if not (anOther is TJQMSlider) then exit;
  aSlider := TJQMSlider (anOther);
  Mini := aSlider.Mini;
  FillHL := aSlider.FillHL;
  Step := aSlider.Step;
  Min := aSlider.Min;
  Max := aSlider.Max;
  Value := aSlider.Value;
end;

constructor TJQMSlider.Create;
begin
  inherited;
  Mini := false;
  FillHL := false;
  Min := 0;
  Max := 100;
  Step := 0;
  Value := 40;
end;

destructor TJQMSlider.Destroy;
begin
  //
  inherited;
end;

function TJQMSlider.Render: string;
var
  tmp : string;
begin
  if Step > 0 then
    tmp := format (' step="%d"', [Step])
  else
    tmp := '';
  Result :=
    format (#9#9#9'<label><input type="range" reftype="slider" id="%s" data-theme="%s" data-highlight="%s" data-mini="%s" value="%d" min="%d" max="%d%s" />%s</label>'#10,
              [Name, Theme, ft[FillHL], ft[Mini], Value, Min, Max, tmp, Caption]);
end;

{ TJQMImage }

constructor TJQMImage.Create;
begin
  inherited;
  Source := '';
  Scale := 100;
end;

destructor TJQMImage.Destroy;
begin
  inherited;
end;

function TJQMImage.Render: string;
begin
  if Source = '' then
    Result := ''
  else
    Result := format (#9#9#9'<img src="%s" width="%d%%" style="margin:auto;display:block"/>  <!-- End of Image --->'#10, [Source, Scale]);
end;

{ TJQMMap }

constructor TJQMMap.Create;
begin
  inherited;
end;

destructor TJQMMap.Destroy;
begin
  //
  inherited;
end;

function TJQMMap.Render: string;
begin
  Result := #9#9#9#9'<!--- Map not yet implemented --->'#10;
end;

{ TJQMRadioGroup }
procedure TJQMRadioGroup.Add (aCaption: string; Checked: boolean);
begin
  Items.AddObject (aCaption, TObject (Checked));
end;

procedure TJQMRadioGroup.Assign (anOther: TJQMItem);
var
  aGroup : TJQMRadioGroup;
begin
  inherited;
  if not (anOther is TJQMRadioGroup) then exit;
  aGroup := TJQMRadioGroup (anOther);
  Items.Assign (aGroup.Items);
end;

constructor TJQMRadioGroup.Create;
begin
  inherited;
  Items := TStringList.Create;
  Horizontal := false;
end;

destructor TJQMRadioGroup.Destroy;
begin
  Items.Free;
  inherited;
end;

function TJQMRadioGroup.Render: string;
var
  i : integer;
  tmp : string;
begin
  Result :=
    #9#9#9'<div data-role="fieldcontain">'#10 +
      format (#9#9#9#9'<fieldset reftype="radiogroup" id="%s" data-role="controlgroup"', [Name]);
  if Horizontal then Result := Result + ' data-type="horizontal"';
  Result := Result + '>'#10 +
        #9#9#9#9#9'<legend>' + Caption + '</legend>'#10;
  for i := 0 to Items.Count - 1 do
    begin
      if integer (Items.Objects[i]) <> 0 then
        tmp := ' checked="checked"'
      else
        tmp := '';
      Result := Result +
        format (#9#9#9#9#9'<label reftype="radio"><input type="radio" name="%s"%s/>%s</label>'#10,
                [Name, tmp, Items[i]]);
    end;
  Result := Result +
      #9#9#9#9'</fieldset>'#10 +
		#9#9#9'</div> <!--- End of RadioBox Group --->'#10;
end;

{ TJQMStyle }

procedure TJQMStyle.Assign (anOther: TJQMStyle);
begin
  Family := anOther.Family;
  Colour := anOther.Colour;
  Weight := anOther.Weight;
  Size := anOther.Size;
  Style := anOther.Style;
  Justification := anOther.Justification;
end;

function TJQMStyle.ColourParam: string;
begin
  Result := format ('color:%s;', [HTMLColour (Colour)]);
end;

constructor TJQMStyle.Create;
begin
  Family := 'Arial';
  Style := fsNormal;
  Weight := fwNormal;
  Size := fsMedium;
  Justification := juLeft;
  Colour := $000000;  // black
end;

destructor TJQMStyle.Destroy;
begin
//
  inherited;
end;

function TJQMStyle.FamilyParam: string;
begin
  Result := format ('font-family:%s;', [Family]);
end;

function TJQMStyle.JustificationParam: string;
begin
  case Justification of
    juCentre : Result := 'margin:0 auto; margin-left:auto; margin-right:auto; align:center; text-align:center;';
    juRight  : Result := 'width:100%; text-align:right;';
    juLeft   : Result := 'text-align:left;';
    //' margin:0 auto; margin-left:auto; margin-right:auto; align:right; text-aligh:right; right:0px;';
  end;
end;

function TJQMStyle.Params: string;
begin
  Result := ColourParam + FamilyParam + WeightParam + SizeParam +
            StyleParam + JustificationParam;
end;

function TJQMStyle.SizeParam: string;
begin
  case Size of
    fsXXSmall : Result := 'font-size:xx-small;';
    fsXSmall  : Result := 'font-size:x-small;';
    fsSmall   : Result := 'font-size:small;';
    fsLarge   : Result := 'font-size:large;';
    fsXLarge  : Result := 'font-size:x-large;';
    fsXXLarge : Result := 'font-size:xx-large;';
    fxSmaller : Result := 'font-size:smaller;';
    fxLarger  : Result := 'font-size:larger;';
  end;
end;

function TJQMStyle.StyleParam: string;
begin
  case Style of
    fsItalic  : Result := 'font-style:italic;';
    fsOblique : Result := 'font-style:oblique;';
  end;
end;

function TJQMStyle.WeightParam: string;
begin
  case Weight of
    fwBold    : Result := 'font-weight:bold;';
    fwBolder  : Result := 'font-weight:bolder;';
    fwLighter : Result := 'font-weight:lighter;';
  end;
end;

{ TJQMResponse }

procedure TJQMResponse.AddItem (aType, aName, aValue : string);
begin
  AddItem (aType, aName, -1, aValue);
end;

procedure TJQMResponse.AddItem (aType, aName: string; anIndex: integer;
  aValue: string);
var
  anItem : TJQMResponseItem;
begin
  anItem := TJQMResponseItem.Create;
  anItem.aType := aType;
  anItem.aName := aName;
  anItem.aValue := aValue;
  anItem.anIndex := anIndex;
  Items.Add (anItem);
end;

procedure TJQMResponse.ChangePage(aName, Options: string);
begin
  AddItem ('page', aName, Options);
end;

procedure TJQMResponse.ChangeState (aName: string; aState: boolean);
begin
  AddItem ('state', aName, ft[aState]);
end;

procedure TJQMResponse.ChangeState (aName: string; anIndex: integer;
  aState: boolean);
begin
  AddItem ('state', aName, anIndex, ft[aState]);
end;

procedure TJQMResponse.ChangeValue (aName : string; aValue : integer);
begin
  AddItem ('amount', aName, IntToStr (aValue));
end;

procedure TJQMResponse.ChangeValue (aName: string; anIndex, aValue: integer);
begin
  AddItem ('amount', aName, anIndex, IntToStr (aValue));
end;

procedure TJQMResponse.ChangeText (aName, aCaption: string);
begin
  AddItem ('text', aName, aCaption);
end;

procedure TJQMResponse.ChangeText (aName: string; anIndex: integer;
  aCaption: string);
begin
  AddItem ('text', aName, anIndex, aCaption);
end;

procedure TJQMResponse.ChangePage (aName: string);
begin
  ChangePage (aName, '');
end;

procedure TJQMResponse.Clear;
var
  i : integer;
begin
  for i := 0 to Items.Count - 1 do
    TJQMResponseItem (Items[i]).Free;
  Items.Clear;
end;

constructor TJQMResponse.Create;
begin
  Items := TList.Create;
end;

destructor TJQMResponse.Destroy;
begin
  Clear;
  Items.Free;
  inherited;
end;

function TJQMResponse.Empty: boolean;
begin
  Result := Items.Count = 0;
end;

function TJQMResponse.ToXml: string;
var
  i : integer;
begin
  Result := '';
  if Empty then exit;
  Result := '<response>';
  for i := 0 to Items.Count - 1 do
    with TJQMResponseItem (Items[i]) do
      if anIndex < 0 then
        Result := Result + format ('<%s name="%s" value="%s"/>', [aType, aName, aValue])
      else
        Result := Result + format ('<%s name="%s" index="%d" value="%s"/>', [aType, aName, anIndex, aValue]);
  Result := Result + '</response>';
end;

{ TJQMLabel }

procedure TJQMLabel.Assign (anOther: TJQMItem);
var
  anLCR : TJQMLabel;
begin
  inherited;
  if not (anOther is TJQMLabel) then exit;
  anLCR := TJQMLabel (anOther);
  Name := anLCR.Name;
  Padding := anLCR.Padding;
  Style.Assign (anLCR.Style);
  Items.Assign (anLCR.Items);
end;

constructor TJQMLabel.Create;
begin
  inherited;
  Padding := 0;
  Style := TJQMStyle.Create;
  Items := TStringList.Create;
end;

destructor TJQMLabel.Destroy;
begin
  Style.Free;
  Items.Clear;
  inherited;
end;

function TJQMLabel.Render: string;
var
  w, i : integer;
  s : string;
begin
  Result := '';
  if Items.Count = 0 then exit;
  w := 100 div Items.Count;
  Result :=
    format (#9#9#9'<table reftype="label" id="%s" border="0" width="100%%" style="%s">'#10,
            [Name, 'padding-top:4px;' + Style.FamilyParam + Style.ColourParam +
             Style.WeightParam + Style.StyleParam + Style.SizeParam]) +
    #9#9#9#9'<tr>'#10;
  if Items.Count = 1 then
    begin
      case Style.Justification of
        juLeft   : s := format ('text-align:left; padding-left:%dpx;', [Padding]);
        juCentre : s := 'text-align:center;';
        juRight  : s := format ('text-align:right; padding-right:%dpx;', [Padding]);
        end;
      Result := Result +
          format (#9#9#9#9#9'<td style="width:%d%%;%s"><span class="ui-label-text">%s</span></td>'#10,
                   [w, s, Items[0]]);
    end
  else
    for i := 0 to Items.Count - 1 do
      begin
        if i = 0 then
          s := format ('text-align:left; padding-left:%dpx;', [Padding])
        else if i = Items.Count - 1 then
          s := format ('text-align:right; padding-right:%dpx;', [Padding])
        else
          s := 'text-align:center;';
        Result := Result +
          format (#9#9#9#9#9'<td style="width:%d%%;%s"><span class="ui-label-text">%s</span></td>'#10,
                   [w, s, Items[i]]);
      end;
  Result := Result +
    #9#9#9#9'</tr>'#10 +
    #9#9#9'</table>  <!-- End of Label --->'#10;
end;

{ TJQMSwitch }

procedure TJQMSwitch.Assign (anOther: TJQMItem);
var
  aSwitch : TJQMSwitch;
begin
  inherited;
  if not (anOther is TJQMSwitch) then exit;
  aSwitch := TJQMSwitch (anOther);
  Labels[false] := aSwitch.Labels[false];
  Labels[true] := aSwitch.Labels[true];
end;

constructor TJQMSwitch.Create;
begin
  inherited;
  Labels[false] := 'Off';
  Labels[true] := 'On';
end;

destructor TJQMSwitch.Destroy;
begin
  //
  inherited;
end;

function TJQMSwitch.Render: string;
begin
  Result := #9#9#9'<li data-role="fieldcontain">'#10 +
	        	format (#9#9#9#9'<label for="%s">%s</label>'#10, [Name, Caption]) +
				    format (#9#9#9#9'<select reftype="switch" name="%s" id="%s" data-role="slider">'#10, [Name, Name]);
  Result := Result + format (#9#9#9#9#9'<option value="off">%s</option>'#10 +
					            #9#9#9#9#9'<option value="on">%s</option>'#10, [Labels[false], Labels[true]]);
  Result := Result + #9#9#9#9'</select>'#10 +
			                #9#9#9'</li>  <!-- End of Switch --->'#10;
end;

{ TJQMKeypad }

procedure TJQMKeypad.Assign (anOther: TJQMItem);
var
  aKeyPad : TJQMKeypad;
begin
  inherited;
  if not (anOther is TJQMKeypad) then exit;
  aKeyPad := TJQMKeypad (anOther);
  Pass := aKeyPad.Pass;
  TitleTheme := aKeyPad.TitleTheme;
  OKTheme := aKeyPad.OKTheme;
  Style.Assign (aKeyPad.Style);
end;

constructor TJQMKeypad.Create;
begin
  inherited;
  Theme := 'a';
  TitleTheme := 'c';
  OKTheme := Theme;
  Pass := false;
  Style := TJQMStyle.Create;
end;

destructor TJQMKeypad.Destroy;
begin
  Style.Free;
  inherited;
end;

function TJQMKeypad.Render: string;
var
  tmp : string;
begin
  if Pass then
    tmp := ' pass="true"'
  else
    tmp := '';
  Result := format (#9#9#9'<fieldset class="ui-grid-solo">'#10 +
	    #9#9#9#9'<div class="ui-block-a"><a id="%s"%s caption="%s" data-role="button" data-theme="%s">%s</a></div>'#10 +
      #9#9#9'</fieldset>'#10, [Name, tmp, Caption, TitleTheme, Caption]);
  Result := Result + format (#9#9#9'<fieldset class="ui-grid-b">'#10 +
			#9#9#9#9'<div class="ui-block-a"><a data-role="button" tag="0" for="%s" data-theme="%s">1</a></div>'#10 +
			#9#9#9#9'<div class="ui-block-b"><a data-role="button" tag="1" for="%s" data-theme="%s">2</a></div>'#10 +
			#9#9#9#9'<div class="ui-block-c"><a data-role="button" tag="2" for="%s" data-theme="%s">3</a></div>'#10 +
		  #9#9#9'</fieldset>'#10, [Name, Theme, Name, Theme, Name, Theme]);
  Result := Result + format (#9#9#9'<fieldset class="ui-grid-b">'#10 +
			#9#9#9#9'<div class="ui-block-a"><a data-role="button" tag="3" for="%s" data-theme="%s">4</a></div>'#10 +
			#9#9#9#9'<div class="ui-block-b"><a data-role="button" tag="4" for="%s" data-theme="%s">5</a></div>'#10 +
			#9#9#9#9'<div class="ui-block-c"><a data-role="button" tag="5" for="%s" data-theme="%s">6</a></div>'#10 +
		  #9#9#9'</fieldset>'#10, [Name, Theme, Name, Theme, Name, Theme]);
  Result := Result + format (#9#9#9'<fieldset class="ui-grid-b">'#10 +
			#9#9#9#9'<div class="ui-block-a"><a data-role="button" tag="6" for="%s" data-theme="%s">7</a></div>'#10 +
			#9#9#9#9'<div class="ui-block-b"><a data-role="button" tag="7" for="%s" data-theme="%s">8</a></div>'#10 +
			#9#9#9#9'<div class="ui-block-c"><a data-role="button" tag="8" for="%s" data-theme="%s">9</a></div>'#10 +
		  #9#9#9'</fieldset>'#10, [Name, Theme, Name, Theme, Name, Theme]);
  Result := Result + format (#9#9#9'<fieldset class="ui-grid-b">'#10 +
			#9#9#9#9'<div class="ui-block-a"><a data-role="button" tag="9" for="%s" data-theme="%s">CLR</a></div>'#10 +
			#9#9#9#9'<div class="ui-block-b"><a data-role="button" tag="10" for="%s" data-theme="%s">0</a></div>'#10 +
			#9#9#9#9'<div class="ui-block-c"><a data-role="button" tag="11" for="%s" data-theme="%s">OK</a></div>'#10 +
		  #9#9#9'</fieldset>'#9#9'<!-- End of Keypad --->'#10, [Name, OKTheme, Name, Theme, Name, OKTheme]);
end;

{ TJQMCheckGroup }

procedure TJQMCheckGroup.Add (aCaption: string; Checked: boolean);
begin
  Items.AddObject (aCaption, TObject (Checked));
end;

procedure TJQMCheckGroup.Assign (anOther: TJQMItem);
var
  aGroup : TJQMCheckGroup;
begin
  inherited;
  if not (anOther is TJQMCheckGroup) then exit;
  aGroup := TJQMCheckGroup (anOther);
  Horizontal := aGroup.Horizontal;
  Items.Assign (aGroup.Items);
end;

constructor TJQMCheckGroup.Create;
begin
  inherited;
  Items := TStringList.Create;
end;

destructor TJQMCheckGroup.Destroy;
begin
  Items.Free;
  inherited;
end;

function TJQMCheckGroup.Render: string;
var
  i : integer;
  tmp : string;
begin
  Result :=
    #9#9#9'<div data-role="fieldcontain">'#10 +
      format (#9#9#9#9'<fieldset reftype="checkgroup" id="%s" data-role="controlgroup"', [Name]);
  if Horizontal then Result := Result + ' data-type="horizontal"';
  Result := Result + '>'#10 +
        #9#9#9#9#9'<legend>' + Caption + '</legend>'#10;
  for i := 0 to Items.Count - 1 do
    begin
      if integer (Items.Objects[i]) <> 0 then
        tmp := ' checked="checked"'
      else
        tmp := '';
      Result := Result +
        format (#9#9#9#9#9'<label reftype="check" id="%s-%d"><input type="checkbox" name="%s-%d" for="%s"%s/>%s</label>'#10,
                [Name, i, Name, i, Name, tmp, Items[i]]);
    end;
  Result := Result +
      #9#9#9#9'</fieldset>'#10 +
		#9#9#9'</div> <!--- End of CheckBox Group --->'#10;
end;

{ TJQMCustomItem }
procedure TJQMCustomItem.Assign (anOther: TJQMItem);
var
  aCustom : TJQMCustomItem;
begin
  inherited;
  if not (anOther is TJQMCustomItem) then exit;
  aCustom := TJQMCustomItem (anOther);
  RenderString := aCustom.RenderString;
end;

constructor TJQMCustomItem.Create;
begin
  RenderString := '';
end;

destructor TJQMCustomItem.Destroy;
begin
  inherited;
end;

function TJQMCustomItem.Render: string;
begin
  Result := RenderString;
end;

{ TJQMMenu }

function TJQMMenu.Add(aString: string): integer;
begin
  Result := Items.Add (aString);
end;

procedure TJQMMenu.Assign (anOther: TJQMItem);
var
  aMenu : TJQMMenu;
begin
  inherited;
  if not (anOther is TJQMMenu) then exit;
  aMenu := TJQMMenu (anOther);
  Items.Assign (aMenu.Items);
end;

constructor TJQMMenu.Create;
begin
  inherited;
  Items := TStringList.Create;
end;

destructor TJQMMenu.Destroy;
begin
  Items.Free;
  inherited;
end;

function TJQMMenu.Render: string;
var
  i : integer;
begin
  Result :=
    #9#9#9'<div data-role="fieldcontain">'#10;
  if length (Caption) > 0 then
    Result := Result +
    format (#9#9#9#9'<label for="%s" class="select">%s</label>'#10, [Name, Caption]);
  Result := Result +
		format (#9#9#9#9'<select name="%s" id="%s" reftype="menu" data-native-menu="false">'#10, [Name, Name]);
  for i := 0 to Items.Count - 1 do
    Result := Result + format (#9#9#9#9#9'<option id="%s-%d" value="%s" class="ui-label-text">%s</option>'#10, [Name, i, Items[i], Items[i]]);
  Result := Result +
		 #9#9#9#9'</select>'#10 +
		 #9#9#9'</div> <!--- End of Menu --->'#10;
end;

end.




