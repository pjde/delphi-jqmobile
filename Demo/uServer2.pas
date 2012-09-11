unit uServer2;

{
  Demo application for Delphi JQuery Mobile Components

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
// editor    http://codiqa.com/embed/editor
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, uJQ,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, Menus;

type
  TServerForm = class(TForm)
    Memo1: TMemo;
    Status1: TStatusBar;
    LinkPortTxt: TEdit;
    Label4: TLabel;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button1: TButton;
    Button5: TButton;
    WSPortTxt: TEdit;
    Label1: TLabel;
    Button6: TButton;
    Bar1: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Memo1DblClick(Sender: TObject);
    procedure PortTxtKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure Show1Click(Sender: TObject);
    procedure Tray1DblClick(Sender: TObject);
    procedure TimerProc (var aMsg : TMessage); message WM_TIMER;
    procedure Button2Click(Sender: TObject);

    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure JQM1GetPage(Sender: TObject; Session: TJQMSession;
      Levels: TStringList; var aPage: TJQMPage);
    procedure JQM1Mon(Sender: TObject; Str: string);
    procedure JQM1SessionCreate(Sender: TObject; Sesson: TJQMSession);
    procedure JQM1SessionDestroy(Sender: TObject; Sesson: TJQMSession);
    procedure JQM1ShowPage (Sender: TObject; Session: TJQMSession; Name: string;
      Params: TStringList; Response: TJQMResponse);
    procedure JQM1Started (Sender: TObject);
    procedure JQM1StateChanged (Sender: TObject; Session: TJQMSession;
      Name: string; Index: Integer; Value: Boolean; Params: TStringList;
      Response: TJQMResponse);
    procedure JQM1ValueChanged (Sender: TObject; Session: TJQMSession;
      Name: string; Index, Value: Integer; Params: TStringList;
      Response: TJQMResponse);
    procedure JQM1TextChanged (Sender: TObject; Session: TJQMSession;
      Name: string; Index: Integer; Value: string; Params: TStringList;
      Response: TJQMResponse);
    procedure JQM1Stopped (Sender: TObject);
    procedure JQM1Swiped (Sender: TObject; Session: TJQMSession; Name: string;
      Params: TStringList; Response: TJQMResponse);
    procedure JQM1Timer (Sender: TObject; Sesson: TJQMSession;
      Params: TStringList; Response: TJQMResponse);
    procedure JQM1TapHeld (Sender: TObject; Session: TJQMSession; Name: string;
      Params: TStringList; Response: TJQMResponse);
    procedure JQM1Tapped (Sender: TObject; Session: TJQMSession; Name: string;
      Params: TStringList; Response: TJQMResponse);
    procedure Button6Click(Sender: TObject);
    procedure Bar1Change(Sender: TObject);
  private
    public
    { Public declarations }
    JQM1 : TJQM;
    LinkPort, WSPort : integer;
    Count : integer;
    MonEnable : boolean;
    procedure Mon (aStr : string);
  end;

var
  ServerForm: TServerForm;

implementation

uses IniFiles, uWebSocket;

const
  de : array [boolean] of string = ('Disabled', 'Enabled');

{$R *.dfm}

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

procedure TServerForm.Bar1Change(Sender: TObject);
var
  i : integer;
  aResponse : TJQMResponse;
begin
  aResponse := TJQMResponse.Create;
  aResponse.ChangeValue ('slider1', Bar1.Position);
  for i := 0 to JQM1.WSServer.Server.ClientCount - 1 do
    TWSClient (JQM1.WSServer.Server.Client[i]).SendString (AnsiString (aResponse.ToXml));
  aResponse.Free;
end;

procedure TServerForm.Button1Click (Sender: TObject);
begin
  MonEnable := not MonEnable;
  Button1.Caption := 'Mon ' + de[MonEnable];
end;

procedure TServerForm.Button2Click (Sender: TObject);
var
  x : integer;
begin
  x := StrToIntDef (LinkPortTxt.Text, 0);
  if x <= 0 then
    begin
      ShowMessage ('Invalid Port Number!');
      LinkPortTxt.Text := IntToStr (LinkPort);
      exit;
    end;
  LinkPort := x;
  with TiniFile.Create (ChangeFileExt (Application.ExeName, '.ini')) do
    begin
      WriteInteger ('Link', 'Port', LinkPort);
      Free;
    end;
  JQM1.Stop;
  JQM1.HTTPPort := LinkPort;
  JQM1.Start;
end;

procedure TServerForm.Button5Click(Sender: TObject);
var
  x : integer;
begin
  x := StrToIntDef (WSPortTxt.Text, 0);
  if x <= 0 then
    begin
      ShowMessage ('Invalid Port Number!');
      WSPortTxt.Text := IntToStr (WSPort);
      exit;
    end;
  WSPort := x;
  with TiniFile.Create (ChangeFileExt (Application.ExeName, '.ini')) do
    begin
      WriteInteger ('WS', 'Port', WSPort);
      Free;
    end;
  JQM1.Stop;
  JQM1.WSPort := WSPort;
  JQM1.Start;
end;

procedure TServerForm.Button6Click(Sender: TObject);
var
  i : integer;
  aResponse : TJQMResponse;
begin
  aResponse := TJQMResponse.Create;
  aResponse.ChangePage ('misc');
  for i := 0 to JQM1.WSServer.Server.ClientCount - 1 do
    TWSClient (JQM1.WSServer.Server.Client[i]).SendString (AnsiString (aResponse.ToXml));
  aResponse.Free;
end;

const
  aw : array [boolean] of string = ('AJAX', 'WEB SOCKET');

procedure TServerForm.Button3Click (Sender: TObject);
var
  i : integer;
  aSession : TJQMSession;
begin
  for i := 0 to JQM1.Sessions.Count - 1 do
    begin
      aSession := JQM1.Sessions[i];
      with aSession do
        begin
          Memo1.Lines.Add ('----------------');
          Memo1.Lines.Add ('ID        : ' + ID);
          Memo1.Lines.Add ('Created   : ' + DateTimeToStr (Created));
          Memo1.Lines.Add ('Host      : ' + Host);
          Memo1.Lines.Add ('Agent     : ' + UserAgent);
          Memo1.Lines.Add ('Transport : ' + aw[UsingWS]);
        end;
    end;
end;

procedure TServerForm.Button4Click (Sender: TObject);
var
  i : integer;
  aResponse : TJQMResponse;
begin
  aResponse := TJQMResponse.Create;
  aResponse.ChangeText ('footer1', 0, 'Footer Changed');
  for i := 0 to JQM1.WSServer.Server.ClientCount - 1 do
    TWSClient (JQM1.WSServer.Server.Client[i]).SendString (AnsiString (aResponse.ToXml));
  aResponse.Free;
end;

procedure TServerForm.FormCreate (Sender: TObject);
var
  FileInfo : TVSFixedFileInfo;
  InfoBuffer : PChar;
  InfoSize, Dummy : cardinal;
  nStr : string;
  aPtr : Pointer;
begin
  MonEnable := true;
  FillChar (FileInfo, SizeOf (FileInfo), 0);
  nStr := Application.ExeName;
  InfoSize := GetFileVersionInfoSize (PChar (nStr), Dummy);
  if InfoSize > 0 then
    begin
      InfoBuffer := StrAlloc (InfoSize);
      if GetFileVersionInfo (PChar (nStr), Dummy, InfoSize, InfoBuffer) then
        begin
          VerQueryValue (InfoBuffer, '\', aPtr, InfoSize);
          FileInfo := PVSFixedFileInfo (aPtr)^;
          Status1.Panels[0].Text := format ('Version %d.%d.%d Build %d  ',
            [HiWord (FileInfo.dwFileVersionMS), LoWord (FileInfo.dwFileVersionMS),
             HiWord (FileInfo.dwFileVersionLS), LoWord (FileInfo.dwFileVersionLS)]);
        end;
      StrDispose (InfoBuffer);
    end;
  with TiniFile.Create (ChangeFileExt (Application.ExeName, '.ini')) do
    begin
      LinkPort := ReadInteger ('Link', 'Port', 80);
      WSPort := ReadInteger ('WS', 'Port', 8002);
      Free;
    end;
  Count := 0;

  JQM1 := TJQM.Create (nil);
  JQM1.OnStarted := JQM1Started;              // Server Started
  JQM1.OnStopped := JQM1Stopped;              // Server Stopped
  JQM1.OnGetPage := JQM1GetPage;              // Page Request
  JQM1.OnShowPage := JQM1ShowPage;            // Show Page
  JQM1.OnTapped := JQM1Tapped;
  JQM1.OnTapHeld := JQM1TapHeld;
  JQM1.OnStateChanged := JQM1StateChanged;
  JQM1.OnValueChanged := JQM1ValueChanged;
  JQM1.OnTextChanged := JQM1TextChanged;
  JQM1.OnSwiped := JQM1Swiped;
  JQM1.OnTimer := JQM1Timer;
  JQM1.OnSessionCreate := JQM1SessionCreate;
  JQM1.OnSessionDestroy := JQM1SessionDestroy;
  JQM1.OnMon := JQM1Mon;

  JQM1.JSFolder := 'C:\Delphi Projects\jQuery\wwwroot';
  JQM1.HTTPPort := LinkPort;
  JQM1.WSPort := WSPort;
  JQM1.Start;
end;

procedure TServerForm.FormDestroy (Sender: TObject);
begin
  JQM1.Stop;
  JQM1.Free;
end;

procedure TServerForm.FormShow (Sender: TObject);
begin
  LinkPortTxt.Text := IntToStr (LinkPort);
  WSPortTxt.Text := IntToStr (WSPort);
  Button1.Caption := 'Mon ' + de[MonEnable];
end;

procedure TServerForm.JQM1GetPage (Sender: TObject; Session: TJQMSession;
  Levels: TStringList; var aPage: TJQMPage);
begin
  if Levels.Count < 2 then exit;
  if (Levels[1] = '') or (CompareText (Levels[1], 'Index') = 0) then // root page
    begin
      aPage := TJQMPage.Create;
      with aPage do
        begin
          Caption := 'Main Menu';
          AddHeader ('header1', 'Main Menu');
          with AddList do
            begin
              Caption := 'A List';
              Add ('Local Controls', '', true);
              Add ('Entertainment', '/entertainment.html', false);
              Add ('Lighting', '/lighting.html', false);
              Add ('Environment', '/environment.html', false);
              Add ('Security', '/security.html', false);
              Add ('Front Gate', '/gate.html', false);
              Add ('Misc', '/misc.html', false);
              Add ('Global Controls', '', true);
              Add ('Shut Down', '/shutdown.html', false);
              Add ('Lock Down', '/lockdown.html', false);
            end;
          AddFooter ('footer1', 'Home Control');
        end;
    end
  else if (CompareText (Levels[1], 'entertainment') = 0) then
    begin
      aPage := TJQMPage.Create;
      with aPage do
        begin
          Caption := 'Entertainment';
          AddParam ('category', Caption);
          if not Session.UsingWS then AddParam ('timer', '2000');
          AddHeader ('header1', Caption);
          AddBtn ('musicon', 'MUSIC ON');
          AddBtn ('musicoff', 'MUSIC OFF');
          with AddMenu ('menu2') do
            begin
              Add ('Music Channel 1');
              Add ('Music Channel 2');
              Add ('Music Channel 3');
            end;
          with AddBtn ('menu1', 'MAIN MENU') do
            begin
              HRef := '/Index.html';
              Theme := 'b';
            end;
          AddFooter ('footer1', 'Home Control');
        end;
    end
  else if (CompareText (Levels[1], 'lighting') = 0) then
    begin
      aPage := TJQMPage.Create;
      with aPage do
        begin
          Caption := 'Lighting';
          AddParam ('category', Caption);
          if not Session.UsingWS then AddParam ('timer', '2000');
          AddHeader ('header1', 'Welcome');
      //    AddImage ('image1', 'ColourBlkBack.png');
          AddKeyPad ('pad1', 'PIN Required', false);
          with AddBtn ('menu1', 'MAIN MENU') do
            begin
              HRef := '/Index.html';
              Theme := 'b';
            end;
          AddFooter ('footer1', 'Macquarie University');
        (*  AddHeader ('header1', Caption);
          AddBtn ('lightsfull', 'FULL');
          AddBtn ('lightsmid', 'MID');
          AddBtn ('lightslow', 'LOW');
          AddBtn ('lightsoff', 'OFF');
          aBtn := AddBtn ('menu1', 'MAIN MENU');
          aBtn.HRef := '/Index.html';
          aBtn.Theme := 'b';
          AddFooter ('footer1', 'Home Control');    *)
        end;
    end
  else if (CompareText (Levels[1], 'environment') = 0) then
    begin
      aPage := TJQMPage.Create;
      with aPage do
        begin
          Caption := 'Environment';
          AddParam ('category', Caption);
          if not Session.UsingWS then AddParam ('timer', '2000');
          AddHeader ('header1', Caption);
          AddBtn ('airon', 'AIR ON');
          AddBtn ('airoff', 'AIR OFF');
          AddBtn ('fanon', 'FAN ON');
          AddBtn ('fanmid', 'FAN MID');
          AddBtn ('fanoff', 'FAN OFF');
          with AddBtn ('menu1', 'MAIN MENU') do
            begin
              HRef := '/Index.html';
              Theme := 'b';
            end;
          AddFooter ('footer1', 'Home Control');
        end;
    end
  else if (CompareText (Levels[1], 'security') = 0) then
    begin
      aPage := TJQMPage.Create;
      with aPage do
        begin
          Caption := 'Security';
          AddParam ('category', Caption);
          if not Session.UsingWS then AddParam ('timer', '2000');
          AddHeader ('header1', Caption);
          AddBtn ('secon', 'SECURITY ON');
          AddBtn ('secoff', 'SECURITY OFF');
           with AddBtn ('menu1', 'MAIN MENU') do
            begin
              HRef := '/Index.html';
              Theme := 'b';
            end;
          AddFooter ('footer1', 'Home Control');
        end;
    end
  else if (CompareText (Levels[1], 'misc') = 0) then
    begin
      aPage := TJQMPage.Create;
      with aPage do
        begin
          Caption := 'Misc';
          AddParam ('category', Caption);
          AddParam ('misc', 'this is misc');
        //  if not Session.UsingWS then AddParam ('timer', '2000');
          AddHeader ('header1', Caption);
          with AddLabel ('label1') do
            begin
              Style.Size := fsLarge;
              Padding := 10;
              Items.Add ('Left');
              Items.Add ('Centre');
              Items.Add ('Right');
            end;
          AddBtn ('btn1', 'Button');
          with AddSlider ('slider1', '', 0, 100) do
            Theme := 'c';
          with AddMenu ('menu5') do
            begin
              Add ('Menu Option 1');
              Add ('Menu Option 2');
              Add ('Menu Option 3');
            end;
          with AddCheckGroup ('check1') do
            begin
              // Horizontal := true;
              Add ('Checked 1' , true);
              Add ('Checked 2' , false);
              Add ('Checked 3' , false);
            end;
          with AddRadioGroup ('radio1') do
            begin
//              Horizontal :=  true;
              Add ('Radio 1' , true);
              Add ('Radio 2' , false);
              Add ('Radio 3' , false);
            end;
          AddKeyPad ('key2', 'Password', true);
          AddSwitch ('switch1');
          with AddBtn ('menu1', 'MAIN MENU') do
            begin
              HRef := '/Index.html';
              Theme := 'b';
            end;
          AddFooter ('footer1', 'Home Control');
        end;
    end
  else if (CompareText (Levels[1], 'gate') = 0) then
    begin
      aPage := TJQMPage.Create;
      with aPage do
        begin
          Caption := 'Front Gate';
          AddParam ('category', Caption);
          if not Session.UsingWS then AddParam ('timer', '2000');
          AddHeader ('header1', Caption);
          AddBtn ('gate', 'OPEN GATE');
          with AddBtn ('menu1', 'MAIN MENU') do
            begin
              HRef := '/Index.html';
              Theme := 'b';
            end;
          AddFooter ('footer1', 'Home Control');
        end;
    end;
end;

procedure TServerForm.JQM1Mon (Sender: TObject; Str: string);
begin
  Mon ('JQ -> ' + Str);
end;

procedure TServerForm.JQM1SessionCreate (Sender: TObject; Sesson: TJQMSession);
begin
  Mon ('New Session Created.');
  // use this event to add additonal info via the Session.Data field
end;

procedure TServerForm.JQM1SessionDestroy(Sender: TObject; Sesson: TJQMSession);
begin
  // use this event to clean up any additonal info used via the Session.Data field
end;

procedure TServerForm.JQM1ShowPage(Sender: TObject; Session: TJQMSession;
  Name: string; Params: TStringList; Response: TJQMresponse);
begin
  Mon ('Show Page ' + Name + '.');
end;

procedure TServerForm.JQM1Started(Sender: TObject);
begin
  Mon ('Web Server Started.');
  Mon ('Point Browser to localhost:' + IntToStr (JQM1.HTTPPort));
end;

procedure TServerForm.JQM1StateChanged (Sender: TObject; Session: TJQMSession;
  Name: string; Index: Integer; Value: Boolean; Params: TStringList;
  Response: TJQMResponse);
var
  i : integer;
begin
  Name := UpperCase (Name);
  Mon ('State Changed ' + Name + ', Index ' + IntToStr (Index) + ' : '  + ft[Value]);
  for i := 0 to Params.Count - 1 do
    Mon (format ('%d  %s', [i, Params[i]]));
end;

procedure TServerForm.JQM1Stopped (Sender: TObject);
begin
  Mon ('Web Server Stopped.');
end;

procedure TServerForm.JQM1Swiped (Sender: TObject; Session: TJQMSession;
  Name: string; Params: TStringList; Response: TJQMResponse);
var
  i : integer;
begin
  Name := UpperCase (Name);
  Mon ('Swiped ' + Name + '.');
  for i := 0 to Params.Count - 1 do
    Mon (format ('%d  %s', [i, Params[i]]));
end;

procedure TServerForm.JQM1TapHeld (Sender: TObject; Session: TJQMSession;
  Name: string; Params: TStringList; Response: TJQMResponse);
var
  i : integer;
begin
  Name := UpperCase (Name);
  Mon ('Tap Held ' + Name + '.');
  for i := 0 to Params.Count - 1 do
    Mon (format ('%d  %s', [i, Params[i]]));
end;

procedure TServerForm.JQM1Tapped (Sender: TObject; Session: TJQMSession;
  Name: string; Params: TStringList; Response: TJQMResponse);
var
  i : integer;
begin
  Name := UpperCase (Name);
  Mon ('Tapped ' + Name);
  for i := 0 to Params.Count - 1 do
    Mon (format ('%d  %s', [i, Params[i]]));
  if Name = 'GATE' then Response.ChangePage ('Index');
  if Name = 'BTN1' then
    begin
      count := count + 1;
      Response.ChangeText ('btn1', 0, 'Tapped ' + IntToStr (count) + ' times');
      Response.ChangeText ('label1', 0, DateToStr (Now));
      Response.ChangeText ('label1', 2, TimeToStr (Now));
    end;
end;

procedure TServerForm.JQM1TextChanged (Sender: TObject; Session: TJQMSession;
  Name: string; Index: Integer; Value: string; Params: TStringList;
  Response: TJQMResponse);
begin
  Mon ('Text Changed ' + Name + ', Index ' + IntToStr (Index)  + ' : ' + Value);
end;

procedure TServerForm.JQM1Timer (Sender: TObject; Sesson: TJQMSession;
  Params: TStringList; Response: TJQMResponse);
var
  i : integer;
begin
  Mon ('Timer ');
  for i := 0 to Params.Count - 1 do
    Mon (format ('%d  %s', [i, Params[i]]));
  Response.ChangeText ('musicoff', IntToStr (Trunc (Now * 86400)));
end;

procedure TServerForm.JQM1ValueChanged (Sender: TObject; Session: TJQMSession;
  Name: string; Index, Value: Integer; Params: TStringList;
  Response: TJQMResponse);
begin
  Mon ('Value Changed ' + Name + ', Index ' + IntToStr (Index) + ' : ' + IntToStr (Value));
  if Name = 'slider1' then
    begin
      Response.ChangeText ('label1', 1, IntToStr (Value));
 ///     Bar1.Position := Value;
    end;
end;

procedure TServerForm.Memo1DblClick (Sender: TObject);
begin
  Memo1.Lines.Clear;
end;

procedure TServerForm.Mon (aStr: string);
begin
  if Memo1.Visible and MonEnable then memo1.Lines.Add (aStr);
end;

procedure TServerForm.PortTxtKeyPress (Sender: TObject; var Key: Char);
begin
  if not CharInSet (Key, ['0'..'9', #8]) then Key := #0;
end;

procedure TServerForm.Show1Click (Sender: TObject);
begin
  Show;
end;

procedure TServerForm.TimerProc (var aMsg: TMessage);
begin
  mon ('Timer ' + IntToStr (aMsg.WParam) + ' triggered');
  case aMsg.WParam of
    1 :
      begin
        KillTimer (Handle, 1);
      end;
  end;
end;

procedure TServerForm.Tray1DblClick (Sender: TObject);
begin
  Show;
end;

end.
