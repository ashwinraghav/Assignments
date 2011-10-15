{********************************************************************************
 *                  Standard Interface Library (SIL)                            *
 *                                                                              *
 *       General purpose library whose design is based in STRONG                *
 *   use of interfaces.                                                         *
 *                                                                              *
 *                                                                              *
 *     Copyright (C) 2000 Mariano Podestá    antiriad@gmail.com                 *
 *     Copyright (C) 2000 Leandro Conde      lconde@str.com.ar                  *
 *     Copyright (C) 2000 Lisandro Podestá   lisandrop@movi.com.ar              *
 *                                                                              *
 *     See License.txt for details.                                             *
 *                                                                              *
 *   This library is free software; you can redistribute it and/or              *
 *   modify it under the terms of the GNU Lesser General Public                 *
 *   License as published by the Free Software Foundation; either               *
 *   version 2.1 of the License, or (at your option) any later version.         *
 *                                                                              *
 *   This library is distributed in the hope that it will be useful,            *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of             *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          *
 *   Lesser General Public License for more details.                            *
 *                                                                              *
 *   You should have received a copy of the GNU Lesser General Public           *
 *   License along with this library; if not, write to the Free Software        *
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA  *
 *                                                                              *
 ********************************************************************************}

unit SilSmPop3Client;

interface

uses
  Sil,
  SilLkInterfaced,
  SilSiPop3Command,
  SilSiMailClient,
  SilSiPop3,
  SilLiStringList,
  SilLiEnumerator;

const
  SListEndMark = '.';

type
  RParams = record
    Index: LongWord;
    Data: String;
  end;

  TPop3Client = class (
    // extends
    TSilInterfacedObject,
    //implements
    IPop3Client)
  private
    FCommand: IPop3ClientCommand;
    FMessages: IMailMessageList;
    FCoders: IStringList;
    FTotalCount: Integer;
    FTotalSize: Integer;
    FBoundaries: IStringList;
    FLastResult: String;
  private
    function DoGetMessage(Number: Integer): IMailMessage;
    function DoExtractParam(const Buffer: String; out Params: RParams): Boolean;
    procedure DoRetrieveMessage(const Msg: IMailMessage);
    procedure DoAddLabelLine(const MimeLabel: IMimeLabel; const Buffer: String);
  protected // IPopClient
    function GetMessages: IMailMessageList;
    function GetCoders: IStringList;
    function Login(const UserName: String; const Password: String): Boolean;
    function Logout: Boolean;
    function RetrieveHeader(Number: Integer; MessageLines: Cardinal): Boolean;
    function RetrieveMessage(Number: Integer): Boolean;
    function RetrieveUIDL(Number: Integer): Boolean;
    function DeleteMessage(Number: Integer): Boolean;
    function RetrieveAllHeaders: Boolean;
    function RetrieveAllMessages: Boolean;
    function RetrieveAllUIDLs: Boolean;
    function DeleteAllMessages: Boolean;
    function RetrieveMessageList: Boolean;
    function RetrieveInfo: Boolean;
  public
    constructor Create(const Command: IPop3ClientCommand);
    destructor Destroy; override;
  end;

implementation

uses
  SilBtStr,
  SilBcChr,
  SilLmStringList,
  SilSmMime,
  SilSmMimeCoder,
  SilLtSerializer;

{ TPop3Client }

constructor TPop3Client.Create(const Command: IPop3ClientCommand);
begin
  inherited Create;

  FTotalCount := -1;
  FTotalSize := -1;

  FCommand := Command;
  FMessages := TMailMessageList.Create;
  FBoundaries := Sil.List.StringList;

  FCoders := Sil.List.StringList;
  FCoders.IgnoreCase := true;
  FCoders.Add(TBase64Coder.Name, TBase64Coder);
  FCoders.Add(TUUCoder.Name, TUUCoder);
  FCoders.Add(TISO8859_1Coder.Name, TISO8859_1Coder);
  FCoders.Add(TQuotedPrintableCoder.Name, TQuotedPrintableCoder);
  FCoders.Add(TWindows1252Coder.Name, TWindows1252Coder);
  FCoders.Add(TDefaultCoder.Name, TDefaultCoder);
end;

destructor TPop3Client.Destroy;
begin
  FCommand := nil;
  FMessages := nil;
  FBoundaries := nil;
  FCoders := nil;

  inherited;
end;

function TPop3Client.GetCoders: IStringList;
begin
  Result := FCoders;
end;

function TPop3Client.GetMessages: IMailMessageList;
begin
  Result := FMessages;
end;

function TPop3Client.DoExtractParam(const Buffer: String; out Params: RParams): Boolean;
var
  i: Integer;
begin
  i := Str.Pos(ccSPC, Buffer);
  Result := i > 0;

  if Result then
  begin
    Params.Index := Str.ToInt(Str.Copy(Buffer, 1, i - 1), 0);
    Params.Data := Str.Trim(Str.Copy(Buffer, i + 1));
  end;
end;

function TPop3Client.DoGetMessage(Number: Integer): IMailMessage;
var
  e: IEnumerator;
begin
  while FMessages.Enumerate(e, Result) do
    if Result.Number = Number then Exit;

  Result := TMailMessage.Create;
  Result.Number := Number;
  FMessages.Add(Result);
end;

function TPop3Client.DeleteAllMessages: Boolean;
var
  i: Integer;
  sResponse: String;
begin
  if FTotalCount < 0 then
  begin
    Result := RetrieveInfo;
    if not Result then Exit;
  end else
    Result := false;

  for i := 1 to FTotalCount do
  begin
    Result := FCommand.Delete(i, sResponse);
    if not Result then Break;
  end;
end;

function TPop3Client.DeleteMessage(Number: Integer): Boolean;
var
  sResponse: String;
begin
  Result := FCommand.Delete(Number, sResponse);
end;

function TPop3Client.Login(const UserName, Password: String): Boolean;
var
  sResponse: String;
begin
  Result := FCommand.WaitGreetings(sResponse) and FCommand.User(UserName, FLastResult);

  if Result then
  begin
    Result := FCommand.Password(Password, sResponse);
    FLastResult := FLastResult + ccCRLF + sResponse;
  end;
end;

function TPop3Client.Logout: Boolean;
begin
  Result := FCommand.Quit(FLastResult);
end;

function TPop3Client.RetrieveAllHeaders: Boolean;
var
  i: Integer;
  bOk: Boolean;
begin
  Result := false;
  i := 1;

  repeat
    bOk := RetrieveHeader(i, 0);
    if bOk then Result := true;
    Inc(i);
  until not bOk;
end;

function TPop3Client.RetrieveAllMessages: Boolean;
var
  i: Integer;
  bOk: Boolean;
begin
  Result := false;
  i := 1;

  repeat
    bOk := RetrieveMessage(i);
    if bOk then Result := true;
    Inc(i);
  until not bOk;
end;

function TPop3Client.RetrieveAllUIDLs: Boolean;
var
  sResponse: String;
  Params: RParams;
  Msg: IMailMessage;
begin
  Result := FCommand.Uidls(sResponse);
  if not Result then Exit;

  // 1 whqtswO00WBw418f9t5JxYwZ
  // .
  while FCommand.ReadLn(sResponse) and (sResponse <> SListEndMark) do
    if DoExtractParam(sResponse, Params) then
    begin
      Msg := DoGetMessage(Params.Index);
      Msg.Uidl := Params.Data;
    end;
end;

function TPop3Client.RetrieveInfo: Boolean;
var
  sResponse: String;
  Params: RParams;
begin
  Result := FCommand.Status(sResponse);
  if not Result then Exit;

  // +OK 2 320
  if DoExtractParam(sResponse, Params) then
  begin
    FTotalCount := Params.Index;
    FTotalSize := Str.ToInt(Params.Data, -1);
  end;
end;

function TPop3Client.RetrieveMessageList: Boolean;
var
  sResponse: String;
  Params: RParams;
  Msg: IMailMessage;
begin
  Result := FCommand.MessageList(sResponse);
  if not Result then Exit;

  // 1 120
  // .
  while FCommand.ReadLn(sResponse) and (sResponse <> SListEndMark) do
    if DoExtractParam(sResponse, Params) then
    begin
      Msg := DoGetMessage(Params.Index);
      Msg.Size := Str.ToInt(Params.Data, -1);
    end;
end;

function TPop3Client.RetrieveUIDL(Number: Integer): Boolean;
var
  sResponse: String;
  Params: RParams;
  Msg: IMailMessage;
begin
  Result := FCommand.Uidl(Number, sResponse);
  if not Result then Exit;

  // +OK 2 QhdPYR:00WBw1Ph7x7
  if DoExtractParam(sResponse, Params) then
  begin
    Msg := DoGetMessage(Params.Index);
    Msg.Uidl := Params.Data;
  end;
end;

function TPop3Client.RetrieveHeader(Number: Integer; MessageLines: Cardinal): Boolean;
var
  sResponse: String;
begin
  Result := FCommand.Header(Number, MessageLines, sResponse);
  if not Result then Exit;
  DoRetrieveMessage(DoGetMessage(Number));
end;

function TPop3Client.RetrieveMessage(Number: Integer): Boolean;
var
  sResponse: String;
begin
  Result := FCommand.Retrieve(Number, sResponse);
  if not Result then Exit;
  DoRetrieveMessage(DoGetMessage(Number));
end;

procedure TPop3Client.DoRetrieveMessage(const Msg: IMailMessage);
var
  sLine: String;
  iPos: Integer;
  bScanLabels: Boolean;
  CurrentPart: IMailPart;
  CurrentLabel: IMimeLabel;
begin
  bScanLabels := true;

  // <...>
  // .
  while FCommand.ReadLn(sLine) and (sLine <> SListEndMark) do
  begin
    if (Length(sLine) = 0) and bScanLabels then
    begin
      bScanLabels := false;
      CurrentPart := nil;
      Continue;
    end;

    if CurrentPart = nil then
    begin
      if not bScanLabels then
      begin
        CurrentPart := TMailPart.Create;
        Msg.Parts.Add(CurrentPart);
      end else
        CurrentPart := Msg.Header;
    end;

    if bScanLabels then
    begin
      if (sLine[1] <> ccSPC) and (sLine[1] <> ccHT) and Str.GetPos(':', sLine, 1, iPos) then
      begin
        CurrentLabel := CurrentPart.Labels.CreateNew(Str.Copy(sLine, 1, iPos - 1));
        CurrentLabel.Coders := FCoders;
        sLine := Str.Copy(sLine, iPos + 2);
      end;

      if CurrentLabel <> nil then DoAddLabelLine(CurrentLabel, Str.Trim(sLine));
    end else
    begin
      if (Length(sLine) > 0) and (sLine[1] = '-') then
        if FBoundaries.IndexOf(sLine) >= 0 then
        begin
          bScanLabels := true;
          CurrentPart := nil;
        end else
        if FBoundaries.IndexOf(Str.Copy(sLine, 1, Length(sLine) - 2)) >= 0 then
          Continue;

      if not bScanLabels and (CurrentPart <> nil) then
        CurrentPart.Body.Add(sLine);
    end;
  end;
end;

procedure TPop3Client.DoAddLabelLine(const MimeLabel: IMimeLabel; const Buffer: String);
var
  i, iPos: Integer;
  sItem, sName, sValue: String;
begin
  if Length(Buffer) = 0 then Exit;
  i := 1;

  repeat
    sItem := Str.Token(Buffer, ';', i);
    if Length(sItem) = 0 then Break;

    if i > 0 then
      iPos := Str.Pos('=', sItem, i) else
      iPos := 0;

    if iPos > 0 then
    begin
      sName := Str.Copy(sItem, 1, iPos - 1);
      sValue := Str.Copy(sItem, iPos + 1);

      if Sil.Text.Compare(sName, 'boundary') = 0 then
        FBoundaries.Add('--' + Str.Replace(sValue, '"', ''));

      MimeLabel.Params.AddNew(sName, sValue);
    end else
    if Length(MimeLabel.Value) = 0 then
      MimeLabel.Value := sItem else
      MimeLabel.Value := MimeLabel.Value + ';' + sItem;
  until i = 0;
end;

end.
