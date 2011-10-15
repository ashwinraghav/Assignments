{********************************************************************************
 *                  Standard Interface Library (SIL)                            *
 *                                                                              *
 *       General purpose library whose design is based in STRONG                *
 *   use of interfaces.                                                         *
 *                                                                              *
 *                                                                              *
 *     Copyright (C) 2000 Mariano Podestá    marianop@intercom.com.ar           *
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

unit FmStep;

interface

{$include Defines.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, DB, IBDatabase, IBCustomDataSet, IBQuery,
  DBCtrls, ComCtrls,

  Sil,
  SilXml,
  
  UfConfig;

type
  TfoStep = class(TForm)
    Label2: TLabel;
    Label3: TLabel;
    cbOnSuccess: TComboBox;
    cbOnFailure: TComboBox;
    gbAction: TGroupBox;
    Label4: TLabel;
    Label11: TLabel;
    cbActionKind: TComboBox;
    nbActionKind: TNotebook;
    Label5: TLabel;
    Label6: TLabel;
    meCommandLine: TMemo;
    edSuccessCode: TEdit;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    edSqlConnection: TEdit;
    edBakSqlConnection: TEdit;
    edBakUserName: TEdit;
    edUserName: TEdit;
    edUserPassword: TEdit;
    meSqlCommand: TMemo;
    btAccept: TButton;
    btCancel: TButton;
    cbEnabled: TCheckBox;
    edName: TEdit;
    edBakFile: TEdit;
    cbBakExternalFilesAsTables: TCheckBox;
    cbBakGarbageCollection: TCheckBox;
    cbBakIgnoreBadChecksums: TCheckBox;
    cbBakIgnoreLimboTransactions: TCheckBox;
    cbBakTransportable: TCheckBox;
    Label1: TLabel;
    edBakLog: TEdit;
    edBakUserPassword: TEdit;
    paSQL: TPanel;
    paCommand: TPanel;
    paBackup: TPanel;
    btBakFile: TButton;
    btBakLog: TButton;
    OpenDialog: TOpenDialog;
    Label12: TLabel;
    edBakCount: TEdit;
    udBakCount: TUpDown;
    Label17: TLabel;
    cbSqlIsolation: TComboBox;
    btSqlIsolation: TButton;
    procedure btCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbActionKindClick(Sender: TObject);
    procedure btAcceptClick(Sender: TObject);
    procedure OnActionDropDown(Sender: TObject);
    procedure btBakFileClick(Sender: TObject);
    procedure btBakLogClick(Sender: TObject);
    procedure ChangeControl(Sender: TObject);
  private
    FActionKinds: TStringArray;
    FActionKind: String;
    FIsChange: Boolean;
    FTaskId: Integer;
    FStepId: Integer;
    FStepCount: Integer;
    FOrder: Integer;
    FLoaded: Boolean;
    FChanged: Boolean;
    procedure DoChanged;
    procedure DoParseXML(const Buffer: String);
    procedure DoReadApplication(const Parameters: IParameters);
    procedure DoReadBackup(const Parameters: IParameters);
    procedure DoReadSql(const Parameters: IParameters);
    function DoGenXML(out Cmd: String): Boolean;
    function DoWriteApplication(const Tag: IXmlTag): Boolean;
    function DoWriteSql(const Tag: IXmlTag): Boolean;
    function DoWriteBackup(const Tag: IXmlTag): Boolean;
    function DoCheckDb(const Connection, User, Password: String): Boolean;
  public
    procedure SetParams(TaskId: Integer; quStep: TIBQuery; const Params: IParameters);
  end;

var
  foStep: TfoStep;

implementation

uses
  DmAgent, UtXml, SilBtVart;

{$R *.dfm}

(*)
<data type="application">
  <params>
    <exitcode>0</exitcode>
  </params>
  <command>calc.exe</command>
</data>

<data type="sql">
  <params>
    <connection>hcc2616:alarma</connection>
    <user>sysdba</user>
    <password>masterkey</password>
  </params>
  <command>execute procedure do_stuff</command>
</data>
(*)

function GetIsolation(Combo: TComboBox): String;
begin
  try
    if Combo.ItemIndex > 0 then
      Result := CIsolation[Combo.ItemIndex - 1] else
      Result := '';
  except
    Result := CIsolation[0];
  end;
end;

function SetIsolation(const Value: String): Integer;
var
  i: Integer;
begin
  for i := 0 to High(CIsolation) do
    if Sil.Text.IsEqual(CIsolation[i], Value) then
    begin
      Result := i + 1;
      Exit;
    end;

  Result := 0;
end;

procedure TfoStep.FormCreate(Sender: TObject);
begin
  Sil.Trace.Enter(Self, 'FormCreate');

  FChanged := false;
  FLoaded := false;

  edName.Text := 'Unnamed';
  Str.ArraySet(FActionKinds, AActionKind);

  cbActionKindClick(nil);

  cbOnSuccess.ItemIndex := 0;
  cbOnFailure.ItemIndex := cbOnFailure.Items.Count - 1;
  cbActionKind.ItemIndex := 0;
  nbActionKind.PageIndex := 0;
  FStepCount := 0;

  Sil.Trace.Leave;
end;

procedure TfoStep.DoChanged;
begin
  if FLoaded then FChanged := true;
end;

procedure TfoStep.DoReadSql(const Parameters: IParameters);
begin
  Sil.Trace.Enter(Self, 'DoReadSql');

  edSqlConnection.Text := Vart.ToStr(Parameters['connection']);
  edUserName.Text := Vart.ToStr(Parameters['user']);
  edUserPassword.Text := Vart.ToStr(Parameters['password']);
  meSqlCommand.Text := Vart.ToStr(Parameters['command']);
  cbSqlIsolation.ItemIndex := SetIsolation(Parameters['isolation']);

  Sil.Trace.Leave;
end;

procedure TfoStep.DoReadApplication(const Parameters: IParameters);
begin
  Sil.Trace.Enter(Self, 'DoReadApplication');

  edSuccessCode.Text := Vart.ToStr(Parameters['exitcode']);
  meCommandLine.Text := Vart.ToStr(Parameters['command']);

  Sil.Trace.Leave;
end;

procedure TfoStep.DoReadBackup(const Parameters: IParameters);
begin
  Sil.Trace.Enter(Self, 'DoReadBackup');

  edBakSqlConnection.Text := Vart.ToStr(Parameters['connection']);
  edBakUserName.Text := Vart.ToStr(Parameters['user']);
  edBakUserPassword.Text := Vart.ToStr(Parameters['password']);
  edBakFile.Text := Vart.ToStr(Parameters['backupfile']);
  udBakCount.Position := Vart.ToInt(Parameters['backupfilecount'], 1);
  cbBakExternalFilesAsTables.Checked := Vart.ToBool(Parameters['externalfilesastables']);
  cbBakGarbageCollection.Checked := Vart.ToBool(Parameters['garbagecollection']);
  cbBakIgnoreBadChecksums.Checked := Vart.ToBool(Parameters['ignorebadchecksums']);
  cbBakIgnoreLimboTransactions.Checked := Vart.ToBool(Parameters['ignorelimbotransactions']);
  cbBakTransportable.Checked := Vart.ToBool(Parameters['transportable']);
  edBakLog.Text := Vart.ToStr(Parameters['logfile']);

  Sil.Trace.Leave;
end;

procedure TfoStep.DoParseXML(const Buffer: String);
var
  Parameters: IParameterList;
begin
  Sil.Trace.Enter(Self, 'DoParseXML');

  if AgentXml.Command(Buffer, FActionKind, Parameters) then
  begin
    if Sil.Text.IsEqual(FActionKind, 'sql') then
      DoReadSql(Parameters) else
    if Sil.Text.IsEqual(FActionKind, 'application') then
      DoReadApplication(Parameters) else
    if Sil.Text.IsEqual(FActionKind, 'backup') then
      DoReadBackup(Parameters);
  end;

  Sil.Trace.Leave;
end;

procedure DoTag(const Tag: IXmlTag; const TagName, TagValue: String);
begin
  with Tag.Childs.Add(nkTag).AsTag do
  begin
    TagKind := tkBlock;
    Name := TagName;
    Data.Text := Str.Trim(TagValue);
  end;
end;

function TfoStep.DoWriteBackup(const Tag: IXmlTag): Boolean;
begin
  Sil.Trace.Enter(Self, 'DoWriteBackup');

  Result := DoCheckDb(edBakSqlConnection.Text, edBakUserName.Text, edBakUserPassword.Text);

  DoTag(Tag, 'connection', edBakSqlConnection.Text);
  DoTag(Tag, 'user', edBakUserName.Text);
  DoTag(Tag, 'password', edBakUserPassword.Text);
  DoTag(Tag, 'backupfile', edBakFile.Text);
  DoTag(Tag, 'backupfilecount', Int.ToStr(udBakCount.Position));
  DoTag(Tag, 'externalfilesastables', Int.ToStr(Ord(cbBakExternalFilesAsTables.Checked)));
  DoTag(Tag, 'garbagecollection', Int.ToStr(Ord(cbBakGarbageCollection.Checked)));
  DoTag(Tag, 'ignorebadchecksums', Int.ToStr(Ord(cbBakIgnoreBadChecksums.Checked)));
  DoTag(Tag, 'ignorelimbotransactions', Int.ToStr(Ord(cbBakIgnoreLimboTransactions.Checked)));
  DoTag(Tag, 'transportable', Int.ToStr(Ord(cbBakTransportable.Checked)));
  DoTag(Tag, 'logfile', edBakLog.Text);

  Sil.Trace.Leave;
end;

function TfoStep.DoWriteSql(const Tag: IXmlTag): Boolean;
begin
  Sil.Trace.Enter(Self, 'DoWriteSql');

  Result := DoCheckDb(edSqlConnection.Text, edUserName.Text, edUserPassword.Text);

  DoTag(Tag, 'connection', edSqlConnection.Text);
  DoTag(Tag, 'user', edUserName.Text);
  DoTag(Tag, 'password', edUserPassword.Text);
  DoTag(Tag, 'isolation', GetIsolation(cbSqlIsolation));

  Sil.Trace.Leave;
end;

function TfoStep.DoWriteApplication(const Tag: IXmlTag): Boolean;
begin
  Sil.Trace.Enter(Self, 'DoWriteApplication');

  DoTag(Tag, 'exitcode', edSuccessCode.Text);
  Result := true;

  Sil.Trace.Leave;
end;

function TfoStep.DoCheckDb(const Connection, User, Password: String): Boolean;
var
  Db: TIBDatabase;
begin
  Db := TIBDatabase.Create(nil);

  try
    try
      Db.LoginPrompt := false;
      Db.DatabaseName := Connection;
      Db.Params.Values['user_name'] := User;
      Db.Params.Values['password'] := Password;
      Db.Open;
      Db.Close;
    finally
      Db.Free;
    end;

    Result := true;
  except
    Result := Application.MessageBox(PChar(Str.Format('Unable to connect to database "%s". Continue anyway?', [Connection])), PChar(Caption), MB_ICONSTOP or MB_YESNO) = ID_YES;
  end;
end;

function TfoStep.DoGenXML(out Cmd: String): Boolean;
var
  Tree: IXmlTree;
  Tag: IXmlTag;
  Command: String;
  Stream: IMemoryStream;
begin
  Sil.Trace.Enter(Self, 'DoGenXML');

  Tree := SilXml.Tool.Tree;
  Result := false;

  with Tree.Root.AsTag do
  begin
    TagKind := tkBlock;
    Name := 'data';
    Arguments.WriteString('type', AActionKind[cbActionKind.ItemIndex]);

    Tag := Childs.Add(nkTag).AsTag;

    with Tag do
    begin
      TagKind := tkBlock;
      Name := 'params';
    end;

    if Sil.Text.IsEqual(FActionKind, 'sql') then
    begin
      Command := meSqlCommand.Text;
      Result := DoWriteSql(Tag);
    end else
    if Sil.Text.IsEqual(FActionKind, 'application') then
    begin
      Command := meCommandLine.Text;
      Result := DoWriteApplication(Tag);
    end else
    if Sil.Text.IsEqual(FActionKind, 'backup') then
    begin
      Command := '$BACKUP';
      Result := DoWriteBackup(Tag);
    end;

    with Childs.Add(nkTag).AsTag do
    begin
      TagKind := tkBlock;
      Name := 'command';
      Data.Text := Str.Trim(Command);
    end;
  end;

  Stream := Sil.Stream.Memory;
  Tree.Write(Stream, false);
  Stream.Position := 0;

  SetLength(Cmd, Stream.Size);
  Stream.Read(Cmd[1], Length(Cmd));

  Sil.Trace.Leave;
end;

procedure TfoStep.SetParams(TaskId: Integer; quStep: TIBQuery; const Params: IParameters);
var
  i: Integer;
begin
  Sil.Trace.Enter(Self, 'SetParams');

  FIsChange := Assigned(quStep);
  FTaskId := TaskId;

  FStepCount := Params['step_count'];
  FOrder := Params['step_order'];

  OnActionDropDown(cbOnSuccess);
  OnActionDropDown(cbOnFailure);

  if FIsChange then
  begin
    FStepId := quStep.FieldByName('step_id').AsInteger;
    DoParseXML(quStep.FieldByName('data').AsString);

    i := Str.ArrayFind(FActionKinds, FActionKind);
    if i >= 0 then cbActionKind.ItemIndex := i;

    cbEnabled.Checked := quStep.FieldByName('enabled').AsInteger = 1;
    edName.Text := quStep.FieldByName('name').AsString;
    cbOnSuccess.ItemIndex := quStep.FieldByName('on_success').AsInteger + 3;
    cbOnFailure.ItemIndex := quStep.FieldByName('on_failure').AsInteger + 3;

    cbActionKindClick(nil);
  end;

  FLoaded := true;

  Sil.Trace.Leave;
end;

procedure TfoStep.btCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfoStep.cbActionKindClick(Sender: TObject);
begin
  FActionKind := FActionKinds[cbActionKind.ItemIndex];
  nbActionKind.PageIndex := cbActionKind.ItemIndex;
end;

procedure TfoStep.btAcceptClick(Sender: TObject);
var
  CmdStr: String;
begin
  Sil.Trace.Enter(Self, 'btAcceptClick');

  if DoGenXML(CmdStr) then
    daAgent.StepCheck(FTaskId, FStepId, edName.Text, CmdStr, cbEnabled.Checked, FOrder, cbOnSuccess.ItemIndex, cbOnFailure.ItemIndex) else
    ModalResult := mrNone;

  Sil.Trace.Leave;
end;

procedure TfoStep.OnActionDropDown(Sender: TObject);
var
  Combo: TComboBox absolute Sender;
begin
  while Combo.Items.Count < 3 + FStepCount do
    Combo.Items.Add(Str.Format('Jump to step #%d', [Combo.Items.Count - 2]));
end;

procedure TfoStep.btBakFileClick(Sender: TObject);
begin
  OpenDialog.FileName := edBakFile.Text;

  if OpenDialog.Execute then
    edBakFile.Text := OpenDialog.FileName;
end;

procedure TfoStep.btBakLogClick(Sender: TObject);
begin
  OpenDialog.FileName := edBakLog.Text;

  if OpenDialog.Execute then
    edBakLog.Text := OpenDialog.FileName;
end;

procedure TfoStep.ChangeControl(Sender: TObject);
begin
  DoChanged;
end;

end.
