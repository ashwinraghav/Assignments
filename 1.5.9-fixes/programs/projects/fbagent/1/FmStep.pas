unit FmStep;

interface

{$include Defines.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, DB, IBDatabase, IBCustomDataSet, IBQuery,
  DBCtrls,

  Sil;

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
    edUserName: TEdit;
    edUserPassword: TEdit;
    meSqlCommand: TMemo;
    btAccept: TButton;
    btCancel: TButton;
    cbEnabled: TCheckBox;
    edName: TEdit;
    procedure btCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbActionKindClick(Sender: TObject);
    procedure btAcceptClick(Sender: TObject);
  private
    FActionKinds: TStringArray;
    FActionKind: String;
    FIsChange: Boolean;
    FTaskId: Integer;
    FStepId: Integer;
    procedure DoParseXML(const Buffer: String);
    procedure DoReadApplication(const Parameters: IParameters; const Command: String);
    procedure DoReadSql(const Parameters: IParameters; const Command: String);
    function DoGenXML: String;
    procedure DoWriteApplication(const Tag: IXmlTag);
    procedure DoWriteSql(const Tag: IXmlTag);
  public
    procedure SetParams(TaskId: Integer; quStep: TIBQuery);
  end;

var
  foStep: TfoStep;

implementation

uses
  DmAgent;

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

procedure TfoStep.FormCreate(Sender: TObject);
begin
  Str.ArraySet(FActionKinds, AActionKind);

  cbActionKindClick(nil);

  cbOnSuccess.ItemIndex := 0;
  cbOnFailure.ItemIndex := cbOnFailure.Items.Count - 1;
  cbActionKind.ItemIndex := 0;
  nbActionKind.PageIndex := 0;
end;

procedure TfoStep.DoReadSql(const Parameters: IParameters; const Command: String);
begin
  edSqlConnection.Text := Parameters['connection'];
  edUserName.Text := Parameters['user'];
  edUserPassword.Text := Parameters['password'];
  meSqlCommand.Text := Command;
end;

procedure TfoStep.DoReadApplication(const Parameters: IParameters; const Command: String);
begin
  edSuccessCode.Text := Parameters['exitcode'];
  meCommandLine.Text := Command;
end;

procedure TfoStep.DoParseXML(const Buffer: String);
var
  Enum: IEnumerator;
  Tree: IXmlTree;
  Root, Tag: IXmlTag;
  Node: IXmlNode;
  Stream: IMemoryStream;
  Command: String;
  Parameters: IParameterList;
begin
  Stream := Sil.Stream.Memory(Buffer);
  Tree := Sil.XML.ReadStream(Stream);
  Parameters := Sil.List.Parameters;
  Root := Tree.Root.AsTag;

  if Assigned(Root) then
  begin
    FActionKind := Str.ToLower(Str.Trim(Root.Arguments.ReadString('type')));

    Tag := Root.GetTag('command', true);
    Command := Tag.Data.Text;

    Tag := Root.GetTag('params', true);

    while Tag.Childs.Enumerate(Enum, Node) do
      if Node.NodeKind = nkTag then
      begin
        Tag := Node.AsTag;
        Parameters[Tag.Name] := Tag.Data.Text;
      end;

    if Str.TextCompare(FActionKind, 'sql') = 0 then
      DoReadSql(Parameters, Command) else
    if Str.TextCompare(FActionKind, 'application') = 0 then
      DoReadApplication(Parameters, Command);
  end;
end;

procedure TfoStep.DoWriteSql(const Tag: IXmlTag);
begin
  with Tag.Childs.Add(nkTag).AsTag do
  begin
    TagKind := tkBlock;
    Name := 'connection';
    Data.Text := edSqlConnection.Text;
  end;

  with Tag.Childs.Add(nkTag).AsTag do
  begin
    TagKind := tkBlock;
    Name := 'user';
    Data.Text := edUserName.Text;
  end;

  with Tag.Childs.Add(nkTag).AsTag do
  begin
    TagKind := tkBlock;
    Name := 'password';
    Data.Text := edUserPassword.Text;
  end;
end;

procedure TfoStep.DoWriteApplication(const Tag: IXmlTag);
begin
  with Tag.Childs.Add(nkTag).AsTag do
  begin
    TagKind := tkBlock;
    Name := 'exitcode';
    Data.Text := edSuccessCode.Text;
  end;
end;

function TfoStep.DoGenXML: String;
var
  Tree: IXmlTree;
  Tag: IXmlTag;
  Command: String;
  Stream: IMemoryStream;
begin
  Tree := Sil.Xml.Tree;

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

    if Str.TextCompare(FActionKind, 'sql') = 0 then
    begin
      Command := meSqlCommand.Text;
      DoWriteSql(Tag);
    end else
    if Str.TextCompare(FActionKind, 'application') = 0 then
    begin
      Command := meCommandLine.Text;
      DoWriteApplication(Tag);
    end;

    with Childs.Add(nkTag).AsTag do
    begin
      TagKind := tkBlock;
      Name := 'command';
      Data.Text := Command;
    end;
  end;

  Stream := Sil.Stream.Memory;
  Tree.Write(Stream, false);
  Stream.Position := 0;

  SetLength(Result, Stream.Size);
  Stream.Read(Result[1], Length(Result));
end;

procedure TfoStep.SetParams(TaskId: Integer; quStep: TIBQuery);
var
  i: Integer;
begin
  FIsChange := Assigned(quStep);
  FTaskId := TaskId;

  if FIsChange then
  begin
    FStepId := quStep.FieldByName('step_id').AsInteger;
    DoParseXML(quStep.FieldByName('data').AsString);

    i := Str.ArrayFind(FActionKinds, FActionKind);
    if i >= 0 then cbActionKind.ItemIndex := i;

    cbEnabled.Checked := quStep.FieldByName('enabled').AsInteger = 1;
    edName.Text := quStep.FieldByName('name').AsString;
    cbOnSuccess.ItemIndex := quStep.FieldByName('on_success').AsInteger;
    cbOnFailure.ItemIndex := quStep.FieldByName('on_failure').AsInteger;

    cbActionKindClick(nil);
  end;
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
  CmdStr := DoGenXML;
  daMain.StepCheck(FTaskId, FStepId, edName.Text, CmdStr, cbEnabled.Checked, cbOnSuccess.ItemIndex, cbOnFailure.ItemIndex);
end;

end.
