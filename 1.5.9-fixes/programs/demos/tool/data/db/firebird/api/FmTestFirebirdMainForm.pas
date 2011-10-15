unit FmTestFirebirdMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ComCtrls,

  Sil, SilTool, SilData,
  SilSiFirebird, Grids, ValEdit, CheckLst, DB, IBCustomDataSet, IBDatabase;

type
  RConfig = record 
    UserName: string;
    Password: string;                              
    Database: string;
    Hostname: string;
    FileName: string;
  end;

  TForm1 = class(TForm)
    Connect: TButton;
    Disconnect: TButton;
    Start: TButton;
    Commit: TButton;
    Rollback: TButton;
    edSQL: TMemo;
    Prepare: TButton;
    Release: TButton;
    Display: TMemo;
    Describe: TButton;
    Execute: TButton;
    Schema: TButton;
    edUserName: TEdit;
    edPassword: TEdit;
    edDatabase: TEdit;
    btBackup: TButton;
    edHostName: TEdit;
    edFileName: TEdit;
    vaParams: TValueListEditor;
    edServerVersion: TEdit;
    btGetInfo: TButton;
    clInfoItems: TCheckListBox;
    btTest: TButton;
    edStatementKind: TEdit;
    procedure ConnectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DisconnectClick(Sender: TObject);
    procedure StartClick(Sender: TObject);
    procedure CommitClick(Sender: TObject);
    procedure RollbackClick(Sender: TObject);
    procedure PrepareClick(Sender: TObject);
    procedure ReleaseClick(Sender: TObject);
    procedure edUserNameExit(Sender: TObject);
    procedure edPasswordExit(Sender: TObject);
    procedure edDatabaseExit(Sender: TObject);
    procedure edHostNameExit(Sender: TObject);
    procedure edFileNameExit(Sender: TObject);
    procedure edSQLExit(Sender: TObject);
    procedure DescribeClick(Sender: TObject);
    procedure ExecuteClick(Sender: TObject);
    procedure SchemaClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btBackupClick(Sender: TObject);
    procedure btGetInfoClick(Sender: TObject);
  private
    FCounter: IPerformanceCounter;       
    FConfig: RConfig;
    FFirebird: IFbApplication;
    FSession: IFbSession;
    FTrans: IFbTransaction;
    FCmnd: IFbCommand;
    FCursors: TComponent;
  private
    procedure DoFillParams;
    function DoGetParams: IParameterList;
    function DoOpen(const Params: IParameterList): IFbCursor;
    procedure DoSave;
    procedure DoRowsetClosed(Sender: TObject);
    procedure DoRelease;                                   
    procedure DoShowResults(const Buffer: IFbBuffer);
  end;

var
  Form1: TForm1;

implementation

uses
  SilStFirebird,
  FmRowset, FmSchema, SilBtVart, SilLiParameters, SilBtTypeInfo;

{$R *.dfm}

// atenea:D:\DATA\DB\ALARMA3.GDB
// linux.apre.siderca.ot:/usr/data/fb/alarma3.gdb

procedure TForm1.FormCreate(Sender: TObject);
begin
  FCounter := Sil.Os.Performance.Create(); 
  FCursors := TComponent.Create(Self);
  FFirebird := Firebird.Application;

  with SilTool.Sv.Configuration.Open('firebird.ini', True).Get('connection', True) do
  begin
    FConfig.Database := ReadString('Database', '', True);
    FConfig.UserName := ReadString('User', 'SYSDBA', True);
    FConfig.Password := ReadString('Password', 'masterkey', True);
    FConfig.Hostname := ReadString('Hostname', '', True);
    FConfig.FileName := ReadString('FileName', '', True);
  end;

  edUserName.Text := FConfig.UserName;
  edPassword.Text := FConfig.Password;
  edDatabase.Text := FConfig.Database;
  edHostName.Text := FConfig.Hostname;
  edFileName.Text := FConfig.FileName;

  if Sil.Os.Filesystem.Exists('sql.dat') then
    edSQL.Lines.LoadFromFile('sql.dat');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Disconnect.Click;
  FreeAndNil(FCursors);
  FCounter := nil;
end;

procedure TForm1.ConnectClick(Sender: TObject);
var
  Params: IParameterList;
begin
  FCounter.Reset;

  Params := Sil.List.Parameters();
  Params[CDpbUserName] := edUserName.Text;
  Params[CDpbPassword] := edPassword.Text;

  FSession := FFirebird.Connect(edHostName.Text, edDatabase.Text, Params);

  Caption := 'connected: time ' + Sil.Float.ToStr(FCounter.ToMilliseconds()) + ' ms';

  Disconnect.Enabled := True;
  Connect.Enabled := False;
  Schema.Enabled := True;


end;

procedure TForm1.DisconnectClick(Sender: TObject);
begin
  FreeAndNil(FormSchema);

  Release.Click;
  
  FSession := nil;

  Caption := 'disconnected';
  Connect.Enabled := True;
  Disconnect.Enabled := False;
  Schema.Enabled := False;
end;

procedure TForm1.StartClick(Sender: TObject);
begin
  if not Assigned(FSession) then Connect.Click;
  FTrans := FSession.StartTransaction();
end;

procedure TForm1.CommitClick(Sender: TObject);
begin
  if Assigned(FTrans) then
  begin
    FTrans.Commit();
    FTrans := nil;
  end;
end;

procedure TForm1.RollbackClick(Sender: TObject);
begin
  if Assigned(FTrans) then
  begin
    FTrans.Rollback();
    FTrans := nil;
  end;
end;

procedure TForm1.PrepareClick(Sender: TObject);
begin
  if not Assigned(FSession) then Connect.Click;

  FCounter.Reset;  
  FCmnd := FSession.Statement(edSQL.Lines.Text).Prepare(FTrans);  
  Caption := 'prepared: time ' + Sil.Float.ToStr(FCounter.ToMilliseconds()) + ' ms';

  edStatementKind.Text := Sil.Typ.Enum.Name(TypeInfo(TFbStatementKind), Ord(FCmnd.Statement.Kind), 'fbsk');

  DoFillParams;

  Release.Enabled := True;
  Prepare.Enabled := False;
end;

procedure TForm1.ReleaseClick(Sender: TObject);
begin
  if Assigned(FCursors) then FCursors.DestroyComponents;
  DoRelease;
end;

procedure TForm1.DoRelease;
begin
  FCmnd := nil;
  Caption := 'unprepared';
  Prepare.Enabled := True;
  Release.Enabled := False;
end;

procedure TForm1.DoSave;
begin
  with SilTool.Sv.Configuration.Open('firebird.ini', True).Get('connection', True) do
  begin
    WriteString('Database', FConfig.Database);
    WriteString('User',     FConfig.UserName);
    WriteString('Password', FConfig.Password);
    WriteString('Hostname', FConfig.Hostname);
    WriteString('FileName', FConfig.FileName);
  end;
end;

procedure TForm1.edUserNameExit(Sender: TObject);
begin
  with Sender as TEdit do
    if Modified then
    begin
      FConfig.UserName := Text;
      DoSave;
    end;
end;

procedure TForm1.edPasswordExit(Sender: TObject);
begin
  with Sender as TEdit do
    if Modified then
    begin
      FConfig.Password := Text;
      DoSave;
    end;
end;

procedure TForm1.edDatabaseExit(Sender: TObject);
begin
  with Sender as TEdit do
    if Modified then
    begin
      FConfig.Database := Text;
      DoSave;
    end;
end;

procedure TForm1.edHostNameExit(Sender: TObject);
begin
  with Sender as TEdit do
    if Modified then
    begin
      FConfig.Hostname := Text;
      DoSave;
    end;
end;

procedure TForm1.edFileNameExit(Sender: TObject);
begin
  with Sender as TEdit do
    if Modified then
    begin
      FConfig.FileName := Text;
      DoSave;
    end;
end;

procedure TForm1.edSQLExit(Sender: TObject);
begin
  edSQL.Lines.SaveToFile('sql.dat');
end;

procedure TForm1.DescribeClick(Sender: TObject);
var
  Enum: IEnumerator;
  Item: IFbVariable;
begin
  if not Assigned(FCmnd) then Prepare.Click;

  if Assigned(FCmnd) then
  begin
    Display.Lines.Clear;
    
    Display.Lines.Add('Parameters:');
    with FCmnd.Statement.Parameters do
      while Enumerate(Enum, Item) do
        Display.Lines.Add(Sil.Str.Format('   %s: %s (%d)', [Item.Name, Item.Domain.Name, Item.Domain.Length]));

    Display.Lines.Add('Fields:');
    with FCmnd.Statement.Fields do
      while Enumerate(Enum, Item) do
        Display.Lines.Add(Sil.Str.Format('   %s: %s(%d)', [Item.Name, Item.Domain.Name, Item.Domain.Length]));
  end;
end;

procedure TForm1.ExecuteClick(Sender: TObject);
var
  Results: IFbBuffer;
begin
  if not Assigned(FCmnd) then Prepare.Click;
  if Assigned(FCmnd) then
  begin
    FCounter.Reset;
    case FCmnd.Statement.Kind of
      fbskSelect:
        TFormRowset.Show(DoOpen(DoGetParams()), FCursors, DoRowsetClosed);
      else
        if FCmnd.Execute(DoGetParams(), @Results) then
          DoShowResults(Results);
    end;
    Caption := 'ready: time ' + Sil.Float.ToStr(FCounter.ToMilliseconds()) + ' ms';
  end;
end;

procedure TForm1.DoFillParams;
var
  Stmt: IFbStatement;
  I: Integer;
  P: IFbVariable;
begin
  vaParams.Strings.Clear;

  Stmt := FCmnd.Statement;
  
  for I := 0 to Stmt.Parameters.Count - 1  do
  begin
    P := Stmt.Parameters.Items[I];
    vaParams.InsertRow(P.Name, '', True);
  end;
end;

function TForm1.DoGetParams: IParameterList;
var
  Name, Value: string;
  I: Integer;
begin
  if Assigned(FCmnd) and (FCmnd.Statement.Parameters.Count > 0) then
  begin
    if vaParams.Strings.Count > 0 then
    begin
      Result := Sil.List.Parameters();
      for I := 0 to vaParams.Strings.Count - 1 do
      begin
        Name := vaParams.Strings.Names[I];
        Value := vaParams.Strings.Values[Name];
        if Sil.Str.IsAssigned(Value) then
          Result[Name] := Value else
          Result[Name] := Sil.Vart.Null;
      end;
    end else
      Result := nil;
  end;
end;

function TForm1.DoOpen(const Params: IParameterList): IFbCursor;
begin
  Result := FCmnd.Open(Params); 
end;

procedure TForm1.SchemaClick(Sender: TObject);
begin
  if not Assigned(FormSchema) then FormSchema := TFormSchema.Create(FSession);
  FormSchema.Show;
end;

procedure TForm1.DoRowsetClosed(Sender: TObject);
begin
  DoRelease;
end;

procedure TForm1.DoShowResults(const Buffer: IFbBuffer);
var
  Enum: IEnumerator;
  Item: IFbData;
  Text: string;
begin
  with Buffer do
    while Enumerate(Enum, Item) do
      Sil.Str.Add(Text, '%30s = %s', [Item.Binding.Name, Item.AnsiString.Value]);
  Display.Lines.Text := Text;
end;

procedure TForm1.btBackupClick(Sender: TObject);
var
  ServMgr: IFbServiceManager;
  Params: IParameterList;
  Backup: IParameterList;
  Data: IParameterList;
  Text: Variant;
begin


  FCounter.Reset;

  Params := Sil.List.Parameters();
  Backup := Sil.List.Parameters();
  Data := Sil.List.Parameters();

  Params[CSpbUserName] := edUserName.Text;
  Params[CSpbPassword] := edPassword.Text;

  ServMgr := FFirebird.ServiceManager(edHostName.Text, Params);

  Backup[CBkpFileName] := edFileName.Text;
  Backup[CSpbVerbose] := True;

  ServMgr.Backup(edDatabase.Text, Backup);

  while True do
  begin
    Data := ServMgr.Query([CInfoLine], 60000);
    Text := Data[CInfoLine]; 
    if Sil.Str.IsAssigned(Text) then
      Display.Lines.Add(Text) else
      Break;
  end;

  Caption := 'connected: time ' + Sil.Float.ToStr(FCounter.ToMilliseconds()) + ' ms';
end;

procedure TForm1.btGetInfoClick(Sender: TObject);
var
  ServMgr: IFbServiceManager;
  Params: IParameterList;
  Data: IParameterList;
  Index: Integer;

  procedure FillData(const Parent: string; const Data: IParameters);
  var
    Enum: IEnumerator;
    Item: RParameter;
  begin
    with Data do
      while Enumerate(Enum, Item) do

        if Sil.Vart.IsObject(Item.Value) then
          FillData(Item.Name + '.', Sil.Vart.ToUnknown(Item.Value) as IParameters) else
          vaParams.InsertRow(Parent + Item.Name, Sil.Vart.ToStr(Item.Value), True);
  end;

begin
  Params := Sil.List.Parameters();
  Data := Sil.List.Parameters();


  Params[CSpbUserName] := edUserName.Text;
  Params[CSpbPassword] := edPassword.Text;
  ServMgr := FFirebird.ServiceManager(edHostName.Text, Params);


  with clInfoItems do
    for Index := 0 to Count - 1 do
      if Checked[Index] then
        Data[Items.Strings[Index]] := Sil.Vart.Unassigned;

  ServMgr.Query(Data);


  
  vaParams.Strings.Clear;

  FillData('', Data);
end;

end.



