unit UmMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  button, ExtCtrls, Grids, DBGrids, Db, StdCtrls, SilVmDataRowset,

  Sil,
  SilData, Menus;

type
  TfoMain = class(TForm)
    Panel1: TPanel;
    Panel4: TPanel;
    btOpen: TButtonEx;
    brExit: TButtonEx;
    Panel7: TPanel;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    btCreateTable: TButtonEx;
    btSaveAs: TButtonEx;
    btClose: TButtonEx;
    Panel2: TPanel;
    Splitter2: TSplitter;
    btCreateIndex: TButtonEx;
    btDeleteIndex: TButtonEx;
    btCreateField: TButtonEx;
    btDeleteField: TButtonEx;
    btFieldUp: TButtonEx;
    btFieldDown: TButtonEx;
    btChangeField: TButtonEx;
    btChangeIndex: TButtonEx;
    sgFields: TStringGrid;
    sgIndexes: TStringGrid;
    btSaveTable: TButtonEx;
    procedure brExitClick(Sender: TObject);
    procedure btOpenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btCreateTableClick(Sender: TObject);
    procedure btSaveAsClick(Sender: TObject);
    procedure btFieldsClick(Sender: TObject);
    procedure btIndexesClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure btCreateFieldClick(Sender: TObject);
    procedure lbTablesClick(Sender: TObject);
    procedure btChangeFieldClick(Sender: TObject);
    procedure btChangeIndexClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btDeleteFieldClick(Sender: TObject);
    procedure btSaveTableClick(Sender: TObject);
    procedure btCreateIndexClick(Sender: TObject);
    procedure btDeleteIndexClick(Sender: TObject);
  private
    FPath: String;
    FFile: String;
    FCurrent: IDataRowset;
    FModified: Boolean;
    procedure DoRead;
    procedure DoClear;
    function DoSave: Boolean;
    function DoClose: Boolean; reintroduce;
    procedure DoRealSave;
    procedure DoOpen(const Name: String);
  public
    { Public declarations }
  end;

var
  foMain: TfoMain;

implementation

uses UmField;

{$R *.DFM}

procedure TfoMain.FormCreate(Sender: TObject);
begin
  FPath := Sil.OS.Process.Current.Info.Path;
end;

procedure TfoMain.FormDestroy(Sender: TObject);
begin
  FCurrent := nil;
end;

procedure TfoMain.DoOpen(const Name: String);
begin
  DoClear;

  FFile := Name;
  FModified := false;
  FPath := Sil.OS.FileSystem.GetFilePath(FFile);

  FCurrent := SilData.Tk.OpenFile(FFile);
  DoRead;
end;

procedure TfoMain.btOpenClick(Sender: TObject);
begin
  OpenDialog.Title := 'Open table...';
  OpenDialog.InitialDir := FPath;

  if OpenDialog.Execute then
    DoOpen(OpenDialog.FileName);
end;

procedure TfoMain.btCreateTableClick(Sender: TObject);
var
  Def: IDataRowsetDef;
  sName: String;
begin
  SaveDialog.Title := 'Create table...';
  SaveDialog.InitialDir := FPath;

  if SaveDialog.Execute then
  begin
    DoClear;

    FFile := SaveDialog.FileName;
    FModified := false;
    sName := Sil.OS.FileSystem.GetFileName(FFile);
    FPath := Sil.OS.FileSystem.GetFilePath(FFile);

    Def := SilData.Tk.CreateFile(FFile);
    FCurrent := Def.Rowset;
  end;
end;

procedure TfoMain.btSaveAsClick(Sender: TObject);
begin
//
end;

function TfoMain.DoClose: Boolean;
begin
  if FModified then
    Result := DoSave else
    Result := true;

  if Result then DoClear;
end;

procedure TfoMain.btCloseClick(Sender: TObject);
begin
  DoClose;
end;


procedure TfoMain.btFieldsClick(Sender: TObject);
begin
//
end;

procedure TfoMain.btIndexesClick(Sender: TObject);
begin
//
end;

procedure TfoMain.brExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfoMain.btCreateFieldClick(Sender: TObject);
var
  Form: TfoField;
  i: Integer;
begin
  Form := TfoField.Create(Self);

  if Form.ShowModal = mrOk then
  begin
    i := sgFields.RowCount;

    if (i = 2) and Str.IsEmpty(sgFields.Cells[0, 1]) then
      Dec(i) else
      sgFields.RowCount := sgFields.RowCount + 1;

    sgFields.Cells[0, i] := Form.edName.Text;
    sgFields.Cells[1, i] := Form.cbType.Text;
    sgFields.Cells[2, i] := Form.edSize.Text;

    sgFields.Row := i;
    FModified := true;
  end;
end;

procedure TfoMain.btChangeFieldClick(Sender: TObject);
var
  Form: TfoField;
  i: Integer;
begin
  Form := TfoField.Create(Self);
  i := sgFields.Row;

  Form.edName.Text := sgFields.Cells[0, i];
  Form.cbType.ItemIndex := Form.cbType.Items.IndexOf(sgFields.Cells[1, i]);
  Form.edSize.Text := sgFields.Cells[2, i];

  if Form.ShowModal = mrOk then
  begin
    sgFields.Cells[0, i] := Form.edName.Text;
    sgFields.Cells[1, i] := Form.cbType.Text;
    sgFields.Cells[2, i] := Form.edSize.Text;
    FModified := true;
  end;
end;

procedure TfoMain.btDeleteFieldClick(Sender: TObject);
var
  i1, i2: Integer;
begin
  if Application.MessageBox(PChar('Delete field?'), PChar(Caption), mb_IconQuestion or mb_OkCancel) <> IdOk then Exit;

  for i1 := sgFields.Row to sgFields.RowCount - 2 do
    for i2 := 0 to sgFields.ColCount - 1 do
      sgFields.Cells[i2, i1] := sgFields.Cells[i2, i1 + 1];

  if sgFields.RowCount = 2 then
  begin
    for i1 := 0 to sgFields.ColCount - 1 do
      sgFields.Cells[i1, 1] := '';
  end else
    sgFields.RowCount := sgFields.RowCount - 1;

  FModified := true;
end;

procedure TfoMain.DoClear;
var
  i: Integer;
begin
  sgFields.RowCount := 2;
  for i := 0 to sgFields.ColCount - 1 do
    sgFields.Cells[i, 1] := '';

  sgIndexes.RowCount := 2;
  for i := 0 to sgIndexes.ColCount - 1 do
    sgIndexes.Cells[i, 1] := '';

  FCurrent := nil;
  FModified := false;
end;

procedure TfoMain.DoRead;
var
  e1: IEnumerator;
  Field: IFieldAccess;
  Index: IDataIndex;
  i: Integer;
  Opt: TDataIndexOption;
  sOpt: String;
begin
  sgFields.ColCount := 3;

  if FCurrent.Fields.Count > 0 then
    sgFields.RowCount := FCurrent.Fields.Count + 1;

  sgFields.Cells[0, 0] := 'Field Name';
  sgFields.Cells[1, 0] := 'Type';
  sgFields.Cells[2, 0] := 'Size';

  while FCurrent.Fields.Enumerate(e1, Field) do
  begin
    i := e1.Iteration + 1;
    sgFields.Cells[0, i] := Field.Name;
    sgFields.Cells[1, i] := Sil.Enum.Name(TypeInfo(TDataFieldType), Ord(Field.DataType), 'ft');
    sgFields.Cells[2, i] := Int.ToStr(Field.Size);
  end;

  sgIndexes.ColCount := 3;

  if FCurrent.Indexes.Count > 1 then
    sgIndexes.RowCount := FCurrent.Indexes.Count;

  sgIndexes.Cells[0, 0] := 'Index Name';
  sgIndexes.Cells[1, 0] := 'Fields';
  sgIndexes.Cells[2, 0] := 'Options';

  while FCurrent.Indexes.Enumerate(e1, Index) do
    if Str.NotEmpty(Index.Name) and (Index.Name[1] <> '$') then
    begin
      i := e1.Iteration + 1;
      sgIndexes.Cells[0, i] := Index.Name;
      sgIndexes.Cells[1, i] := Index.FieldList;
      sOpt := '';

      for Opt := Low(TDataIndexOption) to High(TDataIndexOption) do
        if Opt in Index.Options then
          sOpt := sOpt + Sil.Enum.Name(TypeInfo(TDataIndexOption), Ord(Opt), 'ix') + ',';

      Str.Delete(sOpt, -1);
      sgIndexes.Cells[2, i] := sOpt;
    end;
end;

function TfoMain.DoSave: Boolean;
var
  i: Integer;
begin
  i := Application.MessageBox(PChar('Save Changes for table ' + FFile + '?'), PChar(Caption), Mb_IconQuestion or Mb_YesNoCancel);
  Result := i <> IdCancel;
  if i = IdYes then DoRealSave;
end;

procedure TfoMain.DoRealSave;

  procedure DoDump;
  var
    Def: IDataRowsetDef;
    i, iPos: Integer;
    dType: TDataFieldType;
    Opt: TDataIndexOption;
    Opts: TDataIndexOptions;
    sItem: String;
  begin
    Def := SilData.Tk.CreateFile(FFile + '.tmp');

    for i := 1 to sgFields.RowCount - 1 do
    begin
      dType := TDataFieldType(Sil.Enum.Value(TypeInfo(TDataFieldType), sgFields.Cells[1, i], 'ft'));
      Def.Fields.CreateItem(sgFields.Cells[0, i], dType, Str.ToInt(sgFields.Cells[2, i], 0));
    end;

    for i := 1 to sgIndexes.RowCount - 1 do
      if Str.NotEmpty(sgIndexes.Cells[0, i]) then
      begin
        Opts := [];
        iPos := 0;
        repeat
          sItem := Str.Token(sgIndexes.Cells[2, i], ',', iPos);
          if Str.NotEmpty(sItem) then
          begin
            Opt := TDataIndexOption(Sil.Enum.Value(TypeInfo(TDataIndexOption), sItem, 'ix'));
            Include(Opts, Opt);
          end;
        until iPos = 0;

        Def.Indexes.CreateItem(sgIndexes.Cells[0, i], sgIndexes.Cells[1, i], Opts);
      end;

    Def.Build(Sil.OS.FileSystem.GetFileName(FFile));

    if FCurrent <> nil then
    begin
      Def.Rowset.AppendRecords(FCurrent);
      FCurrent := nil;
    end;
  end;

begin
  DoDump;
  Sil.OS.FileSystem.MoveFile(FFile, FFile + '.back');
  Sil.OS.FileSystem.MoveFile(FFile + '.tmp', FFile);
end;

procedure TfoMain.lbTablesClick(Sender: TObject);
begin
  if not DoSave then Exit;
  DoRead;
end;

procedure TfoMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := DoClose;
end;

procedure TfoMain.btSaveTableClick(Sender: TObject);
begin
  DoSave;
end;

procedure TfoMain.btCreateIndexClick(Sender: TObject);
begin
//
end;

procedure TfoMain.btChangeIndexClick(Sender: TObject);
begin
//
end;

procedure TfoMain.btDeleteIndexClick(Sender: TObject);
var
  i1, i2: Integer;
begin
  if Application.MessageBox(PChar('Delete index?'), PChar(Caption), mb_IconQuestion or mb_OkCancel) <> IdOk then Exit;

  for i1 := sgIndexes.Row to sgIndexes.RowCount - 2 do
    for i2 := 0 to sgIndexes.ColCount - 1 do
      sgIndexes.Cells[i2, i1] := sgIndexes.Cells[i2, i1 + 1];

  if sgIndexes.RowCount = 2 then
  begin
    for i1 := 0 to sgIndexes.ColCount - 1 do
      sgIndexes.Cells[i1, 1] := '';
  end else
    sgIndexes.RowCount := sgIndexes.RowCount - 1;

  FModified := true;
end;

end.
