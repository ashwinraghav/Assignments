unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Sil, SilData;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Add: TButton;
    Memo: TMemo;
    Enum: TButton;
    Next: TButton;
    procedure FormCreate(Sender: TObject);
    procedure AddClick(Sender: TObject);
    procedure EnumClick(Sender: TObject);
    procedure NextClick(Sender: TObject);
  private
    FMap: IDataRowset;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses SilStDataRowset, SilSiDataAccess, SilBtInt;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  Def: IDataRowsetDef;
begin
  Def := SilData.Tk.Memory();

  with Def.Fields do
  begin
    CreateItem('ID', ftInteger);
    CreateItem('Name', ftWideString, 32);
  end;

  with Def.Indexes do
  begin
    CreateItem('ixID', 'ID', [ixUnique]);
    CreateItem('ixNameI', 'Name', [ixUnique, ixIgnoreCase]);
    CreateItem('ixNameU', 'Name', [ixUnique]);
  end;

  Def.Build();

  FMap := Def as IDataRowset;
end;

procedure TForm1.AddClick(Sender: TObject);
var
  X: Integer;
begin
  FMap.ActiveIndexName := 'ixID';
  FMap.Last;
  
  X := Str.ToInt(Edit1.Text);

  FMap.Append;
  FMap.Fields['ID'].AsInteger := X;
  FMap.Fields['Name'].AsString := 'Item' + Int.ToStr(x);
  FMap.Post;
end;

procedure TForm1.EnumClick(Sender: TObject);
var
  ID: Integer;
  Name: string;
begin
  Memo.Clear;
  
  FMap.First;

  while not FMap.IsEof do
  begin
    ID := FMap.Fields['ID'].AsInteger;
    Name := FMap.Fields['Name'].AsString;
    Memo.Lines.Add(Str.Format('%d: id = %d, name = %s', [FMap.CurrentRecord, ID, Name]));
    FMap.Next;
  end;
  
end;

procedure TForm1.NextClick(Sender: TObject);
var
  X: Integer;
begin
  FMap.ActiveIndexName := 'ixID';
  FMap.Last;
  X := FMap.Fields['ID'].AsInteger;
  Caption := Int.ToStr(X);
end;

end.
