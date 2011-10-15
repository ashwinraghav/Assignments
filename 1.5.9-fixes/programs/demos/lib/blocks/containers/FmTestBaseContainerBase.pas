unit FmTestBaseContainerBase;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, StdCtrls,
  Dialogs, Sil, SilContainer;

type
  PDatito = ^RDatito;
  RDatito = record
    Text: string;
    Time: TDateTime;
  end;

  TFormTestBase = class(TForm)
    Vector: TButton;
    List: TButton;
    Add: TButton;
    edCount: TEdit;
    edAdd: TEdit;
    Clear: TButton;
    edClear: TEdit;
    Enum: TButton;
    edEnum: TEdit;
    procedure VectorClick(Sender: TObject);
    procedure ListClick(Sender: TObject);
    procedure AddClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure edCountExit(Sender: TObject);
    procedure ClearClick(Sender: TObject);
    procedure EnumClick(Sender: TObject);
  private
    FCounter: IPerformanceCounter;
    FBase: IBaseContainer;
    N: Integer;
  public
    { Public declarations }
  end;

var
  FormTestBase: TFormTestBase;

implementation

{$R *.dfm}

procedure TFormTestBase.FormCreate(Sender: TObject);
begin
  FCounter := Sil.Os.Performance.Create(); 
end;

procedure TFormTestBase.edCountExit(Sender: TObject);
begin
  N := Sil.Str.ToInt(edCount.Text);
end;

procedure TFormTestBase.VectorClick(Sender: TObject);
begin
  FBase := ckVector.Create(Handler.Create(TypeInfo(Double)));
end;

procedure TFormTestBase.ListClick(Sender: TObject);
begin
  FBase := ckList.Create(Handler.Create(TypeInfo(Double)));
end;

procedure TFormTestBase.AddClick(Sender: TObject);
begin
  FBase.Clear;
  
  FCounter.Reset;

  FBase.Add(nil, N);

  edAdd.Text := Sil.Float.ToStr(FCounter.ToSeconds(), 10, 6);
end;

procedure TFormTestBase.ClearClick(Sender: TObject);
begin
  FCounter.Reset;
  FBase.Clear;
  edClear.Text := Sil.Float.ToStr(FCounter.ToSeconds(), 10, 6);
end;

procedure TFormTestBase.EnumClick(Sender: TObject);
var
  I: HItem;
begin
  FCounter.Reset;
  
  I := HNull;
  while FBase.GetNext(I) do
    FBase.GetData(I);

  edEnum.Text := Sil.Float.ToStr(FCounter.ToSeconds(), 10, 6);
end;

end.
