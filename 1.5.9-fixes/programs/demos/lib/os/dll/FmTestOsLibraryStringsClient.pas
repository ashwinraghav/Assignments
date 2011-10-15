unit FmTestOsLibraryStringsClient;

interface

uses
  SysUtils, Classes, Controls, Forms, StdCtrls,
  Sil, UiTestStringsStub;

type
  TFormTestLibraryClient = class(TForm, IUnknown, IListEvents)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FTestDLL: ITestStringsDll;
  private
    procedure OnListAdd(const List: IList; Index: Integer);
    procedure OnListInsert(const List: IList; Index: Integer);
    procedure OnListDelete(const List: IList; Index: Integer);
  public
    { Public declarations }
  end;

var
  FormTestLibraryClient: TFormTestLibraryClient;

implementation

uses
  UmTestStringsStub;

{$R *.DFM}

procedure TFormTestLibraryClient.FormCreate(Sender: TObject);
begin
  FTestDLL := TTestStringsDll.Create('TestStringsDll.dll');
end;

procedure TFormTestLibraryClient.FormDestroy(Sender: TObject);
begin
  FTestDll := nil;
end;

procedure TFormTestLibraryClient.Button1Click(Sender: TObject);
var
  l: IStringList;
begin
  l := FTestDLL.CreateStringList();
  Sil.Sink.Connect(l, Self);
  l.Add('uno');
  l.Add('dos');
  l.Add('tres');
end;

procedure TFormTestLibraryClient.OnListAdd(const List: IList; Index: Integer);
begin
  memo1.lines.add('agrega');
end;

procedure TFormTestLibraryClient.OnListDelete(const List: IList; Index: Integer);
begin
  memo1.lines.add('borra');
end;

procedure TFormTestLibraryClient.OnListInsert(const List: IList; Index: Integer);
begin
  memo1.lines.add('inserta');
end;

end.
