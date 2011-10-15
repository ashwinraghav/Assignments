unit FmTestOsFileTextFile;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,
  Sil;

type
  TFormTestTextFile = class(TForm)
    btReadMemo: TButton;
    btWriteMemo: TButton;
    edMemo: TMemo;
    procedure btWriteMemoClick(Sender: TObject);
    procedure btReadMemoClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormTestTextFile: TFormTestTextFile;

implementation

{$R *.DFM}

procedure TFormTestTextFile.btWriteMemoClick(Sender: TObject);
var
  f: ITextFile;
  i: Integer;
begin
  f := Sil.OS.FileSystem.CreateTextFile(Sil.OS.Process.Current.Info.Path + 'test.txt');

  for i := 0 to edMemo.lines.Count - 1 do
    f.Stream.WriteLn(edMemo.lines[i]);
end;

procedure TFormTestTextFile.btReadMemoClick(Sender: TObject);
var
  f: ITextFile;
  s: String;
begin
  f := Sil.OS.FileSystem.OpenTextFile(Sil.OS.Process.Current.Info.Path + 'test.txt');

  edMemo.lines.Clear;
  while f.Stream.ReadLn(s) do
    edMemo.lines.add(s);
end;

end.
