unit FmTestOsFileClone;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,

  Sil, StdCtrls;

type
  TFormTestFileClone = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormTestFileClone: TFormTestFileClone;

implementation

{$R *.DFM}

procedure TFormTestFileClone.FormCreate(Sender: TObject);
var
  a, b: IFile;
  s, f: String;
begin
  f := 'FmTestOsFileClone.pas';
  a := Sil.OS.FileSystem.OpenFile(f, fmAccessReadWrite, fmShareReadWrite);
  b := Sil.Clone.Make(a) as IFile;

  SetLength(s, 50);
  a.Stream.Read(s[1], Length(s));
  memo1.lines.add(s);

  SetLength(s, 50);
  b.Stream.Read(s[1], Length(s));
  memo1.lines.add(s);
end;

end.
