unit FmTestOsFileTextReader;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,

  Sil, SilTool;

type
  TFormTestTextReader = class(TForm)
    Memo1: TMemo;
    btReadText: TButton;
    procedure btReadTextClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormTestTextReader: TFormTestTextReader;

implementation

{$R *.DFM}

procedure TFormTestTextReader.btReadTextClick(Sender: TObject);
var
  Source: IFile;
  Reader: IPbPacketReader;
  sBuffer, sLine: String;
  i: Integer;
begin
  Source := OS.FileSystem.OpenFile('.\leer.txt');
  Reader := Sil.Tk.TextReader(Source.Stream);

  while Reader.Read(sBuffer) do
  begin
    i := 1;
    repeat
      sLine := Str.Token(sBuffer, ',', i);
      if Length(sLine) > 0 then memo1.lines.add(Str.Trim(sLine));
    until i = 0;
  end;
end;

end.

