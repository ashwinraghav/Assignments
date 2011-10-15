unit FmTestOsFileReadwrite;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Sil,
  StdCtrls;

type
  TFormTestFileReadwrite = class(TForm)
    btWriteFile: TButton;
    edFileName: TEdit;
    WriteMemo: TMemo;
    btReadFile: TButton;
    ReadMemo: TMemo;
    procedure btWriteFileClick(Sender: TObject);
    procedure btReadFileClick(Sender: TObject);
  private
  public
    { Public declarations }
  end;

var
  FormTestFileReadwrite: TFormTestFileReadwrite;

implementation

{$R *.DFM}

procedure TFormTestFileReadwrite.btWriteFileClick(Sender: TObject);
begin
  with OS.FileSystem.CreateFile(edFileName.Text) do
    Stream.Write(PChar(WriteMemo.Lines.Text)^, Length(WriteMemo.Lines.Text));
end;

procedure TFormTestFileReadwrite.btReadFileClick(Sender: TObject);
var
  Buffer: string;
begin
  ReadMemo.Lines.Clear;
  
  SetLength(Buffer, 1024);

  with OS.FileSystem.OpenFile(edFileName.Text) do
  begin
    Buffer[Stream.Read(Buffer[1], Length(Buffer)-1)] := #0;
    ReadMemo.Lines.Text := Buffer;
  end;
end;

end.
