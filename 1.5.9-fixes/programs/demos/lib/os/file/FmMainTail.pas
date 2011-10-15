unit FmMainTail;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,

  Sil;

type
  TFormMainTail = class (
    // extends
    TForm,
    // implements
    IRunnable )
    Memo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  protected // IRunnable
    procedure Run(const Thread: IThread);
  private
    FFile: ITextFile;
    FTermination: IEvent;
    FThread: IThread;
  private 
    procedure DoAddLine(const Sender: IUnknown; Param: Pointer);
  end;

var
  FormMainTail: TFormMainTail;

const
  LAST_CHUNK = 1024 * 4;

implementation

uses SilOtIpc;

{$R *.DFM}

procedure TFormMainTail.FormCreate(Sender: TObject);
var
  sFile: String;
begin
  sFile := ParamStr(1);
  if Length(sFile) = 0 then Exit;

  Caption := 'Tracing file: ' + sFile;

  FFile := Sil.OS.FileSystem.OpenTextFile(sFile, fmAccessRead, fmShareReadWrite, true);
  FTermination := Sil.OS.Ipc.Event(); 
  FThread := Sil.OS.Thread.Spawn(Self);
end;

procedure TFormMainTail.FormDestroy(Sender: TObject);
var
  Conf: INamedKey;
begin

  if Assigned(FThread) then
  begin
    FTermination.Signal;
    Sil.OS.Wait.Single(FThread, INFINITE);
  end;

  Conf := Sil.OS.Registry.Open('$System\SOFTWARE\SIL\tail', true);

  with Conf.Values do
  begin
    WriteInteger('Left', Left);
    WriteInteger('Top', Top);
    WriteInteger('Width', Width);
    WriteInteger('Height', Height);
  end;
end;

procedure TFormMainTail.DoAddLine(const Sender: IUnknown; Param: Pointer);
begin
  memo.lines.add(PChar(Param));
end;

procedure TFormMainTail.Run(const Thread: IThread);
var
  Change: IFilesystemChangeNotification;
  iSignal: Integer;
  sLine: String;
begin
  if FFile.Stream.Size > LAST_CHUNK then FFile.Stream.Seek(-LAST_CHUNK, soFromEnd);

  Change := Sil.OS.FileSystem.Notification(FFile.Info.Path);

  while Sil.OS.Wait.Any([Change, FTermination], INFINITE, iSignal) do
  begin
    if iSignal = 0 then
    begin
      while FFile.Stream.ReadLn(sLine) do Sil.OS.Thread.SyncCall(DoAddLine, PChar(sLine));
      Change.Reset;
    end else
      Break;
  end;
end;

end.
