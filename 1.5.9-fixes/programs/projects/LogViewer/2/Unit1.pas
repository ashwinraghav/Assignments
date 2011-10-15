unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,

  SIL,
  SilVcl, Menus, WTaskBar, AppEvnts;

type
  TForm1 = class (
    // extends
    TForm,
    // implements
    IRunnable)
    Memo: TMemo;
    Popup: TPopupMenu;
    itLogState: TMenuItem;
    Minimizar1: TMenuItem;
    itRestore: TMenuItem;
    Taskbar: TWinTaskbar;
    itAlwaysOnTop: TMenuItem;
    itClear: TMenuItem;
    Salir1: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure itLogStateClick(Sender: TObject);
    procedure Minimizar1Click(Sender: TObject);
    procedure RestoreClick(Sender: TObject);
    procedure itAlwaysOnTopClick(Sender: TObject);
    procedure itClearClick(Sender: TObject);
    procedure Salir1Click(Sender: TObject);
    procedure PopupPopup(Sender: TObject);
  protected // IRunnable
    procedure Run(const Thread: IThread);
  private
    FThread: IThread;
    FFileName: String;
    FPath: String;
    FOffset: LongWord;
    FMask: String;
    FDiscard: Boolean;
    FInterval: LongWord;
    FLines: LongWord;
    procedure DoAddLine(const Sender: IUnknown; const Ref);
    procedure DoRead;
    procedure Minimized(Sender: TObject);
    procedure Restored(Sender: TObject);
  public
    property Interval: LongWord read FInterval write FInterval;
    property Lines: LongWord read FLines write FLines;
  end;

var
  Form1: TForm1;

const
  LAST_CHUNK = 1024 * 4;

implementation

uses SilLiKey, SilOiThread;

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FInterval := 1000;
  FLines := 200; 
  
  Application.OnMinimize := Minimized;
  Application.OnRestore := Restored;
  FFileName := ParamStr(1);
  if Length(FFileName) = 0 then Exit;

  FPath := Sil.OS.FileSystem.GetFilePath(FFileName);

  if ParamCount > 1 then // @LC: si no viene la máscara, toma la extensión del archivo
    FMask := ParamStr(2);

  Caption := 'Tracing file: ...';
  FOffset := 0;
  FThread := Sil.OS.Thread.Spawn(Self);
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  Conf: INamedKey;
begin
  if FThread <> nil then
  begin
    FThread.Termination.Signal;
    if FThread.IsSuspended then FThread.Resume;
    Sil.OS.Wait.Single(FThread, INFINITE, true);
  end;

  Conf := Sil.OS.Registry.Open('$System\SOFTWARE\Siderca\fcv', true);

  with Conf.Values do
  begin
    WriteInteger('Left', Left);
    WriteInteger('Top', Top);
    WriteInteger('Width', Width);
    WriteInteger('Height', Height);
    WriteString('WindowColor', ColorToString(Self.Color));
    WriteString('FontColor', ColorToString(Self.Font.Color));
    WriteString('FontName', Self.Font.Name);
    WriteInteger('FontSize', Self.Font.Size);
    WriteBoolean('StayOnTop', Self.FormStyle = fsStayOnTop);
    WriteInteger('Interval', Self.Interval);
    WriteInteger('Lines', Self.Lines);
  end;
end;

procedure TForm1.DoAddLine(const Sender: IUnknown; const Ref);
begin
  try
    memo.Lines.BeginUpdate;

    while memo.Lines.Count > Integer(FLines) do
      memo.Lines.Delete(0);
  finally
    memo.Lines.EndUpdate;
  end;

  memo.lines.add(PChar(@Ref));
end;

procedure TForm1.DoRead;
var
  sLine, sName, sLast: String;
  Archive: ITextFile;
  List: IFileInfoList;
begin
  try
    Archive := OS.FileSystem.OpenTextFile(FFileName, fmAccessRead, fmShareReadWrite, true);

    if FOffset <= Archive.Stream.Size then
      Archive.Stream.Position := FOffset else
      Archive.Stream.Position := 0;

    Vcl.SetProp.Value(Self, 'Caption', 'Tracing file: ' + FFileName);
  except
    Vcl.SetProp.Value(Self, 'Caption', 'Error with file: ' + FFileName);
  end;

  if Archive <> nil then
  begin
    if FDiscard and (Archive.Stream.Size > LAST_CHUNK) then
    begin
      FDiscard := false;
      Archive.Stream.Seek(-LAST_CHUNK, soFromEnd);
      Archive.Stream.ReadLn(sLine);
    end;

    while not FThread.Termination.IsSignaled and Archive.Stream.ReadLn(sLine) do
      Sil.OS.Thread.Current.SyncCall(DoAddLine, sLine[1]);

    FOffset := Archive.Stream.Position;
  end;

  if Str.IsEmpty(sLine) and Str.NotEmpty(FMask) then
  begin
    sName := Sil.OS.FileSystem.GetFileName(FFileName);
    List := Sil.OS.FileSystem.GetList(FPath + FMask);

    Sil.Sort.Default(List);
    sLast := List.Items[List.Count - 1].Name;

    if (sName <> sLast) and Str.NotEmpty(sLast) then
    begin
      FFileName := FPath + sLast;
      FOffset := 0;
      //FDiscard := true;
    end;
  end;
end;

procedure TForm1.Run(const Thread: IThread);
begin
  FDiscard := true;

  repeat
    DoRead;
  until Thread.Termination.WaitFor(FInterval) <> wrTimeout;
end;

procedure TForm1.itLogStateClick(Sender: TObject);
var
  NewState: Boolean;
begin
  NewState := not itLogState.Checked;
  itLogState.Checked := NewState;
  if NewState then
    FThread.Suspend else
    FThread.Resume;
end;

procedure TForm1.Minimizar1Click(Sender: TObject);
begin
  Application.Minimize;
end;

procedure TForm1.RestoreClick(Sender: TObject);
begin
  Application.Restore;
end;

procedure TForm1.Minimized(Sender: TObject);
begin
  Taskbar.Icon := Self.Icon; 
  Taskbar.ShowIcon := True;
  Taskbar.Hint := Self.Caption;
  //Hide;
  ShowWindow(Application.Handle, SW_HIDE);
end;

procedure TForm1.Restored(Sender: TObject);
begin
  Taskbar.ShowIcon := False;
  Show;
end;

procedure TForm1.itAlwaysOnTopClick(Sender: TObject);
begin
  itAlwaysOnTop.Checked := not itAlwaysOnTop.Checked;
  if itAlwaysOnTop.Checked then
    FormStyle := fsStayOnTop else
    FormStyle := fsNormal;
end;

procedure TForm1.itClearClick(Sender: TObject);
begin
  Memo.Lines.Clear;
end;

procedure TForm1.Salir1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.PopupPopup(Sender: TObject);
begin
  itAlwaysOnTop.Checked := (FormStyle = fsStayOnTop);
  itLogState.Checked := FThread.IsSuspended;
end;

end.
