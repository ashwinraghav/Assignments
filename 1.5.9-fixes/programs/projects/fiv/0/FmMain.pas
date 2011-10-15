unit FmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Menus,

  Sil, ExtCtrls;

type
  TfoMain = class(TForm)
    meMain: TMainMenu;
    miItem: TMenuItem;
    miExit: TMenuItem;
    miOpen: TMenuItem;
    odFile: TOpenDialog;
    Panel1: TPanel;
    meText: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure miOpenClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FFile: IFile;
    FReader: IReader;
    FMaxLines: Integer;
    FPos: TIntegerArray;
    procedure DoReadConfig;
    procedure DoWriteConfig;
    procedure DoOpenName(const FileName: String);
    procedure DoRead(Fwd: Boolean);
  public
  end;

var
  foMain: TfoMain;

implementation

{$R *.dfm}

procedure TfoMain.DoReadConfig;
var
  Root: INamedKey;
begin
  Root := Sil.OS.Registry.Open('$System\SOFTWARE\SIL\FileViewer', true);

  with Root.Keys.Get('Parameters', kpReadWrite, true) do
  begin
    Self.Top := Values.ReadInteger('Window.Top', 0, true);
    Self.Left := Values.ReadInteger('Window.Left', 0, true);
    Self.Height := Values.ReadInteger('Window.Height', 200, true);
    Self.Width := Values.ReadInteger('Window.Width', 200, true);
  end;
end;

procedure TfoMain.DoWriteConfig;
var
  Root: INamedKey;
begin
  Root := Sil.OS.Registry.Open('$System\SOFTWARE\SIL\FileViewer', true);

  with Root.Keys.Get('Parameters', kpReadWrite, true) do
  begin
    Values.WriteInteger('Window.Top', Self.Top);
    Values.WriteInteger('Window.Left', Self.Left);
    Values.WriteInteger('Window.Height', Self.Height);
    Values.WriteInteger('Window.Width', Self.Width);
  end;
end;

procedure TfoMain.FormCreate(Sender: TObject);
begin
  Application.Title := Self.Caption;
  DoReadConfig;

  if ParamCount > 0 then DoOpenName(ParamStr(1));
end;

procedure TfoMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DoWriteConfig;
end;

procedure TfoMain.DoOpenName(const FileName: String);
begin
  FFile := Sil.OS.FileSystem.OpenFile(FileName, fmAccessRead, fmShareReadWrite);
  FReader := Sil.Stream.Raw.Reader(FFile.Stream);
  Self.Caption := Sil.OS.FileSystem.GetFileName(FileName) + ' - ' + Application.Title;

  meText.Clear;
  DoRead(true);
end;

procedure TfoMain.DoRead(Fwd: Boolean);
var
  iRead, Idx: Integer;
  sLine: String;
begin
  if not Fwd then
  begin
    Idx := Length(FPos) - 1;
    if Idx > 0 then
    begin
      FFile.Stream.Position := FPos[Idx - 1];
      Int.ArrayDelete(FPos, Idx);
    end else
      Exit;
  end else
    Int.ArrayAdd(FPos, FFile.Stream.Position);

  iRead := FMaxLines - 2;

  try
    meText.Lines.BeginUpdate;
    meText.Clear;

    while (iRead > 0) and FReader.ReadLn(sLine) do
    begin
      Dec(iRead);
      meText.Lines.Add(sLine);
    end;
  finally
    meText.Lines.EndUpdate;
  end;
end;

procedure TfoMain.miExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfoMain.miOpenClick(Sender: TObject);
begin
  if odFile.Execute then
    DoOpenName(odFile.FileName);
end;

procedure TfoMain.FormResize(Sender: TObject);
var
  Size: TSize;
  hDc: THandle;
  hFont: hGdiObj;
begin
  hDc := Windows.GetDC(meText.Handle);
  hFont := Windows.SelectObject(hDc, meText.Font.Handle);

  try
    if Windows.GetTextExtentPoint32(hDc, 'Xg', 1, Size) then
      FMaxLines := meText.Height div Size.cy;
  finally
    Windows.SelectObject(hDc, hFont);
    Windows.ReleaseDC(meText.Handle, hDc);
  end;
end;

procedure TfoMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_PRIOR: DoRead(false);
    VK_NEXT:  DoRead(true);
  end;
end;

end.
