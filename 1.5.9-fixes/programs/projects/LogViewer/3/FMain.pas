unit FMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  Dialogs,

  SIL,
  SilVcl, Menus, WTaskBar, AppEvnts,

  UDef, ComCtrls, ExtCtrls, ClipBrd;

type
  TMainForm = class (
    // extends
    TForm,
    // implements
    IRunnable)
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
    N4: TMenuItem;
    itFiltros: TMenuItem;
    HintTimer: TTimer;
    itControles: TMenuItem;
    lvMensajes: TListView;
    itBackgroundColor: TMenuItem;
    itForegroundColor: TMenuItem;
    dgColor: TColorDialog;
    N5: TMenuItem;
    itSelectFont: TMenuItem;
    dgFont: TFontDialog;
    N6: TMenuItem;
    miCopy: TMenuItem;
    edMensaje: TMemo;
    spSplitter: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure itLogStateClick(Sender: TObject);
    procedure Minimizar1Click(Sender: TObject);
    procedure RestoreClick(Sender: TObject);
    procedure itAlwaysOnTopClick(Sender: TObject);
    procedure itClearClick(Sender: TObject);
    procedure Salir1Click(Sender: TObject);
    procedure PopupPopup(Sender: TObject);
    procedure itFiltrosClick(Sender: TObject);
    procedure HintTimerTimer(Sender: TObject);
    procedure itBackgroundColorClick(Sender: TObject);
    procedure itForegroundColorClick(Sender: TObject);
    procedure itSelectFontClick(Sender: TObject);
    procedure miCopyClick(Sender: TObject);
    procedure lvMensajesDblClick(Sender: TObject);
    procedure lvMensajesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  protected // IRunnable
    procedure Run(const Thread: IThread);
  private
    FConfig: INamedKey;
    FThread: IThread;
    FTermination: IEvent;
    FFileName: String;
    FPath: String;
    FOffset: LongWord;
    FMask: String;
    FFirstRead: Boolean;
    FInterval: LongWord;
    FLines: LongWord;
    FFilters: IFilters;
    FHintWnd: THintWindow;
    FHintStep: integer;
    FHintInc: integer;
    FChunk: LongWord;
  protected
    procedure DoAddLine(const Sender: IUnknown; Data: Pointer);
    procedure DoRead;
    procedure Minimized(Sender: TObject);
    procedure Restored(Sender: TObject);
    procedure ShowHint(const AText: String; Replace: Boolean = false);
    procedure HideHint;
    procedure OnHintClick(Sender: TObject);
    procedure DoLoadConfig;
    procedure DoSaveConfig;
  public
    property Interval: LongWord read FInterval write FInterval;
    property Lines: LongWord read FLines write FLines;
  end;

var
  MainForm: TMainForm;

const
  LAST_CHUNK = 1024 * 4;

implementation

{$R *.DFM}

uses
  TypInfo, SilLiKey;

var
  MNumber: Integer = 0;


function SetToString(TypeInfo: PTypeInfo; Value: Integer; Brackets: Boolean): string;
var
  S: TIntegerSet;
  I: Integer;
begin
  Result := '';
  Integer(S) := Value;
  TypeInfo := GetTypeData(TypeInfo).CompType^;
  for I := 0 to SizeOf(Integer) * 8 - 1 do
    if I in S then
    begin
      if Result <> '' then
        Result := Result + ',';
      Result := Result + GetEnumName(TypeInfo, I);
    end;
  if Brackets then
    Result := '[' + Result + ']';
end;

function StringToSet(TypeInfo: PTypeInfo; const Value: string): Integer;
var
  P: PChar;
  EnumName: string;
  EnumValue: Longint;
  EnumInfo: PTypeInfo;

  // grab the next enum name
  function NextWord(var P: PChar): string;
  var
    i: Integer;
  begin
    i := 0;

    // scan til whitespace
    while not (P[i] in [',', ' ', #0,']']) do
      Inc(i);

    SetString(Result, P, i);

    // skip whitespace
    while P[i] in [',', ' ',']'] do
      Inc(i);

    Inc(P, i);
  end;

begin
  Result := 0;
  if Value = '' then Exit;
  P := PChar(Value);

  // skip leading bracket and whitespace
  while P^ in ['[',' '] do
    Inc(P);

  EnumInfo := GetTypeData(TypeInfo)^.CompType^;
  EnumName := NextWord(P);
  while EnumName <> '' do
  begin
    EnumValue := GetEnumValue(EnumInfo, EnumName);
    if EnumValue < 0 then
      raise Sil.Error.Create('Invalid enumeration');
    Include(TIntegerSet(Result), EnumValue);
    EnumName := NextWord(P);
  end;
end;

  
{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FInterval := 1000;
  FLines := 200;

  FTermination := Sil.Os.IPC.Event();
  
  Application.OnMinimize := Minimized;
  Application.OnRestore := Restored;
  FFileName := ParamStr(1);
  if Length(FFileName) = 0 then Exit;

  FConfig := Sil.OS.Registry.Open('$System\SOFTWARE\Siderca\fcv', true);
  Sil.Sink.Connect(FConfig, Self, False);
  FPath := Sil.OS.FileSystem.GetFilePath(FFileName);


  if ParamCount > 1 then // @LC: si no viene la máscara, toma la extensión del archivo
    FMask := ParamStr(2);

  FFilters := AppTool.CreateFilters;

  Caption := 'Tracing file: ...';
  FOffset := 0;

  Taskbar.Hint := Self.Caption;
  Taskbar.ShowIcon := true;

  FThread := Sil.OS.Thread.Spawn(Self);
  DoLoadConfig;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if FThread <> nil then
  begin
    FTermination.Signal;
    if FThread.IsSuspended then FThread.Resume;
    FThread.Termination.WaitFor(INFINITE, true);
  end;

  DoSaveConfig;
  Sil.Sink.Disconnect(FConfig, Self);
  FConfig := nil;
  FTermination := nil;

  FreeAndNil( FHintWnd );
end;

procedure TMainForm.OnHintClick(Sender: TObject);
begin
  HideHint;
end;

procedure TMainForm.DoLoadConfig;
begin
  with FConfig.Values do
  begin
    Self.SetBounds(
      ReadInteger('Left', 0, true),
      ReadInteger('Top', 0, true),
      ReadInteger('Width', 200, true),
      ReadInteger('Height', 400, true));

    Self.Color :=  StringToColor(ReadString('WindowColor', ColorToString(Self.Color), True));
    Self.Font.Color :=  StringToColor(ReadString('FontColor', ColorToString(Self.Font.Color), True));
    Self.Font.Name := ReadString('FontName', Self.Font.Name, True);
    Self.Font.Size := ReadInteger('FontSize', Self.Font.Size, True);
    Self.Font.Style := TFontStyles(Byte(StringToSet(TypeInfo(TFontStyles), ReadString('FontStyle', SetToString(TypeInfo(TFontStyles), Byte(Self.Font.Style), True), True))));
    
    if ReadBoolean('StayOnTop', Self.FormStyle = fsStayOnTop, True) then
      Self.FormStyle := fsStayOnTop else
      Self.FormStyle := fsNormal;

    Self.Interval := ReadInteger('Interval', Self.Interval, True);
    Self.Lines  := ReadInteger('Lines', Self.Lines, True);
    FChunk := ReadInteger('ChunkSize', LAST_CHUNK, true);
  end;

  AppTool.ReadConfig(FFilters, FConfig);
end;

procedure TMainForm.DoSaveConfig;
var
  Conf: INamedKey;
begin
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
    WriteString('FontStyle', SetToString(TypeInfo(TFontStyles), Byte(Self.Font.Style), True));
    WriteInteger('FontSize', Self.Font.Size);
    WriteBoolean('StayOnTop', Self.FormStyle = fsStayOnTop);
    WriteInteger('Interval', Self.Interval);
    //WriteInteger('Lines', Self.Lines);
    WriteBoolean('AlphaBlend', Self.AlphaBlend);
    WriteInteger('AlphaBlendValue', Self.AlphaBlendValue);
  end;
end;

procedure TMainForm.HintTimerTimer(Sender: TObject);
begin
  {if Assigned( FHintWnd ) then
    FHintWnd.Color := Sil.Rgb.MorphRel( clRed, $0085B8FE, FHintStep );}

  FHintWnd.Left := FHintStep;
  Inc( FHintStep, FHintInc );
  if ( FHintStep < 0 ) or ( FHintStep > Screen.Width - FHintWnd.Width ) then
  begin
    FHintInc := -FHintInc;
    Inc( FHintStep, FHintInc );
  end;
end;

procedure TMainForm.DoAddLine(const Sender: IUnknown; Data: Pointer);
var
  s1: string;
  actions: string;
  it: TListItem;
begin
  s1 := string(PChar(Data));
  if not FFilters.GetActions( s1, actions ) then exit;

  actions := Sil.Str.ToLower(actions);

  if (Sil.Str.Pos( 'show', actions ) > 0) then
    with lvMensajes.Items do
    begin
      BeginUpdate;
      try
        while Count > Integer(FLines) do
        Delete(0);

        it := Add(); 
        it.Caption := Int.ToStr(Sil.Os.Locked.Increment(MNumber));
        it.Subitems.Add(s1);
        lvMensajes.Selected := it;
        it.MakeVisible(True);
      finally
        EndUpdate;
      end;
    end;

  if ( Sil.Str.Pos( 'notify', actions ) > 0 ) then
    ShowHint( s1 );
end;

procedure TMainForm.DoRead;
var
  sLine, sName, sLast, Title: String;
  Archive: ITextFile;
  List: IFileInfoList;
begin
  try
    Archive := OS.FileSystem.OpenTextFile(FFileName, fmAccessRead, fmShareReadWrite, true);

    if FOffset <= Archive.Stream.Size then
      Archive.Stream.Position := FOffset else
      Archive.Stream.Position := 0;

    Title := 'Tracing file: ' + FFileName;
    Vcl.SetProp.Value(Self, 'Caption', Title);
    Vcl.SetProp.Value(Taskbar, 'Hint', Title);
  except
    Vcl.SetProp.Value(Self, 'Caption', 'Error with file: ' + FFileName);
  end;

  if Archive <> nil then
  begin
    if FFirstRead then
    begin
      if Archive.Stream.Size > FChunk then
        Archive.Stream.Seek(-FChunk, soFromEnd);

      FFirstRead := false;
      Archive.Stream.ReadLn(sLine); // descarta la linea
    end;

    while not FTermination.IsSignaled and Archive.Stream.ReadLn(sLine) do
      Sil.OS.Thread.SyncCall(DoAddLine, PChar(sLine));

    FOffset := Archive.Stream.Position;
  end;

  if Str.IsEmpty(sLine) and Str.NotEmpty(FMask) then
  begin
    sName := Sil.OS.FileSystem.GetFileName(FFileName);
    List := Sil.OS.FileSystem.GetList(FPath + FMask);

    if List.Count > 0 then
    begin
      Sil.Sort.Default(List);
      sLast := List.Items[List.Count - 1].Name;

      if (sName <> sLast) and Str.NotEmpty(sLast) then
      begin
        FFileName := FPath + sLast;
        FOffset := 0;
      end;
    end;
  end;
end;

procedure TMainForm.Run(const Thread: IThread);
begin
  try
    FFirstRead := true;

    repeat
      DoRead;
    until FTermination.WaitFor(FInterval) <> wrTimeout;
  except
  end;
end;

procedure TMainForm.itLogStateClick(Sender: TObject);
var
  NewState: Boolean;
begin
  NewState := not itLogState.Checked;
  itLogState.Checked := NewState;
  if NewState then
    FThread.Suspend else
    FThread.Resume;
end;

procedure TMainForm.Minimizar1Click(Sender: TObject);
begin
  Application.Minimize;
end;

procedure TMainForm.RestoreClick(Sender: TObject);
begin
  HideHint;
  Application.Restore;
end;

procedure TMainForm.Minimized(Sender: TObject);
begin
  Taskbar.Icon := Self.Icon; 
  Taskbar.ShowIcon := True;
  //Hide;
  ShowWindow(Application.Handle, SW_HIDE);
end;

procedure TMainForm.Restored(Sender: TObject);
begin
  //Taskbar.ShowIcon := False;
  Show;
end;

procedure TMainForm.itAlwaysOnTopClick(Sender: TObject);
begin
  itAlwaysOnTop.Checked := not itAlwaysOnTop.Checked;
  if itAlwaysOnTop.Checked then
    FormStyle := fsStayOnTop else
    FormStyle := fsNormal;
end;

procedure TMainForm.itClearClick(Sender: TObject);
begin
  lvMensajes.Clear;
end;

procedure TMainForm.Salir1Click(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.PopupPopup(Sender: TObject);
begin
  if Assigned( FThread ) then
  begin
    itAlwaysOnTop.Checked := (FormStyle = fsStayOnTop);
    itLogState.Checked := FThread.IsSuspended;
  end;
end;

procedure TMainForm.itFiltrosClick(Sender: TObject);
begin
  if AppTool.ShowFilters(FFilters) then
    AppTool.SaveConfig(FFilters, FConfig);
end;

procedure TMainForm.ShowHint(const AText: String; Replace: Boolean);
var
  s1: string;
  RectArea: TRect;
  iSep, iVDiff, iHDiff: Integer;
  hWnd: THandle;
  tnRect: TRect;
begin
  if not Assigned( FHintWnd ) then
  begin
    FHintWnd := THintWindow.Create(nil);
    FHintWnd.Font.Color := clWhite;
    FHintWnd.Color := $002481FD;
  end;

  ShowWindow( FHintWnd.Handle, SW_HIDE );
  if not Replace then
    with Sil.List.StringList( FHintWnd.Caption ) do
    begin
      Add( AText );

      //if Count > FEngine.Config.HintLines then Delete(0);
      s1 := Text;
    end
  else
    s1 := AText;
    
  RectArea := FHintWnd.CalcHintRect( Screen.Width, s1, nil );
  iHDiff := RectArea.Right - RectArea.Left;
  iVDiff := RectArea.Bottom - RectArea.Top;
  iSep := FHintWnd.Canvas.TextHeight( 'X' );

  hWnd := FindWindow( 'Shell_TrayWnd', nil );
  if ( hWnd <> 0 ) and GetWindowRect( hWnd, tnRect ) then
  begin
    RectArea.Top := tnRect.Top - iVDiff;
    if RectArea.Top < 0 then RectArea.Top := 0;
    RectArea.Left := tnRect.Right - iHDiff - iSep;
    if RectArea.Left < 0 then RectArea.Left := 0;
  end else
  begin
    RectArea.Left := Screen.Width - iHDiff - iSep;
    RectArea.Top := Screen.Height - iVDiff - iSep * 3;
  end;

  if ( FHintInc = 0 ) then
  begin
    FHintStep := RectArea.Left;
    FHintInc := 1;
  end
  else
    RectArea.Left := FHintStep;

  RectArea.Right := RectArea.Left + iHDiff;
  RectArea.Bottom := RectArea.Top + iVDiff;

  FHintWnd.ActivateHint( RectArea, s1 );
  FHintWnd.Update;
  
  HintTimer.Enabled := true;
end;

procedure TMainForm.HideHint;
begin
  HintTimer.Enabled := false;
  if Assigned( FHintWnd ) then
  begin
    FHintWnd.Caption := '';
    if IsWindowVisible( FHintWnd.Handle ) then
      FHintWnd.ReleaseHandle;
  end;
end;

procedure TMainForm.itBackgroundColorClick(Sender: TObject);
begin
  dgColor.Color := Self.Color;
  if dgColor.Execute() then
  begin
    Self.Color := dgColor.Color;
    DoSaveConfig;
  end;
end;

procedure TMainForm.itForegroundColorClick(Sender: TObject);
begin
  dgColor.Color := Self.Font.Color;
  if dgColor.Execute() then
  begin
    Self.Font.Color := dgColor.Color;
    DoSaveConfig;
  end;
end;

procedure TMainForm.itSelectFontClick(Sender: TObject);
begin
  dgFont.Font := Self.Font;
  if dgFont.Execute() then
  begin
    Self.Font := dgFont.Font;
    DoSaveConfig;
  end;
end;

procedure TMainForm.miCopyClick(Sender: TObject);
begin
  Clipboard.AsText := lvMensajes.Selected.SubItems[0];
end;

procedure TMainForm.lvMensajesDblClick(Sender: TObject);
begin
  with lvmensajes do
    if Assigned(Selected) then
      with Selected do
        MessageDlg(Subitems[0], mtInformation, [mbOK], 0);
end;

procedure TMainForm.lvMensajesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if Selected and Assigned(Item) then
    edMensaje.Text := Item.Subitems[0];
end;

end.

