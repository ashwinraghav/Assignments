{********************************************************************************
 *                  Standard Interface Library (SIL)                            *
 *                                                                              *
 *       General purpose library whose design is based in STRONG                *
 *   use of interfaces.                                                         *
 *                                                                              *
 *                                                                              *
 *     Copyright (C) 2000 Mariano Podestá    marianop@intercom.com.ar           *
 *     Copyright (C) 2000 Leandro Conde      lconde@str.com.ar                  *
 *     Copyright (C) 2000 Lisandro Podestá   lisandrop@movi.com.ar              *
 *                                                                              *
 *     See License.txt for details.                                             *
 *                                                                              *
 *   This library is free software; you can redistribute it and/or              *
 *   modify it under the terms of the GNU Lesser General Public                 *
 *   License as published by the Free Software Foundation; either               *
 *   version 2.1 of the License, or (at your option) any later version.         *
 *                                                                              *
 *   This library is distributed in the hope that it will be useful,            *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of             *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          *
 *   Lesser General Public License for more details.                            *
 *                                                                              *
 *   You should have received a copy of the GNU Lesser General Public           *
 *   License along with this library; if not, write to the Free Software        *
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA  *
 *                                                                              *
 ********************************************************************************}

unit FmAgent;

interface

{$include Defines.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, IBCustomDataSet, IBQuery, Grids, DBGrids, ComCtrls,
  StdCtrls, DBCtrls, IBDatabase,

  Sil,

  DmAgent, ExtCtrls, GIFImage, Menus;

type
  TfoMain = class(TForm, ITimerEvents)
    PageControl: TPageControl;
    tsJobs: TTabSheet;
    grTask: TDBGrid;
    dsTask: TDataSource;
    quTask: TIBQuery;
    gbDescription: TGroupBox;
    meDescription: TDBMemo;
    taTask: TIBTransaction;
    quTaskTASK_ID: TIntegerField;
    quTaskNAME: TIBStringField;
    quTaskENABLED: TSmallintField;
    quTaskCATEGORY_ID: TIntegerField;
    quTaskCATEGORY_NAME: TIBStringField;
    quTaskDESCRIPTION: TMemoField;
    quTaskLAST_RUN: TDateTimeField;
    quTaskLAST_STATUS: TIBStringField;
    quTaskNEXT_RUN: TDateTimeField;
    quTaskNOTIFICATION: TMemoField;
    tsAbout: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Image1: TImage;
    Image2: TImage;
    Label8: TLabel;
    quTaskDATE_CREATED: TDateTimeField;
    quTaskDATE_MODIFIED: TDateTimeField;
    tsSettings: TTabSheet;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    edSeConnection: TEdit;
    edSeUserName: TEdit;
    Label15: TLabel;
    Label16: TLabel;
    edSeUserPassword: TEdit;
    edSeMailServer: TEdit;
    edSeMailPort: TEdit;
    btAdd: TButton;
    btChange: TButton;
    btDelete: TButton;
    btClose: TButton;
    StatusBar: TStatusBar;
    btSeUndo: TButton;
    btSeSave: TButton;
    poMain: TPopupMenu;
    miJobNew: TMenuItem;
    miJobEdit: TMenuItem;
    miJobDelete: TMenuItem;
    miJobRun: TMenuItem;
    N1: TMenuItem;
    miJobHistory: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure btAddClick(Sender: TObject);
    procedure btChangeClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure btDeleteClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure quTaskENABLEDGetText(Sender: TField; var Text: String; DisplayText: Boolean);
    procedure quTaskLAST_STATUSGetText(Sender: TField; var Text: String; DisplayText: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure StatusBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
    procedure btSeSaveClick(Sender: TObject);
    procedure btSeUndoClick(Sender: TObject);
    procedure SettingsChanged(Sender: TObject);
    procedure quTaskNEXT_RUNGetText(Sender: TField; var Text: String; DisplayText: Boolean);
    procedure miJobHistoryClick(Sender: TObject);
  private
    FSvcStatus: ITimer;
    FCheckSvcStatus: Boolean;
    FStatusColor: TColor;
    FShowHistoryFirst: Boolean;
    procedure DoCheckStatus;
    procedure DoReadConfig;
    procedure DoReloadDb;
  protected // ITimerEvents
    procedure OnTick(const Event: RTimerEvent);
  end;

var
  foMain: TfoMain;

implementation

uses
  UfConfig,
  UeAgent,
  UtAgent,
  UtCoder,
  UtLog,
  FmTask;

{$R *.dfm}

procedure TfoMain.FormCreate(Sender: TObject);
begin
  Log.Initialize;

  Application.Title := FBAGENT_GUI_TITLE + ' ' + FBAGENT_GUI_VERSION;
  Caption := Application.Title;
  FStatusColor := clWindowText;

  daAgent := TdaAgent.Create(Self);

  DoReadConfig;
  DoReloadDb;

  DoCheckStatus;

  Global.ReadForm('jobs', Self);
  Global.ReadGrid('jobs', Self.grTask);
end;

procedure TfoMain.FormDestroy(Sender: TObject);
begin
  FSvcStatus := nil;

  Global.WriteForm('jobs', Self);
  Global.WriteGrid('jobs', Self.grTask);
  Log.Finalize;
end;

procedure TfoMain.DoReadConfig;
begin
  with Config.GetTag('config/database', true) do
  begin
    edSeConnection.Text := Childs.ReadString('name');
    edSeUserName.Text := Childs.ReadString('username');
    edSeUserPassword.Text := Coder.Decrypt(Childs.ReadString('password'));
  end;

  with Config.GetTag('config/smtp', true) do
  begin
    edSeMailServer.Text := Childs.ReadString('host');
    edSeMailPort.Text := Childs.ReadString('port');
  end;

  btSeSave.Enabled := false;
  btSeUndo.Enabled := false;
end;

procedure TfoMain.DoReloadDb;
begin
  try
    daAgent.ReadConfig;
    quTask.Open;
  except
    Sil.Trace.Exception;

    btAdd.Enabled := false;
    btChange.Enabled := false;
    btDelete.Enabled := false;
  end;
end;

procedure TfoMain.DoCheckStatus;
begin
  if Sil.OS.Process.GetList('fbserver*').Count > 0 then
  begin
    FCheckSvcStatus := true;
    FSvcStatus := Sil.OS.Timer.Create(0, 60000, Self);
    //FSvcStatus.Tick;
  end else
  begin
    FCheckSvcStatus := false;
    Sil.Trace.Log('Local Firebird Server not found');
  end;

  Sil.Trace.Leave;
end;

procedure TfoMain.btAddClick(Sender: TObject);
begin
  Sil.Trace.Enter(Self, 'btAddClick');

  with TfoTask.Create(Self) do
  begin
    if ShowModal = mrOk then
      daAgent.Refresh(quTask);

    Free;
  end;

  Sil.Trace.Leave;
end;

procedure TfoMain.btChangeClick(Sender: TObject);
begin
  Sil.Trace.Enter(Self, 'btChangeClick');

  if not quTask.IsEmpty then
    with TfoTask.Create(Self) do
    begin
      if FShowHistoryFirst then
      begin
        FShowHistoryFirst := false;
        pcTasks.ActivePageIndex := 4;
      end;

      SetParams(quTask);

      if ShowModal = mrOk then
        daAgent.Refresh(quTask);

      Free;
    end;

  Sil.Trace.Leave;
end;

procedure TfoMain.btCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfoMain.btDeleteClick(Sender: TObject);
begin
  Sil.Trace.Enter(Self, 'btDeleteClick');

  if Application.MessageBox('Confirm deletion?', PChar(Caption), MB_ICONQUESTION or MB_OKCANCEL) = ID_OK then
  begin
    daAgent.TaskDelete(quTask.FieldByName('task_id').AsInteger);
    daAgent.Refresh(quTask);
  end;

  Sil.Trace.Leave;
end;

procedure TfoMain.quTaskENABLEDGetText(Sender: TField; var Text: String; DisplayText: Boolean);
begin
  if quTask.IsEmpty then
    Text := '' else
    Text := CYesNo[quTaskENABLED.AsInteger = 1];
end;

procedure TfoMain.quTaskLAST_STATUSGetText(Sender: TField; var Text: String; DisplayText: Boolean);
var
  Date: TDateTime;
begin
  Date := quTaskLAST_RUN.AsDateTime;

  if quTask.IsEmpty then
    Text := '' else
  if Date = 0 then
    Text := '(none)' else
    Text := Str.Format('%s (%s)', [DateTime.ToStr(Date), quTaskLAST_STATUS.AsString]);
end;

procedure TfoMain.quTaskNEXT_RUNGetText(Sender: TField; var Text: String; DisplayText: Boolean);
var
  Date: TDateTime;
begin
  Date := quTaskNext_RUN.AsDateTime;

  if quTask.IsEmpty then
    Text := '' else
  if Date = 0 then
    Text := '(none)' else
    Text := DateTime.ToStr(Date);
end;

procedure TfoMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_F5 then
    daAgent.Refresh(quTask);
end;

procedure TfoMain.OnTick(const Event: RTimerEvent);
{const
  AStatus: array [Boolean] of String = ('Service Stopped', 'Service Running');
  AColor: array [Boolean] of TColor = (clRed, clGreen);
var
  Mutex: IMutex;
  IsRunning: Boolean;}
begin
  {Mutex := Sil.OS.Ipc.Mutex(false, 'FirebirdSQLAgent_Mutex');
  IsRunning := Mutex.WaitFor(0) = wrTimeout;

  FStatusColor := AColor[IsRunning];
  StatusBar.Panels[0].Text := AStatus[IsRunning];}

  daAgent.Refresh(quTask);
end;

procedure TfoMain.StatusBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
begin
  if FCheckSvcStatus then
    with StatusBar.Canvas do
    begin
      StatusBar.Canvas.Font.Color := FStatusColor;
      TextRect(Rect, 4, 3, Panel.Text);
    end;
end;

procedure TfoMain.btSeSaveClick(Sender: TObject);
begin
  Sil.Trace.Enter(Self, 'btSeSaveClick');

  with Config.GetTag('config/database', true) do
  begin
    Childs.WriteString('name', edSeConnection.Text);
    Childs.WriteString('username', edSeUserName.Text);
    Childs.WriteString('password', Coder.Encrypt(edSeUserPassword.Text));
  end;

  with Config.GetTag('config/smtp', true) do
  begin
    Childs.WriteString('host', edSeMailServer.Text);
    Childs.WriteString('port', edSeMailPort.Text);
  end;

  SaveConfig;
  btSeSave.Enabled := false;
  btSeUndo.Enabled := false;
  DoReloadDb;

  Sil.Trace.Leave;
end;

procedure TfoMain.btSeUndoClick(Sender: TObject);
begin
  Sil.Trace.Enter(Self, 'btSeUndoClick');

  ReloadConfig;
  DoReadConfig;

  Sil.Trace.Leave;
end;

procedure TfoMain.SettingsChanged(Sender: TObject);
begin
  btSeSave.Enabled := true;
  btSeUndo.Enabled := true;
end;

procedure TfoMain.miJobHistoryClick(Sender: TObject);
begin
  FShowHistoryFirst := true;
  btChange.Click;
end;

end.
