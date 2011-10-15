unit FmTestOsThreadBasics;

{$include Defines.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,

  Sil;

type
  TFormTestThreadBasics = class(TForm, IRunnable)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure Run(const Sender: IThread);
    procedure DoCall(const Sender: IUnknown; Param: Pointer);
  public
    FThread: IThread;
    FCancel: IEvent;
  end;

var
  FormTestThreadBasics: TFormTestThreadBasics;

implementation

{$R *.DFM}                                       

procedure TFormTestThreadBasics.FormCreate(Sender: TObject);
begin
  FCancel := Sil.OS.Ipc.Event;
  FThread := Sil.OS.Thread.Spawn(Self);
end;

procedure TFormTestThreadBasics.FormDestroy(Sender: TObject);
begin
  FCancel.Signal;
  Sil.OS.Wait.Single(FThread, INFINITE, true);
end;

procedure TFormTestThreadBasics.Run(const Sender: IThread);
var
  dtNow: TDateTime;
begin
  repeat
    dtNow := DateTime.Now;
    Sil.OS.Thread.SyncCall(DoCall, @dtNow);
  until FCancel.WaitFor(1000) = wrSignaled;
end;

procedure TFormTestThreadBasics.DoCall(const Sender: IUnknown; Param: Pointer);
begin
  Caption := Time.ToStr(TDateTime(Param^));
end;

end.
