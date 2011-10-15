unit FmTestOsTimers;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,

  Sil, StdCtrls;

type
  TFormTestOsTimers = class (
    // extends
    TForm,
    // implements
    ITimerEvents)
    lbTime: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    //
  private
    FTimer: ITimer;
  protected // ITimerEvents
    procedure OnTick(const Event: RTimerEvent);
  end;

var
  FormTestOsTimers: TFormTestOsTimers;

implementation

{$R *.DFM}

{ TForm1 }

procedure TFormTestOsTimers.FormCreate(Sender: TObject);
begin
  FTimer := OS.Timer.Create(0, 1000, Self);
end;

procedure TFormTestOsTimers.OnTick(const Event: RTimerEvent);
begin
  lbTime.Caption := Sil.DateTime.ToStr(Sil.DateTime.Now, 'hh:nn:ss');
end;

procedure TFormTestOsTimers.FormDestroy(Sender: TObject);
begin
  FTimer := nil;
end;

end.
 