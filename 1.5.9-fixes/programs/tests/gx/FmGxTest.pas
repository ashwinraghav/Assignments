unit FmGxTest;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Sil, SilSiGx, SilSmGxControl;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FocusChanged(Sender: TObject);
  private
    FMimicControl: TGxControl;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  SilSmGxEntities;
  
{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  Entity: IGxRectangle;
begin
  FMimicControl := TGxControl.Create( nil );
  FMimicControl.Parent := Self;
  FMimicControl.Align := alClient;
  FMimicControl.Color := Self.Color;
  FMimicControl.OnFocusChanged := self.FocusChanged;

  Entity := TGxRectangle.Create(100, 200);
  Entity.Brush.VclBrush.Color := clNavy;

  FMimicControl.View.PanX := -100;
  FMimicControl.View.PanY := -50;
  FMimicControl.Entities.Add( Entity );
  FMimicControl.Invalidate;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if FMimicControl.MouseWheel( Shift, WheelDelta, MousePos ) then
    Handled := true;
end;

procedure TForm1.FocusChanged(Sender: TObject);
begin
  Caption := Sil.Str.Format('(left=%f, top=%f, width=%f, height=%f)', [FMimicControl.FocusedEntity.Left, FMimicControl.FocusedEntity.Top, FMimicControl.FocusedEntity.Width, FMimicControl.FocusedEntity.Height])
end;

end.
