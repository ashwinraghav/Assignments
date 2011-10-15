unit FmTest0;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Sil, SilSi3dgl, SilSe3DGL, SilSe3dgOpenGL, ExtCtrls;

type
  TFormTest0 = class(TForm)
    Panel1: TPanel;
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FEngine: I3dgEngine;
    FParams: R3dgContextParams;
    FContext: I3dgContext;
	  FDim: R3DVector;
		FPoint: TPoint;
		FPressed: Boolean;
		FSpeed: Double;
	  FInMouseMove: Boolean;
    FLight: I3dgLight;
		procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
		procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public declarations }
  end;

var
  FormTest0: TFormTest0;

implementation

uses
  OpenGL,
  SilLog,
  SilSmDC,
  SilSm3dgOpenGL,
  SilSt3dgOpenGL;

{$R *.dfm}

procedure DoCube(
  const Builder: I3dgObjectBuilder;
        Position: R3DPoint;
        Delta: R3DVector;
        Color: TColor);
begin
	Builder.Square(
    Position,
    Vector(Delta.X, Delta.Y, 0), Color, -1);
	Builder.Square(
    Point(Position.X, Position.Y, Position.Z + Delta.Z),
    Vector(Delta.X, Delta.Y, 0), Color, +1);
	Builder.Square(
    Position,
    Vector(Delta.X, 0, Delta.Z), Color, -1);
	Builder.Square(
    Point(Position.X, Position.Y + Delta.Y, Position.Z),
    Vector(Delta.X, 0, Delta.Z), Color, +1);
	Builder.Square(
    Position,
    Vector(0, Delta.Y, Delta.Z), Color, -1);
	Builder.Square(
    Point(Position.X + Delta.X, Position.Y, Position.Z),
    Vector(0, Delta.Y, Delta.Z), Color, +1);
end;

const
  ZColor = $003C3C3C;
  YColor = $0000C400;
  XColor = $00585858;

procedure TFormTest0.FormCreate(Sender: TObject);
begin
  Sil.Trace.Define(LogService, StackFmt);
	FDim.X := 50;
	FDim.Y := 50;
	FDim.Z := 300;
  FormClick(nil);
  FSpeed := 1;
end;

procedure TFormTest0.FormClick(Sender: TObject);
var
  Device: IHandle;
  Builder: I3dgObjectBuilder;
begin
  Device := TDisplayContext.Create(Self.Handle);
  FEngine := TSil3dgOpenglEngine.Create();
  FParams.PixelFormat.Flags := [pfDrawToWindow, pfSupportOpengl, pfDoubleBuffer];
  FContext := FEngine.CreateContext(Device, FParams);

  with FContext.Camera do
  begin
    Position^ := Point( -18,  -13, -100);
    Direction^ := Vector(0, 0,   0);
  end;

  with FContext.CreateObject('ejes') do
  begin
    Position^ := Point(0, 0, 0);
    Direction^ := Vector(0, 0, 0);

    Builder := Define();
    

    Builder := nil;
  end;


  with FContext.CreateObject('ref') do
  begin
    Position^ := Point(-50, 0, 0);
    Direction^ := Vector(0, 0, 0);

    Builder := Define();

    glColor(255,   0,   0, 0); // rojo : eje X
    glLineWidth(4);
    glBegin(GL_LINES);
      glVertex3d(   0, 0, 0);
      glVertex3d( 10, 0, 0);
    glEnd();

    glColor(  0, 255,   0, 0); // verde : eje Y
    glLineWidth(4);
    glBegin(GL_LINES);
      glVertex3d(   0,    0,     0);
      glVertex3d(   0, 10,     0);
    glEnd();

    glColor(  0,   0, 255, 0); // azul: eje Z
    glLineWidth(4);
    glBegin(GL_LINES);
      glVertex3d(   0, 0,     0);
      glVertex3d(   0, 0,  10);
    glEnd();

    Builder := nil;
  end;//(*)

(*)  FLight := FContext.CreateLight('L1');
  with FLight do
  begin
		Position^ := Point(-FDim.X, 0, 0);
		Direction^ := Vector(0, 0, 0);
		Angle^ := 90;
    Resync(nil);
  end;(*)

(*)  with FContext.CreateObject('cubo') do
  begin

    Position^ := Point(FDim.X, 0, 0);
    Direction^ := Vector(45, 0, 0);

    Builder := Define();

    DoCube(
      Builder,
      Point(0, 0, 0),
      Vector(10, 10, 10),
      clRed);

    Builder := nil;
  end;(*)


(*)  with FContext.CreateObject('walls') do
  begin
    Position^ := Point(FDim.X, 0, 0);
    Direction^ := Vector(0, 0, 0);

    Builder := Define();

    Builder.Square( // atras
      Point(0, 0, 0),
      Vector(FDim.X, FDim.Y, 0), XColor, +1);
    Builder.Square( // adelante
      Point(0, 0, FDim.Z),
      Vector(FDim.X, FDim.Y, 0), XColor, -1);
    Builder.Square( // abajo
      Point(0, 0, 0),
      Vector(FDim.X, 0, FDim.Z), YColor, +1);
    Builder.Square( // derecha
      Point(0, 0, 0),
      Vector(0, FDim.Y, FDim.Z), ZColor, +1);
    Builder.Square( // izquierda
      Point(FDim.X, 0, 0),
      Vector(0, FDim.Y, FDim.Z), ZColor, -1);

    Builder := nil;
  end;//(*)

//		p_DoCube(-FDim.X / 2, FDim.Y / 1.2, FDim.Z / 2, 10, 5, 30, clRed);


  Invalidate;
end;

procedure TFormTest0.FormDestroy(Sender: TObject);
begin
  FEngine := nil;
end;

procedure TFormTest0.CreateParams(var Params: TCreateParams);
begin
  inherited;
	Params.Style := Params.Style or WS_CLIPSIBLINGS or WS_CLIPCHILDREN;
end;

procedure TFormTest0.FormPaint(Sender: TObject);
begin
  if Assigned(FContext) then
  begin
    FContext.Render;
    with FContext.Camera do
      Caption :=
        Sil.Int.ToStr(FContext.Frames) + ': ' +
        Sil.Str.Format('C [o = (%f %f %f) d = (%f %f %f)]', [Position.X, Position.Y, Position.Z, Direction.X, Direction.Y, Direction.Z]);
  end;
end;

procedure TFormTest0.FormResize(Sender: TObject);
begin
  FContext.ViewPort.Size.x := Width;
  FContext.ViewPort.Size.y := Height;
end;

procedure TFormTest0.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
	Message.Result := 0;
end;

procedure TFormTest0.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if ssShift in Shift then
    case Key of
      VK_LEFT:      FContext.Camera.Direction.x := FContext.Camera.Direction.x - FSpeed;
      VK_RIGHT:     FContext.Camera.Direction.x := FContext.Camera.Direction.x + FSpeed;
      VK_UP:        FContext.Camera.Direction.y := FContext.Camera.Direction.y + FSpeed;
      VK_DOWN:      FContext.Camera.Direction.y := FContext.Camera.Direction.y - FSpeed;
      Ord('A'),
      Ord('a'):     FContext.Camera.Direction.z := FContext.Camera.Direction.z + FSpeed;
      Ord('Z'),
      Ord('z'):     FContext.Camera.Direction.z := FContext.Camera.Direction.z - FSpeed;
      else
        Exit;
    end
  else
    case Key of
      VK_LEFT:      FContext.Camera.Position.x := FContext.Camera.Position.x - FSpeed;
      VK_RIGHT:     FContext.Camera.Position.x := FContext.Camera.Position.x + FSpeed;
      VK_UP:        FContext.Camera.Position.y := FContext.Camera.Position.y + FSpeed;
      VK_DOWN:      FContext.Camera.Position.y := FContext.Camera.Position.y - FSpeed;
      Ord('A'),
      Ord('a'):     FContext.Camera.Position.z := FContext.Camera.Position.z + FSpeed;
      Ord('Z'),
      Ord('z'):     FContext.Camera.Position.z := FContext.Camera.Position.z - FSpeed;
      else
        Exit;
    end;
  FContext.Render;
  Key := 0;
  Invalidate;
end;

end.
