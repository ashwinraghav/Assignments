unit FmTest1;

interface

uses
  Windows, Messages, Classes, Controls, Forms, ExtCtrls, OpenGL, GlAux, StdCtrls, NumEdits,

  Sil, SilSi3dgl, SilSe3DGL, SilSe3dgOpenGL;

const
  cSphere = 101;
  cEjes   = 102;
  cScene  = 201;

type
  TFormTest1 = class(
    TForm,
    IRunnable )
    pnSidebar: TPanel;
    edViewX: TFloatEdit;
    edViewY: TFloatEdit;
    edViewZ: TFloatEdit;
    Apply: TButton;
    edField: TFloatEdit;
    edZnear: TFloatEdit;
    edZfar: TFloatEdit;
    btDecX: TButton;
    btDecY: TButton;
    btDecZ: TButton;
    btIncX: TButton;
    btIncY: TButton;
    btIncZ: TButton;
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ApplyClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btDecXClick(Sender: TObject);
    procedure btDecYClick(Sender: TObject);
    procedure btDecZClick(Sender: TObject);
    procedure btIncXClick(Sender: TObject);
    procedure btIncYClick(Sender: TObject);
    procedure btIncZClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FDevice: IHandle;
    FEngine: I3dgEngine;
    FParams: R3dgContextParams;
    FContext: I3dgContext;
    FViewport: TRect;
    FViewpoint: R3dPoint;
    FDays: Integer;
    FCounter: IPerformanceCounter;
    FThread: IThread;
    FExit: IEvent;
    FFrames, FTime: Double;
    procedure DoSetupProjection;
  private
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  protected // IRunnable
    procedure Run(const Thread: IThread);
  end;

var
  FormTest1: TFormTest1;

implementation

uses
  SysUtils, DateUtils,
  SilSmDC,
  SilSm3dgOpenGL;

{$R *.dfm}

{ TFormTest1 }

procedure Square(const Position: R3DPoint; const Delta: R3DVector; AColor: TGLColor; Orientation: Integer);
begin
  glBegin(GL_QUADS);
  glColor3bv(@AColor);
  if Delta.X = 0 then
    begin
      glNormal3d(Orientation, 0.0, 0.0);
      glVertex3d(Position.x,            Position.y,             Position.z);
      glVertex3d(Position.x,            Position.y + Delta.y,   Position.z);
      glVertex3d(Position.x,            Position.y + Delta.y,   Position.z + Delta.z);
      glVertex3d(Position.x,            Position.y,             Position.z + Delta.z);
    end
  else if Delta.Y = 0 then
    begin
      glNormal3d(0.0, Orientation, 0.0);
      glVertex3d(Position.x,            Position.y,             Position.z);
      glVertex3d(Position.x,            Position.y,             Position.z + Delta.z);
      glVertex3d(Position.x + Delta.x,  Position.y,             Position.z + Delta.z);
      glVertex3d(Position.x + Delta.x,  Position.y,             Position.z);
    end
  else
    begin
      glNormal3d(0.0, 0.0, Orientation);
      glVertex3d(Position.x,            Position.y,             Position.z);
      glVertex3d(Position.x,            Position.y + Delta.y,   Position.z);
      glVertex3d(Position.x + Delta.x,  Position.y + Delta.y,   Position.z);
      glVertex3d(Position.x + Delta.x,  Position.y,             Position.z);
    end;
  glEnd();
end;

procedure Cube(
        Position: R3DPoint;
        Delta: R3DVector;
        Color: TGLColor);
begin
  Square(
    Position,
    Vector(Delta.X, Delta.Y, 0), Color, -1);
  Square(
    Point(Position.X, Position.Y, Position.Z + Delta.Z),
    Vector(Delta.X, Delta.Y, 0), Color, +1);
  Square(
    Position,
    Vector(Delta.X, 0, Delta.Z), Color, -1);
  Square(
    Point(Position.X, Position.Y + Delta.Y, Position.Z),
    Vector(Delta.X, 0, Delta.Z), Color, +1);
  Square(
    Position,
    Vector(0, Delta.Y, Delta.Z), Color, -1);
  Square(
    Point(Position.X + Delta.X, Position.Y, Position.Z),
    Vector(0, Delta.Y, Delta.Z), Color, +1);
end;

procedure TFormTest1.FormCreate(Sender: TObject);
begin

  FCounter := Sil.Os.Performance.Create();
  FExit := Sil.Os.IPC.Event();
  FThread := Sil.Os.Thread.Spawn(Self);

end;

procedure TFormTest1.FormDestroy(Sender: TObject);
begin
  FExit.Signal;
  FThread.Termination.WaitFor();
  FExit := nil;
  FThread := nil;
end;

procedure TFormTest1.FormResize(Sender: TObject);
begin
  DoSetupProjection;
end;

procedure TFormTest1.Run(const Thread: IThread);
begin
  FDevice := TDisplayContext.Create(Self.Handle);

  FEngine := TSil3dgOpenglEngine.Create();
  FParams.PixelFormat.Flags := [pfDrawToWindow, pfSupportOpengl, pfDoubleBuffer, pfGenericAccelerated];
  FContext := FEngine.CreateContext(FDevice, FParams);
  FContext.MakeCurrent;
  FViewpoint := Point( 2,  2, 50);

  glEnable(GL_DEPTH_TEST);

  glColorMaterial(GL_FRONT, GL_AMBIENT_AND_DIFFUSE);
  glEnable(GL_COLOR_MATERIAL);

  glEnable(GL_LIGHTING);

  glClearColor(0.8, 0.8, 0.8, 0.0);

  glNewList(cSphere, GL_COMPILE);
    Cube(Point(0, 0, 0), Vector(1, 1, 1), clWhite);
  glEndList();

  glNewList(cEjes, GL_COMPILE);
    glColor(255,   0,   0, 0); // rojo : eje X
    glBegin(GL_LINES);
      glVertex3d(   0, 0, 0);
      glVertex3d(1, 0, 0);
    glEnd();

    glColor(  0, 255,   0, 0); // verde : eje Y
    glBegin(GL_LINES);
      glVertex3d(   0,    0,     0);
      glVertex3d(   0, 1,     0);
    glEnd();

    glColor(  0,   0, 255, 0); // azul: eje Z
    glBegin(GL_LINES);
      glVertex3d(   0, 0,     0);
      glVertex3d(   0, 0,  1);
    glEnd();
  glEndList();

  DoSetupProjection;
  FContext.MakeCurrent;

  while FExit.WaitFor(30) = wrTimeout do
  begin
    FFrames := Sil.Os.Performance.ToMseconds(FCounter, True);


    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity;

    with FViewPoint do
      glTranslated(-x, -y, -z);

    glLineWidth(2);

    glPushMatrix();
    glScale(100, 100, 100);
    glCallList(cEjes);
    glPopMatrix();

    glColor3f(1, 1, 1);

    glCallList(cSphere);

    glRotated(4 * 360 * FDays / 365.249, 0, 1, 0);
    glTranslated(10, 0, 0);

    glLineWidth(1);
    glCallList(cEjes);

    glColor3f(0.3, 0.3, 1);

    glPushMatrix();
    glScale(0.5, 0.5, 0.5);
    glCallList(cSphere);
    glPopMatrix();

    glRotated( 10 * 20.1 * FDays / DaysInMonth(Now), 0, 1, 0);
    glTranslated(1, 0, 0);
    glColor3f(0.3, 0.3, 0.3);

    glPushMatrix();
    glScale(0.2, 0.2, 0.2);
    glCallList(cSphere);
    glPopMatrix();


    glFlush;
    SwapBuffers(FDevice.Value);

    FTime := Sil.Os.Performance.ToMSeconds(FCounter);
  end;
end;

procedure TFormTest1.TimerTimer(Sender: TObject);
begin
  Inc(FDays);
  Caption := 'frames ' + Float.ToStr(1000 / FFrames, 3, 2) + ' tiempo ' + Float.ToStr(FTime, 3, 2);
  edViewX.Value := FViewpoint.X;
  edViewY.Value := FViewpoint.Y;
  edViewZ.Value := FViewpoint.Z;
end;

procedure TFormTest1.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style or WS_CLIPSIBLINGS or WS_CLIPCHILDREN;
end;

procedure TFormTest1.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 0;
end;

procedure TFormTest1.ApplyClick(Sender: TObject);
begin
  FViewPoint := Point(edViewX.Value, edViewY.Value, edViewZ.Value);
  DoSetupProjection;
  Invalidate;
end;

procedure TFormTest1.FormShow(Sender: TObject);
begin
  DoSetupProjection;
  Invalidate;
end;

procedure TFormTest1.DoSetupProjection;
begin
  FViewPort := Self.ClientRect;

  glMatrixMode(GL_PROJECTION);

  glLoadIdentity;

  with FViewPort do
  begin
    gluPerspective(edField.Value,  (Right - Left) / (Bottom - Top), edZnear.Value, edZfar.Value);
    glViewport(Left, Top, Right - Left, Bottom - Top);
  end;
end;

procedure TFormTest1.btDecXClick(Sender: TObject);
begin
  edViewX.Value := edViewX.Value - 1;
  ApplyClick(nil);
end;

procedure TFormTest1.btDecYClick(Sender: TObject);
begin
  edViewY.Value := edViewY.Value - 1;
  ApplyClick(nil);
end;

procedure TFormTest1.btDecZClick(Sender: TObject);
begin
  edViewZ.Value := edViewZ.Value - 1;
  ApplyClick(nil);
end;

procedure TFormTest1.btIncXClick(Sender: TObject);
begin
  edViewX.Value := edViewX.Value + 1;
  ApplyClick(nil);
end;

procedure TFormTest1.btIncYClick(Sender: TObject);
begin
  edViewY.Value := edViewY.Value + 1;
  ApplyClick(nil);
end;

procedure TFormTest1.btIncZClick(Sender: TObject);
begin
  edViewZ.Value := edViewZ.Value + 1;
  ApplyClick(nil);
end;

end.
