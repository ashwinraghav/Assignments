unit SilSm3dgOpenGL;

interface

uses
  Windows, OpenGL, 
  Sil,
  SilSe3DGL, SilSi3DGL, SilSe3dgOpenGL;

type
  TSil3dgOpenglEngine = class(
    TSilObject,
    I3dgEngine )
  private
    FList: IInterfaceList;
  protected // I3dgEngine
    function CreateContext(const Device: IHandle; const Data): I3dgContext;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TSil3dgOpenglContext = class(
    TSilObject,
    I3dgSetting,
    I3dgContext )
  private
    FDevice: IHandle;
    FHandle: THandle;
    FViewPort: R2IRect;
    FCamera: I3dgDirection;
    FObjects: IStringList;
    FLights: IInterfaceList;
		FFramesPerSec: Cardinal;
		FStart: TDateTime;
		FFrames: Cardinal;
  private
    procedure DoMakeCurrent;
    procedure DoSetPixelFormat(const DeviceHandle: THandle; const RequestedFormat: TPixelFormatDescriptor);
    procedure DoRenderObjects(const Builder: I3dgMatrixBuilder);    
  protected // I3dgContext
    function GetViewPort: P2IRect;
    function GetCamera: I3dgDirection;
    function GetFramesPerSec: Cardinal; 
    function Projection: I3dgMatrix;
    function CreateObject(const Name: string): I3dgObject;
    function CreateLight(const Name: string): I3dgLight;
    procedure Resync(const Builder: I3dgMatrixBuilder);
    procedure MakeCurrent;
    procedure Render;
  public
    constructor Create(const Device: IHandle; const Data: R3dgContextParams);
    destructor Destroy; override;
    property ViewPort: P2IRect read GetViewPort;
    property Camera: I3dgDirection read GetCamera;
  end;

  TSil3dgOpenglDirection = class(
    TSilObject,
    I3dgSetting,
    I3dgPosition,
    I3dgDirection )
  private
    FPosition: R3DPoint;
    FDirection: R3DVector;
  protected // I3dgPosition
    function GetPosition: P3DPoint;
  protected // I3dgDirection
    function GetDirection: P3DVector;
  protected
    procedure Resync(const Builder: I3dgMatrixBuilder); virtual;
  public
    constructor Create;
    destructor Destroy; override;
  public
    property Position: P3DPoint read GetPosition;
    property Direction: P3DVector read GetDirection;
  end;

  TSil3dgOpenglObject = class(
    TSil3dgOpenglDirection,
    I3dgObject )
  private
    FIdx: Integer;
    FName: string;
  protected // I3dgObject
    function GetID: Integer;
    function GetName: string;
    function Define: I3dgObjectBuilder;
  public
    constructor Create(const Idx: Integer; const Name: string);
    destructor Destroy; override; 
  public
    property ID: Integer read GetID;
    property Name: string read GetName;
  end;

  TSil3dgOpenglLight = class(
    TSil3dgOpenglObject,
    I3dgLight )
  private
    FAngle: LongWord;
  protected
    procedure Resync(const Builder: I3dgMatrixBuilder); override; 
  protected // I3dgLight
    function GetAngle: PLongWord;
  public
    destructor Destroy; override; 
  end;
  
  TSil3dgOpenglObjectBuilder = class(
    TSilObject,
    I3dgObjectBuilder )
  private
    FObject: TSil3dgOpenglObject;
    FApplied: Boolean;
  protected // I3dgObjectBuilder
    procedure Vertex(const X, Y, Z: Double); overload;
    procedure Vertex(const Point: R3DPoint); overload;
    procedure Normal(const X, Y, Z: Double); overload;
    procedure Normal(const Point: R3DVector); overload;
    procedure Color(const Value: TGLColor); overload;
    procedure Square(const Position: R3DPoint; const Delta: R3DVector; AColor: TGLColor; Orientation: Integer);
    procedure Apply;
  public
    constructor Create(const AObject: TSil3dgOpenglObject);
    destructor Destroy; override;
  end;

  TSil3dgOpenglMatrix = class(
    TSilObject,
    I3dgMatrix )
  private
    FPrev: GLenum;
    FMode: GLenum;
  protected
  public
    constructor Create(Mode: GLenum);
    destructor Destroy; override;
  end;

  TSil3dgOpenglMatrixBuilder = class(
    TSilObject,
    I3dgMatrixBuilder )
  protected // I3dgMatrixContruction
    procedure Identity;
    procedure Perspective(
      const FieldOfView: Double;
      const AspectRatio: Double;
      const ZClippingNear: Double;
      const ZClippingFar: Double);
    procedure ViewPort(
            Left, Top: Integer;
            Width, Height: Integer);
  public
    constructor Create(const Matrix: I3dgMatrix);
    destructor Destroy; override; 
  end;    
    

implementation

uses
  SilSf3dgOpengl, SilLtList, SilLiStringList, SilSt3dgOpenGL;

{ TSil3dgOpenglEngine }

constructor TSil3dgOpenglEngine.Create;
begin
  inherited Create;
  FList := Sil.List.InterfaceList();
end;

destructor TSil3dgOpenglEngine.Destroy;
begin
  if Assigned(FList) then
    FList.Clear;
  FList := nil;
  inherited;
end;

function TSil3dgOpenglEngine.CreateContext(const Device: IHandle; const Data): I3dgContext;
begin
  Result := TSil3dgOpenglContext.Create(Device, R3dgContextParams(Data));
  FList.Add(Result);
end;

{ TSil3dgOpenglContext }

constructor TSil3dgOpenglContext.Create(const Device: IHandle; const Data: R3dgContextParams);
var
	LightModel: array[0..3] of GLfloat;
begin
  inherited Create;
	FFramesPerSec := 0;
	FStart := 0;
	FFrames := 0;
  
  FDevice := Device;
  FObjects := Sil.List.StringList(False, InterfaceHandler);
  FLights := Sil.List.InterfaceList();

  DoSetPixelFormat(FDevice.Value, PixelFormat(Data.PixelFormat));
  FHandle := Windows.wglCreateContext(FDevice.Value);
  if FHandle = 0 then raise Sil.Os.Error.Create(Windows.GetLastError(), 'TSil3dgOpenglContext.Create [OpenGL.wglCreateContext]');

  FCamera := TSil3dgOpenglDirection.Create;

	DoMakeCurrent;

	glClearColor(0.0, 0.0, 0.0, 0.0);
	glClearDepth(1.0);

	// Enable depth calculations
	glEnable(GL_DEPTH_TEST);

	// Set the material color to follow the current color
	glColorMaterial(GL_FRONT, GL_AMBIENT_AND_DIFFUSE);
	glEnable(GL_COLOR_MATERIAL);

	// Enable lighting calculations
	glEnable(GL_LIGHTING);

	// Set the color to clear the screen
	glClearColor(0.0, 0.0, 0.0, 0.0);

//

	LightModel[0] := 0.3;
	LightModel[1] := 0.3;
	LightModel[2] := 0.3;
	LightModel[3] := 1;
	glLightModelfv(GL_LIGHT_MODEL_AMBIENT, @LightModel);

	LightModel[0] := 0.2;
	LightModel[1] := 0.2;
	LightModel[2] := 0.2;
	LightModel[3] := 1;
	glMaterialfv(GL_FRONT_AND_BACK, GL_EMISSION, @LightModel);

end;

destructor TSil3dgOpenglContext.Destroy;
begin
  FCamera := nil;
  Windows.wglMakeCurrent(0, 0);
  if FHandle > 0 then
    Windows.wglDeleteContext(FHandle);
  FLights := nil;
  FObjects := nil;
  FDevice := nil;
  inherited;
end;

function TSil3dgOpenglContext.Projection: I3dgMatrix;
begin
//  Result := TSil3dgOpenglMatrix.Create(GL_PROJECTION);
end;

function TSil3dgOpenglContext.CreateObject(const Name: string): I3dgObject;
var
  Index: Integer;
begin
  Index := FObjects.Count + 1;
  Result := TSil3dgOpenglObject.Create(Index, Name);
  FObjects.Add(Name, @Result);
end;

function TSil3dgOpenglContext.CreateLight(const Name: string): I3dgLight;
var
  Index: Integer;
begin
  Index := FLights.Count + 1;
  Result := TSil3dgOpenglLight.Create(Index, Name);
  FLights.Add(Result);
end;

function TSil3dgOpenglContext.GetCamera: I3dgDirection;
begin
  Result := FCamera;
end;

function TSil3dgOpenglContext.GetViewPort: P2IRect;
begin
  Result := @FViewPort;
end;

function TSil3dgOpenglContext.GetFramesPerSec: Cardinal;
begin
  Result := FFramesPerSec;
end;

procedure TSil3dgOpenglContext.Resync(const Builder: I3dgMatrixBuilder);
begin
  with FViewPort do Builder.ViewPort(Origin.X, Origin.Y, Size.X, Size.Y);
  FCamera.Resync(Builder);
end;

procedure TSil3dgOpenglContext.DoSetPixelFormat(const DeviceHandle: THandle; const RequestedFormat: TPixelFormatDescriptor);
var
  Index: Integer;
  PixelFormat: TPixelFormatDescriptor;
begin
	Index := ChoosePixelFormat(DeviceHandle, @RequestedFormat);
  if Index < 1 then
    raise Sil.Os.Error.Create(GetLastError, 'TSil3dgOpenglContext.DoSetPixelFormat [Windows.ChoosePixelFormat]');
	Sil.Os.Error.Check(
    DescribePixelFormat(DeviceHandle, Index, SizeOf(PixelFormat), PixelFormat), 'TSil3dgOpenglContext.DoSetPixelFormat [Windows.DescribePixelFormat]');
	Sil.Os.Error.Check(
    SetPixelFormat(DeviceHandle, Index, @PixelFormat), 'TSil3dgOpenglContext.DoSetPixelFormat [Windows.SetPixelFormat]');
end;

procedure TSil3dgOpenglContext.MakeCurrent;
begin
  DoMakeCurrent;
end;

procedure TSil3dgOpenglContext.Render;
(*)var
  Model: I3dgMatrix;
  Builder: I3dgMatrixBuilder;(*)
begin
  (*)
	DoMakeCurrent;

  Builder := Transform.Contruct(Projection());
  Builder.Identity();
  Builder.Perspective(40.0, ViewPort.Size.x / ViewPort.Size.y, 0.01, 500.0);

  Resync(Builder);

	glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  Model := TSil3dgOpenglMatrix.Create(GL_MODELVIEW);
  try
    Builder := TSil3dgOpenglMatrixBuilder.Create(Model);
    try
      DoRenderObjects(Builder);
    finally
      Builder := nil;
    end;
  finally
    Model := nil;
  end;

	glFlush;
	SwapBuffers(FDevice.Value);

	if FFrames = 0 then FStart := Sil.DateTime.Now;
	Inc(FFrames);
	if Sil.DateTime.Now - FStart > 1 / 24 / 60 / 60 then
	begin
		FFramesPerSec := FFrames;
		FFrames := 0;
	end;
  (*)
end;

procedure TSil3dgOpenglContext.DoMakeCurrent;
begin
	if FHandle <> wglGetCurrentContext() then
    wglMakeCurrent(FDevice.Value, FHandle);
end;

procedure TSil3dgOpenglContext.DoRenderObjects(const Builder: I3dgMatrixBuilder);
var
  Enum: IEnumerator;
  Name: string;
  Item: I3dgObject;
begin
  with FObjects do
    while Enumerate(Enum, Name) do
    begin
      Item := IUnknown(Ptrs[Enum.Iteration]) as I3dgObject;
      Builder.Identity;
      Item.Resync(Builder);
      glCallList(Item.ID);
    end;
end;

{ TSil3dgOpenglDirection }

constructor TSil3dgOpenglDirection.Create;
begin
  inherited Create;
end;

destructor TSil3dgOpenglDirection.Destroy;
begin
  inherited;
end;

function TSil3dgOpenglDirection.GetPosition: P3DPoint;
begin
  Result := @FPosition;
end;

function TSil3dgOpenglDirection.GetDirection: P3DVector;
begin
  Result := @FDirection;
end;

procedure TSil3dgOpenglDirection.Resync(const Builder: I3dgMatrixBuilder);
begin
	glTranslated(FPosition.X, FPosition.Y, FPosition.Z);
	glRotated(FDirection.X, 1.0, 0.0, 0.0); // rotate about the x unit vector
	glRotated(FDirection.Y, 0.0, 1.0, 0.0); // rotate about the y unit vector
	glRotated(FDirection.Z, 0.0, 0.0, 1.0); // rotate about the z unit vector
end;

{ TSil3dgOpenglLight }

destructor TSil3dgOpenglLight.Destroy;
begin
  inherited;
end;

function TSil3dgOpenglLight.GetAngle: PLongWord;
begin
  Result := @FAngle;
end;

procedure TSil3dgOpenglLight.Resync(const Builder: I3dgMatrixBuilder);
var
	Param: array[0..3] of GLFloat;
begin
	Param[0] := FPosition.X;
	Param[1] := FPosition.Y;
	Param[2] := FPosition.Z;
	Param[3] := 1;
	glLightfv(GL_LIGHT0 + FIdx, GL_POSITION, @Param);
	Param[0] := FDirection.X;
	Param[1] := FDirection.Y;
	Param[2] := FDirection.Z;
	Param[3] := 1;
	glLightfv(GL_LIGHT0 + FIdx, GL_SPOT_DIRECTION, @Param);
  glLightf(GL_LIGHT0 + FIdx, GL_SPOT_CUTOFF, FAngle);
  glEnable(GL_LIGHT0 + FIdx);
end;

{ TSil3dgOpenglObject }

constructor TSil3dgOpenglObject.Create(const Idx: Integer; const Name: string);
begin
  inherited Create;
  FIdx := Idx;
  FName := Name;
end;

destructor TSil3dgOpenglObject.Destroy;
begin
  inherited;
end;

function TSil3dgOpenglObject.GetID: Integer;
begin
  Result := FIdx;
end;

function TSil3dgOpenglObject.GetName: string;
begin
  Result := FName;
end;

function TSil3dgOpenglObject.Define: I3dgObjectBuilder;
begin
  Result := TSil3dgOpenglObjectBuilder.Create(Self);
end;

{ TSil3dgOpenglObjectBuilder }

constructor TSil3dgOpenglObjectBuilder.Create(const AObject: TSil3dgOpenglObject);
begin
  inherited Create;
  FObject := AObject;
  if not glIsList(FObject.FIdx) then
    glNewList(FObject.FIdx, GL_COMPILE) else
    raise Sil.Error.Create('IsList!!');
end;

destructor TSil3dgOpenglObjectBuilder.Destroy;
begin
  Apply;
  FObject := nil;
  inherited;
end;

procedure TSil3dgOpenglObjectBuilder.Vertex(const X, Y, Z: Double);
begin
  OpenGL.glVertex3d(X, Y, Z);
end;

procedure TSil3dgOpenglObjectBuilder.Vertex(const Point: R3DPoint);
begin
  with Point do OpenGL.glVertex3d(X, Y, Z);
end;

procedure TSil3dgOpenglObjectBuilder.Normal(const X, Y, Z: Double);
begin
  OpenGL.glNormal3d(X, Y, Z);
end;

procedure TSil3dgOpenglObjectBuilder.Normal(const Point: R3DVector);
begin
  with Point do OpenGL.glNormal3d(X, Y, Z);
end;

procedure TSil3dgOpenglObjectBuilder.Color(const Value: TGLColor);
begin
  glColor3ubv(@Value);  
end;

procedure TSil3dgOpenglObjectBuilder.Apply;
begin
  if not FApplied then
  begin
	  glEndList();
    FApplied := True;
  end;
end;

procedure TSil3dgOpenglObjectBuilder.Square(const Position: R3DPoint; const Delta: R3DVector; AColor: TGLColor; Orientation: Integer);
begin
  glBegin(GL_QUADS);
	Color(AColor);
  if Delta.X = 0 then
    begin
      Normal(Orientation, 0.0, 0.0);
      Vertex(Position.x,            Position.y,             Position.z);
      Vertex(Position.x,            Position.y + Delta.y,   Position.z);
      Vertex(Position.x,            Position.y + Delta.y,   Position.z + Delta.z);
      Vertex(Position.x,            Position.y,             Position.z + Delta.z);
    end
  else if Delta.Y = 0 then
		begin
      Normal(0.0, Orientation, 0.0);
			Vertex(Position.x,            Position.y,             Position.z);
			Vertex(Position.x,            Position.y,             Position.z + Delta.z);
			Vertex(Position.x + Delta.x,  Position.y,             Position.z + Delta.z);
			Vertex(Position.x + Delta.x,  Position.y,             Position.z);
		end
  else
		begin
      Normal(0.0, 0.0, Orientation);
			Vertex(Position.x,            Position.y,             Position.z);
			Vertex(Position.x,            Position.y + Delta.y,   Position.z);
			Vertex(Position.x + Delta.x,  Position.y + Delta.y,   Position.z);
			Vertex(Position.x + Delta.x,  Position.y,             Position.z);
		end;
  glEnd();
end;

{ TSil3dgOpenglMatrix }

constructor TSil3dgOpenglMatrix.Create(Mode: GLenum);
begin
  inherited Create;
  glGetIntegerv(GL_MATRIX_MODE, @FPrev);
  FMode := Mode;
  glMatrixMode(Mode);
end;

destructor TSil3dgOpenglMatrix.Destroy;
begin
  glMatrixMode(FPrev);
  inherited;
end;

{ TSil3dgOpenglMatrixBuilder }

constructor TSil3dgOpenglMatrixBuilder.Create(const Matrix: I3dgMatrix);
begin
end;

destructor TSil3dgOpenglMatrixBuilder.Destroy;
begin
  inherited;
end;

procedure TSil3dgOpenglMatrixBuilder.Identity;
begin
  OpenGL.glLoadIdentity();
end;

procedure TSil3dgOpenglMatrixBuilder.Perspective(const FieldOfView, AspectRatio, ZClippingNear, ZClippingFar: Double);
begin
  OpenGL.gluPerspective(FieldOfView, AspectRatio, ZClippingNear, ZClippingFar);
end;

procedure TSil3dgOpenglMatrixBuilder.ViewPort(Left, Top, Width, Height: Integer);
begin
  OpenGL.glViewPort(Left, Top, Width, Height);
end;

end.
