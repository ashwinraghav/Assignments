unit SilSi3DGL;

interface

uses
  Sil,
  SilSe3DGL;

type
  I3dgEngine = interface;
  I3dgContext = interface;
  I3dgPosition = interface;
  I3dgDirection = interface;
  I3dgMatrix = interface;
  I3dgMatrixBuilder = interface;
  I3dgLight = interface;
  I3dgObject = interface;
  I3dgObjectBuilder = interface;

  I3dgEngine = interface (IInterface)
    ['{808F3DF4-AE40-4440-BFB0-F14FFC77C6DA}']
    function CreateContext(const Handle: IHandle; const Data): I3dgContext;
  end;
  
  I3dgSetting = interface (IInterface)
    ['{F5A7F859-CD2A-459D-9D8A-066EAA283FBF}']
    procedure Resync(const Builder: I3dgMatrixBuilder);
  end;
  
  I3dgContext = interface (I3dgSetting)
    ['{632B8C02-181D-44D7-883B-3B6935008508}']
    function CreateLight(const Name: string): I3dgLight;
    function CreateObject(const Name: string): I3dgObject;
    function GetCamera: I3dgDirection;
    function GetFramesPerSec: Cardinal;
    function GetViewPort: P2IRect;
    procedure MakeCurrent;
    function Projection: I3dgMatrix;
    procedure Render;
    property Camera: I3dgDirection read GetCamera;
    property Frames: Cardinal read GetFramesPerSec;
    property ViewPort: P2IRect read GetViewPort;
  end;
  
  I3dgPosition = interface (I3dgSetting)
    ['{72E7DB89-B59C-44B0-8050-6EA6B151A798}']
    function GetPosition: P3DPoint;
    property Position: P3DPoint read GetPosition;
  end;
  
  I3dgDirection = interface (I3dgPosition)
    ['{9463CB76-A10B-4BC3-A1AE-DE08696A3D6B}']
    function GetDirection: P3DVector;
    property Direction: P3DVector read GetDirection;
  end;
  
  I3dgCamera = interface (I3dgDirection)
    ['{97AE0372-C78F-4D68-907B-F4F8C19D7FED}']
  end;
  
  (*)
    procedure RollX(const Step: GLFloat);
    procedure RollY(const Step: GLFloat);
    procedure RollZ(const Step: GLFloat); (*)

  I3dgMatrix = interface (IInterface)
    ['{BF094735-F259-4981-8429-16998088F3DB}']
  end;
  
  I3dgMatrixBuilder = interface (IInterface)
    ['{B43D0EBD-E762-404F-98C4-165262F32D39}']
    procedure Identity;
    procedure Perspective(const FieldOfView: Double; const AspectRatio: Double; 
            const ZClippingNear: Double; const ZClippingFar: Double);
    procedure ViewPort(Left: Integer; Top: Integer; Width: Integer; Height: 
            Integer);
  end;
  
  I3dgMatrixPrimitive = interface (IInterface)
    ['{7A1436DB-1BF1-4DD0-8559-E33B7775D741}']
  end;
  
  I3dgObject = interface (I3dgDirection)
    ['{BB01DDD9-7937-4AF2-9954-2A60E512950C}']
    function Define: I3dgObjectBuilder;
    function GetID: Integer;
    function GetName: string;
    procedure Resync(const Builder: I3dgMatrixBuilder);
    property ID: Integer read GetID;
    property Name: string read GetName;
  end;
  
  I3dgLight = interface (I3dgObject)
    ['{EEE896B5-FCDA-442A-B8B9-CF2F1EB74E7A}']
    function GetAngle: PLongWord;
    property Angle: PLongWord read GetAngle;
  end;
  
  I3dgObjectBuilder = interface (IInterface)
    ['{AF9E8D90-6FAC-4A93-BDC8-E69E32143111}']
    procedure Apply;
    procedure Color(const Value: TGLColor); overload;
    procedure Normal(const X, Y, Z: Double); overload;
    procedure Normal(const Point: R3DVector); overload;
    procedure Square(const Position: R3DPoint; const Delta: R3DVector; AColor: 
            TGLColor; Orientation: Integer = +1);
    procedure Vertex(const X, Y, Z: Double); overload;
    procedure Vertex(const Point: R3DPoint); overload;
  end;
  
implementation
end.
