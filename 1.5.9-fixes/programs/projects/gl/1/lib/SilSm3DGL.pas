unit SilSm3DGL;

interface

uses
  Sil,
  SilSe3DGL, SilSi3DGL;

type
  TSil3dgEngine = class(
    TSilObject,
    I3dgEngine )
  private 
  protected // I3dgEngine
    function CreateContext(const Handle: IHandle; const Data): I3dgContext;
  public
    constructor Create; 
    destructor Destroy; override; 
  end;

  TSil3dgContext = class(
    TSilObject,
    I3dgContext )
  private
  protected // I3dgContext
    function GetViewPort: P2IRect;
    function GetCamera: I3dgObject;
    function CreateObject(const Name: string): I3dgObject;
    procedure Resync;
  public
    property ViewPort: P2IRect read GetViewPort;
    property Camera: I3dgObject read GetCamera;
  end;

  TSil3dgObject = class(
    TSilObject,
    I3dgObject )
  protected // I3dgObject
    function GetID: Integer;
    function GetName: string;
    function GetPosition: P3SPoint;
    function GetDirection: P3SPoint;
    procedure Resync;
  public
    property ID: Integer read GetID;
    property Name: string read GetName;
    property Position: P3SPoint read GetPosition;
    property Direction: P3SPoint read GetDirection;
  end;

implementation

{ TSil3dgEngine }

constructor TSil3dgEngine.Create;
begin

end;

function TSil3dgEngine.CreateContext(const Handle: IHandle; const Data): I3dgContext;
begin

end;

destructor TSil3dgEngine.Destroy;
begin

  inherited;
end;

{ TSil3dgContext }

function TSil3dgContext.CreateObject(const Name: string): I3dgObject;
begin

end;

function TSil3dgContext.GetCamera: I3dgObject;
begin

end;

function TSil3dgContext.GetViewPort: P2IRect;
begin

end;

procedure TSil3dgContext.Resync;
begin

end;

{ TSil3dgObject }

function TSil3dgObject.GetDirection: P3SPoint;
begin

end;

function TSil3dgObject.GetID: Integer;
begin

end;

function TSil3dgObject.GetName: string;
begin

end;

function TSil3dgObject.GetPosition: P3SPoint;
begin

end;

procedure TSil3dgObject.Resync;
begin

end;

end.
