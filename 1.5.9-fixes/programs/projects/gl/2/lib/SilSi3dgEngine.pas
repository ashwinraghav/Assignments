unit SilSiGlEngine;

interface

uses
  Sil,
  SilSeGlTypes,
  SilSiGlTypes;

type
  IGlEngine = interface;
  IGlContext = interface;
  IGlLight = interface;
  IGlObject = interface;

  IGlEngine = interface
    ['{808F3DF4-AE40-4440-BFB0-F14FFC77C6DA}']
    function CreateContext(const Handle: IHandle; const Params): IGlContext;
  end;

  IGlContext = interface 
    ['{632B8C02-181D-44D7-883B-3B6935008508}']
    
  end;

  IGlPrimitives = interface
    ['{B55A16DF-1A40-4081-A724-6F30F03F6764}']
    
  end;

  IGlCamera = interface
    ['{97AE0372-C78F-4D68-907B-F4F8C19D7FED}']
  end;

  IGlObject = interface
    ['{BB01DDD9-7937-4AF2-9954-2A60E512950C}']
    function GetID: Integer;
    function GetName: string;
    function GetPosition: IGlPosition;
    function GetDirection: IGlDirection;
    property ID: Integer read GetID;
    property Name: string read GetName;
    property Position: IGlPosition read GetPosition;
    property Direction: IGlDirection read GetDirection;
  end;

  IGlLight = interface (IGlObject)
    ['{EEE896B5-FCDA-442A-B8B9-CF2F1EB74E7A}']
    function GetAngle: PLongWord;
    property Angle: PLongWord read GetAngle;
  end;

implementation
end.
