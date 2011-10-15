unit UfDemoService;

{$INCLUDE Defines.inc}

interface

uses
  Sil;

const
  GsDemo: TGUID = '{BD28C403-36C2-4D96-A835-5E69266355B2}';

type
  Demo1 = class(GlobalService)
    class function ID: TGUID; override;
    class function Create: IUnknown; override;
  end;

function Demo2: PGlobalService;

implementation

type
  TDemoService = class(TSilObject)
    constructor Create;
    destructor Destroy; override;
  end;

function DemoCreate(Self: PGlobalService; const Services: IGlobalServiceListV2): IUnknown;
begin
  Result := TDemoService.Create();
end;

const
  NmDemo: String[6] = 'Demo2'#0;
  MService: RGlobalService =
    (
      ID: @GsDemo;
      Name: @NmDemo;
      Create: DemoCreate;
    );

function Demo2: PGlobalService;
begin
  Result := @MService;
end;
    
{ Demo1 }

class function Demo1.Create: IUnknown;
begin
  Result := TDemoService.Create();
end;

class function Demo1.ID: TGUID;
begin
  Result := GsDemo;
end;

{ TDemoService }

constructor TDemoService.Create;
begin
  inherited Create;
end;

destructor TDemoService.Destroy;
begin
  inherited;
end;

end.
 