unit SilSmMotor;

{$INCLUDE Defines.inc}

interface

uses
  Sil, SilSiMotor, SilSiModel;

type
  TSilMotor = class(
    TSilObject,
    IMotor )
  private
    FOwner: TObject;
    procedure DoRules(First, Last: PIRule);
    procedure DoEntities(First, Last: PIEntity);
  protected // IMotor
    procedure Step(const Model: IModel);
  public
    constructor Create(Owner: TObject = nil);
    destructor Destroy; override;
  end;
  
implementation

{ TSilMotor }

constructor TSilMotor.Create(Owner: TObject);
begin
  inherited Create;
  FOwner := Owner;
end;

destructor TSilMotor.Destroy;
begin
  FOwner := nil;
  inherited;
end;

procedure TSilMotor.DoEntities(First, Last: PIEntity);
begin
  while First <> Last do
  begin
    with First.Rules do DoRules(First, Last);
    Inc(First);
  end;
end;

procedure TSilMotor.DoRules(First, Last: PIRule);
begin
  while First <> Last do
  begin
    First.Update;
    Inc(First);
  end;
end;

procedure TSilMotor.Step(const Model: IModel);
begin
  with Model do
  begin
    with Rules do DoRules(First, Last);
    with Entities do DoEntities(First, Last);
  end;
end;

end.
