unit SilOmAdoptRunnable;

interface

uses
  SilLkAggregable,
  
  SilLiTraits,
  SilOiThread;

type
  TSilAdoptRunnable = class(
    TSilAggregableObject,
    IAdoptionTrait,
    IRunnable )
  private
    FInstance: Pointer;
  private 
    function DoGetRunnable: IRunnable;
  protected // IRunnable
    property Runnable: IRunnable read DoGetRunnable implements IRunnable;
  public
    constructor Create(const Controller: IUnknown; const Runnable: IRunnable);
    destructor Destroy; override;
  end;

implementation

uses
  SilLfTraits;

{ TSilAdoptRunnable }

constructor TSilAdoptRunnable.Create(const Controller: IUnknown; const Runnable: IRunnable);
begin
  inherited Create(Controller);
  Adopt(@FInstance, Runnable);
end;

destructor TSilAdoptRunnable.Destroy;
begin
  Adopt(@FInstance, nil);
  inherited;
end;

function TSilAdoptRunnable.DoGetRunnable: IRunnable;
begin
  Result := IRunnable(FInstance);
end;

end.
 