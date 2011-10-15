unit SilOmDispatchRunnable;

interface

uses
  SilLkAggregable,
  
  SilLiTraits,
  SilLiReference,
  SilOiThread;

type
  TSilAdaptDispatchRunnable = class(
    TSilAggregableObject,
    IRunnable,
    IDispatchable,
    IAdoptionTrait )
  private
    FInstance: Pointer;
    FId: Word;
  private
    function DoGetMsg(Id: Word; const Thread: IThread): RThreadRunMessage;
    procedure DoDispatch(Msg: RThreadRunMessage);
    function DoGetDispatchable: IDispatchable;
  protected // IRunnable
    procedure Run(const Thread: IThread);
  protected // IDispatchable
    property Dispatchable: IDispatchable read DoGetDispatchable implements IDispatchable;
  public
    constructor Create(const Controller: IUnknown; const Dispatchable: IDispatchable; Id: Word);
    destructor Destroy; override;
  end;

implementation

uses
  SilLtReference,
  SilLfTraits;

{ TSilAdaptDispatchRunnable }

constructor TSilAdaptDispatchRunnable.Create(const Controller: IUnknown; const Dispatchable: IDispatchable; Id: Word);
begin
  inherited Create(Controller);
  FId := Id;
  Adopt(@FInstance, Dispatchable);
end;

destructor TSilAdaptDispatchRunnable.Destroy;
begin
  Adopt(@FInstance, nil);
  inherited;
end;

procedure TSilAdaptDispatchRunnable.Run(const Thread: IThread);
begin
  DoDispatch(DoGetMsg(FId, Thread));
end;

function TSilAdaptDispatchRunnable.DoGetMsg(Id: Word; const Thread: IThread): RThreadRunMessage;
begin
  Result.Id := Id;
  Result.Thread := Thread;
  Result.Ptr := nil; 
end;

procedure TSilAdaptDispatchRunnable.DoDispatch(Msg: RThreadRunMessage);
begin
  IDispatchable(FInstance).Dispatch(Msg);
end;

function TSilAdaptDispatchRunnable.DoGetDispatchable: IDispatchable;
begin
  Result := IDispatchable(FInstance);
end;

end.
