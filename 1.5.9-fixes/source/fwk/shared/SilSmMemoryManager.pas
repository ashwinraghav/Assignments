unit SilSmMemoryManager;

interface

uses
  SilSiMemoryManager;

type
  TSharedMemoryManager = class(
    TInterfacedObject,
    ISharedMemoryManager )
  private
    FInstance: LongWord;
    FManager: TMemoryManager;
    FGetHeapStatus: function: THeapStatus; 
  protected // ISharedMemoryManager
    function GetInstance: LongWord;
    function GetIsAssigned: Boolean;
    function GetManager: TMemoryManager;
    function GetMemoryStatus: THeapStatus;
  public
    constructor Create(Instance: LongWord);
    destructor Destroy; override;
  end;

implementation

uses
  SilUtDll;

{ TSharedMemoryManager }

constructor TSharedMemoryManager.Create(Instance: LongWord);
begin
  inherited Create;
  FInstance := Instance;
  if not System.IsMemoryManagerSet then
  begin
    GetProcAddress('SilGetMem', FManager.GetMem, Instance);
    GetProcAddress('SilFreeMem', FManager.FreeMem, Instance);
    GetProcAddress('SilReallocMem', FManager.ReallocMem, Instance);
  end else
    System.GetMemoryManager(FManager);
  GetProcAddress('SilHeapStatus', FGetHeapStatus, Instance);
end;

destructor TSharedMemoryManager.Destroy;
begin
  inherited;
end;

function TSharedMemoryManager.GetInstance: LongWord;
begin
  Result := FInstance;
end;

function TSharedMemoryManager.GetIsAssigned: Boolean;
begin
  Result := Assigned(FManager.GetMem)
        and Assigned(FManager.FreeMem)
        and Assigned(FManager.ReallocMem);
end;

function TSharedMemoryManager.GetManager: TMemoryManager;
begin
  Result := FManager;
end;

function TSharedMemoryManager.GetMemoryStatus: THeapStatus;
begin
  Result := FGetHeapStatus();
end;

end.
 