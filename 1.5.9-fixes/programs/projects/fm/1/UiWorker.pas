unit UiWorker;

interface

uses
  Sil;

type
  IFmWorkerThread = interface;
  IFmWorkerItem = interface;
  IFmWorkerResult = interface;

  IFmWorkerThread = interface
    ['{0B04D584-FD1A-48ED-9A70-FD8EABBCAE11}']
    function Post(const Item: IFmWorkerItem; const Data: IUnknown = nil; Param: Pointer = nil): IFmWorkerResult;
    procedure Clear;
    procedure Terminate;
  end;

  IFmWorkerItem = interface
    ['{8A71953A-A3DE-4D28-B5EF-563AD5B68163}']
    procedure Execute(const Owner, Data: IUnknown; Param: Pointer; out Result: LongWord);
  end;

  IFmWorkerResult = interface(IWaitable)
    ['{DCEA2F72-3080-4990-BF91-1F04344F28EC}']
    function GetResult: LongWord;
    property Result: LongWord read GetResult;
  end;

implementation

end.
 