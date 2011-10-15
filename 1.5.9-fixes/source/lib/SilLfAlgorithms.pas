unit SilLfAlgorithms;

interface

uses
  Sil,
  SilLiContainerTypes,
  SilLiContainer;

function ForEach(const First, Last: IContainerCursor; const Kind: TGUID; const Action: IAction): IContainerCursor; overload;
function ForEach(const First: IContainerCursor; const Kind: TGUID; const Action: IAction): IContainerCursor; overload;
function FindFirst(const First, Last: IContainerCursor; const Kind: TGUID; const Test: ITester; out Value): Boolean; overload;

implementation

function ForEach(const First, Last: IContainerCursor; const Kind: TGUID; const Action: IAction): IContainerCursor;
begin
  if First.Clone(Kind, Result) then
    with Result do
      while not IsEqual(Last) do
      begin
        Action.Execute(Result, nil);
        Result.Next()
      end;
end;

function ForEach(const First: IContainerCursor; const Kind: TGUID; const Action: IAction): IContainerCursor;
begin
  if First.Clone(Kind, Result) then
    with Result do
      while not IsValid do
      begin
        Action.Execute(Result, nil);
        Result.Next()
      end;
end;

function FindFirst(const First, Last: IContainerCursor; const Kind: TGUID; const Test: ITester; out Value): Boolean; overload;
begin
  Result := First.Clone(Kind, Value);
  if Result then
    with IContainerCursor(Value) do
      while not Result do  
        Result := not IsEqual(Last) and not Test.Execute(IContainerCursor(Value), nil) and Next();
end;

end.
