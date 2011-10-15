unit UmBlock;

interface

uses
  Sil, SilContainer, Windows,
  UiBlock;

type
  TSilPageManager = class(
    TSilObject,
    IPageManager )
  private
    FPages: IPageList;
  protected // IPageManager
    function GetPages: IPageList;
    function Get(Size: Integer): Pointer;
    procedure Free(var Memory);
  public
    constructor Create(PageSize: Integer = CDefPageSize; Flags: LongWord = PAGE_READWRITE);
    destructor Destroy; override;
  end;

  TSilPageList = class(
    TSilObject,
    IPageList )
  private
    FList: IContainerDynamic;
    FPageSize: Integer;
    FPageFlags: LongWord;
  private
    function DoCompare(Data1, Data2: HData; Param: Pointer): Integer;
  protected // IPageList
    function GetCount: Integer;
    function Enumerate(var Enum: IEnumerator; out Page: IPage): Boolean;
    function Find(Memory: Pointer; out Page: IPage): Boolean; overload;
    function Find(Size: Integer; out Page: IPage): Boolean; overload;
    function Get(Size: Integer): IPage;
    procedure Release(var Page: IPage);
  public
    constructor Create(PageSize: Integer; Flags: LongWord);
    destructor Destroy; override;
  end;

implementation

{ TSilPageManager }

constructor TSilPageManager.Create(PageSize: Integer; Flags: LongWord);
begin
  inherited Create;
  FPages := TSilPageList.Create(PageSize, Flags);
end;

destructor TSilPageManager.Destroy;
begin
  FPages := nil;
  inherited;
end;

function TSilPageManager.GetPages: IPageList;
begin
  Result := FPages;
end;

function TSilPageManager.Get(Size: Integer): Pointer;
begin
  Result := FPages.Get(Size).Alloc(Size);
end;

procedure TSilPageManager.Free(var Memory);
var
  Page: IPage;
begin
  if FPages.Find(Pointer(Memory), Page) then
    Page.Free(Memory) else
    System.Error(reInvalidPtr);
end;

{ TSilPageList }

constructor TSilPageList.Create(PageSize: Integer; Flags: LongWord);
begin
  inherited Create;
  FList := Container.Create(List, Handler.Create(TypeInfo(IPage), Compare.Create(DoCompare())));
  FPageSize := PageSize;
  FPageFlags := Flags;
end;

destructor TSilPageList.Destroy;
begin
  FList := nil;
  inherited;
end;

function TSilPageList.GetCount: Integer;
begin
  Result := FList.Items.Count;
end;

function TSilPageList.Enumerate(var Enum: IEnumerator; out Page: IPage): Boolean;
begin
  
end;

function TSilPageList.Find(Memory: Pointer; out Page: IPage): Boolean;
begin

end;

function TSilPageList.Find(Size: Integer; out Page: IPage): Boolean;
begin

end;

function TSilPageList.Get(Size: Integer): IPage;
begin
  if not Find(Size, Result) then
    Result := nil;
end;

procedure TSilPageList.Release(var Page: IPage);
begin

end;

function TSilPageList.DoCompare(Data1, Data2: HData; Param: Pointer): Integer;
begin
  Result := Integer(Data1) - Integer(Data2);
end;

end.
 