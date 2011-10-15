unit SilSiLists;

interface

uses
  Sil;

type
  PIObject = ^IObject;
  PIInterface = ^IInterface;

  ICollection = interface
    ['{A9E38B04-F020-436F-8153-AE72F1521634}']
    function GetCount: Integer;
    property Count: Integer read GetCount;
  end;

  IInterfaces = interface (ICollection)
    ['{40522552-820F-46A9-BA86-AF3BB05E9AF4}']
    function GetItem(Index: Integer): IInterface;
    function GetObject(const IID: TGUID): IInterface;
    property Item[Index: Integer]: IInterface read GetItem;
    function Enumerate(var Enum; out Item: IInterface): Boolean; overload;
    function Lookup(const IID: TGUID; out Obj): Boolean; overload;
    property Value[const IID: TGUID]: IInterface read GetObject; default;
  end;

  IObjects = interface (ICollection)
    ['{151BC01B-132F-48B0-AE1A-EBEFCF1CA5E7}']
    function GetItem(Index: Integer): IObject;
    function GetObject(const Name: string): IObject;
    function Enumerate(var Enum; out Item: IObject): Boolean;
    function Lookup(const Name: string; const IID: TGUID; out Obj): Boolean; overload;
    property Item[Index: Integer]: IObject read GetItem;
    property Value[const Name: string]: IObject read GetObject; default;
  end;

  IObjectList = interface (IObjects)
    ['{BEDF3AEC-5FF2-4E32-82A4-6C1D86F26EA9}']
    function Add(const Item: IObject): Integer; overload;
    function Remove(const Item: IObject): Integer; overload;
    function Remove(const Name: string): Integer; overload;
    procedure Remove(Index: Integer); overload;
  end;

implementation
end.
 