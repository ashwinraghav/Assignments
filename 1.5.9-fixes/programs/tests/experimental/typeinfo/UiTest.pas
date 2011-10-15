unit UiTest;

interface

type
  {$M+}
  ITest = interface
    ['{E2E46528-F4CA-4AE0-80BF-C724D37DC761}']
    function GetSingle: Single; stdcall;
    procedure SetSingle(const Value: Single); stdcall;
    function GetDouble: Double; stdcall;
    procedure SetDouble(const Value: Double); stdcall;
    function GetExtended: Extended; stdcall;
    procedure SetExtended(const Value: Extended); stdcall;
    function GetString: string; stdcall;
    procedure SetString(const Value: string); stdcall;
    function GetBoolean: Boolean; stdcall;
    procedure SetBoolean(const Value: Boolean); stdcall;
    function GetInteger: Integer; stdcall;
    procedure SetInteger(const Value: Integer); stdcall;
    procedure VoidResult(const Value: string); stdcall;
    function StringResult(const Value: string): string; stdcall;
    function VariantResult(const Value: string): Variant; stdcall;
    function InterfaceResult(const Value: string): IUnknown; stdcall;
    procedure StringOut(const Value: string; out Result: string); stdcall;
    procedure VariantOut(const Value: string; out Result: Variant); stdcall;
    procedure InterfaceOut(const Value: string; out Result: IUnknown); stdcall;
    property Single: Single read GetSingle write SetSingle;
    property Double: Double read GetDouble write SetDouble;
    property Extended: Extended read GetExtended write SetExtended;
  end;
  {$M-}

implementation
end.
