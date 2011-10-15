unit SilSeFirebird;

interface

uses
  Sil,
  SilSeFirebirdClient;

type
  EFirebirdError = class(Exception)
  private
    FSQLCode: Long;
    FErrorCode: Long;
  public
    constructor Create(SQLCode: Long; const Msg: string); overload;
    constructor Create(SQLCode: Long; ErrorCode: Long; const Msg: string); overload;
    property SQLCode: Long read FSQLCode;
    property ErrorCode: Long read FErrorCode;
  end;

  EFirebirdServerError         = class(EFirebirdError);
  EFirebirdAccessDenied     = class(EFirebirdError);
  EFirebirdClientError            = class(EFirebirdError);

implementation

{ EFirebirdError }

constructor EFirebirdError.Create(SQLCode: Long; const Msg: string);
begin
  inherited Create(Msg);
  FSQLCode := SQLCode;
end;

constructor EFirebirdError.Create(SQLCode: Long; ErrorCode: Long; const Msg: string);
begin
  inherited Create(Msg);
  FSQLCode :=  SQLCode;
  FErrorCode := ErrorCode;
end;

end.
