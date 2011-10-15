unit SilSiNtlm;

interface

uses
  Sil,
  SilSeNtlm;

type
  IDesCipher = Interface
    ['{D09528E6-464D-4BC2-ADAF-B90ADEB13ACC}']
    procedure MDInit(var Context: TMD4Ctx);
    procedure MDUpdate(var Context: TMD4Ctx; Input: Pointer; InputLen: LongWord);
    function MDFinal(var Context: TMD4Ctx): String;
    function EcbEncrypt(const Key: String; const Data: array of Byte): String;
  end;

implementation

end.
 