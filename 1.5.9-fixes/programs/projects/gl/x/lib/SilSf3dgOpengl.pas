unit SilSf3dgOpengl;

interface

uses
  Windows, SilSe3dgOpenGL;
  
function PixelFlags(const Value: T3dgPixelFormatFlags): LongWord;
function PixelFormat(const Params: R3dgPixelFormat): TPixelFormatDescriptor;

implementation

uses
  SilSg3dgOpenGL;

function PixelFlags(const Value: T3dgPixelFormatFlags): LongWord;
var
	i: T3dgPixelFormatFlag;
begin
	Result := 0;
	for i := Low(i) to High(i) do
		if i in Value then
      Result := Result or G3dgPixelFormatMap[i];
end;

function PixelFormat(const Params: R3dgPixelFormat): TPixelFormatDescriptor;
begin
  with Result do
  begin
    nSize := SizeOf(Result);
    nVersion := 1;
    dwFlags := PixelFlags(Params.Flags);
    iPixelType := PFD_TYPE_RGBA;
    cColorBits := 24;
    cRedBits := 0; //3;
    cRedShift := 0; //0;
    cGreenBits := 0; //3;
    cGreenShift := 0; //3;
    cBlueBits := 0; //2;
    cBlueShift := 0; //6;
    cAlphaBits := 0; //0;
    cAlphaShift := 0; //0;
    cAccumBits := 0; //32;
    cAccumRedBits := 0; //11;
    cAccumGreenBits := 0; //11;
    cAccumBlueBits := 0; //10;
    cAccumAlphaBits := 0; //0;
    cDepthBits := 32;
    cStencilBits := 0; //8;
    cAuxBuffers := 0;
    bReserved := 0;
    dwLayerMask := 0;
    dwVisibleMask := 0;
    dwDamageMask := 0;
  end;
end;

end.
 