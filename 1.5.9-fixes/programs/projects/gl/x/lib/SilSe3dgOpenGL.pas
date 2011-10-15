unit SilSe3dgOpenGL;

interface

type
	T3dgPixelFormatFlag = (
		pfDrawToWindow,
    pfDrawToBitmap,
    pfSupportGDI,
		pfSupportOpengl,
    pfGenericAccelerated,
    pfGenericFormat,
		pfNeedPalette,
    pfNeedSystemPalette,
    pfDoubleBuffer,
		pfStereo,
    pfSwapExchange,
    pfSwapCopy,
    pfSwapLayerBuffers,
		pfDepthDontCare,
    pfDoubleBufferDontCare,
    pfStereoDontCare
  );

  T3dgPixelFormatFlags = set of T3dgPixelFormatFlag;

  R3dgPixelFormat = record
    Flags: T3dgPixelFormatFlags;
  end;

  R3dgContextParams = record
    PixelFormat: R3dgPixelFormat;
  end;

implementation

uses
  SilSg3dgOpenGL;


end.
