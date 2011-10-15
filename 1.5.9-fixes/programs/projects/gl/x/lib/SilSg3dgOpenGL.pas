unit SilSg3dgOpenGL;

interface

uses
  SilSe3dgOpenGL, Windows;

const
	G3dgPixelFormatMap: array[T3dgPixelFormatFlag] of LongWord =
    (
      PFD_DRAW_TO_WINDOW,
      PFD_DRAW_TO_BITMAP,
      PFD_SUPPORT_GDI,
      PFD_SUPPORT_OPENGL,
      PFD_GENERIC_ACCELERATED,
      PFD_GENERIC_FORMAT,
      PFD_NEED_PALETTE,
      PFD_NEED_SYSTEM_PALETTE,
      PFD_DOUBLEBUFFER,
      PFD_STEREO,
      PFD_SWAP_EXCHANGE,
      PFD_SWAP_COPY,
      PFD_SWAP_LAYER_BUFFERS,
      PFD_DEPTH_DONTCARE,
      PFD_DOUBLEBUFFER_DONTCARE,
      PFD_STEREO_DONTCARE
    );

implementation
end.
 