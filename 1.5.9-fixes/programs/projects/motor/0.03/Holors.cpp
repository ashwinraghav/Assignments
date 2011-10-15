//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Holors.h"



__fastcall Holor::Holor(long Size)
{
  FSize = Size;
  FData = new TMerate[Size];
}

__fastcall Holor::~Holor()
{
    delete[] FData;
    FData = 0;
    FSize = 0;  
}

PMerateArray    __fastcall Holor::Data()  const
{
    return  (PMerateArray) FData;
}

long            __fastcall Holor::Size()  const
{
    return  FSize;
}

PMerate         __fastcall Holor::First() const
{
    return  (PMerate) FData;
}

PMerate         __fastcall Holor::Last()  const
{
    return  (PMerate) (FData + FSize);
}

//---------------------------------------------------------------------------


void  __fastcall Sum(PMerate p, PMerate q, PMerate r)
{
  while (p != q)
  {
    *p = *p + *r;
    asm { wait };
    p++;
    r++;
  }
}

#pragma package(smart_init)
