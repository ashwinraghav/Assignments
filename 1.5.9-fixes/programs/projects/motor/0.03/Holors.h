//---------------------------------------------------------------------------

#ifndef HolorsH
#define HolorsH




typedef
    double    TMerate;

typedef
    TMerate   *PMerate;

const
    CMerateSize   = sizeof(TMerate);

const
    CMerateLimit  = MaxLongint / CMerateSize;

typedef
    TMerate TMerateArray[CMerateLimit];

typedef
    TMerateArray  *PMerateArray;


class Holor
  {
    private:
      PMerate         FData;
      long            FSize;

    public:
      __fastcall Holor(long Size);
      __fastcall ~Holor();


      PMerateArray    __fastcall Data()  const;
      long            __fastcall Size()  const;

      PMerate         __fastcall First() const;
      PMerate         __fastcall Last()  const;
  };


void  __fastcall Sum(PMerate p, PMerate q, PMerate r);

//---------------------------------------------------------------------------
#endif
