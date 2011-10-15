{********************************************************************************
 *                  Standard Interface Library (SIL)                            *
 *                                                                              *
 *       General purpose library whose design is based in STRONG                *
 *   use of interfaces.                                                         *
 *                                                                              *
 *                                                                              *
 *     Copyright (C) 2000 Mariano Podestá    antiriad@gmail.com                 *
 *     Copyright (C) 2000 Leandro Conde      lconde@str.com.ar                  *
 *     Copyright (C) 2000 Lisandro Podestá   lisandrop@movi.com.ar              *
 *                                                                              *
 *     See License.txt for details.                                             *
 *                                                                              *
 *   This library is free software; you can redistribute it and/or              *
 *   modify it under the terms of the GNU Lesser General Public                 *
 *   License as published by the Free Software Foundation; either               *
 *   version 2.1 of the License, or (at your option) any later version.         *
 *                                                                              *
 *   This library is distributed in the hope that it will be useful,            *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of             *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          *
 *   Lesser General Public License for more details.                            *
 *                                                                              *
 *   You should have received a copy of the GNU Lesser General Public           *
 *   License along with this library; if not, write to the Free Software        *
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA  *
 *                                                                              *
 ********************************************************************************}

unit SilBeDataType;

{$I Defines.inc}

interface

uses
  SilBeTypes;

type
{$MINENUMSIZE 1}
  TDataFieldType = (
    ftUnknown,
    ftChar, ftString, ftWideString,
    ftSmallInt, ftInteger, ftLargeInt,
    ftByte, ftWord, ftLongWord,
    ftBoolean,
    ftFloat, ftCurrency,
    ftDate, ftTime, ftDateTime,
    ftBytes, ftVarBytes, ftAutoInc, ftBlob, ftMemo,
    ftDataSet, ftVariant, ftGuid, ftInterface, ftPointer);

type
  TDataTypeID = type LongWord;

type
{$MINENUMSIZE 1}
  TDataType = (
      dtEmpty             ,   //  0x0000 (00)
      dtNull              ,   //  0x0001 (01)
      dtSmallint          ,   //  0x0002 (02)
      dtLongInt           ,   //  0x0003 (03)
      dtSingle            ,   //  0x0004 (04)
      dtDouble            ,   //  0x0005 (05)
      dtCurrency          ,   //  0x0006 (06)
      dtDate              ,   //  0x0007 (07)
      dtWideString        ,   //  0x0008 (08)
      dtDispatch          ,   //  0x0009 (09)
      dtError             ,   //  0x000A (10)
      dtWordBool          ,   //  0x000B (11)
      dtVariant           ,   //  0x000C (12)
      dtInterface         ,   //  0x000D (13)
      dtDecimal           ,   //  0x000E (14)
      dtExtended          ,   //  0x000F (15)
      dtShortInt          ,   //  0x0010 (16)
      dtByte              ,   //  0x0011 (17)
      dtWord              ,   //  0x0012 (18)
      dtLongWord          ,   //  0x0013 (19)
      dtLargeInt          ,   //  0x0014 (20)
      dtLargeWord         ,   //  0x0015 (21)
      dtInteger           ,   //  0x0016 (22)
      dtCardinal          ,   //  0x0017 (23)
      dtVoid              ,   //  0x0018 (24)
      dtHRESULT           ,   //  0x0019 (25)
      dtPointer           ,   //  0x001A (26)
      dtSafearray         ,   //  0x001B (27)
      dtDynarray          ,   //  0x001C (28)
      dtUserdefined       ,   //  0x001D (29)
      dtPAnsiChar         ,   //  0x001E (30)
      dtPWideChar         ,   //  0x001F (31)
      //-------------------------------------
      dtGUID              ,   //  0x0020 (32)
      dtClass             ,   //  0x0021 (33)
      dtObject            ,   //  0x0022 (34)
      dtBoolean           ,   //  0x0023 (35)
      dtLongBool          ,   //  0x0024 (36)
      dtAnsiChar          ,   //  0x0025 (37)
      dtWideChar          ,   //  0x0026 (38)
      dtAnsiString    (*) ,   //  0x0027 (39)
      __dt__unused__0028  ,   //  0x0028 (40)
      __dt__unused__0029  ,   //  0x0029 (41)
      __dt__unused__002A  ,   //  0x002A (42)
      __dt__unused__002B  ,   //  0x002B (43)
      __dt__unused__002C  ,   //  0x002C (44)
      __dt__unused__002D  ,   //  0x002D (45)
      __dt__unused__002E  ,   //  0x002E (46)
      __dt__unused__002F  ,   //  0x002F (47)
      __dt__unused__0030  ,   //  0x0030 (48)
      __dt__unused__0031  ,   //  0x0031 (49)
      __dt__unused__0032  ,   //  0x0032 (50)
      __dt__unused__0033  ,   //  0x0033 (51)
      __dt__unused__0034  ,   //  0x0034 (52)
      __dt__unused__0035  ,   //  0x0035 (53)
      __dt__unused__0036  ,   //  0x0036 (54)
      __dt__unused__0037  ,   //  0x0037 (55)
      __dt__unused__0038  ,   //  0x0038 (56)
      __dt__unused__0039  ,   //  0x0039 (57)
      __dt__unused__003A  ,   //  0x003A (58)
      __dt__unused__003B  ,   //  0x003B (59)
      __dt__unused__003C  ,   //  0x003C (60)
      __dt__unused__003D  ,   //  0x003D (61)
      __dt__unused__003E  ,   //  0x003E (62)
      __dt__unused__003F  ,   //  0x003F (63)
      dtFileTime          ,   //  0x0040 (64)
      dtBlob              ,   //  0x0041 (65)
      __dt__unused__0042  ,   //  0x0042 (66)
      __dt__unused__0043  ,   //  0x0043 (67)
      __dt__unused__0044  ,   //  0x0044 (68)
      __dt__unused__0045  ,   //  0x0045 (69)
      __dt__unused__0046  ,   //  0x0046 (70)
      __dt__unused__0047  ,   //  0x0047 (71)
      __dt__unused__0048      //  0x0048 (72)
      //dtBytes           ,   //  0x0017 (00)
      //dtNumeric         ,(*)//  0x001A (00)
    );

const
  dtUnknown       = TDataType($FFFF); 

const
  dtU1            = TDataType(dtByte);
  dtI1            = TDataType(dtShortint);
  dtU2            = TDataType(dtWord);
  dtI2            = TDataType(dtSmallint);
  dtU4            = TDataType(dtLongWord);
  dtI4            = TDataType(dtLongInt);
  dtU8            = TDataType(dtLargeWord);
  dtI8            = TDataType(dtLargeInt);
  dtR4            = TDataType(dtSingle);
  dtR8            = TDataType(dtDouble);
  dtR10           = TDataType(dtExtended);

type
{$MINENUMSIZE 1}
  TDataFlag = (
      __df__Dummy__01,      //  $01
      __df__Dummy__02,      //  $02
      __df__Dummy__04,      //  $04
      __df__Dummy__08,      //  $08
      dfPointer,            //  $10
      dfArray,              //  $20
      dfByRef,              //  $40
      __df__Dummy__80       //  $80
    );

type
  TDataFlags = set of TDataFlag;

type
  RDataType = packed record
    case Boolean of
      False: (
        Value: TDataType;
        Flags: TDataFlags;
        Size: Word;
        );
      True: (
        ID: TDataTypeID;
        );
  end;

implementation
end.
