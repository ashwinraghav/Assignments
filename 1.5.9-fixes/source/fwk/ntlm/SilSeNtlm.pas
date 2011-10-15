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

unit SilSeNtlm;

interface

{$include Defines.inc}

type
  PBArray = ^TBArray;
  TBArray = array[0..0] of Byte;

  PDWordArray = ^TDWordArray;
  TDWordArray = array [0..0] of LongWord;

  PMD4Ctx = ^TMD4Ctx;
  TMD4Ctx = packed record
    state: array[0..3] of LongWord;
    count: array[0..1] of LongWord;
    buffer: array[0..63] of Byte;
  end;

  PNtlmMessage1 = ^TNtlmMessage1;
  TNtlmMessage1 = packed record
    Protocol: array [0..7] of Char;
    MsgType: Char;
    Zero1: array [0..2] of Byte;
    Flags: Word;
    Zero2: array [0..1] of Byte;
    LenDomain1: Word;
    LenDomain2: Word;
    OffSetDomain: Word;
    Zero3: array [0..1] of Byte;
    LenHost1: Word;
    LenHost2: Word;
    OffSetHost: Word;
    Zero4: array [0..1] of Byte;
  end;

  PNtlmMessage2 = ^TNtlmMessage2;
  TNtlmMessage2 = packed record
    Protocol: array [0..7] of Char;
    MsgType: Char;
    Zero1: array [0..6] of Byte;
    LenMessage: Word;
    Zero2: array [0..1] of Byte;
    Flags: Word;
    Zero3: array [0..1] of Byte;
    Nonce: array [0..7] of Byte;
    Zero4: array [0..7] of Byte;
  end;

  PNtlmMessage3 = ^TNtlmMessage3;
  TNtlmMessage3 = packed record
    Protocol: array [0..7] of Char;
    MsgType: Char;
    Zero1: array [0..2] of Byte;
    Len_LM_Resp1: Word;
    Len_LM_Resp2: Word;
    OffSet_LM_Resp: Word;
    Zero2: array [0..1] of Byte;
    Len_NT_Resp1: Word;
    Len_NT_Resp2: Word;
    OffSet_NT_Resp: Word;
    Zero3: array [0..1] of Byte;
    LenDomain1: Word;
    LenDomain2: Word;
    OffSetDomain: Word;
    Zero4: array [0..1] of Byte;
    LenUser1: Word;
    LenUser2: Word;
    OffSetUser: Word;
    Zero5: array [0..1] of Byte;
    LenHost1: Word;
    LenHost2: Word;
    OffSetHost: Word;
    Zero6: array [0..5] of Byte;
    LenMessage: Word;
    Zero7: array [0..1] of Byte;
    Flags: Word;
    Zero8: array [0..1] of Byte;
  end;

const
  IP : Array [1..64] of Byte =( 58,50,42,34,26,18,10,2,
                                60,52,44,36,28,20,12,4,
                                62,54,46,38,30,22,14,6,
                                64,56,48,40,32,24,16,8,
                                57,49,41,33,25,17, 9,1,
                                59,51,43,35,27,19,11,3,
                                61,53,45,37,29,21,13,5,
                                63,55,47,39,31,23,15,7);

  InvIP : Array [1..64] of Byte =( 40, 8,48,16,56,24,64,32,
                                   39, 7,47,15,55,23,63,31,
                                   38, 6,46,14,54,22,62,30,
                                   37, 5,45,13,53,21,61,29,
                                   36, 4,44,12,52,20,60,28,
                                   35, 3,43,11,51,19,59,27,
                                   34, 2,42,10,50,18,58,26,
                                   33, 1,41, 9,49,17,57,25);

  E : Array [1..48] of Byte =( 32, 1, 2, 3, 4, 5,
                                4, 5, 6, 7, 8, 9,
                                8, 9,10,11,12,13,
                               12,13,14,15,16,17,
                               16,17,18,19,20,21,
                               20,21,22,23,24,25,
                               24,25,26,27,28,29,
                               28,29,30,31,32, 1);

  P : Array [1..32] of Byte =( 16, 7,20,21,
                               29,12,28,17,
                                1,15,23,26,
                                5,18,31,10,
                                2, 8,24,14,
                               32,27, 3, 9,
                               19,13,30, 6,
                               22,11, 4,25);

  SBoxes : Array [1..8,0..3,0..15] of Byte =
          ( ((14, 4,13, 1, 2,15,11, 8, 3,10, 6,12, 5, 9, 0, 7),
            (  0,15, 7, 4,14, 2,13, 1,10, 6,12,11, 9, 5, 3, 8),
            (  4, 1,14, 8,13, 6, 2,11,15,12, 9, 7, 3,10, 5, 0),
            ( 15,12, 8, 2, 4, 9, 1, 7, 5,11, 3,14,10, 0, 6,13)),

            ((15, 1, 8,14, 6,11, 3, 4, 9, 7, 2,13,12, 0, 5,10),
            (  3,13, 4, 7,15, 2, 8,14,12, 0, 1,10, 6, 9,11, 5),
            (  0,14, 7,11,10, 4,13, 1, 5, 8,12, 6, 9, 3, 2,15),
            ( 13, 8,10, 1, 3,15, 4, 2,11, 6, 7,12, 0, 5,14, 9)),

            ((10, 0, 9,14, 6, 3,15, 5, 1,13,12, 7,11, 4, 2, 8),
            ( 13, 7, 0, 9, 3, 4, 6,10, 2, 8, 5,14,12,11,15, 1),
            ( 13, 6, 4, 9, 8,15, 3, 0,11, 1, 2,12, 5,10,14, 7),
            (  1,10,13, 0, 6, 9, 8, 7, 4,15,14, 3,11, 5, 2,12)),

            (( 7,13,14, 3, 0, 6, 9,10, 1, 2, 8, 5,11,12, 4,15),
            ( 13, 8,11, 5, 6,15, 0, 3, 4, 7, 2,12, 1,10,14, 9),
            ( 10, 6, 9, 0,12,11, 7,13,15, 1, 3,14, 5, 2, 8, 4),
            (  3,15, 0, 6,10, 1,13, 8, 9, 4, 5,11,12, 7, 2,14)),

            (( 2,12, 4, 1, 7,10,11, 6, 8, 5, 3,15,13, 0,14, 9),
            ( 14,11, 2,12, 4, 7,13, 1, 5, 0,15,10, 3, 9, 8, 6),
            (  4, 2, 1,11,10,13, 7, 8,15, 9,12, 5, 6, 3, 0,14),
            ( 11, 8,12, 7, 1,14, 2,13, 6,15, 0, 9,10, 4, 5, 3)),

            ((12, 1,10,15, 9, 2, 6, 8, 0,13, 3, 4,14, 7, 5,11),
            ( 10,15, 4, 2, 7,12, 9, 5, 6, 1,13,14, 0,11, 3, 8),
            (  9,14,15, 5, 2, 8,12, 3, 7, 0, 4,10, 1,13,11, 6),
            (  4, 3, 2,12, 9, 5,15,10,11,14, 1, 7, 6, 0, 8,13)),

            (( 4,11, 2,14,15, 0, 8,13, 3,12, 9, 7, 5,10, 6, 1),
            ( 13, 0,11, 7, 4, 9, 1,10,14, 3, 5,12, 2,15, 8, 6),
            (  1, 4,11,13,12, 3, 7,14,10,15, 6, 8, 0, 5, 9, 2),
            (  6,11,13, 8, 1, 4,10, 7, 9, 5, 0,15,14, 2, 3,12)),

            ((13, 2, 8, 4, 6,15,11, 1,10, 9, 3,14, 5, 0,12, 7),
            (  1,15,13, 8,10, 3, 7, 4,12, 5, 6,11, 0,14, 9, 2),
            (  7,11, 4, 1, 9,12,14, 2, 0, 6,10,13,15, 3, 5, 8),
            (  2, 1,14, 7, 4,10, 8,13,15,12, 9, 0, 3, 5, 6,11)));

  PC_1 : Array [1..56] of Byte =( 57,49,41,33,25,17, 9,
                                   1,58,50,42,34,26,18,
                                  10, 2,59,51,43,35,27,
                                  19,11, 3,60,52,44,36,
                                  63,55,47,39,31,23,15,
                                   7,62,54,46,38,30,22,
                                  14, 6,61,53,45,37,29,
                                  21,13, 5,28,20,12, 4);

  PC_2 : Array [1..48] of Byte =( 14,17,11,24, 1, 5,
                                   3,28,15, 6,21,10,
                                  23,19,12, 4,26, 8,
                                  16, 7,27,20,13, 2,
                                  41,52,31,37,47,55,
                                  30,40,51,45,33,48,
                                  44,49,39,56,34,53,
                                  46,42,50,36,29,32);

  ShiftTable : Array [1..16] of Byte =( 1,1,2,2,2,2,2,2,1,2,2,2,2,2,2,1);

  PI_SUBST: array[0..255] of Byte = (
    41, 46, 67, 201, 162, 216, 124, 1, 61, 54, 84, 161, 236, 240, 6,
    19, 98, 167, 5, 243, 192, 199, 115, 140, 152, 147, 43, 217, 188,
    76, 130, 202, 30, 155, 87, 60, 253, 212, 224, 22, 103, 66, 111, 24,
    138, 23, 229, 18, 190, 78, 196, 214, 218, 158, 222, 73, 160, 251,
    245, 142, 187, 47, 238, 122, 169, 104, 121, 145, 21, 178, 7, 63,
    148, 194, 16, 137, 11, 34, 95, 33, 128, 127, 93, 154, 90, 144, 50,
    39, 53, 62, 204, 231, 191, 247, 151, 3, 255, 25, 48, 179, 72, 165,
    181, 209, 215, 94, 146, 42, 172, 86, 170, 198, 79, 184, 56, 210,
    150, 164, 125, 182, 118, 252, 107, 226, 156, 116, 4, 241, 69, 157,
    112, 89, 100, 113, 135, 32, 134, 91, 207, 101, 230, 45, 168, 2, 27,
    96, 37, 173, 174, 176, 185, 246, 28, 70, 97, 105, 52, 64, 126, 15,
    85, 71, 163, 35, 221, 81, 175, 58, 195, 92, 249, 206, 186, 197,
    234, 38, 44, 83, 13, 110, 133, 40, 132, 9, 211, 223, 205, 244, 65,
    129, 77, 82, 106, 220, 55, 200, 108, 193, 171, 250, 36, 225, 123,
    8, 12, 189, 177, 74, 120, 136, 149, 139, 227, 99, 232, 109, 233,
    203, 213, 254, 59, 0, 29, 57, 242, 239, 183, 14, 102, 88, 208, 228,
    166, 119, 114, 248, 235, 117, 75, 10, 49, 68, 80, 180, 143, 237,
    31, 26, 219, 153, 141, 51, 159, 17, 131, 20
  );

const
  MD_PADDING: array[0..63] of Byte = (
    $80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
  );

  S11 = 3;
  S12 = 7;
  S13 = 11;
  S14 = 19;
  S21 = 3;
  S22 = 5;
  S23 = 9;
  S24 = 13;
  S31 = 3;
  S32 = 9;
  S33 = 11;
  S34 = 15;

implementation

end.
