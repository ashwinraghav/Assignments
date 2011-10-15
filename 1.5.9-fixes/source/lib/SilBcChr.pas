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

unit SilBcChr;

{$I Defines.inc}

interface

const
  ccNUL  = #0;
  ccSOH  = #1;
  ccSTX  = #2;
  ccETX  = #3;
  ccEOT  = #4;
  ccENQ  = #5;
  ccACK  = #6;
  ccBEL  = #7;
  ccBS   = #8;
  ccHT   = #9;
  ccLF   = #10;
  ccVT   = #11;
  ccFF   = #12;
  ccCR   = #13;
  ccSO   = #14;
  ccSI   = #15;
  ccDLE  = #16;
  ccDC1  = #17;
  ccDC2  = #18;
  ccDC3  = #19;
  ccDC4  = #20;
  ccNAK  = #21;
  ccSYN  = #22;
  ccETB  = #23;
  ccCAN  = #24;
  ccEM   = #25;
  ccSUB  = #26;
  ccESC  = #27;
  ccFS   = #28;
  ccGS   = #29;
  ccRS   = #30;
  ccUS   = #31;
  ccSPC  = #32;
  ccCRLF = ccCR + ccLF;

const
  AsciiChars: array[0..32] of String[3] =
    ( 'NUL',  // 0
      'SOH',  // 1
      'STX',  // 2
      'ETX',  // 3
      'EOT',  // 4
      'ENQ',  // 5
      'ACK',  // 6
      'BEL',  // 7
      'BS',   // 8
      'HT',   // 9
      'LF',   // 10
      'VT',   // 11
      'FF',   // 12
      'CR',   // 13
      'SO',   // 14
      'SI',   // 15
      'DLE',  // 16
      'DC1',  // 17
      'DC2',  // 18
      'DC3',  // 19
      'DC4',  // 20
      'NAK',  // 21
      'SYN',  // 22
      'ETB',  // 23
      'CAN',  // 24
      'EM',   // 25
      'SUB',  // 26
      'ESC',  // 27
      'FS',   // 28
      'GS',   // 29
      'RS',   // 30
      'US',   // 31
      'SPC'); // 32
      
implementation
end.
 