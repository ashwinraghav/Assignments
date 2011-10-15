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

unit SilSgFirebirdTransaction;

{$INCLUDE Defines.inc}

interface

uses
  SilScFirebirdClient,
  SilSiFirebird;

const
  GFbTransactionModel: array[TFbTransactionModel] of PChar = (
      nil,                                                          // fbtmUnespecified
      Char(isc_tpb_concurrency),                                    // fbtmConcurrency
      Char(isc_tpb_read_committed) + Char(isc_tpb_rec_version),     // fbtmCommittedNoVersion
      Char(isc_tpb_read_committed) + Char(isc_tpb_no_rec_version),  // fbtmCommittedVersion
      Char(isc_tpb_consistency)                                     // fbtmConsistency
    );

const 
  GFbTransactionAccess: array[TFbTransactionAccess] of PChar = (
      nil,                      //  fbtaUnespecified
      Char(isc_tpb_read),       //  fbtaReadOnly
      Char(isc_tpb_write)       //  fbtaReadWrite
    );

const
  GFbTransactionResolution: array[TFbTransactionResolution] of PChar = (
      nil,                      //  fbtrUnespecified
      Char(isc_tpb_wait),       //  fbtrWait,
      Char(isc_tpb_nowait)      //  fbtrNoWait
    );
  
const    
  GFbLockDirective: array[TFbLockDirective] of PChar = (
      nil,                      //  fbldUnespecified
      Char(isc_tpb_shared),     //  fbldShared
      Char(isc_tpb_protected)   //  fbldProtected
    );
    
const
  GFbLockOption: array[TFbLockOption] of PChar = (
      nil,                      //  fbloUnespecified,
      Char(isc_tpb_lock_read),  //  fbloRead,
      Char(isc_tpb_lock_write)  //  fbloWrite
    );

implementation

end.
 