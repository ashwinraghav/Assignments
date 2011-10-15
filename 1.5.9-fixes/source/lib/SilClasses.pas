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

unit SilClasses;

{$I Defines.inc}

interface

uses
  SilLdContainer,

  SilLkAbstractEnumerator,
  SilLkAbstractLocker,
  SilLkAbstractProcedure,
  SilLmLockedSection,
  SilLkLockers,
  SilLkNestedProcedure,
  SilLkNestedAction,
  SilLkInterfaced,

  SilLkList,

  SilLmListEnumerator,
  SilLmInterfaceList,
  SilLmMemoryStream,
  SilLmRandomList,
  SilLmPointerList,
  SilLmRandomPointerList,
  SilLmStringCompare,
  SilLmStringList,
  SilLmStreamBuffer,
  SilLmParameters,
  SilLmValueList,
  SilLmFieldAccess,
  SilLmTrace,
  SilLmEnumeration,
  SilLmEventList,
  SilLmReader,
  SilLmWriter,
  SilLmScheduler,
  SilLmReadWriteLock,
  SilLmInterfaceQueue,

  SilLmLockableExtension,
  SilLmConnectableExtension,

  SilOkFile,
  SilOkFileInfo,
  SilOkDirectoryReader;

type
  TAbstractEnumerator                         = SilLkAbstractEnumerator.TAbstractEnumerator;

type
  TListEnumerator                             = SilLmListEnumerator.TListEnumerator;

type
  TAbstractLocker                             = SilLkAbstractLocker.TAbstractLocker;
  TExtendedLocker                             = SilLkAbstractLocker.TExtendedLocker;

type
  TAbstractProcedure                          = SilLkAbstractProcedure.TAbstractProcedure;

type
  TSilLockedSection                           = SilLmLockedSection.TSilLockedSection;
  TLockedSection                              = SilLmLockedSection.TLockedSection;

type
  TCountedLocker                              = SilLkLockers.TCountedLocker;

type
  TNestedProcedure                            = SilLkNestedProcedure.TNestedProcedure;

type
  TNestedAction                               = SilLkNestedAction.TNestedAction;

type
  TValueList                                  = SilLmValueList.TValueList;

type
  TFieldAccess                                = SilLmFieldAccess.TFieldAccess;
  TStringFieldAccess                          = SilLmFieldAccess.TStringFieldAccess;
  TWideStringFieldAccess                      = SilLmFieldAccess.TWideStringFieldAccess;
  TIntegerFieldAccess                         = SilLmFieldAccess.TIntegerFieldAccess;
  TSmallIntFieldAccess                        = SilLmFieldAccess.TSmallIntFieldAccess;
  TLargeIntFieldAccess                        = SilLmFieldAccess.TLargeIntFieldAccess;
  TLongWordFieldAccess                        = SilLmFieldAccess.TLongWordFieldAccess;
  TWordFieldAccess                            = SilLmFieldAccess.TWordFieldAccess;
  TBooleanFieldAccess                         = SilLmFieldAccess.TBooleanFieldAccess;
  TFloatFieldAccess                           = SilLmFieldAccess.TFloatFieldAccess;
  TDateTimeFieldAccess                        = SilLmFieldAccess.TDateTimeFieldAccess;
  TGuidFieldAccess                            = SilLmFieldAccess.TGuidFieldAccess;
  TInterfaceFieldAccess                       = SilLmFieldAccess.TInterfaceFieldAccess;
  TCharFieldAccess                            = SilLmFieldAccess.TCharFieldAccess;
  TByteFieldAccess                            = SilLmFieldAccess.TByteFieldAccess;
  TMemoFieldAccess                            = SilLmFieldAccess.TMemoFieldAccess;
  TVariantFieldAccess                         = SilLmFieldAccess.TVariantFieldAccess;

type
  TMemoryStream                               = SilLmMemoryStream.TMemoryStream;

type
  TSilList                                    = SilLkList.TSilList;

type
  TInterfaceList                              = SilLmInterfaceList.TInterfaceList;
  TSilInterfaceList                           = SilLmInterfaceList.TSilInterfaceList;

type
  TCustomRandomList                           = SilLmRandomList.TCustomRandomList;
  TRandomList                                 = SilLmRandomList.TRandomList;

type
  TSilPointerList                             = SilLmPointerList.TSilPointerList;
  TRandomPointerList                          = SilLmRandomPointerList.TRandomPointerList;

type
  TPointerList                                = class(SilLmPointerList.TPointerList) end {$IFDEF USE_DEPRECATED} deprecated {$ENDIF};

type
  TStreamBuffer                               = SilLmStreamBuffer.TStreamBuffer;

type
  TSilParameters                              = SilLmParameters.TSilParameters;

type
  TSilStringList                              = SilLmStringList.TSilStringList;

type
  TStringCompare                              = SilLmStringCompare.TStringCompare;

type
  TTrace                                      = SilLmTrace.TTrace;

type
  TEnumeration                                = SilLmEnumeration.TEnumeration;

type
  TEventList                                  = SilLmEventList.TEventList;

type
  TStreamReader                               = SilLmReader.TStreamReader;

type
  TStreamWriter                               = SilLmWriter.TStreamWriter;

type
  TScheduling                                 = SilLmScheduler.TScheduling;

type
  TReadWriteLock                              = SilLmReadWriteLock.TReadWriteLock;

type
  TInterfaceQueue                             = SilLmInterfaceQueue.TInterfaceQueue;

type
  TSilLockableExtension                       = SilLmLockableExtension.TSilLockableExtension;
  TSilConnectableExtension                    = SilLmConnectableExtension.TSilConnectableExtension;

type
  TSilFile                                    = SilOkFile.TSilFile;

type
  TSilBaseFileInfo                            = SilOkFileInfo.TSilBaseFileInfo;
  TSilFileInfo                                = SilOkFileInfo.TSilFileInfo;
  TSilFileInfoCompare                         = SilOkFileInfo.TSilFileInfoCompare;
  TSilFileInfoList                            = SilOkFileInfo.TSilFileInfoList;

type
  TSilBaseDirectoryReader                     = SilOkDirectoryReader.TSilBaseDirectoryReader;
  TSilDirectoryReader                         = SilOkDirectoryReader.TSilDirectoryReader;

implementation
end.
