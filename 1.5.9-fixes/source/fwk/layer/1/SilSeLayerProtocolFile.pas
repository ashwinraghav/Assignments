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

unit SilSeLayerProtocolFile;

{$include Defines.inc}

interface

const
  PS_BASE = $400;

  CM_OPENFILE                 = Succ(PS_BASE);
  CM_OPENFILE_REPLY           = Succ(CM_OPENFILE);
  CM_CREATEFILE               = Succ(CM_OPENFILE_REPLY);
  CM_CREATEFILE_REPLY         = Succ(CM_CREATEFILE);
  CM_READFILE                 = Succ(CM_CREATEFILE_REPLY);
  CM_READFILE_REPLY           = Succ(CM_READFILE);
  CM_WRITEFILE                = Succ(CM_READFILE_REPLY);
  CM_WRITEFILE_REPLY          = Succ(CM_WRITEFILE);
  CM_SEEKFILE                 = Succ(CM_WRITEFILE_REPLY);
  CM_SEEKFILE_REPLY           = Succ(CM_SEEKFILE);
  CM_FLUSHFILE                = Succ(CM_SEEKFILE_REPLY);
  CM_FLUSHFILE_REPLY          = Succ(CM_FLUSHFILE);
  CM_CLOSEFILE                = Succ(CM_FLUSHFILE_REPLY);
  CM_CLOSEFILE_REPLY          = Succ(CM_CLOSEFILE);
  CM_CREATEDIR                = Succ(CM_CLOSEFILE_REPLY);
  CM_CREATEDIR_REPLY          = Succ(CM_CREATEDIR);
  CM_MOVE                     = Succ(CM_CREATEDIR_REPLY);
  CM_MOVE_REPLY               = Succ(CM_MOVE);
  CM_DELETE                   = Succ(CM_MOVE_REPLY);
  CM_DELETE_REPLY             = Succ(CM_DELETE);
  CM_CREATEDIRREADER          = Succ(CM_DELETE_REPLY);
  CM_CREATEDIRREADER_REPLY    = Succ(CM_CREATEDIRREADER);
  CM_DIRREAD                  = Succ(CM_CREATEDIRREADER_REPLY);
  CM_DIRREAD_REPLY            = Succ(CM_DIRREAD);
  CM_DESTROYDIRREADER         = Succ(CM_DIRREAD_REPLY);
  CM_DESTROYDIRREADER_REPLY   = Succ(CM_DESTROYDIRREADER);
  CM_GETINFO                  = Succ(CM_DESTROYDIRREADER_REPLY);
  CM_GETINFO_REPLY            = Succ(CM_GETINFO);
  CM_GETFILESIZE              = Succ(CM_GETINFO_REPLY);
  CM_GETFILESIZE_REPLY        = Succ(CM_GETFILESIZE);
  CM_SETFILESIZE              = Succ(CM_GETFILESIZE_REPLY);
  CM_SETFILESIZE_REPLY        = Succ(CM_SETFILESIZE);
  CM_SETFILEATTR              = Succ(CM_SETFILESIZE_REPLY);
  CM_SETFILEATTR_REPLY        = Succ(CM_SETFILEATTR);
  CM_SETFILETIME              = Succ(CM_SETFILEATTR_REPLY);
  CM_SETFILETIME_REPLY        = Succ(CM_SETFILETIME);

implementation

end.
 