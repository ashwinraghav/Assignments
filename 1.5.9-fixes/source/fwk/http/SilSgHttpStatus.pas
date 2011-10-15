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

unit SilSgHttpStatus;

interface

uses
  SilSiHttpTypes;

const
  GHttpStatusCode: array[THttpStatus] of Word =
    (
      100,
      101,
      200,
      201,
      202,
      203,
      204,
      205,
      206,
      300,
      301,
      302,
      303,
      304,
      305,
      307,
      400,
      401,
      402,
      403,
      404,
      405,
      406,
      407,
      408,
      409,
      410,
      411,
      412,
      413,
      414,
      415,
      416,
      417,
      500,
      501,
      502,
      503,
      504,
      505,
      0
    );

const
  GHttpStatusReason: array[THttpStatus] of string =
    (
      'Continue',
      'Switching Protocols',
      'OK',
      'Created',
      'Accepted',
      'Non-Authoritative Information',
      'No Content',
      'Reset Content',
      'Partial Content',
      'Multiple Choices',
      'Moved Permanently',
      'Found',
      'See Other',
      'Not Modified',
      'Use Proxy',
      'Temporary Redirect',
      'Bad Request',
      'Unauthorized',
      'Payment Required',
      'Forbidden',
      'Not Found',
      'Method Not Allowed',
      'Not Acceptable',
      'Proxy Authentication Required',
      'Request Time-out',
      'Conflict',
      'Gone',
      'Length Required',
      'Precondition Failed',
      'Request Entity Too Large',
      'Request-URI Too Large',
      'Unsupported Media Type',
      'Requested range not satisfiable',
      'Expectation Failed',
      'Internal Server Error',
      'Not Implemented',
      'Bad Gateway',
      'Service Unavailable',
      'Gateway Time-out',
      'HTTP Version not supported',
      'extension code'
    );


function HttpStatusFromCode(Code: Word): THttpStatus;

implementation

function HttpStatusFromCode(Code: Word): THttpStatus;
var
  I: THttpStatus;
begin
  Result := hsxExtension;
  for I := Low(I) to High(I) do
    if GHttpStatusCode[I] = Code then
    begin
      Result := I;
      Break;
    end;
end;

end.
 