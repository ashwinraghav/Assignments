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

unit SilSgFirebirdSchemaQueries;

{$INCLUDE Defines.inc}

interface

uses
  Sil,
  SilScFirebirdSQL;

const
  CSysPrefix = 'RDB$';

const
  CSysTableDomains     = CSysPrefix + 'FIELDS';
    CSysFieldDomainName                   = CSysPrefix + 'FIELD_NAME';
    CSysFieldDomainQueryName              = CSysPrefix + 'QUERY_NAME';
    CSysFieldDomainValidationBlr          = CSysPrefix + 'VALIDATION_BLR';
    CSysFieldDomainValidationSource       = CSysPrefix + 'VALIDATION_SOURCE';
    CSysFieldDomainComputedBlr            = CSysPrefix + 'COMPUTED_BLR';
    CSysFieldDomainComputedSource         = CSysPrefix + 'COMPUTED_SOURCE';
    CSysFieldDomainDefaultValue           = CSysPrefix + 'DEFAULT_VALUE';
    CSysFieldDomainDefaultSource          = CSysPrefix + 'DEFAULT_SOURCE';
    CSysFieldDomainLength                 = CSysPrefix + 'FIELD_LENGTH';
    CSysFieldDomainScale                  = CSysPrefix + 'FIELD_SCALE';
    CSysFieldDomainType                   = CSysPrefix + 'FIELD_TYPE';
    CSysFieldDomainSubtype                = CSysPrefix + 'FIELD_SUB_TYPE';
    CSysFieldDomainMissingValue           = CSysPrefix + 'MISSING_VALUE';
    CSysFieldDomainMissingSource          = CSysPrefix + 'MISSING_SOURCE';
    CSysFieldDomainDescription            = CSysPrefix + 'DESCRIPTION';
    CSysFieldDomainSystemFlag             = CSysPrefix + 'SYSTEM_FLAG';
    CSysFieldDomainQueryHeader            = CSysPrefix + 'QUERY_HEADER';
    CSysFieldDomainSegmentLength          = CSysPrefix + 'SEGMENT_LENGTH';
    CSysFieldDomainEditString             = CSysPrefix + 'EDIT_STRING';
    CSysFieldDomainExternalLength         = CSysPrefix + 'EXTERNAL_LENGTH';
    CSysFieldDomainExternalScale          = CSysPrefix + 'EXTERNAL_SCALE';
    CSysFieldDomainExternalType           = CSysPrefix + 'EXTERNAL_TYPE';
    CSysFieldDomainDimensions             = CSysPrefix + 'DIMENSIONS';
    CSysFieldDomainNullFlag               = CSysPrefix + 'NULL_FLAG';
    CSysFieldDomainCharacterLength        = CSysPrefix + 'CHARACTER_LENGTH';
    CSysFieldDomainCollationId            = CSysPrefix + 'COLLATION_ID';
    CSysFieldDomainCharsetId              = CSysPrefix + 'CHARACTER_SET_ID';
    CSysFieldDomainPrecision              = CSysPrefix + 'FIELD_PRECISION';

const
  CSysTableRelations      = CSysPrefix + 'RELATIONS';
    CSysFieldRelationViewBlr              = CSysPrefix + 'VIEW_BLR';
    CSysFieldRelationViewSource           = CSysPrefix + 'VIEW_SOURCE';
    CSysFieldRelationDescription          = CSysPrefix + 'DESCRIPTION';
    CSysFieldRelationId                   = CSysPrefix + 'RELATION_ID';
    CSysFieldRelationSystemFlag           = CSysPrefix + 'SYSTEM_FLAG';
    CSysFieldRelationDbkeyLength          = CSysPrefix + 'DBKEY_LENGTH';
    CSysFieldRelationFormat               = CSysPrefix + 'FORMAT';
    CSysFieldRelationFieldCount           = CSysPrefix + 'FIELD_ID';
    CSysFieldRelationName                 = CSysPrefix + 'RELATION_NAME';
    CSysFieldRelationSecurityClass        = CSysPrefix + 'SECURITY_CLASS';
    CSysFieldRelationExternalFile         = CSysPrefix + 'EXTERNAL_FILE';
    CSysFieldRelationRuntime              = CSysPrefix + 'RUNTIME';
    CSysFieldRelationExternalDescription  = CSysPrefix + 'EXTERNAL_DESCRIPTION';
    CSysFieldRelationOwnerName            = CSysPrefix + 'OWNER_NAME';
    CSysFieldRelationDefaultClass         = CSysPrefix + 'DEFAULT_CLASS';
    CSysFieldRelationFlags                = CSysPrefix + 'FLAGS';
  
const
  CSysTableProcedures  = CSysPrefix + 'PROCEDURES';
    CSysFieldProcedureName                = CSysPrefix + 'PROCEDURE_NAME';
    CSysFieldProcedureId                  = CSysPrefix + 'PROCEDURE_ID';
    CSysFieldProcedureInputs              = CSysPrefix + 'PROCEDURE_INPUTS';
    CSysFieldProcedureOutputs             = CSysPrefix + 'PROCEDURE_OUTPUTS';
    CSysFieldProcedureDescription         = CSysPrefix + 'DESCRIPTION';
    CSysFieldProcedureSource              = CSysPrefix + 'PROCEDURE_SOURCE';
    CSysFieldProcedureBlr                 = CSysPrefix + 'PROCEDURE_BLR';
    CSysFieldProcedureSecurityClass       = CSysPrefix + 'SECURITY_CLASS';
    CSysFieldProcedureOwnerName           = CSysPrefix + 'OWNER_NAME';
    CSysFieldProcedureRuntime             = CSysPrefix + 'RUNTIME';
    CSysFieldProcedureSystemFlag          = CSysPrefix + 'SYSTEM_FLAG';

const
  CSysTableGenerators       = CSysPrefix + 'GENERATORS';
    CSysFieldGeneratorName                = CSysPrefix + 'GENERATOR_NAME';
    CSysFieldGeneratorId                  = CSysPrefix + 'GENERATOR_ID';
    CSysFieldGeneratorSystemFlag          = CSysPrefix + 'SYSTEM_FLAG';


const
  CSysTableCollations       = CSysPrefix + 'COLLATIONS';
    CSysFieldCollationName                = CSysPrefix + 'COLLATION_NAME';
    CSysFieldCollationId                  = CSysPrefix + 'COLLATION_ID';
    CSysFieldCollationCharsetId           = CSysPrefix + 'CHARACTER_SET_ID';
    CSysFieldCollationAttributes          = CSysPrefix + 'COLLATION_ATTRIBUTES';
    CSysFieldCollationSystemFlag          = CSysPrefix + 'SYSTEM_FLAG';
    CSysFieldCollationDescription         = CSysPrefix + 'DESCRIPTION';
    CSysFieldCollationFunctionName        = CSysPrefix + 'FUNCTION_NAME';


const
  CSysTableCharsets         = CSysPrefix + 'CHARACTER_SETS';
    CSysFieldCharsetName                  = CSysPrefix + 'CHARACTER_SET_NAME';
    CSysFieldCharsetUsage                 = CSysPrefix + 'FORM_OF_USE';
    CSysFieldCharsetCharacterCount        = CSysPrefix + 'NUMBER_OF_CHARACTERS';
    CSysFieldCharsetDefaultCollation      = CSysPrefix + 'DEFAULT_COLLATE_NAME';
    CSysFieldCharsetId                    = CSysPrefix + 'CHARACTER_SET_ID';
    CSysFieldCharsetSystemFlag            = CSysPrefix + 'SYSTEM_FLAG';
    CSysFieldCharsetDescription           = CSysPrefix + 'DESCRIPTION';
    CSysFieldCharsetFunctionName          = CSysPrefix + 'FUNCTION_NAME';
    CSysFieldCharsetBytesPerCharacter     = CSysPrefix + 'BYTES_PER_CHARACTER';
    
const
  CSysTableProcedureParams  = CSysPrefix + 'PROCEDURE_PARAMETERS';
    CSysFieldProcParamName                = CSysPrefix + 'PARAMETER_NAME';
    CSysFieldProcParamProcedure           = CSysPrefix + 'PROCEDURE_NAME';
    CSysFieldProcParamNumber              = CSysPrefix + 'PARAMETER_NUMBER';
    CSysFieldProcParamKind                = CSysPrefix + 'PARAMETER_TYPE';
    CSysFieldProcParamFieldSource         = CSysPrefix + 'FIELD_SOURCE';
    CSysFieldProcParamDescription         = CSysPrefix + 'DESCRIPTION';
    CSysFieldProcParamSystemFlag          = CSysPrefix + 'SYSTEM_FLAG';

const
  GQueryProcedures: PChar = sLineBreak +
      CSqlSelect + sLineBreak +
              CSysFieldProcedureName +          CSqlComma + sLineBreak +
              CSysFieldProcedureId +            CSqlComma + sLineBreak +
              CSysFieldProcedureInputs +        CSqlComma + sLineBreak +
              CSysFieldProcedureOutputs +       CSqlComma + sLineBreak +
              CSysFieldProcedureDescription +   CSqlComma + sLineBreak +
              CSysFieldProcedureSource +        CSqlComma + sLineBreak +
              CSysFieldProcedureBlr +           CSqlComma + sLineBreak +
              CSysFieldProcedureSecurityClass + CSqlComma + sLineBreak +
              CSysFieldProcedureOwnerName +     CSqlComma + sLineBreak +
              CSysFieldProcedureRuntime +       CSqlComma + sLineBreak +
              CSysFieldProcedureSystemFlag +                sLineBreak +
          CSqlFrom + sLineBreak +
              CSysTableProcedures;

const
  CSysParamProcParamProcedure = CSysFieldProcParamProcedure;
  
const
  GQueryProcParams: PChar = sLineBreak +
      CSqlSelect + sLineBreak +
              CSysFieldProcParamName            + CSqlComma + sLineBreak +
              CSysFieldProcParamProcedure       + CSqlComma + sLineBreak +
              CSysFieldProcParamNumber          + CSqlComma + sLineBreak +
              CSysFieldProcParamKind            + CSqlComma + sLineBreak +
              CSysFieldProcParamFieldSource     + CSqlComma + sLineBreak +
              CSysFieldProcParamDescription     + CSqlComma + sLineBreak +
              CSysFieldProcParamSystemFlag                  + sLineBreak +
          CSqlFrom                                          + sLineBreak +
              CSysTableProcedureParams;

const
  GQueryRelations: PChar = sLineBreak +
      CSqlSelect + sLineBreak +
              CSysFieldRelationViewBlr            + CSqlComma + sLineBreak +
              CSysFieldRelationViewSource         + CSqlComma + sLineBreak +
              CSysFieldRelationDescription        + CSqlComma + sLineBreak +
              CSysFieldRelationId                 + CSqlComma + sLineBreak +
              CSysFieldRelationSystemFlag         + CSqlComma + sLineBreak +
              CSysFieldRelationDbkeyLength        + CSqlComma + sLineBreak +
              CSysFieldRelationFormat             + CSqlComma + sLineBreak +                         
              CSysFieldRelationFieldCount         + CSqlComma + sLineBreak +
              CSysFieldRelationName               + CSqlComma + sLineBreak +
              CSysFieldRelationSecurityClass      + CSqlComma + sLineBreak +
              CSysFieldRelationExternalFile       + CSqlComma + sLineBreak +
              CSysFieldRelationRuntime            + CSqlComma + sLineBreak +
              CSysFieldRelationExternalDescription+ CSqlComma + sLineBreak +
              CSysFieldRelationOwnerName          + CSqlComma + sLineBreak +
              CSysFieldRelationDefaultClass       + CSqlComma + sLineBreak +
              CSysFieldRelationFlags                          + sLineBreak +
          CSqlFrom                                            + sLineBreak +
              CSysTableRelations;

const
  GQueryDomains: PChar = sLineBreak +
      CSqlSelect + sLineBreak +
              CSysFieldDomainName                 + CSqlComma + sLineBreak +
              CSysFieldDomainQueryName            + CSqlComma + sLineBreak +
              CSysFieldDomainValidationBlr        + CSqlComma + sLineBreak +
              CSysFieldDomainValidationSource     + CSqlComma + sLineBreak +
              CSysFieldDomainComputedBlr          + CSqlComma + sLineBreak +
              CSysFieldDomainComputedSource       + CSqlComma + sLineBreak +
              CSysFieldDomainDefaultValue         + CSqlComma + sLineBreak +
              CSysFieldDomainDefaultSource        + CSqlComma + sLineBreak +
              CSysFieldDomainLength               + CSqlComma + sLineBreak +
              CSysFieldDomainScale                + CSqlComma + sLineBreak +
              CSysFieldDomainType                 + CSqlComma + sLineBreak +
              CSysFieldDomainSubtype              + CSqlComma + sLineBreak +
              CSysFieldDomainMissingValue         + CSqlComma + sLineBreak +
              CSysFieldDomainMissingSource        + CSqlComma + sLineBreak +
              CSysFieldDomainDescription          + CSqlComma + sLineBreak +
              CSysFieldDomainSystemFlag           + CSqlComma + sLineBreak +
              CSysFieldDomainQueryHeader          + CSqlComma + sLineBreak +
              CSysFieldDomainSegmentLength        + CSqlComma + sLineBreak +
              CSysFieldDomainEditString           + CSqlComma + sLineBreak +
              CSysFieldDomainExternalLength       + CSqlComma + sLineBreak +
              CSysFieldDomainExternalScale        + CSqlComma + sLineBreak +
              CSysFieldDomainExternalType         + CSqlComma + sLineBreak +
              CSysFieldDomainDimensions           + CSqlComma + sLineBreak +
              CSysFieldDomainNullFlag             + CSqlComma + sLineBreak +
              CSysFieldDomainCharacterLength      + CSqlComma + sLineBreak +
              CSysFieldDomainCollationId          + CSqlComma + sLineBreak +
              CSysFieldDomainCharsetId            + CSqlComma + sLineBreak +
              CSysFieldDomainPrecision                        + sLineBreak +
          CSqlFrom                                            + sLineBreak +
              CSysTableDomains;

implementation
end.
