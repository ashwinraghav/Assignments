unit UdOuch;

interface

uses
  UiOuch;

resourcestring
  SOffline                = 'offline';
  SOnline                 = 'online';
  SPassword               = 'password';

resourcestring
  SUserUnknown            = '?';

resourcestring
  SEngineUnknown          = 'n/a';
  SEngineDisconnected     = 'disconnected';
  SEngineConnected        = 'connected';
  SEngineLoggedOn         = 'logged on';

resourcestring
  SCannotEditAccount      = 'La cuenta de usuario especificada no puede ser modificada';

resourcestring
  SAskSendEmptyMessage    = 'El texto del mensaje está vacío. ¿Desea enviarlo igual?';

resourcestring
  SUpdateStarted          = 'buscando nuevas versiones ...';
  SUpdateDownloading      = 'hay nuevas versiones para bajar ...';
  SUpdateDownloaded       = 'nuevas versiones bajadas';
  SUpdateCompleted        = 'no hay nada que actualizar';
  SUpdateChanged          = 'cambios realizados con éxito';
  SUpdateError            = 'falló por error ';

resourcestring
  SUpdateStageStart       = 'actualizando ...';
  SUpdateStageDownload    = 'bajando ...';
  SUpdateStageChange      = 'cambiando ...';
  SUpdateStageEnd         = 'listo';
  SUpdateStageError       = 'errores';

resourcestring
  SUpdateQuestion         = '¿Desea cerrar la aplicación ahora mismo?';

implementation
end.
 