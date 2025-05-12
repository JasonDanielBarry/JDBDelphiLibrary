program GraphXYTestProject;

uses
  Vcl.Forms,
  GraphXYTest in 'Source\GraphXYTest.pas' {Form2},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Windows11 Modern Dark');
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
