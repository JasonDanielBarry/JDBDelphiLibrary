unit GraphXYTest;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Math, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,

  GeometryTypes,
  CustomComponentPanelClass, GraphXYComponent;

type
  TForm2 = class(TForm)
    JDBGraphXY1: TJDBGraphXY;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.FormCreate(Sender: TObject);
const
    POINT_COUNT : integer = 1000;
var
    i           : integer;
    x, y        : double;
    arrPoints   : TArray<TGeomPoint>;
begin
    SetLength( arrPoints, POINT_COUNT );

    for i := 0 to POINT_COUNT-1 do
        begin
            x := 0.1 * i;
            y := 0.1 * power( x, 2 ) * sin( 0.5 * x );

            arrPoints[i].setPoint( x, y );
        end;

    JDBGraphXY1.addLinePlot( 'Series 1', arrPoints, 3 );

    for i := 0 to POINT_COUNT-1 do
        begin
            x := 0.1 * i;
            y := 0.1 * power( x, 2 );

            arrPoints[i].setPoint( x, y );
        end;

    JDBGraphXY1.addLinePlot( 'Series 1', arrPoints, 4, clBlue, TPenStyle.psDashDotDot );

    for i := 0 to POINT_COUNT-1 do
        begin
            x := 0.1 * i;
            y := -0.1 * power( x, 2 );

            arrPoints[i].setPoint( x, y );
        end;

    JDBGraphXY1.addLinePlot( 'Series 1', arrPoints, 5, clRed, TPenStyle.psDash );
end;


end.
