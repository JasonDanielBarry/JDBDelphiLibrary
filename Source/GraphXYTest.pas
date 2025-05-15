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
    POINT_COUNT : integer = 2000;
var
    i           : integer;
    pi_Value,
    x, y        : double;
    arrPoints1,
    arrPoints2,
    arrPoints3,
    arrPoints4  : TArray<TGeomPoint>;
begin
    pi_Value := pi();

    SetLength( arrPoints1, POINT_COUNT + 1 );
    SetLength( arrPoints2, POINT_COUNT + 1 );
    SetLength( arrPoints3, POINT_COUNT + 1 );
    SetLength( arrPoints4, POINT_COUNT div 5 + 1 );

    for i := 0 to POINT_COUNT do
        begin
            x := 0.025 * (i - POINT_COUNT/2);

            y := 0.1 * power( x, 2 ) * cos( 0.5 * x * pi_Value );
            arrPoints1[i].setPoint( x, y );

            y := 0.1 * power( x, 2 );
            arrPoints2[i].setPoint( x, y );

            if ( (i mod 5) = 0 ) then
                begin
                    Randomize();
                    y := y + 5 * Random() - 2.5 + 10;

                    arrPoints4[i div 5].setPoint( x, y );
                end;

            y := -0.1 * power( x, 2 );
            arrPoints3[i].setPoint( x, y );
        end;

    JDBGraphXY1.addLinePlot( 'Series 1', arrPoints1, 3 );
    JDBGraphXY1.addLinePlot( 'Series 2', arrPoints2, 4, clBlue, TPenStyle.psDashDotDot );
    JDBGraphXY1.addLinePlot( 'Series 3', arrPoints3, 5, clRed, TPenStyle.psDash );
    JDBGraphXY1.addScatterPlot( 'Scatter 1', arrPoints4 );
end;


end.
