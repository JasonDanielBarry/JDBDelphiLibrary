unit GraphXYTypes;

interface

    uses
        System.UITypes,
        Vcl.Graphics,
        GeometryTypes;

    type
        EGraphPlotType = (gpLine = 0, gpScatter = 1, gpFuntion = 2);

        TSingleVariableFunction = function(x : double) : double;

        TGraphXYPlot = record
            var
                plottingSize    : integer;
                plotName        : string;
                graphPlotType   : EGraphPlotType;
                plotColour      : TColor;
                lineStyle       : TPenStyle;
                arrDataPoints   : TArray<TGeomPoint>;
                plotFunction    : TSingleVariableFunction;
        end;

implementation

end.
