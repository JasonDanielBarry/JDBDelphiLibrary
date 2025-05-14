unit GraphXYComponent;

interface

    uses
        System.SysUtils, System.Classes, System.UITypes,
        Vcl.Graphics,
        Vcl.Controls,

        GeometryTypes,
        CustomComponentPanelClass,
        GraphXYFrame;

    type
        TJDBGraphXY = class(TCustomComponentPanel)
            private
                customGraphXY : TCustomGraphXY;
            public
                constructor Create(AOwner: TComponent); override;
                destructor Destroy(); override;
                procedure replot();
                //add plots
                    //line plot
                        procedure addLinePlot(  const plotNameIn    : string;
                                                const dataPointsIn  : TArray<TGeomPoint>;
                                                const lineSizeIn    : integer = 2;
                                                const lineColourIn  : TColor = clWindowText;
                                                const lineStyleIn   : TPenStyle = TPenStyle.psSolid );
                    //scatter plot
                        procedure addScatterPlot(   const plotNameIn    : string;
                                                    const dataPointsIn  : TArray<TGeomPoint>;
                                                    const pointSizeIn   : integer = 5;
                                                    const pointColourIn : TColor = TColors.Royalblue    );
        end;

implementation

    //public
        constructor TJDBGraphXY.Create(AOwner: TComponent);
            begin
                inherited create( AOwner );

                customGraphXY := TCustomGraphXY.create(Self);
                customGraphXY.parent    := self;
                customGraphXY.Align     := TAlign.alClient;
                customGraphXY.Visible   := True;
            end;

        destructor TJDBGraphXY.Destroy();
            begin
                FreeAndNil( customGraphXY );

                inherited Destroy();
            end;

        procedure TJDBGraphXY.replot();
            begin
                customGraphXY.replot();
            end;

        //add plots
            //line plot
                procedure TJDBGraphXY.addLinePlot(  const plotNameIn    : string;
                                                    const dataPointsIn  : TArray<TGeomPoint>;
                                                    const lineSizeIn    : integer = 2;
                                                    const lineColourIn  : TColor = clWindowText;
                                                    const lineStyleIn     : TPenStyle = TPenStyle.psSolid );
                    begin
                        customGraphXY.addLinePlot( lineSizeIn, plotNameIn, lineColourIn, lineStyleIn, dataPointsIn );
                    end;

            //scatter plot
                procedure TJDBGraphXY.addScatterPlot(   const plotNameIn    : string;
                                                        const dataPointsIn  : TArray<TGeomPoint>;
                                                        const pointSizeIn   : integer = 5;
                                                        const pointColourIn : TColor = TColors.Royalblue    );
                    begin
                        customGraphXY.addScatterPlot( pointSizeIn, plotNameIn, pointColourIn, dataPointsIn );
                    end;


end.
