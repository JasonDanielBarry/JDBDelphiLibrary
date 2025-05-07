unit GraphXYFrame;

interface

uses
    Winapi.Windows, Winapi.Messages,
    System.SysUtils, System.Variants, System.Classes, System.Generics.Collections, System.UITypes,
    Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,

    GeometryTypes,
    GeomBox,
    GeomPolyLineClass,
    GraphicDrawerObjectAdderClass,
    Drawer2DPaintBoxClass,
    GraphXYTypes
    ;

    type
        TCustomGraphXY = class(TFrame)
            PBGraphXY: TPaintBox;
            //events
                procedure FrameResize(Sender: TObject);
            private
                type
                    TGraphPlotMap = TOrderedDictionary< string, TGraphXYPlot >;
                var
                    graphPlotsMap : TGraphPlotMap;
                //add plots to list
                    procedure addPlotToList(const graphPlotIn : TGraphXYPlot);
                //send graph plot to geometry drawer
                    procedure sendGraphPlotToGeometryDrawer(const graphPlotIn       : TGraphXYPlot;
                                                            var geometryDrawerInOut : TGraphicDrawerObjectAdder);
                //update geometry event
                    procedure updateGeometryEvent(ASender : TObject; var AGeomDrawer : TGraphicDrawerObjectAdder);
                //set graph boundaries
                    procedure setGraphBoundaries(const xMinIn, xMaxIn, yMinIn, yMaxIn : double);
            protected
                //process windows messages
                    procedure wndProc(var messageInOut : TMessage); override;
            public
                //constructor
                    constructor create(AOwner : TComponent); override;
                //destructor
                    destructor destroy(); override;
                //replot graphs
                    procedure replot();
                //add plots
                    //line plot
                        procedure addLinePlot(  const lineSizeIn    : integer;
                                                const plotNameIn    : string;
                                                const lineColourIn  : TColor;
                                                const lineStyle     : TPenStyle;
                                                const dataPointsIn  : TArray<TGeomPoint> );
        end;

implementation

{$R *.dfm}

    //events
        procedure TCustomGraphXY.FrameResize(Sender: TObject);
            begin
                replot();
            end;

    //private
        //add plots to list
            procedure TCustomGraphXY.addPlotToList(const graphPlotIn : TGraphXYPlot);
                begin
                    graphPlotsMap.AddOrSetValue( graphPlotIn.plotName, graphPlotIn );

                    PBGraphXY.updateGeometry( self );
                end;

        //send graph plot to geometry drawer
            procedure TCustomGraphXY.sendGraphPlotToGeometryDrawer( const graphPlotIn       : TGraphXYPlot;
                                                                    var geometryDrawerInOut : TGraphicDrawerObjectAdder );
                begin
                    case ( graphPlotIn.graphPlotType ) of
                        EGraphPlotType.gpLine:
                            begin
                                var tempPolyline := TGeomPolyLine.create( graphPlotIn.arrDataPoints );

                                geometryDrawerInOut.setCurrentDrawingLayer( graphPlotIn.plotName );

                                geometryDrawerInOut.addPolyline(
                                                                    tempPolyline,
                                                                    graphPlotIn.plottingSize,
                                                                    graphPlotIn.plotColour,
                                                                    graphPlotIn.lineStyle
                                                                );

                                FreeAndNil( tempPolyline );
                            end;

                        EGraphPlotType.gpScatter:
                            ;

                        EGraphPlotType.gpFuntion:
                            ;
                    end;
                end;

        //update geometry event
            procedure TCustomGraphXY.updateGeometryEvent(ASender : TObject; var AGeomDrawer : TGraphicDrawerObjectAdder);
                var
                    tempGraphPlotItem : TPair<string, TGraphXYPlot>;
                begin
                    //grid - must be done first

                    //graph plots
                        for tempGraphPlotItem in graphPlotsMap do
                            sendGraphPlotToGeometryDrawer( tempGraphPlotItem.Value, AGeomDrawer );
                end;

        //set graph boundaries
            procedure TCustomGraphXY.setGraphBoundaries(const xMinIn, xMaxIn, yMinIn, yMaxIn : double);
                var
                    newRegion : TGeomBox;
                begin
                    newRegion.setBounds( xMinIn, xMaxIn, yMinIn, yMaxIn, 0, 0 );

                    PBGraphXY.GraphicDrawer.setDrawingRegion(0, newRegion );

                    replot();
                end;

    //protected
        //process windows messages
            procedure TCustomGraphXY.wndProc(var messageInOut : TMessage);
                var
                    graphWasRedrawn                 : boolean;
                    currentMousePositionOnPaintbox  : TPoint;
                begin
                    if ( Assigned( PBGraphXY ) ) then
                        begin
                            //drawing messages
                                PBGraphXY.processWindowsMessages( messageInOut, graphWasRedrawn );

                            //more messages
                                //
                        end;

                    inherited wndProc(messageInOut);
                end;

    //public
        //constructor
            constructor TCustomGraphXY.create(AOwner : TComponent);
                begin
                    inherited Create( AOwner );

                    graphPlotsMap := TGraphPlotMap.Create();

                    PBGraphXY.GraphicDrawer.setDrawingSpaceRatioEnabled( False );
                    PBGraphXY.GraphicDrawer.setGeometryBorderPercentage( 0 );

                    PBGraphXY.setGridVisible( True );
                    PBGraphXY.setOnGraphicUpdateGeometryEvent( updateGeometryEvent );
                end;

        //destructor
            destructor TCustomGraphXY.destroy();
                begin
                    FreeAndNil( graphPlotsMap );

                    inherited destroy();
                end;

        //replot graphs
            procedure TCustomGraphXY.replot();
                begin
                    PBGraphXY.postRedrawGraphicMessage( self );
                end;

        //add plots
            procedure TCustomGraphXY.addLinePlot(   const lineSizeIn    : integer;
                                                    const plotNameIn    : string;
                                                    const lineColourIn  : TColor;
                                                    const lineStyle     : TPenStyle;
                                                    const dataPointsIn  : TArray<TGeomPoint> );
                var
                    newGraphPlot : TGraphXYPlot;
                begin
                    newGraphPlot.plottingSize   := lineSizeIn;
                    newGraphPlot.plotName       := plotNameIn;
                    newGraphPlot.graphPlotType  := EGraphPlotType.gpLine;
                    newGraphPlot.plotColour     := lineColourIn;
                    newGraphPlot.lineStyle      := lineStyle;
                    TGeomPoint.copyPoints( dataPointsIn, newGraphPlot.arrDataPoints );

                    addPlotToList( newGraphPlot );
                end;

end.
