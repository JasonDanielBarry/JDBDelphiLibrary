unit GraphPlotsListClass;

interface

    uses
        GraphicLinePlotClass,
        GraphicScatterPlotClass,
        GraphicMousePointTrackerClass,
        GraphXYTypes,
        GraphicObjectListBaseClass;

    type
        TGraphPlotsList = class(TGraphicObjectListBase)
            public
                procedure addGraphPlot(const graphPlotIn : TGraphXYPlot);
                procedure addMousePointTracker(const mousePointTrackerIn : TGraphicMousePointTracker);
        end;

implementation

    //public
        procedure TGraphPlotsList.addGraphPlot(const graphPlotIn : TGraphXYPlot);
            begin
                case ( graphPlotIn.graphPlotType ) of
                    EGraphPlotType.gpLine:
                        begin
                            var graphicLinePlot := TGraphicLinePlot.create( graphPlotIn.plottingSize,
                                                                            graphPlotIn.plotColour,
                                                                            graphPlotIn.lineStyle,
                                                                            graphPlotIn.arrDataPoints   );

                            addGraphicObject( graphicLinePlot );
                        end;

                    EGraphPlotType.gpScatter:
                        begin
                            var graphicScatterPlot : TGraphicScatterPlot := TGraphicScatterPlot.create( graphPlotIn.plottingSize,
                                                                                                        graphPlotIn.plotColour,
                                                                                                        graphPlotIn.arrDataPoints );

                            addGraphicObject( graphicScatterPlot );
                        end;

                    EGraphPlotType.gpFuntion:
                        ;
                end;
            end;

        procedure TGraphPlotsList.addMousePointTracker(const mousePointTrackerIn : TGraphicMousePointTracker);
            begin
                addGraphicObject( mousePointTrackerIn );
            end;


end.
