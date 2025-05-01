unit GraphicDrawerBaseClass;

interface

    uses
        //Delphi
            system.SysUtils, System.Math, system.types, system.UIConsts, system.UITypes, system.Generics.Collections,
            vcl.Graphics,
            vcl.Direct2D, Winapi.D2D1,
        //custom
            ColourMethods,
            DrawingAxisConversionClass,
            GraphicObjectBaseClass,
            GraphicGeometryClass, GraphicLineClass, GraphicPolylineClass, GraphicPolygonClass,
            GeometryTypes
            ;

    type
        TGraphicDrawerBase = class
            strict protected
                var
                    axisConverter               : TDrawingAxisConverter;
                    Direct2DDrawingCanvas       : TDirect2DCanvas;
                //add graphic drawing object to the drawing object container
                    procedure addGraphicObject(const drawingGeometryIn : TGraphicObject); virtual; abstract;
                //drawing procedures
                    //draw all geometry
                        procedure drawAll(  const canvasWidthIn, canvasHeightIn : integer;
                                            const drawingBackgroundColourIn     : TColor    );
            protected
                var
                    //these variables are declared here to be used in the drawAll procedure
                    //but are set in GraphicDrawerAxisConversionInterfaceClass
                        drawingSpaceRatioEnabled    : boolean;
                        drawingSpaceRatio           : double;
            public
                //constructor
                    constructor create(); virtual;
                //destructor
                    destructor destroy(); override;
        end;

implementation

    //protected
        //drawing procedures
            //draw all geometry
                procedure TGraphicDrawerBase.drawAll(   const canvasWidthIn, canvasHeightIn : integer;
                                                        const drawingBackgroundColourIn     : TColor    );
                    begin
                        //set axis converter canvas dimensions
                            axisConverter.setCanvasDimensions( canvasWidthIn, canvasHeightIn );

                        //set the drawing space ratio
                            if ( drawingSpaceRatioEnabled ) then
                                axisConverter.setDrawingSpaceRatio( drawingSpaceRatio );

                        //clear the canvas
                            Direct2DDrawingCanvas.Brush.Color := drawingBackgroundColourIn;

                            Direct2DDrawingCanvas.FillRect( Rect(0, 0, canvasWidthIn, canvasHeightIn) );
                    end;

    //public
        //constructor
            constructor TGraphicDrawerBase.create();
                begin
                    inherited create();

                    axisConverter := TDrawingAxisConverter.create();
                end;

        //destructor
            destructor TGraphicDrawerBase.destroy();
                begin
                    FreeAndNil( axisConverter );

                    inherited destroy();
                end;

end.
