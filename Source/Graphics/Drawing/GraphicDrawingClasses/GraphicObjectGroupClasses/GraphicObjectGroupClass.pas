unit GraphicObjectGroupClass;

interface

    uses
        Winapi.D2D1,
        system.SysUtils, system.UITypes,
        Vcl.Direct2D,
        GeomBox,
        GraphicDrawingTypes,
        DrawingAxisConversionClass,
        GraphicObjectBaseClass
        ;

    type
        TGraphicObjectGroup = class(TGraphicObject)
            private
                arrGraphicObjectsGroup : TArray<TGraphicObject>;
            protected
                procedure addGraphicObjectToGroup(const graphicObjectIn : TGraphicObject);
            public
                //constructor
                    constructor create();
                //destructor
                    destructor destroy(); override;
                //draw to canvas
                    procedure drawToCanvas( const axisConverterIn   : TDrawingAxisConverter;
                                            var canvasInOut         : TDirect2DCanvas       ); override;
                //bounding box
                    function determineBoundingBox() : TGeomBox; override;
        end;

implementation

    //protected
        procedure TGraphicObjectGroup.addGraphicObjectToGroup(const graphicObjectIn : TGraphicObject);
            var
                arrLen : integer;
            begin
                arrLen := length( arrGraphicObjectsGroup );

                SetLength( arrGraphicObjectsGroup, arrLen + 1 );

                arrGraphicObjectsGroup[ arrLen ] := graphicObjectIn;
            end;

    //public
        //constructor
            constructor TGraphicObjectGroup.create();
                begin
                    inherited create();

                    SetLength( arrGraphicObjectsGroup, 0 );
                end;

        //destructor
            destructor TGraphicObjectGroup.destroy();
                var
                    i, arrLen : integer;
                begin
                    arrLen := length( arrGraphicObjectsGroup );

                    for i := 0 to ( arrLen - 1 ) do
                        FreeAndNil( arrGraphicObjectsGroup[i] );

                    SetLength( arrGraphicObjectsGroup, 0 );

                    inherited destroy();
                end;

        //draw to canvas
            procedure TGraphicObjectGroup.drawToCanvas( const axisConverterIn   : TDrawingAxisConverter;
                                                        var canvasInOut         : TDirect2DCanvas       );
                begin
                    TGraphicObject.drawAllToCanvas(
                                                        arrGraphicObjectsGroup,
                                                        axisConverterIn,
                                                        canvasInOut
                                                  );
                end;

        //bounding box
            function TGraphicObjectGroup.determineBoundingBox() : TGeomBox;
                begin
                    result := TGraphicObject.determineBoundingBox( arrGraphicObjectsGroup );
                end;


end.
