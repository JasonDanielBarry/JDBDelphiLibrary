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
            public
                //constructor
                    constructor create();
                //destructor
                    destructor destroy(); override;
                //add graphic object to array
                    procedure addGraphicObjectToGroup(const graphicObjectIn : TGraphicObject);
                    procedure addGraphicObjectsToGroup(const arrGraphicObjectsIn : TArray<TGraphicObject>);
                //clear the array
                    procedure clearGraphicObjectsGroup(const freeGraphicObjectsIn : boolean = True);
                //draw to canvas
                    procedure drawToCanvas( const axisConverterIn   : TDrawingAxisConverter;
                                            var canvasInOut         : TDirect2DCanvas       ); override;
                //bounding box
                    function determineBoundingBox() : TGeomBox; override;
        end;

implementation

    //protected


    //public
        //constructor
            constructor TGraphicObjectGroup.create();
                begin
                    inherited create();

                    clearGraphicObjectsGroup( False );
                end;

        //destructor
            destructor TGraphicObjectGroup.destroy();
                begin
                    clearGraphicObjectsGroup( True );

                    inherited destroy();
                end;

        //add graphic object to array
            procedure TGraphicObjectGroup.addGraphicObjectToGroup(const graphicObjectIn : TGraphicObject);
                var
                    arrLen : integer;
                begin
                    arrLen := length( arrGraphicObjectsGroup );

                    SetLength( arrGraphicObjectsGroup, arrLen + 1 );

                    arrGraphicObjectsGroup[ arrLen ] := graphicObjectIn;
                end;

            procedure TGraphicObjectGroup.addGraphicObjectsToGroup(const arrGraphicObjectsIn : TArray<TGraphicObject>);
                var
                    i : integer;
                begin
                    for i := 0 to ( length( arrGraphicObjectsIn ) - 1 ) do
                        addGraphicObjectToGroup( arrGraphicObjectsIn[i] );
                end;

        //clear the array
            procedure TGraphicObjectGroup.clearGraphicObjectsGroup(const freeGraphicObjectsIn : boolean = True);
                var
                    i, arrLen : integer;
                begin
                    if NOT( freeGraphicObjectsIn ) then
                        begin
                            SetLength( arrGraphicObjectsGroup, 0 );
                            exit();
                        end;

                    arrLen := length( arrGraphicObjectsGroup );

                    for i := 0 to ( arrLen - 1 ) do
                        FreeAndNil( arrGraphicObjectsGroup[i] );

                    SetLength( arrGraphicObjectsGroup, 0 );
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
