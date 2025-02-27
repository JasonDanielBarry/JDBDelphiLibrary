unit XMLDocumentMethods;

interface

    uses
        system.SysUtils, system.Classes, system.Generics.Collections, system.StrUtils,
        Xml.XMLDoc, Xml.XMLIntf, Xml.xmldom,
        ArrayConversionMethods
        ;

    //read data from XML node
        //boolean
            function tryReadBooleanFromXMLNode(var XMLNodeInOut : IXMLNode; const dataIdentifierIn : string; out boolValueOut : boolean; const defaultValueIn : boolean = False) : boolean;

        //integer
            function tryReadIntegerFromXMLNode(var XMLNodeInOut : IXMLNode; const dataIdentifierIn : string; out integerValueOut : integer; const defaultValueIn : integer = 0) : boolean;

        //double
            function tryReadDoubleFromXMLNode(var XMLNodeInOut : IXMLNode; const dataIdentifierIn : string; out doubleValueOut : double; const defaultValueIn : double = 0) : boolean;

        //string
            function tryReadStringFromXMLNode(var XMLNodeInOut : IXMLNode; const dataIdentifierIn : string; out stringValueOut : string; const defaultValueIn : string = '') : boolean;

        //arrays
            //integer
                function TryReadIntegerArrayFromXMLNode(var XMLNodeInOut : IXMLNode; const dataIdentifierIn : string; out integerArrayOut : TArray<integer>) : boolean;

            //double
                function TryReadDoubleArrayFromXMLNode(var XMLNodeInOut : IXMLNode; const dataIdentifierIn : string; out doubleArrayOut : TArray<double>) : boolean;

            //string
                function TryReadStringArrayFromXMLNode(var XMLNodeInOut : IXMLNode; const dataIdentifierIn : string; out stringArrayOut : TArray<string>) : boolean;

    //write data to XML node
        //boolean
            procedure writeBooleanToXMLNode(var XMLNodeInOut : IXMLNode; const dataIdentifierIn : string; boolValueIn : boolean);

        //integer
            procedure writeIntegerToXMLNode(var XMLNodeInOut : IXMLNode; const dataIdentifierIn : string; integerValueIn : integer);

        //double
            procedure writeDoubleToXMLNode(var XMLNodeInOut : IXMLNode; const dataIdentifierIn : string; doubleValueIn : double);

        //string
            procedure writeStringToXMLNode(var XMLNodeInOut : IXMLNode; const dataIdentifierIn, stringValueIn : string);

        //arrays
            //integer
                procedure writeIntegerArrayToXMLNode(var XMLNodeInOut : IXMLNode; const dataIdentifierIn : string; integerArrayIn : TArray<integer>);

            //double
                procedure writeDoubleArrayToXMLNode(var XMLNodeInOut : IXMLNode; const dataIdentifierIn : string; doubleArrayIn : TArray<double>);

            //string
                procedure writeStringArrayToXMLNode(var XMLNodeInOut : IXMLNode; const dataIdentifierIn : string; stringArrayIn : TArray<string>);

implementation

    const
        ARRAY_ELEMENT_DELIMITER : string = ';';

    //read data from XML node
        //boolean
            function tryReadBooleanFromXMLNode(var XMLNodeInOut : IXMLNode; const dataIdentifierIn : string; out boolValueOut : boolean; const defaultValueIn : boolean = False) : boolean;
                var
                    readSuccessful, dataIsBool  : boolean;
                    readStringValue             : string;
                begin
                    readSuccessful := tryReadStringFromXMLNode( XMLNodeInOut, dataIdentifierIn, readStringValue );

                    dataIsBool := TryStrToBool( readStringValue, boolValueOut );

                    if NOT( readSuccessful AND dataIsBool ) then
                        begin
                            boolValueOut := defaultValueIn;
                            exit( False );
                        end;

                    result := True;
                end;

        //integer
            function tryReadIntegerFromXMLNode(var XMLNodeInOut : IXMLNode; const dataIdentifierIn : string; out integerValueOut : integer; const defaultValueIn : integer = 0) : boolean;
                var
                    readSuccessful, dataIsInteger   : boolean;
                    readStringValue                 : string;
                begin
                    readSuccessful := tryReadStringFromXMLNode( XMLNodeInOut, dataIdentifierIn, readStringValue );

                    dataIsInteger := TryStrToInt( readStringValue, integerValueOut );

                    if NOT( readSuccessful AND dataIsInteger ) then
                        begin
                            integerValueOut := defaultValueIn;
                            exit( False );
                        end;

                    result := True;
                end;

        //double
            function tryReadDoubleFromXMLNode(var XMLNodeInOut : IXMLNode; const dataIdentifierIn : string; out doubleValueOut : double; const defaultValueIn : double = 0) : boolean;
                var
                    readSuccessful, dataIsDouble    : boolean;
                    readStringValue                 : string;
                begin
                    readSuccessful := tryReadStringFromXMLNode( XMLNodeInOut, dataIdentifierIn, readStringValue );

                    dataIsDouble := TryStrToFloat( readStringValue, doubleValueOut );

                    if NOT( readSuccessful AND dataIsDouble ) then
                        begin
                            doubleValueOut := defaultValueIn;
                            exit( False );
                        end;

                    result := True;
                end;

        //string
            function tryReadStringFromXMLNode(var XMLNodeInOut : IXMLNode; const dataIdentifierIn : string; out stringValueOut : string; const defaultValueIn : string = '') : boolean;
                begin
                    if NOT( Assigned(XMLNodeInOut) ) then
                        begin
                            stringValueOut := defaultValueIn;
                            exit( false );
                        end;

                    stringValueOut := trim( XMLNodeInOut.ChildNodes.FindNode( dataIdentifierIn ).Text );

                    result := True;
                end;

        //arrays
            //integer
                function TryReadIntegerArrayFromXMLNode(var XMLNodeInOut : IXMLNode; const dataIdentifierIn : string; out integerArrayOut : TArray<integer>) : boolean;
                    var
                        readSuccessful, IsIntegerArray  : boolean;
                        readStringArray                 : TArray<string>;
                    begin
                        readSuccessful := TryReadStringArrayFromXMLNode( XMLNodeInOut, dataIdentifierIn, readStringArray );

                        IsIntegerArray := tryConvertStringArrayToIntArray( readStringArray, integerArrayOut );

                        if NOT( readSuccessful AND IsIntegerArray ) then
                            begin
                                SetLength( integerArrayOut, 0 );
                                exit( False );
                            end;

                        result := True;
                    end;

            //double
                function TryReadDoubleArrayFromXMLNode(var XMLNodeInOut : IXMLNode; const dataIdentifierIn : string; out doubleArrayOut : TArray<double>) : boolean;
                    var
                        readSuccessful, IsDoubleArray   : boolean;
                        readStringArray                 : TArray<string>;
                    begin
                        readSuccessful := TryReadStringArrayFromXMLNode( XMLNodeInOut, dataIdentifierIn, readStringArray );

                        IsDoubleArray := tryConvertStringArrayToDoubleArray( readStringArray, doubleArrayOut );

                        if NOT( readSuccessful AND IsDoubleArray ) then
                            begin
                                SetLength( doubleArrayOut, 0 );
                                exit( False );
                            end;

                        result := True;
                    end;

            //string
                function TryReadStringArrayFromXMLNode(var XMLNodeInOut : IXMLNode; const dataIdentifierIn : string; out stringArrayOut : TArray<string>) : boolean;
                    var
                        readSuccessful, dataIsArray : boolean;
                        readStringValue             : string;
                    begin
                        readSuccessful := tryReadStringFromXMLNode( XMLNodeInOut, dataIdentifierIn, readStringValue );

                        dataIsArray := Pos( ARRAY_ELEMENT_DELIMITER, readStringValue ) > 1;

                        if NOT( readSuccessful AND dataIsArray ) then
                            begin
                                SetLength( stringArrayOut, 0 );
                                exit( False );
                            end;

                        stringArrayOut := SplitString( readStringValue, ARRAY_ELEMENT_DELIMITER );

                        result := True;
                    end;

    //write data to XML node
        //boolean
            procedure writeBooleanToXMLNode(var XMLNodeInOut : IXMLNode; const dataIdentifierIn : string; boolValueIn : boolean);
                var
                    boolStr : string;
                begin
                    boolStr := BoolToStr( boolValueIn, True );

                    writeStringToXMLNode( XMLNodeInOut, dataIdentifierIn, boolStr );
                end;

        //integer
            procedure writeIntegerToXMLNode(var XMLNodeInOut : IXMLNode; const dataIdentifierIn : string; integerValueIn : integer);
                var
                    intStr : string;
                begin
                    intStr := IntToStr( integerValueIn );

                    writeStringToXMLNode( XMLNodeInOut, dataIdentifierIn, intStr )
                end;

        //double
            procedure writeDoubleToXMLNode(var XMLNodeInOut : IXMLNode; const dataIdentifierIn : string; doubleValueIn : double);
                var
                    doubleStr : string;
                begin
                    doubleStr := FloatToStr( doubleValueIn );

                    writeStringToXMLNode( XMLNodeInOut, dataIdentifierIn, doubleStr )
                end;

        //string
            procedure writeStringToXMLNode(var XMLNodeInOut : IXMLNode; const dataIdentifierIn, stringValueIn : string);
                begin
                    XMLNodeInOut.AddChild( dataIdentifierIn ).Text := trim( stringValueIn );
                end;

        //arrays
            //integer
                procedure writeIntegerArrayToXMLNode(var XMLNodeInOut : IXMLNode; const dataIdentifierIn : string; integerArrayIn : TArray<integer>);
                    var
                        stringArray : TArray<string>;
                    begin
                        stringArray := convertIntArrayToStringArray( integerArrayIn );

                        writeStringArrayToXMLNode( XMLNodeInOut, dataIdentifierIn, stringArray );
                    end;

            //double
                procedure writeDoubleArrayToXMLNode(var XMLNodeInOut : IXMLNode; const dataIdentifierIn : string; doubleArrayIn : TArray<double>);
                    var
                        stringArray : TArray<string>;
                    begin
                        stringArray := convertDoubleArrayToStringArray( doubleArrayIn );

                        writeStringArrayToXMLNode( XMLNodeInOut, dataIdentifierIn, stringArray );
                    end;

            //string
                procedure writeStringArrayToXMLNode(var XMLNodeInOut : IXMLNode; const dataIdentifierIn : string; stringArrayIn : TArray<string>);
                    var
                        concatenatedArray : string;
                    begin
                        concatenatedArray := string.Join( ARRAY_ELEMENT_DELIMITER, stringArrayIn );

                        writeStringToXMLNode( XMLNodeInOut, dataIdentifierIn, concatenatedArray );
                    end;

end.
