unit FileReaderWriterClass;

interface

    uses
        system.SysUtils, system.Classes, system.Generics.Collections, system.StrUtils,
        Xml.XMLDoc, Xml.XMLIntf
        ;

    type
        TFileReaderWriter = class
            private
                const
                    ARRAY_ELEMENT_DELIMITER : string = ';';
                type
                    TIdenifierValueItem = TPair<string, string>;
                    TIdentifierValueMap = TDictionary<string, string>;
                var
                    fileName        : string;
                    fileContentsMap : TIdentifierValueMap;
            public
                //constructor
                    constructor create(const fileNameIn : string); virtual;
                //destructor
                    destructor destroy(); override;
                //load file
                    function loadFile() : boolean; virtual;
                //save file
                    procedure saveFile(); virtual;
                //read methods
                    //single values
                        function tryReadBool(const identifierIn : string; out valueOut : boolean; const defaultValueIn : boolean = False) : boolean;
                        function tryReadInteger(const identifierIn : string; out valueOut : integer; const defaultValueIn : integer = 0) : boolean;
                        function tryReadDouble(const identifierIn : string; out valueOut : double; const defaultValueIn : double = 0) : boolean;
                        function tryReadChar(const identifierIn : string; out valueOut : char; const defaultValueIn : char = ' ') : boolean;
                        function tryReadString(const identifierIn : string; out valueOut : string; const defaultValueIn : string = '') : boolean;
                    //arrays
                        function tryReadIntegerArray(const identifierIn : string; out valueOut : TArray<integer>) : boolean;
                        function tryReadDoubleArray(const identifierIn : string; out valueOut : TArray<double>) : boolean;
                        function tryReadStringArray(const identifierIn : string; out valueOut : TArray<string>) : boolean;
                //write methods
                    //single values
                        procedure writeBool(const identifierIn : string; const valueIn : boolean);
                        procedure writeInteger(const identifierIn : string; const valueIn : integer);
                        procedure writeDouble(const identifierIn : string; const valueIn : double);
                        procedure writeChar(const identifierIn : string; const valueIn : char);
                        procedure writeString(const identifierIn, valueIn : string);
                    //arrays
                        procedure writeIntegerArray(const identifierIn : string; const valueArrayIn : TArray<integer>);
                        procedure writeDoubleArray(const identifierIn : string; const valueArrayIn : TArray<double>);
                        procedure writeStringArray(const identifierIn : string; const valueArrayIn : TArray<string>);
        end;

implementation

    //public
        //constructor
            constructor TFileReaderWriter.create(const fileNameIn : string);
                begin
                    inherited create();

                    fileName := fileNameIn;

                    fileContentsMap := TIdentifierValueMap.Create();
                    fileContentsMap.Clear();
                end;

        //destructor
            destructor TFileReaderWriter.destroy();
                begin
                    FreeAndNil( fileContentsMap );

                    inherited destroy();
                end;

        //load file
            function TFileReaderWriter.loadFile() : boolean;
                var
                    fileDoesNotExist    : boolean;
                    i, nodeCount        : integer;
                    key, value          : string;
                    rootNode, itemNode  : IXMLNode;
                    XMLLoadDocument     : IXMLDocument;
                begin
                    //check that the file exist
                        fileDoesNotExist := NOT( FileExists( fileName ) );

                        if ( fileDoesNotExist ) then
                            exit( false );

                    //clear the map
                        fileContentsMap.Clear();

                    //load in the XML file
                        XMLLoadDocument := LoadXMLDocument( fileName );

                        rootNode := XMLLoadDocument.DocumentElement;

                    //loop through the child nodes
                        nodeCount := rootNode.ChildNodes.Count;

                        for i := 0 to (nodeCount - 1) do
                            begin
                                itemNode := rootNode.ChildNodes[i];

                                //place data in map
                                    key := itemNode.Attributes['ID_KEY'];

                                    value := itemNode.Text;

                                    fileContentsMap.TryAdd( key, value );
                            end;

                    result := True;
                end;

        //save file
            procedure TFileReaderWriter.saveFile();
                var
                    rootNode, itemNode  : IXMLNode;
                    XMLSaveDocument     : IXMLDocument;
                    mapItem             : TIdenifierValueItem;
                begin
                    //set up new document
                        XMLSaveDocument := NewXMLDocument();

                        XMLSaveDocument.Options := [ doNodeAutoIndent ];

                        rootNode := XMLSaveDocument.AddChild('Items');

                    //write each item to the XML document
                        for mapItem in fileContentsMap do
                            begin
                                itemNode := rootNode.AddChild('Item');

                                itemNode.Attributes['ID_KEY'] := mapItem.Key;

                                itemNode.Text := mapItem.Value;
                            end;

                    //save the document
                        XMLSaveDocument.SaveToFile( fileName );
                end;

        //read methods
            //single values
                function TFileReaderWriter.tryReadBool(const identifierIn : string; out valueOut : boolean; const defaultValueIn : boolean = False) : boolean;
                    var
                        keyExists, valueIsBool  : boolean;
                        stringValue             : string;
                    begin
                        //check the ID key exists
                            keyExists := tryReadString( identifierIn, stringValue );

                            if ( NOT(keyExists) ) then
                                begin
                                    valueOut := defaultValueIn;
                                    exit( false );
                                end;

                        //check the data is a boolean
                            valueIsBool := TryStrToBool( stringValue, valueOut );

                            if ( NOT(valueIsBool) ) then
                                begin
                                    valueOut := defaultValueIn;
                                    exit( false );
                                end;

                        result := True;
                    end;

                function TFileReaderWriter.tryReadInteger(const identifierIn : string; out valueOut : integer; const defaultValueIn : integer = 0) : boolean;
                    var
                        keyExists, valueIsInt   : boolean;
                        stringValue             : string;
                    begin
                        //check the ID key exists
                            keyExists := tryReadString( identifierIn, stringValue );

                            if ( NOT(keyExists) ) then
                                begin
                                    valueOut := defaultValueIn;
                                    exit( false );
                                end;

                        //check the data is an integer
                            valueIsInt := TryStrToInt( stringValue, valueOut );

                            if ( NOT(valueIsInt) ) then
                                begin
                                    valueOut := defaultValueIn;
                                    exit( false );
                                end;

                        result := True;
                    end;

                function TFileReaderWriter.tryReadDouble(const identifierIn : string; out valueOut : double; const defaultValueIn : double = 0) : boolean;
                    var
                        keyExists, valueIsDouble    : boolean;
                        stringValue                 : string;
                    begin
                        //check the ID key exists
                            keyExists := tryReadString( identifierIn, stringValue );

                            if ( NOT(keyExists) ) then
                                begin
                                    valueOut := defaultValueIn;
                                    exit( false );
                                end;

                        //check the data is an integer
                            valueIsDouble := TryStrToFloat( stringValue, valueOut );

                            if ( NOT(valueIsDouble) ) then
                                begin
                                    valueOut := defaultValueIn;
                                    exit( false );
                                end;

                        result := True;
                    end;

                function TFileReaderWriter.tryReadChar(const identifierIn : string; out valueOut : char; const defaultValueIn : char = ' ') : boolean;
                    var
                        keyExists, valueIsChar  : boolean;
                        stringValue             : string;
                    begin
                        //check the ID key exists
                            keyExists := tryReadString( identifierIn, stringValue );

                            if ( NOT(keyExists) ) then
                                begin
                                    valueOut := defaultValueIn;
                                    exit( false );
                                end;

                        //check data is a char
                            valueIsChar := ( length(stringValue) = 1 );

                            if ( NOT(valueIsChar) ) then
                                begin
                                    valueOut := defaultValueIn;
                                    exit( false );
                                end;

                        valueOut := stringValue[1];

                        result := True;
                    end;

                function TFileReaderWriter.tryReadString(const identifierIn : string; out valueOut : string; const defaultValueIn : string = '') : boolean;
                    var
                        keyExists : boolean;
                    begin
                        keyExists := fileContentsMap.TryGetValue( identifierIn, valueOut );

                        if ( NOT(keyExists) ) then
                            valueOut := defaultValueIn;

                        valueOut := trim( valueOut );

                        result := keyExists;
                    end;

            //arrays
                function TFileReaderWriter.tryReadIntegerArray(const identifierIn : string; out valueOut : TArray<integer>) : boolean;
                    var
                        canReadValue        : boolean;
                        i, arrLen           : integer;
                        stringValuesArray   : TArray<string>;
                    begin
                        //check if the identifier has a readable
                            canReadValue := tryReadStringArray( identifierIn, stringValuesArray );

                            if ( NOT(canReadValue) ) then
                                begin
                                    valueOut := [];
                                    exit( False );
                                end;

                        //convert data to integers
                            arrLen := Length( stringValuesArray );

                            SetLength( valueOut, arrLen );

                            for i := 0 to (arrLen - 1) do
                                valueOut[i] := StrToInt( stringValuesArray[i] );

                        result := True;
                    end;

                function TFileReaderWriter.tryReadDoubleArray(const identifierIn : string; out valueOut : TArray<double>) : boolean;
                    var
                        canReadValue        : boolean;
                        i, arrLen           : integer;
                        stringValuesArray   : TArray<string>;
                    begin
                        //check if the identifier has a readable
                            canReadValue := tryReadStringArray( identifierIn, stringValuesArray );

                            if ( NOT(canReadValue) ) then
                                begin
                                    valueOut := [];
                                    exit( False );
                                end;

                        //convert data to doubles
                            arrLen := Length( stringValuesArray );

                            SetLength( valueOut, arrLen );

                            for i := 0 to (arrLen - 1) do
                                valueOut[i] := StrToFloat( stringValuesArray[i] );

                        result := True;
                    end;

                function TFileReaderWriter.tryReadStringArray(const identifierIn : string; out valueOut : TArray<string>) : boolean;

                    var
                        keyExists, valueIsArray : boolean;
                        i                       : integer;
                        concatenatedStringArray : string;
                    begin
                        //test for key
                            keyExists := tryReadString( identifierIn, concatenatedStringArray );

                            if ( NOT(keyExists) ) then
                                begin
                                    valueOut := [];
                                    exit( False );
                                end;

                        //test if value is an array
                            valueIsArray := ( Pos( ARRAY_ELEMENT_DELIMITER, concatenatedStringArray ) > 1 );

                            if ( NOT(valueIsArray) ) then
                                begin
                                    valueOut := [];
                                    exit( False );
                                end;

                        //split string into array
                            valueOut := SplitString( concatenatedStringArray, ARRAY_ELEMENT_DELIMITER );

                        //trim elements
                            for i := 0 to (length(valueOut) - 1) do
                                valueOut[i] := trim(valueOut[i]);

                        result := True;
                    end;

        //write methods
            //single values
                procedure TFileReaderWriter.writeBool(const identifierIn : string; const valueIn : boolean);
                    var
                        boolString : string;
                    begin
                        boolString := BoolToStr( valueIn );

                        writeString( identifierIn, boolString );
                    end;

                procedure TFileReaderWriter.writeInteger(const identifierIn : string; const valueIn : integer);
                    var
                        integerString : string;
                    begin
                        integerString := IntToStr( valueIn );

                        writeString( identifierIn, integerString );
                    end;

                procedure TFileReaderWriter.writeDouble(const identifierIn : string; const valueIn : double);
                    var
                        doubleString : string;
                    begin
                        doubleString := FloatToStr( valueIn );

                        writeString( identifierIn, doubleString );
                    end;

                procedure TFileReaderWriter.writeChar(const identifierIn : string; const valueIn : char);
                    begin
                        writeString( identifierIn, valueIn ); //char can cast to string automatically
                    end;

                procedure TFileReaderWriter.writeString(const identifierIn, valueIn : string);
                    var
                        identifierAlreadyUsed : boolean;
                    begin
                        identifierAlreadyUsed := fileContentsMap.TryAdd( identifierIn, trim( valueIn ) );
                    end;

            //arrays
                procedure TFileReaderWriter.writeIntegerArray(const identifierIn : string; const valueArrayIn : TArray<integer>);
                    var
                        i, arrLen           : integer;
                        stringValueArray    : TArray<string>;
                    begin
                        arrLen := length( valueArrayIn );

                        SetLength( stringValueArray, arrLen );

                        for i := 0 to (arrLen - 1) do
                            stringValueArray[i] := IntToStr( valueArrayIn[i] );

                        writeStringArray( identifierIn, stringValueArray );
                    end;


                procedure TFileReaderWriter.writeDoubleArray(const identifierIn : string; const valueArrayIn : TArray<double>);
                    var
                        i, arrLen           : integer;
                        stringValueArray    : TArray<string>;
                    begin
                        arrLen := length( valueArrayIn );

                        SetLength( stringValueArray, arrLen );

                        for i := 0 to (arrLen - 1) do
                            stringValueArray[i] := FloatToStr( valueArrayIn[i] );

                        writeStringArray( identifierIn, stringValueArray );
                    end;

                procedure TFileReaderWriter.writeStringArray(const identifierIn : string; const valueArrayIn : TArray<string>);
                    var
                        i, arrLen               : integer;
                        concatenatedStringArray : string;
                    begin
                        arrLen := length( valueArrayIn );

                        concatenatedStringArray := trim(valueArrayIn[0]);

                        for i := 1 to (arrLen - 1) do
                            concatenatedStringArray := concatenatedStringArray + ARRAY_ELEMENT_DELIMITER + trim(valueArrayIn[i]);

                        writeString( identifierIn ,concatenatedStringArray );
                    end;

end.
