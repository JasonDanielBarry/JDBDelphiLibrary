unit FileReaderWriterClass;

interface

    uses
        system.SysUtils, system.Classes, system.Generics.Collections, system.StrUtils,
        Xml.XMLDoc, Xml.XMLIntf, xml.xmldom,
        ArrayConversionMethods
        ;

    type
        TFileReaderWriter = class
            private
                const
                    ARRAY_ELEMENT_DELIMITER : string = ';';
                    ITEM_PREFIX             : string = 'Item_';
                    DATA_TYPE_STRING        : string = 'DataType';
                    ROOT_STRING             : string = 'Root';
                    VALUE_STRING            : string = 'Value';
                    //data type strings
                        DT_NONE         : string = 'none';
                        DT_BOOL         : string = 'boolean';
                        DT_INT          : string = 'integer';
                        DT_INT_ARRAY    : string = 'integer_array';
                        DT_DOUBLE       : string = 'double';
                        DT_DOUBLE_ARRAY : string = 'double_array';
                        DT_CHAR         : string = 'char';
                        DT_STRING       : string = 'string';
                        DT_STRING_ARRAY : string = 'string_array';
                var
                    fileName        : string;
                    rootNode        : IXMLNode;
                    XMLFileDocument : IXMLDocument;
                //read and write sinlge values to XML
                    function tryReadValueFromXML(const identifierIn, dataTypeIn : string; out valueOut : string) : boolean;
                    procedure writeValueToXML(const identifierIn, dataTypeIn, valueIn : string);
                //read and write arrays to XML
                    function tryReadArrayFromXML(const identifierIn, dataTypeIn : string; out arrayOut : TArray<string>) : boolean;
                    procedure writeArrayToXML(const identifierIn, dataTypeIn : string; arrayIn : TArray<string>);
            protected
                //made a new document
                    procedure resetXMLDocument();

                //create a new node belonging to the root node
                    function createNewNode(const nodeIdentifierIn, nodeDataTypeIn : string) : IXMLNode;
                //check that a node with the identifier exists
                    function checkNodeExists(const nodeIdentifierIn : string) : boolean; overload;
                    function tryGetNode(const nodeIdentifierIn : string; out XMLNodeOut : IXMLNode) : boolean; overload;
                //get an identifier's data type
                    function getNodeDataType(const nodeIdentifierIn : string) : string;
            public
                //constructor
                    constructor create(const fileNameIn : string); virtual;
                //destructor
                    destructor destroy(); override;
                //file methods
                    //load file
                        function loadFile() : boolean;
                    //save file
                        procedure saveFile();
                //read methods
                    //single values
                        function tryReadBool(const identifierIn : string; out valueOut : boolean; const defaultValueIn : boolean = False) : boolean;
                        function tryReadInteger(const identifierIn : string; out valueOut : integer; const defaultValueIn : integer = 0) : boolean;
                        function tryReadDouble(const identifierIn : string; out valueOut : double; const defaultValueIn : double = 0) : boolean;
                        function tryReadChar(const identifierIn : string; out valueOut : char; const defaultValueIn : char = ' ') : boolean;
                        function tryReadString(const identifierIn : string; out valueOut : string; const defaultValueIn : string = '') : boolean;
                    //arrays
                        function tryReadIntegerArray(const identifierIn : string; out arrayOut : TArray<integer>) : boolean;
                        function tryReadDoubleArray(const identifierIn : string; out arrayOut : TArray<double>) : boolean;
                        function tryReadStringArray(const identifierIn : string; out arrayOut : TArray<string>) : boolean;
                //write methods
                    //single values
                        procedure writeBool(const identifierIn : string; const valueIn : boolean);
                        procedure writeInteger(const identifierIn : string; const valueIn : integer);
                        procedure writeDouble(const identifierIn : string; const valueIn : double);
                        procedure writeChar(const identifierIn : string; const valueIn : char);
                        procedure writeString(const identifierIn, valueIn : string); overload;
                    //arrays
                        procedure writeIntegerArray(const identifierIn : string; const arrayIn : TArray<integer>);
                        procedure writeDoubleArray(const identifierIn : string; const arrayIn : TArray<double>);
                        procedure writeStringArray(const identifierIn : string; const arrayIn : TArray<string>);
        end;

implementation

    //private
        //read and write sinlge values to XML
            function TFileReaderWriter.tryReadValueFromXML(const identifierIn, dataTypeIn : string; out valueOut : string) : boolean;
                var
                    nodeExists      : boolean;
                    nodeDataType    : string;
                    itemNode        : IXMLNode;
                begin
                    //initialise output value
                        valueOut := '';

                    //check the node exists
                        nodeExists := tryGetNode( identifierIn, itemNode );

                        if NOT( nodeExists ) then
                            exit( False );

                    //check the node is the required data type
                        nodeDataType := getNodeDataType( identifierIn );

                        if ( nodeDataType <> dataTypeIn ) then
                            exit( False );

                    valueOut := trim( itemNode.ChildNodes.FindNode( VALUE_STRING ).Text );

                    result := True;
                end;

            procedure TFileReaderWriter.writeValueToXML(const identifierIn, dataTypeIn, valueIn : string);
                var
                    nodeAlreadyExists   : boolean;
                    itemNode            : IXMLNode;
                begin
                    //check the node exists
                        nodeAlreadyExists := checkNodeExists( identifierIn );

                        if ( nodeAlreadyExists ) then
                            exit();

                    //write the node's data-type & value
                        itemNode := createNewNode( identifierIn, dataTypeIn );

                        itemNode.AddChild( VALUE_STRING ).text := Trim( valueIn );
                end;

        //read and write arrays to XML
            function TFileReaderWriter.tryReadArrayFromXML(const identifierIn, dataTypeIn : string; out arrayOut : TArray<string>) : boolean;
                var
                    readSuccessful, valueIsArray    : boolean;
                    i                               : integer;
                    concatenatedStringArray         : string;
                begin
                    //test for key
                        readSuccessful := tryReadValueFromXML( identifierIn, dataTypeIn, concatenatedStringArray );

                        if ( NOT(readSuccessful) ) then
                            begin
                                arrayOut := [];
                                exit( False );
                            end;

                    //test if value is an array
                        valueIsArray := ( Pos( ARRAY_ELEMENT_DELIMITER, concatenatedStringArray ) > 1 );

                        if ( NOT(valueIsArray) ) then
                            begin
                                arrayOut := [];
                                exit( False );
                            end;

                    //split string into array
                        arrayOut := SplitString( concatenatedStringArray, ARRAY_ELEMENT_DELIMITER );

                    //trim elements
                        for i := 0 to (length(arrayOut) - 1) do
                            arrayOut[i] := trim(arrayOut[i]);

                    result := True;
                end;

            procedure TFileReaderWriter.writeArrayToXML(const identifierIn, dataTypeIn : string; arrayIn : TArray<string>);
                var
                    i, arrLen               : integer;
                    concatenatedStringArray : string;
                begin
                    arrLen := length( arrayIn );

                    concatenatedStringArray := string.join( ';', arrayIn );

                    writeValueToXML( identifierIn, dataTypeIn, concatenatedStringArray );
                end;

    //protected
        //made a new document
            procedure TFileReaderWriter.resetXMLDocument();
                begin
                    XMLFileDocument         := NewXMLDocument();
                    XMLFileDocument.Options := XMLFileDocument.Options + [doNodeAutoIndent];
                    XMLFileDocument.Active  := True;
                    rootNode                := XMLFileDocument.AddChild( ROOT_STRING );
                end;

        //create a new node belonging to the root node
            function TFileReaderWriter.createNewNode(const nodeIdentifierIn, nodeDataTypeIn : string) : IXMLNode;
                var
                    nodeIdentifierAlreadyUsed   : boolean;
                    newNodeOut                  : IXMLNode;
                begin
                    //check if the node already exists
                        nodeIdentifierAlreadyUsed := checkNodeExists( nodeIdentifierIn );

                        if (nodeIdentifierAlreadyUsed) then
                            exit( nil );

                    //create the new node and assign its data type
                        newNodeOut := rootNode.AddChild( ITEM_PREFIX + nodeIdentifierIn );

                        newNodeOut.AddChild( DATA_TYPE_STRING ).text := nodeDataTypeIn;

                    result := newNodeOut;
                end;

        //check that a node exists
            function TFileReaderWriter.checkNodeExists(const nodeIdentifierIn : string) : boolean;
                var
                    dummyNode : IXMLNode;
                begin
                    result := tryGetNode( nodeIdentifierIn, dummyNode );
                end;

            function TFileReaderWriter.tryGetNode(const nodeIdentifierIn : string; out XMLNodeOut : IXMLNode) : boolean;
                begin
                    //get the node
                        XMLNodeOut := rootNode.ChildNodes.FindNode( ITEM_PREFIX + nodeIdentifierIn );

                    //if item node = nil then the node does not exist
                        result := Assigned( XMLNodeOut );
                end;

        //get an identifier's data type
            function TFileReaderWriter.getNodeDataType(const nodeIdentifierIn : string) : string;
                var
                    identifierExists    : boolean;
                    itemNode            : IXMLNode;
                begin
                    identifierExists := tryGetNode( nodeIdentifierIn, itemNode );

                    if ( NOT(identifierExists) ) then
                        exit( DT_NONE );

                    result := itemNode.ChildNodes.FindNode( DATA_TYPE_STRING ).text;
                end;

    //public
        //constructor
            constructor TFileReaderWriter.create(const fileNameIn : string);
                begin
                    inherited create();

                    fileName := fileNameIn;

                    resetXMLDocument();
                end;

        //destructor
            destructor TFileReaderWriter.destroy();
                begin
                    inherited destroy();
                end;

        //file methods
            //load file
                function TFileReaderWriter.loadFile() : boolean;
                    var
                        fileDoesNotExist    : boolean;
                        i, nodeCount        : integer;
                        key, value          : string;
                        itemNode            : IXMLNode;
                    begin
                        //check that the file exist
                            fileDoesNotExist := NOT( FileExists( fileName ) );

                            if ( fileDoesNotExist ) then
                                exit( false );

                        //load in the XML file
                            XMLFileDocument := LoadXMLDocument( fileName );

                            XMLFileDocument.Active := True;

                        //get the root node
                            rootNode := XMLFileDocument.DocumentElement;

                        result := True;
                    end;

            //save file
                procedure TFileReaderWriter.saveFile();
                    begin
                        //save the document
                            XMLFileDocument.SaveToFile( fileName );
                    end;

        //read methods
            //single values
                function TFileReaderWriter.tryReadBool(const identifierIn : string; out valueOut : boolean; const defaultValueIn : boolean = False) : boolean;
                    var
                        readSuccessful, valueIsBool :  boolean;
                        stringValue                 : string;
                    begin
                        //check the ID key exists
                            readSuccessful := tryReadValueFromXML( identifierIn, DT_BOOL, stringValue );

                            if ( NOT(readSuccessful) ) then
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
                        readSuccessful, valueIsInt  : boolean;
                        stringValue                 : string;
                    begin
                        //check the ID key exists
                            readSuccessful := tryReadValueFromXML( identifierIn, DT_INT, stringValue );

                            if ( NOT(readSuccessful) ) then
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
                        readSuccessful, valueIsDouble   : boolean;
                        stringValue                     : string;
                    begin
                        //check the ID key exists
                            readSuccessful := tryReadValueFromXML( identifierIn, DT_DOUBLE, stringValue );

                            if ( NOT(readSuccessful) ) then
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
                        readSuccessful, valueIsChar : boolean;
                        stringValue                 : string;
                    begin
                        //check the ID key exists
                            readSuccessful := tryReadValueFromXML( identifierIn, DT_CHAR, stringValue );

                            if ( NOT(readSuccessful) ) then
                                begin
                                    valueOut := defaultValueIn;
                                    exit( False );
                                end;

                        //check data is a char
                            valueIsChar := ( length(stringValue) = 1 );

                            if ( NOT(valueIsChar) ) then
                                begin
                                    valueOut := defaultValueIn;
                                    exit( False );
                                end;

                        valueOut := stringValue[1];

                        result := True;
                    end;

                function TFileReaderWriter.tryReadString(const identifierIn : string; out valueOut : string; const defaultValueIn : string = '') : boolean;
                    var
                        readSuccessful : boolean;
                    begin
                        readSuccessful := tryReadValueFromXML( identifierIn, DT_STRING, valueOut );

                        if ( NOT(readSuccessful) ) then
                            valueOut := defaultValueIn;

                        valueOut := trim( valueOut );

                        result := readSuccessful;
                    end;

            //arrays
                function TFileReaderWriter.tryReadIntegerArray(const identifierIn : string; out arrayOut : TArray<integer>) : boolean;
                    var
                        readSuccessful      : boolean;
                        i, arrLen           : integer;
                        stringValuesArray   : TArray<string>;
                    begin
                        //check if the identifier has a readable array
                            readSuccessful := tryReadArrayFromXML( identifierIn, DT_INT_ARRAY, stringValuesArray );

                            if ( NOT(readSuccessful) ) then
                                begin
                                    arrayOut := [];
                                    exit( False );
                                end;

                        //convert data to integers
                            result := tryConvertStringArrayToIntArray( stringValuesArray, arrayOut );
                    end;

                function TFileReaderWriter.tryReadDoubleArray(const identifierIn : string; out arrayOut : TArray<double>) : boolean;
                    var
                        readSuccessful        : boolean;
                        i, arrLen           : integer;
                        stringValuesArray   : TArray<string>;
                    begin
                        //check if the identifier has a readable array
                            readSuccessful := tryReadArrayFromXML( identifierIn, DT_DOUBLE_ARRAY, stringValuesArray );

                            if ( NOT(readSuccessful) ) then
                                begin
                                    arrayOut := [];
                                    exit( False );
                                end;

                        //convert data to doubles
                            result := tryConvertStringArrayToDoubleArray( stringValuesArray, arrayOut );
                    end;

                function TFileReaderWriter.tryReadStringArray(const identifierIn : string; out arrayOut : TArray<string>) : boolean;

                    var
                        readSuccessful, valueIsArray    : boolean;
                        i                               : integer;
                        stringValuesArray               : TArray<string>;
                    begin
                        //check if the identifier has a readable array
                            readSuccessful := tryReadArrayFromXML( identifierIn, DT_STRING_ARRAY, arrayOut );

                            if ( NOT(readSuccessful) ) then
                                begin
                                    arrayOut := [];
                                    exit( False );
                                end;

                        result := True;
                    end;

        //write methods
            //single values
                procedure TFileReaderWriter.writeBool(const identifierIn : string; const valueIn : boolean);
                    var
                        boolString : string;
                    begin
                        boolString := BoolToStr( valueIn );

                        writeValueToXML( identifierIn, DT_BOOL, boolString );
                    end;

                procedure TFileReaderWriter.writeInteger(const identifierIn : string; const valueIn : integer);
                    var
                        integerString : string;
                    begin
                        integerString := IntToStr( valueIn );

                        writeValueToXML( identifierIn, DT_INT, integerString );
                    end;

                procedure TFileReaderWriter.writeDouble(const identifierIn : string; const valueIn : double);
                    var
                        doubleString : string;
                    begin
                        doubleString := FloatToStr( valueIn );

                        writeValueToXML( identifierIn, DT_DOUBLE, doubleString );
                    end;

                procedure TFileReaderWriter.writeChar(const identifierIn : string; const valueIn : char);
                    begin
                        writeValueToXML( identifierIn, DT_CHAR, valueIn ); //char can cast to string automatically
                    end;

                procedure TFileReaderWriter.writeString(const identifierIn, valueIn : string);
                    var
                        identifierAlreadyUsed : boolean;
                    begin
                        writeValueToXML( identifierIn, DT_STRING, valueIn );
                    end;

            //arrays
                procedure TFileReaderWriter.writeIntegerArray(const identifierIn : string; const arrayIn : TArray<integer>);
                    var
                        stringValueArray : TArray<string>;
                    begin
                        stringValueArray := convertIntArrayToStringArray( arrayIn );

                        writeArrayToXML( identifierIn, DT_INT_ARRAY, stringValueArray );
                    end;

                procedure TFileReaderWriter.writeDoubleArray(const identifierIn : string; const arrayIn : TArray<double>);
                    var
                        stringValueArray : TArray<string>;
                    begin
                        stringValueArray := convertDoubleArrayToStringArray( arrayIn );

                        writeArrayToXML( identifierIn, DT_DOUBLE_ARRAY, stringValueArray );
                    end;

                procedure TFileReaderWriter.writeStringArray(const identifierIn : string; const arrayIn : TArray<string>);
                    begin
                        writeArrayToXML( identifierIn, DT_STRING_ARRAY, arrayIn );
                    end;

end.
