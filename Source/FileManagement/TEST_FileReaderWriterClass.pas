unit TEST_FileReaderWriterClass;

interface

    uses
        system.SysUtils,
        DUnitX.TestFramework;

    type
      [TestFixture]
      TTestFileReaderWriterClass = class
      public
        // Simple single Test
        [Test]
        procedure testReadWriteBool();
      end;

implementation

    procedure TTestFileReaderWriterClass.testReadWriteBool();
        begin

        end;

end.
