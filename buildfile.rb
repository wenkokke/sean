define 'sean' do
  project.version = '0.0.1'
  
  compile.uuagc(%w{
    SeAn/HM/AG
    SeAn/HM/AG/Base
    SeAn/HM/AG/StdLib
    SeAn/HM/AG/Printing
    SeAn/HM/AG/HM
  })
  
  compile.ghc(%w{
    ParseLLX
    SeAn/HM/AG
    SeAn/HM/Base
    SeAn/HM/StdLib
    SeAn/HM/Parsing
    SeAn/HM/Printing
    SeAn/Utils/Parsing
  })
  
  compile.ghc(%w{
    ParseLX
    SeAn/HM/AG
    SeAn/HM/Base
    SeAn/HM/StdLib
    SeAn/HM/Parsing
    SeAn/HM/Printing
    SeAn/Utils/Parsing
  })
  
  compile.ghc(%w{
    LLX2LX
  })
  
end