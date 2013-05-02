define 'sean' do
  project.version = '0.0.1'
  
  compile.uuagc(%w{
    SeAn/Lexicon/HM/AG
    SeAn/Lexicon/HM/AG/Base
    SeAn/Lexicon/HM/AG/StdLib
    SeAn/Lexicon/HM/AG/Printing
    SeAn/Lexicon/HM/AG/HM
  })
  
  compile.ghc(%w{
    ParseLLX
    SeAn/Lexicon/HM/AG
    SeAn/Lexicon/HM/Base
    SeAn/Lexicon/HM/Parsing
    SeAn/Lexicon/HM/Printing
    SeAn/Utils/Parsing
  })
  
  compile.ghc(%w{
    ParseLX
    SeAn/Lexicon/HM/AG
    SeAn/Lexicon/HM/Base
    SeAn/Lexicon/HM/Parsing
    SeAn/Lexicon/HM/Printing
    SeAn/Utils/Parsing
  })
  
  compile.ghc(%w{
    LLX2LX
  })
  
end