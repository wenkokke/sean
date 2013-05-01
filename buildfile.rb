define 'sean' do
  project.version = '0.0.1'
  
  compile.uuagc(%w{
    SeAn/Lexicon/AG
    SeAn/Lexicon/AG/Base
    SeAn/Lexicon/AG/StdLib
    SeAn/Lexicon/AG/Printing
    SeAn/Lexicon/AG/HM
  })
  
  compile.ghc(%w{
    Main
    SeAn/Lexicon/AG
    SeAn/Lexicon/Base
    SeAn/Lexicon/StdLib
    SeAn/Lexicon/Parsing
    SeAn/Lexicon/Printing
  })
  
end