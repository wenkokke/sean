define 'sean' do
  project.version = '0.0.1'
  
  compile.ghc(%w{
    ParseLLX
    
    SeAn/Lexicon/HM/AG
    SeAn/Lexicon/HM/Base
    SeAn/Lexicon/HM/Parsing
    SeAn/Lexicon/HM/Printing
    
    SeAn/Utils/AG
    SeAn/Utils/Parsing
  })
  
  compile.uuagc(%w{
    SeAn/Lexicon/HM/AG
    
    SeAn/Lexicon/HM/AG/Base
    SeAn/Lexicon/HM/AG/StdLib
    SeAn/Lexicon/HM/AG/Printing
    SeAn/Lexicon/HM/AG/Instances
    
    SeAn/Lexicon/HM/AG/ToSTL
    SeAn/Lexicon/HM/AG/ToSTL/HM
    SeAn/Lexicon/HM/AG/ToSTL/FTV
    SeAn/Lexicon/HM/AG/ToSTL/TyEnvs
    SeAn/Lexicon/HM/AG/ToSTL/TySubst
    SeAn/Lexicon/HM/AG/ToSTL/FreshNames
  })
  
  compile.ghc(%w{
    LLX2LX
  })
  
end