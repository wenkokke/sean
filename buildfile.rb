define 'sean' do
  project.version = '0.0.1'
  
  compile.uuagc(%w{
    AG AG/Base AG/HM AG/Printing AG/StdLib
  })
  
  compile.ghc(%w{
    Main AG Base Parsing Printing StdLib
  })
  
end