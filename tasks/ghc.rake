module GHC
  def ghc(args)
    source = project._(:src,:main,:hs,args.first.ext('.hs'))
    target = project._(:target,File.basename(source).ext('.exe'))
    deps   = args.drop(1).map {|arg| project._(:src,:main,:hs,arg.ext('.hs'))}
    file target => deps do
      mkdir_p File.dirname(target)
      puts ghc = "ghc --make -i#{project._(:src,:main,:hs)} -o #{target} #{source}"
      fail unless system ghc
    end
    with target
    # extend the clean task
    project.clean do
      Dir[project._(:src,:main,:hs,'*.o'),
          project._(:src,:main,:hs,'*.hi')].each {|file| rm file}
    end
  end
end

class Buildr::CompileTask
  include GHC
end