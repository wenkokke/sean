module UUAGC
  def uuagc(args)
    source = project._(:src,:main,:hs,args.first.ext('.ag'))
    target = source.ext('.hs')
    deps   = args.map {|arg| project._(:src,:main,:hs,arg.ext('.ag'))}
    # create file task
    file target => deps do
      mkdir_p File.dirname(target)
      puts agc = "uuagc -P . -Hdcfws --newtypes #{source}"
      fail unless system agc
    end
    with target
    # update uuagc task
    project.task :uuagc => target
    # extend the clean task
    project.clean { rm target if File.exist?(target) }
  end
end

class Buildr::CompileTask
  include UUAGC
end