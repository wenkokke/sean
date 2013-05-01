require 'yaml'

module SemAnTE
  attr_accessor :file
  @file = "#{ENV['HOME']}/.semante/settings.yml"
  def self.load_file
    if @yaml.nil?
       puts "settings: loading #{@file}"
       @yaml = YAML.load_file(@file)
    end
    @yaml
  end
  def self.[](*keys)
    file = load_file()["SemAnTE"]
    keys.each do |key|
      file = file[key]
    end
    file
  end
end