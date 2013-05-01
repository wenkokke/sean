def exe file
  (ENV['OS'] == 'Windows_NT') ? file.ext('.exe') : file
end