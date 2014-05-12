header = true
File.open('bin/switch-radius.csv', 'w') do |f|
  Dir['bin/*/switch-radius.csv'].each do |file_name|
    line_number = -1
    File.open(file_name).each do |line|
      line_number = line_number + 1
      if line_number == 0 && header == false
        next
      end
      header = false

      f.puts line
    end
  end
end
