require 'csv'

# header = true
# File.open('bin/switch-radius.csv', 'w') do |f|
#   Dir['bin/*/switch-radius.csv'].each do |file_name|
#     line_number = -1
#     File.open(file_name).each do |line|
#       line_number = line_number + 1
#       if line_number == 0 && header == false
#         next
#       end
#       header = false
# 
#       f.puts line
#     end
#   end
# end

#header = true
#File.open('bin/switch-straight-length.csv', 'w') do |f|
#  Dir['bin/*/switch-straight-length.csv'].each do |file_name|
#    line_number = -1
#    File.open(file_name).each do |line|
#      line_number = line_number + 1
#      if line_number == 0 && header == false
#        next
#      end
#      header = false
#
#      f.puts line
#    end
#  end
#end

def merge(base_name)
  switch_turn_length = {}
  File.open("bin/#{base_name}.csv", 'w') do |f|
    header = ""
    Dir["bin/*/#{base_name}.csv"].each do |file_name|
      csv = CSV.new(File.read(file_name))
      array =  csv.to_a
      header = array[0].join(",")
      array[1..-1].each do |row|
        switch_turn_length[row[0..-2]] = row[-1]
      end
    end

    f.puts header
    switch_turn_length.each do |key, value|
      f.puts key.join(",") + "," + value
    end
  end
end


merge 'switch-turn-length'
merge 'switch-straight-length'
merge 'switch-radius'

switch_turn_length = {}
csv = CSV.new(File.read("bin/switch-turn-length.csv"))
csv.to_a[1..-1].each do |row|
  switch_turn_length[[row[0], row[1].sub('-', ''), row[2]]] = row[-1].to_f
end
#p switch_turn_length


switch_radius = {}
csv = CSV.new(File.read("bin/switch-radius.csv"))
csv.to_a[1..-1].each do |row|
  switch_length = switch_turn_length[[row[1], row[2].sub('-', ''), row[3]]]
  percent = (row[5].to_f / switch_length * 100.0).to_i
  if switch_radius[[row[1], row[2].sub('-', ''), row[3], percent]].nil?
    switch_radius[[row[1], row[2].sub('-', ''), row[3], percent]] = row[6]
  else
    if (switch_radius[[row[1], row[2].sub('-', ''), row[3], percent]].to_f - row[6].to_f).abs > 1e-5
      puts "ERROR"
      puts switch_radius[[row[1], row[2].sub('-', ''), row[3], percent]].to_f
      puts row[6].to_f
    end
  end
end

File.open("bin/switch_percent_radius.csv", 'w') do |f|
  f.puts "start_radius,angle,end_radius,percent,radius"
  switch_radius.sort_by { |key, value| key }.each do |key, value|
    if key[0] == "90" && key[2] == "110"
      f.puts key.join(",") + "," + value
    end
  end
end
p switch_radius
