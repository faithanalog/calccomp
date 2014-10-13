#!/usr/bin/env ruby
ARGV.each do |a|
    if a != ":"
        puts (0x100 | Integer(a, 2)).to_s(16)[1..-1]
    end
end
