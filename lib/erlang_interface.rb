require 'rubygems'
require 'erlectricity'
require '../lib/loadaboy.rb'

extend LoadaBoy

receive do |f|
  f.when([:prepare, Array]) do |array|
    f.send!([:result, generate_requests(array[0], array[1])])
    f.receive_loop
  end
end

