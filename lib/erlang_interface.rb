require 'rubygems'
require 'erlectricity'
require 'loadaboy'

extend LoadaBoy

LOADABOY_EXEC_DIR = `echo $LOADABOY_EXEC_DIR`.chomp
require "#{LOADABOY_EXEC_DIR}/Loadafile" if File.exist? "#{LOADABOY_EXEC_DIR}/Loadafile.rb"

receive do |f|
  f.when([:prepare, Array]) do |array|
    f.send!([:result, generate_requests(array[0], array[1])])
    f.receive_loop
  end
end

