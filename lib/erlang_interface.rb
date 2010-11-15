require 'rubygems'
require 'erlectricity'
require 'loadaboy'

extend LoadaBoy

LOADABOY_EXEC_DIR = `echo $LOADABOY_EXEC_DIR`.chomp
require "#{LOADABOY_EXEC_DIR}/Loadafile" if File.exist? "#{LOADABOY_EXEC_DIR}/Loadafile.rb"

receive do |f|
  f.when([:prepare, Array]) do |array|
    workers, jobs, service = array
    f.send!([:result, generate_requests(workers, jobs, service)])
    f.receive_loop
  end
end

