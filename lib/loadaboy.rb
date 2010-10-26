require 'generator'
require 'enumerator'
require "#{LOADABOY_EXEC_DIR}/Loadafile" if File.exist? "#{LOADABOY_EXEC_DIR}/Loadafile.rb"

module LoadaBoy
  def generate_requests(workers_count, jobs_count)
    @generator = Generator.new
    (1..workers_count).map do |i|
      ["worker#{i}".to_sym, generate_urls(jobs_count)]
    end
  end

  def generate_urls(jobs_count)
    urls = []
    (1..jobs_count).map { @generator.generate.to_a }.flatten.each_slice(2) do |slice|
      urls << slice
    end
    urls
  end
end