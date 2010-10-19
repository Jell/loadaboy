require 'generator.rb'
require 'Loadafile.rb'

module LoadaBoy
  def generate_requests(workers_count, jobs_count)
    generator = Generator.new
    (1..workers_count).map do |i|
      ["worker#{i}".to_sym, (1..jobs_count).map { generator.generate.to_a }.flatten(1)]
    end
  end
end