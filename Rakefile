require 'rubygems'
require 'rake'

task :ebuild do
  ERLC_TEST_FLAGS = ""
  ERLC_FLAGS = "-o ../ebin"
  cd "elib"
  sh "erlc  #{ERLC_FLAGS} #{ERLC_TEST_FLAGS} #{Dir["**/*.erl"].join(" ")}"
end

begin
  require 'jeweler'
  Jeweler::Tasks.new do |gem|
    gem.name = "loadaboy"
    gem.executables     = %W(loadaboy)
    gem.summary = %Q{LoadaBoy - Load Test Gem to the Rescue!}
    gem.description = %Q{LoadaBoy is a load testing gem.}
    gem.email = "jean-louis@icehouse.se"
    gem.homepage = "http://github.com/Jell/loadaboy"
    gem.authors = ["Jell"]
    gem.files.exclude 'ebin/test_*', 'elib/minitest.erl'
    gem.files.include(["ext"])
    gem.extensions << 'ext/extconf.rb'
    gem.add_development_dependency "thoughtbot-shoulda", ">= 0"
    gem.add_dependency  "erlectricity", "~>1.1.1"
    # gem is a Gem::Specification... see http://www.rubygems.org/read/chapter/20 for additional settings
  end
  Jeweler::GemcutterTasks.new
rescue LoadError
  puts "Jeweler (or a dependency) not available. Install it with: gem install jeweler"
end

require 'rake/testtask'
Rake::TestTask.new(:test) do |test|
  test.libs << 'lib' << 'test'
  test.pattern = 'test/**/test_*.rb'
  test.verbose = true
end

task :etest do
  sh "cd ebin && ./test_load_test.erl"
end

begin
  require 'rcov/rcovtask'
  Rcov::RcovTask.new do |test|
    test.libs << 'test'
    test.pattern = 'test/**/test_*.rb'
    test.verbose = true
  end
rescue LoadError
  task :rcov do
    abort "RCov is not available. In order to run rcov, you must: sudo gem install spicycode-rcov"
  end
end

task :test => :check_dependencies

task :default => [:test, :etest]

require 'rake/rdoctask'
Rake::RDocTask.new do |rdoc|
  version = File.exist?('VERSION') ? File.read('VERSION') : ""

  rdoc.rdoc_dir = 'rdoc'
  rdoc.title = "loadaboy #{version}"
  rdoc.rdoc_files.include('README*')
  rdoc.rdoc_files.include('lib/**/*.rb')
end
