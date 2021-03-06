# Generated by jeweler
# DO NOT EDIT THIS FILE DIRECTLY
# Instead, edit Jeweler::Tasks in Rakefile, and run the gemspec command
# -*- encoding: utf-8 -*-

Gem::Specification.new do |s|
  s.name = %q{loadaboy}
  s.version = "0.3.0"

  s.required_rubygems_version = Gem::Requirement.new(">= 0") if s.respond_to? :required_rubygems_version=
  s.authors = ["Jell"]
  s.date = %q{2011-02-27}
  s.default_executable = %q{loadaboy}
  s.description = %q{LoadaBoy is a load testing gem.}
  s.email = %q{jean-louis@icehouse.se}
  s.executables = ["loadaboy"]
  s.extensions = ["ext/extconf.rb", "ext/extconf.rb"]
  s.extra_rdoc_files = [
    "LICENSE",
     "README.rdoc"
  ]
  s.files = [
    ".document",
     ".gitignore",
     "LICENSE",
     "README.rdoc",
     "Rakefile",
     "VERSION",
     "bin/loadaboy",
     "ebin/run_loadaboy",
     "elib/load_test.erl",
     "elib/worker.erl",
     "ext/Makefile",
     "ext/extconf.rb",
     "lib/erlang_interface.rb",
     "lib/loadaboy.rb",
     "loadaboy.gemspec",
     "test/helper.rb",
     "test/test_loadaboy.rb"
  ]
  s.homepage = %q{http://github.com/Jell/loadaboy}
  s.rdoc_options = ["--charset=UTF-8"]
  s.require_paths = ["lib"]
  s.rubygems_version = %q{1.3.7}
  s.summary = %q{LoadaBoy - Load Test Gem to the Rescue!}
  s.test_files = [
    "test/helper.rb",
     "test/test_loadaboy.rb"
  ]

  if s.respond_to? :specification_version then
    current_version = Gem::Specification::CURRENT_SPECIFICATION_VERSION
    s.specification_version = 3

    if Gem::Version.new(Gem::VERSION) >= Gem::Version.new('1.2.0') then
      s.add_development_dependency(%q<thoughtbot-shoulda>, [">= 0"])
      s.add_runtime_dependency(%q<erlectricity>, ["~> 1.1.1"])
    else
      s.add_dependency(%q<thoughtbot-shoulda>, [">= 0"])
      s.add_dependency(%q<erlectricity>, ["~> 1.1.1"])
    end
  else
    s.add_dependency(%q<thoughtbot-shoulda>, [">= 0"])
    s.add_dependency(%q<erlectricity>, ["~> 1.1.1"])
  end
end

