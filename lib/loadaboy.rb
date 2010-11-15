require 'enumerator'

module LoadaBoy

  class << self
    def generator(service = 'default', &block)
      define_method "generate_#{service}", &block
    end
    def set_generator(name)
      alias_method(:generate, "generate_#{name}")
    end
  end

  def generate_default
    { :default => "/" }
  end

  alias_method :generate, :generate_default

  protected

  def generate_requests(workers_count, jobs_count, service = nil)
    methods.include?("generate_#{service}") ? LoadaBoy.set_generator(service) : LoadaBoy.set_generator(:default)
    (1..workers_count).map do |i|
      ["worker#{i}".to_sym, generate_urls(jobs_count)]
    end
  end

  def generate_urls(jobs_count)
    (1..jobs_count).inject([]){ |acc, _| acc + generate.to_a}
  end
end

