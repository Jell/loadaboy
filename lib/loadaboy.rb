require 'enumerator'

module LoadaBoy

  class << self
    def generator(&block)
      define_method :generate, &block
    end
  end

  protected

  def generate
    { :default => "/" }
  end

  def generate_requests(workers_count, jobs_count)
    (1..workers_count).map do |i|
      ["worker#{i}".to_sym, generate_urls(jobs_count)]
    end
  end

  def generate_urls(jobs_count)
    (1..jobs_count).inject([]){ |acc, _| acc + generate.to_a}
  end
end

