require 'helper'

class TestLoadaboy < Test::Unit::TestCase
  include LoadaBoy

  context ".generator" do
    should "define a generate function running the given block" do
      LoadaBoy.generator do
        raise "block ran"
      end
      exception = assert_raise(RuntimeError){ generate_test }
      assert_match /block ran/, exception.message
    end
    should "define a generate function running the given block with proper name" do
      LoadaBoy.generator :test do
        raise "block ran"
      end
      exception = assert_raise(RuntimeError){ generate_test }
      assert_match /block ran/, exception.message
    end
  end

  context "#generate" do
    should "return a default url" do
      assert_equal({:default => "/"}, generate)
    end
  end

  context "#generate_requests" do
    should "return a list of n-workers with their url list" do
      stubs(:generate_urls).returns("url_list")
      assert_equal [[:worker1, "url_list"], [:worker2, "url_list"]], generate_requests(2, 1)
    end
    should "set proper generator" do
      LoadaBoy.generator :test do
        request :request, "custom request"
      end
      generate_requests(1, 1, :test)
      assert_equal "custom request", @urls[:request]
    end
  end

  context "#generate_urls" do
    should "return a list of n urls" do
      stubs(:generate).returns("name" => "url")
      assert_equal [["name", "url"], ["name", "url"]], generate_urls(2)
    end
  end

  context "#request" do
    should "add request to url list" do
      @urls = {}
      request :test, "test"
      assert_equal "test", @urls[:test]
    end
  end

end
