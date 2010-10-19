class Generator
  AD_SIZES = [{width: 250, height:360}, {width: 300, height:250}, {width: 336, height:280}, {width: 480, height:280}]

  def random_size
    AD_SIZES[rand(AD_SIZES.length)]
  end

  def random_string(n = 10)
    (1..n).map{ ('a'..'z').to_a[rand(26)] }.join
  end

  def random_keywords(n = 20)
    (1..n).map{random_string(5+rand(20))}.join("+")
  end

  def generate
    size = random_size
    {
      static: "/dynad/static.js",
      ad:     "/dynad/ad/load_test_#{rand(3)}/#{size[:width]}/#{size[:height]}?div_id=#{random_string}&keywords=#{random_keywords}&track_url=http://www.zombo.com/"
    }
  end
end