require 'rubygems'
require 'watir-webdriver'


def hipmunk(location_from, location_to, target_leave_date, target_return_date)
  browser = Watir::Browser.new
  browser.cookies.clear

  browser.window.resize_to(600, 1500)
  
  browser.goto("http://ifconfig.me/")
  ip = browser.strong(:id, "ip_address").text
  puts ip


  browser.goto 'http://www.hipmunk.com/'
  enter_flight_data(browser, location_from, location_to, target_leave_date, target_return_date)
  scape_data(browser)
  puts "_________________________________________________"
  browser.close
end

def enter_flight_data(browser, location_from, location_to, target_leave_date, target_return_date)
  browser.div(:class => "tab-flight").click
  browser.text_field(:id => 'date0-flight').set target_leave_date
  browser.text_field(:id=> 'date1-flight').set target_return_date

  browser.text_field(:id => 'fac1flight').set location_from
  browser.text_field(:id=> 'fac2flight').set location_to
  browser.button(:text => 'Search', :index => 2).click
end

def scape_data(browser)
  browser.element(:class => "full-name", :index => 1).when_present.hover
  (2..101).step(2) do |x|
    begin
      price = browser.element(:class => "price", :index => x).text
      browser.element(:class => "full-name", :index => x).hover
      flight = browser.element(:class => "flightnum").text
      puts Time.now.to_s + " | #{flight} | #{price}"
      browser.driver.execute_script("window.scrollBy(0,130)")
    rescue Exception => e
    end
  end
end



def tor_hipmunk(location_from, location_to, target_leave_date, target_return_date)
  profile = Selenium::WebDriver::Firefox::Profile.new
  profile['network.proxy.socks'] = 'localhost'
  profile['network.proxy.socks_port'] = 9150
  profile['network.proxy.type'] = 1
  browser = Watir::Browser.new :firefox, :profile => profile
  browser.cookies.clear

  browser.window.resize_to(600, 1500)
  browser.goto("http://www.whatsmyip.org/")
  ip = browser.span(:id, "ip").text
  puts ip

  browser.goto 'http://www.hipmunk.com/'
  enter_flight_data(browser, location_from, location_to, target_leave_date, target_return_date)
  scape_data(browser)
  puts "_________________________________________________"
  browser.close
end

#
# Call scrapers
#

puts "Firefox"
hipmunk("CHI - Chicago, IL (Area)", "NYC - New York City, NY (Area)", "Dec 19", "Dec 26")

puts "Tor"
tor_hipmunk("CHI - Chicago, IL (Area)", "NYC - New York City, NY (Area)", "Dec 19", "Dec 26")

