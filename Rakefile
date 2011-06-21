# -*- Mode: ruby -*-

require 'rubygems'
require 'rake'

def jar_name
  text = File.read('project.clj')
  unless /midje\s+"(\d+\.\d+\.\d+(-[A-Z]+)?)"/ =~ text ||
         /midje\s+"(\d+\.\d-alpha\d)"/ =~ text || 
         /midje\s+"(\d+\.\d-beta\d)"/ =~ text || 
    puts "Couldn't find version in project file."
    exit 1
  end
  puts "jar name: #{$1}"
   "midje-#{$1}.jar"
end

def doit(text)
    puts "== " + text
    system(text)
end

task :default => :fresh

desc "Test a fresh build, manual checking for now"
task :fresh do
     doit("lein clean")
     doit("lein jar")
     puts "bin/version and bin/run-tests"
end

task :jar_name do 
  puts jar_name
end

desc "upload to clojars"
task :upload do
  doit("lein pom")
  if File.exist?("midje.jar")
    doit("mv midje.jar #{jar_name} ")
  end
  doit("scp pom.xml #{jar_name} clojars@clojars.org:")
end
