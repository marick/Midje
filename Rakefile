# -*- Mode: ruby -*-

require 'rubygems'
require 'rake'

def jar_name
    text = File.read('project.clj')
    unless /midje\s+"(\d+\.\d+\.\d+)"/ =~ text
    	"Couldn't find version in project file."
	exit 1
   end
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

desc "upload to clojars"
task :upload do
     doit("lein pom")
     doit("mv midje.jar #{jar_name} ")
     doit("scp pom.xml #{jar_name} clojars@clojars.org:")
end
