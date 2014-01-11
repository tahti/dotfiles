require 'rake'

desc "Hook our dotfiles into system-standard positions."
task :install do
  switch_to_zsh
  move_to_private("history")
  move_to_private("bash_history")
  move_to_private("recently-used")
  move_dir_to_private("purple")
  move_dir_to_private("mozilla")
  linkables = Dir.glob('*/**{.symlink}')

  skip_all = false
  overwrite_all = false
  backup_all = true
  hostname = `hostname`.chomp
  linkables.each do |linkable|
    overwrite = false
    backup = false

    file = linkable.split('/').last.split('.symlink').last
    target = "#{ENV["HOME"]}/.#{file}"

    if File.exists?(target) || File.symlink?(target)
      unless skip_all || overwrite_all || backup_all
        puts "File already exists: #{target}, what do you want to do? [s]kip, [S]kip all, [o]verwrite, [O]verwrite all, [b]ackup, [B]ackup all"
        case STDIN.gets.chomp
        when 'o' then overwrite = true
        when 'b' then backup = true
        when 'O' then overwrite_all = true
        when 'B' then backup_all = true
        when 'S' then skip_all = true
        when 's' then next
        end
      end
      FileUtils.rm_rf(target) if overwrite || overwrite_all
      if !File.exists?("$HOME/.#{file}.backup")
       `mv "$HOME/.#{file}" "$HOME/.#{file}.backup"` if backup || backup_all
      end
    end
    `ln -s "$PWD/#{linkable}" "#{target}"`
  end

  if File.exists?("#{ENV["HOME"]}/.lesskey")
    `lesskey`
  end

  if File.exists?("bin") && !File.exists?("#{ENV["HOME"]}/bin")
    `ln -s "$PWD/bin" "#{ENV["HOME"]}/bin"`
  end

  if !File.exists?("#{ENV["HOME"]}/usb")
    `ln -s "#{ENV["HOME"]}/usb" "/media/usb0"`
  end
#create link to configuration of i3 based on hostname 
  if File.exists?("i3/i3.symlink/config-#{hostname}")
  `ln -s "config-#{hostname}" "i3/i3.symlink/config"`
  end
  if !File.exists?("#{ENV["HOME"]}/.fonts")
  `fc-cache -fv`
  end
end

task :uninstall do

  Dir.glob('**/*.symlink').each do |linkable|

    file = linkable.split('/').last.split('.symlink').last
    target = "#{ENV["HOME"]}/.#{file}"

    # Remove all symlinks created during installation
    if File.symlink?(target)
      FileUtils.rm(target)
    end

    # Replace any backups made during installation
    if File.exists?("#{ENV["HOME"]}/.#{file}.backup")
      `mv "$HOME/.#{file}.backup" "$HOME/.#{file}"`
    end

  end
end

task :default => 'install'

def move_dir_to_private (file)
  target = "#{ENV["HOME"]}/.#{file}"
  if !File.directory?(target)
   `mkdir #{target}`
  end
  move_to_private(file)
end

def move_to_private (file)
  target = "#{ENV["HOME"]}/.#{file}"
  dest = "$HOME/Private/.#{file}"
  `touch "#{target}"`
  if !File.symlink?(target)
    if File.exists?(dest) || File.directory?(dest) 
      `mv "#{target}" "#{dest}.Backup"`
    else
      `mv "#{target}" "#{dest}"`
    end
    `ln -s "#{dest}" "#{target}"`
  end
end

def switch_to_zsh
  if ENV["SHELL"] =~ /zsh/
    puts "using zsh"
  else
    print "switch to zsh? (recommended) [ynq] "
    case $stdin.gets.chomp
    when 'y'
      puts "switching to zsh"
      system %Q{chsh -s `which zsh`}
    when 'q'
      exit
    else
      puts "skipping zsh"
    end
  end
end
