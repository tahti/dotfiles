require 'rake'

desc "Hook our dotfiles into system-standard positions."
task :install do
  switch_to_zsh
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
      FileUtils.rm_rf(target) if overwrite || overwrite_all || File.symlink?(target) 
      if !File.exists?("$HOME/.#{file}.backup")
       `mv "$HOME/.#{file}" "$HOME/.#{file}.backup"` if (backup || backup_all) && File.exists?(target) 
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
  if File.exists?("i3/i3.symlink/config-#{hostname}") && !File.exists?("i3/i3.symlink/config") 
  `ln -s "config-#{hostname}" "i3/i3.symlink/config"`
  end
  if !File.exists?("#{ENV["HOME"]}/.fonts")
  `fc-cache -fv`
  end
#move default directories
  run=%x(xdg-user-dirs-update --set DOWNLOADS ~/downloads)
  rename("#{ENV["HOME"]}/Downloads","#{ENV["HOME"]}/downloads")
  run=%x(xdg-user-dirs-update --set DESKTOP ~/data/desktop)
  rename("#{ENV["HOME"]}/Desktop","#{ENV["HOME"]}/data/desktop")
  run=%x(xdg-user-dirs-update --set TEMPLATES ~/data/templates)
  rename("#{ENV["HOME"]}/Templates","#{ENV["HOME"]}/data/templates")
  run=%x(xdg-user-dirs-update --set PUBLICSHARE ~/data/public)
  rename("#{ENV["HOME"]}/Public","#{ENV["HOME"]}/data/public")
  run=%x(xdg-user-dirs-update --set DOCUMENTS ~/data/doci)
  rename("#{ENV["HOME"]}/Documents","#{ENV["HOME"]}/data/doci")
  run=%x(xdg-user-dirs-update --set MUSIC ~/data/muza)
  rename("#{ENV["HOME"]}/Music","#{ENV["HOME"]}/data/muza")
  run=%x(xdg-user-dirs-update --set PICTURES ~/data/pics)
  rename("#{ENV["HOME"]}/Pictures","#{ENV["HOME"]}/data/pics")
  run=%x(xdg-user-dirs-update --set VIDEO ~/data/vids)
  rename("#{ENV["HOME"]}/Videos","#{ENV["HOME"]}/data/vids")
  install_xkb_switcher()
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

def rename(from,to)
  if !File.exists?to
    if File.exists?from
      `mv "#{from}" "#{to}"`
    else
      `mkdir "#{to}"`
    end
  end
  if File.exists?from
    FileUtils.rmdir from, :verbose => true
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

def install_xkb_switcher
  target = "#{ENV["HOME"]}/bin/xkb-switcher"
  if File.exists?(target)
    return
  end
  require 'open-uri'
  open('/tmp/master', 'wb') do |file|
    file << open('https://github.com/ierton/xkb-switch/archive/master.zip').read
  end
  system("unzip -foq /tmp/master.zip -d /tmp")
  if not (system("dpkg-query -s libxkbfile-dev"))
    system("sudo apt install libxkbfile-dev")
  end
  if not (system("dpkg-query -s cmake"))
    system("sudo apt install cmake")
  end
  if not (system("dpkg-query -s make"))
    system("sudo apt install make")
  end
  Dir.chdir("/tmp/xkb-switch-master") do
    if (system('cmake .&& make'))
      `mv xkb-switch "#{target}"`
    end
    `rm -Rf /tmp/xkb-switch-master`
    `rm -Rf /tmp/master.zip`
  end
end

