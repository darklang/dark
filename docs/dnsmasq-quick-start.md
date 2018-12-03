# Dnsmasq quick start

If this guide isn't enough, more detailed instructions can be found [here]( https://passingcuriosity.com/2013/dnsmasq-dev-osx/).

If you didn't follow brew's post install instructions, do so now:
```
brew info dnsmasq
```

Add the following to `/usr/local/etc/dnsmasq.conf`
```
address=/localhost/127.0.0.1
```

Restart dnsmasq
```
sudo launchctl stop homebrew.mxcl.dnsmasq
sudo launchctl start homebrew.mxcl.dnsmasq
```

Configure OSX to use dnsmasq
```
sudo mkdir -p /etc/resolver
sudo tee /etc/resolver/localhost >/dev/null <<EOF
nameserver 127.0.0.1
EOF
```

Test it
```
# Make sure you haven't broken your DNS.
ping -c 1 www.google.com
# Check that .localhost names work
ping -c 1 builtwithdark.localhost
```
