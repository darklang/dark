# Setting up Dnsmasq

Most domains within Dark work just fine. However, if you're working on
BWDServer, you may want to set up DNS so that subdomains of
builtwithdark.localhost work properly.

This is not needed for other development.

## Testing

If you have not set this up, you should see that it doesn't work

```
dig testing.builtwithdark.localhost@127.0.0.1
```

You can run this again after the installation steps below to check that it works. You might also check that you haven't broken your DNS:

```
ping -c 1 www.google.com
```

## Installation

You'll want to install dnsmasq on your machine, not in the docker container.

(This is adapted from [this guide](https://passingcuriosity.com/2013/dnsmasq-dev-osx/)

### On Mac

Install dnsmasq:

```
brew install dnsmasq
```

Follow brew's post-install instructions:

```
brew info dnsmasq
```

(probably `sudo brew services start dnsmasq`)

Add the following to `(brew --prefix)/etc/dnsmasq.conf`

```
address=/localhost/127.0.0.1
```

Restart dnsmasq:

```
sudo brew services restart dnsmasq
```

Configure OSX to use dnsmasq:

```
sudo mkdir -p /etc/resolver
sudo tee /etc/resolver/localhost >/dev/null <<EOF
nameserver 127.0.0.1
EOF
```

### On Linux

A local DNS server is needed to access the application via a `.localhost` TLD. The following is a quick start, adapted from [this guide](https://passingcuriosity.com/2013/dnsmasq-dev-osx/).

Install dnsmasq:

```
apt install dnsmasq
```

Add the following to `/etc/dnsmasq.conf`

```
address=/localhost/127.0.0.1
```

Restart dnsmasq:

```
sudo /etc/init.d/dnsmasq restart
```
