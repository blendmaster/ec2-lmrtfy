# `s/(ec2|ip)-(\d+)-(\d+)-(\d+)-(\d+).*/$1.$2.$3.$4`, for DNS

Resolve those damn ec2-style hostnames into A records, whereever you are,
even if stupid applications ([cough EMR YARN cough][0]) leave off the
`.ec2.internal` part, or you have some bizarre VPN setup that doesn't
let you use amazon's DNS to do the stupid translation for you.

[0]: https://issues.apache.org/jira/browse/HADOOP-2776

e.g. `ip-1-2-3-4.ec2.internal` will immediately resolve to an A record
for `1.2.3.4`, as will `ip-1-2-3-4`, or `ec2-1-2-3-4.compute-1.amazonaws.com`.

All other requests are proxied upstream, defined using a text file in
`resolv.conf` syntax:

    nameserver 104.219.55.89

## Usage

    ec2-lmrtfy [CONFIG_FILE]

The only argument is the path to your config file, default to "./resolv.conf`.

Since it binds to port 53, you'll probably have to run as root, or do whatever
config thing you have to do to get non-root programs to bind to ports <1024.

To actually use it, change your DNS resolver to hit 127.0.0.1. This probably
involves editing /etc/resolv.conf, at least on linux.

Note that if you run `ec2-lmrtfy /etc/resolv.conf` with 127.0.0.1, lookups will
enter an infinite loop as the program proxies to itself.

Also note that distributions like ubuntu like to run dnsmasq by default, also
on localhost, which you'll have to turn off to use this. There's probably a
nicer way to hack in an alternate resolver into `resolver(3)`, but nothing came
immediately to mind. If you know of a better way to do this, please let me
know.
