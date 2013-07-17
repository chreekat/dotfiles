use strict;
use vars qw($VERSION %IRSSI);

use Irssi;

my $nick = "chreekat";

$VERSION = '0.1';
%IRSSI = (
    authors => 'Bryan Richter',
    contact => 'bryan.richter@gmail.com',
    name => "notify_me",
    description => "Uses notify-send when things show up.",
    license => "WTFPL",
    url => "",
    changed => "15 July 2013",
);

sub notify_me {
    my $msg = $_[1];
    if ($ENV{DISPLAY}) {
       if (    # my name!
               $msg =~ /$nick/i ||
               # a question (but not one directed to someone else)
               # $msg =~ /\?/ && $msg !~ /^\S*:/ ||
               # word wars for nanowrimo
               $msg =~ /word war/) {
           system "notify-send", "$_[2]:", "$msg";
        }
    }
    return;
}

sub notify_priv {
    my $data = $_[1];
    notify_me $data;
}


Irssi::signal_add 'message public', 'notify_me';
Irssi::signal_add 'event privmsg', 'notify_priv';

