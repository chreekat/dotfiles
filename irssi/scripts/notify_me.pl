use strict;
use vars qw($VERSION %IRSSI);

use Irssi;

use Text::Wrap;

my $nick = "chreekat";

$VERSION = '0.1';
%IRSSI = (
    authors => 'Bryan Richter',
    contact => 'bryan.richter@gmail.com',
    name => "notify_me",
    description => "Uses notify-send when things show up.",
    license => "WTFPL",
    url => "",
    changed => "14 Jan 2014",
    modules => 'Text::Wrap',
);

sub notify {
    my ($hdr, $msg) = @_;
    system( "notify-send", $hdr, $msg);
}

sub notify_me {
    my ($srv, $msg, $sender, $senderAddr, $window) = @_;

    #Irssi::print "srv: $srv\nmsg: $msg\nnick: $nick\nsenderAddr: $senderAddr\nwindow: $window\n";
    my $to;
    ($to, $msg) = split(/: /, $msg, 2);
    ($msg, $to) = ($to, $msg) unless $msg or not $to;
    #Irssi::print "msg: $msg\nto: $to\n";

    $window =~ s/.+/[$&]/;

    if (!$window or $to eq $nick) {
        notify("$sender $window", "$msg");
    }
}

Irssi::signal_add 'message public', 'notify_me';
Irssi::signal_add 'message private', 'notify_me';

