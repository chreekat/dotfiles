use strict;
use vars qw($VERSION %IRSSI);

use Irssi;

my $nick = "chreekat";
my %ignore_list = (
    snowbot => 1
);


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

    if (!exists($ignore_list{$sender}) && (!$window || $msg =~ /$nick/)) {
        $msg =~ s/^{$nick}([,:] )?//;
        $window =~ s/.+/[$&]/;
        notify("$sender $window", "$msg");
    }
}

Irssi::signal_add 'message public', 'notify_me';
Irssi::signal_add 'message private', 'notify_me';

