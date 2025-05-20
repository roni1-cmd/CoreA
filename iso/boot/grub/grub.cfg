#!/usr/bin/perl
# CoreA ISO Creation Script
# Builds corea.iso using GRUB and kernel binary

use strict;
use warnings;
use File::Copy;
use File::Path qw(make_path);

# Load build configuration
my $build_conf = do 'config/build.conf' or die "Cannot load build.conf: $!";

my $build_dir = $build_conf->{BUILD_DIR} // 'build';
my $kernel_binary = $build_conf->{KERNEL_BINARY} // 'kernel.bin';
my $iso_output = $build_conf->{ISO_OUTPUT} // 'corea.iso';
my $iso_dir = 'iso';
my $grub_dir = "$iso_dir/boot/grub";

# Ensure directories exist
make_path($grub_dir) or die "Cannot create $grub_dir: $!" unless -d $grub_dir;

# Copy kernel binary to iso/boot/
copy("$build_dir/$kernel_binary", "$iso_dir/boot/$kernel_binary")
    or die "Cannot copy $kernel_binary: $!";

# Create or update GRUB configuration
open my $grub_fh, '>', "$grub_dir/grub.cfg"
    or die "Cannot write grub.cfg: $!";
print $grub_fh <<'GRUB_CFG';
set timeout=5
set default=0

menuentry "CoreA OS" {
    multiboot2 /boot/kernel.bin
    boot
}
GRUB_CFG
close $grub_fh;

# Create ISO using grub-mkrescue
system("grub-mkrescue -o $build_dir/$iso_output $iso_dir")
    and die "Failed to create $iso_output: $!";

print "ISO created successfully: $build_dir/$iso_output\n";
