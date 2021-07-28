#!/usr/bin/perl -n

# This utility filters the examples from 86asm.me and runs them through
# assembly/disasembly cycle with the real assembler and disassembler,
# making sure they're correct and up to date.
# It leaves the other lines untouched.

use strict;
use warnings;

END { unlink 'raw' }

# Assemble an instruction, with help of Open Firmware forth
sub fmas
{
	my $insn = shift;

	open my $fth, '|./forth >raw' or die $!;
	print $fth "8086 : c, emit ; fload 86asm.fth $insn";
	print $fth " 8086 = not abort\" imbalance\" bye\n";
	print $fth "1 d# 36 syscall\n"; # bad exit if we aborted above
	close $fth;
	if ($?) {
		system("cat raw");
		die;
	}
}

# Assemble an instruction, using GForth
sub gfas
{
	my $cmd = "warnings off  hex";
	$cmd .= " 8086 : stck 8086 <> abort\" imbalance\" ;";
       	$cmd .= " : c, emit ; include 86asm.fth ";
	$cmd .= shift;
	$cmd .= " stck bye";

	open my $fth, "|gforth -e '$cmd' >raw" or die $!;
	close $fth;
	die if $?;
}

# Just use GForth
sub as { gfas @_ };

# Take the machine code from 'raw' file and disassemble it
sub dis
{
	open my $dis, 'objdump -b binary -m i8086 -zD raw |' or die $!;
	my $mn = '';
	my @bin;

	while (<$dis>) {
		/^\s+[0-9a-f]+:\s+(([0-9a-f]{2} )+)\s+(.+)$/ or next;
		if ($mn) {
			warn "Extra: $3";
			$mn .= '; ';
		}
		push @bin, map { hex $_ } split /[0-9a-f]{2}\K /, $1;
		$mn .= $3;
		$mn =~ s/\s*$//;
	}

	close $dis;
	die if $?;
	return $mn, @bin;
}

sub insn
{
	as shift;	# Assemble
	dis;		# Disassemble
}

our $e;
/\.T[SE]/ and $e = 0;
/GNU Assembler/ and $e = 1;

unless ($e and /\|/ and not /^Assembled/) {
	print;
	next;
}

my @f = split /\|/;
shift @f; pop @f;
my ($mn, @bin) = insn(join "\t", @f);
$mn =~ s/\s+/ /g;

print join '|',
	join (' ', map { sprintf '%02x', $_ } @bin),
	@f, $mn;
print "\n";
