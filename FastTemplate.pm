
##################################################
##
## Name: CGI::FastTemplate
##
##        Copyright (c) 1998 Jason Moore <jmoore@sober.com>.  All rights
##        reserved.
##
##        This program is free software; you can redistribute it and/or
##        modify it under the same terms as Perl itself.
##
##        This program is distributed in the hope that it will be useful,
##        but WITHOUT ANY WARRANTY; without even the implied warranty of
##        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##        Artistic License for more details.
##
##
## Credits:
##        - fancy regexp taken from article by Brian Slesinsky <bslesins@best.com>
##        http://www.hotwired.com/webmonkey/code/97/21/index2a_page4.html?tw=perl
## 
## Documentation:
##        See 
##        'perldoc CGI::FastTemplate'
##        or
##        'perldoc ./FastTemplate'
##
## History:
##        See 'Changes'
##
##
## TODO:
##     - update 'Changes' 
##     - update pod doco
## 
##################################################

package CGI::FastTemplate;

use strict;

$CGI::FastTemplate::VERSION	= '1.02';
$CGI::FastTemplate::ROOT	= undef; 

##################################################
##
sub new
##
##   - instantiates FastTemplate
##
{
	my($class,$root) = @_;
	my $self = {};
	bless $self, $class;

	$self->init;

	$self->{strict}	= 1;

	if (defined($root))
	{
		$self->set_root($root);
	}
	return($self);
}

##################################################
##
sub strict
##
{
	my($self) = shift;
	$self->{strict} = 1;
}

##################################################
##
sub no_strict
##
{
	my($self) = shift;
	undef $self->{strict};
}

##################################################
##
sub clear_all
##
##   - initializes (or clears!) variables
##
{
	my($self) = shift;

	if (!ref($self))
	{
		print STDERR "FastTemplate: Unable to call init without instance.\n";
		return();
	}

	$self->{namespace}	= {};	## main hash where we resolve variables 
	$self->{namespaces}	= [];	## array of hash refs 

	$self->{last_parse}	= undef;	## remember where we stored the last parse so print()
						## will have a default

	$self->{template_name}	= {};	## template name: template file
	$self->{template_data}	= {};	## template name: template content/data 
}
*init = \&clear_all;		## alias to 'clear' : 'init' 

##################################################
##
sub clear_define
##
##   - clears values entered with define() 
##
{
	my($self) = shift;
	$self->{template_name}	= {};
}

##################################################
##
sub clear_tpl
##
##   - clears hash that holds loaded templates. 
##
{
	my($self) = shift;
	$self->{template_data}	= {};
}


##################################################
##
sub clear_href
##
##   - removes from the end, a given number of hash references
##     from the namespace list.
##
##   - 1: number of hash references to erase
##
{
	my($self, $number) = @_;

	if (!defined($number))
	{
		$self->{namespaces} = [];
		return(1);	
	}

	for (1..$number)
	{
		pop(@{$self->{namespaces}});	## toss it away
	}

	return(1);
}

##################################################
##
sub clear_parse
##
##   - clears hash which holds parsed variables
##   - if called with a scalar, only clears that key/element in the namespace.
##     so, $tpl->clear("ROWS") which is almost the same as,
##     $tpl->assign(ROWS => "");
##
{
	my($self, $key) = @_;
	if (!defined($key) || $key eq "")
	{ 
		$self->{namespace}	= {};		## main hash where we resolve variables 
		$self->{last_parse}	= undef;	## remember where we stored the last parse so print()
		return(1);
	}

	delete(${$self->{namespace}}{$key});
	return(1);	
}

*clear = \&clear_parse;				## alias clear -> clear_parse 

##################################################
##
sub set_root
##
##   - sets template root directory.
{
	my($self, $root) = @_;

	## set object default root directory

	$CGI::FastTemplate::ROOT = $root;

	if (ref($self))
	{
		## set instance template dir
		$self->{ROOT} = $root;
	}
	return(1);
}

##################################################
##
sub define
##
##   - sets alias/name to associate with template filenames
##   - note: names are relative to ROOT directory (set with set_root)
##   - e.g. the following works
##       $tpl->set_root("/tmp/docs");
##       $tpl->define( main => "../dev_docs");
##       (assuming you have templates in /tmp/dev_docs)
##
##   - files are not loaded until used, so go nuts when defining.  each line
##     only costs a wee bit of memory and compile time.
##
##   - note: define is cumulative
##
{
	my($self, %define) = @_;

	for (keys(%define))
	{
		$self->{template_name}{$_} = $define{$_};
	}

	return(1);
}

##################################################
##
sub assign
##
##   - assigns values of a HASH directly to internal namespace
##     HASH
##
##   Args:
##   - 1: hash reference (to add to array of namespaces)
##   - 1: hash (to merge with main namespace hash)
##
##   - returns: 1 on success
##
{
	my $self = shift;
	
	if (ref($_[0]) eq "HASH")
	{
		push(@{$self->{namespaces}}, $_[0]);
		return(1);	
	}
	
	my %assign = @_;

	my($name,$value);
	while ( ($name,$value) = each(%assign) )
	{
		$self->{namespace}{$name} = $value;
	}

	return(1);
}


##################################################
##
sub parse
##
##   - parses a scalar to resolve/interpolate any variables
##     it finds.
##
##   - 1: hash of what we are parse in TARGET:SOURCE form
##        NOTE: SOURCE with a "." as the first character get appended
##        to existing TARGET
##
{
	my($self, %parse) = @_;

	my $target;
	for $target (keys(%parse))
	{
		##
		## make all sources an array...
		##

		if (ref($parse{$target}) ne "ARRAY")
		{
			$parse{$target} = [$parse{$target}];
		}

		my($p, $append);

		for $p (@{$parse{$target}})
		{
			if (substr($p,0,1) eq ".")	## detect append
			{
				$append = 1;
				$p = substr($p, 1);
			}	

			if (!exists($self->{template_name}{$p}))
			{
				print STDERR "FastTemplate: Template alias: $p does not exist.\n";		
				next;
			}

			## load template if we need to

			if (!exists($self->{template_data}{$p}))
			{
				# Log->debug("$p is not loaded. slurping.");
				$self->slurp($self->{template_name}{$p}, \$self->{template_data}{$p} );
			}

			## copy SOURCE (template_data) to temp variable
			## (can't use namespace, since we might be appending to it.)
		
			my $temp_parse = $self->{template_data}{$p};
	
			## parse

			$temp_parse =~ s/\$([A-Z][A-Z0-9_]+)/

				my $v = $self->{namespace}{$1};

				if (!defined($v))
				{
					## look in array of hash refs for value of variable
					my $r;					
					for $r (@{$self->{namespaces}})
					{
						if (exists($$r{$1}))	## found it
						{
							$v = $$r{$1};
							last;
						}
					}
				}
				if (!defined($v))		## $v should be empty not undef, to prevent
				{				## warnings under -w
					print STDERR "[CGI::FastTemplate] Warning: no value found for variable: $1\n" 
						if ($self->{strict});
					$v = "";
				}	
				$v;
				/ge;

			$self->{last_parse} = $target;
			## assign temp to final TARGET

			# Log->debug("temp_parse: $temp_parse");

			if ($append)
			{
				$self->{namespace}{$target} .= $temp_parse;
			}
			else
			{
				$self->{namespace}{$target} = $temp_parse;
			}
		}

	}
}

##################################################
##
sub slurp
##
##  - slurps (loads) in file into a scalar.  
##  - cool trick to undef the end of line character
##    grabbed from some usenet posting.  (don't remember)
## 
##  - i think the maximum file size is (2**32-1) approx. 2 megs.
##
##  - 1: filename (minus path)
##  - 2: reference to put result in [optional]
##  returns: scalar
##
##
{
	my($self, $filename, $ref) = @_;
	my($temp);

	if (defined($CGI::FastTemplate::ROOT))
	{
		$filename = $CGI::FastTemplate::ROOT . "/" . $filename;
	} 

	if (!open(TEMPLATE, $filename))
	{
		print STDERR "FastTemplate: slurp: cannot open: $filename ($!)";
		return();
	}

	## cool trick!

	local($/) = undef;
	$temp = <TEMPLATE>;
	close(TEMPLATE);

	if (defined($ref))		## fill reference 
	{
		# Log->debug("ref is defined: $ref");
		$$ref = $temp;
		return(1);
	}

	return($temp);
}


##################################################
##
sub define_nofile
##
## - allows caller to bypass storing templates in files and
##   using define() to map aliased to the file
##
## 1: hash (or hash ref) {template name => raw template data} 
##        e.g. $raw_tpl = 'Hello $NAME.';
##
##	define_nofile(greeting	=> $raw_tpl);
##
## Note: single ticks (literal) in the above example are required when
##       constructing templates to prevent the variables from being 
##       evalualted/interpolated _before_ being passed into the templating
##       module. 
##
## returns: 1 on success, undef on failure
##
{
	my($self) = shift;

	my $href;

	if (ref($_[0]) eq "HASH")
	{
		$href = $_[0]; 			
	}
	else
	{
		my %h = @_;
		$href = \%h;	
	}
	
	my $k;
	for $k (keys(%$href))
	{
		$self->{template_name}{$k} = 1;			## exists will now be true (loading skipped)
		$self->{template_data}{$k} = $$href{$k};	## 
	}
	return(1);
}

*define_raw = \&define_nofile;

##################################################
##
sub print
##
##   - calls built in perl function "print" on given
##     hash key.
##
{
	my($self, $var) = @_;

	if (!defined($var))
	{
		if (!defined($self->{last_parse}))
		{
			print STDERR "FastTemplate: Nothing has been parsed.  Nothing to print.\n";
			return();
		}
		print $self->{namespace}{$self->{last_parse}};
		return(1);
	}

	print $self->{namespace}{$var};
	return(1);
}

1;

##################################################
##
sub fetch
##
##   - returns a scalar ref to a value in the namespace
##
##   - 1: value to fetch
##
##   - returns: scalar ref
##
{
	my($self, $what) = @_;

	if (!exists($self->{namespace}{$what}))
	{
		print STDERR "Unable to fetch $what from FastTemplate object.  Doesn't exist.\n"; 
		return();
	}

	return( \$self->{namespace}{$what} );
}

##################################################
##
sub DESTROY
##
##   - null function - prevents Apache::Registry (mod_perl)
##     for looking for and not finding DESTROY method.
{}



=head1 NAME

CGI::FastTemplate - Perl extension for managing templates, and performing variable interpolation.


=head1 SYNOPSIS

  use CGI::FastTemplate;

  $tpl = new CGI::FastTemplate();
  $tpl = new CGI::FastTemplate("/path/to/templates");

  CGI::FastTemplate->set_root("/path/to/templates");	## all instance will use this path
  $tpl->set_root("/path/to/templates");			## this one instance will use this path

  $tpl->define(		main	=> "main.tpl",
			row	=> "table_row.tpl",
			all	=> "table_all.tpl",
		);

  $tpl->assign(	TITLE	=> "I am the title.");

  my %defaults = (	FONT	=> "<font size=+2 face=helvetica>",
			EMAIL	=> 'jmoore@sober.com',
			);	
  $tpl->assign(\%defaults);


  $tpl->parse(ROWS	=> ".row");		## the '.' appends to ROWS
  $tpl->parse(CONTENT	=> ["row", "all"]);
  $tpl->parse(CONTENT	=> "main");

  $tpl->print();		## defaults to last parsed
  $tpl->print("CONTENT");	## same as print() as "CONTENT" was last parsed

  $ref = $tpl->fetch("CONTENT");


=head1 DESCRIPTION

B<What is a template?>

A template is a text file with variables in it.  When a template is parsed, the 
variables are interpolated to text.  (The text can be a few bytes or a few hundred kilobytes.)
Here is a simple template with one variable ('$NAME'):

    Hello $NAME.  How are you?
	

 
B<When are templates useful?>

Templates are very useful for CGI programming, because adding HTML to your perl code clutters your 
code and forces you to do any HTML modifications.  By putting all of your HTML in separate template
files, you can let a graphic or interface designer change the look of your application without having
to bug you, or let them muck around in your perl code.

B<There are other templating modules on CPAN, what makes FastTemplate different?>

B<Speed>

FastTemplate doesn't use eval, and parses with a single regular expression.  It just
does simple variable interpolation (i.e. there is no logic that you can add to templates - you keep the 
logic in the code).  That's why it's has 'Fast' in it's name!

B<Efficiency>

FastTemplate functions accept and return references whenever possible, which saves
needless copying of arguments (hashes, scalars, etc).

B<Flexibility>

The API is robust and flexible, and allows you to build very complex HTML documents/interfaces.
It is also completely written in perl and works on Unix or NT.  Also, it isn't restricted to building 
HTML documents -- it could be used to build any ascii based document (e.g. postscript, XML, email).

The similar modules on CPAN are: 

    Module          Taco::Template  (KWILLIAMS/Taco-0.04.tar.gz)
    Module          Text::Template  (MJD/Text-Template-0.1b.tar.gz)


B<What are the steps to use FastTemplate?>

The main steps are:

    1. define
    2. assign 
    3. parse
    4. print

These are outlined in detail in CORE METHODS below.

=head1 CORE METHODS

=head2 define(HASH)

The method define() maps a template filename to a (usually shorter) name. e.g.

    my $tpl = new FastTemplate();
    $tpl->define(main	=> "main.tpl",
                 footer	=> "footer.tpl",
			);

This new name is the name that you will use to refer to the templates.  Filenames
should not appear in any place other than a define().

(Note: This is a required step!  This may seem like an annoying extra
step when you are dealing with a trivial example like the one above,
but when you are dealing with dozens of templates, it is very handy to
refer to templates with names that are indepandant of filenames.)

TIP: Since define() does not actually load the templates, it is faster and more legible to define all the templates 
with one call to define().

=head2 define_nofile(HASH)   alias: define_raw(HASH)

Sometimes it is desireable to not have to create a separate template file
for each template (though in the long run it is usually better to do so).
The method define_nofile() allows you to do this.  For example, if you
were writing a news tool where you wanted to bold an item if it was
"new" you could do something like the following:

    my $tpl = new FastTemplate();

    $tpl->define_nofile(	new	=> '<b>$ITEM_NAME</b> <BR>',
                 		old	=> '$ITEM_NAME <BR>');

    if ($new)
    {
	$tpl->parse($ITEM	=> "new");
    }
    else
    {
	$tpl->parse($ITEM	=> "old");
    }

Of course, now you, the programmer has to update how new items are displayed, whereas if it was in a template, you
could offload that task to someone else.


=head2 define_nofile(HASH REF)	alias: define_raw(HASH REF)

A more efficient way of passing your arguments than using a real hash.  Just pass in a hash reference
instead of a real hash. 


=head2 assign(HASH) 

The method assign() assigns values for variables.  In order for a variable in a template
to be interpolated it must be assigned.  There are two forms which have some important differences.
The simple form, is to accept a hash and copy all the key/value pairs into a hash in FastTemplate.  
There is only one hash in FastTemplate, so assigning a value for the same key will overwrite that
key.

    e.g.

    $tpl->assign(TITLE	=> "king kong");
    $tpl->assign(TITLE	=> "godzilla");    ## overwrites "king kong"

=head2 assign(HASH REF)

A much more efficient way to pass in values is to pass in a hash reference.  (This is
particularly nice if you get back a hash or hash reference from a database query.)  Passing
a hash reference doesn't copy the data, but simply keeps the reference in an array.  During parsing
if the value for a variable cannot be found in the main FastTemplate hash, it starts to look through
the array of hash references for the value.  As soon as it finds the value it stops.  It is important
to remember to remove hash references when they are no longer needed.

    e.g.

    my %foo = ("TITLE" => "king kong");
    my %bar = ("TITLE" => "godzilla");

    $tpl->assign(\%foo);	## TITLE resolves to "king kong"
    $tpl->clear_href(1);	## remove last hash ref assignment (\%foo)
    $tpl->assign(\%bar);	## TITLE resolves to "godzilla"

    $tpl->clear_href();		## remove all hash ref assignments

    $tpl->assign(\%foo);	## TITLE resolves to "king kong"
    $tpl->assign(\%bar);	## TITLE _still_ resolves to "king kong"


=head2 parse(HASH)

The parse function is the main function in FastTemplate.  It accepts a hash, where the keys are
the TARGET and the values are the SOURCE templates.  There are three forms the hash can be 
in:

    $tpl->parse(MAIN => "main");		## regular

    $tpl->parse(MAIN => ["table", "main"]);	## compound

    $tpl->parse(MAIN => ".row");		## append

In the regular version, the template named "main" is loaded if it hasn't been already, all the variables
are interpolated, and the result is then stored in FastTemplate as the value MAIN.  If the variable 
'$MAIN' shows up in a later template, it will be interpolated to be the value of the parsed "main" template. 
This allows you to easily nest templates, which brings us to the compound style.  

The compound style is designed to make it easier to nest templates.  The following are equivalent:

    $tpl->parse(MAIN => "footer");		
    $tpl->parse(MAIN => "main");

    ## is the same as:

    $tpl->parse(MAIN => ["table", "main"]);	## this form saves function calls
						## (and makes your code cleaner)

It is important to note that when you are using the compound form, each template after the first, 
must contain the variable that you are parsing the results into.  In the above example, 'main' must 
contain the variable '$MAIN', as that is where the parsed results of 'table' is stored.  If 'main'
does not contain the variable '$MAIN' then the parsed results of 'table' will be lost.

The append style is a bit of a kludge, but it allows you to append the parsed results to the target variable. 
This is most useful when building tables that have an dynamic number of rows - such as data from a database
query.

=head2 strict()

When strict() is on (it is on by default) all variables found during template parsing that are unresolved
have a warning printed to STDERR.  e.g. 

	[CGI::FastTemplate] Warning: no value found for variable: SOME_VARIABLE

Note: STDERR output should be captured and logged by the webserver.  e.g. With apache you can tail the error log 
during development to see the results. e.g. 

	tail -f /etc/httpd/logs/error_log

=head2 no_strict()

Turns off warning messages about unresolved template variables.  This must be set for each instance of CGI::FastTemplate. e.g.

	CGI::FastTemplate->no_strict;		## no 
	
	my $tpl = CGI::FastTemplate;
	$tpl->no_strict;			## yes
 


=head2 print(SCALAR)

The method print() prints the contents of the named variable.  If no variable is given, then it prints
the last variable that was used in a call to parse which I find is a reasonable default.  e.g.

    $tpl->print();         ## continuing from the last example, would print the value of MAIN
    $tpl->print("MAIN");   ## ditto

This method is provided for convenience.  If you need to print somewhere else (e.g. socket, file handle)
you would want to fetch() a reference to the data first. e.g.

    my $data_ref = $tpl->fetch("MAIN");
    print FILE $$data_ref;			## save to a file

=head1 OTHER METHODS

=head2 fetch(SCALAR)

Returns a scalar reference to parsed data.  

    $tpl->parse(CONTENT	=> "main");
    my $content = $tpl->fetch("CONTENT");	

    print $$content;		## print to STDOUT
    print FILE $$content;	## print to filehandle or pipe


=head2 clear()

Note: All of 'clear' functions are for use under mod_perl (or anywhere where your scripts
are persistant).  They generally aren't needed if you are writing CGI scripts.

Clears the internal hash that stores data passed to:

    $tpl->parse();

Often clear() is at the end of a mod_perl script:

    $tpl->print();
    $tpl->clear();

=head2 clear_parse()

See: clear()

=head2 clear_href(NUMBER)

Removes a given number of hash references from the list of hash refs that is built using:

    $tpl->assign(HASH REF);

If called with no arguments, it removes all hash references
from the array.


=head2 clear_define()

Clears the internal hash that stores data passed to:

    $tpl->define();

Note: The hash that holds the loaded templates is not touched with this method.  See: clear_tpl


=head2 clear_tpl()

Clears the internal hash that stores the contents of the templates.  If you are
having problems with template changes not being reflected, try adding this 
method to your script.

=head2 clear_all()

Cleans the module of any data, except for the ROOT directory.  Equivalent to:

    $tpl->clear_define();
    $tpl->clear_href();
    $tpl->clear_tpl();
    $tpl->clear_parse();

=head2 Variables

A variable is defined as:

    $[A-Z0-9][A-Z0-9_]+

This means, that a variable must begin with a dollar sign '$'.  The second character
must be an uppercase letter or digit 'A-Z0-9'.  Remaining characters can include an underscore.

For example, the following are valid variables:

    $FOO
    $F123F
    $TOP_OF_PAGE

=head2 Variable Interpolation (Template Parsing)

When the a template is being scanned for variables, pattern matching is greedy. (For more info on
"greediness" of regexps see L<perlre>.)  This is important, because if there are valid variable
characters after your variable, FastTemplate will consider them to be part of the variable.  The only
way that you can indicate the end of your variable name is to have a character that is not an uppercase 
letter, digit or underscore.  ['A-Z0-9_']

If a variable cannot be resolved to anything, it is converted to an empty string [""].  
Also, a warning is printed to STDERR.  See L<strict()> and L<no_strict()> for more info.

Some examples will make this clearer.

    Assume:

    $FOO = "foo";
    $BAR = "bar";
    $ONE = "1";
    $TWO = "2";	
    $UND = "_";
	
    Variable	Interpolated/Parsed
    ------------------------------------------------
    $FOO		foo	
    $FOO-$BAR	foo-bar
    $ONE_$TWO	_2			## $ONE_ is undefined!	
    $ONE$UND$TWO	1_2
    $$FOO		$foo
    $25,000		$25,000



=head2 FULL EXAMPLE

This example will build an HTML page that will consist of a table.  The table will have 3 numbered rows.
The first step is to decide what templates we need.  In order to make it easy for the table to change to a different
number of rows, we will have a template for the rows of the table, another for the table, and a third for the head/body
part of the HTML page.

Below are the templates. (Pretend each one is in a separate file.) 


  <!-- NAME: main.tpl -->
  <html>
  <head><title>$TITLE</title>
  </head>
  <body>
  $MAIN
  </body>
  </html>
  <!-- END: main.tpl -->
 
 
  <!-- NAME: table.tpl -->
  <table>
  $ROWS
  </table>
  <!-- END: table.tpl -->
 
 
  <!-- NAME: row.tpl -->
  <tr>
  <td>$NUMBER</td>
  <td>$BIG_NUMBER</td>
  </tr>
  <!-- END: row.tpl -->

Now we can start coding...

 ## START ##

  use CGI::FastTemplate;
  my $tpl = new CGI::FastTemplate("/path/to/template/files");
 
  $tpl->define(	main	=> "main.tpl",
		table	=> "table.tpl",
		row	=> "row.tpl",
		);

  $tpl->assign(	TITLE	=> "FastTemplate Test");

  for $n (1..3) {
	$tpl->assign(	NUMBER		=> $n,	
			BIG_NUMBER	=> $n*10);
	$tpl->parse(ROWS	=> ".row"); 
  }

  $tpl->parse(MAIN	=> ["table", "main"]); 
  $tpl->print();

  ## END ##
 
  When run it returns:

  <!-- NAME: main.tpl -->
  <html>
  <head><title>FastTemplate Test</title>
  </head>
  <body>
  <!-- NAME: table.tpl -->
  <table>
  <!-- NAME: row.tpl -->
  <tr>
  <td>1</td>
  <td>10</td>
  </tr>
  <!-- END: row.tpl -->
  <!-- NAME: row.tpl -->
  <tr>
  <td>2</td>
  <td>20</td>
  </tr>
  <!-- END: row.tpl -->
  <!-- NAME: row.tpl -->
  <tr>
  <td>3</td>
  <td>30</td>
  </tr>
  <!-- END: row.tpl -->
  
  </table>
  <!-- END: table.tpl -->

  </body>
  </html>
  <!-- END: main.tpl -->


If you're thinking you could have done the same thing in a few lines
of plain perl, well yes you probably could.  But, how would a graphic
designer tweak the resulting HTML?  How would you have a designer editing
the HTML while you're editing another part of the code?  How would
you save the output to a file, or pipe it to another application
(e.g. sendmail)?  How would you make your application multi-lingual?
How would you build an application that has options for high graphics,
or text-only?  FastTemplate really starts to shine when you are building
mid to large scale web applications, simply because it begins to separate
the application's generic logic from the specific implementation.


=head1 AUTHOR

Jason Moore <jmoore@sober.com>

=head1 SEE ALSO

mod_perl(1).

=cut











