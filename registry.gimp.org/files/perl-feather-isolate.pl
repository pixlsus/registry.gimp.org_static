#!/usr/bin/perl

#     Feather/Isolate script for the GIMP
#     Copyright (C) 2007 Stephen Cavilia

#     This program is free software; you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation; either version 2 of the License, or
#     (at your option) any later version.

#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.

#     You should have received a copy of the GNU General Public License along
#     with this program; if not, write to the Free Software Foundation, Inc.,
#     51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

use strict;
use warnings;

use Gimp ":auto";
use Gimp::Fu;

sub feather_isolate
{
    my ($image, $layer, $radius) = @_;
    $image->undo_group_start();
    gimp_context_push();
    gimp_context_set_default_colors();

    my $base_sel = gimp_layer_new($image, $image->width(), $image->height(), RGBA_IMAGE, "Temporary", 100, NORMAL_MODE);
    $image->add_layer($base_sel, 0);

    $base_sel->fill(3);
    gimp_edit_bucket_fill($base_sel, 0, 0, 100, 0, 0, 0, 0);

    # grow to opaque area
    $image->selection_grow($radius);
    $image->selection_feather($radius);

    # mask
    my $mask = $layer->create_mask(1);
    $layer->add_mask($mask);

    gimp_edit_bucket_fill($mask, 1, 0, 80, 0, 0, 0, 0);

    # subtract original selection, leave blur area
    $base_sel->by_color_select(gimp_context_get_foreground(),
                               0, 1, 1, 0, 0, 0);

    # blur the selection (border)
    plug_in_gauss_iir2(1, $image, $layer, $radius/4, $radius/4);

    # restore full opacity on the old selection
    $base_sel->by_color_select(gimp_context_get_foreground(),
                               0, 2, 1, 0, 0, 0);
    gimp_edit_bucket_fill($mask, 1, 0, 100, 0, 0, 0, 0);

    $image->remove_layer($base_sel);

    gimp_context_pop();
    $image->undo_group_end();
}

register
    "perl_fu_feather_isolate",
    "Feather/Isolate (Perl)",
    "Isolate selected areas with a blurry border",
    "Stephen Cavilia",
    "copyright 2007, Stephen Cavilia",
    "2007",
    "<Image>/Filters/Decor/Feather-Isolate (Perl)",
    "RGBA",
    [
     [ PF_SLIDER, "radius", "Amount to expand selections", 16, [0, 256, 1] ]
    ],
    \&feather_isolate
    ;

exit main();
