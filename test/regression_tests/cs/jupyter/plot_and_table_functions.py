import matplotlib as mpl
import matplotlib.pyplot as plt
import matplotlib.colors as colors
from matplotlib.ticker import FixedLocator, FixedFormatter, FormatStrFormatter
from matplotlib.colors import ListedColormap
from matplotlib.colors import LinearSegmentedColormap
from matplotlib import gridspec
import pandas as pd
import numpy as np
from IPython.display import display

def discrete_cmap(N, base_cmap=None):
    """
    Create an N-bin discrete colormap from the specified input map.
    Based on code from https://gist.github.com/jakevdp/91077b0cae40f8f8244a
    """
    base = plt.cm.get_cmap(base_cmap)
    color_list = base(np.linspace(0, 1, N))
    cmap_name = base.name + str(N)
    return LinearSegmentedColormap.from_list(cmap_name, color_list, N)

def discrete_random_cmap(N, base_cmap=None):
    """
    Create an N-bin discrete colormap from the specified input map.
    Selects color indices at random.
    Based on code from https://gist.github.com/jakevdp/91077b0cae40f8f8244a
    """
    base = plt.cm.get_cmap(base_cmap)
    color_list = base(np.random.uniform(size=N))
    cmap_name = base.name + str(N)
    return LinearSegmentedColormap.from_list(cmap_name, color_list, N)

def discrete_irreg_cmap(discrete_vals, base_cmap=None):
    base = plt.cm.get_cmap(base_cmap)
    normalized_vals = discrete_vals / np.max( discrete_vals )
    color_list = base(normalized_vals)
    cmap_name = base.name + "discrete"
    return LinearSegmentedColormap.from_list(cmap_name, color_list, len( discrete_vals ) )

def make_side_by_side_plots( var1, var2, title1='Title 1', title2='Title 2', barlabel='Bar label',
                             cmap='jet', each=False, figsize_x=16, figsize_y=8,
                             minz=-1.0e-12, maxz=-1.0e-12, n=7, x=(0,1), y=(0,1) ):

    # see: http://stackoverflow.com/questions/10388462/matplotlib-different-size-subplots
    # for a nice treatment of multiple plots with varying widths

    plt.figure(num=None, figsize=(figsize_x, figsize_y), dpi=200, facecolor='w', edgecolor='k')

    # create a "grid" of plots: 1 row, by 2 columns wide
    gs = gridspec.GridSpec(1,2,width_ratios=[4,5])

    ax0=plt.subplot(gs[0])
    ax1=plt.subplot(gs[1])

    if ( minz < 1.0e-11 ):
        minz = np.nanmin(var1)

    if ( maxz < 1.0e-11 ):
        maxz = np.nanmax(var1)

    vals=np.linspace(minz, maxz, n, endpoint=True )
    plot0 = ax0.imshow( var1, interpolation='nearest', cmap=cmap, extent=(np.nanmin(x),
                                    np.nanmax(x), np.nanmin(y), np.nanmax(y)),
                                    vmin=minz, vmax=maxz )
    #ax.  .clim(minz, maxz)

    ax0.set_title(title1)
    plot1 = ax1.imshow( var2, interpolation='nearest', cmap=cmap, extent=(np.nanmin(x),
                                np.nanmax(x), np.nanmin(y), np.nanmax(y)),
                                vmin=minz, vmax=maxz )
    ax1.set_title(title2)
    bar1 = plt.colorbar( plot1 )
    bar1ax = bar1.ax
    bar1ax.text(2.1,0.5,barlabel,rotation=90,size=15)

    plt.show()

def make_plot( var, title='', barlabel='', cmap='jet', discrete=False, minz=-1.0e+12, maxz=-1.0e+12, n=7,
              tick_factor=1.0, tick_offset=0.0, x=(0,1), y=(0,1) ):
    plt.figure(num=None, figsize=(16, 9), dpi=200, facecolor='w', edgecolor='k')
    plt.title( title )

    if ( minz < -1.0e+11 ):
        minz = np.nanmin(var)

    if ( maxz < -1.0e+11 ):
        maxz = np.nanmax(var)

    if ( discrete ):
        vals=np.unique( var.flatten() )
        #myplot = plt.contourf(x, y, var, vals, cmap=cmap)
        myplot = plt.imshow( var, interpolation='nearest', cmap=cmap, extent=(np.nanmin(x),
                                    np.nanmax(x), np.nanmin(y), np.nanmax(y)),
                                    vmin=0.0, vmax=1.0 )
        delta = np.max( vals ) - np.min( vals )
        inc = delta / len( vals )
        #tickvals = range( int( np.min( vals)) , int( np.max( vals )) , int( inc ) )
        adj_tickvals = [tick_factor * i + tick_offset for i in vals ]
        mybar = plt.colorbar( myplot, ticks= vals )
        myax = mybar.ax
        myax.set_yticklabels( vals )

    else:
        vals=np.linspace(minz, maxz, n, endpoint=True )
        #myplot = plt.contourf(x, y, var, vals, cmap=cmap)
        myplot = plt.imshow( var, interpolation='nearest', cmap=cmap, extent=(np.nanmin(x),
                                    np.nanmax(x), np.nanmin(y), np.nanmax(y)) )
        mybar = plt.colorbar( myplot )
        myax = mybar.ax


    plt.clim(minz, maxz)
    myax.text(2.1,0.5,barlabel,rotation=90,size=15)
    plt.show()

def make_diffplot( var, title, barlabel, cmap='seismic', minz=-36., maxz=36., n=10, x=(0,1), y=(0,1) ):
    plt.figure(num=None, figsize=(16, 9), dpi=200, facecolor='w', edgecolor='k' )
    plt.title( title )
    vals=np.linspace(minz, maxz, n, endpoint=True)
#    myplot = plt.contourf(x, y, var, vals, cmap=cmap)
    myplot = plt.imshow( var, interpolation='nearest', cmap=cmap, extent=(np.nanmin(x),
                            np.nanmax(x), np.nanmin(y), np.nanmax(y)) )
    #mybar = plt.colorbar( myplot, ticks=vals)
    plt.clim(minz, maxz)
    mybar = plt.colorbar( myplot)
    myax = mybar.ax
    myax.text(3.,.7,barlabel,rotation=90,size=15)
    plt.show()

def make_scatter( x, y, color, cmap, barlabel='none', title='none', xlab='none', ylab='none',
    xmax=1.0e-36, ymax=1.0e-36, xmin=1.0e-36, ymin=1.0e-36 ):
    if xmax < 1.0e-35:
        xmax=np.nanmax(x)
    if ymax < 1.0e-35:
        ymax=np.nanmax(y)
    if xmin < 1.0e-35:
        xmin=np.nanmin(x)
    if ymin < 1.0e-35:
        ymin=np.nanmin(y)

    plot_minmaxvals = [xmin, ymin, xmax, ymax]

    fig1 = plt.figure(figsize=[15,14])
    gs = plt.GridSpec(100,100,bottom=0.18,left=0.18,right=0.88)

    ax1         = fig1.add_subplot(gs[:,:90])
    ax_colorbar = fig1.add_subplot(gs[:,95:])

    tick_values=np.unique( color )
    mincol = np.nanmin(tick_values)
    maxcol = np.nanmax(tick_values)
    tick_labels=tick_values.tolist()
    n=len(tick_labels)

    ax1.set_title( title )
    ax1.set_xlabel( xlab )
    ax1.set_ylabel( ylab )
    ax1.set_xlim( ( xmin, xmax ) )
    ax1.set_ylim( ( ymin, ymax ) )

    p1 = ax1.scatter(x=x, y=y, c=color , cmap=cmap, edgecolors='none' )

    # plot 1:1 line
    p2 = ax1.plot([xmin,xmax], [ymin,xmax], "r--" )

    norm = mpl.colors.BoundaryNorm(tick_labels, cmap.N)
    cb2 = mpl.colorbar.ColorbarBase(ax=ax_colorbar,
                                    cmap=cmap,
                                    norm=norm,
                                    boundaries=[0] + tick_labels + [tick_labels[-1]+1],
                                    extend='neither',
                                    ticks=tick_labels,
                                    spacing='uniform',
                                    orientation='vertical')

#    tick_locs = tick_labels * 0.96 + 0.5
    # tick_locs = np.linspace( mincol, maxcol, n ) * 0.97 + 3.
    #
    # cbar = fig1.colorbar(p1, ax=ax1, cax=axC_colorbar)
    #
    # cbar.locator = FixedLocator(tick_locs)
    # cbar.formatstrformatter = FormatStrFormatter('%.f')
    # cbar.formatter = FixedFormatter(tick_labels)
    # cbar.update_ticks()
    #
    ax_colorbar.text(2.1,0.5,barlabel,rotation=90,size=15)

    plt.show()


#
# bounds = [1, 2, 4, 7, 8]
# norm = mpl.colors.BoundaryNorm(bounds, cmap.N)
# cb2 = mpl.colorbar.ColorbarBase(ax, cmap=cmap,
#                                 norm=norm,
#                                 boundaries=[0] + bounds + [13],
#                                 extend='both',
#                                 ticks=bounds,
#                                 spacing='proportional',
#                                 orientation='horizontal')
# cb2.set_label('Discrete intervals, some other units')

# from gist: https://gist.github.com/jakevdp/8a992f606899ac24b711
# "Categorical CMAP"

# plt.scatter(iris.data[:, 0], iris.data[:, 1], s=30,
#             c=iris.target, cmap=plt.cm.get_cmap('Greens', 3))
#
# # This function formatter will replace integers with target names
# formatter = plt.FuncFormatter(lambda val, loc: iris.target_names[val])
#
# # We must be sure to specify the ticks matching our target names
# plt.colorbar(ticks=[0, 1, 2], format=formatter);
#
# # Set the clim so that labels are centered on each block
# plt.clim(-0.5, 2.5)


def make_comparison_table( x, y, factor, description, lookup_vals, xlab, ylab, calc_difference=True ):
    count_x=len(x.flatten())
    count_y=len(y.flatten())

    xmean = []
    ymean = []
    lu = []
    desc = []
    count = []
    modeldiff = []

    for index in range( len(lookup_vals) ):

        x_subset = x[ factor == lookup_vals[index] ]
        y_subset = y[ factor == lookup_vals[index] ]
        count_x=len(x_subset)
        count_y=len(y_subset)
        if count_x > 0:
            x_mean = np.nanmean(x_subset)
        else:
            x_mean = np.nan

        if count_y > 0:
            y_mean = np.nanmean(y_subset)
        else:
            y_mean = np.nan

        if x_mean is not np.nan and y_mean is not np.nan:
            mean_both = np.nanmean((x_mean, y_mean ))
            abs_diff = (x_mean - y_mean )
            xmean.append( x_mean )
            ymean.append( y_mean )
            lu.append( lookup_vals[ index ] )
            count.append( count_x )
            desc.append( description[ index ] )
            modeldiff.append( abs_diff )
            if count_x == count_y:
              print('LU: ' + description[ index ] + ' (land-use code ' + str(lookup_vals[index])
                + ', n=' + str(count_x) + ')')
              plt.plot(x[ factor == lookup_vals[index]], y[ factor == lookup_vals[index]],"bo")
              xmin = np.nanmin(x_subset)
              xmax = np.nanmax(x_subset)
              ymin = np.nanmin(y_subset)
              ymax = np.nanmax(y_subset)
              mincoord = np.nanmin([xmin, ymin])
              maxcoord = np.nanmax([xmax, ymax])
              plt.plot([mincoord,maxcoord], [mincoord,maxcoord], "r--" )
              plt.xlabel(xlab)
              plt.ylabel(ylab)
              plt.show()
        else:
            abs_diff = np.nan

    if calc_difference:

        d = { 'Code': lu, 'Description' : desc, 'Count' : count, xlab : xmean, ylab : ymean, 'Difference' : modeldiff }
        df = pd.DataFrame( d )
        df = df[[ 'Code', 'Description', 'Count', xlab, ylab, 'Difference']]
        df = df.dropna()

    else:

        d = { 'Code': lu, 'Description' : desc, 'Count' : count, xlab : xmean, ylab : ymean }
        df = pd.DataFrame( d )
        df = df[[ 'Code', 'Description', 'Count', xlab, ylab ]]
        df = df.dropna()

    return df

def display_pandas_df( df ):
    pd.set_option('precision', 4)
    display( df )


def read_raster(rasterfile):
    '''
    reads a GDAL raster into numpy array for plotting
    also returns meshgrid of x and y coordinates of each cell for plotting
    based on code stolen from:
    http://stackoverflow.com/questions/20488765/plot-gdal-raster-using-matplotlib-basemap
    '''
    try:
        import gdal
    except:
        print('This function requires gdal.')
    try:
        ds = gdal.Open(rasterfile)
    except:
        raise IOError("problem reading raster file {}".format(rasterfile))

    print('\nreading in {} into numpy array...'.format(rasterfile))
    data = ds.ReadAsArray()
    gt = ds.GetGeoTransform()
    proj = ds.GetProjection()

    xres = gt[1]
    yres = gt[5]

    # get the edge coordinates and add half the resolution
    # to go to center coordinates
    xmin = gt[0] + xres * 0.5
    xmax = gt[0] + (xres * ds.RasterXSize) - xres * 0.5
    ymin = gt[3] + (yres * ds.RasterYSize) + yres * 0.5
    ymax = gt[3] + yres * 0.5

    del ds

    print('creating a grid of xy coordinates in the original projection...')
    xy = np.mgrid[xmin:xmax+xres:xres, ymax+yres:ymin:yres]

    # create a mask for no-data values
    data[data<-1.0e+20] = 0

    return data, gt, proj, xy

#
# def zonal_stats_series( x, factor, description, lookup_val, xlab ):
#
#     count_x = x.shape[0]
#
#     xmean = []
#     lu = []
#     desc = []
#     count = []
#
#     d = { 'LU_Code': lu, 'Description' : desc, 'Count' : count, xlab : xmean }
#     df = pd.DataFrame( d )
#
#     for idx in range( countx ):
#
#         xmean = []
#         lu = []
#         desc = []
#         count = []
#
#
#         for index in range( len(lookup_vals) ):
#
#             count_x=len(x[ factor == lookup_vals[index] ])
#             if count_x > 0:
#                 x_mean = np.nanmean(x[ factor == lookup_vals[index] ])
#             else:
#                 x_mean = np.nan
#
#             xmean.append( x_mean )
#             lu.append( lookup_vals[ index ] )
#             count.append( count_x )
#             desc.append( description[ index ] )
#
#     d = { 'LU_Code': lu, 'Description' : desc, 'Count' : count, xlab : xmean }
#     df = pd.DataFrame( d )
#     df = df[ [ 'LU_Code', 'Description', 'Count', xlab ] ]
#     df = df.dropna()
#
#     return( xmean )
