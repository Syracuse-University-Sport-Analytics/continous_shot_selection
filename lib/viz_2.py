from IPython.core.interactiveshell import InteractiveShell
InteractiveShell.ast_node_interactivity = "all"
import pandas as pd
from mpl_toolkits.axes_grid1 import make_axes_locatable
import matplotlib.colors as mcolors
import matplotlib.cm as cm
from matplotlib import ticker
import numpy as np
import os
import time
import math
low_memory=False


import matplotlib.pyplot as plt
plt.style.use('dark_background')

#%matplotlib notebook
import seaborn as sns
#color palletes: https://seaborn.pydata.org/tutorial/color_palettes.html
#https://stackoverflow.com/questions/54130442/querying-data-in-pandas-where-points-are-grouped-by-a-hexbin-function
def hexagonify(x, y, values, func=None):

    hexagonized_list = []

    fig = plt.figure()
    fig.set_visible(False)
    if func is not None:
        image = plt.hexbin(x=x, y=y, C=values, reduce_C_function=func)
    else:
        image = plt.hexbin(x=x, y=y, C=values)

    values = image.get_array()

    verts = image.get_offsets()
    for offc in range(verts.shape[0]):
            binx, biny = verts[offc][0], verts[offc][1]
            val = values[offc]
            if val:
                hexagonized_list.append((binx, biny, val))

    fig.clear()
    plt.close(fig)
    return hexagonized_list

DATA_LOCATION = "cleanedData/"
VISUALIZATION_LOCATION = "visualizations/"
#DATA_LOCATION = "cleanedData/backup/Nov5/"
#VISUALIZATION_LOCATION = "visualizations/test/"
#play_by_play =  pd.read_csv(DATA_LOCATION + "play_by_play.csv")
play_by_play_bins =  pd.read_csv(DATA_LOCATION + "play_by_play_bins.csv")
play_by_play_season_player_team_bins =  pd.read_csv(DATA_LOCATION + "play_by_play_season_player_team_bins.csv")

play_by_play_season_bins = pd.read_csv(DATA_LOCATION + "play_by_play_season_bins.csv")
play_by_play_season_team_bins = pd.read_csv(DATA_LOCATION + "play_by_play_season_team_bins.csv")
play_by_play_season_team_bins['true_shooting_over_expected'] = play_by_play_season_team_bins['true_expected_points']-play_by_play_season_team_bins['expected_points']
play_by_play_bins['true_shooting_over_expected'] = play_by_play_bins['true_expected_points']-play_by_play_bins['expected_points']
play_by_play_season_bins['true_shooting_over_expected'] = play_by_play_season_bins['true_expected_points']-play_by_play_season_bins['expected_points']
play_by_play_season_player_team_bins['true_shooting_over_expected'] = play_by_play_season_player_team_bins['true_expected_points']-play_by_play_season_player_team_bins['expected_points']
play_by_play_season_team_bins['true_shooting_over_expected'] = play_by_play_season_team_bins['true_expected_points']-play_by_play_season_team_bins['expected_points']
from matplotlib.patches import Circle, Rectangle, Arc

def draw_court(ax=None, color='black', lw=2, outer_lines=False):
    # If an axes object isn't provided to plot onto, just get current one
    if ax is None:
        ax = plt.gca()

    # Create the various parts of an NBA basketball court

    # Create the basketball hoop
    # Diameter of a hoop is 18" so it has a radius of 9", which is a value
    # 7.5 in our coordinate system
    hoop = Circle((0, 0), radius=7.5, linewidth=lw, color=color, fill=False)

    # Create backboard
    backboard = Rectangle((-30, -7.5), 60, -1, linewidth=lw, color=color)

    # The paint
    # Create the outer box 0f the paint, width=16ft, height=19ft
    outer_box = Rectangle((-80, -47.5), 160, 190, linewidth=lw, color=color,
                          fill=False)
    # Create the inner box of the paint, widt=12ft, height=19ft
    inner_box = Rectangle((-60, -47.5), 120, 190, linewidth=lw, color=color,
                          fill=False)

    # Create free throw top arc
    top_free_throw = Arc((0, 142.5), 120, 120, theta1=0, theta2=180,
                         linewidth=lw, color=color, fill=False)
    # Create free throw bottom arc
    bottom_free_throw = Arc((0, 142.5), 120, 120, theta1=180, theta2=0,
                            linewidth=lw, color=color, linestyle='dashed')
    # Restricted Zone, it is an arc with 4ft radius from center of the hoop
    restricted = Arc((0, 0), 80, 80, theta1=0, theta2=180, linewidth=lw,
                     color=color)

    # Three point line
    # Create the side 3pt lines, they are 14ft long before they begin to arc
    corner_three_a = Rectangle((-220, -47.5), 0, 140, linewidth=lw,
                               color=color)
    corner_three_b = Rectangle((220, -47.5), 0, 140, linewidth=lw, color=color)
    # 3pt arc - center of arc will be the hoop, arc is 23'9" away from hoop
    # I just played around with the theta values until they lined up with the
    # threes
    three_arc = Arc((0, 0), 475, 475, theta1=22, theta2=158, linewidth=lw,
                    color=color)

    # Center Court
    center_outer_arc = Arc((0, 422.5), 120, 120, theta1=180, theta2=0,
                           linewidth=lw, color=color)
    center_inner_arc = Arc((0, 422.5), 40, 40, theta1=180, theta2=0,
                           linewidth=lw, color=color)

    # List of the court elements to be plotted onto the axes
    court_elements = [hoop, backboard, outer_box, inner_box, top_free_throw,
                      bottom_free_throw, restricted, corner_three_a,
                      corner_three_b, three_arc, center_outer_arc,
                      center_inner_arc]

    if outer_lines:
        # Draw the half court line, baseline and side out bound lines
        outer_lines = Rectangle((-250, -47.5), 500, 470, linewidth=lw,
                                color=color, fill=False)
        court_elements.append(outer_lines)

    # Add the court elements onto the axes
    for element in court_elements:
        ax.add_patch(element)

    return ax

def draw_player_team_season(current_player, current_team, current_season, play_by_play_season_player_team_bins=play_by_play_season_player_team_bins, metric = 'expected_points',size_metric = "estimated_shot_attempts",  title="", metric_label = "", ax=None, color='black', lw=2, outer_lines=False, scale=10, low=-8, high=0.8):

    plt.sca(ax)

    current_player_play_by_play_bins = play_by_play_season_player_team_bins[(play_by_play_season_player_team_bins.player_shooting == current_player) & (play_by_play_season_player_team_bins.season == current_season)& (play_by_play_season_player_team_bins.team == current_team)&(play_by_play_season_player_team_bins.playoffs == False)]
    current_player_play_by_play_bins.to_csv("current_player_team_play_by_play_bins.csv")
    current_player_play_by_play_bins.to_csv(DATA_LOCATION+"generatedData/"+current_player+"_"+current_team+"_"+str(current_season)+"_"+metric+'_bins.csv')
    metric_max = max(current_player_play_by_play_bins[metric])
    metric_min = min(current_player_play_by_play_bins[metric])
    cmap=sns.color_palette("coolwarm", as_cmap=True)
    #bin_plot = sns.scatterplot(x='x_court_bin', y='y_court_bin', data=current_player_play_by_play_bins, hue='true_shooting_over_expected', marker='h', s=scale*4, sizes=(scale, scale),color=cmap(.2), palette=cmap, hue_norm=(-0.8,0.8))
    draw_court(ax=ax,outer_lines=True, color="white", lw=1)

    #bin_plot = sns.scatterplot(x='x_court_bin', y='y_court_bin', data=current_player_play_by_play_bins, hue='true_shooting_over_expected', marker='h', s=scale*4, sizes=(scale, scale),color=cmap(.2), palette=cmap, hue_norm=(low,high))
    bin_plot = sns.scatterplot(x='x_court_bin', y='y_court_bin', data=current_player_play_by_play_bins, hue=metric, marker='h', size=current_player_play_by_play_bins[size_metric], sizes=(2, 18),color=cmap(.2), palette=cmap, hue_norm=(metric_min,metric_max),ax=ax)

    plt.title(title)
    plt.legend(title=metric, loc=(0.95,0.5))


    norm = plt.Normalize(metric_max, metric_min)
    sm = plt.cm.ScalarMappable(cmap=cmap, norm=norm)
    sm.set_array([])
    bin_plot.get_legend().remove()
    plt.axis('off')

    bin_plot.set(xlim=(-300,300))
    bin_plot.set(ylim=(-100,500))
    bin_plot.set(aspect=1)

    #stat_func=None doesn't seem to exist
    #draw_court(ax=ax,outer_lines=True, color="white")

    divider = make_axes_locatable(bin_plot)
    cax = divider.append_axes("bottom", size="3%", pad=0.05)
    bin_plot.figure.colorbar(sm,orientation="horizontal", cax=cax)

    cax.set_xlabel(metric_label)  # cax == cb.ax

    #plt.xlabel("")
    #plt.ylabel("")



    #plt.savefig(VISUALIZATION_LOCATION+current_player+metric+'.png', dpi=600)



    #plt.savefig(VISUALIZATION_LOCATION+current_player+metric+'.png', dpi=600)

def draw_player_team_season_all(current_player, current_team, current_season, play_by_play_season_player_team_bins=play_by_play_season_player_team_bins,  color='black', lw=2, outer_lines=False, scale=10, low=-8, high=0.8):

    fig, (ax1, ax2, ax3) = plt.subplots(ncols=3, sharey=True)

    draw_player_team_season(current_player, current_team, current_season, play_by_play_season_player_team_bins=play_by_play_season_player_team_bins,metric = 'true_shooting_over_expected',metric_label="True Shooting\nOver Expected",ax=ax1, color='black', lw=2, outer_lines=False, scale=10, low=-8, high=0.8)
    draw_player_team_season(current_player, current_team, current_season, play_by_play_season_player_team_bins=play_by_play_season_player_team_bins,metric = 'expected_points',metric_label="Expected Points", size_metric="fga", ax=ax2,title=current_player + " " + str(current_team) + " " + str(current_season) + "-" + str(current_season % 2000+1) + " Season",  color='black', lw=2, outer_lines=False, scale=10, low=-8, high=0.8)
    draw_player_team_season(current_player, current_team, current_season, play_by_play_season_player_team_bins=play_by_play_season_player_team_bins,metric = 'true_expected_points',metric_label="True Expected Points",ax=ax3, color='black', lw=2, outer_lines=False, scale=10, low=-8, high=0.8)
    plt.savefig(VISUALIZATION_LOCATION+current_player+"_"+str(current_season)+"_"+str(current_team)+'_side_by_side.png', dpi=1200)

def draw_player_season(current_player, current_season, play_by_play_bins=play_by_play_bins, metric = 'expected_points',size_metric = "estimated_shot_attempts",  title="", metric_label = "", ax=None, color='black', lw=2, outer_lines=False, scale=10, low=-8, high=0.8):

    plt.sca(ax)

    current_player_play_by_play_bins = play_by_play_bins[(play_by_play_bins.player_shooting == current_player) & (play_by_play_bins.season == current_season)& (play_by_play_bins.playoffs == False)].copy()
    current_player_play_by_play_bins["original_metric"] = current_player_play_by_play_bins[metric]
    #current_player_play_by_play_bins[metric] = np.log(current_player_play_by_play_bins[metric]+4)
    

    metric_max = max(current_player_play_by_play_bins[metric])
    metric_min = min(current_player_play_by_play_bins[metric])
    metric_median = np.mean(current_player_play_by_play_bins[metric])

    if metric_min >= metric_max:
        return None #must have data to render - to fix this could use the code from draw_player_team_season

    normalize = mcolors.TwoSlopeNorm(vcenter=metric_median, vmin=metric_min, vmax=metric_max)

    colormap = cm.coolwarm

    draw_court(ax=ax,outer_lines=True, color="white", lw=1)
    bin_plot = sns.scatterplot(x='x_court_bin', y='y_court_bin', data=current_player_play_by_play_bins,
                               marker='h', size=current_player_play_by_play_bins[size_metric], sizes=(2, 18),
                               c=current_player_play_by_play_bins[metric], cmap=colormap, norm=normalize, ax=ax, zorder=10)

    plt.title(title)
    plt.legend(title=metric, loc=(0.95,0.5))

    sm = plt.cm.ScalarMappable(cmap=colormap, norm=normalize)
    sm.set_array([])
    bin_plot.get_legend().remove()
    plt.axis('off')

    bin_plot.set(xlim=(-300,300))
    bin_plot.set(ylim=(-100,500))
    bin_plot.set(aspect=1)

    #stat_func=None doesn't seem to exist
    #draw_court(ax=ax,outer_lines=True, color="white")


    divider = make_axes_locatable(bin_plot)
    cax = divider.append_axes("bottom", size="3%", pad=0.05)
    cb = bin_plot.figure.colorbar(sm,orientation="horizontal", cax=cax) #, format=ticker.LogFormatterMathtext()
    tick_locator = ticker.MaxNLocator(nbins=3)
    cb.locator = tick_locator
    cb.update_ticks()

    cax.set_xlabel(metric_label)  # cax == cb.ax

    #plt.savefig(VISUALIZATION_LOCATION+current_player+metric+'.png', dpi=600)

def draw_player_season_all(current_player, current_season, play_by_play_bins=play_by_play_bins,  color='black', lw=2, outer_lines=False, scale=10, low=-8, high=0.8):

    fig, (ax1, ax2, ax3) = plt.subplots(ncols=3, sharey=True)
    draw_player_season(current_player, current_season, play_by_play_bins=play_by_play_bins,metric = 'true_shooting_over_expected',metric_label="True Shooting\nOver Expected",ax=ax1, color='black', lw=2, outer_lines=False, scale=10, low=-8, high=0.8)
    draw_player_season(current_player, current_season, play_by_play_bins=play_by_play_bins,metric = 'expected_points',metric_label="Expected Points", size_metric="fga", ax=ax2,title=current_player + " " + str(current_season) + "-" + str(current_season % 2000+1) + " Season",  color='black', lw=2, outer_lines=False, scale=10, low=-8, high=0.8)
    draw_player_season(current_player, current_season, play_by_play_bins=play_by_play_bins,metric = 'true_expected_points',metric_label="True Expected Points",ax=ax3, color='black', lw=2, outer_lines=False, scale=10, low=-8, high=0.8)
    plt.savefig(VISUALIZATION_LOCATION+current_player+"_"+str(current_season)+'_side_by_side.png', dpi=1200)
    plt.close(fig)


def draw_season( current_season, play_by_play_bins=play_by_play_bins, metric = 'expected_points',size_metric = "estimated_shot_attempts",  title="", metric_label = "", ax=None, color='black', lw=2, outer_lines=False, scale=10, low=-8, high=0.8):

    plt.sca(ax)

    current_player_play_by_play_bins = play_by_play_bins[ (play_by_play_bins.season == current_season) & (play_by_play_bins.playoffs == False)].copy()
    current_player_play_by_play_bins.to_csv(DATA_LOCATION+"generatedData/"+str(current_season)+"_"+metric+'_bins.csv')
    metric_max = max(current_player_play_by_play_bins[metric])
    metric_min = min(current_player_play_by_play_bins[metric])
    metric_median = np.mean(current_player_play_by_play_bins[metric])

    normalize = mcolors.TwoSlopeNorm(vcenter=metric_median, vmin=metric_min, vmax=metric_max)

    colormap = cm.coolwarm

    draw_court(ax=ax, outer_lines=True, color="white", lw=1)
    bin_plot = sns.scatterplot(x='x_court_bin', y='y_court_bin', data=current_player_play_by_play_bins,
                               marker='h', size=current_player_play_by_play_bins[size_metric], sizes=(2, 18),
                               c=current_player_play_by_play_bins[metric], cmap=colormap, norm=normalize, ax=ax,
                               zorder=10)

    plt.title(title)
    plt.legend(title=metric, loc=(0.95, 0.5))

    sm = plt.cm.ScalarMappable(cmap=colormap, norm=normalize)
    sm.set_array([])
    bin_plot.get_legend().remove()
    plt.axis('off')

    bin_plot.set(xlim=(-300, 300))
    bin_plot.set(ylim=(-100, 500))
    bin_plot.set(aspect=1)

    # stat_func=None doesn't seem to exist
    # draw_court(ax=ax,outer_lines=True, color="white")

    divider = make_axes_locatable(bin_plot)
    cax = divider.append_axes("bottom", size="3%", pad=0.05)
    cb = bin_plot.figure.colorbar(sm, orientation="horizontal", cax=cax)  # , format=ticker.LogFormatterMathtext()
    tick_locator = ticker.MaxNLocator(nbins=3)
    cb.locator = tick_locator
    cb.update_ticks()

    cax.set_xlabel(metric_label)  # cax == cb.ax


    #plt.savefig(VISUALIZATION_LOCATION+current_player+metric+'.png', dpi=600)

def draw_season_all(current_season, play_by_play_bins=play_by_play_bins,  color='black', lw=2, outer_lines=False, scale=10, low=-8, high=0.8):

    fig, (ax1, ax2, ax3) = plt.subplots(ncols=3, sharey=True)
    draw_season(current_season, play_by_play_bins=play_by_play_bins,metric = 'true_shooting_over_expected',metric_label="True Shooting\nOver Expected",ax=ax1, color='black', lw=2, outer_lines=False, scale=10, low=-8, high=0.8)
    draw_season(current_season, play_by_play_bins=play_by_play_bins,metric = 'expected_points',metric_label="Expected Points", size_metric="fga", ax=ax2,title="NBA " + str(current_season) + "-" + str(current_season % 2000+1) + " Season",  color='black', lw=2, outer_lines=False, scale=10, low=-8, high=0.8)
    draw_season(current_season, play_by_play_bins=play_by_play_bins,metric = 'true_expected_points',metric_label="True Expected Points",ax=ax3, color='black', lw=2, outer_lines=False, scale=10, low=-8, high=0.8)
    plt.savefig(VISUALIZATION_LOCATION+str(current_season)+'_side_by_side.png', dpi=600)

def draw_team_season( current_season, current_team, play_by_play_bins=play_by_play_season_bins, metric = 'expected_points',size_metric = "estimated_shot_attempts",  title="", metric_label = "", ax=None, color='black', lw=2, outer_lines=False, scale=10, low=-8, high=0.8):

    plt.sca(ax)

    current_player_play_by_play_bins = play_by_play_bins[ (play_by_play_bins.season == current_season) &(play_by_play_bins.team == current_team) & (play_by_play_bins.playoffs == False)].copy()
    current_player_play_by_play_bins.to_csv(DATA_LOCATION+"generatedData/"+current_team+"_"+str(current_season)+"_"+metric+'_bins.csv')
    metric_max = max(current_player_play_by_play_bins[metric])
    metric_min = min(current_player_play_by_play_bins[metric])
    metric_median = np.mean(current_player_play_by_play_bins[metric])

    normalize = mcolors.TwoSlopeNorm(vcenter=metric_median, vmin=metric_min, vmax=metric_max)

    colormap = cm.coolwarm

    draw_court(ax=ax, outer_lines=True, color="white", lw=1)
    bin_plot = sns.scatterplot(x='x_court_bin', y='y_court_bin', data=current_player_play_by_play_bins,
                               marker='h', size=current_player_play_by_play_bins[size_metric], sizes=(2, 18),
                               c=current_player_play_by_play_bins[metric], cmap=colormap, norm=normalize, ax=ax,
                               zorder=10)

    plt.title(title)
    plt.legend(title=metric, loc=(0.95, 0.5))

    sm = plt.cm.ScalarMappable(cmap=colormap, norm=normalize)
    sm.set_array([])
    bin_plot.get_legend().remove()
    plt.axis('off')

    bin_plot.set(xlim=(-300, 300))
    bin_plot.set(ylim=(-100, 500))
    bin_plot.set(aspect=1)

    # stat_func=None doesn't seem to exist
    # draw_court(ax=ax,outer_lines=True, color="white")

    divider = make_axes_locatable(bin_plot)
    cax = divider.append_axes("bottom", size="3%", pad=0.05)
    cb = bin_plot.figure.colorbar(sm, orientation="horizontal", cax=cax)  # , format=ticker.LogFormatterMathtext()
    tick_locator = ticker.MaxNLocator(nbins=3)
    cb.locator = tick_locator
    cb.update_ticks()

    cax.set_xlabel(metric_label)  # cax == cb.ax
    #plt.savefig(VISUALIZATION_LOCATION+current_player+metric+'.png', dpi=600)

def draw_team_season_all(current_season, current_team, play_by_play_bins=play_by_play_season_team_bins,  color='black', lw=2, outer_lines=False, scale=10, low=-8, high=0.8):

    fig, (ax1, ax2, ax3) = plt.subplots(ncols=3, sharey=True)
    draw_team_season(current_season, current_team, play_by_play_bins=play_by_play_bins,metric = 'true_shooting_over_expected',metric_label="True Shooting\nOver Expected",ax=ax1, color='black', lw=2, outer_lines=False, scale=10, low=-8, high=0.8)
    draw_team_season(current_season, current_team, play_by_play_bins=play_by_play_bins,metric = 'expected_points',metric_label="Expected Points", size_metric="fga", ax=ax2,title=current_team+" " + str(current_season) + "-" + str(current_season % 2000+1) + " Season",  color='black', lw=2, outer_lines=False, scale=10, low=-8, high=0.8)
    draw_team_season(current_season, current_team, play_by_play_bins=play_by_play_bins,metric = 'true_expected_points',metric_label="True Expected Points",ax=ax3, color='black', lw=2, outer_lines=False, scale=10, low=-8, high=0.8)
    plt.savefig(VISUALIZATION_LOCATION+str(current_season)+"_"+current_team+'_side_by_side.png', dpi=600)
    plt.close(fig)
print("viz lib loaded")