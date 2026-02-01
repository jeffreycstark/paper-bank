"""
Visualizations: Winner/Loser Effect Trajectories 2005-2022
Four waves of Asian Barometer Survey data (W2, W3, W4, W6)
N = 34,035 observations across 14 countries

Created: January 2025
"""

import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import numpy as np

# Set style
plt.style.use('seaborn-v0_8-whitegrid')
plt.rcParams['font.family'] = 'sans-serif'
plt.rcParams['font.size'] = 11

# Load data
data_dir = "/Users/jeffreystark/Development/Research/econdev-authpref/papers/meaning-of-democracy/analysis"
df = pd.read_csv(f'{data_dir}/loser_effect_by_country_4waves.csv')

output_dir = "/Users/jeffreystark/Development/Research/econdev-authpref/papers/meaning-of-democracy/output"

# Create output directory if it doesn't exist
import os
os.makedirs(output_dir, exist_ok=True)

# =============================================================================
# PLOT 1: Thailand - The Full Arc (Hero Plot)
# =============================================================================
thailand = df[df['country_name'] == 'Thailand'].sort_values('wave_year')

fig, ax = plt.subplots(figsize=(10, 6))

# Zero reference line
ax.axhline(y=0, color='gray', linestyle='--', linewidth=0.8, alpha=0.7)

# Area fill
ax.fill_between(thailand['wave_year'], 0, thailand['loser_effect'], 
                alpha=0.3, color='#E63946')

# Line and points
ax.plot(thailand['wave_year'], thailand['loser_effect'], 
        color='#E63946', linewidth=2.5, marker='o', markersize=12, 
        markerfacecolor='#E63946', markeredgecolor='white', markeredgewidth=2)

# Event labels
events = {
    2006: 'Post-2006\ncoup',
    2010: 'Democrat\ngovt',
    2014: '2014\nCOUP',
    2020: 'Military-\nbacked'
}

for year, label in events.items():
    effect = thailand[thailand['wave_year'] == year]['loser_effect'].values[0]
    ax.annotate(label, xy=(year, effect), xytext=(year, effect + 4),
                ha='center', fontsize=9, color='#333333')

# Value labels
for _, row in thailand.iterrows():
    sign = '+' if row['loser_effect'] > 0 else ''
    ax.annotate(f"{sign}{row['loser_effect']:.1f} pp", 
                xy=(row['wave_year'], row['loser_effect']),
                xytext=(row['wave_year'], row['loser_effect'] - 3),
                ha='center', fontsize=10, fontweight='bold', color='#E63946')

ax.set_xticks([2006, 2010, 2014, 2020])
ax.set_xticklabels(['2006\n(W2)', '2010\n(W3)', '2014\n(W4)', '2020\n(W6)'])
ax.set_ylim(-8, 24)
ax.set_ylabel('Loser Effect (percentage points)', fontsize=12)
ax.set_title('Thailand: As Democracy Eroded, Losers Embraced Procedural Values', 
             fontsize=14, fontweight='bold', pad=15)
ax.text(0.5, -0.12, 'Loser effect = % losers procedural − % winners procedural', 
        transform=ax.transAxes, ha='center', fontsize=10, color='gray')

plt.tight_layout()
plt.savefig(f'{output_dir}/fig_thailand_trajectory.png', dpi=300, bbox_inches='tight',
            facecolor='white', edgecolor='none')
plt.savefig(f'{output_dir}/fig_thailand_trajectory.pdf', bbox_inches='tight',
            facecolor='white', edgecolor='none')
print("Saved Thailand trajectory plot")
plt.close()

# =============================================================================
# PLOT 2: Multi-country comparison
# =============================================================================
# Countries with 3+ waves
country_counts = df.groupby('country_name').size()
countries_3plus = country_counts[country_counts >= 3].index.tolist()

multi = df[df['country_name'].isin(countries_3plus)]

fig, ax = plt.subplots(figsize=(12, 7))

# Zero reference
ax.axhline(y=0, color='gray', linestyle='--', linewidth=0.8, alpha=0.7)

# Color palette
colors = {
    'Thailand': '#E63946',
    'South Korea': '#1D3557',
    'Taiwan': '#457B9D',
    'Philippines': '#2A9D8F',
    'Mongolia': '#E9C46A',
    'Japan': '#F4A261',
    'Hong Kong': '#9B5DE5',
    'China': '#00BBF9',
    'Vietnam': '#00F5D4',
    'Malaysia': '#F15BB5'
}

# Plot other countries first (faded)
for country in countries_3plus:
    if country not in ['Thailand', 'South Korea']:
        country_data = multi[multi['country_name'] == country].sort_values('wave_year')
        ax.plot(country_data['wave_year'], country_data['loser_effect'],
                color=colors.get(country, 'gray'), linewidth=1, alpha=0.4,
                marker='o', markersize=5)

# Plot highlighted countries
for country in ['South Korea', 'Thailand']:
    country_data = multi[multi['country_name'] == country].sort_values('wave_year')
    lw = 2.5 if country == 'Thailand' else 2
    ms = 10 if country == 'Thailand' else 8
    ax.plot(country_data['wave_year'], country_data['loser_effect'],
            color=colors[country], linewidth=lw, marker='o', markersize=ms,
            label=country, markeredgecolor='white', markeredgewidth=1.5)

# Add country labels at end points
for country in countries_3plus:
    country_data = multi[multi['country_name'] == country].sort_values('wave_year')
    last = country_data.iloc[-1]
    color = colors.get(country, 'gray')
    alpha = 1.0 if country in ['Thailand', 'South Korea'] else 0.6
    fontweight = 'bold' if country in ['Thailand', 'South Korea'] else 'normal'
    ax.annotate(country, xy=(last['wave_year'], last['loser_effect']),
                xytext=(last['wave_year'] + 0.5, last['loser_effect']),
                fontsize=9, color=color, alpha=alpha, fontweight=fontweight,
                va='center')

ax.set_xticks([2006, 2010, 2014, 2020])
ax.set_xlim(2004, 2024)
ax.set_ylim(-15, 38)
ax.set_ylabel('Loser Effect (percentage points)', fontsize=12)
ax.set_title('Trajectories of the Loser Effect Across Asia (2005-2022)', 
             fontsize=14, fontweight='bold', pad=15)
ax.text(0.5, -0.08, 
        'Data: Asian Barometer Survey. Loser effect = % losers procedural − % winners procedural',
        transform=ax.transAxes, ha='center', fontsize=9, color='gray')

plt.tight_layout()
plt.savefig(f'{output_dir}/fig_multicountry_trajectory.png', dpi=300, bbox_inches='tight',
            facecolor='white', edgecolor='none')
plt.savefig(f'{output_dir}/fig_multicountry_trajectory.pdf', bbox_inches='tight',
            facecolor='white', edgecolor='none')
print("Saved multi-country trajectory plot")
plt.close()

# =============================================================================
# PLOT 3: Thailand dual panel (Loser Effect + % Winners)
# =============================================================================
thailand = df[df['country_name'] == 'Thailand'].sort_values('wave_year')

fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(9, 8), sharex=True)

# Top panel: Loser Effect
ax1.axhline(y=0, color='gray', linestyle='--', linewidth=0.8, alpha=0.7)
ax1.fill_between(thailand['wave_year'], 0, thailand['loser_effect'], 
                 alpha=0.3, color='#E63946')
ax1.plot(thailand['wave_year'], thailand['loser_effect'], 
         color='#E63946', linewidth=2.5, marker='o', markersize=10,
         markerfacecolor='#E63946', markeredgecolor='white', markeredgewidth=2)

for _, row in thailand.iterrows():
    sign = '+' if row['loser_effect'] > 0 else ''
    ax1.annotate(f"{sign}{row['loser_effect']:.1f}", 
                 xy=(row['wave_year'], row['loser_effect']),
                 xytext=(row['wave_year'], row['loser_effect'] + 2.5),
                 ha='center', fontsize=10, fontweight='bold', color='#E63946')

ax1.set_ylabel('Percentage points', fontsize=11)
ax1.set_title('Loser Effect', fontsize=12, fontweight='bold', color='#E63946', loc='left')
ax1.set_ylim(-5, 22)

# Bottom panel: % Winners
ax2.fill_between(thailand['wave_year'], 0, thailand['pct_winner'], 
                 alpha=0.3, color='#457B9D')
ax2.plot(thailand['wave_year'], thailand['pct_winner'], 
         color='#457B9D', linewidth=2.5, marker='o', markersize=10,
         markerfacecolor='#457B9D', markeredgecolor='white', markeredgewidth=2)

for _, row in thailand.iterrows():
    ax2.annotate(f"{row['pct_winner']:.0f}%", 
                 xy=(row['wave_year'], row['pct_winner']),
                 xytext=(row['wave_year'], row['pct_winner'] + 5),
                 ha='center', fontsize=10, fontweight='bold', color='#457B9D')

ax2.set_ylabel('Percent', fontsize=11)
ax2.set_title('% Identifying as Electoral Winners', fontsize=12, fontweight='bold', 
              color='#457B9D', loc='left')
ax2.set_ylim(0, 105)
ax2.set_xticks([2006, 2010, 2014, 2020])
ax2.set_xticklabels(['2006\n(W2)', '2010\n(W3)', '2014\n(W4)', '2020\n(W6)'])

fig.suptitle('Thailand: Democratic Erosion in Two Metrics', 
             fontsize=14, fontweight='bold', y=0.98)
fig.text(0.5, 0.92, 'As fewer citizens "won" elections, losers increasingly valued procedural democracy',
         ha='center', fontsize=11, color='gray')

plt.tight_layout(rect=[0, 0, 1, 0.9])
plt.savefig(f'{output_dir}/fig_thailand_dual.png', dpi=300, bbox_inches='tight',
            facecolor='white', edgecolor='none')
plt.savefig(f'{output_dir}/fig_thailand_dual.pdf', bbox_inches='tight',
            facecolor='white', edgecolor='none')
print("Saved Thailand dual plot")
plt.close()

# =============================================================================
# PLOT 4: Wave-level bar chart
# =============================================================================
wave_data = pd.DataFrame({
    'wave': ['W2\n2005-08', 'W3\n2010-12', 'W4\n2014-16', 'W6\n2019-22'],
    'loser_effect': [6.5, 4.3, 5.1, -1.5],
    'significant': [True, True, True, False],
    'n': [9208, 8237, 9828, 6762]
})

fig, ax = plt.subplots(figsize=(9, 5))

colors = ['#2A9D8F' if sig else '#E76F51' for sig in wave_data['significant']]
bars = ax.bar(wave_data['wave'], wave_data['loser_effect'], color=colors, width=0.6, alpha=0.9)

ax.axhline(y=0, color='#333333', linewidth=1)

for i, (bar, effect, n) in enumerate(zip(bars, wave_data['loser_effect'], wave_data['n'])):
    sign = '+' if effect > 0 else ''
    ypos = effect + 0.4 if effect > 0 else effect - 0.6
    ax.annotate(f'{sign}{effect} pp', xy=(bar.get_x() + bar.get_width()/2, ypos),
                ha='center', va='bottom' if effect > 0 else 'top',
                fontsize=11, fontweight='bold')
    ax.annotate(f'n={n:,}', xy=(bar.get_x() + bar.get_width()/2, -2.8),
                ha='center', fontsize=9, color='gray')

ax.set_ylim(-3.5, 9)
ax.set_ylabel('Loser Effect (percentage points)', fontsize=11)
ax.set_title('The Loser Effect Over Time: Pooled Across Countries', 
             fontsize=14, fontweight='bold', pad=15)

# Legend
sig_patch = mpatches.Patch(color='#2A9D8F', label='Significant (p < 0.001)')
ns_patch = mpatches.Patch(color='#E76F51', label='Not significant')
ax.legend(handles=[sig_patch, ns_patch], loc='upper right', frameon=True)

ax.text(0.5, -0.12, 'Loser effect = % losers procedural − % winners procedural. Total N = 34,035',
        transform=ax.transAxes, ha='center', fontsize=9, color='gray')

plt.tight_layout()
plt.savefig(f'{output_dir}/fig_wave_losereffect.png', dpi=300, bbox_inches='tight',
            facecolor='white', edgecolor='none')
plt.savefig(f'{output_dir}/fig_wave_losereffect.pdf', bbox_inches='tight',
            facecolor='white', edgecolor='none')
print("Saved wave bar chart")
plt.close()

# =============================================================================
# PLOT 5: Slope chart - change from first to last wave
# =============================================================================
slope_data = []
for country in countries_3plus:
    country_df = multi[multi['country_name'] == country].sort_values('wave_year')
    if len(country_df) >= 2:
        first = country_df.iloc[0]
        last = country_df.iloc[-1]
        change = last['loser_effect'] - first['loser_effect']
        slope_data.append({
            'country': country,
            'early': first['loser_effect'],
            'late': last['loser_effect'],
            'early_year': first['wave_year'],
            'late_year': last['wave_year'],
            'change': change
        })

slope_df = pd.DataFrame(slope_data).sort_values('change', ascending=False)

fig, ax = plt.subplots(figsize=(10, 7))

ax.axhline(y=0, color='gray', linestyle='--', linewidth=0.8, alpha=0.7)

for _, row in slope_df.iterrows():
    color = '#E63946' if row['change'] > 0 else '#457B9D'
    alpha = 1.0 if row['country'] in ['Thailand', 'South Korea', 'Taiwan'] else 0.5
    lw = 2 if row['country'] in ['Thailand', 'South Korea', 'Taiwan'] else 1
    
    ax.plot([0, 1], [row['early'], row['late']], color=color, alpha=alpha, linewidth=lw)
    ax.scatter([0, 1], [row['early'], row['late']], color=color, s=50, alpha=alpha, zorder=5)
    
    # Label at end
    sign = '+' if row['change'] > 0 else ''
    ax.annotate(f"{row['country']} ({sign}{row['change']:.1f})", 
                xy=(1, row['late']), xytext=(1.05, row['late']),
                fontsize=9, color=color, alpha=max(alpha, 0.7), va='center')

ax.set_xticks([0, 1])
ax.set_xticklabels(['First Wave\n(2006-2010)', 'Last Wave\n(2014-2020)'], fontsize=11)
ax.set_xlim(-0.2, 1.5)
ax.set_ylabel('Loser Effect (percentage points)', fontsize=11)
ax.set_title('Change in Loser Effect: First to Last Wave', fontsize=14, fontweight='bold', pad=15)

# Legend
increase_patch = mpatches.Patch(color='#E63946', label='Increased')
decrease_patch = mpatches.Patch(color='#457B9D', label='Decreased')
ax.legend(handles=[increase_patch, decrease_patch], loc='upper left')

plt.tight_layout()
plt.savefig(f'{output_dir}/fig_slope_change.png', dpi=300, bbox_inches='tight',
            facecolor='white', edgecolor='none')
plt.savefig(f'{output_dir}/fig_slope_change.pdf', bbox_inches='tight',
            facecolor='white', edgecolor='none')
print("Saved slope chart")
plt.close()

print("\n=== ALL VISUALIZATIONS COMPLETE ===")
