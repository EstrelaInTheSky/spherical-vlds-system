# Library Importation.
import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns
from mpl_toolkits.axes_grid1.inset_locator import inset_axes


def Plotting():
    # Style Setting.
    sns.set_theme(context="notebook", style="whitegrid", palette="bright")
    plt.rcParams.update(
        {
            "font.family": "serif",
            "axes.labelsize": 14,
            "axes.titlesize": 16,
            "legend.fontsize": 12,
            "xtick.labelsize": 12,
            "ytick.labelsize": 12,
            "figure.figsize": (7, 5),
        }
    )
    # Markers and Colors Definement
    markers = {64: "o", 128: "s", 256: "^"}
    colors = {64: "C0", 128: "C1", 256: "C2"}

    # Dataframes Settings.
    sizes = [64, 128, 256]
    dfs = {}
    for size in sizes:
        df = pd.read_csv(
            f"MethodB-C-RSOS-Simulation-{size}.dat", sep=r"\s+", header=None
        )
        df.columns = ["T", "H", "S", "K", "W"]
        dfs[size] = df

    # Single Plot Function.
    def SinglePlot(x, y, title, filename):
        plt.figure()
        for size in sizes:
            sns.lineplot(
                x=x,
                y=y,
                data=dfs[size],
                label=f"Lx = Ly = {size}",
                marker=markers[size],
                linestyle="None",
                markersize=4,
                color=colors[size],
                markeredgecolor="black",
            )

        plt.title(title)
        plt.xlabel(r"$T$")
        plt.ylabel(f"${y}$")
        sns.despine()
        plt.legend(frameon=False, loc="upper left")
        plt.tight_layout()

        # Inset com escala logarítmica
        ax = plt.gca()
        ax_inset = inset_axes(ax, width="30%", height="30%", loc="lower right")
        for size in sizes:
            sns.lineplot(
                x=x,
                y=y,
                data=dfs[size],
                linestyle="-",
                linewidth=1,
                ax=ax_inset,
                color=colors[size],
            )
        ax_inset.set_yscale("log")
        ax_inset.set_title("Log Scale", fontsize=10)
        sns.despine(ax=ax_inset)

        plt.savefig(filename, dpi=300, bbox_inches="tight")
        plt.close()

    # Plotting:
    # Single.
    variaveis = ["H", "S", "K", "W"]
    for var in variaveis:
        SinglePlot("T", var, f"T vs. {var}", f"{var}_Graph.png")
    # Combined.
    fig, axes = plt.subplots(2, 2, figsize=(14, 8), constrained_layout=True)
    sns.despine(fig=fig)

    for ax, var in zip(axes.flatten(), variaveis):
        for size in sizes:
            sns.lineplot(
                ax=ax,
                x="T",
                y=var,
                data=dfs[size],
                label=f"Lx = Ly = {size}",
                marker=markers[size],
                linestyle="None",
                markersize=4,
                color=colors[size],
                markeredgecolor="black",
            )
        ax.set_title(f"T vs. {var}")
        ax.set_xlabel(r"$T \ (s)$")
        ax.set_ylabel(f"${var}$")
        ax.legend(frameon=False, loc="upper left")

        # Log Scale Insets.
        ax_inset = inset_axes(ax, width="30%", height="30%", loc="lower right")
        for size in sizes:
            sns.lineplot(
                x="T",
                y=var,
                data=dfs[size],
                linestyle="-",
                linewidth=1,
                ax=ax_inset,
                color=colors[size],
            )
        ax_inset.set_yscale("log")
        ax_inset.set_title("Log Scale", fontsize=10)
        sns.despine(ax=ax_inset)

    plt.savefig("Combined_Graph.png", dpi=300, bbox_inches="tight")
    plt.close()


if __name__ == "__main__":
    Plotting()
