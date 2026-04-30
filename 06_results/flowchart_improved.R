library(ggplot2)
library(grid)

# ── Colors ──────────────────────────────────────────────────────────────────
G1 <- "#2e7d32"; G2 <- "#388e3c"; G3 <- "#a5d6a7"
G4 <- "#c8e6c9"; G5 <- "#e8f5e9"; G6 <- "#dcedc8"
P1 <- "#ad1457"; P2 <- "#fce4ec"; P3 <- "#f8bbd0"
O1 <- "#bf360c"; O2 <- "#fff3e0"; O3 <- "#ffccbc"
PU1 <- "#4527a0"; PU2 <- "#ede7f6"; PU3 <- "#d1c4e9"
B1 <- "#01579b";  B2 <- "#e1f5fe"; B3 <- "#b3e5fc"
WH <- "white"; PR <- "#fafafa"; DC <- "#fffde7"
TX <- "#1a1a1a"; AR <- "#424242"

# ── Accumulators ─────────────────────────────────────────────────────────────
bg_layers   <- list()
node_layers <- list()
text_layers <- list()
arr_layers  <- list()

add_bg   <- function(x) bg_layers   <<- c(bg_layers,   list(x))
add_node <- function(x) node_layers <<- c(node_layers, list(x))
add_text <- function(x) text_layers <<- c(text_layers, list(x))
add_arr  <- function(x) arr_layers  <<- c(arr_layers,  list(x))

# ── Section background with solid header bar ─────────────────────────────────
sec <- function(x1, x2, y1, y2, label, hc, fc, fs = 5.0) {
  add_bg(annotate("rect", xmin=x1, xmax=x2, ymin=y1, ymax=y2,
                  fill=fc, color=hc, linewidth=0.9))
  add_bg(annotate("rect", xmin=x1, xmax=x2, ymin=y2-0.55, ymax=y2,
                  fill=hc, color=hc, linewidth=0))
  add_text(annotate("text", x=(x1+x2)/2, y=y2-0.275, label=label,
                    color=WH, fontface="bold", size=fs, hjust=0.5,
                    family="sans"))
}

# ── Subsection background ─────────────────────────────────────────────────────
ssec <- function(x1, x2, y1, y2, label, hc, fc, fs = 2.9) {
  add_bg(annotate("rect", xmin=x1, xmax=x2, ymin=y1, ymax=y2,
                  fill=fc, color=hc, linewidth=0.7))
  add_bg(annotate("rect", xmin=x1, xmax=x2, ymin=y2-0.42, ymax=y2,
                  fill=hc, color=hc, linewidth=0))
  add_text(annotate("text", x=(x1+x2)/2, y=y2-0.21, label=label,
                    color=WH, fontface="bold", size=fs, hjust=0.5,
                    family="sans"))
}

# ── Rounded rectangle box ─────────────────────────────────────────────────────
rbox <- function(cx, cy, w, h, label, fc=PR, ec=AR, fs=2.7,
                 lw=0.55, bold=FALSE, thick=FALSE) {
  add_node(annotate("rect",
                    xmin=cx-w/2, xmax=cx+w/2,
                    ymin=cy-h/2, ymax=cy+h/2,
                    fill=fc, color=ec, linewidth=if(thick) 1.1 else lw))
  add_text(annotate("text", x=cx, y=cy, label=label,
                    color=TX, fontface=if(bold) "bold" else "plain",
                    size=fs, hjust=0.5, family="sans"))
}

# ── Parallelogram (data input) ────────────────────────────────────────────────
pbox <- function(cx, cy, w, h, label, fc=G5, ec=G1, fs=2.5, sk=0.22) {
  xs <- c(cx-w/2+sk, cx+w/2+sk, cx+w/2-sk, cx-w/2-sk)
  ys <- c(cy-h/2,    cy-h/2,    cy+h/2,    cy+h/2)
  add_node(annotate("polygon", x=xs, y=ys, fill=fc, color=ec, linewidth=0.6))
  add_text(annotate("text", x=cx, y=cy, label=label,
                    color=TX, size=fs, hjust=0.5, family="sans"))
}

# ── Diamond ───────────────────────────────────────────────────────────────────
dbox <- function(cx, cy, w, h, label, fc=DC, ec=AR, fs=2.3) {
  xs <- c(cx, cx+w/2, cx, cx-w/2)
  ys <- c(cy-h/2, cy, cy+h/2, cy)
  add_node(annotate("polygon", x=xs, y=ys, fill=fc, color=ec, linewidth=0.7))
  add_text(annotate("text", x=cx, y=cy, label=label,
                    color=TX, size=fs, hjust=0.5, family="sans"))
}

# ── Dashed box ────────────────────────────────────────────────────────────────
dashbox <- function(x1, x2, y1, y2, label, ec="#558b2f", fs=2.0) {
  add_node(annotate("rect", xmin=x1, xmax=x2, ymin=y1, ymax=y2,
                    fill=WH, color=ec, linewidth=0.5, linetype="dashed"))
  add_text(annotate("text", x=(x1+x2)/2, y=(y1+y2)/2, label=label,
                    color=ec, size=fs, hjust=0.5, family="sans"))
}

# ── Arrow ─────────────────────────────────────────────────────────────────────
arr <- function(x1, y1, x2, y2, col_=AR, lw=0.45, dash=FALSE) {
  add_arr(annotate("segment", x=x1, y=y1, xend=x2, yend=y2,
                   color=col_, linewidth=lw,
                   linetype=if(dash) "dashed" else "solid",
                   arrow=arrow(length=unit(0.11,"cm"), type="closed")))
}

# ════════════════════════════════════════════════════════════════════════════════
# LAYOUT — xlim(0,14), ylim(-2.5, 29.5)
# ════════════════════════════════════════════════════════════════════════════════

# Section Y ranges
DP_Y1 <- 16.8; DP_Y2 <- 29.4   # Data Preparation
AS_Y1 <- 9.1;  AS_Y2 <- 16.5   # Assignment
SC_Y1 <- 9.1;  SC_Y2 <- 16.5   # Resistance Scenarios
LC_Y1 <- 4.9;  LC_Y2 <- 8.8    # LCP
EP_Y1 <- 0.3;  EP_Y2 <- 4.6    # EPOF

# Subsection X ranges
SS1_X1 <- 0.25; SS1_X2 <- 4.6   # Origin points
SS2_X1 <- 4.8;  SS2_X2 <- 9.15  # Trafficking nodes
SS3_X1 <- 9.35; SS3_X2 <- 13.75 # Resistance surface

# Centers
CX1  <- (SS1_X1+SS1_X2)/2   # 2.425
CX2  <- (SS2_X1+SS2_X2)/2   # 6.975
CX3  <- (SS3_X1+SS3_X2)/2   # 11.55
CX_A <- (0.15+6.75)/2        # 3.45  Assignment
CX_S <- (7.25+13.85)/2       # 10.55 Scenarios
CX_L <- 7.0                  # LCP
CX_E <- 7.0                  # EPOF

BW  <- 3.7    # standard box width
BH  <- 0.50   # standard box height

# ════════════════════════════════════════════════════════════════════════════════
# SECTION BACKGROUNDS
# ════════════════════════════════════════════════════════════════════════════════

sec(0.15, 13.85, DP_Y1, DP_Y2, "Data Preparation", G1, G4, fs=5.5)

ssec(SS1_X1, SS1_X2, DP_Y1+0.15, DP_Y2-0.62,
     "(1) Definition of coca cultivation\norigin points", G2, G3, fs=2.8)
ssec(SS2_X1, SS2_X2, DP_Y1+0.15, DP_Y2-0.62,
     "(2) Definition of trafficking nodes", G2, G3, fs=2.8)
ssec(SS3_X1, SS3_X2, DP_Y1+0.15, DP_Y2-0.62,
     "(3) Construction of resistance surface", G2, G3, fs=2.8)

sec(0.15, 6.75,  AS_Y1, AS_Y2, "(4a) Assignment of Trafficking\nNodes to Origins", P1, P2, fs=4.3)
sec(7.25, 13.85, SC_Y1, SC_Y2, "(4b) Resistance Scenarios", O1, O2, fs=4.3)
sec(0.15, 13.85, LC_Y1, LC_Y2, "(4c) Least-Cost Path (LCP) Computation", PU1, PU2, fs=5.0)
sec(0.15, 13.85, EP_Y1, EP_Y2, "(5) EPOF — Ensemble Path Occurrence Forecasting", B1, B2, fs=4.5)

# ════════════════════════════════════════════════════════════════════════════════
# SS1: ORIGIN POINTS
# ════════════════════════════════════════════════════════════════════════════════
y1 <- c(28.2, 27.05, 25.95, 24.85, 23.75, 21.85)

pbox(CX1, y1[1], BW,   0.55, "Coca grids (SIMCI)", fc=G5, ec=G1)
rbox(CX1, y1[2], BW,   BH,   "Select a year (2017–2022)", fc=PR, ec=G2)
rbox(CX1, y1[3], BW,   BH,   "Select > 20 ha", fc=PR, ec=G2)
rbox(CX1, y1[4], BW,   BH,   "Dissolve contiguous cells", fc=PR, ec=G2)
rbox(CX1, y1[5], BW,   BH,   "Centroids", fc=PR, ec=G2)
rbox(CX1, y1[6], BW+0.2, 0.6, "Origin points\n(annual)", fc=G4, ec=G1,
     bold=TRUE, thick=TRUE, fs=3.0)

dashbox(SS1_X2-0.65, SS1_X2-0.05, 22.6, 24.4,
        "Repeat\nfor each\nyear\n2017–22", ec=G2)

arr(CX1, y1[1]-0.28, CX1, y1[2]+0.25)
arr(CX1, y1[2]-0.25, CX1, y1[3]+0.25)
arr(CX1, y1[3]-0.25, CX1, y1[4]+0.25)
arr(CX1, y1[4]-0.25, CX1, y1[5]+0.25)
arr(CX1, y1[5]-0.25, CX1, y1[6]+0.30)

# ════════════════════════════════════════════════════════════════════════════════
# SS2: TRAFFICKING NODES
# ════════════════════════════════════════════════════════════════════════════════
y2 <- c(28.2, 27.0, 25.9, 24.65, 23.3, 21.85)

pbox(CX2-1.15, y2[1], 1.95, 0.55, "Seizures\ncocaine",  fc=G5, ec=G1, fs=2.4)
pbox(CX2+1.15, y2[1], 1.95, 0.55, "Population\n(GPW)", fc=G5, ec=G1, fs=2.4)
rbox(CX2, y2[2], BW+0.5, BH,   "Select a year (2017–2022)", fc=PR, ec=G2)
rbox(CX2, y2[3], BW+0.5, BH,   "Construct Thiessen polygons", fc=PR, ec=G2)
rbox(CX2, y2[4], BW+0.5, 0.55, "Compute per-capita seizures\nand z-scores",
     fc=PR, ec=G2, fs=2.6)
rbox(CX2, y2[5], BW+0.7, 0.70,
     "Select trafficking nodes\n(z-score \u2265 P99 land; >1000 kg maritime)",
     fc=PR, ec=G2, fs=2.5)
rbox(CX2, y2[6], BW+0.3, 0.6, "Trafficking nodes",
     fc=G4, ec=G1, bold=TRUE, thick=TRUE, fs=3.0)

dashbox(SS2_X2-0.65, SS2_X2-0.05, 22.6, 24.4,
        "Repeat\nfor each\nyear\n2017–22", ec=G2)

arr(CX2-1.15, y2[1]-0.28, CX2-0.5, y2[2]+0.25)
arr(CX2+1.15, y2[1]-0.28, CX2+0.5, y2[2]+0.25)
arr(CX2, y2[2]-0.25, CX2, y2[3]+0.25)
arr(CX2, y2[3]-0.25, CX2, y2[4]+0.275)
arr(CX2, y2[4]-0.275, CX2, y2[5]+0.35)
arr(CX2, y2[5]-0.35,  CX2, y2[6]+0.30)

# ════════════════════════════════════════════════════════════════════════════════
# SS3: RESISTANCE SURFACE
# ════════════════════════════════════════════════════════════════════════════════
y3 <- c(27.7, 26.0, 24.8, 23.45, 21.85)

pbox(CX3, y3[1], BW+0.3, 1.25,
     "  \u2022 Rivers (HydroSHEDS / IGAC)\n  \u2022 Roads (primary, secondary, tertiary, tracks)\n  \u2022 Ports & maritime access points\n  \u2022 Military battalions  \u2022 Water bodies\n  \u2022 Digital Elevation Model (NASADEM)",
     fc=G5, ec=G1, fs=2.1, sk=0.3)
rbox(CX3, y3[2], BW+0.3, BH,   "Rasterize vector layer\nto common grid resolution",
     fc=PR, ec=G2, fs=2.6)
rbox(CX3, y3[3], BW+0.3, 0.55, "Assign dominant category to each pixel\nfollowing fixed priority order",
     fc=PR, ec=G2, fs=2.6)
# Two parallel intermediate outputs
rbox(CX3-1.15, y3[4], 2.0, 0.58, "Categorical\nresistance raster\n(W_cat)", fc=G6, ec=G2, fs=2.3)
rbox(CX3+1.15, y3[4], 1.85, 0.58, "Continuous\nraster\nwith slope",         fc=G6, ec=G2, fs=2.3)
rbox(CX3, y3[5], BW+0.3, 0.6, "Baseline resistance surface — Scenario 1",
     fc=G4, ec=G1, bold=TRUE, thick=TRUE, fs=2.85)

dashbox(SS3_X2-0.60, SS3_X2-0.05, 23.1, 24.95,
        "Repeat\nfor each\nlayer", ec=G2)

arr(CX3, y3[1]-0.625, CX3, y3[2]+0.25)
arr(CX3, y3[2]-0.25,  CX3, y3[3]+0.275)
arr(CX3-0.45, y3[3]-0.275, CX3-1.15, y3[4]+0.29)
arr(CX3+0.45, y3[3]-0.275, CX3+1.15, y3[4]+0.29)
arr(CX3-1.15, y3[4]-0.29,  CX3-0.35, y3[5]+0.30)
arr(CX3+1.15, y3[4]-0.29,  CX3+0.35, y3[5]+0.30)

# ════════════════════════════════════════════════════════════════════════════════
# SECTION 4a: ASSIGNMENT
# ════════════════════════════════════════════════════════════════════════════════
ya <- c(15.55, 14.2, 12.95, 11.35)

rbox(CX_A, ya[1], 5.6, BH,   "For each coca cultivation origin point",
     fc=PR, ec=P1)
dbox(CX_A, ya[2], 4.2, 0.82, "k = 20 nearest nodes\n(selected threshold)",
     fc=DC, ec=P1, fs=2.5)
rbox(CX_A, ya[3], 5.4, BH,   "Identify 20 closest trafficking nodes",
     fc=PR, ec=P1)
rbox(CX_A, ya[4], 5.5, 0.65, "Assigned origin\u2013trafficking\nnode pairs",
     fc=P3, ec=P1, bold=TRUE, thick=TRUE, fs=3.0)

arr(CX_A, ya[1]-0.25, CX_A, ya[2]+0.41, col_=P1)
arr(CX_A, ya[2]-0.41, CX_A, ya[3]+0.25, col_=P1)
arr(CX_A, ya[3]-0.25, CX_A, ya[4]+0.325, col_=P1)

# ════════════════════════════════════════════════════════════════════════════════
# SECTION 4b: RESISTANCE SCENARIOS
# ════════════════════════════════════════════════════════════════════════════════
ys <- c(15.55, 14.55, 13.55, 12.1, 10.5)

rbox(CX_S, ys[1], 5.6, BH,   "Start from baseline categorical\nresistance raster",
     fc=PR, ec=O1, fs=2.7)
rbox(CX_S, ys[2], 5.4, BH,   "Modify mobility and\nrestriction categories",
     fc=PR, ec=O1, fs=2.7)
rbox(CX_S, ys[3], 5.6, BH,   "Integrate modified categorical\nraster with slope factor",
     fc=PR, ec=O1, fs=2.7)
dbox(CX_S, ys[4], 5.0, 0.90,
     "Evaluate 20 candidate scenarios\n(RNA \u00b7 NAL \u00b7 MMD metrics)\n\u2192 converges at S = 14",
     fc=DC, ec=O1, fs=2.3)
rbox(CX_S, ys[5], 3.2, 0.70, "14 scenarios",
     fc=O3, ec=O1, bold=TRUE, thick=TRUE, fs=3.5)

dashbox(13.25, 13.82, 12.85, 15.25, "Repeat\nfor 14\nscenarios", ec=O1)

arr(CX_S, ys[1]-0.25, CX_S, ys[2]+0.25, col_=O1)
arr(CX_S, ys[2]-0.25, CX_S, ys[3]+0.25, col_=O1)
arr(CX_S, ys[3]-0.25, CX_S, ys[4]+0.45, col_=O1)
arr(CX_S, ys[4]-0.45, CX_S, ys[5]+0.35, col_=O1)

# ════════════════════════════════════════════════════════════════════════════════
# SECTION 4c: LCP
# ════════════════════════════════════════════════════════════════════════════════
yl <- c(7.95, 7.25, 6.55, 5.65)

rbox(CX_L, yl[1], 5.2, BH,   "Minimize accumulated resistance with ArcGIS",
     fc=PR, ec=PU1)
rbox(CX_L, yl[2], 4.8, BH,   "Generate optimal path",      fc=PR, ec=PU1)
rbox(CX_L, yl[3], 4.4, BH,   "Store LCP polyline",          fc=PR, ec=PU1)
rbox(CX_L, yl[4], 6.0, 0.58,
     "All modeled LCP routes  (~36,359 km avg/year)",
     fc=PU3, ec=PU1, bold=TRUE, thick=TRUE, fs=3.0)

dashbox(10.6, 13.55, 5.3, 7.75, "Repeat for all\nyears and\n14 scenarios", ec=PU1)

arr(CX_L, yl[1]-0.25, CX_L, yl[2]+0.25, col_=PU1)
arr(CX_L, yl[2]-0.25, CX_L, yl[3]+0.25, col_=PU1)
arr(CX_L, yl[3]-0.25, CX_L, yl[4]+0.29, col_=PU1)

# ════════════════════════════════════════════════════════════════════════════════
# SECTION 5: EPOF
# ════════════════════════════════════════════════════════════════════════════════
ye <- c(3.85, 3.22, 2.59, 1.92, 1.27, 0.58)

rbox(CX_E, ye[1], 5.2, BH,   "Decompose routes into segments",                  fc=PR, ec=B1)
rbox(CX_E, ye[2], 5.2, BH,   "Identify unique geometric segments",              fc=PR, ec=B1)
rbox(CX_E, ye[3], 6.4, BH,   "Count how many times each segment appears across scenarios", fc=PR, ec=B1, fs=2.6)
rbox(CX_E, ye[4], 6.6, 0.55,
     "Assign segment confidence class  (5 levels: p10 \u00b7 p25 \u00b7 p50 \u00b7 p75 \u00b7 p95)",
     fc=PR, ec=B1, fs=2.5)
rbox(CX_E, ye[5], 4.8, BH,   "Filter segments with confidence \u2265 25",       fc=PR, ec=B1)
rbox(CX_E, ye[6], 5.4, 0.6,  "High-confidence trafficking corridors\n(segment-level)",
     fc=B3, ec=B1, bold=TRUE, thick=TRUE, fs=3.0)

arr(CX_E, ye[1]-0.25, CX_E, ye[2]+0.25, col_=B1)
arr(CX_E, ye[2]-0.25, CX_E, ye[3]+0.25, col_=B1)
arr(CX_E, ye[3]-0.25, CX_E, ye[4]+0.275, col_=B1)
arr(CX_E, ye[4]-0.275, CX_E, ye[5]+0.25, col_=B1)
arr(CX_E, ye[5]-0.25, CX_E, ye[6]+0.30, col_=B1)

# ════════════════════════════════════════════════════════════════════════════════
# INTER-SECTION ARROWS
# ════════════════════════════════════════════════════════════════════════════════

# Origin points → Assignment (for each origin)
arr(CX1, y1[6]-0.30, CX_A, ya[1]+0.25)

# Trafficking nodes → Assignment (identify closest)
arr(CX2, y2[6]-0.30, CX_A+0.3, ya[3]+0.25)

# Baseline surface → Resistance Scenarios (dashed, feeds Scenario 1)
arr(CX3, y3[5]-0.30, CX_S+0.6, ys[1]+0.25, col_=G2, dash=TRUE)
add_text(annotate("text", x=12.0, y=17.8, label="feeds Scenario 1",
                  color=G2, size=2.1, hjust=0.5, fontface="italic", family="sans"))

# Assigned pairs → LCP (two arrows converging)
arr(CX_A, ya[4]-0.325, CX_L-2.2, yl[1]+0.25, col_=P1)
# 14 scenarios → LCP
arr(CX_S, ys[5]-0.35,  CX_L+2.2, yl[1]+0.25, col_=O1)

# LCP → EPOF
arr(CX_L, yl[4]-0.29, CX_E, ye[1]+0.25, col_=PU1)

# EPOF → Temporal classification (below)
arr(CX_E, ye[6]-0.30, CX_E, -0.45, col_=B1)

# ════════════════════════════════════════════════════════════════════════════════
# TEMPORAL CLASSIFICATION (below EPOF)
# ════════════════════════════════════════════════════════════════════════════════
rbox(CX_E, -0.82, 12.5, 0.65,
     "Temporal classification \u2014  Persistent (8,465 km)  \u00b7  Regular (20,530 km)  \u00b7  Emerging (11,555 km)  \u00b7  Declining (4,738 km)",
     fc=B3, ec=B1, thick=TRUE, fs=2.6)

# ════════════════════════════════════════════════════════════════════════════════
# LEGEND
# ════════════════════════════════════════════════════════════════════════════════
LY <- -1.85   # legend center y
add_bg(annotate("rect", xmin=0.15, xmax=13.85, ymin=LY-0.62, ymax=LY+0.75,
                fill="#f5f5f5", color="#9e9e9e", linewidth=0.5))
add_text(annotate("text", x=0.35, y=LY+0.57, label="Legend",
                  color="#555555", fontface="bold", size=3.2, hjust=0,
                  family="sans"))

lxs <- c(1.3, 3.5, 5.8, 8.1, 10.5, 12.9)
pbox(lxs[1], LY,   1.85, 0.42, "Input data",          fc=G5, ec=G1,  fs=2.2)
rbox(lxs[2], LY,   1.90, 0.42, "Process step",        fc=PR, ec=AR,  fs=2.2)
dbox(lxs[3], LY,   1.80, 0.56, "Decision /\nParameter", fc=DC, ec=AR, fs=2.1)
rbox(lxs[4], LY,   2.10, 0.42, "Intermediate output", fc=G6, ec=G2,  fs=2.1)
rbox(lxs[5], LY,   2.00, 0.42, "Final output",        fc=B3, ec=B1,  thick=TRUE, fs=2.1)
dashbox(lxs[6]-1.05, lxs[6]+1.05, LY-0.28, LY+0.28, "Iterative procedure", ec=AR)

# ════════════════════════════════════════════════════════════════════════════════
# BUILD PLOT
# ════════════════════════════════════════════════════════════════════════════════
all_layers <- c(bg_layers, node_layers, arr_layers, text_layers)

p <- ggplot()
for (l in all_layers) p <- p + l
p <- p +
  coord_cartesian(xlim=c(0, 14), ylim=c(-2.55, 29.55), expand=FALSE) +
  theme_void() +
  theme(plot.background = element_rect(fill="#F8F9FA", color=NA),
        plot.margin     = margin(8, 8, 8, 8))

# ── Save ─────────────────────────────────────────────────────────────────────
out <- "C:/Users/diana/OneDrive - Universidad de los Andes/Diana_CESED/rutas/modeling_routes_CESED/06_results/"

ggsave(paste0(out, "flowchart_improved.png"), p,
       width=14, height=32, dpi=200, bg="#F8F9FA")
ggsave(paste0(out, "flowchart_improved.pdf"), p,
       width=14, height=32, bg="#F8F9FA")

cat("Guardado en:", out, "\n")
