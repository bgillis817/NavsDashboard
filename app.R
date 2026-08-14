# ============================================================================
#  NORTH SHORE NAVIGATORS — COMBINED DASHBOARD
#  2025 / 2026 season toggle  ·  Hitters + Pitchers
#  Dark theme · pitch sequencing matrix · count & handedness heat maps
# ============================================================================

library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(DT)
library(scales)
library(digest)   # SHA-256 for the access password

# Define before anything else so process functions can use it
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# ── Data URLs ────────────────────────────────────────────────────────────────
# Single files containing all seasons — filtered by Season column after load
HITTERS_URL  <- "https://raw.githubusercontent.com/bgillis817/NavsDashboard/refs/heads/Offense/NavsPAs.csv"
PITCHERS_URL <- "https://raw.githubusercontent.com/bgillis817/NavsDashboard/refs/heads/Pitching/NavigatorsPitchers.csv"

NAVS_LOGO <- "https://nsnavs.com.ismmedia.com/ISM3/std-content/repos/Top/Team/Opponents/navs_logo_new.png"

# ── Lookup tables ─────────────────────────────────────────────────────────────
woba_weights <- c(
  Walk=0.690, HitByPitch=0.690, Single=0.888,
  Double=1.271, Triple=1.616, HomeRun=2.101,
  Out=0, FieldersChoice=0, Sacrifice=0, Error=0
)

team_names <- c(
  UPP_VAL="Upper Valley Nighthawks", VAL_BLU="Valley Blue Sox",
  KEE_SWA="Keene Swampbats",         BRI_B="Bristol Blues",
  MAR_VIN="Martha's Vineyard Sharks",OCE_STA6="Ocean State Waves",
  NOR_ADA="North Adams Steeplecats", MYS_SCH="Mystic Schooners",
  NEW_GUL="Newport Gulls",           SAN_MAI="Sanford Mainers",
  VER_MOU="Vermont Mountaineers",    DAN_WES="Danbury Westerners"
)

pitch_pal <- c(
  Fastball="#ff4655","4-Seam Fastball"="#ff4655","Four-Seam"="#ff4655",
  Sinker="#ff8c42","Two-Seam"="#ff8c42",
  Cutter="#ffd700",Slider="#00d4ff",
  Curveball="#a78bfa",Sweeper="#34d399",
  Changeup="#6ee7b7",Splitter="#f472b6",
  Other="#94a3b8"
)

# ── Scouting report: pitch-type normalization ────────────────────────────────
# The Navs pitcher feed carries duplicate tags for the same pitch (Fastball vs
# 4-Seam, Sinker vs 2-Seam, Changeup vs ChangeUp vs KickCH). Left alone these
# split a by-pitch-type report into half-populated rows. Anything not listed
# here passes through untouched.
PITCH_ALIAS <- c(
  "4-Seam"="Fastball", "Four-Seam"="Fastball", "FourSeamFastBall"="Fastball",
  "FourSeamFastball"="Fastball", "Fastball"="Fastball",
  "2-Seam"="Sinker", "TwoSeamFastBall"="Sinker", "Two-Seam"="Sinker",
  "Sinker"="Sinker",
  "ChangeUp"="Changeup", "KickCH"="Changeup", "Changeup"="Changeup",
  "GyroSL"="Slider", "Slider"="Slider"
)
normalize_pitch <- function(x, on = TRUE) {
  x <- as.character(x)
  if (!isTRUE(on)) return(x)
  hit <- PITCH_ALIAS[x]
  ifelse(is.na(hit), x, unname(hit))
}

# pitch_pal doesn't cover every tag in the feed (Slurve, Knuckleball, ...).
# Fill the gaps from team_pal so a manual colour scale never drops a series.
sr_pitch_pal <- function(levels) {
  levels <- unique(as.character(levels[!is.na(levels)]))
  out <- pitch_pal[levels]
  names(out) <- levels
  miss <- which(is.na(out))
  if (length(miss)) {
    extra <- rep(team_pal, length.out = length(miss))
    out[miss] <- extra
  }
  out
}

# ── Scouting report: section catalog (pitchers) ──────────────────────────────
sr_sections_all <- c(
  "Summary Cards"                    = "kpi",
  "Arsenal Table"                    = "arsenal",
  "Results (Cumulative)"             = "res_all",
  "Results by Pitch Type"            = "res_pt",
  "Results vs L/R"                   = "res_lr",
  "Results by Pitch x Hand"          = "res_pt_lr",
  "Plate Discipline (Cumulative)"    = "pd_all",
  "Plate Discipline by Pitch Type"   = "pd_pt",
  "Plate Discipline vs L/R"          = "pd_lr",
  "Plate Discipline by Pitch x Hand" = "pd_pt_lr",
  "Batted Ball (Cumulative)"         = "bb_all",
  "Batted Ball by Pitch Type"        = "bb_pt",
  "Batted Ball vs L/R"               = "bb_lr",
  "Batted Ball by Pitch x Hand"      = "bb_pt_lr",
  "Velocity & Spin Table"            = "velo_spin",
  "Movement Chart"                   = "movement",
  "Release Points"                   = "release",
  "Heat Maps (vs RHH)"               = "hm_r",
  "Heat Maps (vs LHH)"               = "hm_l",
  "Heat Maps (Combined)"             = "hm_c"
)

team_pal <- c(
  "#ff4655","#4ECDC4","#45B7D1","#96CEB4","#FECA57",
  "#48D1CC","#FA8072","#DDA0DD","#98D8C8","#F7DC6F",
  "#85C1E5","#F8C471","#82E0AA","#D7BDE2","#A9DFBF"
)

# ── Count helpers ─────────────────────────────────────────────────────────────
count_situation <- function(b, s) {
  dplyr::case_when(
    b==0 & s==0          ~ "First Pitch (0-0)",
    b>=2 & s<=1          ~ "Hitter's Count",
    s==2 & b<=1          ~ "Pitcher's Count",
    b==s                 ~ "Even Count",
    b>s                  ~ "Hitter's Ahead",
    TRUE                 ~ "Pitcher's Ahead"
  )
}

# ── Data processing functions (accept pre-filtered data frames) ───────────────
process_hitters <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  tryCatch({
    df <- df %>%
      mutate(
        Date            = as.Date(Date, format="%m/%d/%Y"),
        PitcherTeamFull = ifelse(PitcherTeam %in% names(team_names),
                                 team_names[PitcherTeam], PitcherTeam),
        wOBA_contribution = dplyr::case_when(
          KorBB=="Walk"               ~ woba_weights["Walk"],
          PitchCall=="HitByPitch"     ~ woba_weights["HitByPitch"],
          PlayResult=="Single"        ~ woba_weights["Single"],
          PlayResult=="Double"        ~ woba_weights["Double"],
          PlayResult=="Triple"        ~ woba_weights["Triple"],
          PlayResult=="HomeRun"       ~ woba_weights["HomeRun"],
          PlayResult=="FieldersChoice"~ woba_weights["FieldersChoice"],
          PlayResult=="Sacrifice"     ~ woba_weights["Sacrifice"],
          PlayResult=="Error"         ~ woba_weights["Error"],
          TRUE                        ~ woba_weights["Out"]
        ),
        Balls   = as.double(substr(as.character(Balls),   1, 10)),
        Strikes = as.double(substr(as.character(Strikes), 1, 10)),
        CountSit   = count_situation(Balls, Strikes),
        CountIndiv = paste0(Balls, "-", Strikes)
      ) %>%
      arrange(Batter, PitchNo) %>%
      group_by(Batter) %>%
      mutate(PA_count        = row_number(),
             cumulative_wOBA = cumsum(wOBA_contribution) / PA_count) %>%
      ungroup() %>%
      arrange(Batter, PitcherTeamFull, PitchNo) %>%
      group_by(Batter, PitcherTeamFull) %>%
      mutate(PA_count_opp        = row_number(),
             cumulative_wOBA_opp = cumsum(wOBA_contribution) / PA_count_opp) %>%
      ungroup()
    df
  }, error=function(e) { message("Hitter load error: ", e$message); NULL })
}

process_pitchers <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  tryCatch({
    names(df) <- gsub("\\s+","_",names(df)); names(df) <- gsub("[()]","",names(df))

    df <- df %>%
      mutate(
        Date             = as.Date(Date, format="%m/%d/%Y"),
        RelHeight        = as.double(RelHeight),
        RelSide          = as.double(RelSide),
        RelSpeed         = as.double(RelSpeed),
        SpinRate         = as.double(SpinRate),
        InducedVertBreak = as.double(InducedVertBreak),
        HorzBreak        = as.double(HorzBreak),
        PlateLocHeight   = as.double(PlateLocHeight),
        PlateLocSide     = as.double(PlateLocSide),
        Balls   = as.double(substr(as.character(Balls),   1, 10)),
        Strikes = as.double(substr(as.character(Strikes), 1, 10)),
        CountSit   = count_situation(Balls, Strikes),
        CountIndiv = paste0(Balls, "-", Strikes),
        SwingCheck     = PitchCall %in% c("FoulBall","StrikeSwinging","InPlay"),
        WhiffCheck     = PitchCall == "StrikeSwinging",
        CSWCheck       = PitchCall %in% c("StrikeSwinging","StrikeCalled"),
        ZoneCheck      = between(PlateLocHeight,1.59,3.41) & between(PlateLocSide,-1,1),
        StrikeoutCheck = KorBB == "Strikeout",
        WalkCheck      = KorBB == "Walk",
        HBPCheck       = PitchCall == "HitByPitch",
        HCheck         = PlayResult %in% c("Single","Double","Triple","HomeRun"),
        BBECheck       = TaggedHitType %in% c("GroundBall","LineDrive","FlyBall","Popup"),
        SacCheck       = PlayResult == "Sacrifice",
        BIPCheck       = PlayResult != "Undefined",
        PACheck        = StrikeoutCheck + WalkCheck + HBPCheck + BIPCheck,
        ABCheck        = StrikeoutCheck + BIPCheck - SacCheck
      ) %>%
      arrange(Pitcher, Date, PitchNo) %>%
      group_by(Pitcher) %>%
      mutate(OverallPitchCount = row_number()) %>%
      ungroup()

    # Pitch-pair sequences
    seqs <- df %>%
      arrange(Pitcher, Date, PitchNo) %>%
      group_by(Pitcher, Date) %>%
      mutate(
        prev_type        = lag(TaggedPitchType),
        prev_loc_side    = lag(PlateLocSide),
        prev_loc_height  = lag(PlateLocHeight)
      ) %>%
      ungroup() %>%
      filter(!is.na(prev_type))

    pairs <- seqs %>%
      group_by(Pitcher, prev_type, TaggedPitchType) %>%
      summarise(
        n_pairs    = n(),
        csw_rate   = mean(CSWCheck,  na.rm=TRUE),
        whiff_rate = {sw=sum(SwingCheck,na.rm=TRUE); if(sw>0) sum(WhiffCheck,na.rm=TRUE)/sw else NA_real_},
        zone_rate  = mean(ZoneCheck, na.rm=TRUE),
        chase_rate = {oz=sum(!ZoneCheck,na.rm=TRUE); if(oz>0) sum(SwingCheck[!ZoneCheck],na.rm=TRUE)/oz else NA_real_},
        .groups="drop"
      )

    list(data=df, sequences=seqs, pairs=pairs)
  }, error=function(e) { message("Pitcher load error: ", e$message); NULL })
}

# ── Robust CSV fetch: retry w/ exponential backoff (handles raw 429/503) ─────
read_csv_retry <- function(url, max_tries = 5, base_wait = 1.5) {
  for (i in seq_len(max_tries)) {
    res <- tryCatch(read_csv(url, show_col_types = FALSE),
                    error = function(e) e)
    if (!inherits(res, "error")) return(res)
    if (i < max_tries) {
      wait <- base_wait * (2 ^ (i - 1)) + runif(1, 0, 0.5)  # exp backoff + jitter
      message(sprintf("  fetch failed (try %d/%d): %s — retrying in %.1fs",
                      i, max_tries, conditionMessage(res), wait))
      Sys.sleep(wait)
    } else {
      message("  giving up on ", url, ": ", conditionMessage(res))
    }
  }
  NULL
}

# ── Load raw CSVs once, then split by Season column ──────────────────────────
message("Loading data files...")

raw_hitters_all  <- read_csv_retry(HITTERS_URL)
Sys.sleep(1)  # stagger reads to avoid hammering raw.githubusercontent back-to-back
raw_pitchers_all <- read_csv_retry(PITCHERS_URL)

# Helper: filter a raw df to one season using the year in the Date column
slice_season <- function(df, yr) {
  if (is.null(df)) return(NULL)
  df %>% filter(as.integer(format(as.Date(Date, format="%m/%d/%Y"), "%Y")) == as.integer(yr))
}

seasons <- list(
  `2026` = list(
    hitters  = process_hitters(slice_season(raw_hitters_all,  2026)),
    pitchers = process_pitchers(slice_season(raw_pitchers_all, 2026))
  ),
  `2025` = list(
    hitters  = process_hitters(slice_season(raw_hitters_all,  2025)),
    pitchers = process_pitchers(slice_season(raw_pitchers_all, 2025))
  )
)

# ── Helpers ──────────────────────────────────────────────────────────────────

stat_card <- function(value, label) {
  tags$div(class="stat-card",
    tags$h2(value),
    tags$p(label)
  )
}

slg_stats <- function(d) {
  d %>% summarise(
    # Trackman records walks in KorBB and HBP in PitchCall — NOT in PlayResult
    # (PlayResult is "Undefined" on those rows). Count each from its own column.
    # PA counts terminal events only, so baserunning rows (StolenBase /
    # CaughtStealing) don't inflate the denominator.
    K   = sum(KorBB=="Strikeout", na.rm=TRUE),
    BB  = sum(KorBB=="Walk", na.rm=TRUE),
    HBP = sum(PitchCall=="HitByPitch", na.rm=TRUE),
    Sac = sum(PlayResult=="Sacrifice", na.rm=TRUE),
    BIP = sum(PlayResult %in% c("Single","Double","Triple","HomeRun","Out",
                                "Error","FieldersChoice","Sacrifice"), na.rm=TRUE),
    PA  = K + BB + HBP + BIP,
    H   = sum(PlayResult %in% c("Single","Double","Triple","HomeRun")),
    `1B`= sum(PlayResult=="Single"), `2B`=sum(PlayResult=="Double"),
    `3B`= sum(PlayResult=="Triple"), HR=sum(PlayResult=="HomeRun"),
    AB  = PA-BB-HBP-Sac,
    TB  = `1B`+2*`2B`+3*`3B`+4*HR,
    BA  = ifelse(AB>0, round(H/AB,3),  NA),
    OBP = ifelse((AB+BB+HBP+Sac)>0,
                 round((H+BB+HBP)/(AB+BB+HBP+Sac),3), NA),
    SLG = ifelse(AB>0, round(TB/AB,3), NA),
    wOBA= ifelse((AB+BB+HBP+Sac)>0,
                 round(sum(wOBA_contribution,na.rm=TRUE)/(AB+BB+HBP+Sac),3), NA)
  ) %>% filter(PA>0)
}

dt_opts <- list(
  dom="t", pageLength=30, scrollX=TRUE,
  initComplete=JS(
    "function(s,j){",
    "$(this.api().table().header()).css({'background':'#1e2235','color':'#b0b8d4'});",
    "}"
  )
)

# ── Dark CSS ──────────────────────────────────────────────────────────────────
dark_css <- "
body{background:#0f1117!important;color:#e8eaf0!important;
     font-family:'Segoe UI',-apple-system,BlinkMacSystemFont,sans-serif;}
.navbar{background:#141720!important;border-bottom:1px solid #2a2d3a!important;}
.navbar-brand{color:#fff!important;font-weight:700;font-size:18px;}
.navbar-nav>li>a{color:#b0b8d4!important;font-weight:500;padding:14px 18px!important;}
.navbar-nav>li.active>a,.navbar-nav>li>a:hover{color:#ff4655!important;
  border-bottom:2px solid #ff4655!important;}
.well,.panel,.sidebar-panel,.main-panel{background:#141720!important;border:none!important;}
.panel-default{background:#1a1e2e!important;border:1px solid #2a2d3a!important;border-radius:8px;}
.panel-default>.panel-heading{background:#1e2235!important;color:#fff!important;
  border-bottom:1px solid #2a2d3a;}
.form-control,select.form-control{background:#1e2235!important;color:#e8eaf0!important;
  border:1px solid #2e3350!important;border-radius:6px!important;}
.form-control:focus{border-color:#ff4655!important;
  box-shadow:0 0 0 2px rgba(255,70,85,.25)!important;}
label{color:#b0b8d4!important;font-size:12px;font-weight:600;
  letter-spacing:.5px;text-transform:uppercase;}
.btn-default{background:#1e2235!important;color:#e8eaf0!important;
  border:1px solid #2e3350!important;border-radius:6px!important;}
.btn-default:hover{background:#ff4655!important;border-color:#ff4655!important;color:#fff!important;}
.btn-primary{background:#ff4655!important;border-color:#ff4655!important;
  color:#fff!important;border-radius:6px!important;}
.nav-tabs{border-bottom:1px solid #2a2d3a!important;}
.nav-tabs>li>a{background:#1a1e2e!important;color:#8892b0!important;
  border-color:#2a2d3a!important;border-radius:6px 6px 0 0!important;}
.nav-tabs>li.active>a,.nav-tabs>li>a:hover{background:#ff4655!important;
  color:#fff!important;border-color:#ff4655!important;}
.tab-content{background:#141720!important;border:1px solid #2a2d3a!important;
  border-top:none;padding:20px;border-radius:0 0 8px 8px;}
.dataTables_wrapper{color:#e8eaf0!important;}
table.dataTable thead th{background:#1e2235!important;color:#b0b8d4!important;
  border-bottom:1px solid #2a2d3a!important;}
table.dataTable tbody tr{background:#141720!important;color:#e8eaf0!important;}
table.dataTable tbody tr:nth-child(even){background:#1a1e2e!important;}
table.dataTable tbody tr:hover{background:#1e2235!important;}
.dataTables_filter input,.dataTables_length select{background:#1e2235!important;
  color:#e8eaf0!important;border:1px solid #2e3350!important;}
.stat-card{background:#1a1e2e;border:1px solid #2a2d3a;border-radius:10px;
  padding:18px 20px;margin-bottom:14px;}
.stat-card h2{color:#ff4655;margin:0 0 4px;font-size:28px;font-weight:700;}
.stat-card p{color:#8892b0;margin:0;font-size:12px;text-transform:uppercase;letter-spacing:.5px;}
.section-header{color:#fff;font-size:18px;font-weight:700;border-left:3px solid #ff4655;
  padding-left:12px;margin:20px 0 4px;}
.section-sub{color:#8892b0;font-size:12px;margin:0 0 16px 15px;}
/* Scouting report preview tables */
.sr-tbl{width:100%;border-collapse:collapse;font-size:12px;margin:0 0 6px 0;}
.sr-tbl th{background:#1a1d2a;color:#e8eaf0;font-weight:600;text-align:center;
  padding:6px 10px;border:1px solid #2a2d3a;white-space:nowrap;}
.sr-tbl td{color:#c3c9dd;text-align:center;padding:5px 10px;
  border:1px solid #22252f;white-space:nowrap;}
.sr-tbl tbody tr:nth-child(even){background:#141720;}
.sr-tbl tbody tr:hover{background:#1f2333;}
.filter-bar{background:#1a1e2e;border:1px solid #2a2d3a;border-radius:8px;
  padding:14px 18px;margin-bottom:18px;}
/* Data-load error banner */
.load-error{background:#3a1a1e;border:1px solid #ff4655;border-radius:8px;
  padding:14px 18px;margin-bottom:16px;color:#ff9aa2;font-weight:600;font-size:14px;}
/* Season toggle pill */
.season-toggle{display:flex;gap:8px;margin-bottom:6px;}
.season-btn{padding:6px 20px;border-radius:20px;border:1px solid #2e3350;
  background:#1e2235;color:#8892b0;cursor:pointer;font-size:13px;font-weight:600;
  transition:all .2s;}
.season-btn.active{background:#ff4655;border-color:#ff4655;color:#fff;}
.checkbox label,.radio label{color:#b0b8d4!important;}
input[type=checkbox],input[type=radio]{accent-color:#ff4655;}
hr{border-color:#2a2d3a!important;}
h4,h5{color:#e8eaf0!important;}
.col-sm-3{background:#0f1117!important;}
::-webkit-scrollbar{width:6px;height:6px;}
::-webkit-scrollbar-track{background:#0f1117;}
::-webkit-scrollbar-thumb{background:#2a2d3a;border-radius:3px;}
"

# ── ggplot dark theme ─────────────────────────────────────────────────────────
theme_navs <- function(base_size=12) {
  theme_minimal(base_size=base_size) %+replace% theme(
    plot.background  =element_rect(fill="#0f1117",color=NA),
    panel.background =element_rect(fill="#141720",color=NA),
    panel.grid.major =element_line(color="#2a2d3a",linewidth=.3),
    panel.grid.minor =element_blank(),
    axis.text        =element_text(color="#8892b0",size=9),
    axis.title       =element_text(color="#b0b8d4",size=10,face="bold"),
    plot.title       =element_text(color="#ffffff",size=14,face="bold",hjust=.5),
    plot.subtitle    =element_text(color="#8892b0",size=10,hjust=.5),
    legend.background=element_rect(fill="#1a1e2e",color=NA),
    legend.key       =element_rect(fill="#1a1e2e",color=NA),
    legend.text      =element_text(color="#b0b8d4",size=9),
    legend.title     =element_text(color="#e8eaf0",size=10,face="bold"),
    strip.background =element_rect(fill="#1e2235",color=NA),
    strip.text       =element_text(color="#e8eaf0",size=9,face="bold")
  )
}

heat_fills <- c("#141720","#1a3a5c","#1565c0","#ff4655","#ffeb3b")

# ── ggplotly wrapper: hover tooltips + dark styling ──────────────────────────
to_dark_plotly <- function(gg) {
  plotly::ggplotly(gg, tooltip="text") %>%
    plotly::layout(paper_bgcolor="#0f1117", plot_bgcolor="#141720",
                   font=list(color="#b0b8d4"),
                   legend=list(font=list(color="#b0b8d4"))) %>%
    plotly::config(displaylogo=FALSE)
}

# ── Location heatmap helper ───────────────────────────────────────────────────
loc_heatmap <- function(d, title_str, subtitle_str="", flip_side=FALSE) {
  if (is.null(d) || nrow(d) < 5) {
    return(ggplot() +
      annotate("text",x=0,y=2.5,label="Insufficient data",color="#8892b0",size=5) +
      theme_navs() +
      theme(axis.text=element_blank(),axis.title=element_blank(),panel.grid=element_blank()))
  }
  x_col <- if (flip_side) -d$PlateLocSide else d$PlateLocSide
  ggplot(data.frame(x=x_col, y=d$PlateLocHeight),
         aes(x=x, y=y)) +
    stat_density_2d(aes(fill=after_stat(density)),geom="raster",contour=FALSE) +
    scale_fill_gradientn(colours=heat_fills,guide="none") +
    annotate("rect",xmin=-1,xmax=1,ymin=1.6,ymax=3.4,
             fill=NA,color="#ffffff",linewidth=.7) +
    geom_vline(xintercept=0,linewidth=.3,color="#2a2d3a",linetype="dashed") +
    geom_hline(yintercept=2.5,linewidth=.3,color="#2a2d3a",linetype="dashed") +
    ylim(1,4) + xlim(-2,2) +
    labs(title=title_str, subtitle=subtitle_str,
         x=if(flip_side)"Horizontal (Hitter's View)" else "Horizontal (Pitcher's View)",
         y="Vertical") +
    theme_navs()
}

# ── Count dropdown choices builder ───────────────────────────────────────────
count_choices <- function(d_col) {
  indiv   <- sort(unique(d_col))
  grouped <- c("Hitter's Count","Pitcher's Count","Even Count",
                "First Pitch (0-0)","Hitter's Ahead","Pitcher's Ahead")
  c("All Counts",
    setNames("──grouped──","── Grouped ──"),
    setNames(grouped,grouped),
    setNames("──indiv──","── Individual ──"),
    setNames(indiv, indiv))
}

filter_by_count <- function(d, cnt_val, sit_col, indiv_col) {
  if (is.null(cnt_val) || cnt_val %in% c("All Counts","──grouped──","──indiv──")) return(d)
  grouped_lvls <- c("Hitter's Count","Pitcher's Count","Even Count",
                    "First Pitch (0-0)","Hitter's Ahead","Pitcher's Ahead")
  if (cnt_val %in% grouped_lvls) d %>% filter(.data[[sit_col]]   == cnt_val)
  else                            d %>% filter(.data[[indiv_col]] == cnt_val)
}

# ============================================================================
#  UI
# ============================================================================
# ── App-wide access password ─────────────────────────────────────────────────
# The password is stored ONLY as a SHA-256 hash, so this file is safe to keep
# in a public repo — the hash can't be reversed into the password.
#
# To set or change the password, run this locally in R and paste the result:
#   digest::digest("your-password-here", algo = "sha256", serialize = FALSE)
#
APP_PASSWORD_SHA256 <- "822cde38bb48c064a685efdd41ec2c9fb58216adf71837a89b96d993bed53647"

# Optional overrides (used if present) — a bundled file or an env var. These let
# a deploy pipeline inject the password later without editing this file.
get_app_password_hash <- function() {
  bundled <- "app_password.txt"
  if (file.exists(bundled)) {
    pw <- trimws(readLines(bundled, n = 1, warn = FALSE))
    if (nchar(pw) > 0)
      return(digest::digest(pw, algo = "sha256", serialize = FALSE))
  }
  ev <- Sys.getenv("APP_PASSWORD")
  if (nchar(ev) > 0)
    return(digest::digest(ev, algo = "sha256", serialize = FALSE))
  APP_PASSWORD_SHA256
}

# ── Login screen ─────────────────────────────────────────────────────────────
# The real UI (main_ui) is only ever sent to the browser after a correct
# password, because it is rendered server-side into output$app_gate. Nothing
# about the dashboard is present in the page source before that.
login_css <- "
.login-wrap{display:flex;align-items:center;justify-content:center;
  min-height:100vh;background:#0f1117;font-family:'Inter',system-ui,sans-serif;}
.login-card{background:#1a1e2e;border:1px solid #2a2d3a;border-radius:12px;
  padding:34px 38px;width:340px;text-align:center;
  box-shadow:0 8px 30px rgba(0,0,0,.55);}
.login-card h3{color:#fff;font-weight:700;margin:0 0 6px;font-size:19px;}
.login-card p.sub{color:#8892b0;font-size:12px;margin:0 0 20px;}
.login-card .form-group{margin-bottom:14px;}
.login-card input[type=password]{background:#141720;color:#e8eaf0;
  border:1px solid #2e3350;border-radius:6px;width:100%;padding:8px 12px;}
.login-card .btn{background:#ff4655;color:#fff;border:none;border-radius:6px;
  padding:8px 0;width:100%;font-weight:600;margin-top:4px;}
.login-card .btn:hover{background:#e03e4c;}
.login-err{color:#ff4655;font-size:12px;margin-top:12px;min-height:16px;}
"

login_ui <- tags$div(class="login-wrap",
  tags$div(class="login-card",
    tags$h3("North Shore Navigators"),
    tags$p(class="sub", "Enter the access password to continue."),
    passwordInput("login_pw", NULL, value="", placeholder="Password"),
    actionButton("login_btn", "Enter", class="btn"),
    uiOutput("login_msg"),
    tags$script(HTML("document.addEventListener('keydown',function(e){if(e.key==='Enter'){var b=document.getElementById('login_btn');if(b){b.click();}}});"))
  )
)

main_ui <- navbarPage(
  title = tags$span(
    tags$img(src=NAVS_LOGO, height="30px",
             style="margin-right:10px;vertical-align:middle;"),
    "North Shore Navigators"
  ),
  id="mainNav", collapsible=TRUE,
  header=tags$head(
    tags$style(HTML(dark_css)),
    # JS for season toggle pills
    tags$script(HTML("
      Shiny.addCustomMessageHandler('setSeason', function(s) {
        $('.season-btn').removeClass('active');
        $('#sbtn_' + s).addClass('active');
      });
      Shiny.addCustomMessageHandler('setSeasonP', function(s) {
        $('#pbtn_2026, #pbtn_2025').removeClass('active');
        $('#pbtn_' + s).addClass('active');
      });
      Shiny.addCustomMessageHandler('setSeasonSR', function(s) {
        $('#srbtn_2026, #srbtn_2025').removeClass('active');
        $('#srbtn_' + s).addClass('active');
      });
    "))
  ),

  # ══════════════════════════════════════════════════════════════════════════
  # HITTERS TAB
  # ══════════════════════════════════════════════════════════════════════════
  tabPanel("Hitters",
    sidebarLayout(
      sidebarPanel(width=3,
        # ── Season toggle ──
        tags$div(class="section-header","Season"),
        tags$div(class="season-toggle",
          tags$button("2026", id="sbtn_2026", class="season-btn active",
                      onclick="Shiny.setInputValue('h_season','2026',{priority:'event'})"),
          tags$button("2025", id="sbtn_2025", class="season-btn",
                      onclick="Shiny.setInputValue('h_season','2025',{priority:'event'})")
        ),
        tags$hr(),
        uiOutput("h_playerSelect"),
        uiOutput("h_dateRangeUI"),
        uiOutput("h_opponentUI"),
        uiOutput("h_paRangeUI"),
        uiOutput("h_pitchTypeUI"),
        tags$hr(),
        uiOutput("h_playerInfo")
      ),
      mainPanel(width=9,
        uiOutput("h_dataStatus"),
        tabsetPanel(
          # Overview
          tabPanel("Overview",
            br(),
            fluidRow(
              column(3,uiOutput("h_card_pa")),
              column(3,uiOutput("h_card_woba")),
              column(3,uiOutput("h_card_obp")),
              column(3,uiOutput("h_card_slg"))
            ),
            br(),
            tags$div(class="section-header","Spray Chart"),
            tags$div(class="section-sub","Colored by play result \u00b7 distances in feet"),
            plotOutput("h_spray",height="500px")
          ),
          # Plate Discipline
          tabPanel("Plate Discipline",
            br(),
            tags$div(class="filter-bar",
              fluidRow(
                column(4, uiOutput("h_pd_pitch_ui")),
                column(4, uiOutput("h_pd_count_ui")),
                column(4,
                  radioButtons("h_pd_hand","Pitcher Throws",
                               choices=c("Combined","Right","Left"),
                               selected="Combined",inline=TRUE))
              )
            ),
            fluidRow(
              column(3,uiOutput("h_pd_card_zsw")),
              column(3,uiOutput("h_pd_card_zcon")),
              column(3,uiOutput("h_pd_card_whiff")),
              column(3,uiOutput("h_pd_card_chase"))
            ),
            br(),
            tags$div(class="section-header","Pitch Outcomes by Location"),
            tags$div(class="section-sub",
              HTML(paste0(
                "<span style='color:#4ECDC4;'>&#9679; Contact (zone)</span>&nbsp;&nbsp;",
                "<span style='color:#ff4655;'>&#x2715; Whiff (zone)</span>&nbsp;&nbsp;",
                "<span style='color:#ffd700;'>&#9679; Called Strike</span>&nbsp;&nbsp;",
                "<span style='color:#34d399;'>&#9679; Ball (zone)</span>&nbsp;&nbsp;",
                "<span style='color:#ff8c42;'>&#9679; Chase</span>&nbsp;&nbsp;",
                "<span style='color:#8892b0;'>&#9675; Take (out of zone)</span>"
              ))
            ),
            plotOutput("h_pd_plot",height="520px")
          ),
          # wOBA Trends
          tabPanel("wOBA Trends",
            br(),
            tags$div(class="section-header","Rolling Cumulative wOBA"),
            tags$div(class="section-sub","Reference lines: Average (.320) · Good (.370) · Great (.420)"),
            plotOutput("h_woba",height="340px"),
            br(),
            tags$div(class="section-header","wOBA by Opponent"),
            plotOutput("h_wobaOpp",height="340px")
          ),
          # Splits
          tabPanel("Splits",
            br(),
            tags$div(class="section-header","Stats by Pitch Type"),
            dataTableOutput("h_ptTable"),
            br(),
            tags$div(class="section-header","Righty / Lefty Splits"),
            dataTableOutput("h_lrTable"),
            br(),
            tags$div(class="section-header","Pitch Type \u00d7 Handedness"),
            dataTableOutput("h_ptLrTable")
          ),
          # Batted Ball
          tabPanel("Batted Ball",
            br(),
            tags$div(class="filter-bar",
              fluidRow(
                column(4, uiOutput("h_bb_pitch_ui")),
                column(4,
                  radioButtons("h_bb_hand","Pitcher Throws",
                               choices=c("Combined","Right","Left"),
                               selected="Combined",inline=TRUE))
              )
            ),
            fluidRow(
              column(3, uiOutput("h_bb_card_gb")),
              column(3, uiOutput("h_bb_card_fb")),
              column(3, uiOutput("h_bb_card_ld")),
              column(3, uiOutput("h_bb_card_pu"))
            ),
            br(),
            tags$div(class="section-header","Cumulative Batted Ball Rates"),
            tags$div(class="section-sub","Overall totals for selected filters"),
            dataTableOutput("h_bb_totalTable"),
            br(),
            tags$div(class="section-header","Batted Ball Rates by Pitch Type"),
            dataTableOutput("h_bb_ptTable"),
            br(),
            tags$div(class="section-header","Batted Ball Rates by Handedness"),
            dataTableOutput("h_bb_lrTable"),
            br(),
            tags$div(class="section-header","Batted Ball Rates by Pitch Type \u00d7 Handedness"),
            dataTableOutput("h_bb_ptLrTable")
          ),
          # Heat Maps
          tabPanel("Heat Maps",
            br(),
            tags$div(class="section-header","Strike Zone Heat Maps"),
            tags$div(class="section-sub","Hitter's perspective (plate side flipped)"),
            tags$div(class="filter-bar",
              fluidRow(
                column(4, uiOutput("h_hm_pitch_ui")),
                column(4, uiOutput("h_hm_count_ui")),
                column(4,
                  radioButtons("h_hm_hand","Pitcher Throws",
                               choices=c("Combined","Right","Left"),
                               selected="Combined",inline=TRUE))
              )
            ),
            plotOutput("h_heatmap",height="460px")
          )
        )
      )
    )
  ),

  # ══════════════════════════════════════════════════════════════════════════
  # PITCHERS TAB
  # ══════════════════════════════════════════════════════════════════════════
  tabPanel("Pitchers",
    sidebarLayout(
      sidebarPanel(width=3,
        # ── Season toggle ──
        tags$div(class="section-header","Season"),
        tags$div(class="season-toggle",
          tags$button("2026", id="pbtn_2026", class="season-btn active",
                      onclick="Shiny.setInputValue('p_season','2026',{priority:'event'})"),
          tags$button("2025", id="pbtn_2025", class="season-btn",
                      onclick="Shiny.setInputValue('p_season','2025',{priority:'event'})")
        ),
        tags$hr(),
        uiOutput("p_playerSelect"),
        uiOutput("p_dateUI"),
        uiOutput("p_pitchTypeUI"),
        selectInput("p_batterSide","Batter Side",choices=c("All","Right","Left")),
        uiOutput("p_pitchRangeUI"),
        tags$hr(),
        uiOutput("p_pitcherInfo")
      ),
      mainPanel(width=9,
        uiOutput("p_dataStatus"),
        tabsetPanel(
          # Summary
          tabPanel("Summary",
            br(),
            fluidRow(
              column(3,uiOutput("p_card_n")),
              column(3,uiOutput("p_card_csw")),
              column(3,uiOutput("p_card_whiff")),
              column(3,uiOutput("p_card_zone"))
            ),
            br(),
            tags$div(class="section-header","Pitch Arsenal"),
            dataTableOutput("p_arsenal"),
            br(),
            tags$div(class="section-header","Results by Pitch Type"),
            dataTableOutput("p_results"),
            br(),
            tags$div(class="section-header","L / R Splits"),
            dataTableOutput("p_splits")
          ),
          # Movement & Release
          tabPanel("Movement & Release",
            br(),
            tags$div(class="section-header","Pitch Movement"),
            tags$div(class="section-sub","Horizontal break vs. induced vertical break \u00b7 hover for pitch detail"),
            plotlyOutput("p_movement",height="420px"),
            br(),
            tags$div(class="section-header","Release Points"),
            plotlyOutput("p_release",height="370px")
          ),
          # Pitch Sequencing
          tabPanel("Pitch Sequencing",
            br(),
            tags$div(class="section-header","Back-to-Back Pitch Matrix"),
            tags$div(class="section-sub","Click any cell to see location heatmaps for that pair"),
            tags$div(class="filter-bar",
              fluidRow(
                column(4,
                  selectInput("p_seq_metric","Matrix Metric",
                              choices=c("CSW%"="csw_rate","Whiff%"="whiff_rate",
                                        "Zone%"="zone_rate","Chase%"="chase_rate"))
                ),
                column(4,
                  selectInput("p_seq_hand","Batter Side",choices=c("All","Right","Left"))
                )
              )
            ),
            fluidRow(
              column(7,
                plotOutput("p_seqMatrix",height="520px",click="p_mat_click")
              ),
              column(5,
                tags$div(class="section-header",style="font-size:14px;","Pair Locations"),
                fluidRow(
                  column(6,
                    tags$p(textOutput("p_seq_lbl1"),
                           style="color:#b0b8d4;font-size:11px;font-weight:600;text-align:center;"),
                    plotOutput("p_seq_loc1",height="230px")
                  ),
                  column(6,
                    tags$p(textOutput("p_seq_lbl2"),
                           style="color:#b0b8d4;font-size:11px;font-weight:600;text-align:center;"),
                    plotOutput("p_seq_loc2",height="230px")
                  )
                ),
                br(),
                uiOutput("p_seq_stats")
              )
            )
          ),
          # Heat Maps
          tabPanel("Heat Maps",
            br(),
            tags$div(class="section-header","Pitch Location Heat Maps"),
            tags$div(class="section-sub","Pitcher's perspective"),
            tags$div(class="filter-bar",
              fluidRow(
                column(4, uiOutput("p_hm_pitch_ui")),
                column(4, uiOutput("p_hm_count_ui")),
                column(4,
                  radioButtons("p_hm_hand","Batter Side",
                               choices=c("Combined","Right","Left"),
                               selected="Combined",inline=TRUE))
              )
            ),
            plotOutput("p_heatmap",height="480px")
          ),
          # Pitch Locations
          tabPanel("Pitch Locations",
            br(),
            tags$div(class="section-header","Individual Pitch Locations"),
            tags$div(class="section-sub",
              "Every pitch as a dot \u2014 hover for detail; useful for small samples the heat map smooths over."),
            tags$div(class="filter-bar",
              fluidRow(
                column(3, uiOutput("p_pl_pitch_ui")),
                column(3, uiOutput("p_pl_count_ui")),
                column(3,
                  radioButtons("p_pl_hand","Batter Side",
                               choices=c("Combined","Right","Left"),
                               selected="Combined",inline=TRUE)),
                column(3,
                  radioButtons("p_pl_color","Color By",
                               choices=c("Pitch Type"="pitch","Outcome"="outcome"),
                               selected="pitch",inline=TRUE))
              )
            ),
            plotlyOutput("p_pitchloc",height="480px")
          ),
          # Velocity & Spin
          tabPanel("Velocity & Spin",
            br(),
            tags$div(class="section-header","Velocity Over Time"),
            plotOutput("p_velo",height="360px"),
            br(),
            tags$div(class="section-header","Spin Rate Over Time"),
            plotOutput("p_spin",height="360px")
          ),
          # Count Splits
          tabPanel("Count Splits",
            br(),
            tags$div(class="section-header","Results by Count"),
            dataTableOutput("p_countTable"),
            br(),
            tags$div(class="section-header","Pitch Type \u00d7 Batter Side"),
            dataTableOutput("p_ptHandTable")
          ),
          # Batted Ball
          tabPanel("Batted Ball",
            br(),
            tags$div(class="filter-bar",
              fluidRow(
                column(4, uiOutput("p_bb_pitch_ui")),
                column(4,
                  radioButtons("p_bb_hand","Batter Side",
                               choices=c("Combined","Right","Left"),
                               selected="Combined",inline=TRUE))
              )
            ),
            fluidRow(
              column(3, uiOutput("p_bb_card_gb")),
              column(3, uiOutput("p_bb_card_fb")),
              column(3, uiOutput("p_bb_card_ld")),
              column(3, uiOutput("p_bb_card_pu"))
            ),
            br(),
            tags$div(class="section-header","Cumulative Batted Ball Rates"),
            tags$div(class="section-sub","Overall totals for selected filters"),
            dataTableOutput("p_bb_totalTable"),
            br(),
            tags$div(class="section-header","Batted Ball Rates by Pitch Type"),
            dataTableOutput("p_bb_ptTable"),
            br(),
            tags$div(class="section-header","Batted Ball Rates by Batter Side"),
            dataTableOutput("p_bb_lrTable"),
            br(),
            tags$div(class="section-header","Batted Ball Rates by Pitch Type \u00d7 Batter Side"),
            dataTableOutput("p_bb_ptLrTable")
          )
        )
      )
    )
  ),

  # ══════════════════════════════════════════════════════════════════════════
  # SCOUTING REPORTS TAB (pitchers)
  # ══════════════════════════════════════════════════════════════════════════
  tabPanel("Scouting Reports",
    sidebarLayout(
      sidebarPanel(width=3,
        tags$div(class="section-header","Season"),
        tags$div(class="season-toggle",
          tags$button("2026", id="srbtn_2026", class="season-btn active",
                      onclick="Shiny.setInputValue('sr_season','2026',{priority:'event'})"),
          tags$button("2025", id="srbtn_2025", class="season-btn",
                      onclick="Shiny.setInputValue('sr_season','2025',{priority:'event'})")
        ),
        tags$hr(),

        tags$div(class="section-header","Outings"),
        radioButtons("sr_date_mode", NULL,
                     choices=c("All outings"="all",
                               "On or after a date"="after",
                               "Pick specific outings"="pick"),
                     selected="all"),
        conditionalPanel("input.sr_date_mode == 'after'",
          uiOutput("sr_date_from_ui")),
        conditionalPanel("input.sr_date_mode == 'pick'",
          uiOutput("sr_date_pick_ui")),
        tags$hr(),

        tags$div(class="section-header","Pitchers"),
        uiOutput("sr_players_ui"),
        fluidRow(
          column(6, actionButton("sr_sel_all","All",   class="btn-default btn-sm")),
          column(6, actionButton("sr_sel_none","Clear",class="btn-default btn-sm"))
        ),
        tags$hr(),

        selectInput("sr_hand","Batter Side Filter",
                    choices=c("Combined","Right","Left"), selected="Combined"),
        checkboxInput("sr_norm_pitch","Merge duplicate pitch tags", value=TRUE),
        numericInput("sr_min_n","Min pitches per row", value=3, min=1, max=50, step=1),
        tags$hr(),

        tags$div(class="section-header","Sections to Include"),
        checkboxGroupInput("sr_sections", NULL,
                           choices  = sr_sections_all,
                           selected = c("kpi","arsenal","res_pt","res_pt_lr")),
        tags$hr(),

        tags$div(class="section-header","PDF Layout"),
        radioButtons("sr_layout", NULL,
                     choices=c("One page per pitcher"="player",
                               "One chart per page"="single",
                               "Group pitchers together"="combined"),
                     selected="player"),
        tags$hr(),
        downloadButton("sr_download","Generate PDF",
                       class="btn-primary", style="width:100%;"),
        tags$br(), tags$br(),
        uiOutput("sr_status_ui")
      ),
      mainPanel(width=9,
        uiOutput("sr_dataStatus"),
        tags$div(class="section-header","Report Preview"),
        tags$div(class="section-sub",
                 "Live preview of the selected sections. The PDF renders every selected pitcher; the preview shows the first five."),
        uiOutput("sr_preview_ui")
      )
    )
  )
)

# ============================================================================
#  SERVER
# ============================================================================

# ── Top-level UI: login gate only. The dashboard is injected after auth. ─────
ui <- bootstrapPage(
  tags$head(tags$style(HTML(login_css))),
  uiOutput("app_gate")
)

server <- function(input, output, session) {

  # ── Login gate ─────────────────────────────────────────────────────────────
  authed     <- reactiveVal(FALSE)
  login_fail <- reactiveVal(0L)
  login_msg_txt <- reactiveVal("")

  observeEvent(input$login_btn, {
    target <- get_app_password_hash()
    if (nchar(target) == 0 || target == "PASTE_YOUR_HASH_HERE") {
      # Fail closed: never quietly leave the app open to everyone.
      login_msg_txt("No password configured. Set APP_PASSWORD_SHA256 in app.R.")
      return(invisible())
    }
    entered <- if (is.null(input$login_pw)) "" else input$login_pw
    if (nchar(entered) > 0 &&
        identical(digest::digest(entered, algo = "sha256", serialize = FALSE),
                  target)) {
      authed(TRUE)
      login_msg_txt("")
    } else {
      login_fail(login_fail() + 1L)
      login_msg_txt("Incorrect password.")
    }
  })

  output$login_msg <- renderUI({
    tags$div(class="login-err", login_msg_txt())
  })

  output$app_gate <- renderUI({
    if (authed()) main_ui else login_ui
  })
  outputOptions(output, "app_gate", suspendWhenHidden = FALSE)


  # ── Season inputs with defaults ───────────────────────────────────────────
  h_season <- reactive({ input$h_season %||% "2026" })
  p_season <- reactive({ input$p_season %||% "2026" })

  # Sync pill highlight via JS
  observeEvent(h_season(), {
    session$sendCustomMessage("setSeason", h_season())
  })
  observeEvent(p_season(), {
    session$sendCustomMessage("setSeasonP", p_season())
  })

  # ── Data-load status banners ──────────────────────────────────────────────
  output$h_dataStatus <- renderUI({
    if (is.null(seasons[[h_season()]]$hitters))
      tags$div(class="load-error",
        "\u26A0 Hitter data failed to load for this season \u2014 likely a GitHub ",
        "rate-limit (HTTP 429) or fetch error at startup. Reload the app; if it ",
        "persists, redeploy.")
  })
  output$p_dataStatus <- renderUI({
    if (is.null(seasons[[p_season()]]$pitchers))
      tags$div(class="load-error",
        "\u26A0 Pitcher data failed to load for this season \u2014 likely a GitHub ",
        "rate-limit (HTTP 429) or fetch error at startup. Reload the app; if it ",
        "persists, redeploy.")
  })

  # ── Reactive data accessors ───────────────────────────────────────────────
  h_raw <- reactive({ seasons[[h_season()]]$hitters })
  p_raw <- reactive({
    res <- seasons[[p_season()]]$pitchers
    if (is.null(res)) return(NULL)
    res$data
  })
  p_seqs  <- reactive({
    res <- seasons[[p_season()]]$pitchers
    if (is.null(res)) return(NULL)
    res$sequences
  })
  p_pairs <- reactive({
    res <- seasons[[p_season()]]$pitchers
    if (is.null(res)) return(NULL)
    res$pairs
  })

  # ==========================================================================
  #  HITTER UI & REACTIVES
  # ==========================================================================

  output$h_playerSelect <- renderUI({
    req(!is.null(h_raw()))
    prev <- isolate(input$h_batter)
    choices <- sort(unique(h_raw()$Batter))
    sel <- if (!is.null(prev) && prev %in% choices) prev else choices[1]
    selectInput("h_batter","Select Batter",choices=choices,selected=sel,selectize=TRUE)
  })

  output$h_dateRangeUI <- renderUI({
    req(input$h_batter, !is.null(h_raw()))
    d <- range(h_raw() %>% filter(Batter==input$h_batter) %>% pull(Date), na.rm=TRUE)
    dateRangeInput("h_dateRange","Date Range",start=d[1],end=d[2],
                   min=d[1],max=d[2],format="mm/dd/yyyy")
  })

  output$h_opponentUI <- renderUI({
    req(input$h_batter, !is.null(h_raw()))
    d <- h_raw() %>% filter(Batter==input$h_batter)
    if (!is.null(input$h_dateRange))
      d <- d %>% filter(Date>=input$h_dateRange[1], Date<=input$h_dateRange[2])
    opp <- d %>%
      group_by(PitcherTeam,PitcherTeamFull) %>%
      summarise(pas=n_distinct(PA_count),.groups="drop") %>%
      filter(PitcherTeam!="",!is.na(PitcherTeam)) %>% arrange(PitcherTeamFull)
    choices <- setNames(opp$PitcherTeam,
                        paste0(opp$PitcherTeamFull," (",opp$pas," PAs)"))
    selectInput("h_opp","Opponent",choices=c("All Opponents"="all",choices))
  })

  output$h_paRangeUI <- renderUI({
    req(input$h_batter, !is.null(h_raw()))
    mx <- h_raw() %>% filter(Batter==input$h_batter) %>% pull(PA_count) %>% max(na.rm=TRUE)
    tagList(
      numericInput("h_paMin","Min PA",1,1,mx),
      numericInput("h_paMax","Max PA",mx,1,mx)
    )
  })

  output$h_pitchTypeUI <- renderUI({
    req(!is.null(h_raw()))
    selectInput("h_pitchType","Pitch Type",
                choices=c("All",sort(unique(h_raw()$TaggedPitchType))))
  })

  output$h_playerInfo <- renderUI({
    d <- h_base(); if(is.null(d)||nrow(d)==0) return(NULL)
    dr <- range(d$Date,na.rm=TRUE)
    tags$div(class="stat-card",
      tags$p(style="color:#8892b0;font-size:11px;",toupper(paste(h_season(),"season"))),
      tags$h2(style="font-size:20px;",max(d$PA_count,na.rm=TRUE)," PAs"),
      tags$p(format(dr[1],"%m/%d")," – ",format(dr[2],"%m/%d"))
    )
  })

  h_base <- reactive({
    req(input$h_batter, !is.null(h_raw()))
    d <- h_raw() %>% filter(Batter==input$h_batter)
    if (!is.null(input$h_dateRange))
      d <- d %>% filter(Date>=input$h_dateRange[1], Date<=input$h_dateRange[2])
    if (!is.null(input$h_opp) && input$h_opp!="all")
      d <- d %>% filter(PitcherTeam==input$h_opp)
    d
  })

  h_filt <- reactive({
    req(input$h_paMin, input$h_paMax)
    d <- h_base()
    if (!is.null(input$h_pitchType) && input$h_pitchType!="All")
      d <- d %>% filter(TaggedPitchType==input$h_pitchType)
    d %>% filter(PA_count>=input$h_paMin, PA_count<=input$h_paMax)
  })

  h_stats <- reactive({
    d <- h_filt(); if(is.null(d)||nrow(d)==0) return(NULL)
    hits  <- sum(d$PlayResult %in% c("Single","Double","Triple","HomeRun"))
    walks <- sum(d$KorBB=="Walk", na.rm=TRUE); hbp <- sum(d$PitchCall=="HitByPitch", na.rm=TRUE)
    sac <- sum(d$PlayResult=="Sacrifice", na.rm=TRUE)
    pa <- n_distinct(d$PA_count); ab <- pa-walks-hbp-sac
    tb <- sum(d$PlayResult=="Single")+2*sum(d$PlayResult=="Double")+
          3*sum(d$PlayResult=="Triple")+4*sum(d$PlayResult=="HomeRun")
    woba_s <- sum(d$wOBA_contribution,na.rm=TRUE)
    list(pa=pa,
         woba=if(pa>0)round(woba_s/pa,3) else NA,
         obp =if(pa>0)round((hits+walks+hbp)/pa,3) else NA,
         slg =if(ab>0)round(tb/ab,3) else NA)
  })

  output$h_card_pa   <- renderUI({ s<-h_stats();req(s); stat_card(s$pa,"Plate Appearances") })
  output$h_card_woba <- renderUI({ s<-h_stats();req(s); stat_card(sprintf("%.3f",s$woba),"wOBA") })
  output$h_card_obp  <- renderUI({ s<-h_stats();req(s); stat_card(sprintf("%.3f",s$obp),"OBP") })
  output$h_card_slg  <- renderUI({ s<-h_stats();req(s); stat_card(sprintf("%.3f",s$slg),"SLG") })

  output$h_spray <- renderPlot({
    d <- h_filt() %>%
      filter(!is.na(LastTrackedDistance), !is.na(Bearing), !is.na(PlayResult)) %>%
      mutate(
        # Convert polar (bearing degrees from centre, distance ft) to Cartesian
        # Bearing: 0 = straight up (CF), negative = left, positive = right
        bearing_rad = Bearing * pi / 180,
        x = LastTrackedDistance * sin(bearing_rad),
        y = LastTrackedDistance * cos(bearing_rad)
      )
    req(nrow(d) > 0)

    # ── Field geometry constants (feet) ──────────────────────────────────────
    # Home plate at origin. Bases at 90-ft diamond. Outfield wall ~330-400 ft.
    base  <- 90 / sqrt(2)   # ~63.6 ft — half-diagonal of 90-ft square
    foul_len <- 330
    cf_dist  <- 400

    # Infield dirt circle radius
    dirt_r <- 95

    # Arc helper
    arc_df <- function(r, a_from, a_to, n=200) {
      a <- seq(a_from, a_to, length.out=n) * pi/180
      data.frame(x=r*sin(a), y=r*cos(a))
    }

    # Foul line tips (where wall meets foul line) — 330 ft at 45° bearing
    fl_x <- foul_len * sin(45 * pi/180)   # 233.3
    fl_y <- foul_len * cos(45 * pi/180)   # 233.3

    # Outfield wall polygon:
    #   home plate → left foul tip → arc LF corner → arc CF (400) → arc RF corner → right foul tip → home
    wall_arc_l  <- arc_df(330,  -45, -20)   # LF line down to LC gap
    wall_arc_cf <- arc_df(cf_dist, -20, 20) # LC gap through CF to RC gap
    wall_arc_r  <- arc_df(330,   20,  45)   # RC gap to RF line
    wall_poly <- rbind(
      data.frame(x=0,       y=0),
      data.frame(x=-fl_x,   y=fl_y),
      wall_arc_l,
      wall_arc_cf,
      wall_arc_r,
      data.frame(x= fl_x,   y=fl_y)
    )

    # Infield dirt
    dirt_arc   <- arc_df(dirt_r, -45, 45)
    dirt_poly  <- rbind(data.frame(x=0,y=0), dirt_arc, data.frame(x=0,y=0))

    # Base paths
    bases <- data.frame(
      bx = c(0,  base, 0, -base, 0),
      by = c(0,  base, 2*base, base, 0)
    )

    # Foul lines
    foul_l <- data.frame(x=c(0,-foul_len), y=c(0,foul_len))
    foul_r <- data.frame(x=c(0, foul_len), y=c(0,foul_len))

    ggplot() +
      # Outfield grass
      geom_polygon(data=wall_poly, aes(x=x,y=y), fill="#1a2e1a", color=NA) +
      # Infield dirt
      geom_polygon(data=dirt_poly, aes(x=x,y=y), fill="#3d2b1f", color=NA) +
      # Infield grass square
      geom_polygon(data=bases, aes(x=bx,y=by), fill="#1a2e1a", color=NA) +
      # Outfield wall arc
      geom_path(data=rbind(data.frame(x=-fl_x,y=fl_y),
                            wall_arc_l, wall_arc_cf, wall_arc_r,
                            data.frame(x=fl_x, y=fl_y)),
                aes(x=x,y=y), color="#6b7a8d", linewidth=1.4) +
      # Foul lines (home → wall tip)
      geom_segment(aes(x=0,y=0,xend=-fl_x,yend=fl_y),
                   color="#6b7a8d",linewidth=.9) +
      geom_segment(aes(x=0,y=0,xend= fl_x,yend=fl_y),
                   color="#6b7a8d",linewidth=.9) +
      # Base paths
      geom_path(data=bases, aes(x=bx,y=by), color="#8892b0", linewidth=.8) +
      # Bases
      geom_point(data=data.frame(x=c(base,0,-base),y=c(base,2*base,base)),
                 aes(x=x,y=y), color="#ffffff", size=3, shape=15) +
      # Home plate
      geom_point(aes(x=0,y=0), color="#ffffff", size=4, shape=18) +
      # Pitcher's mound
      geom_point(aes(x=0,y=60.5), color="#5a3e2b", size=6, shape=16) +
      # Distance labels
      annotate("text", x=0,        y=cf_dist+12,  label="400", color="#6b7280", size=3) +
      annotate("text", x=-fl_x-18, y=fl_y+5,      label="330", color="#6b7280", size=3) +
      annotate("text", x= fl_x+18, y=fl_y+5,      label="330", color="#6b7280", size=3) +
      # Batted balls
      geom_point(data=d, aes(x=x, y=y, color=PlayResult),
                 size=3.2, alpha=.85) +
      scale_color_manual(
        values=c(Single="#4ECDC4", Double="#45B7D1", HomeRun="#ff4655",
                 Out="#8892b0",    Triple="#a78bfa",  FieldersChoice="#FECA57",
                 Sacrifice="#48D1CC", Error="#FA8072"),
        name="Result"
      ) +
      coord_fixed(xlim=c(-280,280), ylim=c(-10,420)) +
      labs(title=paste(input$h_batter, "—", h_season(), "Spray Chart")) +
      theme_navs() +
      theme(axis.text=element_blank(), axis.title=element_blank(),
            panel.grid=element_blank(), panel.background=element_rect(fill="#0f1117",color=NA))
  }, bg="#0f1117")

  # ── Plate Discipline ───────────────────────────────────────────────────────

  # Filter data for plate discipline (uses h_base, not h_filt — pitch type / count applied separately)
  h_pd_data <- reactive({
    req(input$h_batter, !is.null(h_raw()))
    d <- h_base()
    if (!is.null(input$h_pd_pitch) && input$h_pd_pitch != "All Pitches")
      d <- d %>% filter(TaggedPitchType == input$h_pd_pitch)
    d <- filter_by_count(d, input$h_pd_count, "CountSit", "CountIndiv")
    if (!is.null(input$h_pd_hand) && input$h_pd_hand != "Combined")
      d <- d %>% filter(PitcherThrows == input$h_pd_hand)
    d %>%
      filter(!is.na(PlateLocSide), !is.na(PlateLocHeight)) %>%
      mutate(
        InZone   = between(PlateLocHeight,1.59,3.41) & between(PlateLocSide,-1,1),
        IsSwing  = PitchCall %in% c("FoulBall","StrikeSwinging","InPlay"),
        IsWhiff  = PitchCall == "StrikeSwinging",
        IsCSW    = PitchCall %in% c("StrikeSwinging","StrikeCalled"),
        IsContact= PitchCall %in% c("FoulBall","InPlay"),
        # Outcome category for colour coding (hitter POV — flip side)
        PD_side  = -PlateLocSide,
        Outcome  = dplyr::case_when(
          IsContact & InZone                    ~ "Contact (zone)",
          IsWhiff   & InZone                    ~ "Whiff (zone)",
          PitchCall == "StrikeCalled"           ~ "Called Strike",
          PitchCall %in% c("BallCalled","HitByPitch") & InZone ~ "Ball (zone)",
          IsSwing   & !InZone                   ~ "Chase",
          TRUE                                   ~ "Take (out of zone)"
        )
      )
  })

  output$h_pd_pitch_ui <- renderUI({
    req(!is.null(h_raw()))
    selectInput("h_pd_pitch","Pitch Type",
                choices=c("All Pitches",sort(unique(h_raw()$TaggedPitchType))))
  })
  output$h_pd_count_ui <- renderUI({
    req(!is.null(h_raw()), input$h_batter)
    selectInput("h_pd_count","Count / Situation",
                choices=count_choices(h_base()$CountIndiv))
  })

  pd_stat_card <- function(val, n_den, label) {
    tags$div(class="stat-card",
      tags$h2(paste0(round(val*100,1),"%")),
      tags$p(style="color:#6b7280;font-size:11px;margin:2px 0 0;",
             paste0(round(val*sum(!is.na(n_den))),"/",sum(!is.na(n_den)))),
      tags$p(label)
    )
  }

  output$h_pd_card_zsw <- renderUI({
    d <- h_pd_data(); req(d, nrow(d)>0)
    zone_pitches <- d %>% filter(InZone)
    v <- if(nrow(zone_pitches)>0) mean(zone_pitches$IsSwing,na.rm=TRUE) else 0
    tags$div(class="stat-card",
      tags$h2(paste0(round(v*100,1),"%")),
      tags$p(style="color:#6b7280;font-size:11px;margin:2px 0;",
             paste0(sum(zone_pitches$IsSwing,na.rm=TRUE),"/",nrow(zone_pitches))),
      tags$p("Zone Swing%")
    )
  })
  output$h_pd_card_zcon <- renderUI({
    d <- h_pd_data(); req(d, nrow(d)>0)
    zone_swings <- d %>% filter(InZone, IsSwing)
    v <- if(nrow(zone_swings)>0) mean(zone_swings$IsContact,na.rm=TRUE) else 0
    tags$div(class="stat-card",
      tags$h2(paste0(round(v*100,1),"%")),
      tags$p(style="color:#6b7280;font-size:11px;margin:2px 0;",
             paste0(sum(zone_swings$IsContact,na.rm=TRUE),"/",nrow(zone_swings))),
      tags$p("Zone Contact%")
    )
  })
  output$h_pd_card_whiff <- renderUI({
    d <- h_pd_data(); req(d, nrow(d)>0)
    swings <- d %>% filter(IsSwing)
    v <- if(nrow(swings)>0) mean(swings$IsWhiff,na.rm=TRUE) else 0
    tags$div(class="stat-card",
      tags$h2(paste0(round(v*100,1),"%")),
      tags$p(style="color:#6b7280;font-size:11px;margin:2px 0;",
             paste0(sum(swings$IsWhiff,na.rm=TRUE),"/",nrow(swings))),
      tags$p("Whiff%")
    )
  })
  output$h_pd_card_chase <- renderUI({
    d <- h_pd_data(); req(d, nrow(d)>0)
    ozone <- d %>% filter(!InZone)
    v <- if(nrow(ozone)>0) mean(ozone$IsSwing,na.rm=TRUE) else 0
    tags$div(class="stat-card",
      tags$h2(paste0(round(v*100,1),"%")),
      tags$p(style="color:#6b7280;font-size:11px;margin:2px 0;",
             paste0(sum(ozone$IsSwing,na.rm=TRUE),"/",nrow(ozone))),
      tags$p("Chase%")
    )
  })

  output$h_pd_plot <- renderPlot({
    d <- h_pd_data(); req(d, nrow(d)>0)

    outcome_pal <- c(
      "Contact (zone)"    = "#4ECDC4",
      "Whiff (zone)"      = "#ff4655",
      "Called Strike"     = "#ffd700",
      "Ball (zone)"       = "#34d399",
      "Chase"             = "#ff8c42",
      "Take (out of zone)"= "#8892b0"
    )
    outcome_shape <- c(
      "Contact (zone)"    = 16,
      "Whiff (zone)"      = 4,
      "Called Strike"     = 16,
      "Ball (zone)"       = 16,
      "Chase"             = 16,
      "Take (out of zone)"= 1
    )

    # draw takes last so coloured swings sit on top
    d_ordered <- d %>%
      mutate(Outcome=factor(Outcome, levels=c(
        "Take (out of zone)","Ball (zone)","Called Strike",
        "Chase","Contact (zone)","Whiff (zone)")))

    ggplot(d_ordered, aes(x=PD_side, y=PlateLocHeight,
                          color=Outcome, shape=Outcome)) +
      # strike zone box
      annotate("rect",xmin=-1,xmax=1,ymin=1.59,ymax=3.41,
               fill=NA,color="#ffffff",linewidth=1) +
      # inner zone quadrants
      annotate("segment",x=c(-1/3,1/3,-1,-1),xend=c(-1/3,1/3,1,1),
               y=c(1.59,1.59,(3.41-1.59)/3+1.59,(3.41-1.59)*2/3+1.59),
               yend=c(3.41,3.41,(3.41-1.59)/3+1.59,(3.41-1.59)*2/3+1.59),
               color="#2a2d3a",linewidth=.4) +
      geom_jitter(size=2.8, alpha=.80, width=.01, height=.01) +
      scale_color_manual(values=outcome_pal, name="Outcome") +
      scale_shape_manual(values=outcome_shape, name="Outcome") +
      xlim(-2.2,2.2) + ylim(0.8,4.2) +
      labs(
        title=paste(input$h_batter,"—",h_season(),"Plate Discipline"),
        subtitle=paste("Pitch:",input$h_pd_pitch %||% "All",
                       "| Count:",input$h_pd_count %||% "All",
                       "| Throws:",input$h_pd_hand %||% "Combined",
                       "| n =",nrow(d)),
        x="Horizontal (Hitter's View)",
        y="Vertical Height (ft)"
      ) +
      annotate("text",x=-2.1,y=c(1.0),
               label=paste(nrow(d),"pitches shown"),
               color="#6b7280",size=3,hjust=0) +
      theme_navs() +
      theme(legend.position="right",
            legend.key=element_rect(fill="#1a1e2e",color=NA))
  }, bg="#0f1117")

  output$h_woba <- renderPlot({
    d <- h_filt() %>% arrange(PA_count) %>% filter(is.finite(cumulative_wOBA))
    req(nrow(d)>0)
    last_val <- tail(d$cumulative_wOBA,1)
    ggplot(d,aes(x=PA_count,y=cumulative_wOBA)) +
      geom_hline(yintercept=c(.320,.370,.420),linetype="dashed",color="#2a2d3a") +
      annotate("text",x=min(d$PA_count),y=c(.325,.375,.425),
               label=c("Average (.320)","Good (.370)","Great (.420)"),
               hjust=0,size=3,color="#8892b0") +
      geom_line(color="#ff4655",linewidth=1.6) +
      geom_point(color="#ff4655",size=2) +
      annotate("text",x=max(d$PA_count),y=last_val,
               label=sprintf("%.3f",last_val),
               hjust=-0.15,size=4,fontface="bold",color="#ff4655") +
      labs(title=paste(input$h_batter,"—",h_season(),"Rolling wOBA"),
           x="Plate Appearance",y="wOBA") +
      theme_navs()
  }, bg="#0f1117")

  output$h_wobaOpp <- renderPlot({
    d <- h_base() %>% filter(!is.na(PitcherTeamFull),PitcherTeamFull!="")
    req(nrow(d)>0)
    teams   <- sort(unique(d$PitcherTeamFull))
    col_map <- setNames(team_pal[seq_along(teams)],teams)
    finals  <- d %>% group_by(PitcherTeamFull) %>%
      summarise(fp=max(PA_count_opp),fw=last(cumulative_wOBA_opp),.groups="drop")
    p <- ggplot(d,aes(x=PA_count_opp,y=cumulative_wOBA_opp,color=PitcherTeamFull)) +
      geom_hline(yintercept=.320,linetype="dashed",color="#2a2d3a") +
      geom_line(linewidth=1.3,alpha=.85) +
      geom_point(size=1.8,alpha=.85) +
      scale_color_manual(values=col_map,name="Opponent") +
      labs(title=paste(input$h_batter,"—",h_season(),"wOBA by Opponent"),
           x="PAs vs Opponent",y="Cumulative wOBA") +
      theme_navs()
    for (i in seq_len(nrow(finals)))
      p <- p + annotate("text",x=finals$fp[i],y=finals$fw[i],
                        label=sprintf("%.3f",finals$fw[i]),
                        hjust=-0.1,size=3,color=col_map[finals$PitcherTeamFull[i]])
    p
  }, bg="#0f1117")

  output$h_ptTable   <- renderDataTable({
    datatable(h_filt()%>%group_by(PitchType=TaggedPitchType)%>%slg_stats(),
              options=dt_opts,rownames=FALSE)
  })
  output$h_lrTable   <- renderDataTable({
    datatable(h_filt()%>%group_by(Throws=PitcherThrows)%>%slg_stats(),
              options=dt_opts,rownames=FALSE)
  })
  output$h_ptLrTable <- renderDataTable({
    datatable(h_filt()%>%group_by(Throws=PitcherThrows,PitchType=TaggedPitchType)%>%slg_stats(),
              options=dt_opts,rownames=FALSE)
  })

  # ── Hitter Batted Ball ────────────────────────────────────────────────────
  bb_stats_h <- function(d) {
    d %>% summarise(
      BBE  = sum(TaggedHitType %in% c("GroundBall","LineDrive","FlyBall","Popup"), na.rm=TRUE),
      GB   = sum(TaggedHitType=="GroundBall", na.rm=TRUE),
      FB   = sum(TaggedHitType=="FlyBall",    na.rm=TRUE),
      LD   = sum(TaggedHitType=="LineDrive",  na.rm=TRUE),
      PU   = sum(TaggedHitType=="Popup",      na.rm=TRUE),
      `GB%`  = ifelse(BBE>0, paste0(round(GB/BBE*100,1),"%"), "—"),
      `FB%`  = ifelse(BBE>0, paste0(round(FB/BBE*100,1),"%"), "—"),
      `LD%`  = ifelse(BBE>0, paste0(round(LD/BBE*100,1),"%"), "—"),
      `PU%`  = ifelse(BBE>0, paste0(round(PU/BBE*100,1),"%"), "—"),
      `GB/FB`= ifelse(FB>0,  as.character(round(GB/FB,2)), "—"),
      .groups="drop"
    ) %>% filter(BBE>0)
  }

  h_bb_data <- reactive({
    d <- h_filt()
    if (!is.null(input$h_bb_pitch) && input$h_bb_pitch!="All Pitches")
      d <- d %>% filter(TaggedPitchType==input$h_bb_pitch)
    if (!is.null(input$h_bb_hand) && input$h_bb_hand!="Combined")
      d <- d %>% filter(PitcherThrows==input$h_bb_hand)
    d %>% filter(TaggedHitType %in% c("GroundBall","LineDrive","FlyBall","Popup"))
  })

  output$h_bb_pitch_ui <- renderUI({
    req(!is.null(h_raw()))
    selectInput("h_bb_pitch","Pitch Type",
                choices=c("All Pitches",sort(unique(h_raw()$TaggedPitchType))))
  })

  bb_card <- function(val, n, denom, label) {
    tags$div(class="stat-card",
      tags$h2(paste0(round(val*100,1),"%")),
      tags$p(style="color:#6b7280;font-size:11px;margin:2px 0;",paste0(n,"/",denom)),
      tags$p(label)
    )
  }

  output$h_bb_card_gb <- renderUI({
    d<-h_bb_data();req(nrow(d)>0)
    n<-sum(d$TaggedHitType=="GroundBall",na.rm=TRUE); tot<-nrow(d)
    bb_card(n/tot,n,tot,"GB%")
  })
  output$h_bb_card_fb <- renderUI({
    d<-h_bb_data();req(nrow(d)>0)
    n<-sum(d$TaggedHitType=="FlyBall",na.rm=TRUE); tot<-nrow(d)
    bb_card(n/tot,n,tot,"FB%")
  })
  output$h_bb_card_ld <- renderUI({
    d<-h_bb_data();req(nrow(d)>0)
    n<-sum(d$TaggedHitType=="LineDrive",na.rm=TRUE); tot<-nrow(d)
    bb_card(n/tot,n,tot,"LD%")
  })
  output$h_bb_card_pu <- renderUI({
    d<-h_bb_data();req(nrow(d)>0)
    n<-sum(d$TaggedHitType=="Popup",na.rm=TRUE); tot<-nrow(d)
    bb_card(n/tot,n,tot,"PU%")
  })

  output$h_bb_ptTable <- renderDataTable({
    d<-h_bb_data();req(nrow(d)>0)
    datatable(d%>%group_by(PitchType=TaggedPitchType)%>%bb_stats_h(),
              options=dt_opts,rownames=FALSE)
  })
  output$h_bb_totalTable <- renderDataTable({
    d<-h_bb_data();req(nrow(d)>0)
    datatable(d%>%mutate(Total="All")%>%group_by(Total)%>%bb_stats_h(),
              options=dt_opts,rownames=FALSE)
  })
  output$h_bb_lrTable <- renderDataTable({
    d<-h_bb_data();req(nrow(d)>0)
    datatable(d%>%group_by(Throws=PitcherThrows)%>%bb_stats_h(),
              options=dt_opts,rownames=FALSE)
  })
  output$h_bb_ptLrTable <- renderDataTable({
    d<-h_bb_data();req(nrow(d)>0)
    datatable(d%>%group_by(Throws=PitcherThrows,PitchType=TaggedPitchType)%>%bb_stats_h(),
              options=dt_opts,rownames=FALSE)
  })

  # Hitter heat map UIs
  output$h_hm_pitch_ui <- renderUI({
    req(!is.null(h_raw()))
    selectInput("h_hm_pitch","Pitch Type",
                choices=c("All Pitches",sort(unique(h_raw()$TaggedPitchType))))
  })
  output$h_hm_count_ui <- renderUI({
    req(!is.null(h_raw()), input$h_batter)
    d <- h_base()
    selectInput("h_hm_count","Count / Situation",choices=count_choices(d$CountIndiv))
  })

  output$h_heatmap <- renderPlot({
    req(!is.null(h_raw()), input$h_batter)
    d <- h_base()
    if (!is.null(input$h_hm_pitch) && input$h_hm_pitch!="All Pitches")
      d <- d %>% filter(TaggedPitchType==input$h_hm_pitch)
    d <- filter_by_count(d, input$h_hm_count, "CountSit", "CountIndiv")
    if (!is.null(input$h_hm_hand) && input$h_hm_hand!="Combined")
      d <- d %>% filter(PitcherThrows==input$h_hm_hand)
    sub <- paste("Pitch:",input$h_hm_pitch %||% "All",
                 "| Count:",input$h_hm_count %||% "All",
                 "| Throws:",input$h_hm_hand %||% "Combined")
    loc_heatmap(d, paste(input$h_batter,"—",h_season(),"Heat Map"), sub, flip_side=TRUE)
  }, bg="#0f1117")

  # ==========================================================================
  #  PITCHER UI & REACTIVES
  # ==========================================================================

  output$p_playerSelect <- renderUI({
    req(!is.null(p_raw()))
    prev    <- isolate(input$p_pitcher)
    choices <- sort(unique(p_raw()$Pitcher))
    sel     <- if (!is.null(prev) && prev %in% choices) prev else choices[1]
    selectInput("p_pitcher","Select Pitcher",choices=choices,selected=sel,selectize=TRUE)
  })

  output$p_dateUI <- renderUI({
    req(input$p_pitcher, !is.null(p_raw()))
    dates   <- sort(unique(p_raw() %>% filter(Pitcher==input$p_pitcher) %>% pull(Date)))
    choices <- setNames(as.character(dates), format(dates,"%m/%d/%Y"))
    tagList(
      checkboxGroupInput("p_dates","Select Outing(s)",
                         choices=choices, selected=as.character(dates)),
      fluidRow(
        column(6, actionButton("p_selAll","All",  class="btn-default btn-sm")),
        column(6, actionButton("p_selNone","Clear",class="btn-default btn-sm"))
      )
    )
  })

  output$p_pitchTypeUI <- renderUI({
    req(!is.null(p_raw()))
    selectInput("p_pitchType","Pitch Type",
                choices=c("All",sort(unique(p_raw()$TaggedPitchType))))
  })

  output$p_pitchRangeUI <- renderUI({
    req(input$p_pitcher, !is.null(p_raw()))
    mx <- p_raw() %>% filter(Pitcher==input$p_pitcher) %>%
      pull(OverallPitchCount) %>% max(na.rm=TRUE)
    tagList(
      numericInput("p_pitchMin","Min Pitch #",1,1,mx),
      numericInput("p_pitchMax","Max Pitch #",mx,1,mx)
    )
  })

  output$p_pitcherInfo <- renderUI({
    d <- p_filt(); if(is.null(d)||nrow(d)==0) return(NULL)
    tags$div(class="stat-card",
      tags$p(style="color:#8892b0;font-size:11px;",toupper(paste(p_season(),"season"))),
      tags$h2(style="font-size:20px;",nrow(d)," pitches"),
      tags$p(n_distinct(d$Date)," outings")
    )
  })

  observeEvent(input$p_selAll, {
    req(input$p_pitcher, !is.null(p_raw()))
    dates <- sort(unique(p_raw() %>% filter(Pitcher==input$p_pitcher) %>% pull(Date)))
    updateCheckboxGroupInput(session,"p_dates",selected=as.character(dates))
  })
  observeEvent(input$p_selNone, {
    updateCheckboxGroupInput(session,"p_dates",selected=character(0))
  })

  p_filt <- reactive({
    req(input$p_pitcher, input$p_pitchMin, input$p_pitchMax, !is.null(p_raw()))
    d <- p_raw() %>% filter(Pitcher==input$p_pitcher)
    if (!is.null(input$p_dates) && length(input$p_dates)>0)
      d <- d %>% filter(Date %in% as.Date(input$p_dates))
    else return(NULL)
    if (!is.null(input$p_pitchType) && input$p_pitchType!="All")
      d <- d %>% filter(TaggedPitchType==input$p_pitchType)
    if (input$p_batterSide!="All")
      d <- d %>% filter(BatterSide==input$p_batterSide)
    d %>% filter(OverallPitchCount>=input$p_pitchMin, OverallPitchCount<=input$p_pitchMax)
  })

  # Stat cards
  output$p_card_n <- renderUI({
    d<-p_filt();req(d); stat_card(nrow(d),"Pitches")
  })
  output$p_card_csw <- renderUI({
    d<-p_filt();req(d)
    stat_card(paste0(round(mean(d$CSWCheck,na.rm=TRUE)*100,1),"%"),"CSW%")
  })
  output$p_card_whiff <- renderUI({
    d<-p_filt();req(d)
    sw <- sum(d$SwingCheck,na.rm=TRUE)
    stat_card(if(sw>0)paste0(round(sum(d$WhiffCheck,na.rm=TRUE)/sw*100,1),"%") else "—","Whiff%")
  })
  output$p_card_zone <- renderUI({
    d<-p_filt();req(d)
    stat_card(paste0(round(mean(d$ZoneCheck,na.rm=TRUE)*100,1),"%"),"Zone%")
  })

  output$p_arsenal <- renderDataTable({
    d<-p_filt();req(d,nrow(d)>0)
    tbl <- d %>% group_by(Pitch=TaggedPitchType) %>%
      summarise(
        Pitches=n(),
        `Avg Velo`=round(mean(RelSpeed,na.rm=TRUE),1),
        `Max Velo`=round(max(RelSpeed,na.rm=TRUE),1),
        `Spin`=round(mean(SpinRate,na.rm=TRUE),0),
        IVB=round(mean(InducedVertBreak,na.rm=TRUE),1),
        HB=round(mean(HorzBreak,na.rm=TRUE),1),
        `Rel H`=round(mean(RelHeight,na.rm=TRUE),2),
        `Rel S`=round(mean(RelSide,na.rm=TRUE),2),
        `CSW%`=paste0(round(mean(CSWCheck,na.rm=TRUE)*100,1),"%"),
        `Zone%`=paste0(round(mean(ZoneCheck,na.rm=TRUE)*100,1),"%"),
        `Whiff%`={sw=sum(SwingCheck,na.rm=TRUE);paste0(if(sw>0)round(sum(WhiffCheck,na.rm=TRUE)/sw*100,1)else 0,"%")},
        .groups="drop"
      ) %>%
      mutate(Usage=scales::percent(Pitches/sum(Pitches), accuracy=0.1)) %>%
      dplyr::select(Pitch, Pitches, Usage, `Avg Velo`, `Max Velo`, Spin, IVB, HB, `Rel H`, `Rel S`, `CSW%`, `Zone%`, `Whiff%`)
    datatable(tbl,options=dt_opts,rownames=FALSE)
  })

  output$p_results <- renderDataTable({
    d<-p_filt();req(d,nrow(d)>0)
    tbl <- d %>% group_by(Pitch=TaggedPitchType) %>%
      summarise(
        Pitches=n(),PA=sum(PACheck,na.rm=TRUE),AB=sum(ABCheck,na.rm=TRUE),
        H=sum(HCheck,na.rm=TRUE),
        `1B`=sum(PlayResult=="Single"),`2B`=sum(PlayResult=="Double"),
        `3B`=sum(PlayResult=="Triple"),HR=sum(PlayResult=="HomeRun"),
        SO=sum(StrikeoutCheck,na.rm=TRUE),BB=sum(WalkCheck,na.rm=TRUE),
        .groups="drop"
      ) %>%
      mutate(TB=`1B`+2*`2B`+3*`3B`+4*HR,
             AVG=sprintf("%.3f",ifelse(AB>0,H/AB,NA)),
             OBP=sprintf("%.3f",ifelse(PA>0,(H+BB)/PA,NA)),
             SLG=sprintf("%.3f",ifelse(AB>0,TB/AB,NA))) %>%
      dplyr::select(Pitch,Pitches,PA,SO,BB,H,HR,AVG,OBP,SLG)
    datatable(tbl,options=dt_opts,rownames=FALSE)
  })

  output$p_splits <- renderDataTable({
    d<-p_filt();req(d,nrow(d)>0)
    tbl <- d %>% group_by(Side=BatterSide) %>%
      summarise(
        Pitches=n(),PA=sum(PACheck,na.rm=TRUE),AB=sum(ABCheck,na.rm=TRUE),
        H=sum(HCheck,na.rm=TRUE),SO=sum(StrikeoutCheck,na.rm=TRUE),
        BB=sum(WalkCheck,na.rm=TRUE),
        `CSW%`=paste0(round(mean(CSWCheck,na.rm=TRUE)*100,1),"%"),
        .groups="drop"
      ) %>%
      mutate(AVG=sprintf("%.3f",ifelse(AB>0,H/AB,NA)),
             OBP=sprintf("%.3f",ifelse(PA>0,(H+BB)/PA,NA)))
    datatable(tbl,options=dt_opts,rownames=FALSE)
  })

  output$p_movement <- renderPlotly({
    d<-p_filt();req(d,nrow(d)>0)
    gg <- ggplot(d,aes(x=HorzBreak,y=InducedVertBreak,color=TaggedPitchType)) +
      geom_hline(yintercept=0,color="#2a2d3a",linewidth=.8) +
      geom_vline(xintercept=0,color="#2a2d3a",linewidth=.8) +
      geom_point(aes(text=paste0(TaggedPitchType,
                                 "<br>Velo: ",round(RelSpeed,1)," mph",
                                 "<br>Spin: ",round(SpinRate,0)," rpm",
                                 "<br>IVB: ",round(InducedVertBreak,1)," in",
                                 "<br>HB: ",round(HorzBreak,1)," in",
                                 "<br>Count: ",Balls,"-",Strikes)),
                 size=2.5,alpha=.7) +
      stat_ellipse(aes(group=TaggedPitchType),level=.68,linewidth=.6,linetype="dashed",alpha=.4) +
      scale_color_manual(values=pitch_pal,na.value="#94a3b8",name="Pitch Type") +
      xlim(-30,30)+ylim(-30,30) +
      labs(title=paste(input$p_pitcher,"—",p_season(),"Movement"),
           x="Horizontal Break (in)",y="Induced Vertical Break (in)") +
      theme_navs()
    to_dark_plotly(gg)
  })

  output$p_release <- renderPlotly({
    d<-p_filt();req(d,nrow(d)>0)
    gg <- ggplot(d,aes(x=RelSide,y=RelHeight,color=TaggedPitchType)) +
      geom_hline(yintercept=0,color="#2a2d3a",linewidth=.5) +
      geom_vline(xintercept=0,color="#2a2d3a",linewidth=.5) +
      geom_point(aes(text=paste0(TaggedPitchType,
                                 "<br>Velo: ",round(RelSpeed,1)," mph",
                                 "<br>Rel H: ",round(RelHeight,2)," ft",
                                 "<br>Rel S: ",round(RelSide,2)," ft",
                                 "<br>Spin: ",round(SpinRate,0)," rpm")),
                 size=2.5,alpha=.7) +
      scale_color_manual(values=pitch_pal,na.value="#94a3b8",name="Pitch Type") +
      xlim(-4,4)+ylim(2,7) +
      labs(title=paste(input$p_pitcher,"—",p_season(),"Release Points"),
           x="Horizontal Release (ft)",y="Vertical Release (ft)") +
      theme_navs()
    to_dark_plotly(gg)
  })

  # ── Pitch Sequencing ──────────────────────────────────────────────────────
  seq_clicked <- reactiveValues(first=NULL, second=NULL)

  # Recompute pairs with batter-side filter if needed
  p_pairs_filtered <- reactive({
    req(!is.null(p_pairs()), input$p_pitcher)
    d <- p_pairs() %>% filter(Pitcher==input$p_pitcher)
    hand <- input$p_seq_hand %||% "All"
    if (hand != "All") {
      seqs_h <- p_seqs() %>%
        filter(Pitcher==input$p_pitcher, BatterSide==hand)
      d <- seqs_h %>%
        group_by(Pitcher, prev_type, TaggedPitchType) %>%
        summarise(
          n_pairs    = n(),
          csw_rate   = mean(CSWCheck,na.rm=TRUE),
          whiff_rate = {sw=sum(SwingCheck,na.rm=TRUE);if(sw>0)sum(WhiffCheck,na.rm=TRUE)/sw else NA_real_},
          zone_rate  = mean(ZoneCheck,na.rm=TRUE),
          chase_rate = {oz=sum(!ZoneCheck,na.rm=TRUE);if(oz>0)sum(SwingCheck[!ZoneCheck],na.rm=TRUE)/oz else NA_real_},
          .groups="drop"
        )
    }
    d
  })

  output$p_seqMatrix <- renderPlot({
    d <- p_pairs_filtered(); req(d, nrow(d)>0)
    metric <- input$p_seq_metric %||% "csw_rate"
    all_types <- sort(unique(c(d$prev_type, d$TaggedPitchType)))
    mat <- expand.grid(prev_type=all_types,TaggedPitchType=all_types,stringsAsFactors=FALSE) %>%
      left_join(d, by=c("prev_type","TaggedPitchType")) %>%
      mutate(
        val    = .data[[metric]],
        n_pairs= ifelse(is.na(n_pairs),0L,n_pairs),
        label  = ifelse(n_pairs>0, paste0(round(val*100,0),"%\n(n=",n_pairs,")"),"")
      )
    metric_lbl <- c(csw_rate="CSW%",whiff_rate="Whiff%",
                    zone_rate="Zone%",chase_rate="Chase%")[metric]
    ggplot(mat,aes(x=prev_type,y=TaggedPitchType,fill=val)) +
      geom_tile(color="#0f1117",linewidth=1.2) +
      geom_text(aes(label=label),size=3.5,fontface="bold",color="#ffffff",lineheight=.85) +
      scale_fill_gradient2(low="#1565c0",mid="#1a1e2e",high="#ff4655",
                           midpoint=.4,na.value="#1a1e2e",limits=c(0,1),
                           labels=scales::percent_format(),name=metric_lbl) +
      labs(x="First Pitch (Previous)",y="Second Pitch (Current)",
           title=paste(input$p_pitcher,"—",p_season(),"Sequencing")) +
      theme_navs() +
      theme(panel.grid=element_blank(),
            axis.text.x=element_text(angle=30,hjust=1,size=10,face="bold"),
            axis.text.y=element_text(size=10,face="bold"),
            legend.position="right") +
      coord_fixed()
  }, bg="#0f1117")

  observeEvent(input$p_mat_click, {
    req(input$p_pitcher, !is.null(p_pairs()))
    d <- p_pairs_filtered()
    all_types <- sort(unique(c(d$prev_type, d$TaggedPitchType)))
    xi <- round(input$p_mat_click$x); yi <- round(input$p_mat_click$y)
    if (xi>=1 && xi<=length(all_types) && yi>=1 && yi<=length(all_types)) {
      seq_clicked$first  <- all_types[xi]
      seq_clicked$second <- all_types[yi]
    }
  })

  output$p_seq_lbl1 <- renderText({
    if(!is.null(seq_clicked$first))  paste("1st:", seq_clicked$first) else "1st pitch (click)"
  })
  output$p_seq_lbl2 <- renderText({
    if(!is.null(seq_clicked$second)) paste("2nd:", seq_clicked$second) else "2nd pitch (click)"
  })

  output$p_seq_loc1 <- renderPlot({
    req(seq_clicked$first, input$p_pitcher, !is.null(p_seqs()))
    loc <- p_seqs() %>%
      filter(Pitcher==input$p_pitcher, prev_type==seq_clicked$first) %>%
      transmute(PlateLocSide=prev_loc_side, PlateLocHeight=prev_loc_height) %>%
      filter(!is.na(PlateLocSide),!is.na(PlateLocHeight))
    loc_heatmap(loc, paste0(seq_clicked$first,"\n(n=",nrow(loc),")"))
  }, bg="#0f1117")

  output$p_seq_loc2 <- renderPlot({
    req(seq_clicked$first, seq_clicked$second, input$p_pitcher, !is.null(p_seqs()))
    loc <- p_seqs() %>%
      filter(Pitcher==input$p_pitcher,
             prev_type==seq_clicked$first,
             TaggedPitchType==seq_clicked$second) %>%
      dplyr::select(PlateLocSide, PlateLocHeight) %>%
      filter(!is.na(PlateLocSide),!is.na(PlateLocHeight))
    loc_heatmap(loc, paste0(seq_clicked$second,"\n(n=",nrow(loc),")"))
  }, bg="#0f1117")

  output$p_seq_stats <- renderUI({
    req(seq_clicked$first, seq_clicked$second)
    pair <- p_pairs_filtered() %>%
      filter(prev_type==seq_clicked$first, TaggedPitchType==seq_clicked$second)
    if(nrow(pair)==0)
      return(tags$div(class="stat-card",tags$p(style="color:#8892b0;","No data for this sequence.")))
    tags$div(class="stat-card",
      tags$p(style="color:#ff4655;font-weight:700;font-size:13px;",
             seq_clicked$first," → ",seq_clicked$second),
      tags$p(style="color:#8892b0;margin:4px 0;",strong(pair$n_pairs)," sequences"),
      tags$p(style="color:#8892b0;margin:4px 0;","CSW%: ",strong(paste0(round(pair$csw_rate*100,1),"%"))),
      tags$p(style="color:#8892b0;margin:4px 0;","Whiff%: ",strong(paste0(round(pair$whiff_rate*100,1),"%"))),
      tags$p(style="color:#8892b0;margin:4px 0;","Zone%: ",strong(paste0(round(pair$zone_rate*100,1),"%"))),
      tags$p(style="color:#8892b0;margin:4px 0;","Chase%: ",strong(paste0(round(pair$chase_rate*100,1),"%")))
    )
  })

  # ── Pitcher Heat Maps ─────────────────────────────────────────────────────
  output$p_hm_pitch_ui <- renderUI({
    req(!is.null(p_raw()))
    selectInput("p_hm_pitch","Pitch Type",
                choices=c("All Pitches",sort(unique(p_raw()$TaggedPitchType))))
  })
  output$p_hm_count_ui <- renderUI({
    d <- p_filt(); if(is.null(d)) return(NULL)
    selectInput("p_hm_count","Count / Situation",choices=count_choices(d$CountIndiv))
  })

  output$p_heatmap <- renderPlot({
    d <- p_filt(); req(d, nrow(d)>0)
    if (!is.null(input$p_hm_pitch) && input$p_hm_pitch!="All Pitches")
      d <- d %>% filter(TaggedPitchType==input$p_hm_pitch)
    d <- filter_by_count(d, input$p_hm_count, "CountSit", "CountIndiv")
    if (!is.null(input$p_hm_hand) && input$p_hm_hand!="Combined")
      d <- d %>% filter(BatterSide==input$p_hm_hand)
    sub <- paste("Count:",input$p_hm_count %||% "All",
                 "| Batter:",input$p_hm_hand %||% "Combined")
    if (is.null(input$p_hm_pitch) || input$p_hm_pitch=="All Pitches") {
      if(nrow(d)<5) return(ggplot()+annotate("text",x=0,y=0,label="Not enough data",color="#8892b0",size=6)+theme_navs())
      ggplot(d,aes(x=PlateLocSide,y=PlateLocHeight)) +
        stat_density_2d(aes(fill=after_stat(density)),geom="raster",contour=FALSE) +
        scale_fill_gradientn(colours=heat_fills,guide="none") +
        annotate("rect",xmin=-1,xmax=1,ymin=1.6,ymax=3.4,fill=NA,color="#ffffff",linewidth=.7) +
        ylim(1,4)+xlim(-1.8,1.8) +
        facet_wrap(~TaggedPitchType,ncol=3) +
        labs(title=paste(input$p_pitcher,"—",p_season(),"Heat Maps"),subtitle=sub,
             x="Horizontal",y="Vertical") +
        theme_navs()
    } else {
      loc_heatmap(d, paste(input$p_pitcher,"—",p_season(),input$p_hm_pitch,"Heat Map"), sub)
    }
  }, bg="#0f1117")

  # ── Pitch Locations (individual dots, hover) ──────────────────────────────
  output$p_pl_pitch_ui <- renderUI({
    req(!is.null(p_raw()))
    selectInput("p_pl_pitch","Pitch Type",
                choices=c("All Pitches",sort(unique(p_raw()$TaggedPitchType))))
  })
  output$p_pl_count_ui <- renderUI({
    d <- p_filt(); if(is.null(d)) return(NULL)
    selectInput("p_pl_count","Count / Situation",choices=count_choices(d$CountIndiv))
  })
  output$p_pitchloc <- renderPlotly({
    d <- p_filt(); req(d, nrow(d)>0)
    if (!is.null(input$p_pl_pitch) && input$p_pl_pitch!="All Pitches")
      d <- d %>% filter(TaggedPitchType==input$p_pl_pitch)
    d <- filter_by_count(d, input$p_pl_count, "CountSit", "CountIndiv")
    if (!is.null(input$p_pl_hand) && input$p_pl_hand!="Combined")
      d <- d %>% filter(BatterSide==input$p_pl_hand)
    d <- d %>% filter(!is.na(PlateLocSide), !is.na(PlateLocHeight))
    validate(need(nrow(d)>0, "No pitches for this selection"))

    d <- d %>% mutate(Outcome=dplyr::case_when(
      PitchCall=="StrikeSwinging"                ~ "Whiff",
      PitchCall=="StrikeCalled"                  ~ "Called Strike",
      PitchCall=="InPlay"                        ~ "In Play",
      PitchCall=="FoulBall"                      ~ "Foul",
      PitchCall %in% c("BallCalled","HitByPitch")~ "Ball",
      TRUE                                       ~ "Other"))
    d$hover <- paste0(d$TaggedPitchType,
                      "<br>Velo: ",round(d$RelSpeed,1)," mph",
                      "<br>Count: ",d$Balls,"-",d$Strikes,
                      "<br>Result: ",d$Outcome,
                      "<br>Loc: ",round(d$PlateLocSide,2),", ",round(d$PlateLocHeight,2))

    base <- ggplot(d,aes(x=PlateLocSide,y=PlateLocHeight)) +
      annotate("rect",xmin=-1,xmax=1,ymin=1.6,ymax=3.4,fill=NA,color="#ffffff",linewidth=.7) +
      ylim(1,4)+xlim(-1.8,1.8) +
      labs(title=paste(input$p_pitcher,"—",p_season(),"Pitch Locations"),
           x="Horizontal",y="Vertical") +
      theme_navs()

    if ((input$p_pl_color %||% "pitch")=="outcome") {
      d$Outcome <- factor(d$Outcome,
        levels=c("Whiff","Called Strike","In Play","Foul","Ball","Other"))
      pl_outcome_pal <- c("Whiff"="#ff4655","Called Strike"="#ffd700","In Play"="#00d4ff",
                          "Foul"="#a78bfa","Ball"="#64748b","Other"="#94a3b8")
      gg <- base + geom_point(aes(color=Outcome,text=hover),size=2.4,alpha=.85) +
        scale_color_manual(values=pl_outcome_pal,name="Outcome",drop=FALSE)
    } else {
      gg <- base + geom_point(aes(color=TaggedPitchType,text=hover),size=2.4,alpha=.85) +
        scale_color_manual(values=pitch_pal,na.value="#94a3b8",name="Pitch Type")
    }
    if (is.null(input$p_pl_pitch) || input$p_pl_pitch=="All Pitches")
      gg <- gg + facet_wrap(~TaggedPitchType,ncol=3)
    to_dark_plotly(gg)
  })

  # ── Velo / Spin ───────────────────────────────────────────────────────────
  trend_plot <- function(d, y_col, y_lab, title_str) {
    if(is.null(d)||nrow(d)==0) return(NULL)
    pd <- d %>% arrange(Date,PitchNo) %>%
      group_by(TaggedPitchType) %>% mutate(idx=row_number()-1) %>% ungroup()
    smry <- pd %>% group_by(TaggedPitchType) %>%
      summarise(avg=mean(.data[[y_col]],na.rm=TRUE),
                mn=min(.data[[y_col]],na.rm=TRUE),
                mx=max(.data[[y_col]],na.rm=TRUE),
                max_idx=max(idx), last_v=last(.data[[y_col]]),.groups="drop") %>%
      mutate(lbl=paste0(round(avg,1),if(y_col=="RelSpeed")" mph\n(" else " rpm\n(",
                        round(mn,0),"-",round(mx,0),")"))
    ggplot(pd,aes(x=idx,y=.data[[y_col]],color=TaggedPitchType)) +
      geom_line(linewidth=1.2,alpha=.85) +
      geom_point(size=1.5,alpha=.4) +
      geom_text(data=smry,aes(x=max_idx,y=last_v,label=lbl,color=TaggedPitchType),
                hjust=-0.1,vjust=.5,size=2.8,fontface="plain",lineheight=.85) +
      scale_color_manual(values=pitch_pal,na.value="#94a3b8") +
      scale_x_continuous(expand=expansion(mult=c(.02,.18))) +
      labs(title=title_str,x="Pitch Count",y=y_lab,color="Pitch Type") +
      theme_navs()
  }

  output$p_velo <- renderPlot({
    trend_plot(p_filt(),"RelSpeed","Velocity (MPH)",
               paste(input$p_pitcher,"—",p_season(),"Velocity"))
  }, bg="#0f1117")
  output$p_spin <- renderPlot({
    trend_plot(p_filt(),"SpinRate","Spin Rate (RPM)",
               paste(input$p_pitcher,"—",p_season(),"Spin Rate"))
  }, bg="#0f1117")

  # ── Count / Pitch×Hand split tables ──────────────────────────────────────
  output$p_countTable <- renderDataTable({
    d<-p_filt();req(d,nrow(d)>0)
    tbl <- d %>% group_by(Count) %>%
      summarise(
        Pitches=n(),PA=sum(PACheck,na.rm=TRUE),AB=sum(ABCheck,na.rm=TRUE),
        H=sum(HCheck,na.rm=TRUE),SO=sum(StrikeoutCheck,na.rm=TRUE),
        BB=sum(WalkCheck,na.rm=TRUE),
        `CSW%`=paste0(round(mean(CSWCheck,na.rm=TRUE)*100,1),"%"),
        `Zone%`=paste0(round(mean(ZoneCheck,na.rm=TRUE)*100,1),"%"),
        .groups="drop"
      ) %>%
      mutate(AVG=sprintf("%.3f",ifelse(AB>0,H/AB,NA)),
             OBP=sprintf("%.3f",ifelse(PA>0,(H+BB)/PA,NA))) %>%
      arrange(Count)
    datatable(tbl,options=dt_opts,rownames=FALSE)
  })

  output$p_ptHandTable <- renderDataTable({
    d<-p_filt();req(d,nrow(d)>0)
    tbl <- d %>% group_by(Pitch=TaggedPitchType,Side=BatterSide) %>%
      summarise(
        Pitches=n(),
        `CSW%`=paste0(round(mean(CSWCheck,na.rm=TRUE)*100,1),"%"),
        `Zone%`=paste0(round(mean(ZoneCheck,na.rm=TRUE)*100,1),"%"),
        `Whiff%`={sw=sum(SwingCheck,na.rm=TRUE);paste0(if(sw>0)round(sum(WhiffCheck,na.rm=TRUE)/sw*100,1)else 0,"%")},
        `Avg Velo`=round(mean(RelSpeed,na.rm=TRUE),1),
        .groups="drop"
      )
    datatable(tbl,options=dt_opts,rownames=FALSE)
  })

  # ── Pitcher Batted Ball ───────────────────────────────────────────────────
  bb_stats_p <- function(d) {
    d %>% summarise(
      BBE  = n(),
      GB   = sum(TaggedHitType=="GroundBall", na.rm=TRUE),
      FB   = sum(TaggedHitType=="FlyBall",    na.rm=TRUE),
      LD   = sum(TaggedHitType=="LineDrive",  na.rm=TRUE),
      PU   = sum(TaggedHitType=="Popup",      na.rm=TRUE),
      `GB%`  = ifelse(BBE>0, paste0(round(GB/BBE*100,1),"%"), "—"),
      `FB%`  = ifelse(BBE>0, paste0(round(FB/BBE*100,1),"%"), "—"),
      `LD%`  = ifelse(BBE>0, paste0(round(LD/BBE*100,1),"%"), "—"),
      `PU%`  = ifelse(BBE>0, paste0(round(PU/BBE*100,1),"%"), "—"),
      `GB/FB`= ifelse(FB>0,  as.character(round(GB/FB,2)), "—"),
      .groups="drop"
    ) %>% filter(BBE>0)
  }

  p_bb_data <- reactive({
    d <- p_filt(); req(d, nrow(d)>0)
    if (!is.null(input$p_bb_pitch) && input$p_bb_pitch!="All Pitches")
      d <- d %>% filter(TaggedPitchType==input$p_bb_pitch)
    if (!is.null(input$p_bb_hand) && input$p_bb_hand!="Combined")
      d <- d %>% filter(BatterSide==input$p_bb_hand)
    d %>% filter(TaggedHitType %in% c("GroundBall","LineDrive","FlyBall","Popup"))
  })

  output$p_bb_pitch_ui <- renderUI({
    req(!is.null(p_raw()))
    selectInput("p_bb_pitch","Pitch Type",
                choices=c("All Pitches",sort(unique(p_raw()$TaggedPitchType))))
  })

  output$p_bb_card_gb <- renderUI({
    d<-p_bb_data();req(nrow(d)>0)
    n<-sum(d$TaggedHitType=="GroundBall",na.rm=TRUE); tot<-nrow(d)
    bb_card(n/tot,n,tot,"GB%")
  })
  output$p_bb_card_fb <- renderUI({
    d<-p_bb_data();req(nrow(d)>0)
    n<-sum(d$TaggedHitType=="FlyBall",na.rm=TRUE); tot<-nrow(d)
    bb_card(n/tot,n,tot,"FB%")
  })
  output$p_bb_card_ld <- renderUI({
    d<-p_bb_data();req(nrow(d)>0)
    n<-sum(d$TaggedHitType=="LineDrive",na.rm=TRUE); tot<-nrow(d)
    bb_card(n/tot,n,tot,"LD%")
  })
  output$p_bb_card_pu <- renderUI({
    d<-p_bb_data();req(nrow(d)>0)
    n<-sum(d$TaggedHitType=="Popup",na.rm=TRUE); tot<-nrow(d)
    bb_card(n/tot,n,tot,"PU%")
  })

  output$p_bb_ptTable <- renderDataTable({
    d<-p_bb_data();req(nrow(d)>0)
    datatable(d%>%group_by(Pitch=TaggedPitchType)%>%bb_stats_p(),
              options=dt_opts,rownames=FALSE)
  })
  output$p_bb_totalTable <- renderDataTable({
    d<-p_bb_data();req(nrow(d)>0)
    datatable(d%>%mutate(Total="All")%>%group_by(Total)%>%bb_stats_p(),
              options=dt_opts,rownames=FALSE)
  })
  output$p_bb_lrTable <- renderDataTable({
    d<-p_bb_data();req(nrow(d)>0)
    datatable(d%>%group_by(Side=BatterSide)%>%bb_stats_p(),
              options=dt_opts,rownames=FALSE)
  })
  output$p_bb_ptLrTable <- renderDataTable({
    d<-p_bb_data();req(nrow(d)>0)
    datatable(d%>%group_by(Pitch=TaggedPitchType,Side=BatterSide)%>%bb_stats_p(),
              options=dt_opts,rownames=FALSE)
  })

  # ══════════════════════════════════════════════════════════════════════════
  # SCOUTING REPORTS (pitchers)
  # ══════════════════════════════════════════════════════════════════════════
  sr_season <- reactive({ input$sr_season %||% "2026" })

  observeEvent(sr_season(), {
    session$sendCustomMessage("setSeasonSR", sr_season())
  })

  # Season-sliced pitcher rows with a normalized pitch column bolted on.
  sr_raw <- reactive({
    res <- seasons[[sr_season()]]$pitchers
    if (is.null(res) || is.null(res$data) || nrow(res$data)==0) return(NULL)
    res$data %>%
      mutate(
        Pitch = normalize_pitch(TaggedPitchType,
                                isTRUE(input$sr_norm_pitch %||% TRUE)),
        # wOBA against. Weighted only on the pitch that ended the PA — every
        # other pitch contributes NA so it stays out of the numerator.
        wOBAcon = dplyr::case_when(
          KorBB == "Walk"                ~ unname(woba_weights["Walk"]),
          PitchCall == "HitByPitch"      ~ unname(woba_weights["HitByPitch"]),
          PlayResult == "Single"         ~ unname(woba_weights["Single"]),
          PlayResult == "Double"         ~ unname(woba_weights["Double"]),
          PlayResult == "Triple"         ~ unname(woba_weights["Triple"]),
          PlayResult == "HomeRun"        ~ unname(woba_weights["HomeRun"]),
          PlayResult == "FieldersChoice" ~ unname(woba_weights["FieldersChoice"]),
          PlayResult == "Sacrifice"      ~ unname(woba_weights["Sacrifice"]),
          PlayResult == "Error"          ~ unname(woba_weights["Error"]),
          KorBB == "Strikeout"           ~ unname(woba_weights["Out"]),
          PlayResult == "Out"            ~ unname(woba_weights["Out"]),
          TRUE                           ~ NA_real_
        ))
  })

  sr_all_dates <- reactive({
    d <- sr_raw(); if (is.null(d)) return(as.Date(character(0)))
    sort(unique(d$Date))
  })

  output$sr_date_from_ui <- renderUI({
    dts <- sr_all_dates(); req(length(dts)>0)
    dateInput("sr_date_from","Include outings on or after",
              value=min(dts), min=min(dts), max=max(dts))
  })

  output$sr_date_pick_ui <- renderUI({
    dts <- sr_all_dates(); req(length(dts)>0)
    lbl <- format(dts, "%b %d, %Y")
    selectizeInput("sr_dates_pick","Outings", choices=setNames(as.character(dts), lbl),
                   selected=as.character(dts), multiple=TRUE,
                   options=list(plugins=list("remove_button")))
  })

  # Date scope applied before anything else, so player lists reflect it too.
  sr_scoped <- reactive({
    d <- sr_raw(); req(!is.null(d))
    mode <- input$sr_date_mode %||% "all"
    if (mode == "after" && !is.null(input$sr_date_from)) {
      d <- d %>% filter(Date >= as.Date(input$sr_date_from))
    } else if (mode == "pick" && !is.null(input$sr_dates_pick) &&
               length(input$sr_dates_pick) > 0) {
      d <- d %>% filter(Date %in% as.Date(input$sr_dates_pick))
    }
    if (!is.null(input$sr_hand) && input$sr_hand != "Combined")
      d <- d %>% filter(BatterSide == input$sr_hand)
    d
  })

  sr_pitcher_list <- reactive({
    d <- sr_scoped(); if (is.null(d) || nrow(d)==0) return(character(0))
    sort(unique(d$Pitcher))
  })

  output$sr_players_ui <- renderUI({
    pl <- sr_pitcher_list()
    if (length(pl)==0) return(tags$p(style="color:#8892b0;","No outings in range."))
    checkboxGroupInput("sr_players", NULL, choices=pl, selected=pl[1])
  })

  observeEvent(input$sr_sel_all, {
    updateCheckboxGroupInput(session,"sr_players", selected=sr_pitcher_list())
  })
  observeEvent(input$sr_sel_none, {
    updateCheckboxGroupInput(session,"sr_players", selected=character(0))
  })

  output$sr_dataStatus <- renderUI({
    d <- sr_scoped()
    if (is.null(d) || nrow(d)==0)
      return(tags$div(class="section-sub", style="color:#ff4655;",
                      "No pitches match the current outing filter."))
    tags$div(class="section-sub",
             paste0(format(nrow(d), big.mark=","), " pitches \u00b7 ",
                    length(unique(d$Date)), " outings \u00b7 ",
                    length(sr_pitcher_list()), " pitchers in scope"))
  })

  output$sr_status_ui <- renderUI({
    n <- length(input$sr_players %||% character(0))
    s <- length(input$sr_sections %||% character(0))
    tags$div(class="section-sub",
             paste0(n, " pitcher", if (n==1) "" else "s", " \u00b7 ",
                    s, " section", if (s==1) "" else "s", " selected"))
  })

  # ── Metric builders ────────────────────────────────────────────────────────
  sr_pct <- function(x) ifelse(is.finite(x), paste0(round(x*100,1),"%"), "\u2014")
  sr_num <- function(x, d=1) ifelse(is.finite(x), as.character(round(x,d)), "\u2014")

  sr_results_tbl <- function(d, ...) {
    d %>% group_by(...) %>%
      summarise(
        Pitches = n(),
        PA  = sum(PACheck, na.rm=TRUE),
        AB  = sum(ABCheck, na.rm=TRUE),
        H   = sum(HCheck, na.rm=TRUE),
        `1B`= sum(PlayResult=="Single",  na.rm=TRUE),
        `2B`= sum(PlayResult=="Double",  na.rm=TRUE),
        `3B`= sum(PlayResult=="Triple",  na.rm=TRUE),
        HR  = sum(PlayResult=="HomeRun", na.rm=TRUE),
        SO  = sum(StrikeoutCheck, na.rm=TRUE),
        BB  = sum(WalkCheck, na.rm=TRUE),
        csw = mean(CSWCheck, na.rm=TRUE),
        sw  = sum(SwingCheck, na.rm=TRUE),
        wh  = sum(WhiffCheck, na.rm=TRUE),
        woba_sum = sum(wOBAcon, na.rm=TRUE),
        woba_n   = sum(!is.na(wOBAcon)),
        .groups="drop"
      ) %>%
      mutate(
        TB   = `1B` + 2*`2B` + 3*`3B` + 4*HR,
        AVG  = ifelse(AB>0, sprintf("%.3f", H/AB), "\u2014"),
        OBP  = ifelse(PA>0, sprintf("%.3f", (H+BB)/PA), "\u2014"),
        SLG  = ifelse(AB>0, sprintf("%.3f", TB/AB), "\u2014"),
        `K%`  = sr_pct(ifelse(PA>0, SO/PA, NA)),
        `BB%` = sr_pct(ifelse(PA>0, BB/PA, NA)),
        `CSW%`   = sr_pct(csw),
        `Whiff%` = sr_pct(ifelse(sw>0, wh/sw, NA)),
        wOBA     = ifelse(woba_n>0, sprintf("%.3f", woba_sum/woba_n), "\u2014")
      ) %>%
      dplyr::select(-csw,-sw,-wh,-TB,-`1B`,-`2B`,-`3B`,-AB,-woba_sum,-woba_n)
  }

  sr_pd_tbl <- function(d, ...) {
    d %>% group_by(...) %>%
      summarise(
        Pitches = n(),
        zone    = mean(ZoneCheck, na.rm=TRUE),
        z_n     = sum(ZoneCheck, na.rm=TRUE),
        z_sw    = sum(SwingCheck & ZoneCheck, na.rm=TRUE),
        z_wh    = sum(WhiffCheck & ZoneCheck, na.rm=TRUE),
        o_n     = sum(!ZoneCheck, na.rm=TRUE),
        o_sw    = sum(SwingCheck & !ZoneCheck, na.rm=TRUE),
        o_wh    = sum(WhiffCheck & !ZoneCheck, na.rm=TRUE),
        sw      = sum(SwingCheck, na.rm=TRUE),
        wh      = sum(WhiffCheck, na.rm=TRUE),
        csw     = mean(CSWCheck, na.rm=TRUE),
        .groups="drop"
      ) %>%
      mutate(
        `Zone%`       = sr_pct(zone),
        `Swing%`      = sr_pct(ifelse(Pitches>0, sw/Pitches, NA)),
        `Z-Swing%`    = sr_pct(ifelse(z_n>0, z_sw/z_n, NA)),
        `Z-Contact%`  = sr_pct(ifelse(z_sw>0, (z_sw-z_wh)/z_sw, NA)),
        `Chase%`      = sr_pct(ifelse(o_n>0, o_sw/o_n, NA)),
        `O-Contact%`  = sr_pct(ifelse(o_sw>0, (o_sw-o_wh)/o_sw, NA)),
        `Whiff%`      = sr_pct(ifelse(sw>0, wh/sw, NA)),
        `CSW%`        = sr_pct(csw)
      ) %>%
      dplyr::select(-zone,-z_n,-z_sw,-z_wh,-o_n,-o_sw,-o_wh,-sw,-wh,-csw)
  }

  sr_bb_tbl <- function(d, ...) {
    d %>%
      filter(TaggedHitType %in% c("GroundBall","LineDrive","FlyBall","Popup")) %>%
      mutate(EV = suppressWarnings(as.numeric(ExitSpeed)),
             LA = suppressWarnings(as.numeric(Angle))) %>%
      group_by(...) %>%
      summarise(
        BBE = n(),
        gb  = sum(TaggedHitType=="GroundBall", na.rm=TRUE),
        fb  = sum(TaggedHitType=="FlyBall",    na.rm=TRUE),
        ld  = sum(TaggedHitType=="LineDrive",  na.rm=TRUE),
        pu  = sum(TaggedHitType=="Popup",      na.rm=TRUE),
        ev_avg = mean(EV, na.rm=TRUE),
        ev_max = suppressWarnings(max(EV, na.rm=TRUE)),
        la_avg = mean(LA, na.rm=TRUE),
        hh  = sum(EV >= 90, na.rm=TRUE),
        brl = sum(EV >= 90 & LA >= 8 & LA <= 32, na.rm=TRUE),
        ev_n = sum(!is.na(EV)),
        .groups="drop"
      ) %>%
      mutate(
        `GB%`      = sr_pct(gb/BBE),
        `LD%`      = sr_pct(ld/BBE),
        `FB%`      = sr_pct(fb/BBE),
        `PU%`      = sr_pct(pu/BBE),
        `Avg EV`   = sr_num(ev_avg),
        `Max EV`   = sr_num(ev_max),
        `Avg LA`   = sr_num(la_avg),
        `HardHit%` = sr_pct(ifelse(ev_n>0, hh/ev_n, NA)),
        `Barrel%`  = sr_pct(ifelse(ev_n>0, brl/ev_n, NA))
      ) %>%
      dplyr::select(-gb,-fb,-ld,-pu,-ev_avg,-ev_max,-la_avg,-hh,-brl,-ev_n)
  }

  sr_arsenal_tbl <- function(d) {
    d %>% group_by(Pitch) %>%
      summarise(
        Pitches = n(),
        `Avg Velo` = sr_num(mean(RelSpeed, na.rm=TRUE)),
        `Max Velo` = sr_num(suppressWarnings(max(RelSpeed, na.rm=TRUE))),
        Spin = sr_num(mean(SpinRate, na.rm=TRUE), 0),
        IVB  = sr_num(mean(InducedVertBreak, na.rm=TRUE)),
        HB   = sr_num(mean(HorzBreak, na.rm=TRUE)),
        `Rel H` = sr_num(mean(RelHeight, na.rm=TRUE), 2),
        `Rel S` = sr_num(mean(RelSide, na.rm=TRUE), 2),
        csw = mean(CSWCheck, na.rm=TRUE),
        sw  = sum(SwingCheck, na.rm=TRUE),
        wh  = sum(WhiffCheck, na.rm=TRUE),
        .groups="drop"
      ) %>%
      mutate(Usage = sr_pct(Pitches/sum(Pitches)),
             `CSW%` = sr_pct(csw),
             `Whiff%` = sr_pct(ifelse(sw>0, wh/sw, NA))) %>%
      dplyr::select(Pitch, Pitches, Usage, `Avg Velo`, `Max Velo`, Spin,
                    IVB, HB, `Rel H`, `Rel S`, `CSW%`, `Whiff%`) %>%
      arrange(desc(Pitches))
  }

  sr_velo_spin_tbl <- function(d) {
    d %>% group_by(Pitch) %>%
      summarise(
        N = n(),
        `Avg Velo` = sr_num(mean(RelSpeed, na.rm=TRUE)),
        `Max Velo` = sr_num(suppressWarnings(max(RelSpeed, na.rm=TRUE))),
        `Velo SD`  = sr_num(sd(RelSpeed, na.rm=TRUE), 2),
        `Avg Spin` = sr_num(mean(SpinRate, na.rm=TRUE), 0),
        `Max Spin` = sr_num(suppressWarnings(max(SpinRate, na.rm=TRUE)), 0),
        `Extension`= sr_num(mean(suppressWarnings(as.numeric(Extension)), na.rm=TRUE), 2),
        IVB = sr_num(mean(InducedVertBreak, na.rm=TRUE)),
        HB  = sr_num(mean(HorzBreak, na.rm=TRUE)),
        .groups="drop"
      ) %>% arrange(desc(N))
  }

  sr_kpi_row <- function(d) {
    pa  <- sum(d$PACheck, na.rm=TRUE)
    sw  <- sum(d$SwingCheck, na.rm=TRUE)
    ozn <- sum(!d$ZoneCheck, na.rm=TRUE)
    k   <- sum(d$StrikeoutCheck, na.rm=TRUE)
    bb  <- sum(d$WalkCheck, na.rm=TRUE)
    wn  <- sum(!is.na(d$wOBAcon))
    bbe <- d[d$TaggedHitType %in% c("GroundBall","LineDrive","FlyBall","Popup"), , drop=FALSE]
    ev  <- suppressWarnings(as.numeric(bbe$ExitSpeed))
    ev  <- ev[!is.na(ev)]
    data.frame(
      Pitches  = format(nrow(d), big.mark=","),
      PA       = pa,
      `K%`     = sr_pct(ifelse(pa>0, k/pa, NA)),
      `BB%`    = sr_pct(ifelse(pa>0, bb/pa, NA)),
      `K-BB%`  = sr_pct(ifelse(pa>0, (k-bb)/pa, NA)),
      `CSW%`   = sr_pct(mean(d$CSWCheck, na.rm=TRUE)),
      `Whiff%` = sr_pct(ifelse(sw>0, sum(d$WhiffCheck,na.rm=TRUE)/sw, NA)),
      `Zone%`  = sr_pct(mean(d$ZoneCheck, na.rm=TRUE)),
      `Chase%` = sr_pct(ifelse(ozn>0, sum(d$SwingCheck & !d$ZoneCheck, na.rm=TRUE)/ozn, NA)),
      `HardHit%` = sr_pct(if (length(ev)>0) mean(ev >= 90) else NA),
      wOBA     = ifelse(wn>0, sprintf("%.3f", sum(d$wOBAcon, na.rm=TRUE)/wn), "\u2014"),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }

  sr_hm_plot <- function(d, title_str) {
    d <- d %>% filter(!is.na(PlateLocSide), !is.na(PlateLocHeight))
    if (nrow(d) < 5)
      return(ggplot() + annotate("text",x=0,y=0,label="Insufficient data",
                                 color="#8892b0",size=5) + theme_navs() +
             labs(title=title_str) +
             theme(axis.text=element_blank(),axis.title=element_blank(),
                   panel.grid=element_blank()))
    keep <- d %>% count(Pitch) %>% filter(n >= 5) %>% pull(Pitch)
    d <- d %>% filter(Pitch %in% keep)
    if (nrow(d) < 5)
      return(ggplot() + annotate("text",x=0,y=0,label="Insufficient data",
                                 color="#8892b0",size=5) + theme_navs() +
             labs(title=title_str) +
             theme(axis.text=element_blank(),axis.title=element_blank(),
                   panel.grid=element_blank()))
    ggplot(d, aes(x=PlateLocSide, y=PlateLocHeight)) +
      stat_density_2d(aes(fill=after_stat(density)), geom="raster", contour=FALSE) +
      scale_fill_gradientn(colours=heat_fills, guide="none") +
      annotate("rect", xmin=-1, xmax=1, ymin=1.6, ymax=3.4,
               fill=NA, color="#ffffff", linewidth=.6) +
      facet_wrap(~Pitch) +
      xlim(-2,2) + ylim(1,4) +
      labs(title=title_str, x="Horizontal (Pitcher's View)", y="Vertical") +
      theme_navs()
  }

  # ── Item builder: one named list entry per selected section ────────────────
  sr_build_items <- function(d, sections) {
    min_n <- max(1, as.integer(input$sr_min_n %||% 3))
    out <- list()
    add <- function(key, type, title, data=NULL, plot=NULL) {
      out[[key]] <<- list(type=type, title=title, data=data, plot=plot)
    }
    if ("kpi" %in% sections)
      add("kpi","table","Summary", data=sr_kpi_row(d))
    if ("arsenal" %in% sections)
      add("arsenal","table","Pitch Arsenal", data=sr_arsenal_tbl(d))
    if ("res_all" %in% sections)
      add("res_all","table","Results (Cumulative)",
          data=sr_results_tbl(d %>% mutate(Split="All"), Split))
    if ("res_pt" %in% sections)
      add("res_pt","table","Results by Pitch Type",
          data=sr_results_tbl(d, Pitch) %>% filter(Pitches>=min_n))
    if ("res_lr" %in% sections)
      add("res_lr","table","Results vs L/R",
          data=sr_results_tbl(d, Side=BatterSide))
    if ("res_pt_lr" %in% sections)
      add("res_pt_lr","table","Results by Pitch Type x Handedness",
          data=sr_results_tbl(d, Pitch, Side=BatterSide) %>% filter(Pitches>=min_n))
    if ("pd_all" %in% sections)
      add("pd_all","table","Plate Discipline (Cumulative)",
          data=sr_pd_tbl(d %>% mutate(Split="All"), Split))
    if ("pd_pt" %in% sections)
      add("pd_pt","table","Plate Discipline by Pitch Type",
          data=sr_pd_tbl(d, Pitch) %>% filter(Pitches>=min_n))
    if ("pd_lr" %in% sections)
      add("pd_lr","table","Plate Discipline vs L/R",
          data=sr_pd_tbl(d, Side=BatterSide))
    if ("pd_pt_lr" %in% sections)
      add("pd_pt_lr","table","Plate Discipline by Pitch Type x Handedness",
          data=sr_pd_tbl(d, Pitch, Side=BatterSide) %>% filter(Pitches>=min_n))
    if ("bb_all" %in% sections)
      add("bb_all","table","Batted Ball (Cumulative)",
          data=sr_bb_tbl(d %>% mutate(Split="All"), Split))
    if ("bb_pt" %in% sections)
      add("bb_pt","table","Batted Ball by Pitch Type",
          data=sr_bb_tbl(d, Pitch) %>% filter(BBE>=1))
    if ("bb_lr" %in% sections)
      add("bb_lr","table","Batted Ball vs L/R",
          data=sr_bb_tbl(d, Side=BatterSide))
    if ("bb_pt_lr" %in% sections)
      add("bb_pt_lr","table","Batted Ball by Pitch Type x Handedness",
          data=sr_bb_tbl(d, Pitch, Side=BatterSide) %>% filter(BBE>=1))
    if ("velo_spin" %in% sections)
      add("velo_spin","table","Velocity & Spin", data=sr_velo_spin_tbl(d))
    if ("movement" %in% sections)
      add("movement","plot","Pitch Movement", plot=
        ggplot(d %>% filter(!is.na(HorzBreak), !is.na(InducedVertBreak)),
               aes(x=HorzBreak, y=InducedVertBreak, color=Pitch)) +
          geom_hline(yintercept=0, color="#2a2d3a", linewidth=.8) +
          geom_vline(xintercept=0, color="#2a2d3a", linewidth=.8) +
          geom_point(alpha=.7, size=1.8) +
          scale_color_manual(values=sr_pitch_pal(d$Pitch), na.value="#94a3b8") +
          labs(title="Pitch Movement", x="Horizontal Break (in)",
               y="Induced Vertical Break (in)", color=NULL) +
          theme_navs())
    if ("release" %in% sections)
      add("release","plot","Release Points", plot=
        ggplot(d %>% filter(!is.na(RelSide), !is.na(RelHeight)),
               aes(x=RelSide, y=RelHeight, color=Pitch)) +
          geom_point(alpha=.7, size=1.8) +
          scale_color_manual(values=sr_pitch_pal(d$Pitch), na.value="#94a3b8") +
          labs(title="Release Points", x="Release Side (ft)",
               y="Release Height (ft)", color=NULL) +
          theme_navs())
    if ("hm_r" %in% sections)
      add("hm_r","plot","Locations vs RHH",
          plot=sr_hm_plot(d %>% filter(BatterSide=="Right"), "Locations vs RHH"))
    if ("hm_l" %in% sections)
      add("hm_l","plot","Locations vs LHH",
          plot=sr_hm_plot(d %>% filter(BatterSide=="Left"), "Locations vs LHH"))
    if ("hm_c" %in% sections)
      add("hm_c","plot","Locations (Combined)",
          plot=sr_hm_plot(d, "Locations (Combined)"))
    out[sections[sections %in% names(out)]]
  }

  # ── Preview ────────────────────────────────────────────────────────────────
  # Static HTML table so the preview stays inside a single renderUI instead of
  # spawning one DT output per section per pitcher.
  sr_html_tbl <- function(df) {
    if (is.null(df) || nrow(df)==0)
      return(tags$p(style="color:#8892b0;font-size:12px;","No rows."))
    df[] <- lapply(df, as.character)
    tags$div(style="overflow-x:auto;margin-bottom:14px;",
      tags$table(class="sr-tbl",
        tags$thead(tags$tr(lapply(names(df), function(n) tags$th(n)))),
        tags$tbody(lapply(seq_len(nrow(df)), function(i)
          tags$tr(lapply(seq_len(ncol(df)), function(j) tags$td(df[i,j])))))
      ))
  }

  sr_preview_data <- reactive({
    req(input$sr_players, length(input$sr_players)>0)
    secs <- input$sr_sections %||% character(0)
    req(length(secs)>0)
    d_all <- sr_scoped(); req(!is.null(d_all), nrow(d_all)>0)
    players <- head(input$sr_players, 5)
    res <- lapply(players, function(pl) {
      d <- d_all %>% filter(Pitcher == pl)
      if (nrow(d)==0) return(NULL)
      list(player=pl, n=nrow(d), items=sr_build_items(d, secs))
    })
    Filter(Negate(is.null), res)
  })

  output$sr_preview_ui <- renderUI({
    pv <- sr_preview_data()
    if (length(pv)==0)
      return(tags$p(style="color:#8892b0;","Select at least one pitcher and one section."))
    tagList(lapply(seq_along(pv), function(i) {
      pl <- pv[[i]]
      tagList(
        tags$div(class="section-header",
                 paste0(pl$player, "  \u00b7  ", format(pl$n, big.mark=","), " pitches")),
        lapply(seq_along(pl$items), function(j) {
          it <- pl$items[[j]]
          if (identical(it$type,"table")) {
            tagList(tags$div(class="section-sub", it$title), sr_html_tbl(it$data))
          } else {
            tagList(tags$div(class="section-sub", it$title),
                    plotOutput(paste0("sr_pv_",i,"_",j), height="380px"))
          }
        }),
        tags$hr()
      )
    }))
  })

  # Plot outputs are registered separately; renderUI only emits the placeholders.
  observe({
    pv <- sr_preview_data()
    for (i in seq_along(pv)) {
      for (j in seq_along(pv[[i]]$items)) {
        local({
          ii <- i; jj <- j
          it <- pv[[ii]]$items[[jj]]
          if (identical(it$type,"plot") && !is.null(it$plot))
            output[[paste0("sr_pv_",ii,"_",jj)]] <- renderPlot({ it$plot })
        })
      }
    }
  })

  # ── PDF export ─────────────────────────────────────────────────────────────
  output$sr_download <- downloadHandler(
    filename = function() {
      paste0("Navs_PitcherScoutingReport_", sr_season(), "_",
             format(Sys.Date(), "%Y%m%d"), ".pdf")
    },
    content = function(file) {
      players  <- input$sr_players %||% character(0)
      sections <- input$sr_sections %||% character(0)
      if (length(players)==0 || length(sections)==0) {
        showNotification("Pick at least one pitcher and one section.", type="warning")
        return(invisible())
      }
      d_all <- sr_scoped()
      req(!is.null(d_all), nrow(d_all)>0)

      scope_txt <- {
        dts <- sort(unique(d_all$Date))
        if (length(dts)==0) "" else
          paste0(format(min(dts),"%b %d"), " \u2013 ", format(max(dts),"%b %d, %Y"),
                 "  (", length(dts), " outings)")
      }

      # Table rendered as a ggplot so it can share a page with charts.
      make_tbl_plot <- function(tbl, title_str, fsize=3, title_size=10) {
        tbl <- as.data.frame(tbl, check.names=FALSE)
        tbl[] <- lapply(tbl, as.character)
        n_cols <- ncol(tbl); n_rows <- nrow(tbl)
        if (n_cols==0 || n_rows==0)
          return(ggplot() + labs(title=title_str) + theme_void())
        tbl_long <- data.frame(
          x = rep(seq_len(n_cols), each=n_rows+1),
          y = rep(c(n_rows+1, seq(n_rows,1)), n_cols),
          label = c(rbind(names(tbl), do.call(cbind, lapply(tbl, as.character)))),
          is_header = rep(c(TRUE, rep(FALSE,n_rows)), n_cols),
          stringsAsFactors=FALSE
        )
        ggplot(tbl_long, aes(x=x, y=y, label=label)) +
          geom_tile(aes(fill=is_header), color="grey70", linewidth=0.3) +
          geom_text(aes(fontface=ifelse(is_header,"bold","plain"),
                        color=ifelse(is_header,"#222222","#333333")),
                    size=fsize, hjust=0.5) +
          scale_fill_manual(values=c("FALSE"="white","TRUE"="#e8e8e8"), guide="none") +
          scale_color_identity() +
          labs(title=title_str) +
          theme_void(base_size=9) +
          theme(plot.title=element_text(face="bold", size=title_size, hjust=0,
                                        margin=margin(b=6)),
                plot.margin=margin(10,10,10,10),
                plot.background=element_rect(fill="white", color=NA))
      }

      page_header <- function(txt) {
        grid::pushViewport(grid::viewport(x=0.5, y=0.965, width=1, height=0.07,
                                          just="center"))
        grid::grid.rect(gp=grid::gpar(fill="#111111", col=NA))
        grid::grid.text(txt, x=0.5, y=0.5,
                        gp=grid::gpar(fontsize=13, col="white", fontface="bold"))
        grid::popViewport()
      }

      # The app is dark-themed; print needs white.
      to_print_theme <- function(p) {
        p + theme(plot.background=element_rect(fill="white", color=NA),
                  panel.background=element_rect(fill="white", color=NA),
                  panel.grid.major=element_line(color="grey85"),
                  panel.grid.minor=element_blank(),
                  axis.text=element_text(color="#333333"),
                  axis.title=element_text(color="#222222"),
                  plot.title=element_text(color="#111111"),
                  plot.subtitle=element_text(color="#444444"),
                  legend.background=element_rect(fill="white", color=NA),
                  legend.key=element_rect(fill="white", color=NA),
                  legend.text=element_text(color="#333333"),
                  strip.background=element_rect(fill="grey90", color=NA),
                  strip.text=element_text(color="#222222"))
      }

      sec_label <- function(sec) {
        lbl <- names(sr_sections_all)[match(sec, sr_sections_all)]
        if (length(lbl)==0 || is.na(lbl)) sec else lbl
      }

      hdr <- function(player) {
        paste0(player, "    |    North Shore Navigators    |    ", sr_season(),
               " Scouting Report", if (nzchar(scope_txt)) paste0("    |    ", scope_txt) else "",
               "    |    ", format(Sys.Date(), "%b %d, %Y"))
      }

      layout <- input$sr_layout %||% "player"
      pdf(file, width=17, height=11, onefile=TRUE)
      on.exit(dev.off(), add=TRUE)

      built <- lapply(players, function(pl) {
        d <- d_all %>% filter(Pitcher == pl)
        if (nrow(d)==0) return(NULL)
        list(player=pl, items=sr_build_items(d, sections))
      })
      built <- Filter(function(x) !is.null(x) && length(x$items)>0, built)

      if (layout == "combined") {
        # One page per section, every pitcher stacked into a single table.
        for (sec in sections) {
          tbls <- lapply(built, function(x) {
            it <- x$items[[sec]]
            if (is.null(it) || it$type != "table" || is.null(it$data) ||
                nrow(it$data)==0) return(NULL)
            df <- as.data.frame(it$data, check.names=FALSE)
            df[] <- lapply(df, as.character)
            cbind(Pitcher=x$player, df)
          })
          tbls <- Filter(Negate(is.null), tbls)
          if (length(tbls) > 0) {
            keys <- unique(unlist(lapply(tbls, names)))
            tbls <- lapply(tbls, function(df) {
              miss <- setdiff(keys, names(df))
              for (m in miss) df[[m]] <- "\u2014"
              df[, keys, drop=FALSE]
            })
            big <- do.call(rbind, tbls)
            n_r <- nrow(big)
            chunk <- 26
            for (start in seq(1, n_r, by=chunk)) {
              part <- big[start:min(start+chunk-1, n_r), , drop=FALSE]
              grid::grid.newpage()
              page_header(paste0(sec_label(sec), "    |    North Shore Navigators    |    ",
                                 sr_season(),
                                 if (nzchar(scope_txt)) paste0("    |    ", scope_txt) else ""))
              print(make_tbl_plot(part, sec_label(sec), fsize=3.4, title_size=12),
                    vp=grid::viewport(x=0.5, y=0.46, width=0.94, height=0.88))
            }
          } else {
            # Plot sections can't be merged — one page per pitcher instead.
            for (x in built) {
              it <- x$items[[sec]]
              if (is.null(it) || it$type != "plot" || is.null(it$plot)) next
              grid::grid.newpage()
              page_header(hdr(x$player))
              print(to_print_theme(it$plot),
                    vp=grid::viewport(x=0.5, y=0.46, width=0.94, height=0.88))
            }
          }
        }
      } else {
        for (x in built) {
          player <- x$player
          plot_list <- x$items
          single_mode <- (layout == "single")
          gg_items <- lapply(names(plot_list), function(nm) {
            it <- plot_list[[nm]]
            if (it$type == "plot" && !is.null(it$plot)) {
              to_print_theme(it$plot)
            } else if (it$type == "table" && !is.null(it$data) && nrow(it$data) > 0) {
              make_tbl_plot(it$data, it$title,
                            fsize = if (single_mode) 5.2
                                    else if (length(plot_list) <= 2) 4.6
                                    else if (length(plot_list) <= 4) 3.8 else 3)
            } else NULL
          })
          gg_items <- Filter(Negate(is.null), gg_items)
          if (length(gg_items) == 0) next
          n_items <- length(gg_items)

          if (single_mode) {
            for (i in seq_len(n_items)) {
              grid::grid.newpage()
              page_header(hdr(player))
              print(gg_items[[i]],
                    vp=grid::viewport(x=0.5, y=0.46, width=0.94, height=0.88))
            }
            next
          }

          ncol <- max(1, ceiling(sqrt(n_items)))
          nrow_ <- max(1, ceiling(n_items / ncol))
          grid::grid.newpage()
          page_header(hdr(player))
          grid::pushViewport(grid::viewport(x=0.5, y=0.46, width=0.98, height=0.90))
          gap <- 0.008; cell_w <- 1/ncol; cell_h <- 1/nrow_
          for (i in seq_len(n_items)) {
            r <- ceiling(i / ncol)
            cc <- i - (r-1)*ncol
            print(gg_items[[i]],
                  vp=grid::viewport(x=(cc-0.5)*cell_w, y=1-(r-0.5)*cell_h,
                                    width=cell_w-gap, height=cell_h-gap,
                                    just="center"))
          }
          grid::popViewport()
        }
      }
    }
  )
}

shinyApp(ui=ui, server=server)
