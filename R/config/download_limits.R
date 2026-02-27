# R/config/download_limits.R
#
# Tunable thresholds for download size warnings and hard stops.
# Edit these values to adjust when users are warned or blocked.
#
# WARN_*       — personal API key: user sees a modal and must confirm to continue.
# SHARED_MAX_* — shared/fallback API key: hard stop, cannot proceed regardless.

DOWNLOAD_LIMITS <- list(

  # ── Personal API key thresholds ─────────────────────────────────────────────
  # Warn when the post-filter record count or BIN count exceeds these values.
  # The user is shown a modal explaining the size and must confirm to proceed.
  WARN_RECORDS = 100000,
  WARN_BINS    = 1000,

  # ── Shared API key thresholds ────────────────────────────────────────────────
  # Hard stop — download cannot continue when using the shared/fallback key.
  # Set equal to or lower than the WARN_* values above.
  SHARED_MAX_RECORDS = 100000,
  SHARED_MAX_BINS    = 1000
)
