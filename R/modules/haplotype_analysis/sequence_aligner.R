# R/modules/haplotype_analysis/sequence_aligner.R

#' @title SequenceAligner R6 Class
#' @description Handles DNA sequence alignment and comparison with validation
#' @importFrom R6 R6Class
#' @importFrom Biostrings DNAStringSet
#' @importFrom DECIPHER AlignSeqs
#' @export
SequenceAligner <- R6::R6Class(
  "SequenceAligner",

  public = list(
    #' @description Initialize aligner with parameters
    #' @param min_overlap Minimum overlap required (default 100bp)
    #' @param match_score Score for matching bases (default 1)
    #' @param mismatch_penalty Penalty for mismatches (default -1)
    #' @param gap_penalty Penalty for gaps (default -2)
    #' @param logger Logger instance
    initialize = function(min_overlap = 100,
                          match_score = 1,
                          mismatch_penalty = -1,
                          gap_penalty = -2,
                          logger = NULL) {
      private$min_overlap <- min_overlap
      private$match_score <- match_score
      private$mismatch_penalty <- mismatch_penalty
      private$gap_penalty <- gap_penalty
      private$logger <- logger
      private$error_boundary <- ErrorBoundary$new()

      # Validate Bioconductor packages
      private$validate_dependencies()
    },

    #' @description Align multiple sequences and identify common regions
    #' @param sequences Character vector of DNA sequences
    #' @return List containing aligned sequences and analysis
    align_sequences = function(sequences) {
      private$error_boundary$catch({
        # Validate input
        validation <- private$validate_sequences(sequences)
        if (!validation$valid) {
          private$log_error("Invalid sequence data", validation$messages)
          return(NULL)
        }

        private$log_info(sprintf("Aligning %d sequences", length(sequences)))

        # Convert to DNAStringSet and align
        dna_set <- Biostrings::DNAStringSet(sequences)
        aligned <- DECIPHER::AlignSeqs(dna_set,
                                       iterations = 2,
                                       refinements = 1,
                                       processors = 1,
                                       verbose = FALSE)

        # Find common regions
        aligned_seqs <- Biostrings::DNAStringSet(aligned)
        consensus <- private$find_common_regions(aligned_seqs)

        private$log_info(sprintf("Alignment complete. Coverage: %.2f%%",
                                 consensus$coverage * 100))

        list(
          sequences = aligned_seqs,
          common_regions = consensus$regions,
          coverage = consensus$coverage,
          alignment_length = width(aligned_seqs)[1],
          timestamp = Sys.time()
        )
      })
    },

    #' @description Compare aligned sequences
    #' @param aligned_result Result from align_sequences()
    #' @return Matrix of pairwise differences
    compare_sequences = function(aligned_result) {
      private$error_boundary$catch({
        # Validate input
        if (!private$validate_alignment(aligned_result)) {
          return(NULL)
        }

        seqs <- aligned_result$sequences
        regions <- aligned_result$common_regions

        # Check overlap threshold
        if (sum(regions) < private$min_overlap) {
          private$log_warn(sprintf("Insufficient overlap: %d bp (min: %d)",
                                   sum(regions), private$min_overlap))
          return(NULL)
        }

        # Calculate pairwise differences
        n_seqs <- length(seqs)
        diffs <- matrix(0, n_seqs, n_seqs)

        for (i in 1:(n_seqs-1)) {
          for (j in (i+1):n_seqs) {
            diffs[i,j] <- diffs[j,i] <- private$compare_pair(
              seqs[[i]], seqs[[j]], regions
            )
          }
        }

        private$log_info(sprintf("Compared %d sequences", n_seqs))
        diffs
      })
    },

    #' @description Get current parameters
    #' @return List of parameters
    get_parameters = function() {
      list(
        min_overlap = private$min_overlap,
        match_score = private$match_score,
        mismatch_penalty = private$mismatch_penalty,
        gap_penalty = private$gap_penalty
      )
    }
  ),

  private = list(
    min_overlap = NULL,
    match_score = NULL,
    mismatch_penalty = NULL,
    gap_penalty = NULL,
    logger = NULL,
    error_boundary = NULL,

    validate_dependencies = function() {
      required_pkgs <- c("Biostrings", "DECIPHER")
      missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]

      if (length(missing_pkgs) > 0) {
        stop(sprintf("Missing required packages: %s. Install using BiocManager::install()",
                     paste(missing_pkgs, collapse = ", ")))
      }
    },

    validate_sequences = function(sequences) {
      messages <- character()

      if (length(sequences) == 0) {
        messages <- c(messages, "No sequences provided")
      } else {
        invalid_seq <- sapply(sequences, function(seq) {
          !grepl("^[ACGT-]+$", seq, ignore.case = TRUE)
        })

        if (any(invalid_seq)) {
          messages <- c(messages, "Invalid sequence characters detected")
        }
      }

      list(
        valid = length(messages) == 0,
        messages = messages
      )
    },

    validate_alignment = function(aligned_result) {
      if (is.null(aligned_result) ||
          !all(c("sequences", "common_regions", "coverage") %in% names(aligned_result))) {
        private$log_error("Invalid alignment result structure")
        return(FALSE)
      }
      TRUE
    },

    find_common_regions = function(aligned_seqs) {
      align_matrix <- Biostrings::as.matrix(aligned_seqs)
      valid_cols <- apply(align_matrix, 2, function(col) !any(col == "-"))

      total_cols <- ncol(align_matrix)
      covered_cols <- sum(valid_cols)

      list(
        regions = valid_cols,
        coverage = covered_cols / total_cols
      )
    },

    compare_pair = function(seq1, seq2, regions) {
      s1 <- as.character(seq1)[regions]
      s2 <- as.character(seq2)[regions]
      sum(s1 != s2, na.rm = TRUE)
    },

    # Logging helpers
    log_info = function(message) {
      if (!is.null(private$logger)) {
        private$logger$info(message)
      }
    },

    log_warn = function(message) {
      if (!is.null(private$logger)) {
        private$logger$warn(message)
      }
    },

    log_error = function(message, details = NULL) {
      if (!is.null(private$logger)) {
        private$logger$error(message, details)
      }
    }
  )
)
