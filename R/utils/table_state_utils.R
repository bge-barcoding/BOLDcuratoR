# R/utils/table_state_utils.R

library(jsonlite)

#' Initialize table state store
#' @param state_id Unique state identifier
#' @param initial_data Optional initial state data
#' @return State store object
init_table_state <- function(state_id, initial_data = NULL) {
  state <- list(
    id = state_id,
    flags = list(),
    notes = list(),
    selections = list(),
    metadata = list(
      version = 1,
      last_updated = Sys.time(),
      update_count = 0
    )
  )
  
  if (!is.null(initial_data)) {
    state <- update_table_state(state, initial_data)
  }
  
  state
}

#' Get current table state
#' @param state State store object
#' @param processid Optional process ID to get specific state
#' @return Current state or specific process state
get_table_state <- function(state, processid = NULL) {
  if (is.null(processid)) return(state)
  
  list(
    flags = state$flags[[processid]],
    notes = state$notes[[processid]],
    selected = state$selections[[processid]]
  )
}

#' Update table state
#' @param state State store object
#' @param new_data New state data
#' @return Updated state
update_table_state <- function(state, new_data) {
  if (is.null(state) || is.null(new_data)) return(state)
  
  # Validate and normalize data
  new_data <- validate_state_data(new_data)
  
  # Update relevant sections
  if (!is.null(new_data$flags)) {
    state$flags <- normalize_flag_data(new_data$flags)
  }
  if (!is.null(new_data$notes)) {
    state$notes <- normalize_note_data(new_data$notes)
  }
  if (!is.null(new_data$selections)) {
    state$selections <- as.list(new_data$selections)
  }
  
  # Update metadata
  state$metadata$last_updated <- Sys.time()
  state$metadata$update_count <- state$metadata$update_count + 1
  
  state
}

#' Sync state between tables
#' @param data Table data frame
#' @param current_state Current state list
#' @return Data with synced states
sync_table_states <- function(data, current_state) {
  if (is.null(data) || nrow(data) == 0) return(data)
  
  # Ensure required columns exist
  if (!"flag" %in% names(data)) data$flag <- NA_character_
  if (!"curator_notes" %in% names(data)) data$curator_notes <- NA_character_
  if (!"selected" %in% names(data)) data$selected <- FALSE
  
  # Create lookup for efficiency
  row_indices <- setNames(seq_len(nrow(data)), data$processid)
  
  # Sync flags
  if (!is.null(current_state$flags)) {
    flags <- normalize_flag_data(current_state$flags)
    for (pid in names(flags)) {
      idx <- row_indices[pid]
      if (!is.na(idx)) {
        data$flag[idx] <- flags[[pid]]$value
      }
    }
  }
  
  # Sync notes
  if (!is.null(current_state$notes)) {
    notes <- normalize_note_data(current_state$notes)
    for (pid in names(notes)) {
      idx <- row_indices[pid]
      if (!is.na(idx)) {
        data$curator_notes[idx] <- notes[[pid]]$text
      }
    }
  }
  
  # Sync selections
  if (!is.null(current_state$selections)) {
    for (pid in names(current_state$selections)) {
      idx <- row_indices[pid]
      if (!is.na(idx)) {
        data$selected[idx] <- TRUE
      }
    }
  }
  
  data
}

#' Handle flag change event
#' @param state Current state
#' @param flag_data Flag event data
#' @return Updated state
handle_flag_change <- function(state, flag_data) {
  if (is.null(flag_data$processid)) return(state)
  
  if (!is.null(flag_data$flag) && nchar(flag_data$flag) > 0) {
    state$flags[[flag_data$processid]] <- list(
      value = flag_data$flag,
      timestamp = Sys.time(),
      metadata = flag_data[setdiff(names(flag_data), c("processid", "flag"))]
    )
  } else {
    state$flags[[flag_data$processid]] <- NULL
  }
  
  state$metadata$last_updated <- Sys.time()
  state$metadata$update_count <- state$metadata$update_count + 1
  
  state
}

#' Handle note change event
#' @param state Current state
#' @param note_data Note event data
#' @return Updated state
handle_note_change <- function(state, note_data) {
  if (is.null(note_data$processid)) return(state)
  
  if (!is.null(note_data$notes) && nchar(note_data$notes) > 0) {
    state$notes[[note_data$processid]] <- list(
      text = note_data$notes,
      timestamp = Sys.time(),
      metadata = note_data[setdiff(names(note_data), c("processid", "notes"))]
    )
  } else {
    state$notes[[note_data$processid]] <- NULL
  }
  
  state$metadata$last_updated <- Sys.time()
  state$metadata$update_count <- state$metadata$update_count + 1
  
  state
}

#' Get table callback JavaScript
#' @param ns Namespace function
#' @param state_data Current state data
#' @return JavaScript callback code
get_table_callback <- function(ns, state_data = NULL) {
  if (is.null(ns)) return(NULL)
  
  # Convert state data to JSON
  state_json <- if (!is.null(state_data)) {
    jsonlite::toJSON(state_data, auto_unbox = TRUE)
  } else {
    "{}"
  }
  
  JS(sprintf("
    function(table) {
      // Initialize state management
      const stateManager = {
        state: %s,
        
        getState: function(processid) {
          return processid ? this.state[processid] : this.state;
        },
        
        setState: function(processid, type, value) {
          if (!this.state[processid]) this.state[processid] = {};
          this.state[processid][type] = value;
          this.state[processid].timestamp = Date.now();
          this.notifyStateChange(processid, type, value);
        },
        
        notifyStateChange: function(processid, type, value) {
          const rowData = table.row(`[data-processid='${processid}']`).data();
          if (!rowData) return;
          
          const payload = {
            processid: processid,
            [type]: value,
            species: rowData.species || '',
            timestamp: Date.now()
          };
          
          if (type === 'flag') {
            Shiny.setInputValue('%s', payload, {priority: 'event'});
          } else if (type === 'notes') {
            Shiny.setInputValue('%s', payload, {priority: 'event'});
          }
          
          // Dispatch event for cross-table sync
          const event = new CustomEvent('tableStateChange', {
            detail: { processid, type, value, tableId: table.table().node().id }
          });
          document.dispatchEvent(event);
        },
        
        restoreState: function() {
          table.rows().every((idx) => {
            const row = table.row(idx);
            const data = row.data();
            if (!data?.processid) return;
            
            const state = this.getState(data.processid);
            if (state) {
              const $row = $(row.node());
              if (state.flag) {
                $row.find('.specimen-flag').val(state.flag);
              }
              if (state.notes) {
                $row.find('.specimen-notes').val(state.notes);
              }
            }
          });
        }
      };
      
      // Event handlers
      table.on('change', '.specimen-flag', function() {
        const row = table.row($(this).closest('tr'));
        const data = row.data();
        if (!data?.processid) return;
        
        stateManager.setState(data.processid, 'flag', this.value);
      });
      
      table.on('change', '.specimen-notes', function() {
        const row = table.row($(this).closest('tr'));
        const data = row.data();
        if (!data?.processid) return;
        
        stateManager.setState(data.processid, 'notes', this.value);
      });
      
      // State change listener for cross-table sync
      document.addEventListener('tableStateChange', (e) => {
        const { processid, type, value, tableId } = e.detail;
        if (tableId === table.table().node().id) return;
        
        table.rows().every((idx) => {
          const row = table.row(idx);
          const data = row.data();
          if (data?.processid === processid) {
            const $row = $(row.node());
            if (type === 'flag') {
              $row.find('.specimen-flag').val(value);
            } else if (type === 'notes') {
              $row.find('.specimen-notes').val(value);
            }
          }
        });
      });
      
      // Restore state on draw
      table.on('draw', () => {
        setTimeout(() => stateManager.restoreState(), 100);
      });
      
      // Initial state restoration
      stateManager.restoreState();
    }
  ", state_json, ns('specimen_flag'), ns('specimen_notes')))
}

#' Normalize flag data
#' @param flag_data Raw flag data
#' @return Normalized flag list
normalize_flag_data <- function(flag_data) {
  if (is.null(flag_data)) return(list())
  if (!is.list(flag_data)) return(list())
  
  lapply(flag_data, function(flag) {
    if (is.character(flag)) {
      list(value = flag, timestamp = Sys.time())
    } else if (is.list(flag)) {
      if (is.null(flag$value)) flag$value <- flag$flag
      if (is.null(flag$timestamp)) flag$timestamp <- Sys.time()
      flag
    } else {
      NULL
    }
  })
}

#' Normalize note data
#' @param note_data Raw note data
#' @return Normalized note list
normalize_note_data <- function(note_data) {
  if (is.null(note_data)) return(list())
  if (!is.list(note_data)) return(list())
  
  lapply(note_data, function(note) {
    if (is.character(note)) {
      list(text = note, timestamp = Sys.time())
    } else if (is.list(note)) {
      if (is.null(note$text)) note$text <- note$note
      if (is.null(note$timestamp)) note$timestamp <- Sys.time()
      note
    } else {
      NULL
    }
  })
}

#' Validate state data
#' @param data State data to validate
#' @return Validated and cleaned state data
validate_state_data <- function(data) {
  if (is.null(data)) return(NULL)
  if (!is.list(data)) return(NULL)
  
  # Clean and validate each component
  list(
    flags = if (!is.null(data$flags)) normalize_flag_data(data$flags) else NULL,
    notes = if (!is.null(data$notes)) normalize_note_data(data$notes) else NULL,
    selections = if (!is.null(data$selections)) {
      if (is.list(data$selections)) data$selections else list()
    } else NULL
  )
}

#' Save table state to file
#' @param state State object to save
#' @param file File path
#' @return Logical indicating success
save_table_state <- function(state, file) {
  if (is.null(state) || is.null(file)) return(FALSE)
  
  tryCatch({
    saveRDS(state, file)
    TRUE
  }, error = function(e) FALSE)
}

#' Load table state from file
#' @param file File path
#' @return Loaded state or NULL
load_table_state <- function(file) {
  if (is.null(file) || !file.exists(file)) return(NULL)
  
  tryCatch({
    state <- readRDS(file)
    if (verify_state_integrity(state)) state else NULL
  }, error = function(e) NULL)
}

#' Verify state data integrity
#' @param state State object to verify
#' @return Logical indicating valid state
verify_state_integrity <- function(state) {
  if (is.null(state) || !is.list(state)) return(FALSE)
  if (is.null(state$id)) return(FALSE)
  
  required_fields <- c("flags", "notes", "selections", "metadata")
  all(required_fields %in% names(state))
}

#' Clean state data
#' @param state State to clean
#' @return Cleaned state object
clean_state_data <- function(state) {
  if (!verify_state_integrity(state)) return(init_table_state("cleaned"))
  
  # Remove NULL or invalid entries
  state$flags <- Filter(Negate(is.null), normalize_flag_data(state$flags))
  state$notes <- Filter(Negate(is.null), normalize_note_data(state$notes))
  state$selections <- Filter(Negate(is.null), state$selections)
  
  # Update metadata
  state$metadata$last_updated <- Sys.time()
  
  state
}