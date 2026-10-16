## server.R ##

server <- function(input, output, session) {
	
	###############################################
	# BOOKMARKING URL #############################
	###############################################
  	observe({
    	urlInput <- sub("#", "", session$clientData$url_hash)
    	if(!is.null(urlInput)){
    		if(urlInput %in% c("selection", "isafe", "functional", "age", "report", "download", "region_download", "batch_download", "uploadReigon", "tutorial", "Information")){
    			updateTabItems(session, "navMenu", selected = urlInput)
    		}
    		if(grepl("^chr(\\d{1,2})\\:(\\d+)\\-(\\d+)$",urlInput)){
    			coord <- gsub("chr","",strsplit(urlInput, "[:-]")[[1]])
    			# Update entries
    			updateSelectInput(session, 'chrInput', selected = coord[1])
    			updateTextInput(session, 'minPosInput', value = as.character(coord[2]))
    			updateTextInput(session, 'maxPosInput', value = as.character(coord[3]))
    			click('UpdateButt')
    		}
    		if(grepl("#pop=",urlInput)){
    			# Get info
    			coordWpops <- gsub("chr|pop|#","",strsplit(urlInput, "=")[[1]])
    			urlCoord <- strsplit(coordWpops[1], "[:-]")[[1]]
    			urlPops <- strsplit(coordWpops[2], ";")[[1]]
    		
	    		# Update entries
	    		updateSelectInput(session, 'chrInput', selected = urlCoord[1])
	    		updateTextInput(session, 'minPosInput', value = as.character(urlCoord[2]))
	    		updateTextInput(session, 'maxPosInput', value = as.character(urlCoord[3]))
	    		updateCheckboxGroupButtons(session, 'popsInput', selected=urlPops)
	    		# Click Button to show
	    		click('UpdateButt')
	    	}
    	}
  	})


	###############################################
	# MODAL DIALOGUES #############################
	###############################################

	# First Pop Up ###############################
	observeEvent("", {
		showModal(modalDialog(
			includeHTML("intro_text.html"),
			easyClose = TRUE,
			footer = tagList(
				actionButton(inputId = "modalButton", 
					label = "UNDERSTOOD",
					icon = icon("thumbs-up"),
					color = "success")
				)
			))
		})


	observeEvent(input$modalButton,{
		removeModal()
		})

	# Cite Us - Pop Up #################
	observeEvent(input$openCiteUs, {
		showModal(
			modalDialog(
				includeHTML("citeUs_text.html"),
				easyClose = TRUE,
				footer=NULL
				)
			)
		})
	
	# Function that Return the UI for a modal dialog with ID selection input. 
	# If 'failed' is TRUE, then display a message that the previous value was invalid.
	dataModal <- function(failed = FALSE) {
		modalDialog(
			h2("Quick search by ID"),
			span("This action will only update the chromosome and the coordinates while keeping the rest of filters as they were"),
			br(),
			br(),
			textInput("rsidSearch",
				"Enter an ID:",
				placeholder = "Variant ID, Gene symbol or Ensembl ID",
				width = "90%"
				),
			div(HTML("Examples: rs3827760, <em>EDAR</em>, ENSG00000139618...")),
			br(),
			sliderInput(
				'contextSpace',
				'Flanking region on each side:',
				min   = 0,
				max   = 10,
				step  = 0.5,
				value = 5,
				post  = " kb",
				width = "80%"
				),
			span("If a variant ID provided, this value must be > 0kb"),
			br(),
			if (failed)
			div(tags$b("(!) INVALID ID SEARCH", style = "color: red;")),
			easyClose = FALSE,
			size      = "m",
			fade      = TRUE,
			footer    = tagList(
				modalButton("Cancel"),
				actionButton("search", "Search")
				)
			)
	}
	
	# Show modal when button is clicked 
	observeEvent(input$SearchID, {
		showModal(dataModal())
		})
	
	# Function to retrieve chr and coordinate
	# When OK button is pressed, attempt to load the data set. If successful,
	# remove the modal. If not show another modal, but this time with a failure
	# message.
	observeEvent(input$search, {
		if (!is.null(input$rsidSearch) && nzchar(input$rsidSearch)) {
			# Run function to get coordinates
			idcoords <- getGenomicCoordinates(input$rsidSearch)

			# Check that ID was found
			if(nrow(idcoords)>0) {
				# Consider the flanking region
				flanking <- ifelse(input$contextSpace==0, 0,(input$contextSpace*1000))

				# UPDATE DATA
				updateSelectInput(session, "chrInput", selected = idcoords$chr[1])

				# UPDATE coords
				updateTextInput(session, "minPosInput",
					label = 'Start Position',
					value = idcoords$start[1]-flanking
					)
				updateTextInput(session, "maxPosInput",
					label = 'End Position',
					value = idcoords$end[1]+flanking
					)

			# Remove modal and click update button
			click("UpdateButt")     
			removeModal()
			}else{showModal(dataModal(failed = TRUE))}
			}else{
				showModal(dataModal(failed = TRUE))
			}
			})
	

	###############################################
	# FILTERS AND REACTIVE VALUES (btns etc) ######
	###############################################
	
	### Coordinates 
	# Value with current chr
	chrSelect <- reactive({
		input$UpdateButt
		isolate({
			input$chrInput
			})
		}) 

	# Vector with minmax input
	regionSelect <- reactiveValues(min=109500927, max=109615828)
	
	# Sweet Alert
	observeEvent(input$UpdateButt,{
		coordS <- as.numeric(input$minPosInput)
		coordE <- as.numeric(input$maxPosInput)
		if(!is.na(coordS)&!is.na(coordE)){
			if(coordE-coordS < 2000000){
				regionSelect$min <- coordS
				regionSelect$max <- coordE
				} else {
					sendSweetAlert(
						session = session,
						title = "Error!",
						closeOnClickOutside = FALSE,
						text = tags$span(
							tags$h2("Coordinates must be within a 2Mb range!"),tags$br(),"We have automatically modified your input to match the maximum length allowed."),
						html = TRUE,
						type = "error"
						)
					updateTextInput(session, "maxPosInput", label = 'End Position', value = coordS + 2*10^6)
				}
			}
			})
	observeEvent(input$UpdateButt,{
	    # Check limits and change number
	    if(is.na(as.numeric(input$minPosInput))){
	    	updateTextInput(session, "minPosInput", label = 'Start Position', value = as.character(regionSelect$min))
	    	sendSweetAlert(
	    		session = session,
	    		title = "Warning!",
	    		closeOnClickOutside = FALSE,
	    		text = tags$span(
	    			tags$h2("Coordinates must be numeric"),tags$br(),
	    			"Check the input on the Start Position, then press the",tags$b("UPDATE"), "button again."),
	    		html = TRUE,
	    		type = "warning"
	    		)
	    }
	    })
	observeEvent(input$UpdateButt,{
	    # Check limits and change number
	    if(is.na(as.numeric(input$maxPosInput))){
	    	updateTextInput(session, "maxPosInput", label = 'Start Position', value = as.character(regionSelect$max))
	    	sendSweetAlert(
	    		session = session,
	    		title = "Warning!",
	    		closeOnClickOutside = FALSE,
	    		text = tags$span(
	    			tags$h2("Coordinates must be numeric"),tags$br(),
	    			"Check the input on the End Position, then press the",tags$b("UPDATE"), "button again."),
	    		html = TRUE,
	    		type = "warning"
	    		)
	    }
	    })
	  #Observation to control max lenght of the input regions
	# Zoom buttons
	observeEvent(input$zoomCoordenates,{

		# Get absolute min max for the present chromosome
		ABSminNmax <- getSliderValues(chrSelect(),NULL) %>% as.data.frame  

		# Compute increment of coordinates input (100k change)
		newStart <- as.numeric(input$minPosInput)+(as.numeric(input$zoomCoordenates)*100000)
		newEnd <- as.numeric(input$maxPosInput)-(as.numeric(input$zoomCoordenates)*100000)

		# If chromosome end reached, keep last position with data
		if(newStart < ABSminNmax$minV){newStart<-ABSminNmax$minV}
		if(newEnd > ABSminNmax$maxV){newEnd <-ABSminNmax$maxV}
		
		# Check limits and change number
		if(newStart < newEnd){
			if(newEnd-newStart< 2000000){
				updateTextInput(session, "minPosInput", label = 'Start Position', value = newStart)
				updateTextInput(session, "maxPosInput", label = 'End Position', value = newEnd)
				} else {
					sendSweetAlert(
						session = session,
						title = "Error!",
						closeOnClickOutside = FALSE,
						text = tags$span(
							tags$h2("Coordinates must be within a 2Mb range!"),tags$br(),"We have automatically modified your input to match the maximum length allowed."),
						html = TRUE,
						type = "error"
						)
					updateTextInput(session, "maxPosInput", label = 'End Position', value = newStart + 2*10^6)
				}
				} else {
					sendSweetAlert(
						session = session,
						title = "Warning!",
						closeOnClickOutside = FALSE,
						text = tags$span(
							tags$h2("You can not ZOOM IN anymore."),tags$br()," Since the zoom is 100kb at a time, next increment would invert the ",tags$b("START"),"and",tags$b("END"), "coordinates.",tags$br(),"If you would like a smaller increment, please, update it manually."),
						html = TRUE,
						type = "warning"
						)
				}
			# After action, unclick botton
			updateRadioGroupButtons(session, "zoomCoordenates", selected=character(0))

			})
	output$wholeRegion <- renderText({paste0(chrSelect(),":",regionSelect$min,"-", regionSelect$max)})

	### Metapopts-Pops link
	observe({
		if(length(input$metapopsInput)!=0){
			popstoSelect <- c()
			for(selMetapop in input$metapopsInput){
				popstoSelect <- c(popstoSelect,get(paste0(selMetapop, "pops")))
			}
			updateCheckboxGroupButtons(session, 'popsInput', selected = popstoSelect)
		}
		})
	
	### Populations
	# Vector with selected populations
	popSelect <- reactive({
		input$UpdateButt
		isolate({
			input$popsInput %>% as.vector()
			})
		})
	observe({
		if (input$selectAllPopsInput > 0){
			if (input$selectAllPopsInput %% 2 == 0){
				updateCheckboxGroupButtons(session, 'popsInput', selected = popsData$pops)
				updateCheckboxGroupButtons(session, 'metapopsInput', selected = metapopsData$metapops)
				updateActionButton(session, inputId="selectAllPopsInput", label = "Deselect All")
			}
			else {
				updateCheckboxGroupButtons(session, 'popsInput', selected = '')
				updateCheckboxGroupButtons(session, 'metapopsInput', selected = '')
				updateActionButton(session, inputId="selectAllPopsInput", label = "Select All")
			}
		}
		})
	
	### VEP
	observe({
		if (input$selectAllVEPInput > 0){
			if (input$selectAllVEPInput %% 2 == 0){
				updateCheckboxGroupInput(session, 'VEPInput', selected = '')
				updateActionButton(session, inputId="selectAllVEPInput", label = "Select All")
			}
			else {
				updateCheckboxGroupInput(session, 'VEPInput', selected = VEPinfo$outLabels)
				updateActionButton(session, inputId="selectAllVEPInput", label = "Deselect All")
			}
		}
		})
	
	### Clinvar
	observe({
		if (input$selectAllClinVarInput > 0){
			if (input$selectAllClinVarInput %% 2 == 0){
				updateCheckboxGroupInput(session, 'ClinVarInput', selected = '')
				updateActionButton(session, inputId="selectAllClinVarInput", label = "Select All")
			}
			else {
				updateCheckboxGroupInput(session, 'ClinVarInput', selected = ClinVarLabels$ClinicalSign)
				updateActionButton(session, inputId="selectAllClinVarInput", label = "Deselect All")
			}
		}
		})
	
	# When click Update button, update report button as well
	observeEvent(input$UpdateButt, {
		click("ButtonNum2")
		})
	
	# Change max and mins (not SELECTION!) from filters according to chr infoData
	observeEvent(input$UpdateButt,{

		# Get absolute min max for this chromosome
		minNmax <- getSliderValues(chrSelect(),input$AgeModelInput) %>% as.data.frame  

		# Update widgets according to this
		updateSliderInput(session, "GWASInput", min = minNmax$minV[3], max = minNmax$maxV[3])
		updateSliderInput(session, "NofPmidsInput", min = minNmax$minV[2],max = minNmax$maxV[2])
		updateSliderInput(session, "AgeModeInput", min = minNmax$minV[1], max = minNmax$maxV[1])
		})

	### Overview
	# Same but for the right side of the report plot
	observeEvent(input$ButtonNum2,{

		minNmax <- getSliderValues(chrSelect(), overviewSelect$Clock) %>% as.data.frame  
		updateSliderInput(session, "Overview_AgeMode",
			min = minNmax$minV[1],max = minNmax$maxV[1])
	})

	# Get Overview info all together
	overviewSelect <- reactiveValues(
		Pop='CHS', 
		Clock='Jnt', 
		AgeMode=c(55.444,2000), 
		qualScore=c(0.75, 1)
		)

	# Update Overview info when refress
	observeEvent(input$ButtonNum2,{
		overviewSelect$Pop <- input$Overview_Pop
		overviewSelect$Clock <- input$Overview_Clock
		overviewSelect$AgeMode <- input$Overview_AgeMode
		overviewSelect$qualScore <- input$Overview_qualScore
	})

	### Download
	# Update headers in download options
	observeEvent(input$UpdateButt, {
		updatePickerInput(session,"headersDownload",choices = set_download()[[2]])
		})

	# Select/Desselect button download tab
	observe({
		if (input$selectAllheaders > 0){
			if (input$selectAllheaders %% 2 == 0){
				updatePickerInput(session,"headersDownload", selected='')
				updateActionButton(session, inputId="selectAllheaders", label = "Select All")
			}
			else {
				updatePickerInput(session,"headersDownload", selected=set_download()[[2]])
				updateActionButton(session, inputId="selectAllheaders", label = "Deselect All")
			}
		}
		})
	

	###############################################
	# REACTIVE DATASETS ###########################
	###############################################

	# info SELECTION TAB
	set_selection <- reactive({
		input$UpdateButt
		isolate({

			raw_ihs = sqlToDf(chrSelect(),regionSelect$min, regionSelect$max,'ihs') %>% dplyr::select(physicalPos, rsid, starts_with(popSelect())) %>% filter_at(vars(-c("physicalPos","rsid")), any_vars(!is.na(.))) %>% as.data.table

			raw_nsl = sqlToDf(chrSelect(),regionSelect$min,regionSelect$max,'nsl') %>% dplyr::select(physicalPos, rsid, starts_with(popSelect())) %>% 
			filter_at(vars(-c("physicalPos","rsid")), any_vars(!is.na(.))) %>% 
			as.data.table

			raw_isafe = sqlToDf(chrSelect(),regionSelect$min,regionSelect$max,'isafe') %>% dplyr::select(physicalPos,rsid,starts_with(popSelect())) %>%  dplyr::filter_at(vars(-c("physicalPos","rsid")), any_vars(!is.na(.))) %>% as.data.table

			list(raw_ihs,raw_nsl,raw_isafe)
			})
		})

	# Info pvalues 
	set_pvalues <- reactive({

		# iHS all & subset
		all_ihs_pval = sqlToDf(chrSelect(),regionSelect$min,regionSelect$max,'ihs_pval') %>% dplyr::filter_at(vars(-c("physicalPos","rsid")), any_vars(!is.na(.))) %>% as.data.table
		raw_ihs_pval = all_ihs_pval %>% dplyr::select(physicalPos,rsid,starts_with(popSelect())) %>%  dplyr::filter_at(vars(-c("physicalPos","rsid")), any_vars(!is.na(.))) %>% as.data.table

		# nSL all & subset
		all_nsl_pval= sqlToDf(chrSelect(),regionSelect$min,regionSelect$max,'nsl_pval') %>% dplyr::filter_at(vars(-c("physicalPos","rsid")), any_vars(!is.na(.))) %>% as.data.table
		raw_nsl_pval = all_nsl_pval %>% dplyr::select(physicalPos,rsid,starts_with(popSelect())) %>%  dplyr::filter_at(vars(-c("physicalPos","rsid")), any_vars(!is.na(.))) %>% as.data.table

		# iSAFE all & subset
		all_isafe_pval= sqlToDf(chrSelect(),regionSelect$min,regionSelect$max,'isafe_pval') %>%  dplyr::filter_at(vars(-c("physicalPos","rsid")), any_vars(!is.na(.))) %>% as.data.table
		raw_isafe_pval = all_isafe_pval %>% dplyr::select(physicalPos,rsid,starts_with(popSelect())) %>%  dplyr::filter_at(vars(-c("physicalPos","rsid")), any_vars(!is.na(.))) %>% as.data.table

		list(raw_ihs_pval,raw_nsl_pval,raw_isafe_pval, all_ihs_pval, all_nsl_pval, all_isafe_pval)
		})

	set_filter_ihs <- reactive({
		input$UpdateButt
		isolate({
			
			# Pvalue
			pval = input$pval_ihs

			# Raw values
			selection_melt_ihs <- meltDFtoPlot(set_selection()[[1]], 'ihs') %>% mutate(AD = ifelse(value > 0, "Derived", "Ancestral"))

			# Pvalues
			pvalues_melt_ihs =  meltDFtoPlot(set_pvalues()[[1]], 'ihs')

			# Filter accordingly
			selection_melt_filtered_ihs = newFilterThreshold(df_stat_melted=selection_melt_ihs,df_pval_melted=pvalues_melt_ihs,pval=pval, extreme=T)

			# Output
			list(selection_melt_ihs,selection_melt_filtered_ihs)

			})
		})

	set_filter_nsl <- reactive({
		input$UpdateButt
		isolate({
			# Pvalue
			pval = input$pval_nsl

			# Raw values
			selection_melt_nsl <- meltDFtoPlot(set_selection()[[2]], 'nsl') %>% mutate(AD = ifelse(value > 0, "Derived", "Ancestral"))

			# Pvalues
			pvalues_melt_nsl =  meltDFtoPlot(set_pvalues()[[2]], 'nsl')

			# Filter accordingly
			selection_melt_filtered_nsl = newFilterThreshold(df_stat_melted=selection_melt_nsl,df_pval_melted=pvalues_melt_nsl,pval=pval, extreme=T)

			# Output
			list(selection_melt_nsl,selection_melt_filtered_nsl)

			})
		})

	set_selection_to_table <- reactive({
		input$UpdateButt
		isolate({

			render_ihs = set_filter_ihs()[[2]]
			render_nsl = set_filter_nsl()[[2]]

			if(input$extremeValuesInput_ihs){
				render_ihs = render_ihs[thres_ad!='Not extreme']
			}

			if(input$extremeValuesInput_nsl){
				render_nsl = render_nsl[thres_ad!='Not extreme']
			}

			#Rendering table
			if(nrow(render_ihs) < 1 & nrow(render_nsl) > 1){
				dcast_nsl = dcast(render_nsl,physicalPos + rsid ~ POP)
				#Suffix manual				
				names(dcast_nsl)[3:ncol(dcast_nsl)] = paste0(names(dcast_nsl)[3:ncol(dcast_nsl)] , '_nSL')
				render_table = dcast_nsl
			}
			else if(nrow(render_ihs) > 1 & nrow(render_nsl) < 1 ){
				dcast_ihs = dcast(render_ihs,physicalPos + rsid ~ POP)
				
				#Suffix manual
				names(dcast_ihs)[3:ncol(dcast_ihs)] = paste0(names(dcast_ihs)[3:ncol(dcast_ihs)] , '_iHS')
				render_table = dcast_ihs
			}
			else if(nrow(render_ihs) < 1 & nrow(render_nsl) < 1){
				# render_table = set_selection()[0]
				render_table   = data.table('physicalPos'=as.numeric(),'rsid'=as.character())
			}
			else{
				dcast_ihs    = dcast(render_ihs,physicalPos + rsid ~ POP)
				dcast_nsl    = dcast(render_nsl,physicalPos + rsid ~ POP)

				#Suffix manual
				names(dcast_nsl)[3:ncol(dcast_nsl)] = paste0(names(dcast_nsl)[3:ncol(dcast_nsl)] , '_nSL')
				names(dcast_ihs)[3:ncol(dcast_ihs)] = paste0(names(dcast_ihs)[3:ncol(dcast_ihs)] , '_iHS')
				render_table = merge(dcast_ihs,dcast_nsl,all=T)
			}
			names(render_table)[1] = 'Position'
			render_table		
			})
		})
	
	# info FAV MUTATION TAB
	set_favallel <- reactive({
		input$UpdateButt
		isolate({

			# Pvalue
			pval = input$pval_isafe

			# Raw values
			selection_melt_isafe <- meltDFtoPlot(set_selection()[[3]], 'isafe')
			selection_melt_isafe$AD = paste0('Top ',pval*100,'%')

			# Pvalues
			pvalues_melt_isafe =  meltDFtoPlot(set_pvalues()[[3]], 'isafe')

			# Filter accordingly
			isafe_melt_filtered = newFilterThreshold(df_stat_melted=selection_melt_isafe,df_pval_melted=pvalues_melt_isafe,pval=pval, extreme=T)


			if(input$extremeISAFE){
				isafe_melt_filtered = isafe_melt_filtered[thres_ad != 'Not extreme']
				if(nrow(isafe_melt_filtered)!= 0){
					render_table = dcast(isafe_melt_filtered, physicalPos + rsid ~ POP)
					} else{
						render_table= data.table('Position'=numeric(), 'rsid'=character())
					}
					} else {
						render_table = set_selection()[[3]]
					}

					names(render_table)[1] = 'Position'

					df_popsWithData <- isafe_melt_filtered %>% dplyr::filter(value >= 0.05) %>% as.data.table	
					popsWithData = unique(df_popsWithData$POP)


					list(selection_melt_isafe, 
						isafe_melt_filtered, 
						render_table, 
						popsWithData)
					})
		})

	# info FUNCT TAB
	set_functional <- reactive({
		input$UpdateButt
		isolate({
			
			raw_functional = sqlToDf(chrSelect(),regionSelect$min,regionSelect$max,'functional')

			raw_functional$NumGWASStudies <- NA
			raw_functional$NumDGNAsso <- NA

			# Add GWAS_count and Disease_count
			if(sum(!is.na(raw_functional$accession))>0){
				raw_functional$NumGWASStudies[!is.na(raw_functional$accession)] <- do.call(rbind, lapply(strsplit(raw_functional$accession[!is.na(raw_functional$accession)],";"), function(x) length(x)))	
			}
			if(sum(!is.na(raw_functional$diseaseId))>0){
				raw_functional$NumDGNAsso[!is.na(raw_functional$diseaseId)] <- do.call(rbind, lapply(strsplit(raw_functional$diseaseId[!is.na(raw_functional$diseaseId)],";"), function(x) length(x)))
			}
			
			# data() %>% dplyr::select(physicalPos, rsid, gene_effect, effect, impact, TFbinding,DNasePeak, motif, DNaseFootprint, eQTL, matchedTFmotif, matchedDNaseFootprint, ranking,risk_allele, risk_allele_freq, trait, accession, NumGWASStudies, context, diseaseName, diseaseId, NumDGNAsso, diseaseType, source, DSI, DPI, EI, NofPmids, ClinicalSignificance, ClinSigSimple,PhenotypeList) %>% filter_at(vars(-c("physicalPos","rsid")), any_vars(!is.na(.))) %>% as.data.table
			raw_functional %>% filter_at(vars(-c("physicalPos","rsid")), any_vars(!is.na(.))) %>% as.data.table
			})
		})

	set_snpeff <- reactive({
		input$UpdateButt
		isolate({

			filtered  = set_functional() %>%
			dplyr::select(physicalPos,rsid,gene_effect, effect, impact) %>% na.omit %>% unique

			VEPmask1   = VEPinfo$outLabels %in% input$VEPInput

			toPlot     = SplitDf(filtered,c1=c("gene_effect","impact","effect"),c2=c("effect"),s1=",",s2="&")

			toPlot     = toPlot[effect %in% VEPinfo$inLabels[VEPmask1]] 

			toPlot$rank = 28
			for(i in unique(na.omit(toPlot$effect))){
				toPlot[effect == i]$rank = VEPinfo[inLabels == i]$rank
			}

			# toPlot = toPlot[impact!='MODIFIER']
			toPlot = toPlot[toPlot[,.I[which.min(rank)], by=physicalPos]$V1]
			toPlot = toPlot[,1:5]

			VEPmask2 = VEPinfo$inLabels %in% as.character(toPlot$effect %>% unique)

			# Mask to filter
			# #The default order will be alphabetized unless specified as below:
			toPlot$effect <- factor(toPlot$effect,levels=VEPinfo$inLabels[VEPmask2])
			toPlot$Labs = gsub("_"," ",toPlot$effect)
			toPlot$Labs = str_to_title(toPlot$Labs)

			list(toPlot,VEPmask2)
			})
		})

	# info GEVA TAB
	set_age <- reactive({
		input$UpdateButt
		isolate({
			tmp = sqlToDf(chrSelect(),regionSelect$min,regionSelect$max,'geva') 

			toFilter <- tmp %>% dplyr::select(physicalPos, rsid, AlleleRef, AlleleAlt, AlleleAnc, ends_with(input$AgeModelInput))
			names(toFilter)<- gsub(paste0('_',input$AgeModelInput),'', names(toFilter))
			toPlot <- toFilter %>% filter_at(vars(starts_with('Age')), any_vars(!is.na(.))) %>% as.data.table

			toPlot <- toPlot[AgeMode >= input$AgeModeInput[1] & AgeMode <= input$AgeModeInput[2] & QualScore >= input$qualScoreInput[1] &  QualScore <= input$qualScoreInput[2]] 
			
			toPlot
			})
		})
	
	# info FINAL REPORT
	set_report <- reactive({
		input$ButtonNum2
		isolate({

			# Generate data
			filtered <- mergeAllDF(chrSelect(),regionSelect$min,regionSelect$max) %>% dplyr::select(physicalPos, rsid, 
				starts_with(overviewSelect$Pop), effect, impact,'RDBranking'=ranking, 
				ClinicalSignificance, DSI, EI, NumGWASStudies, 
				paste0(c('AgeMode_','QualScore_','AgeCI95Lower_', 'AgeCI95Upper_'), overviewSelect$Clock))


			# Change columns to ease operate with the)
			names(filtered) = gsub(paste0(overviewSelect$Pop,'_'), "", names(filtered))
			names(filtered) = gsub(paste0("_", overviewSelect$Clock),"", names(filtered))


			# Filter according the side gadgets
			filtered = filtered[AgeMode > overviewSelect$AgeMode[1] & AgeMode < overviewSelect$AgeMode[2]]
			filtered = filtered[QualScore > overviewSelect$qualScore[1] & QualScore < overviewSelect$qualScore[2]]
			
			# Modify dataset to obtain Plot
			expandDF <- SplitDf(filtered, c1=c("effect","impact"), c2=c("effect"),s1=",",s2="&")
			expandDF$rank <- 0
			for(i in unique(na.omit(expandDF$effect))){
				expandDF[effect == i]$rank = VEPinfo[inLabels == i]$rank
			}

			expandDF$rank[expandDF$rank==0]<-NA	
			toPlot <- expandDF[expandDF[,.I[which.min(rank)], by=list(physicalPos, rsid)]$V1]

			toPlot$isafe[is.na(toPlot$isafe)]<-0
			toPlot$ihs[is.na(toPlot$ihs)]<-0
			toPlot$nsl[is.na(toPlot$nsl)]<-0

			toPlot
			})
		})

	# Subset prioretized regions
	set_infoBoxes<-reactive({
		input$ButtonNum2
		isolate({
			# Sort and change columns names
			# Generate data
			filtered <- mergeAllDF(chrSelect(),regionSelect$min,regionSelect$max) %>% 
			dplyr::select(physicalPos, rsid, starts_with(overviewSelect$Pop), NumGWASStudies, effect, impact,'RDBranking'=ranking, ClinicalSignificance, diseaseId, NofPmids,paste0(c('AgeMode_','QualScore_','AgeCI95Lower_', 'AgeCI95Upper_'), overviewSelect$Clock))
			# Change columns to ease operate with them
			names(filtered) = gsub(paste0(overviewSelect$Pop,'_'), "", names(filtered))
			names(filtered) = gsub(paste0("_", overviewSelect$Clock),"", names(filtered))

			# Filter according the side gadgets
			filtered = filtered[AgeMode > overviewSelect$AgeMode[1] & AgeMode < overviewSelect$AgeMode[2]]
			filtered = filtered[QualScore > overviewSelect$qualScore[1] & QualScore < overviewSelect$qualScore[2]]
			
			# Modify dataset to obtain Plot
			expandDF <- SplitDf(filtered, c1=c("effect","impact"), c2=c("effect"),s1=",",s2="&")
			expandDF$rankSnpEFF <- 0
			for(i in unique(na.omit(expandDF$effect))){expandDF[effect == i]$rankSnpEFF = VEPinfo[inLabels == i]$rank}
			expandDF$rankSnpEFF[expandDF$rankSnpEFF==0]<-NA	
			toPlot <- expandDF[expandDF[,.I[which.min(rankSnpEFF)], by=list(physicalPos, rsid)]$V1]

			toPlot$isafe[is.na(toPlot$isafe)]<-0
			toPlot$ihs[is.na(toPlot$ihs)]<-0
			toPlot$nsl[is.na(toPlot$nsl)]<-0
			toPlot$ihsABS <- abs(toPlot$ihs)
			toPlot$nslABS <- abs(toPlot$nsl)
			toPlot$ClinVarRank <- match(toPlot$ClinicalSignificance, ClinVarLabels$ClinicalSign, nomatch=NA)

			toShow <- toPlot %>% dplyr::select(physicalPos, rsid, isafe, ihs, ihsABS, nsl, nslABS, NumGWASStudies, rankSnpEFF, effect, impact, RDBranking,	ClinicalSignificance, ClinVarRank, diseaseId, NofPmids)

			toShow
			})
		})

	# Subset prioretized regions
	set_prior<-reactive({
		input$ButtonNum2
		isolate({
			# Sort and change columns names
			toFilter <- set_report() %>% 
			dplyr::arrange(
				desc(isafe), 
				desc(abs(ihs)), 
				desc(abs(nsl)),
				desc(NumGWASStudies),
				rank,
				RDBranking) %>%
			dplyr::select(
				'Position'=physicalPos, 
				rsid, 
				'iSAFE'=isafe,
				'iHS'=ihs,
				'nSL'=nsl,
				'GWAS assoc.'=NumGWASStudies,
				'Variant effect'=effect,
				'Regulome Score'=RDBranking,
				'Generations'=AgeMode
				)
			
			# Subset top 20
			finalSubset <-toFilter[1:20,]
			finalSubset$'Variant effect' <- str_to_title(gsub("_"," ",finalSubset$'Variant effect'))

			finalSubset
			})
		})

	# Subset final Report
	set_download <- reactive({
		input$UpdateButt
		isolate({
			
			df_stat = set_selection_to_table()
			names(df_stat)[1] = 'physicalPos'

			df_isafe = set_favallel()[[3]]
			names(df_isafe)[1] = 'physicalPos'
			# manually forced to merge if non extreme values
			if(ncol(df_isafe) > 2){
				names(df_isafe)[3:length(names(df_isafe))] <- paste0(names(df_isafe)[3:length(names(df_isafe))], "_isafe")
				} else {
					NULL
				}


				df_snpeff = set_snpeff()[[1]]
				names(df_snpeff)[1] = 'physicalPos'

				df_geva = set_age()
				names(df_geva)[1] = 'physicalPos'

				df_func = set_functional() %>% dplyr::select(-c(effect, impact,gene_effect)) %>% 
				dplyr::filter((ranking >= input$RegulomeInput[1] & ranking <= input$RegulomeInput[2]) | is.na(ranking)) %>%
				dplyr::filter((ClinicalSignificance %in% input$ClinVarInput) | is.na(ClinicalSignificance)) %>%
				dplyr::filter((NumGWASStudies >= input$GWASInput[1] & ranking <= input$RegulomeInput[2]) | is.na(ranking)) %>%					
				dplyr::filter((NumGWASStudies > input$GWASInput[1] & NumGWASStudies < input$GWASInput[2]) | is.na(NumGWASStudies)) %>%
				dplyr::filter((DSI >= input$DSIInput[1] & DSI <= input$DSIInput[2]) | is.na(DSI)) %>% 
				dplyr::filter((EI >= input$EInput[1] & EI <= input$EInput[2]) | is.na(EI)) %>%
				dplyr::filter((NofPmids >= input$NofPmidsInput[1] & NofPmids <= input$NofPmidsInput[2]) | is.na(NofPmids))

			# Merge All
			allDF = Reduce(function(x, y) merge(x, y, all=TRUE, by=c("physicalPos", "rsid")),list(df_stat,df_isafe,df_snpeff,df_func,df_geva))

			if(input$FilterType=="OR"){
				toDownload = allDF 
			}
			
			if(input$FilterType=="AND"){
				toDownload = allDF %>% dplyr::filter_at(vars(-c("physicalPos","rsid")),all_vars(!is.na(.)))
			}

			toDownload <- toDownload %>% dplyr::select('Position'=physicalPos, rsid, ends_with('_ihs'), ends_with('_nsl'), ends_with('_isafe'), 'Variant effect'=effect,'Impact'= impact, 'Gene Context'=gene_effect,'TF binding'=TFbinding,'DNase Peak'=DNasePeak,'Motif'=motif,'DNase Footprint'=DNaseFootprint,eQTL,'Matched TF Motif'=matchedTFmotif,'Matched DNase Footprint'=matchedDNaseFootprint,'Regulome Score'=ranking,
				'Clinical Significance'=ClinicalSignificance,'Phenotype List'=PhenotypeList,
				'Risk Allele'=risk_allele,'Allele freq.'= risk_allele_freq,'Trait'= trait, 
				'Accession'=accession,'context'=context, 'Number of associations'=NumGWASStudies,
				'Disease'= diseaseName, 'Disease ID'=  diseaseId, 'Disease type'= diseaseType,'Source'=  source, DSI, DPI, 'Evidence Index'= EI, 'Pubmed ids'= NofPmids, starts_with('Age')) 
			set_download_names <- names(toDownload)

			list(toDownload, set_download_names)
			})
		})


	#############################################################
	###################### PLOTS  ###############################
	#############################################################

	############################ PLOTS SELECTION
	# Structure boxes with plots
	output$box_general <- renderUI({
		div(
			style = "position: relative; backgroundColor: #ecf0f5",
			tabBox(
				id        = "box_general",
				width     = NULL,
				height    = NULL,
				tabPanel(title   = "iHS",
					withSpinner(
						plotlyOutput("plot_general_ihs"),
						type  = 4,
						color = "#d33724",
						size  = 0.7
						)
					),
				tabPanel(
					title   = "nSL",
					withSpinner(
						plotlyOutput("plot_general_nsl"),
						type  = 4,
						color = "#d33724",
						size  = 0.7
						)
					)

				)
			)
		}) 
	output$box_bypop <- renderUI({
		div(style = "position: relative; backgroundColor: #ecf0f5",
			tabBox(
				id        = "box_bypop",
				width     = NULL,
				height    = returnHeigh(popSelect()) + 100,
				tabPanel(
					title   = "iHS",
					withSpinner(
						plotlyOutput("plot_bypop_ihs",  height = returnHeigh(popSelect())),
						type  = 4,
						color = "#d33724",
						size  = 0.7
						)
					),
				tabPanel(
					title   = "nSL",
					withSpinner(
						plotlyOutput("plot_bypop_nsl",  height = returnHeigh(popSelect())),
						type  = 4,
						color = "#d33724",
						size  = 0.7
						)),
				tabPanel(
					title   = "Table",
					withSpinner(
						dataTableOutput('table_selection'),
						type  = 4,
						color = "#d33724",
						size  = 0.7
						)
					)
				)
			)
		}) 

	# Filter & plot
	plot_general_ihs <- reactive({
		input$UpdateButt
		isolate({
			# Generate data
			tmp = set_filter_ihs()

			toPlot        = tmp[[1]]
			toPlot_filter = tmp[[2]]

			# Plot the data
			fig_iHS <- plotBoxPlot(toPlot, toPlot_filter, chrSelect(), input$extremeValuesInput_ihs, popSelect(),'iHS')

			fig_iHS %>% layout(xaxis = list(title = ""), 
				yaxis = list (title = "iHS"))
			})
		})

	plot_general_nsl <- reactive({
		input$UpdateButt
		isolate({
			tmp = set_filter_nsl()

			toPlot = tmp[[1]]
			toPlot_filter = tmp[[2]]
			fig_nSL <- plotBoxPlot(toPlot, toPlot_filter, chrSelect(), input$extremeValuesInput_nsl, popSelect(),'nSL')

			fig_nSL %>% layout(xaxis = list(title = ""), yaxis = list (title = "nSL"))
			})
		})

	plot_bypop_ihs <- reactive({
		input$UpdateButt
		isolate({
			toPlot = set_filter_ihs()[[2]]

			if(input$extremeValuesInput_ihs){
				toShow <- plotScatterPlot(toPlot, 'iHS','thres_ad',unique(toPlot$POP))  
				}else{
					toShow <- plotScatterPlot(toPlot, 'iHS','AD',unique(toPlot$POP))
				}
				toShow %>% layout(xaxis=list(range = c(regionSelect$min, regionSelect$max)))
				})
		})

	plot_bypop_nsl <- reactive({
		input$UpdateButt
		isolate({
			toPlot = set_filter_nsl()[[2]]
			if(input$extremeValuesInput_nsl){
				toShow <- plotScatterPlot(toPlot, 'nSL','thres_ad',unique(toPlot$POP))  
				}else{
					toShow <- plotScatterPlot(toPlot, 'nSL','AD',unique(toPlot$POP))
				}
				toShow %>% layout(xaxis=list(range = c(regionSelect$min, regionSelect$max)))
				})
		})

	# Call plot and output
	output$plot_general_ihs <- renderPlotly({plot_general_ihs()})
	output$plot_general_nsl <- renderPlotly({plot_general_nsl()})
	output$plot_bypop_ihs   <- renderPlotly({plot_bypop_ihs()})
	output$plot_bypop_nsl   <- renderPlotly({plot_bypop_nsl()})

	#Table selection
	output$table_selection <- renderDT(
		set_selection_to_table(),
		options = list(pageLength = 50,scrollX=TRUE)
		)
	


############################ FAVORED MUTATION 
output$box_isafe <- renderUI({
	div(
		style       = "position: relative; backgroundColor: #ecf0f5",
		tabBox(
			id        = "box_isafe",
			width     = NULL,
			height    = NULL,
			tabPanel(
				title   = "iSAFE",
				withSpinner(
					plotlyOutput("plot_isafe_boxplot"),
					type  = 4,
					color = "#d33724",
					size  = 0.7
					)
				)
			)
		)
	})

output$scatter_isafe <- renderUI({
	div(
		style       = "position: relative; backgroundColor: #ecf0f5",
		tabBox(
			id        = "scatter_isafe",
			width     = NULL,
			height    = returnHeighisafe(set_favallel()[[4]]) + 100,
			tabPanel(
				title   = "iSAFE",
				withSpinner(
					plotlyOutput('plot_isafe_scatter',
						height = returnHeighisafe(set_favallel()[[4]])),
					type  = 4,
					color = "#d33724",
					size  = 0.7
					)
				),
			tabPanel(
				title   = "Table",
				withSpinner(
					dataTableOutput('table_isafe'),
					type  = 4,
					color = "#d33724",
					size  = 0.7
					)
				)
			)
		)
	})

	# Filter & plot
	plot_isafe_boxplot <- reactive({
		input$UpdateButt
		isolate({
			# Retrieve data
			toPlot        = set_favallel()[[1]] %>% as.data.table
			toPlot_filter = set_favallel()[[2]] %>% as.data.table

			toPlot_filter = toPlot_filter[value > 0.05]
			# Plot the data
			fig_isafe <- plotBoxPlot(toPlot, toPlot_filter, chrSelect(), input$extremeISAFE, popSelect(),'iSAFE')

			fig_isafe %>% layout(xaxis = list(title = ""), yaxis = list (title = "iSAFE"))
			})
		})

	# Plot by pop
	plot_scatter_isafe <- reactive({
		
		input$UpdateButt
		isolate({
			
			# Generate data			
			toPlot = set_favallel()[[2]] %>% dplyr::filter(value >= 0.05) %>% as.data.table

			fltPops = set_favallel()[[4]]
			toPlot$POP <- factor(toPlot$POP, levels =popsData$pops[popsData$pops %in% fltPops])

			if(nrow(toPlot)>0){
			# Plot the data
			panel <- . %>% plot_ly(x = ~physicalPos, y= ~value) %>%
			add_trace(type = 'scatter',
				mode       = 'markers',
				color      = ~POP,
				colors     = as.vector(popsData$colors[popsData$pops %in% fltPops]),
				text       = ~paste0('<b>', POP,' &#8594; ', rsid,'<br></b>iSAFE: ',
					ifelse(thres_ad != 'Not extreme', paste0(round(value,2),"<b> (!)  TOP ",input$pval_isafe*100,"%</b>"),
						round(value,2))),
				hoverinfo  = 'text',
				showlegend = F,
				marker     = list(size = 10,line = list(color = 'black',width = 1))) %>%
			add_annotations(text = ~unique(POP),
				x         = 0.5,
				y         = 1,
				yref      = "paper",
				xref      = "paper",
				yanchor   = "bottom",
				showarrow = FALSE,
				font      = list(size = 15) ) %>%
			layout(showlegend = FALSE,
				shapes        = list(
					list(type   = "rect",
						x0        = 0,
						x1        = 1,
						xref      = "paper",
						y0        = 0,
						y1        = 16,
						yanchor   = 1,
						yref      = "paper",
						ysizemode = "pixel",
						fillcolor = toRGB("gray80"),
						line      = list(color = "transparent")
						),
					list(
						type = "line", 
						x0 = 0, 
						x1 = 1, 
						xref = "paper",
						y0 = ~unique(thres), 
						y1 = ~unique(thres), 
						line = list(color = "orange"),
						width = 4, 
						dash = 'dot',
						opacity = ~ifelse(unique(thres) == 0, 0.0, 0.4)
						)
					),
				xaxis = list(title = 'Position (bp)',
					range = c(regionSelect$min, regionSelect$max)),
				yaxis = list(title = 'iSAFE',range=c(0.025,max(toPlot$value)+0.1))
				)

			#Apply panel function to generate the plot
			iSAFE_plot <- toPlot %>% 
			group_by(POP) %>% 
			do(p = panel(.)) %>% 
			subplot(nrows = NROW(.), 
				shareX = TRUE, 
				shareY=TRUE, 
				which_layout = 1,
				margin       = 0.005,
				heights      = returnHeighPop(fltPops))
			} else {
				iSAFE_plot <- plot_ly() %>%
				add_markers(
					x = as.numeric(regionSelect$min)+((as.numeric(regionSelect$max)-as.numeric(regionSelect$min))/2),
					y = 0.5,
					opacity = 0.005,
					hoverinfo = 'none',
					showlegend = F) %>%
				add_annotations(
					x=as.numeric(regionSelect$min)+((as.numeric(regionSelect$max)-as.numeric(regionSelect$min))/2),
					y=0.5,
					xref = "x",
					yref = "y",
					text = "No data with these conditions",
					xanchor = 'center',
					showarrow = F) %>% 
				layout(
					xaxis = list(title = 'Position (bp)'),
					yaxis = list(title = 'iSAFE'))
			}		
			iSAFE_plot
			})
		})
	

	# Call plot and output
	output$plot_isafe_boxplot <- renderPlotly({plot_isafe_boxplot()})
	output$plot_isafe_histogram <- renderPlotly({plot_isafe_histogram()})
	output$plot_isafe_scatter <- renderPlotly({plot_scatter_isafe()})

	# Table isafe
	output$table_isafe <- renderDT(
		set_favallel()[[3]],
		selection = 'none',
		options = list(pageLength = 25,scrollX=TRUE)
		)




	############################ PLOTS FUNCT CHARACT 
	################### SNPEFF
	# Structure boxes with plots
	output$box_SNPEFF <- renderUI({
		div(
			style = "position: relative; backgroundColor: #ecf0f5",
			tabBox(
				id = "box_SNPEFF",
				width = NULL,
				height = 550,
				tabPanel(
					title = "SnpEff",
					withSpinner(
						plotlyOutput("plot_SNPEFF", height = 480),
						type = 4,
						color = "#d33724", 
						size = 0.7 
						)
					),
				tabPanel(
					title = "Table",
					withSpinner(
							# table_isafe
							dataTableOutput('table_snpeff'),
							type = 4,
							color = "#d33724", 
							size = 0.7 
							)
					)
				)
			)
		})

	# Filter and plot
	plot_SNPEFF <- reactive({
		input$UpdateButt
		isolate({
			toPlot = set_snpeff()[[1]]
			VEPmask_plot = set_snpeff()[[2]]

			if(nrow(toPlot) < 1){
				fig <- plot_ly() %>% add_markers(
					x = as.numeric(regionSelect$min)+((as.numeric(regionSelect$max)-as.numeric(regionSelect$min))/2),
					y = 0.5,
					opacity = 0.005,
					hoverinfo = 'none',
					showlegend = F) %>%
				add_annotations(
					x=as.numeric(regionSelect$min)+((as.numeric(regionSelect$max)-as.numeric(regionSelect$min))/2),
					y=0.5,
					xref = "x",
					yref = "y",
					text = "No data with these conditions",
					xanchor = 'center',
					showarrow = F) %>% 
				layout(
					xaxis = list(title = 'Position (bp)'),
					yaxis = list(
						title = '',
						type = 'category',
						autorange="reversed",
						categoryorder="array",
						categoryarray=str_to_title(gsub("_"," ",VEPinfo$inLabels))))
				}else{
					fig <- toPlot %>% plot_ly(x = ~physicalPos, 
						y = ~Labs,
						color= ~impact,
						type="scatter",
						mode="markers",
						text = ~paste0('<b>', rsid,'</b><br> ', Labs, '<br>', impact),
						hoverinfo = 'text',
						marker = list(size = 7,
							line = list(color = 'black', width = 2))) %>% 
					layout(title="",
						yaxis = list(
							title = '',
							type = 'category',
							autorange="reversed",
							categoryorder="array",
							categoryarray=str_to_title(gsub("_"," ",VEPinfo$inLabels[VEPmask_plot]))),
						xaxis=list(range = c(regionSelect$min, regionSelect$max),title = 'Position (bp)')
						)
					fig
				}
				})
		})


	# Call plot and output
	output$plot_SNPEFF <- renderPlotly({plot_SNPEFF()})		

	# FILTERING AGAIN BEFORE RENDER
	output$table_snpeff <- renderDT(
		set_snpeff()[[1]] %>% dplyr::select('Position'=physicalPos, rsid, effect, impact, 'Gene Context'=gene_effect) %>% dplyr::filter_at(vars(-c("Position", "rsid")), any_vars(!is.na(.))),
		selection = 'none',
		options = list(pageLength = 10, scrollX=TRUE)
		)

	################### REGULOMEDB
	# Structure boxes with plots
	output$box_ReguDB <- renderUI({
		div(
			style = "position: relative; backgroundColor: #ecf0f5",
			tabBox(
				id = "box_ReguDB",
				width = NULL,
				height = 550,
				tabPanel(
					title = "Regulome DB",
					withSpinner(
						plotlyOutput("plot_ReguDB", height = 480),
						type = 4,
						color = "#d33724", 
						size = 0.7 
						)
					),
				tabPanel(
					title = "Table",
					withSpinner(
						# table_isafe
						dataTableOutput('table_ReguDB'),
						type = 4,
						color = "#d33724", 
						size = 0.7 
						)
					)
				)
			)
		})

	# Filter and plot
	plot_ReguDB <- reactive({
		input$UpdateButt
		isolate({
			# Modify data toplot
			filtered <- set_functional() %>% 
			dplyr::select(physicalPos, rsid, TFbinding, DNasePeak, motif, DNaseFootprint, eQTL, matchedTFmotif, matchedDNaseFootprint, ranking) %>% 
			dplyr::filter_at(vars(-c("physicalPos", "rsid")), any_vars(!is.na(.))) %>% as.data.table
			toPlot <- filtered[ranking >= input$RegulomeInput[1] & ranking <= input$RegulomeInput[2]]


			# PLOT
			if(nrow(toPlot)<1){
				rankRegu <- plot_ly() %>% add_markers(
					x = as.numeric(regionSelect$min)+((as.numeric(regionSelect$max)-as.numeric(regionSelect$min))/2),
					y = 0.5,
					opacity = 0.005,
					hoverinfo = 'none',
					showlegend = F) %>%
				add_annotations(
					x=as.numeric(regionSelect$min)+((as.numeric(regionSelect$max)-as.numeric(regionSelect$min))/2),
					y=0.5,
					xref = "x",
					yref = "y",
					text = "No data with these conditions",
					xanchor = 'center',
					showarrow = F) %>% 
				layout(
					xaxis = list(title = 'Position (bp)'),
					yaxis = list(
						title = '',
						type = 'category',
						autorange="reversed",
						categoryorder="array",
						categoryarray=regulomeLabels))
				} else{
					rankRegu <- toPlot %>% plot_ly(x = ~physicalPos, 
						y = ~ranking,
						color = ~ranking,
						colors=ReguColors,
						name = 'rank', 
						type = 'scatter', 
						mode = 'markers',
						text = ~paste0('<b>', rsid,'</b><br>', chrSelect(), ':', physicalPos,'<br><br>',
							ifelse(TFbinding==1, "TFbinding <br>",""),
							ifelse(DNasePeak==1, "DNasePeak <br>",""),
							ifelse(motif==1, "motif <br>",""),
							ifelse(DNaseFootprint==1, "DNaseFootprint <br>",""),
							ifelse(eQTL==1, "eQTL <br>",""),
							ifelse(matchedTFmotif==1, "matchedTFmotif","")),
						hoverinfo = 'text',
						marker = list(size=7,line = list(color = '#404241', width = 1.5))) %>% 
					layout(yaxis = list(title = 'Score',
						type = 'category',
						autorange="reversed"),
					xaxis=list(range = c(regionSelect$min, regionSelect$max), title = 'Position (bp)'),
					showlegend = FALSE)
				} 
				rankRegu 
				})
		})

	# Call plot and output
	output$plot_ReguDB <- renderPlotly({plot_ReguDB()})

	#FILTERING AGAING BEFORE RENDRING
	output$table_ReguDB <- renderDT(
		set_functional()[ranking >= input$RegulomeInput[1] & ranking <= input$RegulomeInput[2]] %>% dplyr::select('Position'=physicalPos, rsid, 'TF binding'=TFbinding,'DNase Peak'=DNasePeak,'Motif'=motif,'DNase Footprint'=DNaseFootprint,eQTL,'Matched TF Motif'=matchedTFmotif,'Matched DNase Footprint'=matchedDNaseFootprint,'Score'=ranking) %>%dplyr::filter_at(vars(-c("Position", "rsid")), any_vars(!is.na(.))) %>% dplyr::mutate_at(vars(-c("Position", "rsid","Score")),as.logical),
		selection = 'none',
		options = list(pageLength = 10,scrollX=TRUE)
		)


	################### CLINVAR
	# Structure boxes with plots
	output$box_ClinVar <- renderUI({
		div(
			style = "position: relative; backgroundColor: #ecf0f5",
			tabBox(
				id = "box_ClinVar",
				width = NULL,
				height = 550,
				tabPanel(
					title = "ClinVar",
					withSpinner(
						plotlyOutput("plot_ClinVar_plot", height = 480),
						type = 4,
						color = "#d33724", 
						size = 0.7 
						)
					),
				tabPanel(
					title = "Proportion",
					withSpinner(
						plotlyOutput("plot_ClinVar_pie", height = 480),
						type = 4,
						color = "#d33724", 
						size = 0.7 
						)
					),
				tabPanel(
					title = "Table",
					withSpinner(
						# table_isafe
						dataTableOutput('table_ClinVar'),
						type = 4,
						color = "#d33724", 
						size = 0.7 
						)
					)
				)
			)
		})

	# Filter and plot
	plot_ClinVar_plot <- reactive({
		input$UpdateButt
		isolate({

			# Filter data
			toPlot <- set_functional() %>% dplyr::select(physicalPos, rsid, ClinicalSignificance, ClinSigSimple, PhenotypeList) %>%
			dplyr::filter(ClinicalSignificance %in% input$ClinVarInput, !is.na(rsid)) %>% 
			dplyr::filter_at(vars(-c("physicalPos", "rsid")), any_vars(!is.na(.)))

			ClinVarmask = names(ClinVarPal) %in% as.character(toPlot$ClinicalSignificance %>% unique)


			if(nrow(toPlot)<1){
				figClinVar <- plot_ly() %>% add_markers(
					x = as.numeric(regionSelect$min)+((as.numeric(regionSelect$max)-as.numeric(regionSelect$min))/2),
					y = 0.5,
					opacity = 0.005,
					hoverinfo = 'none',
					showlegend = F) %>%
				add_annotations(
					x=as.numeric(regionSelect$min)+((as.numeric(regionSelect$max)-as.numeric(regionSelect$min))/2),
					y=0.5,
					xref = "x",
					yref = "y",
					text = "No data with these conditions",
					xanchor = 'center',
					showarrow = F) %>% 
				layout(
					xaxis = list(title = 'Position (bp)'),
					yaxis = list(
						title = '',
						type = 'category',
						autorange="reversed",
						categoryorder="array",
						categoryarray=names(ClinVarPal)))
				} else{
			# Plot
			figClinVar <- toPlot %>% plot_ly(x = ~physicalPos, 
				y = ~ClinicalSignificance,
				color=~ClinicalSignificance,
				colors=ClinVarPal[ClinVarmask],
				type="scatter",
				mode="markers",
				marker = list(
					size=10,
					line = list(color = '#404241', width = 1)),
				text = ~paste0('<b>', rsid,'</b><br>', 
					ClinicalSignificance, '<br> ',
					chrSelect(), ':', physicalPos),
				hoverinfo = 'text') %>% 
			layout(title="",
				yaxis = list(
					title = '',
					type = 'category',
					autorange="reversed",
					categoryorder="array",
					categoryarray=names(ClinVarPal)[ClinVarmask]),
				xaxis=list(range = c(regionSelect$min, regionSelect$max), title = 'Position (bp)'),
				showlegend = FALSE); 
		}
		figClinVar

		})
		})

	plot_ClinVar_pie <- reactive({
		input$UpdateButt
		isolate({
			# Filter data
			toPlot <- set_functional() %>% dplyr::select(physicalPos, rsid, ClinicalSignificance, ClinSigSimple, PhenotypeList) %>%
			dplyr::filter(ClinicalSignificance %in% input$ClinVarInput,!is.na(rsid)) %>% 
			dplyr::filter_at(vars(-c("physicalPos", "rsid")), any_vars(!is.na(.)))
			numClin <- data.table(table(toPlot$ClinicalSignificance))
			names(numClin)<- c('ClinicalTrait','Counts')
			
			#Plot
			ClinVarpie <- numClin %>% plot_ly(labels = ~ClinicalTrait, values = ~Counts,
				marker = list(colors = ClinVarPal[numClin$ClinicalTrait]))
			ClinVarpie <- ClinVarpie %>% add_pie(hole = 0.45)
			ClinVarpie <- ClinVarpie %>% layout(title = "ClinVar Consequences",
				showlegend = F,
				xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
				yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
			ClinVarpie
			})
		})
	# Call plot and output
	output$plot_ClinVar_plot = renderPlotly({plot_ClinVar_plot()})
	output$plot_ClinVar_pie  = renderPlotly({plot_ClinVar_pie()})
	
	#FILTERING AGAING BEFORE RENDER
	output$table_ClinVar     = renderDT(set_functional() %>% dplyr::select('Position'=physicalPos, rsid, ClinicalSignificance, ClinSigSimple, PhenotypeList) %>%
		dplyr::filter(ClinicalSignificance %in% input$ClinVarInput, !is.na(rsid)) %>% 
		dplyr::filter_at(vars(-c("Position", "rsid")), any_vars(!is.na(.))),
		selection = 'none',
		options = list(pageLength = 10,scrollX=TRUE)
		)


	################### GWASCAT
	# Structure boxes with plots
	output$box_GWAScat <- renderUI({
		div(
			style = "position: relative; backgroundColor: #ecf0f5",
			tabBox(
				id = "box_GWAScat",
				width = NULL,
				height = 550,
				tabPanel(
					title = "Number of associations",
					withSpinner(
						plotlyOutput("plot_GWAScat_plot", height = 480),
						type = 4,
						color = "#d33724", 
						size = 0.7 
						)
					),
				tabPanel(
					title = "Traits Proportion",
					withSpinner(
						plotlyOutput("plot_GWAScat_pie", height = 380),
						type = 4,
						color = "#d33724", 
						size = 0.7 
						)
					),
				tabPanel(
					title = "Table",
					withSpinner(
						# table_isafe
						dataTableOutput('table_GWAScat'),
						type = 4,
						color = "#d33724", 
						size = 0.7 
						)
					)
				)
			)
		})

	# Filter and plot
	plot_GWAScat_plot <- reactive({
		input$UpdateButt
		isolate({
			# Filter data
			toFilter <- set_functional() %>% dplyr::select( physicalPos, rsid, risk_allele, risk_allele_freq, 
				trait, accession, context, NumGWASStudies) %>%
			dplyr::filter_at(vars(-c("physicalPos", "rsid")), any_vars(!is.na(.)))
			toFilter$NumGWASStudies[is.na(toFilter$NumGWASStudies)]<-0
			toPlot <- toFilter %>% dplyr::filter(between(NumGWASStudies, input$GWASInput[1], input$GWASInput[2]))
			
			if(nrow(toPlot)<1){
				fig <- plot_ly() %>% add_markers(
					x = as.numeric(regionSelect$min)+((as.numeric(regionSelect$max)-as.numeric(regionSelect$min))/2),
					y = 2,
					opacity = 0.005,
					hoverinfo = 'none',
					showlegend = F) %>%
				add_annotations(
					x=as.numeric(regionSelect$min)+((as.numeric(regionSelect$max)-as.numeric(regionSelect$min))/2),
					y=2,
					xref = "x",
					yref = "y",
					text = "No data with these conditions",
					xanchor = 'center',
					showarrow = F) %>% 
				layout(
					xaxis = list(title = 'Position (bp)'),
					yaxis = list(
						title = ''))
				} else{
			# Plot
			fig <- toPlot %>% plot_ly(x = ~physicalPos, 
				y = ~NumGWASStudies,
				type="scatter",
				mode="markers",
				text = ~paste0('<b>', rsid,'</b>: ', 
					NumGWASStudies, ' study/es'),
				hoverinfo = 'text',
				colors = c('firebrick', 'darkblue'),
				showlegend = F,
				marker = list(size = 10,
					color = 'rgba(255, 182, 193, .9)',
					line = list(color = 'rgba(152, 0, 0, .8)',
						width = 2))) %>% 
			layout(xaxis=list(range = c(regionSelect$min, regionSelect$max), title = 'Position (bp)'));
		}
		fig
		})
		})
	# GWAS plot
	plot_GWAScat_pie <- reactive({
		input$UpdateButt
		isolate({
			toFilter <- set_functional() %>% dplyr::select( physicalPos, rsid, risk_allele, risk_allele_freq, 
				trait, accession, context, NumGWASStudies) %>%
			dplyr::filter_at(vars(-c("physicalPos", "rsid")), any_vars(!is.na(.))) %>% 
			dplyr::filter(between(NumGWASStudies, input$GWASInput[1], input$GWASInput[2]))

			numTraits <- data.table(table(unlist(strsplit(toFilter$trait, ";"))))
			fig1 <- numTraits %>%  plot_ly(labels = ~V1, values = ~N)
			fig1 <- fig1 %>% add_pie(hole = 0.45)
			fig1 <- fig1 %>% layout(title = paste0("Traits appearing in GWAS catalog"),
				showlegend = F,
				xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
				yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
			fig1
			})
		})

	# Call plot and output
	output$plot_GWAScat_plot <- renderPlotly({plot_GWAScat_plot()})
	output$plot_GWAScat_pie <- renderPlotly({plot_GWAScat_pie()})
	
	# FILTERING AGAIN BEFORE RENDER
	output$table_GWAScat <- renderDT(set_functional() %>% dplyr::filter(between(NumGWASStudies, input$GWASInput[1], input$GWASInput[2])) %>%
		dplyr::select('Position'=physicalPos, rsid, 
			'Risk Allele'=risk_allele,'Allele freq.'= risk_allele_freq,'Trait'= trait, 
			'Accession'=accession,'context'=context, 'Number of associations'=NumGWASStudies) %>%
		dplyr::filter_at(vars(-c("Position", "rsid")), any_vars(!is.na(.))),
		selection = 'none',
		options = list(pageLength = 10,scrollX=TRUE)
		)

	################### DISGENET
	# Structure boxes with plots
	output$box_DisGeNET <- renderUI({
		div(
			style = "position: relative; backgroundColor: #ecf0f5",
			tabBox(
				id = "box_DisGeNET",
				width = NULL,
				height = 550,
				tabPanel(
					title = "Disease Specificity Index",
					withSpinner(
						plotlyOutput("plot_DisGeNET", height = 480),
						type = 4,
						color = "#d33724", 
						size = 0.7 
						)
					),
				tabPanel(
					title = "Table",
					withSpinner(
						# table_isafe
						dataTableOutput('table_DisGeNET'),
						type = 4,
						color = "#d33724", 
						size = 0.7 
						)
					)
				)
			)
		})

	# Filter and plot
	plot_DisGeNET <- reactive({
		input$UpdateButt
		isolate({

			# Filter
			toPlot <- set_functional() %>% dplyr::select(physicalPos, rsid, diseaseName, diseaseId, diseaseType, source, DSI, DPI, EI, NofPmids,NumDGNAsso) %>%
			dplyr::filter_at(vars(-c("physicalPos", "rsid")), any_vars(!is.na(.))) %>% as.data.table

			toPlot = toPlot[DSI >= input$DSIInput[1] & DSI <= input$DSIInput[2] & EI >= input$EInput[1] & EI <= input$EInput[2] & NofPmids >= input$NofPmidsInput[1] & NofPmids <= input$NofPmidsInput[2]]
			toPlot$NofPmids[is.na(toPlot$NofPmids)] <- 0
			toPlot$EI[is.na(toPlot$EI)] <- NULL

			# Plot
			if(nrow(toPlot)<1){
				fig <- plot_ly() %>% add_markers(
					x = as.numeric(regionSelect$min)+((as.numeric(regionSelect$max)-as.numeric(regionSelect$min))/2),
					y = 2,
					opacity = 0.005,
					hoverinfo = 'none',
					showlegend = F) %>%
				add_annotations(
					x=as.numeric(regionSelect$min)+((as.numeric(regionSelect$max)-as.numeric(regionSelect$min))/2),
					y=2,
					xref = "x",
					yref = "y",
					text = "No data with these conditions",
					xanchor = 'center',
					showarrow = F) %>% 
				layout(
					xaxis = list(title = 'Position (bp)'),
					yaxis = list(
						title = ''))
				} else{
					fig <- toPlot %>% plot_ly(x = ~physicalPos,
						y = ~DSI,
						type = 'scatter', 
						mode = 'markers',
						marker = list(
							color=~EI,
							line = list(color = 'black', width = 2),
							size=10,
							colorbar = list(title = "Evidence Index"),
							colorscale='Viridis',
							showscale = TRUE),
						text = ~paste0('<b>', rsid,'</b><br>',
							NumDGNAsso, 
							' associations(s)<br>',
							'<b>Type: </b>', gsub(";", ", ",diseaseType),
							'<br><b> Evidence Index:</b> ',ifelse(!is.na(EI), EI,"?"),
							'<br><br>', NofPmids, ' Pubmed Identifier(s)'),
						hoverinfo = 'text') %>% 
					layout(title="",
						yaxis = list(title = 'Disease Specificity Index',
							range = c(0, 1.3)),
						xaxis=list(title="Position (bp)"),
						range = c(regionSelect$min, regionSelect$max))
				}
				fig

				})
		})
	
	# Call plot and output
	output$plot_DisGeNET <- renderPlotly({plot_DisGeNET()})
	output$table_DisGeNET <- renderDT(set_functional()[DSI >= input$DSIInput[1] & DSI <= input$DSIInput[2] & EI >= input$EInput[1] & EI <= input$EInput[2] & NofPmids >= input$NofPmidsInput[1] & NofPmids <= input$NofPmidsInput[2]] %>% dplyr::select('Position'=physicalPos, rsid,  'Disease'= diseaseName, 'Disease ID'=  diseaseId, 'Disease type'= diseaseType,'Source'=  source, DSI, DPI, 'Evidence Index'= EI, 'Pubmed ids'= NofPmids) %>%dplyr::filter_at(vars(-c("Position", "rsid")), any_vars(!is.na(.))),selection = 'none',options = list(pageLength = 5,scrollX=TRUE)
		)

	
	############################ PLOTS AGE
	# PLOT 9: AGE PLOT -----------------------------|
	output$box_geva <- renderUI({
		div(
			style = "position: relative; backgroundColor: #ecf0f5",
			tabBox(
				id = "box_geva",
				width = NULL,
				height = 750,
				tabPanel(
					title = "Age Distribution",
					withSpinner(
						plotlyOutput("plot_geva", height = 660),
						type = 4,
						color = "#d33724", 
						size = 0.7 
						)
					),
				tabPanel(
					title = "Table",
					withSpinner(
						# table_isafe
						dataTableOutput('table_age'),
						type = 4,
						color = "#d33724", 
						size = 0.7 
						)
					)
				)
			)
		})  

	plot_geva <- reactive({
		input$UpdateButt
		isolate({		

			toPlot = set_age()

			# Adjust vars
			if(input$GenYears>0){
				toPlot <- toPlot %>% dplyr::mutate_at(vars(starts_with('Age')), ~. *input$GenYears)
				Yunits <- "years"
				GenLong <- paste0("Age\n(",input$GenYears," ", Yunits, "/generation)")
				} else{
					Yunits <- "generations"
					GenLong <- paste0("Age\n(generations)")
				}
				ClockLong <- GEVAclocks$plotTitle[GEVAclocks$modelClocks==input$AgeModelInput]

				if(nrow(toPlot)<1){
					figAge <- plot_ly() %>% add_markers(
						x = as.numeric(regionSelect$min)+((as.numeric(regionSelect$max)-as.numeric(regionSelect$min))/2),
						y = 2000,
						opacity = 0.005,
						hoverinfo = 'none',
						showlegend = F) %>%
					add_annotations(
						x=as.numeric(regionSelect$min)+((as.numeric(regionSelect$max)-as.numeric(regionSelect$min))/2),
						y=2000,
						xref = "x",
						yref = "y",
						text = "No data with these conditions",
						xanchor = 'center',
						showarrow = F) %>% 
					layout(
						xaxis = list(title = 'Position (bp)'),
						yaxis = list(
							title = ''))
					} else{

			# Plot the data
			figAge <- plot_ly(data = toPlot, x=~physicalPos)

			# If error bars marked
			if(input$errorBars){
				figAge <-figAge %>% add_trace(
					data=toPlot,
					y = ~AgeMode,
					type = 'scatter', 
					mode = 'markers',
					name = ClockLong,
					size= ~QualScore,
					opacity= ~QualScore,
					color= ~QualScore,
					colors= colorByQuality(floor(min(toPlot$QualScore)*10),
						ceiling(max(toPlot$QualScore)*10)),
					marker = list(line = list(color = '#26202b')),
					error_y =  list(type = "data", 
						symmetric = FALSE, 
						arrayminus = ~AgeCI95Lower,
						array = ~AgeCI95Upper,
						color = '#26202b',
						size=0.05),
					showlegend = FALSE,
					text = ~paste0(
						'<b>', rsid,'</b><br>chr', chrSelect(),':', physicalPos,
						"<br><b>",AgeMode," ", Yunits,"</b><br>(",
						round(AgeCI95Lower,2),"-", round(AgeCI95Upper,2),")"),
					hoverinfo = 'text')
				} else {
				# If not error bars
				figAge <-figAge %>% add_trace(
					data=toPlot,
					y = ~AgeMode,
					type = 'scatter', 
					mode = 'markers',
					name = ClockLong,
					size= ~QualScore,
					opacity= ~QualScore,
					color= ~QualScore,
					colors= colorByQuality(floor(min(toPlot$QualScore)*10),
						ceiling(max(toPlot$QualScore)*10)),
					marker = list(line = list(color = '#26202b')),
					showlegend = FALSE,
					text = ~paste0(
						'<b>', rsid,'</b><br>chr', chrSelect(),':', physicalPos,
						"<br><b>",AgeMode," ", Yunits,"</b><br>(",
						round(AgeCI95Lower,2),"-", round(AgeCI95Upper,2),")"),
					hoverinfo = 'text')
			}
			figAge <-figAge %>%
			colorbar(title = "Quality Score",
				thickness=15, len=0.25,tickmode ="auto",nticks=0,
				bordercolor = '#26202b') %>% 
			layout(xaxis = list(title = "Position (bp)", 
				range = c(regionSelect$min, regionSelect$max)),
			yaxis = list(title = GenLong),
			title=paste0(ClockLong))
		}
		figAge
		})
		})

	# Call plot and output
	output$plot_geva <- renderPlotly({plot_geva()})

	# Table GEVA
	output$table_age<- renderDT(
		set_age(),
		selection = 'none',
		options = list(pageLength = 25, scrollX=TRUE)
		)

	############################ GENERAL REPORT 

	# BUTTON LINK TO POPHUMAN
	output$pophumanLink <- renderUI({
		actionButton("PHlinkbutton", 
			label = " PopHuman", 
			icon = icon("gitter"),
			onclick =paste0("window.open('https://pophuman.uab.cat/?loc=chr",
				chrSelect(),"%3A",regionSelect$min,"..",regionSelect$max,
				"&tracks=DNA%2Cgene_annotations%2Crecomb_Bherer2017_sexavg_10kb%2CiHS_",
				overviewSelect$Pop,"_10kb%2CTajima_D_",overviewSelect$Pop,
				"_10kb&highlight=', '_blank')"),
			width="95%",
			style="
			border: 1.8px solid #ffffff;
			color: #343a40;
			background-color: #f3c073;
			padding: 15px 32px;
			text-align: center;
			display: inline-block;
			font-size: 16px;
			margin: 10px 5px;
			cursor: pointer;
			border-radius: 5px;")
		})

	# BUTTON LINK TO POPHUMANSCAN (optional)
	output$pophumanscanLink <- renderUI({

		# Pare region
		overlapsPHS <- phs_overlap(as.numeric(chrSelect()),as.numeric(regionSelect$min),as.numeric(regionSelect$max))
		#Check if overlaps with phs regios
		if(!is.null(overlapsPHS)){
			linkToPHS <- paste0("window.open('https://pophumanscan.uab.cat/tables.php?coord=",overlapsPHS,"', '_blank')")
			} else {
				linkToPHS <-"window.open('https://pophumanscan.uab.cat/tables.php?coord=', '_blank')"
			}

			actionButton("PHSlinkbutton", 
				label = " PopHumanScan", 
				icon = icon("fingerprint"),
				onclick = linkToPHS,
				width="95%",
				style="
				border: 1.8px solid #ffffff;
				color: #343a40;
				background-color: #f18686;
				padding: 15px 32px;
				text-align: center;
				display: inline-block;
				font-size: 16px;
				margin: 10px 5px;
				cursor: pointer;
				border-radius: 5px;")
			})


	# Box with Jbrowse
	output$box_jbrows <- renderUI({
		div(
			style = "position: relative; backgroundColor: #ecf0f5",
			tabBox(
				id = "box_jbrows",
				width = NULL,
				height = 550,
				tabPanel(
					title = "",
					h3('Genomic Context'),br(),
					withSpinner(
						# this adds to the browser to the UI, and specifies the output ID in the server
						JBrowseROutput("browserOutput",  
							width = "99%", height = "300px"),
						type = 4,
						color = "#d33724", 
						size = 0.7 
						)
					)
				)
			)
		})

	assembly = assembly("https://jbrowse.org/genomes/hg19/fasta/hg19.fa.gz", bgzip = TRUE)
	annot = track_feature("https://s3.amazonaws.com/jbrowse.org/genomes/hg19/GRCh37_latest_genomic.sort.gff.gz",assembly)
	clinvar = track_variant("https://ftp.ncbi.nlm.nih.gov/pub/clinvar/vcf_GRCh37/clinvar.vcf.gz",assembly)


	# recomb = track_wiggle("https://hgdownload.soe.ucsc.edu/gbdb/hg19/decode/SexAveraged.bw",assembly)
	all_tracks = tracks(annot,clinvar)
	custom_theme = JBrowseR::theme("#343a40", "#343a40")

	default_session <- default_session(assembly,c(annot))
	output$browserOutput <- renderJBrowseR(
		JBrowseR(
			"View",
			assembly = assembly,
			location = paste0(chrSelect(),":",regionSelect$min,"..",regionSelect$max),
			theme = custom_theme,
			tracks   = all_tracks,
			defaultSession = default_session
			)
		)

	########### PLOT
	# Haplotypes
	haplo_plots <- reactive({
		req(phv_enable_haplotypes)
		# temp file to subset vcf
		outfile <- tempfile(fileext='.vcf.gz')
		on.exit(unlink(c(outfile, paste0(outfile, '.tbi'))), add = TRUE)
		# Command to subset pophuman vcf
		vcf_source <- file.path(phv_vcf_dir, paste0('chr', chrSelect(), overviewSelect$Pop, '.vcf.gz'))
		x = paste0(
			shQuote(phv_bcftools), ' view -r ', chrSelect(), ':', regionSelect$min, '-', regionSelect$max,
			' -m2 -M2 -v snps -c1 ', shQuote(vcf_source),
			' | sed "s/|||//g" | ', shQuote(phv_bcftools), ' view -Oz -o ', shQuote(outfile))
		status <- system(x)
		validate(need(status == 0 && file.exists(outfile),
			"The haplotype data could not be extracted for this region."))

		#get selected rsid if non selected get first
		idx = input$tablePrior_rows_selected
		if(is.null(idx)){
			idx = 1
		}
		# warning(print(set_prior()[idx,2]))
		# load rehh file
		hh        = rehh::data2haplohh(outfile,polarize_vcf=T)

		# rehh plot's estimations
		ehh       = rehh::calc_ehh(hh,mrk=set_prior()[idx,2])
		furcation = rehh::calc_furcation(hh,mrk=set_prior()[idx,2])
		haplen    = rehh::calc_haplen(furcation)	

		# return objects to plot
		list(ehh,furcation,hh)
		})

	output$haplo1 	<- renderPlot({
		req(phv_enable_haplotypes)
		plot(haplo_plots()[[1]])
		})

	output$haplo2 <- renderPlot({
		req(phv_enable_haplotypes)
		plot(haplo_plots()[[2]])
		})  

	# Structure boxes with plots
	output$box_report <- renderUI({
		div(
			style = "position: relative; backgroundColor: #ecf0f5",
			tabBox(
				id = "box_report",
				width = NULL,
				height = 850,
				tabPanel(
					title = "",
					h3('Overview'),
					tags$div(
						tags$span(style = "color: #367FA9;font-size:16px", 
							"(!) To modify the data displayed in the plot below, please, refer to the filters at the right side of this page.")),br(),br(),
					withSpinner(
						plotlyOutput("plot_report", height = 700),
						type = 4,
						color = "#d33724", 
						size = 0.7 
						)
					)
				)
			)
		})
	
	# Filter & plot
	set_threshold_report <- reactive({
		input$ButtonNum2
		isolate({

			toPlot = set_report()

			# Get pvalues data for the population of interest
			ihs_pval_pop = set_pvalues()[[4]] %>% dplyr::select(physicalPos,rsid,starts_with(overviewSelect$Pop)) %>%  dplyr::filter_at(vars(-c("physicalPos","rsid")), any_vars(!is.na(.)))
			nsl_pval_pop = set_pvalues()[[5]] %>% dplyr::select(physicalPos,rsid,starts_with(overviewSelect$Pop)) %>%  dplyr::filter_at(vars(-c("physicalPos","rsid")), any_vars(!is.na(.)))
			isafe_pval_pop = set_pvalues()[[6]] %>% dplyr::select(physicalPos,rsid,starts_with(overviewSelect$Pop)) %>%  dplyr::filter_at(vars(-c("physicalPos","rsid")), any_vars(!is.na(.)))

			# Get positions with an extreme pvalues
			x = ihs_pval_pop[ihs_pval_pop[[overviewSelect$Pop]] <= input$pval_ihs]$physicalPos
			y = nsl_pval_pop[nsl_pval_pop[[overviewSelect$Pop]] <= input$pval_nsl]$physicalPos
			z = isafe_pval_pop[isafe_pval_pop[[overviewSelect$Pop]] <= input$pval_isafe]$physicalPos

			tresh_ihs = tryCatch(
			{
				min(abs(toPlot[physicalPos %in% x]$ihs))
				},warning = function(e){
					0
					},error = function(e){
						0
					}
					)
			tresh_nsl = tryCatch(
			{
				min(abs(toPlot[physicalPos %in% y]$nsl))
				},warning = function(e){
					0
					},error = function(e){
						0
					}
					)
			tresh_isafe = tryCatch(
			{
				min(toPlot[physicalPos %in% z]$isafe)
				},warning = function(e){
					0
					},error = function(e){
						0
					}
					)
			cutoff = c(tresh_ihs,tresh_nsl,tresh_isafe)
			cutoff
			})
		})

	plot_report <- reactive({
		# input$ButtonNum2
		# isolate({
			# Add data to plot
			toPlot = set_report()

			# Get tresholds
			tresh_ihs   = as.numeric(set_threshold_report()[1])
			tresh_nsl   = as.numeric(set_threshold_report()[2])
			tresh_isafe = as.numeric(set_threshold_report()[3])
			
			# PLOT!
			fig <- toPlot %>%
			plot_ly(
				x = ~physicalPos,
				y = ~isafe,
				type = 'scatter',
				mode = 'markers',
				color = ~str_to_title(gsub("_"," ", effect)),
				colors='Paired',
				size = ~abs(ihs)+abs(nsl),
				sizes = c(7, 25),
				marker = list(opacity = 0.75, sizemode = 'diameter', 
					line = list(width = 1.25, color = '#000000')),
				text = ~paste0('<b>', rsid,'</b> &#8594; ', 
					chrSelect(), ':', physicalPos,'<br><br>',
					ifelse(isafe!=0, 
						paste0("<b>iSAFE:</b> ", isafe,'<br>'),""),
					ifelse((tresh_isafe!=0 & isafe >= tresh_isafe), 
						paste0(" <b>(!) top ", input$pval_isafe * 100,"%</b><br>"),""),
					ifelse(ihs!=0, 
						paste0("<br><b>iHS:</b> ",ihs,'<br>'),""),
					ifelse((tresh_ihs!=0 & abs(ihs)>=tresh_ihs), 
						paste0(" <b>(!) top ", input$pval_ihs*100,"%</b><br>"),""),
					ifelse(nsl!=0, 
						paste0("<br><b>nSL:</b> ",nsl,'<br>'),""),
					ifelse((tresh_nsl!=0 & abs(nsl)>=tresh_nsl), 
						paste0(" <b>(!) top ", input$pval_nsl*100,"%</b><br>"),""),
					'<br><b>',str_to_title(gsub("_"," ", effect)),'</b> (',tolower(impact), ')<br>',
					ifelse(!is.na(ClinicalSignificance), paste0(ClinicalSignificance,"<br>"),""),
					ifelse(!is.na(NumGWASStudies), paste0("GWAS hits:",NumGWASStudies,"<br>"),""),
					ifelse(!is.na(DSI), paste0("Disease sp. index:",DSI,"<br>"),""),
					ifelse(!is.na(AgeMode), paste0("<b>Age: ", AgeMode, " gen.</b> [",
						AgeCI95Lower,"-",AgeCI95Upper,"] (QS=", QualScore,")<br>"),""),"<br>"
					),
				hoverinfo = 'text')

				# Check if the table is highlighted
				s = input$tablePrior_rows_selected
				
				# If any, add annotations
				if(length(s)){
					m <- set_prior()[s,]
					fig <- fig %>% layout(
						annotations = list(
							x = m$Position,
							y = m$iSAFE,
							text = paste0("<b>",s,":</b> ",m$rsid),
							text = "Center Anchor",
							showarrow = T,
					 		# Styling annotations' text:
					 		font = list(size = 14)
					 		)
						)
				}

				fig <- fig %>%
				layout(showlegend=TRUE, legend = list(orientation = 'h', y = -0.3,
					itemsizing='constant'),
				xaxis = list(title = 'Position (bp)',
					range = c(regionSelect$min, regionSelect$max)),
				yaxis = list(title = 'iSAFE'),
				title = paste0('Overview for ', overviewSelect$Pop))

			# Add threshold line	
			fig <- fig %>% layout(
				shapes = list(
					type = "line", 
					x0 = 0, 
					x1 = 1, 
					xref = "paper",
					y0 = tresh_isafe, 
					y1 = tresh_isafe, 
					line = list(color = "orange"),
					width = 4, 
					dash = 'dot',
					opacity = ~ifelse(tresh_isafe == 0, 0.0, 0.4)
					)
				)

			fig
		# })
		})

	# Call plot and output
	output$plot_report <- renderPlotly({plot_report()})


	########### TABLE
	# Box for the prioretized variants
	output$box_prior <- renderUI({
		div(
			style = "position: relative; backgroundColor: #ecf0f5",
			tabBox(
				id = "box_prior",
				width = NULL,
				height = 500,
				tabPanel(
					title = "",
					h3('Top 20 Variants'),
					tags$div(
						tags$span(style = "color: #367FA9;font-size:16px", 
							"The following variants are ordered by: 1) iSAFE value, 2) iHS, 3) nSL, 4) GWAS Catalog associations, 5) RegulomeDB score and 6) SnpEFF impact")),
					tags$div(
						tags$span(style = "color: #367FA9;font-size:16px", 
							if (phv_enable_haplotypes) {
								"By clicking a row, an arrow identifying the variant selected will appear in the plot above, and the EHH and haplotype furcations around the selected SNV will be updated in the section below."
							} else {
								"By clicking a row, an arrow identifying the variant selected will appear in the plot above. Haplotype plots are available on the main PopHumanVar server at the UAB."
							})),
					br(),
					tags$div(
						tags$span(style = "color: #367FA9;font-size:16px",
							"Visit the Downloads section to see all variants in the region.")),
					br(),
					br(),
					withSpinner(
						dataTableOutput('tablePrior'),
						type = 4,
						color = "#d33724", 
						size = 0.7 
						)
					)
				)
			)
		})

	# Table with top 20 variants
	output$tablePrior <- DT::renderDataTable(
		set_prior(),
		options = list(pageLength = 20,scrollX=TRUE),
		selection ='single',
		server = FALSE
		)

	############ INFO BOX WITH SUMMARY
	output$info_isafe <- renderInfoBox({
		tresh_isafe = set_threshold_report()[3]
		info <- as.vector(unlist(getInfoBoxData('isafe', set_infoBoxes())))

		if(is.null(info)){
			infoBox(
				title    = HTML('Favored Mutation: <b><span style="text-transform:lowercase">i</span><span style="text-transform:uppercase">SAFE</span></b>'),
				subtitle="for iSAFE in this region",
				value="No data",
				icon     = icon("asterisk"),
				color    = "olive"
				)
			} else {
				if(as.numeric(info[3])==1){
					infoBox(
						title    = HTML('Favored Mutation: <b><span style="text-transform:lowercase">i</span><span style="text-transform:uppercase">SAFE</span></b>'),
						value=HTML(paste0("<a href='https://www.ncbi.nlm.nih.gov/snp/",info[1],"' target='_blank'>",info[1],"</a>")),
						subtitle=paste0("reports the most extreme iSAFE value: ", 
							round(as.numeric(info[2]),4),
							ifelse((tresh_isafe!=0 & as.numeric(info[2]) > tresh_isafe),
								paste0(" and belongs to the top ",input$pval_isafe*100,"%"),"")),
						icon     = icon("asterisk"),
						color    = "olive")
					} else{
						infoBox(
							title    = HTML('Favored Mutation: <b><span style="text-transform:lowercase">i</span><span style="text-transform:uppercase">SAFE</span></b>'),
							value = HTML(paste0("<a href='https://www.ncbi.nlm.nih.gov/snp/",info[1],"' target='_blank'>",info[1],"</a>")),
							subtitle=paste0("... and ", as.numeric(info[3]) - 1," more report the most extreme iSAFE value: ", round(as.numeric(info[2]),4),
								ifelse((tresh_isafe!=0 & as.numeric(info[2]) > tresh_isafe),
									paste0(" and belong to the top ",input$pval_isafe*100,"%"),"")),
							icon     = icon("asterisk"),
							color    = "olive")
					}
				}
				})	
	output$info_iHS <- renderInfoBox({
		tresh_ihs   = set_threshold_report()[1]
		info <- as.vector(unlist(getInfoBoxData('ihs', set_infoBoxes())))
		if(is.null(info)){
			infoBox(
				title= HTML('Selection: <b><span style="text-transform:lowercase">i</span><span style="text-transform:uppercase">HS</span></b>'),
				subtitle="for iHS in this region",
				value="No data",
				icon = icon("stream"),
				color = "yellow"
				)
			} else {
				if(as.numeric(info[3])==1){
					infoBox(
						title    = HTML('Selection: <b><span style="text-transform:lowercase">i</span><span style="text-transform:uppercase">HS</span></b>'),
						value=HTML(paste0("<a href='https://www.ncbi.nlm.nih.gov/snp/",info[1],"' target='_blank'>",info[1],"</a>")),
						subtitle=paste0("reports the most extreme iHS value: ", round(as.numeric(info[2]),2),
							ifelse((tresh_ihs!=0 & abs(as.numeric(info[2])) >= tresh_ihs),
								paste0(" and belongs to the top ", input$pval_ihs*100, "%"),"")),
						icon = icon("stream"),
						color = "yellow")
				} 
				else{
					infoBox(
						title    = HTML('Selection: <b><span style="text-transform:lowercase">i</span><span style="text-transform:uppercase">HS</span></b>'),
						value=HTML(paste0("<a href='https://www.ncbi.nlm.nih.gov/snp/",info[1],"' target='_blank'>",info[1],"</a>")),
						subtitle=paste0("... and ",as.numeric(info[3]) - 1," more report the most extreme iHS value: ", round(as.numeric(info[2]),2),
							ifelse((tresh_ihs!=0 & abs(as.numeric(info[2])) >= tresh_ihs),
								paste0(" and belongs to the top ", input$pval_ihs*100, "%"),"")),
						icon = icon("stream"),
						color = "yellow")
				}
			}
			})
	output$info_nsl <- renderInfoBox({
		tresh_nsl   = set_threshold_report()[2]
		info <- as.vector(unlist(getInfoBoxData('nsl', set_infoBoxes())))
		
		if(is.null(info)){
			infoBox(
				title    = HTML('Selection: <b><span style="text-transform:lowercase">n</span><span style="text-transform:uppercase">SL</span></b>'),
				subtitle="for nSL in this region",
				value="No data",
				icon = icon("stream"),
				color = "red"
				)
			} else {
				if(as.numeric(info[3])==1){
					infoBox(
						title    = HTML('Selection: <b><span style="text-transform:lowercase">n</span><span style="text-transform:uppercase">SL</span></b>'),
						value=HTML(paste0("<a href='https://www.ncbi.nlm.nih.gov/snp/",info[1],"' target='_blank'>",info[1],"</a>")),
						subtitle=paste0("reports the most extreme nSL value: ", round(as.numeric(info[2]),2),
							ifelse((tresh_nsl!=0 & abs(as.numeric(info[2])) >= tresh_nsl),
								paste0(" and belongs to the top ", input$pval_nsl*100,"%"),"")),
						icon = icon("stream"),
						color = "red"
						)
					} else{
						infoBox(
							title    = HTML('Selection: <b><span style="text-transform:lowercase">n</span><span style="text-transform:uppercase">SL</span></b>'),
							value=HTML(paste0("<a href='https://www.ncbi.nlm.nih.gov/snp/",info[1],"' target='_blank'>",info[1],"</a>")),
							subtitle=paste0("... and ",as.numeric(info[3]) - 1," more report the most extreme nSL value: ", round(as.numeric(info[2]),2),
								ifelse((tresh_nsl!=0 & abs(as.numeric(info[2])) >= tresh_nsl),
									paste0(" and belong to the top ", input$pval_nsl*100,"%"),"")
								),
							icon = icon("stream"),
							color = "red"
							)
					}
				}
				})
	output$info_snpeff <- renderInfoBox({
		info <- as.vector(unlist(getInfoBoxData('snpeff', set_infoBoxes())))
		if(is.null(info)){
			infoBox(
				title    = HTML('Functional: <b>S<span style="text-transform:lowercase">np</span><span style="text-transform:uppercase">EFF</span></b>'),
				subtitle="for SnpEFF in this region",
				value="No data",
				icon = icon("layer-group"),
				color = "purple")
			} else {
				if(as.numeric(info[3])==1){
					infoBox(
						title    = HTML('Functional: <b>S<span style="text-transform:lowercase">np</span><span style="text-transform:uppercase">EFF</span></b>'),
						value=HTML(paste0("<a href='https://www.ncbi.nlm.nih.gov/snp/",info[1],"' target='_blank'>",info[1],"</a>")),
						subtitle=paste0("reports the most strong impact: ", str_to_title(gsub("_"," ",info[2]))),
						icon = icon("layer-group"),
						color = "purple")
					} else{
						infoBox(
							title    = HTML('Functional: <b>S<span style="text-transform:lowercase">np</span><span style="text-transform:uppercase">EFF</span></b>'),
							value=HTML(paste0("<a href='https://www.ncbi.nlm.nih.gov/snp/",info[1],"' target='_blank'>",info[1],"</a>")),
							subtitle=paste0("... and ",as.numeric(info[3]) - 1," other report the most strong impact: ", str_to_title(gsub("_"," ",info[2]))),
							icon = icon("layer-group"),
							color = "purple")
					}
				}
				})
	output$info_ReguDB <- renderInfoBox({
		
		info <- as.vector(unlist(getInfoBoxData('rgd', set_infoBoxes())))

		if(is.null(info)){
			infoBox(
				title    = HTML('Functional: <b>R<span style="text-transform:lowercase">egulome</span><span style="text-transform:uppercase">DB</span></b>'),
				subtitle="for RegulomeDB in this region",
				value="No data",
				icon("layer-group"),
				color = "navy")
			} else {
				if(as.numeric(info[3])==1){
					infoBox(
						title    = HTML('Functional: <b>R<span style="text-transform:lowercase">egulome</span><span style="text-transform:uppercase">DB</span></b>'),
						value=HTML(paste0("<a href='https://regulomedb.org/regulome-summary/?regions=",info[1],"' target='_blank'>",info[1],"</a>")),
						subtitle=paste0("reports the highest RegulomeDB score: ", info[2]),
						icon("layer-group"),
						color = "navy")
					} else{
						infoBox(
							title    = HTML('Functional: <b>R<span style="text-transform:lowercase">egulome</span><span style="text-transform:uppercase">DB</span></b>'),
							# value=HTML(paste0("<a href='https://www.ncbi.nlm.nih.gov/snp/",info[1],")),
							value=HTML(paste0("<a href='https://regulomedb.org/regulome-summary/?regions=",info[4],"&genome=GRCh37&maf=0.01' target='_blank'>",info[1],"</a>")),
							subtitle=paste0("... and ",as.numeric(info[3]) - 1," other report the highest RegulomeDB score:  ", info[2]),
							icon("layer-group"),
							color = "navy")
					}
				}
				})
	output$info_ClinVar <- renderInfoBox({

		info <- as.vector(unlist(getInfoBoxData('clinvar', set_infoBoxes())))

		if(is.null(info)){
			infoBox(
				title    = HTML('Functional: <b>C<span style="text-transform:lowercase">lin</span>V<span style="text-transform:lowercase">ar</span></b>'),
				subtitle="for ClinVar in this region",
				value="No data",
				icon = icon("layer-group"),
				color = "teal")
			} else {
				if(as.numeric(info[3])==1){
					infoBox(
						title    = HTML('Functional: <b>C<span style="text-transform:lowercase">lin</span>V<span style="text-transform:lowercase">ar</span></b>'),
						value=HTML(paste0("<a href='https://www.ncbi.nlm.nih.gov/snp/",info[1],"' target='_blank'>",info[1],"</a>")),
						subtitle=paste0("reports the interpretation: ", str_to_title(gsub("_"," ",info[2]))),
						icon = icon("layer-group"),
						color = "teal")
					} else{
						infoBox(
							title    = HTML('Functional: <b>C<span style="text-transform:lowercase">lin</span>V<span style="text-transform:lowercase">ar</span></b>'),
							value=HTML(paste0("<a href='https://www.ncbi.nlm.nih.gov/snp/",info[1],"' target='_blank'>",info[1],"</a>")),
							subtitle=paste0("... and ",as.numeric(info[3]) - 1," other report the interpretation: ", str_to_title(gsub("_"," ",info[2]))),
							icon = icon("layer-group"),
							color = "teal")
					}
				}
				})
	output$info_DGN <- renderInfoBox({
		
		info <- as.vector(unlist(getInfoBoxData('dgn', set_infoBoxes())))
		
		if(is.null(info)){
			infoBox(
				title    = HTML('Functional: <b>D<span style="text-transform:lowercase">is</span>G<span style="text-transform:lowercase">e</span>NET</b>'),
				subtitle="for DisGeNET in this region",
				value="No data",
				icon("layer-group"),
				color = "green"
				)
			} else {
				if(as.numeric(info[3])==1){
					infoBox(
						title    = HTML('Functional: <b>D<span style="text-transform:lowercase">is</span>G<span style="text-transform:lowercase">e</span>NET</b>'),
						value=HTML(paste0("<a href='https://www.disgenet.org/browser/2/1/0/",info[1],"/' target='_blank'>",info[1],"</a>")),
						subtitle=paste0("reports the highest number of associated PubmedIDs: ", info[2]),
						icon("layer-group"),
						color = "green")
					} else{
						infoBox(
							title    = HTML('Functional: <b>D<span style="text-transform:lowercase">is</span>G<span style="text-transform:lowercase">e</span>NET</b>'),
							value=HTML(paste0("<a href='https://www.disgenet.org/browser/2/1/0/",info[1],"/' target='_blank'>",info[1],"</a>")),
							subtitle=paste0("... and ",as.numeric(info[3]) - 1," other report the highest number of associated PubmedIDs: ", info[2]),
							icon("layer-group"),
							color = "green")
					}
				}
				})
	output$info_GWAScat <- renderInfoBox({
		
		info <- as.vector(unlist(getInfoBoxData('gwas', set_infoBoxes())))
		
		if(is.null(info)){
			infoBox(
				title    = HTML('Functional: <b>GWAS C<span style="text-transform:lowercase">atalog</span></b>'),
				subtitle="for GWAS Catalog in this region",
				value="No data",
				icon("layer-group"),
				color = "maroon")
			} else {
				if(as.numeric(info[3])==1){
					infoBox(
						title    = HTML('Functional: <b>GWAS C<span style="text-transform:lowercase">atalog</span></b>'),
						value=HTML(paste0("<a href='https://www.ebi.ac.uk/gwas/variants/",info[1],"' target='_blank'>",info[1],"</a>")),
						subtitle=paste0("reports the highest number of hits: ", info[2]),
						icon("layer-group"),
						color = "maroon")
					} else{
						infoBox(
							title    = HTML('Functional: <b>GWAS C<span style="text-transform:lowercase">atalog</span></b>'),
							value=HTML(paste0("<a href='https://www.ebi.ac.uk/gwas/variants/",info[1],"' target='_blank'>",info[1],"</a>")),
							subtitle=paste0("... and ",as.numeric(info[3]) - 1," other report the highest number of hits: ", info[2]),
							icon("layer-group"),
							color = "maroon")
					}
				}
				})
	
	output$DescriptionregionTitle <- renderText({
		paste0("<h2>region (GRCh37/hg19): <b>chr",chrSelect(),":",
			regionSelect$min,"-",regionSelect$max,'</b></h2>')
		})

	############ DOWNLOAD TAB
	output$show_bt_download <- renderUI({
		toShow <- set_download()[[1]] %>% dplyr::select(input$headersDownload) 
		if((ncol(toShow)>=1) & (nrow(toShow)!=0)){
			downloadButton(
				outputId = "bt_download",
				label    = "Download",
				icon     = icon("download"),
				style="width:100%;
				border: auto;
				color: white;
				background-color: #219086;
				padding: 15px 32px;
				text-align: center;
				display: inline-block;
				font-size: 18px;
				margin: 10px 5px;
				cursor: pointer;
				border-radius: 5px;"
				)
		}
		})
	output$bt_download <- downloadHandler(
		filename = function() {
			paste0("pophumanvar_", chrSelect(), "_", regionSelect$min, "_",  regionSelect$max, ".csv", sep = "")
			},
			content = function(file) {
				fwrite(set_download()[[1]] %>% dplyr::select(input$headersDownload), file)
			}
			)	
	# Download
	output$dtDownload = renderDT(server=TRUE,{
		
		data = set_download()[[1]] %>% dplyr::select(input$headersDownload) 

		datatable(
			data,
			style = 'bootstrap',
			# class = 'thead-dark',
			selection = 'none',
			filter = 'top',
			extensions = 'Buttons', 
			options = list(
				pageLength = 25,	
				scrollX=TRUE
				)
			)
		})

	########BATCH
	# Check controls
	checkStatus <- reactiveValues(data = NULL, show = "error_")

	# If input, check format (true or "error_*")
	observe({
		req(input$batch_regions)
		checkStatus$show <- checkRegionsList(input$batch_regions)
		})

  	# If format correct, read file and add as data, else, reset
  	observeEvent(checkStatus$show,{
  		if(isTRUE(checkStatus$show)){
  			checkStatus$data <- TRUE
  			}else{
  				click("dl_reset")
  			}
  			})

  	### If everything okey: #########
  	# Generate dataset to show
  	set_download_batch <- reactive({
  		req(input$batch_regions)
  		if(isTRUE(checkStatus$data) & isTRUE(checkStatus$show)){
  			df = fread(input$batch_regions$datapath)
  			out = list()
  			for(i in 1:nrow(df)){
  				out[[paste0(i)]] = cbind(nchr = df$V1[i], mergeAllDF(local(df$V1[i]), local(df$V2[i]), local(df$V3[i])) %>% as.data.table)
  			}
  			dfMerge = rbindlist(out)
  			dfMerge
  		}
  		})
  	# Show download button
  	output$show_download <- renderUI({
  		if(isTRUE(checkStatus$show)){
  			downloadButton(
  				outputId = "bt_download_batch",
  				label    = "Download",
  				icon     = icon("download"),
  				style="width:20%;
  				border: auto;
  				color: white;
  				background-color: #219086;
  				padding: 10px 22px;
  				text-align: center;
  				display: inline-block;
  				font-size: 14px;
  				margin: 10px 5px;
  				cursor: pointer;
  				border-radius: 5px;")
  		}
  		})
  	# Decide whether to shot the table or not
  	output$showTable <- reactive({
    	# If check is positive, show filtering panel
    	if(isTRUE(checkStatus$show)){
    		out <- TRUE
    		}else{
    			out <- FALSE}
    			out
    			})
  	# Show the table
  	outputOptions(output, "showTable", suspendWhenHidden = FALSE)

	# Render batch Download
	output$batch_download = renderDT({
		tmp = set_download_batch()
		datatable(
			tmp,
			style = 'bootstrap',
			options = list(language = list(emptyTable = 'My Custom No Data Message'),pageLength = 20, scrollX=TRUE)
			)
		})
	# Allow download
	output$bt_download_batch <- downloadHandler(
		filename = function() {
			paste0("pophumanvar_custom_batch.csv.gz", sep = "")
			},
			content = function(file) {
				fwrite(set_download_batch(), file)
			}
			)

  	### If smth wrong: #########
  	# Error alerts that will appear on the screen if the conditions are not met
  	output$hidden_alert <- renderUI({
  		if(!isTRUE(checkStatus$show)){
  			if(checkStatus$show=="error_empty"){
  				sendSweetAlert(
  					session = session,
  					title = "Error!",
  					closeOnClickOutside = FALSE,
  					text = tags$span(
  						tags$h2("Your file is empty"),
  						tags$br(),
  						"Please, add one or more sets of coordinates and try again",
  						tags$br(),
  						tags$br()
  						),
  					html = TRUE,
  					type = "error")
  				} else if(checkStatus$show=="error_txt"){
  					sendSweetAlert(
  						session = session,
  						title = "Error!",
  						closeOnClickOutside = FALSE,
  						text = tags$span(
  							tags$h2("Your file is not a CSV file"),
  							tags$br(),
  							"Please, try again using a .csv/.txt file",
  							tags$br(),
  							tags$br()
  							),
  						html = TRUE,
  						type = "error")
  					} else if(checkStatus$show=="error_format"){
  						sendSweetAlert(
  							session = session,
  							title = "Error!",
  							closeOnClickOutside = FALSE,
  							text = tags$span(
  								tags$h2("Your file does not match our format requirements"),
  								tags$br(),
  								"Please, check the instructions, modify your file if convinient, and try again. It should be <#chr>,<
  								<sart>,<end>",
  								tags$br(),
  								tags$br()
  								),
  							html = TRUE,
  							type = "error")
  						} else if(checkStatus$show=="error_size"){
  							sendSweetAlert(
  								session = session,
  								title = "Error!",
  								closeOnClickOutside = FALSE,
  								text = tags$span(
  									tags$h2("The regions specified are larger than 50 Mb"),
  									tags$br(),
  									"Please, reduce the number of regions and try again.",
  									tags$br(),
  									tags$br()
  									),
  								html = TRUE,
  								type = "error")
  						}
  					}
  					})
  	observeEvent(input$dl_reset, {
  		checkStatus$data <- NULL
  		reset('batch_regions')
  		})



	# Upload-and-email workflow removed during recovery.
}
