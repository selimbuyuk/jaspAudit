// Copyright (C) 2013-2018 University of Amsterdam
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

// When making changes to this file always mention @koenderks as a
// reviewer in the Pull Request

import QtQuick									2.8
import QtQuick.Layouts							1.3
import JASP.Controls							1.0
import JASP.Widgets								1.0
import JASP										1.0

import "./common"	as Common


Form {
	columns:							2
	
	

  	DropDown
    {
      id: "mltask"
    	name:								"mltask"
    	indexDefaultValue:					0
    	label:								qsTr("Machine Learning Type")
    	values:
    		[
    		{ label: qsTr("Binary Classification"), 			value: "binclass",id: "binclass"},
    		{ label: qsTr("Regression"), 			value: "regression", id: "regression"},
    	]
    }
    
    Group{}
    
		Group
		{
		RadioButtonGroup{
			name:									"predictBool"
			title:									qsTr("Generate predictions or use own?")

      RadioButton{label:"Use own predictions"; id:"ownPrediction"; value:"ownPrediction" }
			RadioButton{label:"Generate predictions";id:"genPredictions"; value:"genPredictions"     ;
			    			Group
          {
            title: "Pick the algorithms"
            visible : mltask.value == "binclass" ? true:false
            CheckBox { name: "svm"; label: qsTr("Support Vector Machines"); checked: true}
            CheckBox { name: "rf";	label: qsTr("Random Forest"); }
            CheckBox { name: "lr";	label: qsTr("Logistic Regression") ; }
          }
          Group
          {
          title: "Pick the algorithms"
            visible : mltask.value == "regression" ? true:false
            CheckBox { name: "sreg";   label: qsTr("Simple Regression"); checked: true }
          }
          Group
          {
           // IntegerField
            //{
            //  name: "kmodels"
            //  label: qsTr("Generate K models")
            //  defaultValue: 3
            //  fieldWidth: 60
            //  min: 1
            //  max: 5
            //}
            
            DropDown
            {
              label: "Select the model based on"
              name: "perfFocus"
              values: mltask.value == "binclass" ? [{label: "Matthew's Correlation Coefficient", value: "mcc"},  {label:"F1-Score", value:"f1"}]:["RMSE", "MSE", "MAE"]
            }
          }
        }
      }
    }
    
	
  title: qsTr("Pick Variables")
	VariablesForm
	{
		id: 							variablesFairness
		preferredHeight:				jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList
		{
			name: 						"variablesFairness"
			title: "Variables"
		}

		AssignedVariablesList
		{
			id: 						group
			name: 						"group"
			title: 						qsTr("Sensitive attribute")
			singleVariable:				true
			allowedColumns:				["nominal", "nominalText"]
		}
				AssignedVariablesList
		{
			id: 						target
			name: 						"target"
			title: 						qsTr("Target")
			singleVariable:				true
			allowedColumns:				["nominal", "nominalText"]
		}
		
		AssignedVariablesList
		{
			id:									featPred
			name:								"featPred"
			singleVariable:				false//genPredictions.checked ? false:true
			implicitHeight		: genPredictions.checked ? 100 * preferencesModel.uiScale:50
			title:							genPredictions.checked ?qsTr("Features"):qsTr("Predictions")
			allowedColumns:["scale", "ordinal", "nominal", "nominalText"]
		}
	}

Group{
		Label	{ text : qsTr("Choose a reference group")	}
		TableView
		{

			property int designDataColumns : 1


			id					: selectedDesign2
			implicitWidth		: (form.implicitWidth) /2
			implicitHeight		: 150 * preferencesModel.uiScale
			source: [{name: "group", use: "levels"}]

			modelType			: JASP.Simple
			name				: "selectedDesign2"

			columnNames			: qsTr("")
			cornerText			: qsTr("Sensitive groups")
			initialColumnCount	: 1// -1 because the first "column" is not a column but the row header
			columnCount			: 1

			itemType			: JASP.Double
			rowCount			: 0
			initialRowCount		: 0

			itemDelegate: Item
			{

				Rectangle
				{

					id: backgroundRect
					color: rowIndex === tableView.rowSelected ? jaspTheme.grayLighter : jaspTheme.white
					anchors
					{
						fill:			parent
						topMargin:		-selectedDesign2.view.itemVerticalPadding
						bottomMargin:	-selectedDesign2.view.itemVerticalPadding
					}

					MouseArea
					{
						anchors.fill: parent
						onClicked:
						{
							tableView.colSelected = columnIndex
							tableView.rowSelected = rowIndex
						}
					}
				}

				Label
				{
					text						: tableView.getDefaultValue(columnIndex, rowIndex)
					anchors.verticalCenter		: parent.verticalCenter
					anchors.horizontalCenter	: parent.horizontalCenter
					onTextChanged:
					{
						selectedDesign2.itemChanged(columnIndex, rowIndex, value, inputType)
					}
				}
			}

			rowNumberDelegate: Rectangle
			{
				// identical to default but with changed colors
				color: rowIndex === tableView.rowSelected ? jaspTheme.grayLighter : jaspTheme.white// : jaspTheme.analysisBackgroundColor
				Text
				{
					text:					tableView.getRowHeaderText(headerText, rowIndex);
					color:					jaspTheme.textEnabled
					anchors.centerIn:		parent;
					horizontalAlignment:	Text.AlignHCenter
					verticalAlignment:		Text.AlignVCenter
					leftPadding:			3 * preferencesModel.uiScale
					elide:					Text.ElideRight;
					width:					parent.width
					height:					parent.width
					font:					jaspTheme.font
				}

				MouseArea
				{
					anchors.fill: parent
					onClicked:
					{
						if (tableView.rowSelected === rowIndex)
							rowIndex = -1
						tableView.rowSelected = rowIndex;
					}
				}
			}

			columnHeaderDelegate : Rectangle
			{
				// identical to the default definition in TableView, but this does not change color when the column is selected
				color: jaspTheme.analysisBackgroundColor
				Text { text: tableView.getColHeaderText(headerText, columnIndex); anchors.centerIn: parent; font: jaspTheme.font; color:	jaspTheme.textEnabled }
				MouseArea
				{
					anchors.fill: parent
					onClicked:
					{
						if (tableView.colSelected === columnIndex)
							columnIndex = -1
						tableView.colSelected = columnIndex;
					}
				}
			}


		}
	CIField { name: "fthreshold"; label: qsTr("Fairness threshold");defaultValue : 80 }

		IntegerField { name: "selectedRow"; label: qsTr("debug selected row"); defaultValue: selectedDesign2.rowSelected; negativeValues: true; visible: false }
		

	}


  	


	Item
	{
		Layout.preferredHeight:			download.height
		Layout.preferredWidth: 			parent.width
		Layout.columnSpan:				2

		Button
		{
			id:							download
			enabled: 					values.count > 0
			anchors.right:				parent.right
			text:						qsTr("<b>Download Report</b>")
			onClicked:					form.exportResults()
		}
	}
 

 Section
 {
  columns: 1 
  title: qsTr("Fairness Criteria")
  
  //Zet dit bij de report section!!!
  

    
  Group
  {
    HelpButton
  	{
  		toolTip:			qsTr("Click to learn more about Q1.")
  		helpPage:			"auditFairness"
  		Layout.columnSpan:	1
  	}
  	
    RadioButtonGroup
    {
    name: "q1"
    id: "q1"
    title: qsTr("Should we focus on a single class?")
    RadioButton{id:"q1option1"; label: qsTr("Yes"); name:"q1option1"; value:"yes";  checked: true}
    RadioButton{id:"q1option2"; label: qsTr("No"); value:"no"}
    }
   }
  
  Group
  {
  
    HelpButton
  	{
  		toolTip:			qsTr("Click to learn more about Q2.")
  		helpPage:			"auditQ2Helper"
  		Layout.columnSpan:	1
  	}

    RadioButtonGroup
    {
    name: "q2"
    id: "q2"
    title: q1option1.checked ? qsTr("Should we focus on correctly or incorrectly classified instances?") : qsTr("Should all elements of the confusion matrix be considered?")
    RadioButton{id:"q2option1"; label: q1option1.checked ? qsTr("Correctly"): qsTr("Yes"); name:"q2option1"; value: q1option1.checked ? "corr": "yes";  checked: true}
    RadioButton{id:"q2option2"; label: q1option1.checked ? qsTr("Incorrectly"): qsTr("No"); name:"q2option2"; value: q1option1.checked ? "incorr": "no";  checked: true}
    }
   }
   
  Group
  {
    id: "g3" 
    visible: (q1option1.checked & q2option2.checked) | (q1option2.checked & q2option1.checked)? false: true
    HelpButton
  	{
  		toolTip:			qsTr("Click to learn more about Q3.")
  		helpPage:			"auditQ2Helper"
  		Layout.columnSpan:	1
  	}

    RadioButtonGroup
    {
    name: "q3"
    id: "q3"
    title: q2option1.value == "corr" ? qsTr("Should we focus on true positives or true negative rates?"):  qsTr("Should the ground truth labels be considered?")
    RadioButton{id:"q3option1"; label: q2option1.checked ? qsTr("TP"): qsTr("Yes"); name:"q3option1"; value:
    g3.visible ? (q2option1.checked ? "tp": "yes"): "";  checked: true}
    RadioButton{id:"q3option2"; label: q2option1.checked ? qsTr("TN"): qsTr("No"); name:"q3option2"; value:
    g3.visible ? (q2option1.checked ? "tn": "no"): ""; checked: true}
    }
   }
  }
  
  Section
  {
    title: qsTr("Data Split Preferences")
    
    RadioButtonGroup
    {
      
		title: 									qsTr("Holdout Test Data")
		id:                     holdoutData
		name: 									"holdoutData"

		RadioButton
		{
			id:									holdoutManual
			name:								"holdoutManual"
			childrenOnSameRow:					true
			text:								qsTr("Sample")

			Row
			{
				PercentField
				{
					name:						"testDataManual"
					defaultValue:				20
					min:						5
					max:						95
					afterLabel:					qsTr("% of all data")
				}
			}
		}

		CheckBox
		{
			id:									addIndicator
			name:								"addIndicator"
			text:								qsTr("Add generated indicator to data")
			Layout.leftMargin:					20
			enabled:							holdoutManual.checked

			ComputedColumnField
			{
				name:							"testIndicatorColumn"
				text:							qsTr("Column name")
				fieldWidth:						120
				visible:						addIndicator.checked
			}
		}

		RadioButton
		{
			name: 								"testSetIndicator"
			label: 								qsTr("Test set indicator")
			childrenOnSameRow: 					true

			DropDown
			{
				id: 							testSetIndicatorVariable
				name: 							"testSetIndicatorVariable"
				showVariableTypeIcon: 			true
				addEmptyValue: 					true
				placeholderText: 				qsTr("None")
			}
		}
	
    }
  }
  
  Section
  {
    title:						qsTr("Training Parameters")
    visible: genPredictions.checked ? true:false

		Group
		{
		//this does not work, seems that checkboxes in JASP dont have signals 
	   enabled: rf.checked ? true:false
			title:								qsTr("Algorithmic Settings RF")

			PercentField
			{
				name:							"baggingFraction"
				text:							qsTr("Training data used per tree")
				defaultValue:					50
				min:							5
				max:							95
			}

			RowLayout
			{
				DropDown
				{
					id:							noOfPredictors
					name:						"noOfPredictors"
					indexDefaultValue:			0
					label:						qsTr("Features per split")
					values:
						[
						{ label: qsTr("Auto"), 		value: "auto"},
						{ label: qsTr("Manual"), 	value: "manual"}
					]
				}

				IntegerField
				{
					name:						"numberOfPredictors"
					defaultValue:				1
					min:						0
					max:						10000
					visible:					noOfPredictors.currentIndex == 1
				}
			}

			CheckBox
			{
				text:							qsTr("Scale features")
				name:							"scaleVariablesRF"
				checked:						true
			}

			CheckBox
			{
				name:							"setSeed"
				text:							qsTr("Set seed")
				childrenOnSameRow:				true

				IntegerField
				{
					name:						"seed"
					defaultValue:				1
					min:						-999999
					max:						999999
					fieldWidth:					60
				}
			}

		RadioButtonGroup
		{
			title:								qsTr("Number of Trees")
			name:								"modelOptimization"

			RadioButton
			{
				text:							qsTr("Fixed")
				name:							"manual"

				IntegerField
				{
					name:						"noOfTrees"
					text:						qsTr("Trees")
					defaultValue:				100
					min:						1
					max:						500000
					fieldWidth:					60
				}
			}

			RadioButton
			{
				id:								optimizeModel
				text:							qsTr("Optimized")
				name:							"optimized"
				checked:						true

				IntegerField
				{
					name:						"maxTrees"
					text:						qsTr("Max. trees")
					defaultValue:				100
					min:						1
					max:						500000
					fieldWidth:					60
				}
			}
		}
	}
	
		Group{
			title:								qsTr("Algorithmic Settings SVM")

			DropDown
			{
				id:								weights
				name:							"weights"
				indexDefaultValue:				0
				label:							qsTr("Weights")
				values:
					[
					{ label: qsTr("Linear"),	value: "linear"},
					{ label: qsTr("Radial"),	value: "radial"},
					{ label: qsTr("Polynomial"),value: "polynomial"},
					{ label: qsTr("Sigmoid"),	value: "sigmoid"}
				]
			}

			DoubleField
			{
				name:							"degree"
				text:							qsTr("Degree")
				defaultValue:					3
				min:							1
				enabled:						weights.value == "polynomial"
				Layout.leftMargin:				10 * preferencesModel.uiScale
			}

			DoubleField
			{
				name:							"gamma"
				text:							qsTr("Gamma parameter")
				defaultValue:					1
				min:							0
				enabled:						weights.value != "linear"
				Layout.leftMargin:				10 * preferencesModel.uiScale
			}

			DoubleField
			{
				name:							"complexityParameter"
				text:							qsTr("r parameter")
				defaultValue:					0
				min:							0
				enabled:						weights.value == "polynomial" | weights.value == "sigmoid"
				Layout.leftMargin:				10 * preferencesModel.uiScale
			}

			DoubleField
			{
				name:							"cost"
				text:							qsTr("Cost of constraints violation")
				defaultValue:					1
				min:							0.001
			}

			DoubleField
			{
				name:							"tolerance"
				text:							qsTr("Tolerance of termination criterion")
				defaultValue:					0.001
				min:							0.001
			}

			DoubleField
			{
				name:							"epsilon"
				text:							qsTr("Epsilon")
				defaultValue:					0.01
				min:							0.001
			}

			CheckBox
			{
				text:							qsTr("Scale features")
				name:							"scaleVariablesSVM"
				checked:						true
			}

			CheckBox
			{
				name:							"setSeedSVM"
				text:							qsTr("Set seed")
				childrenOnSameRow:				true

				IntegerField
				{
					name:						"seedSVM"
					defaultValue:				1
					min:						-999999
					max:						999999
					fieldWidth:					60
				}
			}
		
	}
	
	
	Group
	{
	
			title:								qsTr("Algorithmic Settings Boosting")

			DoubleField
			{
				name:							"shrinkage"
				text:							qsTr("Shrinkage")
				defaultValue:					0.1
				min:							0
				max:							1
			}

			IntegerField
			{
				name:							"interactionDepth"
				text:							qsTr("Interaction depth")
				defaultValue:					1
				min:							1
				max:							99
			}

			IntegerField
			{
				name:							"minObservationsInNode"
				text:							qsTr("Min. observations in node")
				defaultValue:					10
				min:							1
				max:							50000
			}

			PercentField
			{
				name:							"baggingFractionBoost"
				text:							qsTr("Training data used per tree")
				defaultValue:					50
			}

			CheckBox
			{
				text:							qsTr("Scale features")
				name:							"scaleVariablesBoost"
				checked:						true
			}

			CheckBox 
			{
				name:							"setSeedBoost"
				text:							qsTr("Set seed")
				childrenOnSameRow:				true

				IntegerField 
				{
					name:						"seedBoost"
					defaultValue:				1
					min:						-999999
					max:						999999
					fieldWidth:					60
				}
			}
		
	}
	}

Section 
{
  title :qsTr("Output Options")
	RadioButtonGroup
		{
			text:									qsTr("Grouping Options")
			name:									"enableGroup"
			RadioButton{label: "Group all fairness measures into one plot"; value: "groupPlot"; checked: true}
			RadioButton{label: "Plot measures seperately"; value: "singlePlot"}
		}
		
		Group{
	CheckBox
		{
			text:									qsTr("Highlight threshhold range in plots")
			name:									"enableThreshold"
			checked: true
		}
		
	CheckBox
		{
		  enabled: genPredictions.checked ? true: false
			text:									qsTr("Show all performance metrics")
			name:									"performanceMeasuresAll"
		}
}
}
}


//verbinden met R moet makkelijker zijn, anders hardcoden
//mss dictionary



