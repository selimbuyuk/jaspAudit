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
	
	
	
	Section{
  	title: qsTr("Machine Learning")
  	columns: 1
  	DropDown
    {
      id: "mltask"
    	name:								"mltask"
    	indexDefaultValue:					0
    	label:								qsTr("Machine Learning Type")
    	values:
    		[
    		{ label: qsTr("Regular Binary Classification"), 			value: "binclass",id: "binclass"},
    		{label: qsTr("Fairness-Aware Binary Classification"), 			value: "fairclass",id: "binclass"},    		{ label: qsTr("Regression"), 			value: "regression", id: "regression"},
    	]
    }
    
    DropDown
    {
      visible: true
    	name:								"mlalgo"
    	indexDefaultValue:					0
    	label:	qsTr("Algorithm")
    	values: mltask.value == "binclass" ? [
    		{ label: qsTr("Support Vector Machines"), 			value: "svm"},
    		{ label: qsTr("Neural Network"), 			value: "nn"},
    		{ label: qsTr("XGBoost"), 			value: "xgb"},
    	] : (mltask.value == "fairclass" ? [
    		{ label: qsTr("FairXGBOOST"), 			value: "fxgb"},
    		{ label: qsTr("Fair-AdaBoost"), 			value: "fada"},
    		{ label: qsTr("FairGBM"), 			value: "fgbm"},
    	] : [
    		{ label: qsTr("Simple Regression"), 			value: "sreg"},
    		{ label: qsTr("Logistic Regression"), 			value: "lr"}
    	]) 
    		
    }
    
  VariablesForm
	{
		AvailableVariablesList
		{
			name:								"variables"
		}

		AssignedVariablesList
		{
			id:									target
			name:								"target"
			title:								qsTr("Target")
			singleVariable:						true
			allowedColumns:						["ordinal", "nominal", "nominalText"]
		}

		AssignedVariablesList
		{
			id:									predictors
			name:								"predictors"
			title:								qsTr("Features")
			allowedColumns:						["scale", "ordinal", "nominal", "nominalText"]
			allowAnalysisOwnComputedColumns:	false
		}
	}

    
    
	
	}
  Section{
  title: qsTr("Pick Variables")
	VariablesForm
	{
		id: 							variablesFormBenfordsLaw
		preferredHeight:				jaspTheme.smallDefaultVariablesFormHeight

		AvailableVariablesList
		{
			name: 						"variablesFormBenfordsLaw"
		}

		AssignedVariablesList
		{
			id: 						group
			name: 						"group"
			title: 						qsTr("Group")
			singleVariable:				true
			allowedColumns:				["nominal", "nominalText"]
		}
				AssignedVariablesList
		{
			id: 						predicted
			name: 						"predicted"
			title: 						qsTr("Predicted")
			singleVariable:				true
			allowedColumns:				["nominal", "nominalText"]
		}
				AssignedVariablesList
		{
			id: 						actual
			name: 						"actual"
			title: 						qsTr("Actual")
			singleVariable:				true
			allowedColumns:				["nominal", "nominalText"]
		}

	}

	Group
	{
		Label	{ text : qsTr("Choose a Level")	}
		TableView
		{

			property int designDataColumns : 1


			id					: selectedDesign2
			implicitWidth		: (form.implicitWidth) /2
			implicitHeight		: 150 * preferencesModel.uiScale
			source: [{name: "group", use: "levels"}]

			modelType			: JASP.Simple
			name				: "selectedDesign2"

			columnNames			: qsTr("Counts")
			cornerText			: qsTr("Level")
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
 }

 Section
 {
  columns: 1 
  title: qsTr("Fairness Measures")
    
  Group
  {
    HelpButton
  	{
  		toolTip:			qsTr("Click to learn more about Q1.")
  		helpPage:			"auditQ2Helper"
  		Layout.columnSpan:	1
  	}
  	
    RadioButtonGroup
    {
    name: "q1"
    id: "q1"
    title: qsTr("Should the truth abels be used?")
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
    title: q1option1.checked ? qsTr("Should all classes of the confusion matrix be used?") : qsTr("Should absolute values or proportions be used?")
    RadioButton{id:"q2option1"; label: q1option1.checked ? qsTr("Yes"): qsTr("Absolute"); name:"q2option1"; value:q1option1.checked ? "yes": "abs";  checked: true}
    RadioButton{id:"q2option2"; label: q1option1.checked ? qsTr("No"): qsTr("Proportional"); name:"q2option2"; value:q1option1.checked ? "no": "prop";  checked: true}
    }
   }
   
  Group
  {
    id: "g3"
    visible: (q1option1.checked && q2option2.value == "no" && q2option2.checked) ? true: false
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
    title: qsTr("Should we focus on correctly or incorrectly classified classes?")
    RadioButton{id:"q3option1"; label: qsTr("Correctly"); name:"q3option1"; value:"corr";  checked: true}
    RadioButton{id:"q3option2"; label: qsTr("Incorrectly"); name:"q3option2"; value:"incorr";  checked: true}
    }
   }
   
  Group
  {
    visible: (g3.visible) ? true: false
    HelpButton
  	{
  		toolTip:			qsTr("Click to learn more about Q4.")
  		helpPage:			"auditQ2Helper"
  		Layout.columnSpan:	1
  	}

    RadioButtonGroup
    {
    name: "q4"
    id: "q4"
    title: q3option1.checked ? qsTr("Should we focus on true positive or true negative rates") : qsTr("Should we focus on false positive or false negative rates?")
    RadioButton{id:"q4option1"; label: q3option1.checked ? qsTr("True Positives"): qsTr("False Positives"); name:"q3option1"; value: q3option1.checked ? "tp" : "fp";  checked: true}
    RadioButton{id:"q4option2"; label: q3option1.checked ? qsTr("True Negatives"): qsTr("False Negatives"); name:"q3option2"; value: q3option1.checked ? "fp" : "fn";  checked: true}
    }
   }
    
  }

  Section {
  title: qsTr("Performance Measures")

  		CheckBox
		{
			text:									qsTr("Enable Performance Metrics Group")
			name:									"performanceMeasuresGroup"
		}

		CheckBox
		{
			text:									qsTr("Enable Performance Metrics All")
			name:									"performanceMeasuresAll"
		}

  }
}


//verbinden met R moet makkelijker zijn, anders hardcoden
//mss dictionary



