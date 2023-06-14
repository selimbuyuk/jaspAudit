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
  	columns: 2
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
    
    Group
    {
      title: "Pick the algorithms"
      Group
      {
        visible : mltask.value == "binclass" ? true:false
        CheckBox { name: "svm"; label: qsTr("Support Vector Machines"); checked: true}
        CheckBox { name: "rf";	label: qsTr("Random Forest"); }
        CheckBox { name: "boost";	label: qsTr("Boosting") ; }
      }
      
      Group
      {
        visible : mltask.value == "fairclass" ? true:false
        CheckBox { name: "fxgb";   label: qsTr("FairXGBoost"); checked: true }
        CheckBox { name: "fada";	label: qsTr("Fair-AdaBoost")}
        CheckBox { name: "fgbm";	label: qsTr("FairBGM")}
      }
      
      Group
      {
        visible : mltask.value == "regression" ? true:false
        CheckBox { name: "sreg";   label: qsTr("Simple Regression"); checked: true }
        CheckBox { name: "lr";	label: qsTr("Logistic Regression")}
      }
    }
    
    Group
    {
      IntegerField
      {
        name: "kmodels"
        label: qsTr("Generate K models")
        defaultValue: 3
        fieldWidth: 60
        min: 1
        max: 5
      }
    }
    
	}
  Section{
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
			title: 						qsTr("Group")
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
			id:									features
			name:								"features"
			title:								qsTr("Features")
			allowedColumns:	["scale", "ordinal", "nominal", "nominalText"]
			allowAnalysisOwnComputedColumns:	false
		}

	}
	
Group{
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

			columnNames			: qsTr("")
			cornerText			: qsTr("Subgroup")
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
  		helpPage:			"auditFairness"
  		Layout.columnSpan:	1
  	}
  	
    RadioButtonGroup
    {
    name: "q1"
    id: "q1"
    title: qsTr("Should the ground truth labels be considered?")
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
    title: q1option1.checked ? qsTr("Should all elements of the confusion matrix be considered?") : qsTr("Should the absolute values or the proportions of the favorable predictions be used?")
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
    title: qsTr("Should we focus on correctly or incorrectly classified instances?")
    RadioButton{id:"q3option1"; label: qsTr("Correctly"); name:"q3option1"; value:(g3.visible) ? "corr": "";  checked: true}
    RadioButton{id:"q3option2"; label: qsTr("Incorrectly"); name:"q3option2"; value:(g3.visible) ? "incorr": "";  checked: true}
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
    title: q3option1.checked ? qsTr("Should we focus on true positive or true negative rates?") : qsTr("Should we focus on false positive or false negative rates?")
    RadioButton{id:"q4option1"; label: q3option1.checked ? qsTr("True Positives"): qsTr("False Positives"); name:"q3option1"; value: (g3.visible) ? q3option1.checked ? "tp" : "fp": ""; checked: true}
    RadioButton{id:"q4option2"; label: q3option1.checked ? qsTr("True Negatives"): qsTr("False Negatives"); name:"q3option2"; value: (g3.visible) ? q3option1.checked ? "tn" : "fn": "";  checked: true}
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



