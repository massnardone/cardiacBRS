# **README -- cBRS App**

This app analyzes Cardiac Baroreflex Sensitivity using both time and frequency domain analyses. Functionality of the app requires the helper function for calculations, so ensure you are using the Rproject file for proper source location.

<br>

### **LABCHART ANALYSIS**

-   Ensure the LabChart file is in second and not in minutes.
-   Setup / Display Settings / Check: Always seconds & Uncheck: Display date

<br>

#### *Step 1*

Set up the DataPad. Three columns are required: 1. Statistics / Time at Maximum Calculate from source channel: Finometer 2. Selection & Active Point / Time 3. Statistics / Maximum Value Calculate from source channel: Finometer

![alt text]("https://github.com/massnardone/cardiacBRS/Pictures/Picture 1 - LabChart Data Pad Setup.png")

<br>

#### *Step 2*

-   Highlight data of interest and run the ECG analysis to flag all R intervals. Review carefully (green dots on R waves) to ensure accuracy.
-   ECG Analysis / Settings\
-   Data source: ECG Check: Selection
-   OK

![alt text]("https://github.com/massnardone/cardiacBRS/Pictures/Picture 2 - ECG Analysis.png")

<br>

#### *Step 3*

-   Ensure data of interest is still highlighted. Click: Multiple Add to Datapad.
-   Using the "Event Markers" from the ECG channel, for 0.5s after event marker, through the current selection of data
-   **NOTE**: the 0.5 seconds is arbitrary and dependent on the participant's heart rate. If heart rates begin to exceed 100, may want to lower the number to 0.3 or 0.4 seconds

![alt text]("https://github.com/massnardone/cardiacBRS/Pictures/Picture 3 - Multiple Add to Data Pad.png")

<br>

#### *Step 4*

Copy data in the DataPad to a separate Excel sheet.

<br> <br>

### **EXCEL SETUP**

<br>

#### *Step 1*

You need 4 columns in the Excel sheet. The names and positioning of these columns need to be EXACT. Below is a schematic to show the organization of the excel sheet in relationship to the ECG and Finometer signal in LabChart.

<br>

**4 columns**\
Column 1: SBP_Time\
Column 2: R_Time\
Column 3: SBP\
Column 4: RR

![alt text]("https://github.com/massnardone/cardiacBRS/Pictures/Picture 4 - ECG and BP Alignment.png")

<br>

#### *Step 2*

-   Calculate RR as shown in picture.

![alt text]("https://github.com/massnardone/cardiacBRS/Pictures/Picture 5 - Excel Setup.png")

-   **NOTE:** The last row in column D (RR) should be empty
-   **NOTE:** The R script can be finicky when reading this Excel. Before closing, click on column E, highlight columns until infinity (Ctrl + Shift + →), and delete. Do the same thing for rows.

<br>

#### **OPEN R**

-   Open cBRS R Project file. Then, open Cardiac Baroreflex Sensitivity Script.
-   Click "Run App"
-   Browse your computer for the input file and run analysis!

![alt text]("https://github.com/massnardone/cardiacBRS/Pictures/Picture 6 - R.png")

<br> <br>

### **INFOMRATION ON ANALYSES**

<br>

#### *TIME DOMAIN ANALSIS*

This analysis computes the sequences of RR interval and SBP rises or falls of 3 or more cardiac cycles. If the next value is greater or lower than the current value, this program will flag it as either UP or DOWN respectively. It also finds the slopes of these Sequences and will print the mean.

<br>

#### *FREQUENCY DOMAIN ANALYSIS*

This analysis computes the transfer function of SBP as the input variable and RR as the output variable. Spectral power of SBP and RR are computed along with their respective signal gain and coherence.

### **Link to Relevant Papers**

[1. Persson et al. 2001. Time versus frequency domain techniques for assessing baroreflex sensitivity. Journal of Hypertension](https://journals.lww.com/jhypertension/Fulltext/2001/10000/Time_versus_frequency_domain_techniques_for.1.aspx?casa_token=EhhrI1I20o4AAAAA:9VxbF2BdMAkJ9GOWymoaU7NpDaMM3mjyitfQQUwHOcAMXfTC8ms5DE5lAHrkCGqRt3hLqGqpOCMPcKOV1VIyhFk)

[2. La Rovere et al. 2008.Baroreflex Sensitivity: Measurement and Clinical Implications. Annals of Noninvasive Electrocardiology](https://onlinelibrary.wiley.com/doi/10.1111/j.1542-474X.2008.00219.x)

[3.Parati et al. 2000. How to measure baroreflex sensitivity. From the cardiovascular laboratory to daily life. Journal of Hypertension](https://journals.lww.com/jhypertension/Fulltext/2000/18010/How_to_measure_baroreflex_sensitivity__from_the.3.aspx?casa_token=X_SgZCooRmUAAAAA:4d5DTHrjQ35izXTWWoQkpN24gbdojNppdS4CaVPc3QJ1MBbjkIr_02JBvHnQTIUi1QEUpGOHdrbGzzH1tYRQpOg)
