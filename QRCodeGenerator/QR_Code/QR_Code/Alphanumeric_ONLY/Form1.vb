Imports System
Public Class Form1

    Public Class CurrentModule

        Public Function IsBlack(ByVal matrix_copy(,) As Integer, ByVal x_coord As Integer, ByVal y_coord As Integer) As Boolean
            If (matrix_copy(x_coord, y_coord) = 1) Or
               (matrix_copy(x_coord, y_coord) = 2) Or
               (matrix_copy(x_coord, y_coord) = 6) Or
               (matrix_copy(x_coord, y_coord) = 8) Then
                Return True
            Else
                Return False
            End If
        End Function

        Public Function IsWhite(ByVal matrix_copy(,) As Integer, ByVal x_coord As Integer, ByVal y_coord As Integer) As Boolean
            If (matrix_copy(x_coord, y_coord) = 0) Or
               (matrix_copy(x_coord, y_coord) = 5) Or
               (matrix_copy(x_coord, y_coord) = 7) Or
               (matrix_copy(x_coord, y_coord) = 9) Then

                Return True
            Else
                Return False
            End If
        End Function



        Public Sub PaintModule(ByVal matrix(,) As Integer, ByVal x_coord As Integer, ByVal y_coord As Integer, ByRef PictureBox1 As PictureBox, ByVal Colour As Brush)
            Dim Drawing As Graphics = Graphics.FromImage(PictureBox1.Image)
            Dim height As Integer = PictureBox1.Height
            Dim partition As Integer = height / 21

            Drawing.FillRectangle(Colour, x_coord * partition, y_coord * partition, partition, partition)
            PictureBox1.Refresh()
        End Sub

    End Class


    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Me.BackColor = Color.FromArgb(204, 0, 0)
        Label25.Visible = False

    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Dim input_text As String = TextBox1.Text
        Dim length_input As Integer = TextBox1.TextLength
        Dim length_stream As Integer = 4 + 9 + (11 * (length_input \ 2)) + (6 * (length_input Mod 2))
        Dim encoding_list As String = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ $%*+-./:"
        Dim split_list As New List(Of String)
        Dim extra As String = ""
        Dim validation_return As Boolean = Validation(input_text, length_input, encoding_list)
        Dim binary_string As String
        Dim pad_codeword_1 As String = "11101100"
        Dim pad_codeword_2 As String = "00010001"
        Dim codewords_string As String
        Dim data_codewords() As String
        Dim error_codewords As New List(Of String)
        Dim final_data As New List(Of String)
        Dim message_polynomial_coefficiants As New List(Of String)
        Dim temp As String
        Dim stopwatch As New Stopwatch

        stopwatch = Stopwatch.StartNew

        ProgressBar1.Value = 0

        For Each Lbl As Label In Me.Controls.OfType(Of Label)()

            Lbl.Text = ""

        Next

        Label25.Text = "Original Message: " & input_text & vbCr

        split_list.Clear()

        If validation_return = True Then
            If length_input Mod 2 = 0 Then 'If the input splits exactly into groups of 2

                For a = 1 To (length_input \ 2)
                    split_list.Insert(0, input_text.Substring(length_input + (a * -2), 2)) 'Insert each group of two into a list
                Next

            ElseIf length_input Mod 2 = 1 Then 'If the input doesn't split exactly into groups of 2 (one extra character)

                If length_input = 1 Then 'If there is only one character then insert it into the list 
                    split_list.Insert(0, input_text.Substring(0, 1))
                Else
                    extra = input_text.Substring(length_input - 1, 1) 'Else put the single extra character into a temp variable
                    input_text = input_text.Remove(length_input - 1, 1) 'And insert the remaining groups of two into the list
                    Dim temp_length_input As Integer = input_text.Length

                    For a = 1 To (temp_length_input \ 2)
                        split_list.Insert(0, input_text.Substring(temp_length_input + (a * -2), 2))
                    Next

                    Label4.Text = "Temp Length: " & temp_length_input

                End If
            End If

            binary_string = Conversion(split_list, extra, length_input, encoding_list)
            Label9.Text = binary_string & vbCr & "Original Bit Stream Length (without terminator) : " & length_stream 'Length of bit stream WITHOUT any terminator or additional padding bits

            While ((binary_string.Length) Mod 8) <> 0 'Codewords must be 8 bits in length, pad with 0 until true
                binary_string &= "0"
            End While

            Label10.Text = binary_string & vbCr & "New Length: " & binary_string.Length & vbCr & "Added: " & ((binary_string.Length - length_stream) - 4) & vbCr & ("Difference: ") & (128 - binary_string.Length)

            If binary_string.Count = 128 Then
                codewords_string = CodewordSplit(binary_string)
            Else

                For a = 0 To ((128 - binary_string.Length) \ 8) - 1
                    If (a Mod 2) = 0 Then
                        binary_string &= pad_codeword_1
                    Else
                        binary_string &= pad_codeword_2
                    End If
                Next
            End If

            codewords_string = CodewordSplit(binary_string)
            codewords_string = codewords_string.Replace(",", "")
            data_codewords = codewords_string.Split("-")

            Label11.Text = "Data Codewords"
            For b = 0 To data_codewords.Count - 1
                Label11.Text = Label11.Text & vbCr & data_codewords(b)

            Next
            '-------------ERROR CORRECTION-------------'
            For c = 0 To data_codewords.Count - 2 'Generates message polynomial coefficiants
                temp = Convert.ToInt32(data_codewords(c), 2)
                Label12.Text = Label12.Text & vbCr & temp
                message_polynomial_coefficiants.Add(temp)
            Next

            Label13.Text = 0.5 'Ok so im not sure why this works but its fine

            Dim antilog_list As New List(Of String)

            For a = 0 To 255
                Label13.Text = Label13.Text * 2

                If Label13.Text >= 256 Then
                    Label13.Text = Label13.Text Xor 285
                End If
                antilog_list.Add(Label13.Text)
            Next

            For a = 0 To antilog_list.Count - 1
                Label14.Text = Label14.Text & vbCr & antilog_list(a)
            Next 'End of the bits im not sure how they work

            For a = 0 To 15 'Recursiveley calls the error correction function to generate codewords
                message_polynomial_coefficiants = ErrorCorrectionGeneration(message_polynomial_coefficiants, antilog_list)
            Next

            Label18.Text = "Error Codewords"

            message_polynomial_coefficiants.RemoveRange(10, 6) 'Removes the trailing 0's

            'For a = 0 To message_polynomial_coefficiants.Count - 1 'Displays error correction codewords
            '    Label18.Text = Label18.Text & vbCr & message_polynomial_coefficiants(a)
            'Next

            'Label18.Text = message_polynomial_coefficiants.Count

            Dim hold As Integer


            Label18.Visible = False
            For a = 0 To message_polynomial_coefficiants.Count - 1
                hold = CInt(message_polynomial_coefficiants(a))
                Label18.Text = Convert.ToString(hold, 2).PadLeft(8, "0")
                error_codewords.Add(Label18.Text)
            Next

            Label18.ResetText()

            Label18.Text = "Error Codewords"
            For a = 0 To error_codewords.Count - 1
                Label18.Text = Label18.Text & vbCr & error_codewords(a)
            Next
            Label18.Visible = False

            final_data.AddRange(data_codewords)
            final_data.AddRange(error_codewords)
            final_data.RemoveAt(16)

            Label19.Text = "FINAL DATA"
            Label21.Text = ""
            For a = 0 To final_data.Count - 1
                Label19.Text = Label19.Text & vbCr & final_data(a)
                Label21.Text = Label21.Text & final_data(a)
            Next

            Dim final_data_string As String = Label21.Text

            '-----ERROR CORRECTION-----

            DisplayingMatrix(final_data_string)

        End If

        For i As Integer = 0 To split_list.Count - 1
            Label1.Text += (split_list(i)) & vbCr
        Next

        Label2.Text = "Groups of two: " & length_input \ 2 & vbCr & "Extra: " & extra
        Label3.Text = "Original Length:" & length_input

        System.Threading.Thread.Sleep(500)
        stopwatch.Stop()
        Label25.Text = Label25.Text & "Time Elapsed: " & stopwatch.Elapsed.TotalSeconds & " Seconds"

    End Sub

    Public Function Validation(ByVal input_text, ByVal length_input, ByVal encoding_list)
        Dim test As Boolean = False

        If length_input = 0 Then
            MessageBox.Show("Text field empty", "Error")
            PictureBox1.SendToBack()
        ElseIf (length_input > 20) Then 'Encoding strictly in Version 1-M: 21 x 21 modules MAX 20 Alphanumeric Characters 
            ''Max number of binary data bits for 1-M is 128, for Alphanumeric only, we will reach a max of 123 (20 input characters)
            MessageBox.Show("Text exceeds character capacity", "Error")
        Else
            test = True
        End If

        For a = 0 To length_input - 1
            If encoding_list.Contains(input_text.Substring(a, 1)) = False Then 'Checks if the input contains only valid Alphanumeric Characters
                test = False
                MessageBox.Show("Contains illegal characters", "Error")
                Exit For
            End If
        Next

        Return test

    End Function
    Public Function Conversion(ByVal split_list As List(Of String), ByVal extra As String, ByVal length_input As Integer, ByVal encoding_list As String)
        Dim decimal_list As New List(Of Integer)
        Dim current As String
        Dim position As Integer = 0
        Dim value As Integer = 0
        Dim binary_list As New List(Of String)
        Dim binary_string As String
        Dim mode_indicator As String = "0010" 'Only encoding Alphanumeric for this project

        Label5.ResetText()

        For a = 0 To (length_input \ 2) - 1 'For each of the pairs of two elements, gets their equivalent positions in the encoding table
            current = split_list(a)
            For b = 0 To 1
                position = encoding_list.IndexOf(current.Substring(b, 1))
                decimal_list.Add(position)
            Next
        Next

        For c = 1 To decimal_list.Count - 1 Step 2 'For pairs of numbers, multiply the first by 45 and add to the second
            value = (decimal_list(c - 1) * 45) + decimal_list(c)
            Label6.Text = Convert.ToString(value, 2).PadLeft(11, "0") 'Converts the calculated value to 11-bit Binary String
            binary_list.Add(Label6.Text)
            Label5.Text = Label5.Text & vbCr & value
        Next

        For d = 0 To binary_list.Count - 1
            Label7.Text = Label7.Text & vbCr & binary_list(d)
        Next

        If extra <> "" Then 'If theres an extra digit, convert to 6-bit Binary string
            binary_list.Add(Convert.ToString(encoding_list.IndexOf(extra), 2).PadLeft(6, "0"))
            Label7.Text = Label7.Text & vbCr & "Extra: " & binary_list(binary_list.Count - 1)
        End If

        binary_list.Insert(0, Convert.ToString(length_input, 2).PadLeft(9, "0")) 'Convert the character count to a 9-bit Binary string ''Added before the encoded data, after mode indicator
        Label7.Text = Label7.Text & vbCr & "Character Count: " & binary_list(0)
        binary_list.Insert(0, mode_indicator) 'Adds mode indicator for Alphanumeric Mode before character count, before data
        binary_list.Add("0000") 'Terminator added at the very end of the data string
        Label7.Text = Label7.Text & vbCr & "Mode Indicator: " & binary_list(0)

        'For a = 0 To binary_list.Count - 1 ''Prints entire binary string as one
        '    Label8.Text = Label8.Text & binary_list(a)
        'Next

        binary_string = String.Join(",", binary_list)
        binary_string = binary_string.Replace(",", "")

        Return binary_string

    End Function

    Public Function CodewordSplit(ByVal binary_string As String)
        Dim data_codewords As New List(Of String)
        Dim codewords_string

        For a = 0 To 15
            data_codewords.Add(binary_string.Substring((a * 8), 8) & "-")
        Next

        codewords_string = String.Join(",", data_codewords)

        Return codewords_string

    End Function

    Public Function PlacingIntoMatrix(ByVal final_data_string As String)
        Dim matrix(20, 20) As Integer '21 Modules x 21 Modules

        '0 = White
        '1 = Black
        '2 = Dark Module
        '3 = Reserved Area
        '4 = Blank
        '5 = Timing Pattern ("White")
        '6 = Timing Pattern ("Black")
        '7 = Finder Pattern ("White")
        '8 = Finder Pattern ("Black")
        '9 = Seperator

        matrix = FinderPatterns() 'Adds 3 Finder Patterns in correct places

        For a = 0 To 7 '-----Upper Left Seperators-----
            matrix(7, a) = 9
        Next

        For a = 0 To 7
            matrix(a, 7) = 9
        Next '-----Upper Left Seperators-----

        For a = 0 To 7 '-----Upper Right Seperators-----
            matrix(13, a) = 9
        Next

        For a = 13 To 20
            matrix(a, 7) = 9
        Next '-----Upper Right Seperators-----

        For a = 0 To 7 '-----Lower Left Seperators-----
            matrix(a, 13) = 9
        Next

        For a = 13 To 20
            matrix(7, a) = 9
        Next '-----Lower Left Seperators-----

        For a = 0 To 8 '-----Upper Left Reserved Area-----
            matrix(8, a) = 3
        Next

        For a = 0 To 8
            matrix(a, 8) = 3
        Next '-----Upper Left Reserved Area-----

        For a = 13 To 20 '-----Upper Right Reserved Area-----
            matrix(a, 8) = 3
        Next

        For a = 14 To 20 '-----Lower Left Reserved Area-----
            matrix(8, a) = 3
        Next

        For a = 8 To 12 'Horizontal Timing Pattern
            If a Mod 2 = 0 Then
                matrix(a, 6) = 6
            Else
                matrix(a, 6) = 5
            End If
        Next

        For a = 8 To 12 'Vertical Timing Pattern
            If a Mod 2 = 0 Then
                matrix(6, a) = 6
            Else
                matrix(6, a) = 5
            End If
        Next

        matrix(8, 13) = 2 'Dark Module

        Dim counter As Integer = 0
        Dim data_string As String

        For b = 0 To 11 'First Column: Up
            For a = 0 To 1
                matrix(20 - a, 20 - b) = CInt(final_data_string.Substring(counter, 1))
                counter += 1
            Next

        Next

        For b = 0 To 11 'Second Column: Down
            For a = 0 To 1
                matrix(18 - a, 9 + b) = CInt(final_data_string.Substring(counter, 1))
                counter += 1
            Next
        Next

        For b = 0 To 11 'Third Column: Up
            For a = 0 To 1
                matrix(16 - a, 20 - b) = CInt(final_data_string.Substring(counter, 1))
                counter += 1
            Next

        Next

        For b = 0 To 11 'Fourth Column: Down
            For a = 0 To 1
                matrix(14 - a, 9 + b) = CInt(final_data_string.Substring(counter, 1))
                counter += 1
            Next
        Next

        For b = 0 To 20 'Fifth Column: Up
            For a = 0 To 1
                If matrix(12 - a, 20 - b) <> 4 Then
                    matrix(12 - a, 20 - b) = matrix(12 - a, 20 - b)
                Else
                    matrix(12 - a, 20 - b) = CInt(final_data_string.Substring(counter, 1))
                    counter += 1
                End If
            Next
        Next

        For b = 0 To 20 'Sixth Column: Down
            For a = 0 To 1
                If matrix(10 - a, b) <> 4 Then
                    matrix(10 - a, b) = matrix(10 - a, b)
                Else
                    matrix(10 - a, b) = CInt(final_data_string.Substring(counter, 1))
                    counter += 1
                End If
            Next
        Next

        For b = 0 To 3 'Seventh Column: Up
            For a = 0 To 1
                matrix(8 - a, 12 - b) = CInt(final_data_string.Substring(counter, 1))
                counter += 1
            Next
        Next

        For b = 0 To 3 'Eighth Column: Down
            For a = 0 To 1
                matrix(5 - a, 9 + b) = CInt(final_data_string.Substring(counter, 1))
                counter += 1
            Next
        Next

        For b = 0 To 3 'Ninth Column: Up
            For a = 0 To 1
                matrix(3 - a, 12 - b) = CInt(final_data_string.Substring(counter, 1))
                counter += 1
            Next
        Next

        For b = 0 To 3 'Tenth Column: Down
            For a = 0 To 1
                matrix(1 - a, 9 + b) = CInt(final_data_string.Substring(counter, 1))
                counter += 1
            Next
        Next

        Label22.Text = counter

        For a = 0 To 20 'Prints out entire matrix to a label
            For b = 0 To 20


            Next

        Next

        Dim matrix_copy(20, 20) As Integer
        Array.Copy(matrix, matrix_copy, matrix.Length)

        matrix = Masking(matrix_copy, matrix)


        For a = 0 To 20
            For b = 0 To 20
                Label16.Text = Label16.Text & matrix(b, a)
            Next
            Label16.Text = Label16.Text & vbCr
        Next

        Return matrix

    End Function

    Public Function FinderPatterns()
        Dim matrix(20, 20) As Integer

        For a = 0 To 20 'Fills the matrix with blank character (4)
            For b = 0 To 20
                matrix(b, a) = 4
            Next
        Next

        For a = 0 To 6 '-----Upper Left Finder Pattern-----
            For b = 0 To 6
                matrix(b, a) = 8
            Next
        Next

        For a = 1 To 5
            For b = 1 To 5
                matrix(b, a) = 7
            Next
        Next

        For a = 2 To 4
            For b = 2 To 4
                matrix(b, a) = 8
            Next
        Next '-----Upper Left Finder Pattern-----

        For a = 14 To 20 '-----Lower Left Finder Pattern-----
            For b = 0 To 6
                matrix(b, a) = 8
            Next
        Next

        For a = 15 To 19
            For b = 1 To 5
                matrix(b, a) = 7
            Next
        Next

        For a = 16 To 18
            For b = 2 To 4
                matrix(b, a) = 8
            Next
        Next '-----Lower Left Finder Pattern-----


        For a = 0 To 6 '-----Upper Right Finder Pattern-----
            For b = 14 To 20
                matrix(b, a) = 8
            Next
        Next

        For a = 1 To 5
            For b = 15 To 19
                matrix(b, a) = 7
            Next
        Next

        For a = 2 To 4
            For b = 16 To 18
                matrix(b, a) = 8
            Next
        Next '-----Upper Right Finder Pattern-----


        Return matrix

    End Function



    Public Sub DisplayingMatrix(ByVal final_data_string As String)
        Dim matrix(20, 20) As Integer
        PictureBox1.Image = New Bitmap(PictureBox1.Width, PictureBox1.Height)
        Dim Drawing As Graphics = Graphics.FromImage(PictureBox1.Image)
        Dim height As Integer = PictureBox1.Height
        Dim partition As Integer = height / 21

        Dim CurrentModule As New CurrentModule

        matrix = PlacingIntoMatrix(final_data_string)


        Drawing.DrawLine(Pens.Black, 0, 0, 0, 420) 'Left Border
        Drawing.DrawLine(Pens.Black, 0, 0, 420, 0) 'Top Border
        Drawing.DrawLine(Pens.Black, 420, 0, 420, 420) 'Right Border
        Drawing.DrawLine(Pens.Black, 0, 420, 420, 420) 'Bottom Border

        For a = 0 To 20 'Draw vertical grid lines
            Drawing.DrawLine(Pens.Black, partition + (a * partition), 0, partition + (a * partition), height)
        Next

        For a = 0 To 20 'Draw horizontal grid lines
            Drawing.DrawLine(Pens.Black, 0, partition + (a * partition), height, partition + (a * partition))
        Next

        PictureBox1.Refresh()

        For a = 0 To 20
            For b = 0 To 20
                ProgressBar1.Increment(2)
                If matrix(b, a) = 0 Then 'Fill with a white square if current position is a 0 (White)
                    CurrentModule.PaintModule(matrix, b, a, PictureBox1, Brushes.White)
                ElseIf matrix(b, a) = 1 Then 'Fill with a black square if current position is a 1 (Black)
                    CurrentModule.PaintModule(matrix, b, a, PictureBox1, Brushes.Black)
                ElseIf matrix(b, a) = 2 Then 'Fill with a Gold square if current position is a 2 (Dark Module)
                    CurrentModule.PaintModule(matrix, b, a, PictureBox1, Brushes.Black)
                ElseIf matrix(b, a) = 3 Then 'Fill with a Blue square if current position is a 3 (Reserved Area)
                    CurrentModule.PaintModule(matrix, b, a, PictureBox1, Brushes.MediumBlue)
                ElseIf matrix(b, a) = 4 Then 'Fill with a Grey square if current position is a 4 (Blank)
                    CurrentModule.PaintModule(matrix, b, a, PictureBox1, Brushes.DimGray)
                ElseIf matrix(b, a) = 5 Then 'Fill with a White square if current position is a 5 (Timing Pattern ("White"))
                    CurrentModule.PaintModule(matrix, b, a, PictureBox1, Brushes.White)
                ElseIf matrix(b, a) = 6 Then 'Fill with a Black square if current position is a 6 (Timing Pattern ("Black"))
                    CurrentModule.PaintModule(matrix, b, a, PictureBox1, Brushes.Black)
                ElseIf matrix(b, a) = 7 Then 'Fill with a White square if current position is a 7 (Finder Pattern ("White"))
                    CurrentModule.PaintModule(matrix, b, a, PictureBox1, Brushes.White)
                ElseIf matrix(b, a) = 8 Then 'Fill with a Black square if current position is a 8 (Finder Pattern ("Black"))
                    CurrentModule.PaintModule(matrix, b, a, PictureBox1, Brushes.Black)
                ElseIf matrix(b, a) = 9 Then 'Fill with a White square if current position is a 9 (Seperator Pattern)
                    CurrentModule.PaintModule(matrix, b, a, PictureBox1, Brushes.White)
                End If
            Next
        Next

        Label25.Visible = True

    End Sub

    Public Function ErrorCorrectionGeneration(ByRef message_polynomial_coefficiants As List(Of String), ByVal antilog_list As List(Of String))
        Dim generator_polynomial_alpha_exponents As New List(Of Integer)(New Integer() {0, 251, 67, 46, 61, 118, 70, 64, 94, 32, 45})
        Dim first_term_alpha_notation As String


        For a = 0 To antilog_list.Count - 1
            If antilog_list(a) = message_polynomial_coefficiants(0) Then
                first_term_alpha_notation = a
            End If
        Next

        Label15.Text = first_term_alpha_notation
        first_term_alpha_notation = CInt(first_term_alpha_notation)

        For a = 0 To generator_polynomial_alpha_exponents.Count - 1

            If generator_polynomial_alpha_exponents(a) + first_term_alpha_notation >= 255 Then
                generator_polynomial_alpha_exponents(a) = (generator_polynomial_alpha_exponents(a) + first_term_alpha_notation) Mod 255

            Else
                generator_polynomial_alpha_exponents(a) = generator_polynomial_alpha_exponents(a) + first_term_alpha_notation
            End If
        Next

        For a = 0 To generator_polynomial_alpha_exponents.Count - 1
            generator_polynomial_alpha_exponents(a) = antilog_list(generator_polynomial_alpha_exponents(a))
            Label17.Text = Label17.Text & vbCr & generator_polynomial_alpha_exponents(a)
        Next

        'Label20.Text = "Message: " & message_polynomial_coefficiants.Count & vbCr & "Generator: " & generator_polynomial_alpha_exponents.Count

        For a = 0 To generator_polynomial_alpha_exponents.Count - 1
            message_polynomial_coefficiants(a) = message_polynomial_coefficiants(a) Xor generator_polynomial_alpha_exponents(a)

        Next

        message_polynomial_coefficiants.RemoveAt(0)
        message_polynomial_coefficiants.Add(0)

        Return message_polynomial_coefficiants

    End Function

    Public Function Masking(ByVal matrix_copy(,) As Integer, ByVal matrix(,) As Integer)
        Dim counter As Integer = 0
        Dim score_list As New List(Of Integer)
        Dim masked_matrix(20, 20) As Integer
        Dim mask_chosen As Integer = 0

        Label23.Text = "Scores"



        'Initial masks are applied To matrix_copy, the array 'matrix' should never be altered
        'Once mask has been evaluated, copy of matrix Is reset to the original state And the next mask Is applied
        'Once all the evaluation scores are In, re-apply selected mask

        score_list.Add(MaskPattern0(matrix_copy)) 'Adds the score for Mask Pattern 1 to a list
        Array.Copy(matrix, matrix_copy, matrix.Length) 'Resets matrix_copy to original state

        score_list.Add(MaskPattern1(matrix_copy)) 'Adds the score for Mask Pattern 2 to a list
        Array.Copy(matrix, matrix_copy, matrix.Length) 'Resets matrix_copy to original state

        score_list.Add(MaskPattern2(matrix_copy)) 'Adds the score for Mask Pattern 3 to a list
        Array.Copy(matrix, matrix_copy, matrix.Length) 'Resets matrix_copy to original state

        score_list.Add(MaskPattern3(matrix_copy)) 'Adds the score for Mask Pattern 4 to a list
        Array.Copy(matrix, matrix_copy, matrix.Length) 'Resets matrix_copy to original state

        score_list.Add(MaskPattern4(matrix_copy)) 'Adds the score for Mask Pattern 5 to a list
        Array.Copy(matrix, matrix_copy, matrix.Length) 'Resets matrix_copy to original state

        score_list.Add(MaskPattern5(matrix_copy)) 'Adds the score for Mask Pattern 6 to a list
        Array.Copy(matrix, matrix_copy, matrix.Length) 'Resets matrix_copy to original state

        score_list.Add(MaskPattern6(matrix_copy)) 'Adds the score for Mask Pattern 7 to a list
        Array.Copy(matrix, matrix_copy, matrix.Length) 'Resets matrix_copy to original state

        score_list.Add(MaskPattern7(matrix_copy)) 'Adds the score for Mask Pattern 8 to a list
        Array.Copy(matrix, matrix_copy, matrix.Length) 'Resets matrix_copy to original state



        For a = 0 To score_list.Count - 1 'Output the scorelist for each mask pattern
            Label23.Text = Label23.Text & vbCr & score_list(a)
        Next


        If score_list.IndexOf(score_list.Min) = 0 Then 'This IF Statement checks which mask produced the lowest penalty and re-applies it
            MaskPattern0(matrix_copy)
            mask_chosen = 0
        ElseIf score_list.IndexOf(score_list.Min) = 1 Then
            MaskPattern1(matrix_copy)
            mask_chosen = 1
        ElseIf score_list.IndexOf(score_list.Min) = 2 Then
            MaskPattern2(matrix_copy)
            mask_chosen = 2
        ElseIf score_list.IndexOf(score_list.Min) = 3 Then
            MaskPattern3(matrix_copy)
            mask_chosen = 3
        ElseIf score_list.IndexOf(score_list.Min) = 4 Then
            MaskPattern4(matrix_copy)
            mask_chosen = 4
        ElseIf score_list.IndexOf(score_list.Min) = 5 Then
            MaskPattern5(matrix_copy)
            mask_chosen = 5
        ElseIf score_list.IndexOf(score_list.Min) = 6 Then
            MaskPattern6(matrix_copy)
            mask_chosen = 6
        ElseIf score_list.IndexOf(score_list.Min) = 7 Then
            MaskPattern7(matrix_copy)
            mask_chosen = 7
        End If

        Label25.Text = Label25.Text & "Mask Pattern Chosen: " & mask_chosen & vbCr

        Array.Copy(matrix_copy, masked_matrix, masked_matrix.Length)

        masked_matrix = FormatAndMaskInfo(masked_matrix, mask_chosen) 'Add the Format and Masking information

        Return masked_matrix 'Return the masked matrix once evaluation and selection of mask has been carried out
    End Function

    Public Function MaskPattern0(ByRef matrix_copy(,) As Integer)
        Dim score As Integer = 0

        '((row + column) Mod 2 == 0)

        For a = 0 To 20
            For b = 0 To 20
                If (matrix_copy(b, a) = 0) Or (matrix_copy(b, a) = 1) Then 'Only checking data/error bits
                    If ((a + b) Mod 2) = 0 Then 'Check if formula for this mask pattern = 0
                        If matrix_copy(b, a) = 0 Then 'If current bit is 0, change to 1
                            matrix_copy(b, a) = 1
                        ElseIf matrix_copy(b, a) = 1 Then 'If current bit is 1, change to 0
                            matrix_copy(b, a) = 0
                        End If
                    End If
                End If
            Next
        Next

        score = EvalutingMask(matrix_copy)


        Return score
    End Function

    Public Function MaskPattern1(ByRef matrix_copy(,) As Integer)
        Dim score As Integer = 0

        '((row) Mod 2 == 0)

        For a = 0 To 20
            For b = 0 To 20
                If (matrix_copy(b, a) = 0) Or (matrix_copy(b, a) = 1) Then 'Only checking data/error bits
                    If ((a) Mod 2) = 0 Then 'Check if formula for this mask pattern = 0
                        If matrix_copy(b, a) = 0 Then 'If current bit is 0, change to 1
                            matrix_copy(b, a) = 1
                        ElseIf matrix_copy(b, a) = 1 Then 'If current bit is 1, change to 0
                            matrix_copy(b, a) = 0
                        End If
                    End If
                End If
            Next
        Next

        score = EvalutingMask(matrix_copy)


        Return score
    End Function

    Public Function MaskPattern2(ByRef matrix_copy(,) As Integer)
        Dim score As Integer = 0

        '((column) Mod 3 == 0)

        For a = 0 To 20
            For b = 0 To 20
                If (matrix_copy(b, a) = 0) Or (matrix_copy(b, a) = 1) Then 'Only checking data/error bits
                    If ((b) Mod 3) = 0 Then 'Check if formula for this mask pattern = 0
                        If matrix_copy(b, a) = 0 Then 'If current bit is 0, change to 1
                            matrix_copy(b, a) = 1
                        ElseIf matrix_copy(b, a) = 1 Then 'If current bit is 1, change to 0
                            matrix_copy(b, a) = 0
                        End If
                    End If
                End If
            Next
        Next

        score = EvalutingMask(matrix_copy)


        Return score
    End Function

    Public Function MaskPattern3(ByRef matrix_copy(,) As Integer)
        Dim score As Integer = 0

        '((row + column) Mod 3 == 0)

        For a = 0 To 20
            For b = 0 To 20
                If (matrix_copy(b, a) = 0) Or (matrix_copy(b, a) = 1) Then 'Only checking data/error bits
                    If ((a + b) Mod 3) = 0 Then 'Check if formula for this mask pattern = 0
                        If matrix_copy(b, a) = 0 Then 'If current bit is 0, change to 1
                            matrix_copy(b, a) = 1
                        ElseIf matrix_copy(b, a) = 1 Then 'If current bit is 1, change to 0
                            matrix_copy(b, a) = 0
                        End If
                    End If
                End If
            Next
        Next

        score = EvalutingMask(matrix_copy)


        Return score
    End Function

    Public Function MaskPattern4(ByRef matrix_copy(,) As Integer)
        Dim score As Integer = 0

        '((floor(row / 2) + floor(column / 3)) mod 2) = 0

        For a = 0 To 20
            For b = 0 To 20
                If (matrix_copy(b, a) = 0) Or (matrix_copy(b, a) = 1) Then 'Only checking data/error bits
                    If ((Math.Floor(a / 2) + Math.Floor(b / 3) Mod 2) = 0) Then 'Check if formula for this mask pattern = 0
                        If matrix_copy(b, a) = 0 Then 'If current bit is 0, change to 1
                            matrix_copy(b, a) = 1
                        ElseIf matrix_copy(b, a) = 1 Then 'If current bit is 1, change to 0
                            matrix_copy(b, a) = 0
                        End If
                    End If
                End If
            Next
        Next

        score = EvalutingMask(matrix_copy)

        Return score
    End Function

    Public Function MaskPattern5(ByRef matrix_copy(,) As Integer)
        Dim score As Integer = 0

        '((row * column) mod 2) + ((row * column) mod 3) = 0

        For a = 0 To 20
            For b = 0 To 20
                If (matrix_copy(b, a) = 0) Or (matrix_copy(b, a) = 1) Then 'Only checking data/error bits
                    If (((a * b) Mod 2) + ((a * b) Mod 3) = 0) Then 'Check if formula for this mask pattern = 0
                        If matrix_copy(b, a) = 0 Then 'If current bit is 0, change to 1
                            matrix_copy(b, a) = 1
                        ElseIf matrix_copy(b, a) = 1 Then 'If current bit is 1, change to 0
                            matrix_copy(b, a) = 0
                        End If
                    End If
                End If
            Next
        Next

        score = EvalutingMask(matrix_copy)

        Return score
    End Function

    Public Function MaskPattern6(ByRef matrix_copy(,) As Integer)
        Dim score As Integer = 0

        '( ((row * column) mod 2) + ((row * column) mod 3) ) mod 2 = 0

        For a = 0 To 20
            For b = 0 To 20
                If (matrix_copy(b, a) = 0) Or (matrix_copy(b, a) = 1) Then 'Only checking data/error bits
                    If (((((a * b) Mod 2) + ((a * b) Mod 3)) Mod 2) = 0) Then 'Check if formula for this mask pattern = 0
                        If matrix_copy(b, a) = 0 Then 'If current bit is 0, change to 1
                            matrix_copy(b, a) = 1
                        ElseIf matrix_copy(b, a) = 1 Then 'If current bit is 1, change to 0
                            matrix_copy(b, a) = 0
                        End If
                    End If
                End If
            Next
        Next

        score = EvalutingMask(matrix_copy)

        Return score
    End Function

    Public Function MaskPattern7(ByRef matrix_copy(,) As Integer)
        Dim score As Integer = 0

        '( ((row * column) mod 2) + ((row * column) mod 3) ) mod 2 = 0

        For a = 0 To 20
            For b = 0 To 20
                If (matrix_copy(b, a) = 0) Or (matrix_copy(b, a) = 1) Then 'Only checking data/error bits
                    If (((((a + b) Mod 2) + ((a * b) Mod 3)) Mod 2) = 0) Then 'Check if formula for this mask pattern = 0
                        If matrix_copy(b, a) = 0 Then 'If current bit is 0, change to 1
                            matrix_copy(b, a) = 1
                        ElseIf matrix_copy(b, a) = 1 Then 'If current bit is 1, change to 0
                            matrix_copy(b, a) = 0
                        End If
                    End If
                End If
            Next
        Next

        score = EvalutingMask(matrix_copy)

        Return score
    End Function

    Public Function EvalutingMask(ByVal matrix_copy(,) As Integer)
        Dim score As Integer = 0
        Dim condition_1_score As Integer = 0
        Dim condition_2_score As Integer = 0
        Dim condition_3_score As Integer = 0
        Dim condition_4_score As Integer = 0

        Dim break As Boolean = False

        Dim dark_module_count As Integer = 0
        Dim ratio_dark_module As Integer = 0
        Dim round_down As Integer = 0
        Dim round_up As Integer = 0

        Dim pattern_1_count As Integer = 0
        Dim pattern_2_count As Integer = 0

        Dim CurrentModule As New CurrentModule

        '-----Condition 1-----

        For c = 0 To 20 'Check for Black Horizontal
            For a = 0 To 16
                If CurrentModule.IsBlack(matrix_copy, a, c) = True And
               CurrentModule.IsBlack(matrix_copy, a + 1, c) = True And
               CurrentModule.IsBlack(matrix_copy, a + 2, c) = True And
               CurrentModule.IsBlack(matrix_copy, a + 3, c) = True And
               CurrentModule.IsBlack(matrix_copy, a + 4, c) = True Then
                    condition_1_score += 3
                    For b = (a + 4) + 1 To 20
                        If CurrentModule.IsBlack(matrix_copy, b, c) = True Then
                            condition_1_score += 1
                            a = b
                        Else
                            a = b
                            b = 20
                        End If
                    Next
                End If
            Next
        Next

        For c = 0 To 20 'Check for White Horizontal
            For a = 0 To 16
                If CurrentModule.IsWhite(matrix_copy, a, c) = True And
               CurrentModule.IsWhite(matrix_copy, a + 1, c) = True And
               CurrentModule.IsWhite(matrix_copy, a + 2, c) = True And
               CurrentModule.IsWhite(matrix_copy, a + 3, c) = True And
               CurrentModule.IsWhite(matrix_copy, a + 4, c) = True Then
                    condition_1_score += 3
                    For b = (a + 4) + 1 To 20
                        If CurrentModule.IsBlack(matrix_copy, b, c) = True Then
                            condition_1_score += 1
                            a = b
                        Else
                            a = b
                            b = 20
                        End If
                    Next
                End If
            Next
        Next

        For c = 0 To 20 'Check for Black Vertical
            For a = 0 To 16
                If CurrentModule.IsBlack(matrix_copy, c, a) = True And
               CurrentModule.IsBlack(matrix_copy, c, a + 1) = True And
               CurrentModule.IsBlack(matrix_copy, c, a + 2) = True And
               CurrentModule.IsBlack(matrix_copy, c, a + 3) = True And
               CurrentModule.IsBlack(matrix_copy, c, a + 4) = True Then
                    condition_1_score += 3
                    For b = (a + 4) + 1 To 20
                        If CurrentModule.IsBlack(matrix_copy, c, b) = True Then
                            condition_1_score += 1
                            a = b
                        Else
                            a = b
                            b = 20
                        End If
                    Next
                End If
            Next
        Next


        For c = 0 To 20 'Check for White Vertical
            For a = 0 To 16
                If CurrentModule.IsWhite(matrix_copy, c, a) = True And
               CurrentModule.IsWhite(matrix_copy, c, a + 1) = True And
               CurrentModule.IsWhite(matrix_copy, c, a + 2) = True And
               CurrentModule.IsWhite(matrix_copy, c, a + 3) = True And
               CurrentModule.IsWhite(matrix_copy, c, a + 4) = True Then
                    condition_1_score += 3
                    For b = (a + 4) + 1 To 20
                        If CurrentModule.IsBlack(matrix_copy, c, b) = True Then
                            condition_1_score += 1
                            a = b
                        Else
                            a = b
                            b = 20
                        End If
                    Next
                End If
            Next
        Next

        Label26.Text = condition_1_score

        '-----Condition 1-----

        '-----Condition 2-----

        For b = 0 To 18 Step 2 'Checking for 2x2 blocks of the same colour - Excluding Left and Bottom
            For a = 1 To 19
                If CurrentModule.IsBlack(matrix_copy, a, b) = True And
                   CurrentModule.IsBlack(matrix_copy, a, b + 1) = True And
                   CurrentModule.IsBlack(matrix_copy, a + 1, b) = True And
                   CurrentModule.IsBlack(matrix_copy, a + 1, b + 1) = True Then
                    condition_2_score += 3
                End If
                If CurrentModule.IsWhite(matrix_copy, a, b) = True And
                   CurrentModule.IsWhite(matrix_copy, a, b + 1) = True And
                   CurrentModule.IsWhite(matrix_copy, a + 1, b) = True And
                   CurrentModule.IsWhite(matrix_copy, a + 1, b + 1) = True Then
                    condition_2_score += 3
                End If
            Next
        Next

        For b = 0 To 18 Step 2 'Checking for 2x2 blocks of the same colour - Excluding Right and Bottom
            For a = 0 To 18
                If CurrentModule.IsBlack(matrix_copy, a, b) = True And
                   CurrentModule.IsBlack(matrix_copy, a, b + 1) = True And
                   CurrentModule.IsBlack(matrix_copy, a + 1, b) = True And
                   CurrentModule.IsBlack(matrix_copy, a + 1, b + 1) = True Then
                    condition_2_score += 3
                End If
                If CurrentModule.IsWhite(matrix_copy, a, b) = True And
                   CurrentModule.IsWhite(matrix_copy, a, b + 1) = True And
                   CurrentModule.IsWhite(matrix_copy, a + 1, b) = True And
                   CurrentModule.IsWhite(matrix_copy, a + 1, b + 1) = True Then
                    condition_2_score += 3
                End If
            Next
        Next

        For b = 1 To 19 Step 2 'Checking for 2x2 blocks of the same colour - Excluding Top and Left
            For a = 1 To 19
                If CurrentModule.IsBlack(matrix_copy, a, b) = True And
                   CurrentModule.IsBlack(matrix_copy, a, b + 1) = True And
                   CurrentModule.IsBlack(matrix_copy, a + 1, b) = True And
                   CurrentModule.IsBlack(matrix_copy, a + 1, b + 1) = True Then
                    condition_2_score += 3
                End If
                If CurrentModule.IsWhite(matrix_copy, a, b) = True And
                   CurrentModule.IsWhite(matrix_copy, a, b + 1) = True And
                   CurrentModule.IsWhite(matrix_copy, a + 1, b) = True And
                   CurrentModule.IsWhite(matrix_copy, a + 1, b + 1) = True Then
                    condition_2_score += 3
                End If
            Next
        Next

        For b = 1 To 19 Step 2 'Checking for 2x2 blocks of the same colour - Excluding Top and Right
            For a = 0 To 18
                If b = 17 Then
                    b = 18
                End If
                If CurrentModule.IsBlack(matrix_copy, a, b) = True And
                   CurrentModule.IsBlack(matrix_copy, a, b + 1) = True And
                   CurrentModule.IsBlack(matrix_copy, a + 1, b) = True And
                   CurrentModule.IsBlack(matrix_copy, a + 1, b + 1) = True Then
                    condition_2_score += 3
                End If
                If CurrentModule.IsWhite(matrix_copy, a, b) = True And
                   CurrentModule.IsWhite(matrix_copy, a, b + 1) = True And
                   CurrentModule.IsWhite(matrix_copy, a + 1, b) = True And
                   CurrentModule.IsWhite(matrix_copy, a + 1, b + 1) = True Then
                    condition_2_score += 3
                End If
            Next
        Next

        '-----Condition 2-----

        '-----Condition 3-----


        For b = 0 To 20 'Iterating through the matrix, checking rows for both patterns
            For a = 0 To 10
                If CurrentModule.IsWhite(matrix_copy, a + 0, b) = True And
                   CurrentModule.IsWhite(matrix_copy, a + 1, b) = True And
                   CurrentModule.IsWhite(matrix_copy, a + 2, b) = True And
                   CurrentModule.IsWhite(matrix_copy, a + 3, b) = True And
                   CurrentModule.IsBlack(matrix_copy, a + 4, b) = True And
                   CurrentModule.IsWhite(matrix_copy, a + 5, b) = True And
                   CurrentModule.IsBlack(matrix_copy, a + 6, b) = True And
                   CurrentModule.IsBlack(matrix_copy, a + 7, b) = True And
                   CurrentModule.IsBlack(matrix_copy, a + 8, b) = True And
                   CurrentModule.IsWhite(matrix_copy, a + 9, b) = True And
                   CurrentModule.IsBlack(matrix_copy, a + 10, b) = True Then
                    pattern_2_count += 1
                ElseIf CurrentModule.IsBlack(matrix_copy, a + 0, b) = True And
                   CurrentModule.IsWhite(matrix_copy, a + 1, b) = True And
                   CurrentModule.IsBlack(matrix_copy, a + 2, b) = True And
                   CurrentModule.IsBlack(matrix_copy, a + 3, b) = True And
                   CurrentModule.IsBlack(matrix_copy, a + 4, b) = True And
                   CurrentModule.IsWhite(matrix_copy, a + 5, b) = True And
                   CurrentModule.IsBlack(matrix_copy, a + 6, b) = True And
                   CurrentModule.IsWhite(matrix_copy, a + 7, b) = True And
                   CurrentModule.IsWhite(matrix_copy, a + 8, b) = True And
                   CurrentModule.IsWhite(matrix_copy, a + 9, b) = True And
                   CurrentModule.IsWhite(matrix_copy, a + 10, b) = True Then
                    pattern_1_count += 1
                End If
            Next
        Next

        For a = 0 To 20 'Iterating through the matrix, checking columns for both patterns
            For b = 0 To 10
                If CurrentModule.IsWhite(matrix_copy, a, b + 0) = True And
                   CurrentModule.IsWhite(matrix_copy, a, b + 1) = True And
                   CurrentModule.IsWhite(matrix_copy, a, b + 2) = True And
                   CurrentModule.IsWhite(matrix_copy, a, b + 3) = True And
                   CurrentModule.IsBlack(matrix_copy, a, b + 4) = True And
                   CurrentModule.IsWhite(matrix_copy, a, b + 5) = True And
                   CurrentModule.IsBlack(matrix_copy, a, b + 6) = True And
                   CurrentModule.IsBlack(matrix_copy, a, b + 7) = True And
                   CurrentModule.IsBlack(matrix_copy, a, b + 8) = True And
                   CurrentModule.IsWhite(matrix_copy, a, b + 9) = True And
                   CurrentModule.IsBlack(matrix_copy, a, b + 10) = True Then
                    pattern_2_count += 1
                ElseIf CurrentModule.IsBlack(matrix_copy, a, b + 0) = True And
                   CurrentModule.IsWhite(matrix_copy, a, b + 1) = True And
                   CurrentModule.IsBlack(matrix_copy, a, b + 2) = True And
                   CurrentModule.IsBlack(matrix_copy, a, b + 3) = True And
                   CurrentModule.IsBlack(matrix_copy, a, b + 4) = True And
                   CurrentModule.IsWhite(matrix_copy, a, b + 5) = True And
                   CurrentModule.IsBlack(matrix_copy, a, b + 6) = True And
                   CurrentModule.IsWhite(matrix_copy, a, b + 7) = True And
                   CurrentModule.IsWhite(matrix_copy, a, b + 8) = True And
                   CurrentModule.IsWhite(matrix_copy, a, b + 9) = True And
                   CurrentModule.IsWhite(matrix_copy, a, b + 10) = True Then
                    pattern_1_count += 1
                End If
            Next
        Next



        condition_3_score = 40 * (pattern_1_count + pattern_2_count)

        '-----Condition 3-----

        '-----Condition 4-----

        For a = 0 To 20 'Counts the total number of dark modules in the matrix
            For b = 0 To 20
                If CurrentModule.IsBlack(matrix_copy, a, b) = True Then
                    dark_module_count += 1
                End If
            Next
        Next

        ratio_dark_module = (dark_module_count / matrix_copy.Length) * 100 'Caclulates the ratio of dark modules to total modules



        round_down = (CLng(ratio_dark_module / 5) * 5)

        If ratio_dark_module Mod 5 = 0 Then 'Finds the previous and next multiples of 5

            If round_down = ratio_dark_module Then
                round_down -= 5
                round_up = round_down + 10
            Else
                round_up = round_down + 5

            End If

        ElseIf round_down < ratio_dark_module Then
            round_up = round_down + 5

        ElseIf round_down > ratio_dark_module Then
            round_up = round_down
            round_down -= 5

        End If

        round_down = (Math.Abs(round_down - 50)) / 5
        round_up = (Math.Abs(round_up - 50)) / 5

        If round_down > round_up Then
            condition_4_score = round_up * 10
        Else
            condition_4_score = round_down * 10
        End If

        '-----Condition 4-----

        score = condition_1_score + condition_2_score + condition_3_score + condition_4_score 'Accumulates penalty scores from the 4 conditions


        Return score
    End Function

    Public Function FormatAndMaskInfo(ByRef masked_matrix(,) As Integer, ByVal mask_chosen As Integer)
        Dim FormatAndMaskInfoString As String = ""

        If mask_chosen = 0 Then 'Format and Mask Info corresponding to each mask chosen
            FormatAndMaskInfoString = "101010000010010"
        ElseIf mask_chosen = 1 Then
            FormatAndMaskInfoString = "101000100100101"
        ElseIf mask_chosen = 2 Then
            FormatAndMaskInfoString = "101111001111100"
        ElseIf mask_chosen = 3 Then
            FormatAndMaskInfoString = "101101101001011"
        ElseIf mask_chosen = 4 Then
            FormatAndMaskInfoString = "100010111111001"
        ElseIf mask_chosen = 5 Then
            FormatAndMaskInfoString = "100000011001110"
        ElseIf mask_chosen = 6 Then
            FormatAndMaskInfoString = "100111110010111"
        ElseIf mask_chosen = 7 Then
            FormatAndMaskInfoString = "100101010100000"
        End If

        For a = 0 To 5 'Following Iteration statements place the Format and Mask Info into the matrix
            masked_matrix(a, 8) = CInt(FormatAndMaskInfoString.Substring(a, 1))
            masked_matrix(8, 5 - a) = CInt(FormatAndMaskInfoString.Substring(a + 9, 1))
        Next

        For a = 13 To 20
            masked_matrix(a, 8) = CInt(FormatAndMaskInfoString.Substring(a - 6, 1))
        Next

        For a = 0 To 6
            masked_matrix(8, 20 - a) = CInt(FormatAndMaskInfoString.Substring(a, 1))
        Next

        masked_matrix(7, 8) = CInt(FormatAndMaskInfoString.Substring(6, 1))
        masked_matrix(8, 8) = CInt(FormatAndMaskInfoString.Substring(7, 1))
        masked_matrix(8, 7) = CInt(FormatAndMaskInfoString.Substring(8, 1))

        Return masked_matrix
    End Function

End Class
